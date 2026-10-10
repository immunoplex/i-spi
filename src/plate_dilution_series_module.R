# =============================================================================
# plate_dilution_series_module.R  --  "Plate Dilution Series" QC tab.
#
# Reproduces the faceted "Dilution Series by Plate" figure from the analysis
# notebooks: one facet per plate, log-log axes (x = dilution, y = antibody_mfi),
# and one line+marker trace per group, joining the raw standard-curve points
# (rows of madi_results.xmap_standard) across the dilution series.
#
# Two sub-tabs, both driving a facet-by-plate grid:
#
#   * "Analytes" -- a Standard-curve SOURCE selector filters to a single source;
#                   each plate facet then shows one trace per antigen+feature
#                   (analyte) combination. MASKING/UNMASKING is supported here
#                   (see below) -- built directly with plotly::plot_ly() +
#                   plotly::subplot() (NOT ggplot2/ggplotly) so every point can
#                   carry real customdata for click identification, mirroring
#                   std_curve_view_module.R's Explore-fits curve plot exactly.
#
#   * "Sources"  -- ANTIGEN + FEATURE selectors filter to a single analyte; each
#                   plate facet then shows one trace per standard-curve source.
#                   In addition, any TEST SAMPLE (patientid / timeperiod) that
#                   was run at more than one dilution on a plate is overlaid as
#                   its own dashed trace, labelled by patientid + timeperiod --
#                   useful for reading optimization plates. Read-only: still
#                   built with ggplot2 + plotly::ggplotly() (pds_facet_plot()),
#                   unchanged. Masked points are shown identically to included
#                   ones here (no mask/unmask support on this sub-tab).
#
# MASKING ON THE ANALYTES TAB (see mask_ui_helpers.R for the shared pieces):
# Clicking an included point stages it for masking; double-clicking a masked
# (hollow) point stages it for unmasking; a "Save / Apply mask" (or "Unmask")
# button opens the same reason/scope/keep-fits/dry-run modal the Explore-fits
# tab uses, and resolves to the SAME backend calls (apply_mask/apply_unmask,
# calib_data_access.R). The plot spans every plate (and, for one source, every
# antigen) so contamination can be SEEN in context -- but a staged batch is
# still pinned to the ONE curve_id (one plate + one antigen/feature) the first
# staged point belongs to; a click on a point belonging to a DIFFERENT curve
# while a batch is active is refused with a notification rather than mixed in
# or silently swapped. Masking a well across the rest of that antigen's plates
# is then just: stage there, save, then repeat on the next plate -- separate
# saves per plate are expected and fine.
#
# X axis: log10-scaled, with decade tick marks labelled in natural units
# (e.g. 0.001, 0.01, 0.1, 1) tilted 60 degrees; titled "Dilution Fraction".
#
# Reads raw standards / samples via fetch_raw_standard() / fetch_raw_sample()
# (calib_data_access.R), like the Data tab does -- masked/mask_reason on these
# rows is always LIVE (no calib_standards/calib_blanks snapshot involved on
# this tab at all, so there's no staleness lag in what's rendered as
# masked/unmasked here, unlike Explore-fits' plotted fit line).
#
# Contract (mirrors the calib_* modules):
#   pool           : DBI/pool handle (db_pool)
#   scope          : reactive -> list(study, experiment, project_id)
#   reload_trigger : reactiveVal(int); bump to force a re-read (experiment change,
#                    Data-tab Refresh, mask/unmask save here). Optional; defaults
#                    to a no-op.
#   calib_dirty    : reactiveVal(int); the SAME reactive std_curve_view_module.R
#                    (Explore fits) and std_curve_calc_module.R (Compute fits)
#                    share. Read here so a mask/unmask/recompute done on THOSE
#                    tabs refreshes this tab's stale-fit banner; bumped here so
#                    a mask/unmask done on THIS tab shows up as stale over
#                    there too. Optional; defaults to a no-op reactiveVal.
# =============================================================================

# ---- UI ---------------------------------------------------------------------
plateDilutionSeriesUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$div(
      style = "margin:6px 0 10px;color:#555;",
      help_icon("qc.plate_dilution_series", ns),
      shiny::tags$p(
        "Standard-curve points from each plate, joined across the dilution ",
        "series on log-log axes. One facet per plate. Masked points are kept ",
        "in view so contamination that needs masking is easy to spot.")
    ),
    shiny::tabsetPanel(
      id = ns("pds_subtabs"),

      # ---- Analytes: pick a source, trace per antigen+feature -------------
      shiny::tabPanel(
        title = "Analytes",
        shiny::fluidRow(
          shiny::column(
            width = 3,
            shiny::wellPanel(
              shiny::h4("Standard source"),
              shiny::helpText(
                "Filter to a single standard-curve source. Each plate then ",
                "shows one trace per antigen + feature. Click an included ",
                "point to stage it for masking (click again to unstage); ",
                "double-click a masked (hollow) point to stage it for ",
                "unmasking. A staged batch is pinned to one plate + antigen ",
                "at a time -- mask other plates separately."),
              shiny::uiOutput(ns("analytes_source_ui"))
            )
          ),
          shiny::column(
            width = 9,
            shiny::uiOutput(ns("analytes_stale_banner")),
            shiny::uiOutput(ns("analytes_status")),
            shinycssloaders::withSpinner(
              shiny::uiOutput(ns("analytes_plot_ui")),
              type = 4, color = "#337ab7"),
            shiny::uiOutput(ns("analytes_mask_selection")),
            shiny::uiOutput(ns("analytes_unmask_selection"))
          )
        )
      ),

      # ---- Sources: pick an antigen+feature, trace per source -------------
      shiny::tabPanel(
        title = "Sources",
        shiny::fluidRow(
          shiny::column(
            width = 3,
            shiny::wellPanel(
              shiny::h4("Analyte"),
              shiny::helpText(
                "Filter to a single antigen + feature. Each plate then shows ",
                "one trace per standard-curve source, plus any test sample run ",
                "at more than one dilution (dashed, labelled patient/timeperiod)."),
              shiny::uiOutput(ns("sources_antigen_ui")),
              shiny::uiOutput(ns("sources_feature_ui"))
            )
          ),
          shiny::column(
            width = 9,
            shiny::uiOutput(ns("sources_status")),
            shinycssloaders::withSpinner(
              shiny::uiOutput(ns("sources_plot_ui")),
              type = 4, color = "#337ab7")
          )
        )
      )
    )
  )
}

# ---- Server -----------------------------------------------------------------
plateDilutionSeriesServer <- function(id, pool, scope,
                                      reload_trigger = shiny::reactiveVal(0),
                                      calib_dirty     = shiny::reactiveVal(0)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

    shiny::observeEvent(input$help_show, {
      hid <- input$help_show
      body <- help_modal_body(hid, ns)
      if (is.null(body)) return()
      shiny::showModal(shiny::modalDialog(
        title = help_modal_title(hid), body,
        easyClose = TRUE, size = "l", footer = shiny::modalButton("Close")))
    })

    NONE_SRC  <- "(no source)"     # pretty label for the __none__ / blank source
    NONE_FEAT <- "(no feature)"    # pretty label for a missing feature
    N_COLS    <- 4L                # facet columns (matches the notebook: n_cols)

    # Pretty-print the `source` natural-key value (mirrors the viewer module).
    src_label <- function(x) {
      x <- as.character(x)
      ifelse(is.na(x) | x %in% c("__none__", ""), NONE_SRC, x)
    }
    feat_label <- function(x) {
      x <- as.character(x)
      ifelse(is.na(x) | x %in% c("__none__", ""), NONE_FEAT, x)
    }

    # plate facet order: sort by the first integer in the plate label, then
    # alphabetically, so "Plate 2" precedes "Plate 10".
    order_plate_factor <- function(plate_lbl) {
      plate_lbl <- as.character(plate_lbl)
      pint <- suppressWarnings(as.integer(stringr::str_extract(plate_lbl, "\\d+")))
      lev  <- unique(plate_lbl[order(pint, plate_lbl)])
      factor(plate_lbl, levels = lev)
    }

    # Shared prep for any raw xmap_* points frame (standards OR samples): coerce
    # dilution/MFI to numeric, build antigen/feature/plate/source helper columns,
    # and drop rows that can't sit on log-log axes. Masked rows are KEPT -- and,
    # since this reads the BASE table (fetch_raw_standard = SELECT * FROM
    # xmap_standard), `masked`/`mask_reason` (if present in df) are passed
    # through untouched: they are the LIVE flag, not a fit-time snapshot.
    prep_points <- function(df) {
      if (is.null(df) || !nrow(df)) return(NULL)
      if (!"feature"      %in% names(df)) df$feature <- NA_character_
      if (!"antigen"      %in% names(df)) df$antigen <- NA_character_
      if (!"source"       %in% names(df)) df$source  <- NA_character_
      if (!"antibody_mfi" %in% names(df)) return(NULL)
      if (!"dilution"     %in% names(df)) return(NULL)
      plate_raw <- if ("plate" %in% names(df) && !all(is.na(df$plate)))
                     as.character(df$plate)
                   else if ("plateid" %in% names(df)) as.character(df$plateid)
                   else "plate"
      num <- function(x) suppressWarnings(as.numeric(as.character(x)))
      df$dil_num     <- num(df$dilution)
      df$mfi_num     <- num(df$antibody_mfi)
      df$feature_lbl <- feat_label(df$feature)
      df$antigen_lbl <- as.character(df$antigen)
      df$plate_lbl   <- plate_raw
      df <- df[is.finite(df$dil_num) & df$dil_num > 0 &
               is.finite(df$mfi_num) & df$mfi_num > 0, , drop = FALSE]
      if (!nrow(df)) return(NULL)
      df
    }

    # ---- raw standards for the current scope, prepped for plotting ---------
    # Depends on calib_dirty() too (not just reload_trigger()) so a mask/unmask
    # done on the Explore-fits tab -- which touches the SAME xmap_standard rows
    # -- refreshes this raw view as well, not only a save made here.
    std_prepped <- shiny::reactive({
      s <- scope(); shiny::req(s$study, s$experiment, s$project_id)
      reload_trigger()  # dependency so Refresh / mask-save re-reads
      calib_dirty()      # dependency so a mask/unmask from Explore-fits/Compute-fits re-reads too

      raw <- tryCatch(
        fetch_raw_standard(pool, project = s$project_id,
                           study = s$study, experiment = s$experiment),
        error = function(e) { message("[plate-dilution] std fetch failed: ",
                                       conditionMessage(e)); NULL })
      df <- prep_points(raw)
      if (is.null(df)) return(NULL)
      df$source_lbl  <- src_label(df$source)
      df$analyte_lbl <- ifelse(
        df$feature_lbl == NONE_FEAT,
        df$antigen_lbl,
        paste0(df$antigen_lbl, " | ", df$feature_lbl))
      df$plate_f <- order_plate_factor(df$plate_lbl)
      df
    })

    # ---- test samples for the selected analyte (Sources tab overlay) -------
    # Keep only patientid/timeperiod series measured at >1 distinct dilution on a
    # given plate -- the dilution series worth joining and labelling.
    sources_samples <- shiny::reactive({
      s <- scope()
      shiny::req(s$study, s$experiment, s$project_id,
                 input$sources_antigen, input$sources_feature)
      reload_trigger()

      raw <- tryCatch(
        fetch_raw_sample(pool, project = s$project_id,
                         study = s$study, experiment = s$experiment),
        error = function(e) { message("[plate-dilution] sample fetch failed: ",
                                       conditionMessage(e)); NULL })
      smp <- prep_points(raw)
      if (is.null(smp)) return(NULL)
      if (!all(c("patientid", "timeperiod") %in% names(smp))) return(NULL)

      smp <- smp[smp$antigen_lbl == input$sources_antigen &
                 smp$feature_lbl == input$sources_feature, , drop = FALSE]
      if (!nrow(smp)) return(NULL)

      smp$sample_lbl <- paste0(as.character(smp$patientid), " | ",
                               as.character(smp$timeperiod))
      # >1 distinct dilution, evaluated per (patient/timeperiod, plate)
      key  <- paste(smp$sample_lbl, smp$plate_lbl, sep = "\r")
      ndil <- tapply(smp$dil_num, key, function(v) length(unique(v)))
      keep <- names(ndil)[ndil >= 2]
      smp  <- smp[key %in% keep, , drop = FALSE]
      if (!nrow(smp)) return(NULL)
      smp
    })

    # ---- shared facet-plot builder (SOURCES TAB ONLY -- read-only) --------
    # df        : prepped standards (already filtered for the sub-tab)
    # group_col : column to colour/trace standards on ("analyte_lbl"/"source_lbl")
    # group_lab : legend title
    # samples   : optional prepped test-sample frame (Sources tab); overlaid as
    #             dashed traces keyed on $sample_lbl.
    pds_facet_plot <- function(df, group_col, group_lab,
                               samples = NULL, n_cols = N_COLS) {
      fmt <- function(x) format(x, trim = TRUE, scientific = FALSE)

      base <- data.frame(
        dil_num   = df$dil_num,
        mfi_num   = df$mfi_num,
        plate_lbl = as.character(df$plate_lbl),
        series    = as.character(df[[group_col]]),
        type      = "Standard",
        stringsAsFactors = FALSE)
      base$hover_txt <- paste0(
        group_lab, ": ", base$series,
        "<br>plate: ",    base$plate_lbl,
        "<br>dilution: ", fmt(base$dil_num),
        "<br>MFI: ",      fmt(base$mfi_num))

      has_samples <- !is.null(samples) && nrow(samples) > 0
      combined <- base
      if (has_samples) {
        samp <- data.frame(
          dil_num   = samples$dil_num,
          mfi_num   = samples$mfi_num,
          plate_lbl = as.character(samples$plate_lbl),
          series    = as.character(samples$sample_lbl),
          type      = "Test sample",
          stringsAsFactors = FALSE)
        samp$hover_txt <- paste0(
          "Test sample: ", samp$series,
          "<br>plate: ",    samp$plate_lbl,
          "<br>dilution: ", fmt(samp$dil_num),
          "<br>MFI: ",      fmt(samp$mfi_num))
        combined <- rbind(base, samp)
      }

      combined$plate_f <- order_plate_factor(combined$plate_lbl)
      combined <- combined[order(combined$plate_f, combined$type,
                                 combined$series, combined$dil_num), , drop = FALSE]

      # x decade breaks, natural-unit labels (no scientific notation)
      xpos <- combined$dil_num[is.finite(combined$dil_num) & combined$dil_num > 0]
      x_breaks <- if (length(xpos)) 10^(seq(floor(log10(min(xpos))),
                                            ceiling(log10(max(xpos)))))
                  else ggplot2::waiver()
      nat_lab <- function(v) formatC(v, format = "fg", big.mark = ",",
                                     drop0trailing = TRUE)

      pal <- scales::hue_pal()(max(length(unique(combined$series)), 1L))
      names(pal) <- unique(combined$series)

      mapping <- if (has_samples)
        ggplot2::aes(x = dil_num, y = mfi_num, colour = series,
                     group = interaction(series, type),
                     linetype = type, text = hover_txt)
      else
        ggplot2::aes(x = dil_num, y = mfi_num, colour = series,
                     group = series, text = hover_txt)

      g <- ggplot2::ggplot(combined, mapping) +
        ggplot2::geom_line(linewidth = 0.4, na.rm = TRUE) +
        ggplot2::geom_point(size = 1.1, na.rm = TRUE) +
        ggplot2::scale_x_log10(breaks = x_breaks, labels = nat_lab,
                               expand = ggplot2::expansion(mult = 0.04)) +
        ggplot2::scale_y_log10() +
        ggplot2::scale_colour_manual(values = pal, name = group_lab) +
        ggplot2::facet_wrap(~ plate_f, ncol = n_cols) +
        ggplot2::labs(x = "Dilution Fraction", y = "antibody_mfi",
                      title = "Dilution series by plate") +
        ggplot2::theme_bw(base_size = 11) +
        ggplot2::theme(
          panel.grid.minor = ggplot2::element_blank(),
          strip.background = ggplot2::element_rect(fill = "#eef2f7", colour = NA),
          strip.text       = ggplot2::element_text(face = "bold", size = 9),
          axis.text.x      = ggplot2::element_text(angle = 60, hjust = 1),
          legend.title     = ggplot2::element_text(size = 10),
          plot.title       = ggplot2::element_text(size = 13, face = "bold"))
      if (has_samples)
        g <- g + ggplot2::scale_linetype_manual(
          values = c("Standard" = "solid", "Test sample" = "dashed"),
          name = NULL)

      p <- plotly::ggplotly(g, tooltip = "text")
      # tilt tick labels 60 deg on EVERY facet x-axis (ggplotly makes xaxis,
      # xaxis2, ...); belt-and-suspenders over the theme angle above.
      ax <- grep("^xaxis", names(p$x$layout), value = TRUE)
      if (!length(ax)) ax <- "xaxis"
      for (nm in ax) p$x$layout[[nm]]$tickangle <- -60
      plotly::layout(p, margin = list(t = 50, b = 95),
                     legend = list(title = list(text = group_lab)))
    }

    # facet grid -> pixel height (rows * per-row + padding for title/legend)
    grid_height_px <- function(n_plates, n_cols = N_COLS,
                               per_row = 250, pad = 150) {
      n_rows <- max(ceiling(n_plates / n_cols), 1L)
      n_rows * per_row + pad
    }

    # =====================================================================
    # ANALYTES sub-tab: source selector -> trace per antigen+feature, with
    # click-to-mask / double-click-to-unmask (see mask_ui_helpers.R for the
    # shared scope/keep-fits/dry-run pieces this reuses).
    # =====================================================================
    output$analytes_source_ui <- shiny::renderUI({
      df <- std_prepped()
      if (is.null(df)) return(shiny::helpText("No standard-curve data."))
      srcs <- sort(unique(df$source_lbl))
      keep <- shiny::isolate(input$analytes_source)
      sel  <- if (!is.null(keep) && keep %in% srcs) keep else srcs[[1]]
      shiny::selectInput(ns("analytes_source"), NULL,
                         choices = srcs, selected = sel)
    })

    analytes_data <- shiny::reactive({
      df <- std_prepped(); shiny::req(df, input$analytes_source)
      df[df$source_lbl == input$analytes_source, , drop = FALSE]
    })

    output$analytes_status <- shiny::renderUI({
      df <- analytes_data()
      if (is.null(df) || !nrow(df))
        return(shiny::div(class = "alert alert-warning",
                          "No usable standard points for this source."))
      shiny::div(style = "margin-bottom:6px;color:#555;",
        sprintf("%d plate(s) \u00b7 %d antigen+feature trace(s) \u00b7 source: %s",
                length(unique(df$plate_f)),
                length(unique(df$analyte_lbl)),
                input$analytes_source))
    })

    # ---- point identity -----------------------------------------------
    # Every point already carries its full raw NK (project_id, study_accession,
    # experiment_accession, plateid, plate, nominal_sample_dilution, source,
    # wavelength, antigen, feature) and masked/mask_reason straight from
    # xmap_standard (fetch_raw_standard() is SELECT *), so no extra query is
    # needed to know what's masked -- only to resolve a CLICKED point's
    # curve_id, done lazily per click (resolve_click_curve_id()), never at
    # render time.
    #
    # pt_key disambiguates a point within the CURRENT (single-source) plot:
    # plate + well + dilution alone can repeat across analytes (the same well
    # is read for every antigen/feature on that plate), so the key includes
    # analyte_lbl too. This is a DIFFERENT, richer format than the "std|well|
    # dilution" shape build_mask_change_plan() expects -- see
    # analytes_to_mask_keys() below for the translation, done only once a
    # batch's points are all confirmed to share one curve_id.
    analytes_points <- shiny::reactive({
      df <- analytes_data(); shiny::req(df, nrow(df) > 0)
      df$pt_key <- paste("std", as.character(df$plate_lbl), as.character(df$well),
                        as.character(df$dilution), as.character(df$analyte_lbl),
                        sep = "\u0001")
      df
    })

    # Resolve ONE point's curve_id via resolve_curve_id() (calib_data_access.R),
    # AFTER replicating the exact sentinel substitution curve_lookup was built
    # with (curve_lookup_functions.R's pull_as_char(): every NK column except
    # project_id is TEXT, and a missing/blank value is stored as the literal
    # STRING "__none__" -- never an actual SQL NULL). Passing xmap_standard's
    # raw value straight through (e.g. a genuinely NA wavelength on a
    # bead-array run, or a blank source/feature) sends an actual NULL
    # parameter, and `NULL IS NOT DISTINCT FROM '__none__'` is FALSE -- it
    # never matches, which is why every click failed, not just some.
    #
    # Falls back to matching WITHOUT wavelength/nominal_sample_dilution if the
    # full match still misses. Those two are the columns most likely to have
    # representation drift between xmap_standard's typed value and the
    # as.character() snapshot curve_lookup took at upload time -- the same
    # class of issue resolve_std_mask_ids() already works around for
    # `dilution`, by design never requiring it to string-match exactly.
    resolve_click_curve_id <- function(row) {
      sentinel_char <- function(x) {
        x <- as.character(x)
        if (length(x) != 1 || is.na(x) || trimws(x) == "") "__none__" else x
      }
      nk <- list(
        project_id              = as.integer(row$project_id),
        study_accession         = sentinel_char(row$study_accession),
        experiment_accession    = sentinel_char(row$experiment_accession),
        plateid                 = sentinel_char(row$plateid),
        plate                   = sentinel_char(row$plate),
        nominal_sample_dilution = sentinel_char(row$nominal_sample_dilution),
        source                  = sentinel_char(row$source),
        wavelength              = sentinel_char(row$wavelength),
        antigen                 = sentinel_char(row$antigen),
        feature                 = sentinel_char(row$feature))
      cid <- tryCatch(resolve_curve_id(pool, nk), error = function(e) NA)
      if (!is.na(cid)) return(cid)

      # Fallback: identical match minus the two representation-risky columns.
      tryCatch({
        res <- .calib_q(pool, sprintf(
          "SELECT curve_id FROM %s
            WHERE project_id = $1 AND study_accession = $2 AND experiment_accession = $3
              AND plateid = $4 AND plate = $5 AND antigen = $6 AND feature = $7 AND source = $8",
          .tbl(TBL_CURVE_LOOKUP)),
          params = list(nk$project_id, nk$study_accession, nk$experiment_accession,
                       nk$plateid, nk$plate, nk$antigen, nk$feature, nk$source))
        if (nrow(res) >= 1) res$curve_id[1] else NA
      }, error = function(e) NA)
    }

    # Translate a set of pt_keys (all confirmed to share one curve_id, see the
    # click handlers) into the "std|well|dilution" shape
    # build_mask_change_plan()/resolve_std_mask_ids() expect. No blanks on this
    # tab -- the Analytes plot never shows them (fetch_raw_standard() only), so
    # every key is a standard.
    analytes_to_mask_keys <- function(keys) {
      if (!length(keys)) return(character(0))
      df <- analytes_points()
      rows <- df[match(keys, df$pt_key), , drop = FALSE]
      paste("std", as.character(rows$well), as.character(rows$dilution), sep = "|")
    }

    # Human-readable line per staged key for the selection lists below.
    analytes_pretty_keys <- function(keys) {
      df <- analytes_points()
      rows <- df[match(keys, df$pt_key), , drop = FALSE]
      sprintf("plate %s | well %s | dil %s | %s",
             rows$plate_lbl, rows$well, rows$dilution, rows$analyte_lbl)
    }

    # Keys of points CURRENTLY masked, straight from the LIVE xmap_standard
    # flag already on analytes_points() -- no calib_standards/calib_blanks
    # snapshot involved on this tab at all, so there is no staleness lag in
    # what renders as masked/unmasked here (unlike Explore-fits' fit LINE,
    # which can lag behind a deferred recompute -- see the stale banner below
    # for that).
    analytes_masked_keys <- shiny::reactive({
      df <- analytes_points()
      m <- as.logical(df$masked) %in% TRUE
      if (!any(m)) return(character(0))
      unique(df$pt_key[m])
    })

    # ---- staging: a batch is pinned to ONE curve_id -----------------------
    a_staged     <- shiny::reactiveVal(character(0))  # keys staged to MASK
    a_unstaged   <- shiny::reactiveVal(character(0))  # keys staged to UNMASK
    a_active_cid <- shiny::reactiveVal(NULL)           # the ONE curve_id this batch is pinned to
    a_highlight_set   <- shiny::reactiveVal(character(0))
    a_unhighlight_set <- shiny::reactiveVal(character(0))
    a_highlight_tick  <- shiny::reactiveVal(0)

    # active_cid is only released once BOTH staged sets are empty -- e.g.
    # finishing (saving) just the mask side while an unmask batch for the SAME
    # curve is still pending must NOT free the pin for a different curve.
    a_maybe_clear_active <- function() {
      if (!length(a_staged()) && !length(a_unstaged())) a_active_cid(NULL)
    }
    a_bump         <- function() a_highlight_tick(a_highlight_tick() + 1)
    a_clear_mask   <- function() { a_staged(character(0));   a_highlight_set(character(0));   a_bump(); a_maybe_clear_active() }
    a_clear_unmask <- function() { a_unstaged(character(0)); a_unhighlight_set(character(0)); a_bump(); a_maybe_clear_active() }
    a_reset_stage  <- function() {
      a_staged(character(0));   a_highlight_set(character(0))
      a_unstaged(character(0)); a_unhighlight_set(character(0))
      a_active_cid(NULL)
      a_bump()
    }
    # Changing the source selector clears staging, same as changing curve/
    # method does on the Explore-fits tab -- a staged batch only makes sense
    # for the source it was built under.
    shiny::observeEvent(input$analytes_source, a_reset_stage(), ignoreInit = TRUE)

    # A single click toggles an INCLUDED point in/out of the mask-staged set.
    # Masked points are ignored here -- unmasked via double-click instead, so
    # the two gestures never fight over the same point.
    shiny::observeEvent(plotly::event_data("plotly_click", source = ns("analytes_plot")), {
      ev <- tryCatch(plotly::event_data("plotly_click", source = ns("analytes_plot")),
                     error = function(e) NULL)
      cd <- if (is.data.frame(ev) && "customdata" %in% names(ev)) ev$customdata else NULL
      cd <- as.character(cd); cd <- cd[!is.na(cd) & nzchar(cd)]
      if (!length(cd)) return()
      key <- cd[[1]]
      if (key %in% analytes_masked_keys()) return()   # masked -> unmask via double-click

      cur <- a_staged()
      if (key %in% cur) { a_staged(setdiff(cur, key)); a_maybe_clear_active(); return() }

      df <- analytes_points(); row <- df[df$pt_key == key, , drop = FALSE][1, ]
      if (!nrow(row) || is.na(row$pt_key[1])) return()
      cid <- resolve_click_curve_id(row)
      if (is.na(cid)) {
        shiny::showNotification(
          "Couldn't resolve this point to a registered curve (it may be curve-level masked).",
          type = "error", duration = 6)
        return()
      }
      active <- a_active_cid()
      if (!is.null(active) && !identical(as.character(active), as.character(cid))) {
        shiny::showNotification(
          "You're already staging masking for a different plate/antigen. Save or clear that batch before starting another.",
          type = "warning", duration = 6)
        return()
      }
      if (is.null(active)) a_active_cid(cid)
      a_staged(c(cur, key))
    }, ignoreInit = TRUE)

    # A DOUBLE click toggles a MASKED point in/out of the unmask-staged set.
    # The onRender shim (attach_mask_click_shim(), mask_ui_helpers.R) fires
    # input$analytes_pt_dblclick = list(key, nonce) only on a genuine
    # double-click on a point.
    shiny::observeEvent(input$analytes_pt_dblclick, {
      ev <- input$analytes_pt_dblclick
      cd <- if (is.list(ev)) ev$key else ev
      cd <- as.character(cd); cd <- cd[!is.na(cd) & nzchar(cd)]
      if (!length(cd)) return()
      key <- cd[[1]]
      if (!(key %in% analytes_masked_keys())) return()  # only masked points can be unmasked

      cur <- a_unstaged()
      if (key %in% cur) { a_unstaged(setdiff(cur, key)); a_maybe_clear_active(); return() }

      df <- analytes_points(); row <- df[df$pt_key == key, , drop = FALSE][1, ]
      if (!nrow(row) || is.na(row$pt_key[1])) return()
      cid <- resolve_click_curve_id(row)
      if (is.na(cid)) {
        shiny::showNotification(
          "Couldn't resolve this point to a registered curve (it may be curve-level masked).",
          type = "error", duration = 6)
        return()
      }
      active <- a_active_cid()
      if (!is.null(active) && !identical(as.character(active), as.character(cid))) {
        shiny::showNotification(
          "You're already staging masking for a different plate/antigen. Save or clear that batch before starting another.",
          type = "warning", duration = 6)
        return()
      }
      if (is.null(active)) a_active_cid(cid)
      a_unstaged(c(cur, key))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$analytes_mask_clear,   a_clear_mask(),   ignoreInit = TRUE)
    shiny::observeEvent(input$analytes_unmask_clear, a_clear_unmask(), ignoreInit = TRUE)
    shiny::observeEvent(input$analytes_mask_highlight, {
      a_highlight_set(a_staged()); a_highlight_tick(a_highlight_tick() + 1)
    })
    shiny::observeEvent(input$analytes_unmask_highlight, {
      a_unhighlight_set(a_unstaged()); a_highlight_tick(a_highlight_tick() + 1)
    })

    # ---- staged-selection lists + Highlight/Clear/Save controls -----------
    output$analytes_mask_selection <- shiny::renderUI({
      sel <- a_staged()
      if (!length(sel)) return(NULL)  # the wellPanel helpText already explains clicking
      pretty <- analytes_pretty_keys(sel)
      hs <- a_highlight_set()
      hint <- if (!length(hs)) {
        "Not highlighted on plot yet \u2014 press \u201cHighlight selected\u201d to ring them."
      } else if (!setequal(hs, sel)) {
        sprintf("Plot shows an OLDER highlight (%d point(s)); selection changed \u2014 press \u201cHighlight selected\u201d to refresh.",
                length(hs))
      } else "Plot highlight is current."
      shiny::tagList(
        shiny::tags$strong(sprintf("%d point(s) staged to mask:", length(sel))),
        shiny::tags$ul(lapply(pretty, shiny::tags$li)),
        shiny::tags$div(style = "color:#787878;font-size:11px;margin-bottom:6px;", hint),
        shiny::div(
          shiny::actionButton(ns("analytes_mask_highlight"), "Highlight selected",
                              class = "btn-default btn-sm"),
          shiny::actionButton(ns("analytes_mask_clear"), "Clear selection",
                              class = "btn-default btn-sm"),
          shiny::span(style = "float:right;",
            shiny::actionButton(ns("analytes_mask_save"), "Save / Apply mask",
                                class = "btn-warning btn-sm"))
        )
      )
    })

    output$analytes_unmask_selection <- shiny::renderUI({
      sel <- a_unstaged()
      if (!length(sel)) return(NULL)
      pretty <- analytes_pretty_keys(sel)
      hs <- a_unhighlight_set()
      hint <- if (!length(hs)) {
        "Not highlighted on plot yet \u2014 press \u201cHighlight selected\u201d to ring them."
      } else if (!setequal(hs, sel)) {
        sprintf("Plot shows an OLDER highlight (%d point(s)); selection changed \u2014 press \u201cHighlight selected\u201d to refresh.",
                length(hs))
      } else "Plot highlight is current."
      shiny::tagList(
        shiny::tags$strong(sprintf("%d point(s) staged to UNMASK:", length(sel))),
        shiny::tags$ul(lapply(pretty, shiny::tags$li)),
        shiny::tags$div(style = "color:#787878;font-size:11px;margin-bottom:6px;", hint),
        shiny::div(
          shiny::actionButton(ns("analytes_unmask_highlight"), "Highlight selected",
                              class = "btn-default btn-sm"),
          shiny::actionButton(ns("analytes_unmask_clear"), "Clear selection",
                              class = "btn-default btn-sm"),
          shiny::span(style = "float:right;",
            shiny::actionButton(ns("analytes_unmask_save"), "Unmask",
                                class = "btn-warning btn-sm"))
        )
      )
    })

    # ---- MASK modal: reason + scope + keep-fits + dry-run (no writes yet) --
    shiny::observeEvent(input$analytes_mask_save, {
      if (!length(a_staged())) return()
      shiny::showModal(shiny::modalDialog(
        title = "Apply mask",
        shiny::textAreaInput(ns("analytes_mask_reason_txt"),
          "Reason (required) \u2014 applies to all points in this save",
          placeholder = "e.g. plate-edge contamination; implausible replicate", rows = 2),
        mask_scope_input(ns, "analytes_mask_scope"),
        mask_keep_fits_input(ns, "analytes_mask_keep_fits"),
        shiny::uiOutput(ns("analytes_mask_dryrun")),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("analytes_mask_apply"), "Apply mask",
                              class = "btn-danger")),
        easyClose = FALSE, size = "l"))
    })

    a_mask_plan <- shiny::reactive({
      build_mask_change_plan(pool, analytes_to_mask_keys(a_staged()), a_active_cid(),
                             input$analytes_mask_scope %||% "antigen")
    })

    output$analytes_mask_dryrun <- shiny::renderUI({
      pl <- a_mask_plan(); if (is.null(pl)) return(NULL)
      mask_dryrun_body(pl, isTRUE(input$analytes_mask_keep_fits))
    })

    shiny::observeEvent(input$analytes_mask_apply, {
      reason <- trimws(input$analytes_mask_reason_txt %||% "")
      if (!nzchar(reason)) {
        shiny::showNotification("A reason is required to apply a mask.",
                                type = "error", duration = NULL)
        return()
      }
      pl <- a_mask_plan()
      if (is.null(pl) || !length(pl$std_ids)) {
        shiny::showNotification("Nothing resolved to mask.", type = "error", duration = NULL); return()
      }
      keep <- isTRUE(input$analytes_mask_keep_fits)
      res <- tryCatch(
        apply_mask(pool, std_ids = pl$std_ids, blk_ids = integer(0),
                  group_curve_ids = pl$grp, reason = reason, set_masked = TRUE,
                  delete_fits = !keep),
        error = function(e) { shiny::showNotification(conditionMessage(e),
                                type = "error", duration = NULL); NULL })
      if (is.null(res)) return()
      shiny::removeModal()
      a_clear_mask()
      reload_trigger(reload_trigger() + 1)  # this tab's own raw-data refresh
      calib_dirty(calib_dirty() + 1)        # Explore-fits / Compute-fits stale refresh
      msg <- if (keep)
        sprintf("Masked %d point(s); kept existing fits and flagged %d curve(s) out of date. Submit a fit job on the Compute-fits tab when you're done batching masking edits.",
                res$masked_std, res$group_n)
      else
        sprintf("Masked %d point(s); deleted fits for %d curve(s). Recompute on the Compute-fits tab to get a revised fit.",
                res$masked_std, res$group_n)
      shiny::showNotification(msg, type = "message", duration = 10)
    })

    # ---- UNMASK modal: scope + keep-fits + dry-run (no writes yet) --------
    shiny::observeEvent(input$analytes_unmask_save, {
      if (!length(a_unstaged())) return()
      shiny::showModal(shiny::modalDialog(
        title = "Restore (unmask) points",
        shiny::p(paste("Unmasking returns these points to the fit. By default the existing fit",
                       "for the affected group(s) is deleted so it can be recomputed with the",
                       "points restored.")),
        mask_scope_input(ns, "analytes_unmask_scope"),
        mask_keep_fits_input(ns, "analytes_unmask_keep_fits"),
        shiny::uiOutput(ns("analytes_unmask_dryrun")),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("analytes_unmask_apply"), "Unmask",
                              class = "btn-danger")),
        easyClose = FALSE, size = "l"))
    })

    a_unmask_plan <- shiny::reactive({
      build_mask_change_plan(pool, analytes_to_mask_keys(a_unstaged()), a_active_cid(),
                             input$analytes_unmask_scope %||% "antigen")
    })

    output$analytes_unmask_dryrun <- shiny::renderUI({
      pl <- a_unmask_plan(); if (is.null(pl)) return(NULL)
      mask_dryrun_body_unmask(pl, isTRUE(input$analytes_unmask_keep_fits))
    })

    shiny::observeEvent(input$analytes_unmask_apply, {
      pl <- a_unmask_plan()
      if (is.null(pl) || !length(pl$std_ids)) {
        shiny::showNotification("Nothing resolved to unmask.", type = "error", duration = NULL); return()
      }
      keep <- isTRUE(input$analytes_unmask_keep_fits)
      res <- tryCatch(
        apply_unmask(pool, std_ids = pl$std_ids, blk_ids = integer(0),
                    group_curve_ids = pl$grp, delete_fits = !keep),
        error = function(e) { shiny::showNotification(conditionMessage(e),
                                type = "error", duration = NULL); NULL })
      if (is.null(res)) return()
      shiny::removeModal()
      a_clear_unmask()
      reload_trigger(reload_trigger() + 1)
      calib_dirty(calib_dirty() + 1)
      msg <- if (keep)
        sprintf("Unmasked %d point(s); kept existing fits and flagged %d curve(s) out of date. Submit a fit job on the Compute-fits tab when you're done batching masking edits.",
                res$unmasked_std, res$group_n)
      else
        sprintf("Unmasked %d point(s); deleted fits for %d curve(s). Recompute on the Compute-fits tab to get a revised fit.",
                res$unmasked_std, res$group_n)
      shiny::showNotification(msg, type = "message", duration = 10)
    })

    # ---- stale-fit banner: 5(a) -- one banner around the whole figure, ----
    # listing every antigen (within the currently displayed source) that has
    # at least one curve flagged stale (calib_recalc_flag, via
    # fetch_calc_status_scoped()'s needs_recalc column -- the SAME aggregated,
    # already-NK-safe source of truth the Compute-fits status table uses, so
    # there's no separate per-row curve_id resolution needed just to render
    # this). Depends on calib_dirty() (bumped by a mask/unmask save on EITHER
    # this tab or Explore-fits) and reload_trigger().
    a_stale_antigens <- shiny::reactive({
      s <- scope(); shiny::req(s$study, s$experiment, s$project_id, input$analytes_source)
      reload_trigger(); calib_dirty()
      cs <- tryCatch(fetch_calc_status_scoped(pool, s$project_id, s$study, s$experiment),
                    error = function(e) NULL)
      if (is.null(cs) || !nrow(cs)) return(character(0))
      # Match the CURRENT source selection the SAME way analytes_data() does
      # (via src_label()), rather than guessing the raw sentinel's exact form.
      cs$source_lbl <- src_label(cs$source)
      cs <- cs[cs$source_lbl == input$analytes_source & cs$needs_recalc %in% TRUE, , drop = FALSE]
      if (!nrow(cs)) return(character(0))
      analyte <- ifelse(feat_label(cs$feature) == NONE_FEAT, as.character(cs$antigen),
                        paste0(as.character(cs$antigen), " | ", feat_label(cs$feature)))
      sort(unique(analyte))
    })

    output$analytes_stale_banner <- shiny::renderUI({
      stale <- a_stale_antigens(); if (!length(stale)) return(NULL)
      shiny::tags$div(
        style = paste("border: 4px solid #D32F2F; border-radius: 4px; padding: 8px;",
                      "background: #FFF5F5; margin-bottom: 8px;"),
        shiny::tags$div(style = "color:#D32F2F;font-weight:bold;font-size:14px;margin-bottom:4px;",
          "\u26a0 FITS OUT OF DATE \u2014 masking changes are pending recalculation (frequentist and Bayesian)."),
        shiny::tags$div(style = "color:#a33;font-size:12px;",
          sprintf("Affected antigen%s: %s", if (length(stale) == 1) "" else "s",
                 paste(stale, collapse = ", "))),
        shiny::tags$div(style = "color:#a33;font-size:11px;margin-top:2px;",
          "Submit a fit job on the Compute-fits tab to refresh."))
    })

    # ---- the plot itself: plotly::plot_ly() + plotly::subplot(), one panel
    # per plate. Built directly (NOT ggplot2/ggplotly) so every point carries
    # real customdata for the click shim above. Depends only on the DATA and
    # the explicit "Highlight selected" snapshots -- NOT on a_staged()/
    # a_unstaged() directly -- so a click never forces a full re-render of a
    # (potentially large) multi-panel figure; only pressing "Highlight
    # selected" does.
    #
    # Panels share the SAME x/y range and decade ticks (computed once over the
    # WHOLE filtered source, not per plate) so relative position across plates
    # stays comparable -- the reason this view exists. Each panel keeps its
    # OWN axis object (shareX/shareY = FALSE) and carries the plate name as
    # its y-axis title; this is more repetitive than a single shared strip
    # header would be, but avoids hand-computed per-panel annotation
    # placement, which is easy to get subtly wrong. Legend entries are
    # deduplicated (legendgroup + showlegend) so each antigen+feature shows
    # once no matter how many plates it appears on.
    analytes_plot_ly <- function(df, highlight_keys = character(0),
                                 unhighlight_keys = character(0)) {
      df$log_x <- log10(df$dil_num)
      df$log_y <- log10(df$mfi_num)
      df <- df[is.finite(df$log_x) & is.finite(df$log_y), , drop = FALSE]
      if (!nrow(df)) return(NULL)

      dec_label <- function(v) {
        if (v >= 1e5 || v < 1e-2) formatC(v, format = "g", digits = 2)
        else formatC(v, format = "fg", big.mark = ",", drop0trailing = TRUE)
      }
      decade_axis <- function(vals, title, tickangle = 0) {
        lo <- min(vals); hi <- max(vals); span <- max(hi - lo, 0.5); pad <- 0.06 * span
        decs <- seq(floor(lo), ceiling(hi))
        if (!length(decs)) decs <- round(c(lo, hi))
        list(range = c(lo - pad, hi + pad), tickmode = "array",
             tickvals = as.list(decs),
             ticktext = as.list(vapply(decs, function(k) dec_label(10^k), character(1))),
             tickangle = tickangle, title = title, zeroline = FALSE)
      }
      xax_shared <- decade_axis(df$log_x, "Dilution Fraction", tickangle = -60)

      pal <- scales::hue_pal()(max(length(unique(df$analyte_lbl)), 1L))
      names(pal) <- sort(unique(df$analyte_lbl))

      plates  <- levels(df$plate_f)
      if (!length(plates)) plates <- sort(unique(as.character(df$plate_lbl)))
      n_cols  <- max(min(N_COLS, length(plates)), 1L)
      n_rows  <- max(ceiling(length(plates) / n_cols), 1L)

      seen_legend <- character(0)  # antigen labels already shown in the legend
      panels <- lapply(plates, function(pl) {
        d <- df[as.character(df$plate_lbl) == pl, , drop = FALSE]
        if (!nrow(d)) return(plotly::plot_ly(source = ns("analytes_plot")))
        d <- d[order(d$analyte_lbl, d$log_x), , drop = FALSE]
        yax <- decade_axis(df$log_y, paste0(pl, " \u2014 antibody_mfi"))
        # source MUST match the id plotly::event_data("plotly_click", source = ...)
        # queries below -- every panel is tagged with the SAME source (mirrors
        # std_curve_view_module.R's single-panel plot_ly(source = ...) exactly;
        # here it's per-panel because each panel is its own plot_ly() call before
        # plotly::subplot() merges them). p$x$source is also forced explicitly on
        # the merged widget below as a second, redundant guarantee.
        p <- plotly::plot_ly(source = ns("analytes_plot"))
        for (an in sort(unique(d$analyte_lbl))) {
          dd <- d[d$analyte_lbl == an, , drop = FALSE]
          masked_now <- as.logical(dd$masked) %in% TRUE
          show_leg <- !(an %in% seen_legend)
          seen_legend <<- union(seen_legend, an)
          reason_txt <- ifelse(masked_now,
            ifelse(!is.na(dd$mask_reason) & nzchar(dd$mask_reason),
                  paste0(" \u2014 MASKED: ", dd$mask_reason), " \u2014 MASKED"), "")
          hover <- sprintf("%s<br>plate: %s<br>well: %s<br>dilution: %s<br>MFI: %s%s",
                           an, pl, dd$well, format(dd$dilution, trim = TRUE),
                           format(dd$mfi_num, trim = TRUE, big.mark = ","), reason_txt)
          p <- plotly::add_trace(p, x = dd$log_x, y = dd$log_y,
                                 type = "scatter", mode = "lines+markers",
                                 name = an, legendgroup = an, showlegend = show_leg,
                                 line = list(color = pal[[an]], width = 1),
                                 marker = list(color = pal[[an]], size = 8,
                                               symbol = ifelse(masked_now, "circle-open", "circle"),
                                               line = list(color = pal[[an]], width = 1.5)),
                                 customdata = dd$pt_key, hovertext = hover, hoverinfo = "text")
        }
        # Ring overlay for staged/highlighted points -- a separate, legend-
        # free trace drawn on top, mirroring the Explore-fits pattern.
        hi <- d[d$pt_key %in% highlight_keys, , drop = FALSE]
        if (nrow(hi))
          p <- plotly::add_markers(p, x = hi$log_x, y = hi$log_y,
                                   marker = list(color = "rgba(0,0,0,0)", size = 15,
                                                 line = list(color = "#E31A1C", width = 3)),
                                   customdata = hi$pt_key, showlegend = FALSE, hoverinfo = "skip")
        un <- d[d$pt_key %in% unhighlight_keys, , drop = FALSE]
        if (nrow(un))
          p <- plotly::add_markers(p, x = un$log_x, y = un$log_y,
                                   marker = list(color = "rgba(0,0,0,0)", size = 15,
                                                 line = list(color = "#1F78B4", width = 3)),
                                   customdata = un$pt_key, showlegend = FALSE, hoverinfo = "skip")
        plotly::layout(p, xaxis = xax_shared, yaxis = yax)
      })

      p <- plotly::subplot(panels, nrows = n_rows, shareX = FALSE, shareY = FALSE,
                           titleX = TRUE, titleY = TRUE,
                           margin = c(0.03, 0.03, 0.08, 0.05))
      p <- plotly::layout(p, showlegend = TRUE,
                          legend = list(title = list(text = "Antigen | feature")),
                          margin = list(t = 30, b = 10))
      # Belt-and-suspenders: force the SAME source directly onto the merged
      # widget, regardless of whether plotly::subplot() propagates the
      # per-panel `source` set above. This is the single attribute
      # plotly::event_data("plotly_click", source = ns("analytes_plot"))
      # actually keys off of; without it matching exactly, clicks render
      # and hover fine (those don't depend on `source`) but never reach the
      # Shiny click observer at all.
      p$x$source <- ns("analytes_plot")
      attach_mask_click_shim(p, ns("analytes_pt_dblclick"))
    }

    output$analytes_plot_ui <- shiny::renderUI({
      df <- analytes_data(); shiny::req(df, nrow(df) > 0)
      h <- grid_height_px(length(unique(df$plate_f)))
      plotly::plotlyOutput(ns("analytes_plot"), height = paste0(h, "px"))
    })

    output$analytes_plot <- plotly::renderPlotly({
      df <- analytes_points(); shiny::req(df, nrow(df) > 0)
      analytes_plot_ly(df, highlight_keys = a_highlight_set(),
                       unhighlight_keys = a_unhighlight_set())
    })

    # =====================================================================
    # SOURCES sub-tab: antigen + feature selectors -> trace per source,
    #                  plus multi-dilution test-sample overlay. Read-only,
    #                  unchanged (see the module header for why).
    # =====================================================================
    output$sources_antigen_ui <- shiny::renderUI({
      df <- std_prepped()
      if (is.null(df)) return(shiny::helpText("No standard-curve data."))
      ags  <- sort(unique(df$antigen_lbl))
      keep <- shiny::isolate(input$sources_antigen)
      sel  <- if (!is.null(keep) && keep %in% ags) keep else ags[[1]]
      shiny::selectInput(ns("sources_antigen"), "Antigen",
                         choices = ags, selected = sel)
    })

    # feature choices depend on the chosen antigen
    output$sources_feature_ui <- shiny::renderUI({
      df <- std_prepped(); shiny::req(df, input$sources_antigen)
      feats <- sort(unique(df$feature_lbl[df$antigen_lbl == input$sources_antigen]))
      if (!length(feats)) return(NULL)
      keep <- shiny::isolate(input$sources_feature)
      sel  <- if (!is.null(keep) && keep %in% feats) keep else feats[[1]]
      shiny::selectInput(ns("sources_feature"), "Feature",
                         choices = feats, selected = sel)
    })

    sources_data <- shiny::reactive({
      df <- std_prepped()
      shiny::req(df, input$sources_antigen, input$sources_feature)
      df[df$antigen_lbl  == input$sources_antigen &
         df$feature_lbl == input$sources_feature, , drop = FALSE]
    })

    output$sources_status <- shiny::renderUI({
      df <- sources_data()
      if (is.null(df) || !nrow(df))
        return(shiny::div(class = "alert alert-warning",
                          "No usable standard points for this antigen + feature."))
      smp      <- sources_samples()
      n_series <- if (is.null(smp)) 0L else length(unique(smp$sample_lbl))
      samp_txt <- if (n_series > 0)
                    sprintf(" \u00b7 %d multi-dilution test-sample series", n_series)
                  else " \u00b7 no multi-dilution test samples"
      shiny::div(style = "margin-bottom:6px;color:#555;",
        sprintf("%d plate(s) \u00b7 %d source trace(s)%s \u00b7 %s | %s",
                length(unique(df$plate_f)),
                length(unique(df$source_lbl)),
                samp_txt, input$sources_antigen, input$sources_feature))
    })

    output$sources_plot_ui <- shiny::renderUI({
      df <- sources_data(); shiny::req(df, nrow(df) > 0)
      smp    <- sources_samples()
      plates <- unique(c(as.character(df$plate_lbl),
                         if (!is.null(smp)) as.character(smp$plate_lbl)))
      h <- grid_height_px(length(plates))
      plotly::plotlyOutput(ns("sources_plot"), height = paste0(h, "px"))
    })

    output$sources_plot <- plotly::renderPlotly({
      df <- sources_data(); shiny::req(df, nrow(df) > 0)
      pds_facet_plot(df, "source_lbl", "Source", samples = sources_samples())
    })

    invisible(NULL)
  })
}
