# =============================================================================
# plate_dilution_series_module.R  --  "Plate Dilution Series" QC tab.
#
# Reproduces the faceted "Dilution Series by Plate" figure from the analysis
# notebooks: one facet per plate, log-log axes (x = dilution, y = antibody_mfi),
# and one line+marker trace per group, joining the raw standard-curve points
# (rows of madi_results.xmap_standard) across the dilution series.
#
# Masked standard points are INTENTIONALLY retained: masking happens at the next
# QC layer. This overview exists precisely to spot the points that need
# consistent masking (e.g. well contamination) before that step.
#
# Two sub-tabs, both driving the SAME facet-by-plate grid:
#
#   * "Analytes" -- a Standard-curve SOURCE selector filters to a single source;
#                   each plate facet then shows one trace per antigen+feature
#                   (analyte) combination.
#
#   * "Sources"  -- ANTIGEN + FEATURE selectors filter to a single analyte; each
#                   plate facet then shows one trace per standard-curve source.
#                   In addition, any TEST SAMPLE (patientid / timeperiod) that
#                   was run at more than one dilution on a plate is overlaid as
#                   its own dashed trace, labelled by patientid + timeperiod --
#                   useful for reading optimization plates.
#
# X axis: log10-scaled, with decade tick marks labelled in natural units
# (e.g. 0.001, 0.01, 0.1, 1) tilted 60 degrees; titled "Dilution Fraction".
#
# Self-contained module. It does NOT touch the Bead Count or Standard Curve
# modules; it only reads raw standards / samples via fetch_raw_standard() /
# fetch_raw_sample() (defined in calib_data_access.R), like the Data tab does.
#
# Contract (mirrors the calib_* modules):
#   pool           : DBI/pool handle (db_pool)
#   scope          : reactive -> list(study, experiment, project_id)
#   reload_trigger : reactiveVal(int); bump to force a re-read (experiment change,
#                    Data-tab Refresh, mask save). Optional; defaults to a no-op.
# =============================================================================

# ---- UI ---------------------------------------------------------------------
plateDilutionSeriesUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$div(
      style = "margin:6px 0 10px;color:#555;",
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
                "shows one trace per antigen + feature."),
              shiny::uiOutput(ns("analytes_source_ui"))
            )
          ),
          shiny::column(
            width = 9,
            shiny::uiOutput(ns("analytes_status")),
            shinycssloaders::withSpinner(
              shiny::uiOutput(ns("analytes_plot_ui")),
              type = 4, color = "#337ab7")
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
                                      reload_trigger = shiny::reactiveVal(0)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

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
    # and drop rows that can't sit on log-log axes. Masked rows are KEPT.
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
    std_prepped <- shiny::reactive({
      s <- scope(); shiny::req(s$study, s$experiment, s$project_id)
      reload_trigger()  # dependency so Refresh / mask-save re-reads

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

    # ---- shared facet-plot builder ----------------------------------------
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
    # ANALYTES sub-tab: source selector -> trace per antigen+feature
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

    output$analytes_plot_ui <- shiny::renderUI({
      df <- analytes_data(); shiny::req(df, nrow(df) > 0)
      h <- grid_height_px(length(unique(df$plate_f)))
      plotly::plotlyOutput(ns("analytes_plot"), height = paste0(h, "px"))
    })

    output$analytes_plot <- plotly::renderPlotly({
      df <- analytes_data(); shiny::req(df, nrow(df) > 0)
      pds_facet_plot(df, "analyte_lbl", "Antigen | feature")
    })

    # =====================================================================
    # SOURCES sub-tab: antigen + feature selectors -> trace per source,
    #                  plus multi-dilution test-sample overlay
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
