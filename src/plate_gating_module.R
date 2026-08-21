# =============================================================================
# plate_gating_module.R  --  "Plates gating" comparison mode (sample QC by plate)
#
# Hosted by std_curve_compare_module.R: the Compare-fits tab keeps its
# "Comparison mode" radio; selecting "Plates gating" swaps the body to this
# module. Reproduces the three-figure gating panel from the analysis notebook,
# for ONE source / antigen / feature under ONE approach (method):
#
#   1. Gate composition by plate -- horizontal STACKED bar, % of samples per
#      gate on each plate (sums to 100%): In Range (green), Below LLOQ (blue),
#      Above ULOQ (orange).
#   2. MFI by plate -- one HALF-VIOLIN of the plate's MFI distribution plus the
#      individual sample dots, dots coloured by gate status (no box/whiskers).
#   3. Standard curves by plate -- raw dilution vs MFI, one trace per plate,
#      formatted like plate_dilution_series (log10 axes, natural-unit decade
#      labels tilted 60 deg, x titled "Dilution Fraction").
#
# GATE -- three-way, by where the sample sits relative to the per-curve/method
# limits (precision / pcov is NOT used; there is no out-of-range bucket):
#   LLOQ = dilution * lloq_conc ; ULOQ = dilution * uloq_conc
#   * When final_concentration is finite: classify it against LLOQ / ULOQ.
#   * When final_concentration is undefined or infinite (response off the curve):
#     classify the ASSAY RESPONSE (sample MFI) against the curve response at
#     LLOQ / ULOQ, interpolated from the standards (raw response, same scale).
#   Result: below_LLOQ / in_range / above_ULOQ.
#   Missing ONE limit opens that side: a point above a lone LLOQ (ULOQ missing),
#   or below a lone ULOQ (LLOQ missing), is in_range. No usable value, or both
#   limits missing -> unclassifiable, dropped (not a fourth category).
#
# Data: only functions already used by the compare / plate-dilution modules --
#   fetch_curve_batch, fetch_curve_lookup, fetch_calib_samples,
#   fetch_calib_diagnostics, fetch_calib_standards, fetch_raw_sample.
#
# Contract:
#   pool           : DBI/pool handle (db_pool)
#   scope          : reactive -> list(study, experiment, project_id)
#   reload_trigger : reactiveVal(int); optional, bump to force a re-read.
# =============================================================================

# gate ordering / display / colours (below/in/above LOQ only)
.PG_LEVELS <- c("in_range", "below_LLOQ", "above_ULOQ")
.PG_LABELS <- c(in_range = "In Range", below_LLOQ = "Below LLOQ",
                above_ULOQ = "Above ULOQ")
.PG_COLORS <- c(in_range = "#21be21", below_LLOQ = "#375ace",
                above_ULOQ = "#ffa500")
# figures 1 & 2 show exactly these three (== all gate levels)
.PG_SHOW <- c("in_range", "below_LLOQ", "above_ULOQ")

# ---- UI ---------------------------------------------------------------------
plateGatingUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 3,
        shiny::wellPanel(
          shiny::h4("Gating filters"),
          shiny::helpText(
            "Gate back-calculated test samples against each plate's LLOQ/ULOQ, ",
            "for one source / antigen / feature."),
          shiny::uiOutput(ns("source_ui")),
          shiny::uiOutput(ns("antigen_ui")),
          shiny::uiOutput(ns("feature_ui")),
          shiny::radioButtons(ns("approach"), "Approach",
            choices = c("Frequentist" = "frequentist", "Bayesian" = "bayesian"),
            selected = "frequentist", inline = TRUE)
        )
      ),
      shiny::column(
        width = 9,
        shiny::uiOutput(ns("status"))
      )
    ),
    shiny::hr(),
    shiny::fluidRow(
      shiny::column(
        width = 5,
        shiny::h4("Gate composition by plate"),
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("bar_plot"), height = "420px"),
          type = 4, color = "#337ab7")
      ),
      shiny::column(
        width = 7,
        shiny::h4("MFI by plate (gate status)"),
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("mfi_plot"), height = "420px"),
          type = 4, color = "#337ab7")
      )
    ),
    shiny::hr(),
    shiny::h4("Standard curves by plate"),
    shinycssloaders::withSpinner(
      plotly::plotlyOutput(ns("sc_plot"), height = "470px"),
      type = 4, color = "#337ab7")
  )
}

# ---- Server -----------------------------------------------------------------
plateGatingServer <- function(id, pool, scope,
                              reload_trigger = shiny::reactiveVal(0)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns  <- session$ns
    `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
    num <- function(x) suppressWarnings(as.numeric(as.character(x)))

    NONE_SRC  <- "(no source)"
    NONE_FEAT <- "(no feature)"

    src_label  <- function(x) { x <- as.character(x)
      ifelse(is.na(x) | x %in% c("__none__", ""), NONE_SRC, x) }
    feat_label <- function(x) { x <- as.character(x)
      ifelse(is.na(x) | x %in% c("__none__", ""), NONE_FEAT, x) }

    # key-part normalizers, shared by the two MFI-join key builders so
    # calib_samples <-> xmap_sample keys line up (id parts trimmed to text;
    # dilution parsed to a canonical numeric string so 100 == 100.0 == 1e2).
    .norm    <- function(x) trimws(as.character(x))
    .dilnorm <- function(x) {
      v <- suppressWarnings(as.numeric(as.character(x)))
      ifelse(is.na(v), trimws(as.character(x)), formatC(v, format = "g", digits = 12))
    }

    # Vectorized 3-way classifier for a value against a lower (LLOQ) and upper
    # (ULOQ) bound; bounds may be NA per element. With BOTH bounds present it's
    # the ordinary below/in/above split. With only ONE bound present the other
    # side is open, so anything on the inside of the present bound is in_range
    # (point above a lone LLOQ, or below a lone ULOQ). No usable value or no
    # bounds at all -> NA (dropped by the figures).
    pg_classify <- function(value, lo, hi) {
      n  <- length(value)
      lo <- rep_len(lo, n); hi <- rep_len(hi, n)
      out <- rep(NA_character_, n)
      fin    <- is.finite(value)
      has_lo <- is.finite(lo); has_hi <- is.finite(hi)
      both   <- fin & has_lo & has_hi
      onlylo <- fin & has_lo & !has_hi
      onlyhi <- fin & !has_lo & has_hi
      out[both & value <  lo]                 <- "below_LLOQ"
      out[both & value >= lo & value <= hi]   <- "in_range"
      out[both & value >  hi]                 <- "above_ULOQ"
      out[onlylo & value <  lo]               <- "below_LLOQ"
      out[onlylo & value >= lo]               <- "in_range"
      out[onlyhi & value >  hi]               <- "above_ULOQ"
      out[onlyhi & value <= hi]               <- "in_range"
      out
    }

    scope_val <- shiny::reactive(if (!is.null(scope)) scope()
                                 else list(study = NULL, experiment = NULL, project_id = NA))

    # plate label ordering: first integer in the label, then alphabetical.
    order_plate_factor <- function(plate_lbl) {
      plate_lbl <- as.character(plate_lbl)
      pint <- suppressWarnings(as.integer(stringr::str_extract(plate_lbl, "\\d+")))
      lev  <- unique(plate_lbl[order(pint, plate_lbl)])
      factor(plate_lbl, levels = lev)
    }

    # ---- curves in scope (curve_id + antigen/feature/plate/source) ----------
    curves <- shiny::reactive({
      s <- scope_val(); shiny::req(s$study, s$experiment)
      reload_trigger()
      b <- tryCatch(fetch_curve_batch(pool, s$study, s$experiment, s$project_id),
                    error = function(e) NULL)
      if (is.null(b) || !nrow(b)) return(NULL)
      lk   <- tryCatch(fetch_curve_lookup(pool), error = function(e) NULL)
      keep <- intersect(c("curve_id", "plateid", "plate", "source"), names(lk))
      if (!is.null(lk) && "curve_id" %in% keep)
        b <- merge(b, unique(lk[, keep, drop = FALSE]), by = "curve_id", all.x = TRUE)
      for (col in c("plateid", "plate", "source"))
        if (is.null(b[[col]])) b[[col]] <- NA
      b$source_lbl  <- src_label(b$source)
      b$feature_lbl <- feat_label(b$feature)
      b$antigen_lbl <- as.character(b$antigen)
      b$plate_lbl   <- ifelse(!is.na(b$plate), as.character(b$plate),
                              as.character(b$plateid))
      b
    })

    # ---- filter selectors ---------------------------------------------------
    output$source_ui <- shiny::renderUI({
      cv <- curves()
      if (is.null(cv)) return(shiny::helpText("No curves in scope yet."))
      srcs <- sort(unique(cv$source_lbl))
      keep <- shiny::isolate(input$pg_source)
      sel  <- if (!is.null(keep) && keep %in% srcs) keep else srcs[[1]]
      shiny::selectInput(ns("pg_source"), "Source", choices = srcs, selected = sel)
    })
    output$antigen_ui <- shiny::renderUI({
      cv <- curves(); shiny::req(cv, input$pg_source)
      ags  <- sort(unique(cv$antigen_lbl[cv$source_lbl == input$pg_source]))
      keep <- shiny::isolate(input$pg_antigen)
      sel  <- if (!is.null(keep) && keep %in% ags) keep else ags[[1]]
      shiny::selectInput(ns("pg_antigen"), "Antigen", choices = ags, selected = sel)
    })
    output$feature_ui <- shiny::renderUI({
      cv <- curves(); shiny::req(cv, input$pg_source, input$pg_antigen)
      fts <- sort(unique(cv$feature_lbl[cv$source_lbl == input$pg_source &
                                        cv$antigen_lbl == input$pg_antigen]))
      if (!length(fts)) return(NULL)
      keep <- shiny::isolate(input$pg_feature)
      sel  <- if (!is.null(keep) && keep %in% fts) keep else fts[[1]]
      shiny::selectInput(ns("pg_feature"), "Feature", choices = fts, selected = sel)
    })

    # curves matching the chosen source/antigen/feature (one per plate)
    selected_curves <- shiny::reactive({
      cv <- curves()
      shiny::req(cv, input$pg_source, input$pg_antigen, input$pg_feature)
      cv[cv$source_lbl  == input$pg_source &
         cv$antigen_lbl == input$pg_antigen &
         cv$feature_lbl == input$pg_feature, , drop = FALSE]
    })

    # ---- raw sample MFI for the whole experiment (joined into fig 2) --------
    # xmap_sample carries antibody_mfi; calib_samples does not. Aggregate to the
    # sample-identity key (mean over replicate wells) for a clean 1:1 join.
    sample_mfi <- shiny::reactive({
      s <- scope_val(); shiny::req(s$study, s$experiment, s$project_id)
      reload_trigger()
      raw <- tryCatch(fetch_raw_sample(pool, project = s$project_id,
                        study = s$study, experiment = s$experiment),
                      error = function(e) NULL)
      if (is.null(raw) || !nrow(raw) || !"antibody_mfi" %in% names(raw)) return(NULL)
      raw$.mfi <- num(raw$antibody_mfi)
      raw <- raw[is.finite(raw$.mfi), , drop = FALSE]
      if (!nrow(raw)) return(NULL)
      # FIXED key order + normalization, identical to the gated() builder below:
      # plate | antigen | feature(labelled) | sampleid | patientid | timeperiod | dilution
      getc <- function(nm) if (nm %in% names(raw)) raw[[nm]] else rep(NA, nrow(raw))
      raw$.key <- paste(.norm(getc("plate")), .norm(getc("antigen")),
                        feat_label(getc("feature")), .norm(getc("sampleid")),
                        .norm(getc("patientid")), .norm(getc("timeperiod")),
                        .dilnorm(getc("dilution")), sep = "\r")
      agg <- tapply(raw$.mfi, raw$.key, function(v) mean(v, na.rm = TRUE))
      data.frame(.key = names(agg), mfi = as.numeric(agg),
                 stringsAsFactors = FALSE)
    })

    # ---- gated samples across the selected plates ---------------------------
    # One row per back-calculated sample (curve/method sample identity), tagged
    # with plate + gate (below/in/above LOQ, by concentration) + (joined) MFI.
    gated <- shiny::reactive({
      cvf <- selected_curves(); shiny::req(nrow(cvf) > 0)
      meth <- input$approach %||% "frequentist"
      mfi_map <- sample_mfi()

      rows <- lapply(seq_len(nrow(cvf)), function(i) {
        cid <- cvf$curve_id[i]; plate_lbl <- cvf$plate_lbl[i]
        sm <- tryCatch(fetch_calib_samples(pool, cid, meth), error = function(e) NULL)
        if (is.null(sm) || !nrow(sm)) return(NULL)
        dg <- tryCatch(fetch_calib_diagnostics(pool, cid, meth), error = function(e) NULL)
        lloq_conc <- if (!is.null(dg) && nrow(dg) && "lloq_conc" %in% names(dg)) num(dg$lloq_conc[1]) else NA_real_
        uloq_conc <- if (!is.null(dg) && nrow(dg) && "uloq_conc" %in% names(dg)) num(dg$uloq_conc[1]) else NA_real_

        n     <- nrow(sm)
        dil   <- if ("dilution" %in% names(sm)) num(sm$dilution) else rep(NA_real_, n)
        final <- if ("final_concentration" %in% names(sm)) num(sm$final_concentration) else rep(NA_real_, n)
        if (length(dil)   != n) dil   <- rep(NA_real_, n)
        if (length(final) != n) final <- rep(NA_real_, n)

        chr_col <- function(nm) if (nm %in% names(sm)) as.character(sm[[nm]]) else rep(NA_character_, n)
        pid <- chr_col("patientid"); tp <- chr_col("timeperiod"); sid <- chr_col("sampleid")

        # assay response (MFI) per sample -- both the y in figure 2 and the
        # classification fallback when final_concentration is unusable. Same key
        # + normalizers as sample_mfi() so the join lines up.
        mfi <- rep(NA_real_, n)
        if (!is.null(mfi_map)) {
          keys <- paste(.norm(plate_lbl), .norm(input$pg_antigen),
                        .norm(input$pg_feature), .norm(sid), .norm(pid),
                        .norm(tp), .dilnorm(dil), sep = "\r")
          mfi <- mfi_map$mfi[match(keys, mfi_map$.key)]
        }

        # Curve response at the LLOQ/ULOQ concentrations, interpolated from the
        # standards (raw response, same scale as the sample MFI). Only computed
        # when some sample needs the response fallback.
        resp_lloq <- NA_real_; resp_uloq <- NA_real_
        if (any(!is.finite(final))) {
          sp <- tryCatch(fetch_calib_standards(pool, cid, meth), error = function(e) NULL)
          if (!is.null(sp) && nrow(sp) && "assay_response_raw" %in% names(sp)) {
            xx <- num(sp$log10_concentration)
            if (all(!is.finite(xx)) && "concentration" %in% names(sp))
              xx <- log10(num(sp$concentration))
            yy <- num(sp$assay_response_raw)
            ok <- is.finite(xx) & is.finite(yy)
            if (sum(ok) >= 2) {
              a  <- tapply(yy[ok], xx[ok], mean)          # mean over replicate concs
              xu <- as.numeric(names(a)); yu <- as.numeric(a); o <- order(xu)
              fint <- stats::approxfun(xu[o], yu[o], rule = 2)
              if (is.finite(lloq_conc)) resp_lloq <- fint(log10(lloq_conc))
              if (is.finite(uloq_conc)) resp_uloq <- fint(log10(uloq_conc))
            }
          }
        }

        # Classify by CONCENTRATION when final is usable (final vs the dilution-
        # scaled LLOQ/ULOQ), else by the ASSAY RESPONSE (MFI vs the curve
        # response at LLOQ/ULOQ). Missing bounds are handled by pg_classify.
        use_conc <- is.finite(final)
        value <- ifelse(use_conc, final,           mfi)
        lo    <- ifelse(use_conc, dil * lloq_conc,  resp_lloq)
        hi    <- ifelse(use_conc, dil * uloq_conc,  resp_uloq)
        gate  <- pg_classify(value, lo, hi)

        # Build from equal-length (n) vectors ONLY; scalars are post-assigned via
        # $<- (reliable length-1 recycle). Putting a length-1 bit64 integer64
        # curve_id directly in data.frame() mis-recycles and throws
        # "differing number of rows: 1, n" -- hence as.character() + post-assign.
        d <- data.frame(
          patientid = pid, timeperiod = tp, sampleid = sid,
          dilution  = dil, final = final, gate = gate,
          s_id = paste0(pid, "_", tp),
          stringsAsFactors = FALSE)
        d$curve_id  <- as.character(cid)
        d$plate_lbl <- as.character(plate_lbl)
        d$mfi       <- mfi   # computed above (also used for the fallback gate)
        d
      })
      out <- do.call(rbind, rows)
      shiny::validate(shiny::need(!is.null(out) && nrow(out) > 0,
        "No back-calculated samples for this source/antigen/feature under the chosen approach."))
      out$plate_f <- order_plate_factor(out$plate_lbl)
      out
    })

    # ---- status line --------------------------------------------------------
    output$status <- shiny::renderUI({
      cvf <- tryCatch(selected_curves(), error = function(e) NULL)
      if (is.null(cvf) || !nrow(cvf))
        return(shiny::div(class = "alert alert-warning",
                          "Pick a source, antigen and feature."))
      g <- tryCatch(gated(), error = function(e) NULL)
      if (is.null(g)) return(NULL)
      n_mfi <- sum(is.finite(g$mfi))
      shiny::div(style = "color:#555;",
        sprintf("%s | %s \u00b7 source: %s \u00b7 %s \u00b7 %d plate(s) \u00b7 %d sample(s)%s",
                input$pg_antigen, input$pg_feature, input$pg_source,
                input$approach, length(unique(g$plate_f)), nrow(g),
                if (n_mfi < nrow(g)) sprintf(" \u00b7 MFI available for %d", n_mfi) else ""))
    })

    # ---- Figure 1: stacked % gate composition per plate ---------------------
    # below / in / above LOQ, per plate, summing to 100%.
    output$bar_plot <- plotly::renderPlotly({
      g <- gated()
      g <- g[g$gate %in% .PG_SHOW, , drop = FALSE]
      shiny::validate(shiny::need(nrow(g) > 0,
        "No in-range / below-LLOQ / above-ULOQ samples for this selection."))
      lv <- levels(g$plate_f)
      lv <- lv[lv %in% unique(as.character(g$plate_lbl))]   # plates actually present
      tab <- table(factor(g$plate_lbl, levels = lv),
                   factor(g$gate, levels = .PG_SHOW))
      pct <- prop.table(tab, margin = 1) * 100
      pct[is.nan(pct)] <- 0
      plates <- rownames(pct)

      p <- plotly::plot_ly()
      for (gt in .PG_SHOW) {
        p <- plotly::add_trace(p, type = "bar", orientation = "h",
          y = plates, x = as.numeric(pct[, gt]),
          name = .PG_LABELS[[gt]],
          marker = list(color = .PG_COLORS[[gt]]),
          text = sprintf("%.1f%%", as.numeric(pct[, gt])),
          textposition = "inside", insidetextanchor = "middle",
          hovertemplate = paste0(.PG_LABELS[[gt]], ": %{x:.1f}%<extra></extra>"))
      }
      plotly::layout(p, barmode = "stack",
        xaxis = list(title = "Percentage", range = c(0, 100), ticksuffix = "%"),
        yaxis = list(title = "Plate", type = "category",
                     categoryorder = "array", categoryarray = rev(lv)),
        legend = list(title = list(text = "Gate status")),
        margin = list(l = 90, t = 20))
    })

    # ---- Figure 2: half-violin of MFI per plate + dots coloured by gate -----
    # All classifiable dots kept (below / in / above LOQ). Y is log10 (values
    # plotted as log10(MFI) on a linear axis so the KDE is computed in log space)
    # with natural-unit decade labels.
    output$mfi_plot <- plotly::renderPlotly({
      g <- gated()
      g <- g[g$gate %in% .PG_SHOW & is.finite(g$mfi) & g$mfi > 0, , drop = FALSE]
      shiny::validate(shiny::need(nrow(g) > 0,
        "No in/below/above-LOQ samples with MFI for this selection (no xmap_sample match)."))
      g$logmfi <- log10(g$mfi)
      lv <- levels(g$plate_f)
      lv <- lv[lv %in% unique(as.character(g$plate_lbl))]   # plates actually present
      g$xpos <- match(as.character(g$plate_lbl), lv)        # numeric plate position

      # half violins (one per plate) on the +x side; dots jittered to the -x side
      p <- plotly::plot_ly()
      p <- plotly::add_trace(p, type = "violin",
        x = g$xpos, y = g$logmfi, side = "positive", points = FALSE,
        line = list(color = "#9e9e9e"), fillcolor = "rgba(158,158,158,0.25)",
        width = 0.9, hoverinfo = "skip", showlegend = FALSE,
        scalemode = "count", meanline = list(visible = FALSE))
      for (gt in .PG_SHOW) {
        d <- g[g$gate == gt, , drop = FALSE]; if (!nrow(d)) next
        p <- plotly::add_trace(p, type = "scatter", mode = "markers",
          x = d$xpos - stats::runif(nrow(d), 0.05, 0.28), y = d$logmfi,
          name = .PG_LABELS[[gt]], legendgroup = gt,
          marker = list(color = .PG_COLORS[[gt]], size = 6, opacity = 0.8),
          hoverinfo = "text",
          text = sprintf("%s<br>plate %s<br>MFI %.4g<br>%s",
                         d$s_id, d$plate_lbl, d$mfi, .PG_LABELS[[gt]]))
      }

      # y decade ticks: integer log10 positions, labelled in natural units
      ydec <- seq(floor(min(g$logmfi)), ceiling(max(g$logmfi)))
      ylab <- vapply(10^ydec, function(v)
        formatC(v, format = "fg", big.mark = ",", drop0trailing = TRUE), character(1))
      plotly::layout(p,
        xaxis = list(title = "Plate", tickmode = "array",
                     tickvals = seq_along(lv), ticktext = lv,
                     range = c(0.4, length(lv) + 0.9)),
        yaxis = list(title = "MFI", tickmode = "array",
                     tickvals = ydec, ticktext = ylab),
        legend = list(title = list(text = "Gate status")),
        margin = list(t = 20))
    })

    # ---- Figure 3: raw standard curves by plate (plate_dilution styling) ----
    output$sc_plot <- plotly::renderPlotly({
      cvf <- selected_curves(); shiny::req(nrow(cvf) > 0)
      meth <- input$approach %||% "frequentist"
      dat <- do.call(rbind, lapply(seq_len(nrow(cvf)), function(i) {
        sp <- tryCatch(fetch_calib_standards(pool, cvf$curve_id[i], meth),
                       error = function(e) NULL)
        if (is.null(sp) || !nrow(sp)) return(NULL)
        y <- if ("assay_response_raw" %in% names(sp)) num(sp$assay_response_raw)
             else num(sp$response_model)
        data.frame(dil_num = num(sp$dilution), mfi_num = y,
                   plate_lbl = cvf$plate_lbl[i], stringsAsFactors = FALSE)
      }))
      shiny::validate(shiny::need(!is.null(dat) && nrow(dat) > 0,
        "No standard points for this selection."))
      dat <- dat[is.finite(dat$dil_num) & dat$dil_num > 0 &
                 is.finite(dat$mfi_num) & dat$mfi_num > 0, , drop = FALSE]
      shiny::validate(shiny::need(nrow(dat) > 0, "No positive standard points to plot."))
      dat$plate_f <- order_plate_factor(dat$plate_lbl)
      dat <- dat[order(dat$plate_f, dat$dil_num), , drop = FALSE]
      dat$hover_txt <- sprintf("plate: %s<br>dilution: %s<br>MFI: %s",
        dat$plate_lbl, format(dat$dil_num, trim = TRUE, scientific = FALSE),
        format(dat$mfi_num, trim = TRUE, scientific = FALSE))

      xpos <- dat$dil_num[is.finite(dat$dil_num) & dat$dil_num > 0]
      x_breaks <- if (length(xpos)) 10^(seq(floor(log10(min(xpos))),
                                            ceiling(log10(max(xpos))))) else ggplot2::waiver()
      nat_lab <- function(v) formatC(v, format = "fg", big.mark = ",", drop0trailing = TRUE)

      plates <- levels(dat$plate_f)
      pal <- scales::hue_pal()(max(length(plates), 1L)); names(pal) <- plates

      gg <- ggplot2::ggplot(dat,
              ggplot2::aes(x = dil_num, y = mfi_num, colour = plate_f,
                           group = plate_f, text = hover_txt)) +
        ggplot2::geom_line(linewidth = 0.4, na.rm = TRUE) +
        ggplot2::geom_point(size = 1.3, na.rm = TRUE) +
        ggplot2::scale_x_log10(breaks = x_breaks, labels = nat_lab,
                               expand = ggplot2::expansion(mult = 0.04)) +
        ggplot2::scale_y_log10() +
        ggplot2::scale_colour_manual(values = pal, name = "Plate") +
        ggplot2::labs(x = "Dilution Fraction", y = "Antibody MFI",
                      title = sprintf("%s | %s \u2014 %s",
                                      input$pg_antigen, input$pg_feature, input$pg_source)) +
        ggplot2::theme_bw(base_size = 11) +
        ggplot2::theme(
          panel.grid.minor = ggplot2::element_blank(),
          axis.text.x      = ggplot2::element_text(angle = 60, hjust = 1),
          legend.title     = ggplot2::element_text(size = 10),
          plot.title       = ggplot2::element_text(size = 13, face = "bold"))

      p  <- plotly::ggplotly(gg, tooltip = "text")
      ax <- grep("^xaxis", names(p$x$layout), value = TRUE)
      if (!length(ax)) ax <- "xaxis"
      for (nm in ax) p$x$layout[[nm]]$tickangle <- -60
      plotly::layout(p, margin = list(t = 50, b = 95))
    })

    invisible(NULL)
  })
}
