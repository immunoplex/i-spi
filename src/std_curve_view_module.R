# =============================================================================
# std_curve_view_module.R  --  the calibration-curve VIEWER (read + plot).
#
# Split out of the former monolithic std_curve_module.R (pure refactor: behavior
# unchanged). This half OWNS viewing: antigen/curve/method/model selectors, the
# plot (+ overlays, legend, diagnostics), and the result tables. Masking will be
# added here later (it lives in the plot, in the context of the fit).
#
# Contract with the parent / sibling calc module (shared reactiveVals):
#   scope        : reactive(list(study, experiment, project_id))  -- app scope
#   calib_dirty  : reactiveVal(int)  -- bumped when calib_* changes (job done /
#                  future mask); this module re-reads calib_* when it changes.
#   selected_curve : reactiveVal(list(curve_id, method) | NULL) -- this module
#                  WRITES the current selection so a heatmap click (later) or a
#                  sibling can observe/drive it.
# Returns list(curve_id, method, bundle) for other consumers.
# =============================================================================

stdCurveViewUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(
      width = 3,
      shiny::wellPanel(
        shiny::h4("Standard curve"),
        shiny::uiOutput(ns("current_experiment_label")),
        shiny::uiOutput(ns("scope_debug")),
        shiny::uiOutput(ns("antigen_ui")),
        shiny::uiOutput(ns("source_ui")),
        shiny::uiOutput(ns("curve_ui")),
        shiny::tags$hr(),
        shiny::uiOutput(ns("method_ui")),
        shiny::uiOutput(ns("model_ui")),
        shiny::tags$hr(),
        shiny::tags$div(
          shiny::checkboxInput(ns("precision_smooth"),
            "Smooth precision profile (LOESS)", value = FALSE),
          shiny::actionLink(ns("precision_help"), "?",
            style = "margin-left:4px;font-weight:bold;color:#337ab7;"),
          shiny::tags$div(style = "font-size:11px;color:#787878;",
            "A cosmetic smoother over the precision profile for presentation. It does ",
            "not change the fit or the estimates \u2014 raise Bayesian sampling on the ",
            "Compute-fits tab to reduce real Monte-Carlo variability first.")
        )
      )
    ),
    shiny::column(
      width = 9,
      shiny::uiOutput(ns("sc_status")),
      shiny::uiOutput(ns("sc_plot_area"))
    )
  )
}

stdCurveViewServer <- function(id, pool, scope = NULL,
                               calib_dirty = shiny::reactiveVal(0),
                               selected_curve = shiny::reactiveVal(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
    ns <- session$ns

    is_valid_sel <- function(x) !is.null(x) && length(x) && !is.na(x[1]) &&
                                nzchar(x[1]) && !(x[1] %in% c("Click here"))
    cur <- shiny::reactive({
      if (!is.null(scope)) scope() else list(study = NULL, experiment = NULL, project_id = NA)
    })

    lookup <- shiny::reactive({
      s <- cur()
      if (!is_valid_sel(s$study) || !is_valid_sel(s$experiment))
        return(fetch_curve_lookup(pool, study = "__no_such_study__"))   # DB -> 0 rows, no whole-table load
      # DB-side scope filter (was: whole curve_lookup pulled to R then filtered).
      fetch_curve_lookup(pool, study = s$study, experiment = s$experiment,
                         project = if (is.na(s$project_id)) NULL else s$project_id)
    })

    response_lbl <- shiny::reactive({
      s <- cur(); shiny::req(is_valid_sel(s$study), is_valid_sel(s$experiment))
      response_label(response_var_of(fetch_raw_header(pool, project = s$project_id, study = s$study, experiment = s$experiment)))
    })

    output$current_experiment_label <- shiny::renderUI({
      s <- cur()
      if (!is_valid_sel(s$study) || !is_valid_sel(s$experiment))
        return(shiny::helpText("Select a study and experiment to view standard curves."))
      shiny::tagList(
        shiny::tags$b(sprintf("%s / %s", s$study, s$experiment)),
        shiny::br(), shiny::tags$small(sprintf("Assay response: %s", response_lbl())),
        shiny::tags$hr())
    })

    output$scope_debug <- shiny::renderUI({
      if (!isTRUE(getOption("ispi.std_curve_debug", FALSE))) return(NULL)
      s <- cur()
      lk <- tryCatch(lookup(), error = function(e) structure(data.frame(), err = conditionMessage(e)))
      shiny::tags$pre(style = "font-size:11px;background:#f6f6f6;padding:6px;border:1px solid #ddd;",
        paste(collapse = "\n", c(
          sprintf("cur = [%s / %s / %s]", s$study %||% "NULL", s$experiment %||% "NULL", s$project_id %||% "NULL"),
          sprintf("lookup(): %s rows", if (is.null(lk)) "NULL" else nrow(lk)),
          sprintf("antigens = [%s]", if (!is.null(lk) && nrow(lk)) paste(sort(unique(lk$antigen)), collapse=", ") else ""),
          sprintf("input antigen/curve/method = [%s / %s / %s]",
                  input$antigen %||% "NULL", input$curve %||% "NULL", input$method %||% "NULL"))))
    })

    output$antigen_ui <- shiny::renderUI({
      lk <- lookup()
      ags <- if (!is.null(lk) && nrow(lk)) sort(unique(lk$antigen)) else character(0)
      keep <- shiny::isolate(input$antigen)
      sel <- if (!is.null(keep) && nzchar(keep) && keep %in% ags) keep
             else if (length(ags)) ags[1] else NULL
      shiny::selectizeInput(session$ns("antigen"), "Antigen", choices = ags, selected = sel)
    })

    # Pretty-print the `source` natural-key value: the '__none__' sentinel (and
    # blanks) mean "no source recorded".
    src_label <- function(x) {
      x <- as.character(x)
      ifelse(is.na(x) | x %in% c("__none__", ""), "(no source)", x)
    }

    # All curves for the selected antigen, BEFORE any source narrowing. `source`
    # is part of the 10-column curve natural key, so one antigen can carry
    # several curves that differ only by source; those are indistinguishable in
    # the Curve dropdown without the dedicated selector below.
    antigen_curves <- shiny::reactive({
      shiny::req(input$antigen)
      lk <- lookup()
      if (is.null(lk) || !nrow(lk)) return(lk[0, , drop = FALSE])
      lk[lk$antigen == input$antigen, , drop = FALSE]
    })

    # Distinct standard-curve sources present for the current antigen.
    antigen_sources <- shiny::reactive({
      ac <- antigen_curves()
      if (is.null(ac) || !nrow(ac)) character(0)
      else sort(unique(as.character(ac$source)))
    })

    # Source selector -- rendered ONLY when the antigen actually has more than
    # one source to choose between (a lone source needs no picker).
    output$source_ui <- shiny::renderUI({
      srcs <- antigen_sources()
      if (length(srcs) < 2) return(NULL)
      choices <- stats::setNames(srcs, src_label(srcs))
      keep <- shiny::isolate(input$source)
      sel  <- if (!is.null(keep) && keep %in% srcs) keep else srcs[[1]]
      shiny::selectizeInput(session$ns("source"), "Standard curve source",
                            choices = choices, selected = sel)
    })

    # Curves for the selected antigen AND source. With >1 source we pin to the
    # chosen one (defaulting to the first until the selector initialises); with a
    # single source the antigen set is used unchanged.
    matching_curves <- shiny::reactive({
      ac <- antigen_curves()
      if (is.null(ac) || !nrow(ac)) return(ac)
      srcs <- antigen_sources()
      if (length(srcs) >= 2) {
        ssel <- input$source
        if (is.null(ssel) || !(ssel %in% srcs)) ssel <- srcs[[1]]
        ac <- ac[as.character(ac$source) == ssel, , drop = FALSE]
      }
      ac
    })

    output$curve_ui <- shiny::renderUI({
      mc <- matching_curves()
      if (is.null(mc) || !nrow(mc))
        return(shiny::selectizeInput(session$ns("curve"), "Curve", choices = character(0)))
      # Fold source into the label too, so curves stay distinguishable even when
      # the source picker is hidden (single source) or ignored.
      labels <- with(mc, {
        base <- paste0("plate=", plateid, " | feat=", feature,
                       " | dil=", nominal_sample_dilution, " | \u03bb=", wavelength)
        has_src <- !(is.na(source) | source %in% c("__none__", ""))
        ifelse(has_src, paste0(base, " | src=", source), base)
      })
      choices <- setNames(as.character(mc$curve_id), labels)
      keep <- shiny::isolate(input$curve)
      sel <- if (!is.null(keep) && keep %in% choices) keep
             else if (length(choices)) choices[[1]] else NULL
      shiny::selectizeInput(session$ns("curve"), "Curve", choices = choices, selected = sel)
    })

    curve_id <- shiny::reactive({ shiny::req(input$curve); input$curve })
    method   <- shiny::reactive(input$method)

    output$method_ui <- shiny::renderUI({
      cid <- curve_id()
      meths <- tryCatch(fetch_calib_methods(pool, cid), error = function(e) character(0))
      pretty <- c(bayesian = "Bayesian", frequentist = "Frequentist")
      labs <- ifelse(meths %in% names(pretty), pretty[meths], meths)
      choices <- if (length(meths)) stats::setNames(meths, labs) else character(0)
      keep <- shiny::isolate(input$method)
      sel  <- if (!is.null(keep) && keep %in% meths) keep
              else if (length(meths)) meths[1] else NULL
      shiny::selectInput(session$ns("method"), "Method (view)", choices = choices, selected = sel)
    })

    # Publish the current selection for sibling modules (heatmap click, etc.).
    shiny::observe({
      cid <- tryCatch(curve_id(), error = function(e) NULL)
      m   <- tryCatch(method(),  error = function(e) NULL)
      if (!is.null(cid) && !is.null(m)) selected_curve(list(curve_id = cid, method = m))
    })

    # -- the one bundle every panel reads from. calib_dirty() forces a refetch
    #    when a sibling changes calib_* (job completes; future: mask save).
    bundle <- shiny::reactive({
      calib_dirty()
      shiny::req(curve_id(), method())
      fetch_calib_bundle(pool, curve_id(), method())
    })

    has_calib <- shiny::reactive({
      if (!shiny::isTruthy(curve_id()) || !shiny::isTruthy(method())) return(FALSE)
      b <- bundle(); nrow(b$fit_best) > 0 && nrow(b$grid) > 0
    })

    # -- status banner (per-curve) ----------------------------------------
    output$sc_status <- shiny::renderUI({
      if (!isTRUE(has_calib())) {
        return(shiny::div(class = "well", style = "background:#f5f5f5;",
          shiny::strong("No fitted calibration for this curve/method yet."),
          shiny::br(),
          shiny::span("Use the ", shiny::strong("Compute fits"), " tab to submit a fit; ",
                      "results appear here when done.")))
      }
      b <- bundle(); resp <- response_lbl()
      shiny::div(style = "margin:4px 0 8px;",
        shiny::strong("Fitted: "), family_label(b$fit_best$model_name[1]),
        shiny::span(sprintf("  \u00b7  %s  \u00b7  response: %s", method(), resp)))
    })

    output$sc_plot_area <- shiny::renderUI({
      if (!isTRUE(has_calib())) return(NULL)
      shiny::tabsetPanel(
        shiny::tabPanel("Curve",
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("curve_plot"), height = "460px"), type = 4, color = "#337ab7"),
          shiny::uiOutput(ns("fda_ribbon_area")),
          shiny::uiOutput(ns("mask_selection")),
          shiny::uiOutput(ns("unmask_selection")),
          shiny::uiOutput(ns("diagnostics_panel"))),
        shiny::tabPanel("Precision",
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("precision_plot"), height = "460px"), type = 4, color = "#337ab7"),
          shiny::uiOutput(ns("precision_panel"))),
        shiny::tabPanel("Model selection", DT::DTOutput(ns("fits_table"))),
        shiny::tabPanel("Parameters",      DT::DTOutput(ns("params_table"))),
        shiny::tabPanel("Back-calculated samples", DT::DTOutput(ns("samples_table")))
      )
    })

    # -- curve plot: ONE visualization for both methods, from the shared
    #    calib_* contract. Fitted line + CI band from calib_grid; observed
    #    standards from calib_standards (split on `included`); INDIVIDUAL blanks
    #    parked at the left edge (blanks have no concentration -- x is plotting
    #    only). X-range = padded union of standard + sample concentrations. Axis
    #    positions are log10 (decade spacing); tick LABELS are natural units.
    #    Every point carries customdata (type|well|dilution) so clicks can
    #    identify it for masking.
    output$curve_plot <- plotly::renderPlotly({
      prog <- shiny::Progress$new(session = session); on.exit(prog$close())
      prog$set(message = "Loading curve data\u2026", value = 0.2)
      b <- bundle(); shiny::req(nrow(b$grid) > 0)
      g    <- b$grid[order(b$grid$log10_concentration), ]
      resp <- response_lbl()
      cid  <- curve_id(); meth <- method()
      num  <- function(x) suppressWarnings(as.numeric(x))

      # Standards (already on the fit scale).
      sp <- tryCatch(fetch_calib_standards(pool, cid, meth), error = function(e) NULL)
      if (!is.null(sp) && nrow(sp)) {
        sp$log10_concentration <- num(sp$log10_concentration)
        sp$response_model      <- num(sp$response_model)
        sp <- sp[is.finite(sp$log10_concentration) & is.finite(sp$response_model), , drop = FALSE]
      }
      # Samples -- back-calculated concentration on x; y = the fitted response at
      # that concentration (a back-calc sample lies ON the curve by construction:
      # c = f^-1(r) so (c, r) = (c, f(c))). Interpolate the grid for y.
      smp <- tryCatch(fetch_calib_samples(pool, cid, meth), error = function(e) NULL)
      # smp_x <- if (!is.null(smp) && nrow(smp)) {
      #   cc <- num(smp$predicted_concentration); cc[!is.finite(cc) | cc <= 0] <- NA
      #   cc2 <- num(smp$predicted_concentration); cc2[!is.finite(cc2) | cc2 <= 0] <- NA
      #   log10(ifelse(is.na(cc), cc2, cc))
      # } else numeric(0)
      # smp_x <- smp_x[is.finite(smp_x)]
      smp_x <- if (!is.null(smp) && nrow(smp)) {
        x <- num(smp$predicted_concentration)   # already log10(conc) = x-hat
        x[!is.finite(x)] <- NA
        x
      } else numeric(0)
      smp_x <- smp_x[is.finite(smp_x)]

      smp_x_win <- smp_x   # kept for the on-curve sample markers below
      # X-range = padded union of ACTUAL data (standards + samples); grid excluded.
      std_x <- if (!is.null(sp) && nrow(sp)) sp$log10_concentration else numeric(0)
      xs <- c(std_x, smp_x); xs <- xs[is.finite(xs)]
      if (!length(xs)) xs <- c(0, 1)
      xmin <- min(xs); xmax <- max(xs)
      span <- max(xmax - xmin, 0.5); pad <- 0.04 * span
      xlo <- xmin - pad; xhi <- xmax + pad
      # Blanks sit just inside the (data-driven) left edge, in their own gutter.
      blank_x <- xlo + 0.015 * (xhi - xlo)
      decs <- seq(ceiling(xlo), floor(xhi))
      if (!length(decs)) decs <- round(c(xlo, xhi))
      dec_label <- function(k) { v <- 10^k
        if (v >= 1e5 || v < 1e-2) formatC(v, format = "g", digits = 2)
        else formatC(v, format = "fg", big.mark = ",", drop0trailing = TRUE) }
      ticktext <- vapply(decs, dec_label, character(1))

      # y-scale: determine whether response_model is log10(raw) from the persisted
      # data (the actual fit output), so tick LABELS can show natural units.
      y_is_log <- TRUE
      if (!is.null(sp) && nrow(sp)) {
        raw <- num(sp$assay_response_raw); mod <- sp$response_model
        ok <- is.finite(raw) & raw > 0 & is.finite(mod)
        if (sum(ok) >= 3)
          y_is_log <- isTRUE(mean(abs(mod[ok] - log10(raw[ok])), na.rm = TRUE) < 0.05)
      }

      prog$set(message = "Rendering plot\u2026", value = 0.75)
      p <- plotly::plot_ly(source = session$ns("curve_plot"))
      p <- plotly::add_ribbons(p, data = g, x = ~log10_concentration,
                               ymin = ~ci_lower, ymax = ~ci_upper,
                               name = "95% band", line = list(width = 0),
                               opacity = 0.25, hoverinfo = "skip")
      p <- plotly::add_lines(p, data = g, x = ~log10_concentration,
                             y = ~predicted_response, name = "Fitted")

      # Samples on the curve: y interpolated from the grid at each sample's x.
      # Semi-transparent (overlap is expected; we'll refine dense display later).
      if (length(smp_x_win)) {
        sy <- tryCatch(stats::approx(g$log10_concentration, g$predicted_response,
                                     xout = smp_x_win, rule = 2)$y,
                       error = function(e) rep(NA_real_, length(smp_x_win)))
        keep <- is.finite(smp_x_win) & is.finite(sy)
        if (any(keep))
          p <- plotly::add_markers(p, x = smp_x_win[keep], y = sy[keep], name = "Test sample",
                 marker = list(color = "rgba(80,80,80,0.45)", symbol = "circle", size = 5),
                 hoverinfo = "skip")
      }

      # Standards: COLOUR = FDA 2018 verdict (frequentist-pinned, identical on both
      # method tabs); SHAPE = mask state (filled = in fit, hollow = masked). Staged
      # points get the ring overlay below. One trace per verdict -> colour legend;
      # symbol varies within a trace. customdata unchanged so click-masking is intact.
      # No frequentist fit -> fall back to the plain fit/masked scheme.
      if (!is.null(sp) && nrow(sp)) {
        sp$masked <- !(as.logical(sp$included) %in% TRUE)
        mask_note <- function(row_masked, reason, base) {
          if (!row_masked) return(base)
          if (!is.na(reason) && nzchar(reason)) paste0(base, " \u2014 MASKED: ", reason)
          else paste0(base, " \u2014 MASKED")
        }
        prog$set(message = "Classifying FDA 2018 (cross-plate)\u2026", value = 0.5)
        fc <- if (isTRUE(has_freq())) tryCatch(fda_class(), error = function(e) NULL) else NULL
        have_fda <- !is.null(fc) && nrow(fc$levels) > 0
        if (have_fda) {
          lv  <- fc$levels
          idx <- match(round(sp$log10_concentration, 4), round(num(lv$log10c), 4))
          sp$fda <- ifelse(is.na(idx), "NA", lv$flag[idx])
          cvv <- lv$cv[idx]; recv <- lv$recovery[idx]
          cvt  <- ifelse(is.na(cvv),  "n/a", sprintf("%.1f%%", cvv))
          rect <- ifelse(is.na(recv), "n/a", sprintf("%.0f%%", recv))
          base <- sprintf("standard | well %s | dil %s | FDA: %s | level CV %s | recovery %s",
                          sp$well, sp$dilution, FDA_LABEL[sp$fda], cvt, rect)
        } else {
          sp$fda <- "NA"
          base <- sprintf("standard | well %s | dil %s", sp$well, sp$dilution)
        }
        sp$hover <- vapply(seq_len(nrow(sp)),
          function(i) mask_note(sp$masked[i], sp$mask_reason[i], base[i]), character(1))

        draw_std <- function(p, d, nm, col) {
          if (!nrow(d)) return(p)
          plotly::add_markers(p, x = d$log10_concentration, y = d$response_model, name = nm,
            marker = list(color = col, size = 9,
              symbol = ifelse(d$masked, "circle-open", "circle"),
              line = list(color = col, width = 1.5)),
            customdata = paste("std", d$well, d$dilution, sep = "|"),
            hovertext = d$hover, hoverinfo = "text")
        }
        if (have_fda) {
          for (fl in names(FDA_COLORS)) {
            d <- sp[sp$fda == fl, , drop = FALSE]
            p <- draw_std(p, d, FDA_LABEL[[fl]], FDA_COLORS[[fl]])
          }
        } else {
          p <- draw_std(p, sp[!sp$masked, , drop = FALSE], "Standard (fit)",    "#1F78B4")
          p <- draw_std(p, sp[ sp$masked, , drop = FALSE], "Standard (masked)", "#B2182B")
        }
      }

      # Individual blanks parked at the left gutter, own color; masked = lighter.
      bl <- tryCatch(fetch_calib_blanks(pool, cid, meth), error = function(e) NULL)
      if (!is.null(bl) && nrow(bl)) {
        bl$response_model <- num(bl$response_model)
        bl <- bl[is.finite(bl$response_model), , drop = FALSE]
        bl$x <- blank_x
        binc <- bl[  as.logical(bl$included) %in% TRUE, , drop = FALSE]
        bexc <- bl[!(as.logical(bl$included) %in% TRUE), , drop = FALSE]
        if (nrow(binc))
          p <- plotly::add_markers(p, data = binc, x = ~x, y = ~response_model,
                 name = "Blank", marker = list(color = "#6A3D9A", symbol = "diamond", size = 8),
                 customdata = ~paste("blk", well, "", sep = "|"),
                 hovertext = ~sprintf("blank | well %s", well), hoverinfo = "text")
        if (nrow(bexc))
          p <- plotly::add_markers(p, data = bexc, x = ~x, y = ~response_model,
                 name = "Blank (masked)", marker = list(color = "#CAB2D6", symbol = "diamond-open", size = 9),
                 customdata = ~paste("blk", well, "", sep = "|"),
                 hovertext = ~sprintf("blank MASKED | well %s%s", well,
                     ifelse(is.na(mask_reason) | mask_reason == "", "", paste0(" \u2014 ", mask_reason))),
                 hoverinfo = "text")
      }

      # -- reference overlays as TOGGLEABLE TRACES (not layout shapes), so each
      #    appears in the plotly legend and the user can turn it on/off. All are
      #    strictly finite-guarded. d = b$diagnostics row.
      d <- b$diagnostics
      dv <- function(col) { if (!is.null(d) && nrow(d) && col %in% names(d)) {
                              v <- suppressWarnings(as.numeric(d[[col]][1]))
                              if (is.finite(v)) v else NA_real_ } else NA_real_ }
      # y-extent for vertical lines / x-extent for horizontals (data-space).
      yext <- range(c(num(g$predicted_response),
                      if (!is.null(sp)) sp$response_model), na.rm = TRUE)
      add_vline <- function(p, x, nm, col, dash, grp)
        plotly::add_lines(p, x = c(x, x), y = yext, name = nm, legendgroup = grp,
          line = list(color = col, dash = dash, width = 1.2), hoverinfo = "name")
      add_hline <- function(p, y, nm, col, dash, grp)
        plotly::add_lines(p, x = c(xlo, xhi), y = c(y, y), name = nm, legendgroup = grp,
          line = list(color = col, dash = dash, width = 1.2), hoverinfo = "name")

      # LLOQ / ULOQ -- concentration limits (VERTICAL, red dotted).
      if (!is.na(dv("lloq_log10"))) p <- add_vline(p, dv("lloq_log10"), "LLOQ", "#B2182B", "dot", "loq")
      if (!is.na(dv("uloq_log10"))) p <- add_vline(p, dv("uloq_log10"), "ULOQ", "#B2182B", "dot", "loq")
      # Shape-based LOQ (second-derivative flatness limits from curveRcore).
      # Distinct hue + dash, own legend group so it toggles independently of the
      # precision LOQ. Columns: shape_{l,u}loq_log10 (x-axis == log10 conc).
      if (!is.na(dv("shape_lloq_log10"))) p <- add_vline(p, dv("shape_lloq_log10"), "Shape LLOQ", "#E7298A", "longdash", "shape_loq")
      if (!is.na(dv("shape_uloq_log10"))) p <- add_vline(p, dv("shape_uloq_log10"), "Shape ULOQ", "#E7298A", "longdash", "shape_loq")
      # LOD lower/upper -- RESPONSE limits (HORIZONTAL, teal dash).
      if (!is.na(dv("lower_lod_response"))) p <- add_hline(p, dv("lower_lod_response"), "LOD lower", "#1B9E77", "dash", "lod")
      if (!is.na(dv("upper_lod_response"))) p <- add_hline(p, dv("upper_lod_response"), "LOD upper", "#1B9E77", "dash", "lod")
      # RDL lower/upper -- concentration (VERTICAL) + corresponding response read
      # off the fitted curve (HORIZONTAL), orange dash-dot, one legend group.
      rdl_proj <- function(xv) if (is.na(xv)) NA_real_ else
        tryCatch(stats::approx(g$log10_concentration, g$predicted_response, xout = xv, rule = 2)$y,
                 error = function(e) NA_real_)
      for (lohi in c("lower", "upper")) {
        xv <- dv(paste0("rdl_", lohi, "_log10"))
        if (!is.na(xv)) {
          p <- add_vline(p, xv, sprintf("RDL %s", lohi), "#D95F02", "dashdot", "rdl")
          yv <- rdl_proj(xv)
          if (is.finite(yv)) p <- add_hline(p, yv, sprintf("RDL %s (resp)", lohi), "#D95F02", "dashdot", "rdl")
        }
      }

      # Inflection -- toggleable marker. NOTE: inflect_x/inflect_y scale vs the
      # log10_concentration axis is still being confirmed; only plot it when it
      # actually falls within the visible window so a mis-scaled value can't sit
      # off-canvas or distort. (Placement to be verified against curveRcore.)
      ix <- dv("inflect_x"); iy <- dv("inflect_y")
      if (!is.na(ix) && !is.na(iy) && ix >= xlo && ix <= xhi)
        p <- plotly::add_markers(p, x = ix, y = iy, name = "Inflection",
               marker = list(color = "#000000", symbol = "star", size = 12,
                             line = list(color = "#FFFFFF", width = 1)),
               hovertext = sprintf("inflection | x=%.3g y=%.3g", ix, iy), hoverinfo = "text")

      # Y-axis: response_model is log-positioned when y_is_log. Relabel ticks to
      # NATURAL units at each decade + its rounded log-midpoint (1, 3, 10, 30, ...).
      yaxis <- list(title = resp, zeroline = FALSE)
      if (isTRUE(y_is_log)) {
        yvals <- num(if (!is.null(sp)) sp$response_model else NULL)
        yvals <- c(yvals, num(g$predicted_response))
        yvals <- yvals[is.finite(yvals)]
        if (length(yvals)) {
          d0 <- floor(min(yvals)); d1 <- ceiling(max(yvals))
          decs_y <- seq(d0, d1)
          mids_y <- decs_y + 0.5                      # log10(sqrt(10)) ~ 0.5 -> "3"
          ytv <- sort(c(decs_y, mids_y))
          ytv <- ytv[ytv >= d0 & ytv <= d1]
          ynat <- function(v) {
            n <- 10^v
            if (n >= 1e5 || n < 1e-2) formatC(n, format = "g", digits = 1)
            else formatC(signif(n, 1), format = "fg", big.mark = ",", drop0trailing = TRUE)
          }
          yaxis <- list(title = resp, zeroline = FALSE, tickmode = "array",
                        tickvals = as.list(ytv),
                        ticktext = as.list(vapply(ytv, ynat, character(1))))
        }
      }

      # Staged-point highlight (user-controlled): outline the snapshot captured
      # at the last "Highlight selected" press. Depends on highlight_tick so it
      # only re-renders on demand, not on every click.
      highlight_tick()
      hs <- shiny::isolate(highlight_set())
      if (length(hs)) {
        hx <- c(); hy <- c()
        if (!is.null(sp) && nrow(sp)) {
          key_s <- paste("std", sp$well, sp$dilution, sep = "|")
          m <- key_s %in% hs
          if (any(m)) { hx <- c(hx, sp$log10_concentration[m]); hy <- c(hy, sp$response_model[m]) }
        }
        if (exists("bl") && !is.null(bl) && nrow(bl)) {
          key_b <- paste("blk", bl$well, "", sep = "|")
          m <- key_b %in% hs
          if (any(m)) { hx <- c(hx, bl$x[m]); hy <- c(hy, bl$response_model[m]) }
        }
        ok <- is.finite(hx) & is.finite(hy)
        if (any(ok))
          p <- plotly::add_markers(p, x = hx[ok], y = hy[ok], name = "Staged to mask",
                 marker = list(color = "rgba(0,0,0,0)", size = 16,
                               line = list(color = "#E31A1C", width = 3)),
                 hoverinfo = "skip")
      }

      # Unmask-staged highlight: same snapshot mechanism (shared highlight_tick),
      # a GREEN ring to signal "coming back into the fit" vs the red mask ring.
      uhs <- shiny::isolate(unhighlight_set())
      if (length(uhs)) {
        hx <- c(); hy <- c()
        if (!is.null(sp) && nrow(sp)) {
          key_s <- paste("std", sp$well, sp$dilution, sep = "|")
          m <- key_s %in% uhs
          if (any(m)) { hx <- c(hx, sp$log10_concentration[m]); hy <- c(hy, sp$response_model[m]) }
        }
        if (exists("bl") && !is.null(bl) && nrow(bl)) {
          key_b <- paste("blk", bl$well, "", sep = "|")
          m <- key_b %in% uhs
          if (any(m)) { hx <- c(hx, bl$x[m]); hy <- c(hy, bl$response_model[m]) }
        }
        ok <- is.finite(hx) & is.finite(hy)
        if (any(ok))
          p <- plotly::add_markers(p, x = hx[ok], y = hy[ok], name = "Staged to unmask",
                 marker = list(color = "rgba(0,0,0,0)", size = 16,
                               line = list(color = "#33A02C", width = 3)),
                 hoverinfo = "skip")
      }

      p <- plotly::layout(p,
        xaxis = list(title = "Concentration", zeroline = FALSE,
                     range = c(xlo, xhi), tickmode = "array",
                     tickvals = as.list(decs), ticktext = as.list(ticktext)),
        yaxis = yaxis,
        legend = list(orientation = "v", x = 1.02, y = 1, xanchor = "left"),
        margin = list(r = 10))
      # Register click events (source id scoped to this module) for masking.
      # doubleClick = FALSE: we own the double-click gesture (unmask) below, so
      # stop plotly's default double-click axis-reset from firing with it.
      p <- plotly::config(p, doubleClick = FALSE)
      p <- plotly::event_register(p, "plotly_click")
      # Double-click a MASKED point to stage it for UNMASKING. plotly emits no
      # per-point double-click event (plotly_doubleclick carries no point), so we
      # detect it ourselves: pair two plotly_click events on the SAME point within
      # 500 ms and push that point's customdata to a Shiny input. Single clicks
      # still drive mask-staging via event_data() above; the mask observer ignores
      # already-masked points so the two gestures don't collide.
      htmlwidgets::onRender(p, "
        function(el, x, inputId) {
          var lastKey = null, lastT = 0;
          el.on('plotly_click', function(d) {
            if (!d || !d.points || !d.points.length) return;
            var cd = d.points[0].customdata;
            if (cd === undefined || cd === null) return;
            var now = Date.now();
            if (cd === lastKey && (now - lastT) < 500) {
              Shiny.setInputValue(inputId, {key: cd, nonce: now}, {priority: 'event'});
              lastKey = null; lastT = 0;
            } else { lastKey = cd; lastT = now; }
          });
        }", data = session$ns("pt_dblclick"))
    })

    # =====================================================================
    # MASKING -- step 1: read-only point selection (no DB writes yet).
    # Clicks accumulate a staged set of points (by their customdata identity:
    # "std|well|dilution" or "blk|well|"). Shown as a list with a Clear button.
    # =====================================================================
    staged <- shiny::reactiveVal(character(0))  # customdata keys, staged to mask
    unstaged <- shiny::reactiveVal(character(0)) # customdata keys, staged to UNMASK
    # Highlight is USER-CONTROLLED: the plot only draws staged outlines for the
    # snapshot captured at the last "Highlight selected" press -- so ordinary
    # clicks stage silently (no re-render) and the user pays the re-render only
    # when they ask. highlight_set is that snapshot; highlight_tick forces the
    # plot to re-read it on demand. unhighlight_set is the parallel snapshot for
    # unmask staging; both share the one tick (one re-render redraws both rings).
    highlight_set   <- shiny::reactiveVal(character(0))
    unhighlight_set <- shiny::reactiveVal(character(0))
    highlight_tick  <- shiny::reactiveVal(0)

    # The customdata keys of the points that are CURRENTLY masked on this curve+
    # method, derived exactly as the plot derives its hollow markers (included ==
    # FALSE in calib_standards / calib_blanks). Drives the click routing below:
    # a masked point is unmasked (double-click), not masked (single-click).
    # Refreshes with calib_dirty so it tracks the plot after any mask/unmask save.
    masked_keys <- shiny::reactive({
      calib_dirty()
      cid <- curve_id(); meth <- method()
      if (!shiny::isTruthy(cid) || !shiny::isTruthy(meth)) return(character(0))
      keys <- character(0)
      sp <- tryCatch(fetch_calib_standards(pool, cid, meth), error = function(e) NULL)
      if (!is.null(sp) && nrow(sp)) {
        m <- !(as.logical(sp$included) %in% TRUE)
        if (any(m)) keys <- c(keys, paste("std", sp$well[m], sp$dilution[m], sep = "|"))
      }
      bl <- tryCatch(fetch_calib_blanks(pool, cid, meth), error = function(e) NULL)
      if (!is.null(bl) && nrow(bl)) {
        m <- !(as.logical(bl$included) %in% TRUE)
        if (any(m)) keys <- c(keys, paste("blk", bl$well[m], "", sep = "|"))
      }
      unique(keys)
    })

    # A single click toggles an INCLUDED point in/out of the mask-staged set.
    # Masked points are ignored here -- they are unmasked via double-click (the
    # onRender shim -> input$pt_dblclick) so the two gestures never fight over the
    # same point. Only real data points carry customdata; lines/legend give NULL.
    shiny::observeEvent(plotly::event_data("plotly_click", source = session$ns("curve_plot")), {
      # The click event is registered ONCE on the plot object in the render
      # (plotly::event_register(p, ...) above); it must NOT be re-registered here.
      # Just read the payload, and never let a malformed click be fatal: guard the
      # read, require a customdata identity, and take the first if several arrive.
      ev <- tryCatch(
        plotly::event_data("plotly_click", source = session$ns("curve_plot")),
        error = function(e) { message("sc_view: plotly_click read failed: ", conditionMessage(e)); NULL })
      cd <- if (is.data.frame(ev) && "customdata" %in% names(ev)) ev$customdata else NULL
      cd <- as.character(cd); cd <- cd[!is.na(cd) & nzchar(cd)]
      if (!length(cd)) return()
      cd <- cd[[1]]
      if (cd %in% masked_keys()) return()   # masked -> unmasked via double-click
      cur_set <- staged()
      staged(if (cd %in% cur_set) setdiff(cur_set, cd) else c(cur_set, cd))
    }, ignoreInit = TRUE)

    # A DOUBLE click toggles a MASKED point in/out of the unmask-staged set. The
    # onRender shim fires input$pt_dblclick = list(key, nonce) only on a genuine
    # double-click on a point; we still verify the point is currently masked
    # (double-clicking an included point does nothing to unmask).
    shiny::observeEvent(input$pt_dblclick, {
      ev <- input$pt_dblclick
      cd <- if (is.list(ev)) ev$key else ev
      cd <- as.character(cd); cd <- cd[!is.na(cd) & nzchar(cd)]
      if (!length(cd)) return()
      cd <- cd[[1]]
      if (!(cd %in% masked_keys())) return()  # only masked points can be unmasked
      cur_set <- unstaged()
      unstaged(if (cd %in% cur_set) setdiff(cur_set, cd) else c(cur_set, cd))
    }, ignoreInit = TRUE)

    # Clear helpers. bump() forces ONE plot re-render so rings appear/disappear.
    # clear_mask/clear_unmask clear one side; reset_stage clears BOTH (used when
    # the viewed curve/method changes -- a staged set only makes sense for the
    # curve it was selected on).
    bump <- function() highlight_tick(highlight_tick() + 1)
    clear_mask   <- function() { staged(character(0));   highlight_set(character(0));   bump() }
    clear_unmask <- function() { unstaged(character(0)); unhighlight_set(character(0)); bump() }
    reset_stage  <- function() {
      staged(character(0)); highlight_set(character(0))
      unstaged(character(0)); unhighlight_set(character(0))
      bump()
    }
    shiny::observeEvent(list(curve_id(), method()), reset_stage(), ignoreInit = TRUE)
    shiny::observeEvent(input$mask_clear,   clear_mask(),   ignoreInit = TRUE)
    shiny::observeEvent(input$unmask_clear, clear_unmask(), ignoreInit = TRUE)

    # "Highlight selected": snapshot the current staging and force ONE re-render.
    shiny::observeEvent(input$mask_highlight, {
      highlight_set(staged())
      highlight_tick(highlight_tick() + 1)
    })
    shiny::observeEvent(input$unmask_highlight, {
      unhighlight_set(unstaged())
      highlight_tick(highlight_tick() + 1)
    })

    # Human-readable staged list + Highlight (snapshot) + Clear + Save controls.
    output$mask_selection <- shiny::renderUI({
      sel <- staged()
      if (!length(sel)) {
        # If unmask staging is active, its own panel is showing -- don't repeat
        # the help here. Otherwise show the combined mask/unmask instructions.
        if (length(unstaged())) return(NULL)
        return(shiny::helpText(
          "Click an included point to stage it for masking (click it again to unstage). ",
          "Double-click a masked (hollow) point to stage it for unmasking."))
      }
      pretty <- vapply(sel, function(k) {
        parts <- strsplit(k, "|", fixed = TRUE)[[1]]
        typ <- if (identical(parts[1], "blk")) "blank" else "standard"
        w   <- if (length(parts) > 1) parts[2] else "?"
        d   <- if (length(parts) > 2 && nzchar(parts[3])) sprintf(" | dil %s", parts[3]) else ""
        sprintf("%s: well %s%s", typ, w, d)
      }, character(1))
      # Is the on-plot highlight snapshot current with the staged set?
      hs <- highlight_set()
      hint <- if (!length(hs)) {
        "Not highlighted on plot yet \u2014 press \u201cHighlight selected\u201d to ring them."
      } else if (!setequal(hs, sel)) {
        sprintf("Plot shows an OLDER highlight (%d point(s)); selection changed \u2014 press \u201cHighlight selected\u201d to refresh.",
                length(hs))
      } else {
        "Plot highlight is current."
      }
      shiny::tagList(
        shiny::tags$strong(sprintf("%d point(s) staged to mask:", length(sel))),
        shiny::tags$ul(lapply(pretty, shiny::tags$li)),
        shiny::tags$div(style = "color:#787878;font-size:11px;margin-bottom:6px;", hint),
        shiny::div(
          shiny::actionButton(session$ns("mask_highlight"), "Highlight selected",
                              class = "btn-default btn-sm"),
          shiny::actionButton(session$ns("mask_clear"), "Clear selection",
                              class = "btn-default btn-sm"),
          shiny::span(style = "float:right;",
            shiny::actionButton(session$ns("mask_save"), "Save / Apply mask",
                                class = "btn-warning btn-sm"))
        )
      )
    })


    # -- MASKING step 2: reason prompt + DRY-RUN preview (still no writes) ----
    parse_key <- function(k) strsplit(k, "|", fixed = TRUE)[[1]]

    shiny::observeEvent(input$mask_save, {
      if (!length(staged())) return()
      shiny::showModal(shiny::modalDialog(
        title = "Apply mask",
        shiny::textAreaInput(session$ns("mask_reason_txt"),
          "Reason (required) \u2014 applies to all points in this save",
          placeholder = "e.g. plate-edge contamination; implausible replicate", rows = 2),
        shiny::uiOutput(session$ns("mask_dryrun")),
        shiny::uiOutput(session$ns("mask_diag")),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("mask_apply"), "Apply mask (delete fits)",
                              class = "btn-danger")),
        easyClose = FALSE, size = "l"))
    })

    mask_plan <- shiny::reactive({
      sel <- staged(); cid <- curve_id()
      if (!length(sel) || !shiny::isTruthy(cid)) return(NULL)
      keys <- lapply(sel, parse_key)
      is_blk <- vapply(keys, function(p) identical(p[1], "blk"), logical(1))
      std_w <- vapply(keys[!is_blk], function(p) p[2], character(1))
      std_d <- vapply(keys[!is_blk], function(p) if (length(p) > 2) p[3] else "", character(1))
      blk_w <- vapply(keys[is_blk],  function(p) p[2], character(1))
      std_ids <- tryCatch(resolve_std_mask_ids(pool, cid, std_w, std_d), error = function(e) integer(0))
      blk_ids <- tryCatch(resolve_blk_mask_ids(pool, cid, blk_w),        error = function(e) integer(0))
      grp     <- tryCatch(curve_group_members(pool, cid),               error = function(e) integer(0))
      # A masked blank invalidates EVERY group it feeds (source-less fan-out,
      # across sources and methods), not just the viewed curve's group.
      blk_grp <- if (length(blk_ids))
        tryCatch(curve_ids_for_blanks(pool, blk_ids), error = function(e) integer(0)) else integer(0)
      grp <- sort(unique(c(grp, blk_grp)))
      counts  <- tryCatch(calib_group_rowcounts(pool, grp),             error = function(e) integer(0))
      list(std_ids = std_ids, blk_ids = blk_ids, grp = grp, counts = counts,
           n_std = length(std_w), n_blk = length(blk_w))
    })

    output$mask_dryrun <- shiny::renderUI({
      pl <- mask_plan(); if (is.null(pl)) return(NULL)
      total_del <- sum(pl$counts)
      shiny::tagList(
        shiny::tags$hr(),
        shiny::tags$strong("This will:"),
        shiny::tags$ul(
          shiny::tags$li(sprintf("set masked = true on %d standard row(s) [xmap_standard_id: %s]",
            length(pl$std_ids), paste(pl$std_ids, collapse = ", "))),
          shiny::tags$li(sprintf("set masked = true on %d blank row(s) [xmap_buffer_id: %s]",
            length(pl$blk_ids), paste(pl$blk_ids, collapse = ", "))),
          shiny::tags$li(sprintf("DELETE all calib_* fits for %d affected curve%s (all groups touched, incl. every group a masked blank feeds): %d row(s) total",
            length(pl$grp), if (length(pl$grp) == 1) "" else "s", total_del))),
        if (length(pl$counts))
          shiny::tags$div(style = "font-size:11px;color:#787878;",
            paste(sprintf("%s: %d", names(pl$counts), as.integer(pl$counts)), collapse = "  \u00b7  ")),
        if ((length(pl$std_ids) + length(pl$blk_ids)) != (pl$n_std + pl$n_blk))
          shiny::tags$div(style = "color:#B2182B;",
            "\u26a0 Some staged points did not resolve to a unique row \u2014 review before applying."),
        shiny::tags$div(style = "margin-top:6px;color:#555;",
          shiny::tags$em("After applying, this curve's fit is removed and its group shows ",
                         shiny::tags$b("needs calculation"),
                         ". Recompute on the Compute-fits tab to get a revised fit.")))
    })

    # Read-only diagnostic panel: shows exactly what the resolvers see, so a
    # "Nothing resolved to mask." can be understood at a glance. Compares the raw
    # NK-join wells (from xmap_*) against the staged wells and the calib_standards
    # wells the plot was built from. Remove once masking is confirmed stable.
    output$mask_diag <- shiny::renderUI({
      sel <- staged(); cid <- tryCatch(curve_id(), error = function(e) NULL)
      if (!length(sel) || is.null(cid)) return(NULL)
      keys <- lapply(sel, parse_key)
      is_blk <- vapply(keys, function(p) identical(p[1], "blk"), logical(1))
      std_w <- vapply(keys[!is_blk], function(p) p[2], character(1))
      blk_w <- vapply(keys[is_blk],  function(p) p[2], character(1))
      d <- tryCatch(diagnose_mask_resolution(pool, cid, std_w, blk_w),
                    error = function(e) list(error = conditionMessage(e)))
      fmt <- function(v) if (!length(v)) "(none)" else paste(v, collapse = ", ")
      lines <- if (!is.null(d$error)) paste("diagnostic error:", d$error) else c(
        sprintf("curve_id staged ............ %s", d$curve_id),
        sprintf("curve_id in curve_lookup ... %s", if (isTRUE(d$curve_in_lookup)) "yes" else "NO"),
        "",
        sprintf("STANDARDS staged wells ..... %s", fmt(d$staged_std_wells)),
        sprintf("  xmap NK-join rows ........ %d", d$std_join_rows),
        sprintf("  xmap join wells .......... %s", fmt(d$std_join_wells)),
        sprintf("  calib_standards wells .... %s", fmt(d$calib_std_wells)),
        sprintf("  -> matched (will mask) ... %s", fmt(d$std_matched)),
        "",
        sprintf("BLANKS staged wells ........ %s", fmt(d$staged_blk_wells)),
        sprintf("  xmap NK-join rows ........ %d", d$blk_join_rows),
        sprintf("  xmap join wells .......... %s", fmt(d$blk_join_wells)),
        sprintf("  -> matched (will mask) ... %s", fmt(d$blk_matched)))
      shiny::tagList(
        shiny::tags$hr(),
        shiny::tags$strong("Mask resolution diagnostic"),
        shiny::tags$pre(
          style = "font-size:11px;background:#f6f6f6;padding:6px;border:1px solid #ddd;white-space:pre-wrap;",
          paste(lines, collapse = "\n")))
    })

    # -- MASKING step 3: APPLY (the one destructive step) ------------------
    shiny::observeEvent(input$mask_apply, {
      reason <- trimws(input$mask_reason_txt %||% "")
      if (!nzchar(reason)) {
        shiny::showNotification("A reason is required to apply a mask.",
                                type = "error", duration = NULL)
        return()
      }
      pl <- mask_plan()
      if (is.null(pl) || (!length(pl$std_ids) && !length(pl$blk_ids))) {
        shiny::showNotification("Nothing resolved to mask.", type = "error", duration = NULL); return()
      }
      res <- tryCatch(
        apply_mask(pool, std_ids = pl$std_ids, blk_ids = pl$blk_ids,
                   group_curve_ids = pl$grp, reason = reason, set_masked = TRUE),
        error = function(e) { shiny::showNotification(conditionMessage(e),
                                type = "error", duration = NULL); NULL })
      if (is.null(res)) return()
      shiny::removeModal()
      clear_mask()                     # clear mask staging + red rings
      calib_dirty(calib_dirty() + 1)   # plot removed, status flips to needs-calc
      shiny::showNotification(
        sprintf("Masked %d point(s); deleted fits for %d curve(s) in the group. Recompute on the Compute-fits tab to get a revised fit.",
                res$masked_std + res$masked_blk, res$group_n),
        type = "message", duration = 10)
    })

    # =====================================================================
    # UNMASKING -- the inverse flow. Double-click stages masked points (above);
    # this section renders the staged list + Highlight/Clear/Save, previews the
    # change (DRY-RUN), and applies it. Mirrors the mask flow deliberately; the
    # resolvers and group/rowcount helpers are shared (they key on well and are
    # mask-state-agnostic). Unmasking needs no reason and CLEARS mask_reason.
    # =====================================================================
    output$unmask_selection <- shiny::renderUI({
      sel <- unstaged()
      if (!length(sel)) return(NULL)
      pretty <- vapply(sel, function(k) {
        parts <- strsplit(k, "|", fixed = TRUE)[[1]]
        typ <- if (identical(parts[1], "blk")) "blank" else "standard"
        w   <- if (length(parts) > 1) parts[2] else "?"
        d   <- if (length(parts) > 2 && nzchar(parts[3])) sprintf(" | dil %s", parts[3]) else ""
        sprintf("%s: well %s%s", typ, w, d)
      }, character(1))
      uhs <- unhighlight_set()
      hint <- if (!length(uhs)) {
        "Not highlighted on plot yet \u2014 press \u201cHighlight selected\u201d to ring them."
      } else if (!setequal(uhs, sel)) {
        sprintf("Plot shows an OLDER highlight (%d point(s)); selection changed \u2014 press \u201cHighlight selected\u201d to refresh.",
                length(uhs))
      } else {
        "Plot highlight is current."
      }
      shiny::tagList(
        shiny::tags$strong(sprintf("%d point(s) staged to unmask:", length(sel))),
        shiny::tags$ul(lapply(pretty, shiny::tags$li)),
        shiny::tags$div(style = "color:#787878;font-size:11px;margin-bottom:6px;", hint),
        shiny::div(
          shiny::actionButton(session$ns("unmask_highlight"), "Highlight selected",
                              class = "btn-default btn-sm"),
          shiny::actionButton(session$ns("unmask_clear"), "Clear selection",
                              class = "btn-default btn-sm"),
          shiny::span(style = "float:right;",
            shiny::actionButton(session$ns("unmask_save"), "Save / Restore points",
                                class = "btn-success btn-sm")))
      )
    })

    # DRY-RUN plan for unmasking. Identical shape to mask_plan (intentional twin):
    # resolve staged wells -> xmap ids, expand affected groups (blank fan-out
    # included), count calib_* rows that the recompute-invalidation will delete.
    unmask_plan <- shiny::reactive({
      sel <- unstaged(); cid <- curve_id()
      if (!length(sel) || !shiny::isTruthy(cid)) return(NULL)
      keys <- lapply(sel, parse_key)
      is_blk <- vapply(keys, function(p) identical(p[1], "blk"), logical(1))
      std_w <- vapply(keys[!is_blk], function(p) p[2], character(1))
      std_d <- vapply(keys[!is_blk], function(p) if (length(p) > 2) p[3] else "", character(1))
      blk_w <- vapply(keys[is_blk],  function(p) p[2], character(1))
      std_ids <- tryCatch(resolve_std_mask_ids(pool, cid, std_w, std_d), error = function(e) integer(0))
      blk_ids <- tryCatch(resolve_blk_mask_ids(pool, cid, blk_w),        error = function(e) integer(0))
      grp     <- tryCatch(curve_group_members(pool, cid),               error = function(e) integer(0))
      # An unmasked blank re-enters EVERY group it feeds (source-less fan-out),
      # so every such group's fit is stale -- mirror the mask fan-out exactly.
      blk_grp <- if (length(blk_ids))
        tryCatch(curve_ids_for_blanks(pool, blk_ids), error = function(e) integer(0)) else integer(0)
      grp <- sort(unique(c(grp, blk_grp)))
      counts  <- tryCatch(calib_group_rowcounts(pool, grp),             error = function(e) integer(0))
      list(std_ids = std_ids, blk_ids = blk_ids, grp = grp, counts = counts,
           n_std = length(std_w), n_blk = length(blk_w))
    })

    output$unmask_dryrun <- shiny::renderUI({
      pl <- unmask_plan(); if (is.null(pl)) return(NULL)
      total_del <- sum(pl$counts)
      shiny::tagList(
        shiny::tags$hr(),
        shiny::tags$strong("This will:"),
        shiny::tags$ul(
          shiny::tags$li(sprintf("set masked = false and clear the mask reason on %d standard row(s) [xmap_standard_id: %s]",
            length(pl$std_ids), paste(pl$std_ids, collapse = ", "))),
          shiny::tags$li(sprintf("set masked = false and clear the mask reason on %d blank row(s) [xmap_buffer_id: %s]",
            length(pl$blk_ids), paste(pl$blk_ids, collapse = ", "))),
          shiny::tags$li(sprintf("DELETE all calib_* fits for %d affected curve%s (all groups touched, incl. every group an unmasked blank feeds): %d row(s) total",
            length(pl$grp), if (length(pl$grp) == 1) "" else "s", total_del))),
        if (length(pl$counts))
          shiny::tags$div(style = "font-size:11px;color:#787878;",
            paste(sprintf("%s: %d", names(pl$counts), as.integer(pl$counts)), collapse = "  \u00b7  ")),
        if ((length(pl$std_ids) + length(pl$blk_ids)) != (pl$n_std + pl$n_blk))
          shiny::tags$div(style = "color:#B2182B;",
            "\u26a0 Some staged points did not resolve to a unique row \u2014 review before applying."),
        shiny::tags$div(style = "margin-top:6px;color:#555;",
          shiny::tags$em("After applying, this curve's fit is removed and its group shows ",
                         shiny::tags$b("needs calculation"),
                         ". Recompute on the Compute-fits tab to get a fit with the points restored.")))
    })

    shiny::observeEvent(input$unmask_save, {
      if (!length(unstaged())) return()
      shiny::showModal(shiny::modalDialog(
        title = "Restore (unmask) points",
        shiny::p(paste("Unmasking returns these points to the fit. The existing fit for",
                       "the affected group(s) is deleted so it can be recomputed with the",
                       "points restored.")),
        shiny::uiOutput(session$ns("unmask_dryrun")),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("unmask_apply"), "Unmask (delete fits)",
                              class = "btn-danger")),
        easyClose = FALSE, size = "l"))
    })

    shiny::observeEvent(input$unmask_apply, {
      pl <- unmask_plan()
      if (is.null(pl) || (!length(pl$std_ids) && !length(pl$blk_ids))) {
        shiny::showNotification("Nothing resolved to unmask.", type = "error", duration = NULL); return()
      }
      res <- tryCatch(
        apply_unmask(pool, std_ids = pl$std_ids, blk_ids = pl$blk_ids,
                     group_curve_ids = pl$grp),
        error = function(e) { shiny::showNotification(conditionMessage(e),
                                type = "error", duration = NULL); NULL })
      if (is.null(res)) return()
      shiny::removeModal()
      clear_unmask()                   # clear unmask staging + green rings
      calib_dirty(calib_dirty() + 1)   # plot removed, status flips to needs-calc
      shiny::showNotification(
        sprintf("Unmasked %d point(s); deleted fits for %d curve(s) in the group. Recompute on the Compute-fits tab to get a revised fit.",
                res$unmasked_std + res$unmasked_blk, res$group_n),
        type = "message", duration = 10)
    })

    # =====================================================================
    # PRECISION PROFILE tab. Shares the Curve tab's x-axis (data-driven range,
    # natural-unit decade ticks) and the LLOQ/ULOQ + RDL vertical reference
    # lines. y = pcov (%CV of back-calculated concentration), capped at 100 in
    # view. Profile line = calib_grid.pcov; sample points = calib_samples.pcov;
    # horizontal = pcov_threshold (the sample gate). The LOQ verticals should
    # cross the profile at the threshold by construction (LOQ = precision-limit
    # concentration). Counts of points leaving the top are labelled per side.
    # =====================================================================
    output$precision_plot <- plotly::renderPlotly({
      b <- bundle(); shiny::req(nrow(b$grid) > 0)
      g   <- b$grid[order(b$grid$log10_concentration), ]
      d   <- b$diagnostics
      num <- function(x) suppressWarnings(as.numeric(x))
      cid <- curve_id(); meth <- method()
      dv  <- function(col) { if (!is.null(d) && nrow(d) && col %in% names(d)) {
                v <- num(d[[col]][1]); if (is.finite(v)) v else NA_real_ } else NA_real_ }

      gx <- num(g$log10_concentration); gp <- num(g$pcov)
      thr <- dv("pcov_threshold")
      YCAP <- 100

      # Standards + samples define the x-range (same rule as the curve plot).
      sp <- tryCatch(fetch_calib_standards(pool, cid, meth), error = function(e) NULL)
      std_x <- if (!is.null(sp) && nrow(sp)) num(sp$log10_concentration) else numeric(0)
      std_x <- std_x[is.finite(std_x)]
      smp <- tryCatch(fetch_calib_samples(pool, cid, meth), error = function(e) NULL)
      # smp_x <- numeric(0); smp_p <- numeric(0)
      # if (!is.null(smp) && nrow(smp)) {
      #   cc <- num(smp$predicted_concentration); cc[!is.finite(cc) | cc <= 0] <- NA
      #   cc2 <- num(smp$predicted_concentration); cc2[!is.finite(cc2) | cc2 <= 0] <- NA
      #   smp_x_all <- log10(ifelse(is.na(cc), cc2, cc))   # may contain NA (failed inversion)
      #   smp_p_all <- num(smp$pcov)                        # finite pcov even if x is NA
      #   plc <- is.finite(smp_x_all) & is.finite(smp_p_all)
      #   smp_x <- smp_x_all[plc]; smp_p <- smp_p_all[plc]  # plottable subset
      # } else { smp_x_all <- numeric(0); smp_p_all <- numeric(0) }
      smp_x <- numeric(0); smp_p <- numeric(0)
      if (!is.null(smp) && nrow(smp)) {
        # predicted_concentration is ALREADY log10(on-curve conc) = x-hat, the same
        # axis as calib_grid.log10_concentration. Do NOT log10() it again, and do NOT
        # drop <= 0 (x-hat is legitimately negative for concentrations below 1).
        smp_x_all <- num(smp$predicted_concentration)
        smp_x_all[!is.finite(smp_x_all)] <- NA
        smp_p_all <- num(smp$pcov)
        plc <- is.finite(smp_x_all) & is.finite(smp_p_all)
        smp_x <- smp_x_all[plc]; smp_p <- smp_p_all[plc]
      } else { smp_x_all <- numeric(0); smp_p_all <- numeric(0) }
      xs <- c(std_x, smp_x); xs <- xs[is.finite(xs)]
      if (!length(xs)) xs <- c(0, 1)
      xmin <- min(xs); xmax <- max(xs); span <- max(xmax - xmin, 0.5)
      pad <- 0.04 * span; xlo <- xmin - pad; xhi <- xmax + pad
      decs <- seq(ceiling(xlo), floor(xhi)); if (!length(decs)) decs <- round(c(xlo, xhi))
      dec_label <- function(k) { v <- 10^k
        if (v >= 1e5 || v < 1e-2) formatC(v, format = "g", digits = 2)
        else formatC(v, format = "fg", big.mark = ",", drop0trailing = TRUE) }
      ticktext <- vapply(decs, dec_label, character(1))

      # Profile within the visible x-window, for count-splitting and the trace.
      inwin <- is.finite(gx) & gx >= xlo & gx <= xhi
      # Over-100 samples split at the profile minimum (the boat's bottom). Counts
      # RECONCILE with the panel total: placeable ones go low/high; over-100
      # samples with NO valid concentration (failed inversion) can't be sided.
      xmin_pcov <- if (any(inwin & is.finite(gp))) gx[inwin][which.min(gp[inwin])] else mean(c(xlo, xhi))
      over_all  <- is.finite(smp_p_all) & smp_p_all > YCAP
      n_over    <- sum(over_all)                    # == panel's "above 100%"
      placeable <- is.finite(smp_x_all)
      n_lo  <- sum(over_all & placeable & smp_x_all <  xmin_pcov)
      n_hi  <- sum(over_all & placeable & smp_x_all >= xmin_pcov)
      n_unp <- n_over - n_lo - n_hi   # by subtraction -> labels always sum to n_over


      p <- plotly::plot_ly()
      # Precision profile (clip drawn line to the cap+a hair so it visibly exits).
      gp_draw <- pmin(gp, YCAP * 1.001)
      if (isTRUE(input$precision_smooth)) {
        # Cosmetic LOESS over the profile (in-range points only). Raw stays faint.
        p <- plotly::add_lines(p, x = gx, y = gp_draw, name = "Profile (raw)",
               line = list(color = "rgba(31,120,180,0.30)"), hoverinfo = "skip")
        ok_s <- is.finite(gx) & is.finite(gp) & gp <= YCAP
        if (sum(ok_s) >= 6) {
          sx <- gx[ok_s]; sy <- gp[ok_s]
          df_s <- data.frame(sx = sx, sy = sy)
          sm <- tryCatch(stats::loess(sy ~ sx, data = df_s, span = 0.3), error = function(e) NULL)
          if (!is.null(sm)) {
            xs_s <- sort(sx)
            ys_s <- pmin(as.numeric(predict(sm, newdata = data.frame(sx = xs_s))), YCAP)
            p <- plotly::add_lines(p, x = xs_s, y = ys_s, name = "Precision profile (LOESS)",
                   line = list(color = "#1F78B4", width = 2.5), hoverinfo = "skip")
          }
        }
      } else {
        p <- plotly::add_lines(p, x = gx, y = gp_draw, name = "Precision profile (%CV)",
               line = list(color = "#1F78B4"), hoverinfo = "skip")
      }
      # Sample points (their pcov), clamped into view at the cap.
      if (length(smp_x))
        p <- plotly::add_markers(p, x = smp_x, y = pmin(smp_p, YCAP), name = "Test sample %CV",
               marker = list(color = "rgba(80,80,80,0.5)", size = 5),
               hovertext = sprintf("%%CV=%.1f", smp_p), hoverinfo = "text")
      # pcov threshold (horizontal) -- the sample gate.
      if (!is.na(thr))
        p <- plotly::add_lines(p, x = c(xlo, xhi), y = c(thr, thr), name = "pcov threshold",
               line = list(color = "#333333", dash = "dash", width = 1.2), hoverinfo = "name")
      # LLOQ / ULOQ + RDL verticals (should meet the profile at the threshold).
      yext <- c(0, YCAP)
      addv <- function(p, x, nm, col, dash) if (is.na(x)) p else
        plotly::add_lines(p, x = c(x, x), y = yext, name = nm,
          line = list(color = col, dash = dash, width = 1.2), hoverinfo = "name")
      p <- addv(p, dv("lloq_log10"), "LLOQ", "#B2182B", "dot")
      p <- addv(p, dv("uloq_log10"), "ULOQ", "#B2182B", "dot")
      # Shape-based LOQ overlaid alongside the precision LOQ, so the two
      # definitions can be compared on the same axis.
      p <- addv(p, dv("shape_lloq_log10"), "Shape LLOQ", "#E7298A", "longdash")
      p <- addv(p, dv("shape_uloq_log10"), "Shape ULOQ", "#E7298A", "longdash")
      p <- addv(p, dv("rdl_lower_log10"), "RDL lower", "#D95F02", "dashdot")
      p <- addv(p, dv("rdl_upper_log10"), "RDL upper", "#D95F02", "dashdot")

      # Off-plot count labels, just below the cap, on each side. low+high+unplaceable
      # equals the panel's "above 100%" total.
      anns <- list()
      lbl <- function(x, txt) list(x = x, y = YCAP, xref = "x", yref = "y",
        text = txt, showarrow = FALSE, yanchor = "top",
        font = list(size = 10, color = "#B2182B"))
      if (n_lo > 0) anns <- c(anns, list(lbl(max(xlo, xmin_pcov - 0.5 * span),
                                             sprintf("%d low pts > 100%%", n_lo))))
      if (n_hi > 0) anns <- c(anns, list(lbl(min(xhi, xmin_pcov + 0.5 * span),
                                             sprintf("%d high pts > 100%%", n_hi))))
      if (n_unp > 0) anns <- c(anns, list(lbl(mean(c(xlo, xhi)),
                                             sprintf("%d pts > 100%% (no valid conc)", n_unp))))

      plotly::layout(p, annotations = anns,
        xaxis = list(title = "Concentration", zeroline = FALSE,
                     range = c(xlo, xhi), tickmode = "array",
                     tickvals = as.list(decs), ticktext = as.list(ticktext)),
        yaxis = list(title = "Precision (%CV of concentration)", zeroline = FALSE,
                     range = c(0, YCAP)),
        legend = list(orientation = "v", x = 1.02, y = 1, xanchor = "left"),
        margin = list(r = 10))
    })

    # -- precision point-estimate panel -----------------------------------
    output$precision_panel <- shiny::renderUI({
      b <- bundle(); d <- b$diagnostics
      if (!nrow(d)) return(shiny::helpText("No diagnostics for this curve/method."))
      num <- function(x) suppressWarnings(as.numeric(x))
      gv <- function(col) { if (col %in% names(d)) { v <- num(d[[col]][1])
              if (is.finite(v)) v else NA_real_ } else NA_real_ }
      g3 <- function(v) if (is.na(v)) "\u2014" else formatC(v, format = "g", digits = 3)
      g   <- b$grid; gp <- num(g$pcov); gx <- num(g$log10_concentration)
      okp <- is.finite(gp) & is.finite(gx)
      minp <- if (any(okp)) min(gp[okp]) else NA_real_
      minx <- if (any(okp)) gx[okp][which.min(gp[okp])] else NA_real_
      thr <- gv("pcov_threshold")
      smp <- tryCatch(fetch_calib_samples(pool, curve_id(), method()), error = function(e) NULL)
      sp_pcov <- if (!is.null(smp) && nrow(smp)) num(smp$pcov) else numeric(0)
      n_samp   <- length(sp_pcov[is.finite(sp_pcov)])
      n_over_t <- if (!is.na(thr)) sum(sp_pcov > thr, na.rm = TRUE) else NA
      n_over_c <- sum(sp_pcov > 100, na.rm = TRUE)
      lloqc <- gv("lloq_conc"); uloqc <- gv("uloq_conc")
      dr_dec <- if (!is.na(gv("lloq_log10")) && !is.na(gv("uloq_log10")))
                  gv("uloq_log10") - gv("lloq_log10") else NA_real_
      dr_fold <- if (!is.na(dr_dec)) 10^dr_dec else NA_real_
      shiny::tags$div(
        style = "margin-top:6px;padding:6px 8px;background:#fafafa;border:1px solid #eee;font-size:12px;line-height:1.7;",
        shiny::tags$div(sprintf("LLOQ = %s | ULOQ = %s  (conc)", g3(lloqc), g3(uloqc))),
        shiny::tags$div(sprintf("Dynamic range = %s decades (%s-fold)", g3(dr_dec), g3(dr_fold))),
        shiny::tags$div(sprintf("pcov threshold = %s%%", g3(thr))),
        shiny::tags$div(sprintf("Best precision = %s%% at conc %s (log10 %s)",
                                g3(minp), g3(if (is.na(minx)) NA else 10^minx), g3(minx))),
        shiny::tags$div(sprintf("Samples: %d total \u00b7 %s above threshold \u00b7 %d above 100%%",
                                n_samp, if (is.na(n_over_t)) "\u2014" else as.character(n_over_t), n_over_c)))
    })

    # Shared explainer (from the sampling handoff) for the ? links here and on
    # the Compute-fits tab.
    precision_help_text <- paste(
      "The Bayesian precision profile is estimated from the model's posterior draws,",
      "and its smoothness is governed by how many draws we keep: the total is",
      "chains \u00d7 sampling, where sampling is the number of post-warmup draws per",
      "chain. Because the %CV plotted is a ratio of posterior quantities, too few",
      "draws make it wobble from point to point \u2014 much of the jaggedness is",
      "Monte-Carlo noise, not real assay behavior. Raising sampling reduces the",
      "noise roughly with the square root of the draw count, so quadrupling draws",
      "roughly halves the wobble. Increase draws through sampling rather than",
      "chains. The one thing more draws will NOT fix is the blow-up at the very low",
      "and very high ends of the curve: there the response is nearly flat, so the",
      "back-calculated concentration is genuinely ill-conditioned and the high,",
      "unstable %CV at the extremes is real \u2014 it reflects the assay's detection",
      "limits, not a sampling artifact.")
    shiny::observeEvent(input$precision_help, {
      shiny::showModal(shiny::modalDialog(
        title = "Precision profile & Bayesian sampling",
        shiny::p(precision_help_text),
        easyClose = TRUE, footer = shiny::modalButton("Close")))
    })

    # -- diagnostics / LOQ summary ----------------------------------------
    output$diagnostics_panel <- shiny::renderUI({
      b <- bundle(); d <- b$diagnostics
      if (!nrow(d)) return(shiny::helpText("No diagnostics for this curve/method."))
      loq_c <- calib_loq(d, "conc")
      loq_s <- calib_loq(d, "conc", "shape")
      best  <- if (nrow(b$fit_best)) family_label(b$fit_best$model_name[1]) else "\u2014"
      gv <- function(col) { if (col %in% names(d)) {
              v <- suppressWarnings(as.numeric(d[[col]][1])); if (is.finite(v)) v else NA_real_
            } else NA_real_ }
      g3 <- function(v) if (is.na(v)) "\u2014" else formatC(v, format = "g", digits = 3)
      shiny::tags$div(
        style = "margin-top:6px;padding:6px 8px;background:#fafafa;border:1px solid #eee;font-size:12px;line-height:1.7;",
        shiny::tags$div(shiny::tags$b("Best model: "), best),
        shiny::tags$div(sprintf("LLOQ = %s | ULOQ = %s  (conc)", g3(loq_c$lloq), g3(loq_c$uloq))),
        shiny::tags$div(sprintf("Shape LOQ (conc) = %s / %s", g3(loq_s$lloq), g3(loq_s$uloq))),
        shiny::tags$div(sprintf("LOD (response) = %s / %s",
                                g3(gv("lower_lod_response")), g3(gv("upper_lod_response")))),
        shiny::tags$div(sprintf("RDL (conc) = %s / %s",
                                g3(gv("rdl_lower_conc")), g3(gv("rdl_upper_conc")))),
        shiny::tags$div(sprintf("Inflection: x(log10) = %s, response = %s",
                                g3(gv("inflect_x")), g3(gv("inflect_y")))),
        shiny::tags$div(style = "color:#787878;margin-top:3px;",
          "Toggle any curve element on/off using the plot legend.")
      )
    })

    # =====================================================================
    # FDA 2018 standard-curve classification (frequentist-pinned; identical on
    # both method tabs). One tile per concentration level UNDER the Curve plot,
    # coloured by the 4-level verdict. Hidden when the selected curve has no
    # frequentist fit (no grid to back-calculate accuracy against).
    # =====================================================================
    has_freq <- shiny::reactive({
      cid <- tryCatch(curve_id(), error = function(e) NULL)
      if (!shiny::isTruthy(cid)) return(FALSE)
      "frequentist" %in% tryCatch(fetch_calib_methods(pool, cid), error = function(e) character(0))
    })

    fda_class <- shiny::reactive({
      calib_dirty()
      shiny::req(curve_id(), has_freq())
      tryCatch(fda2018_classify_group(pool, curve_id()), error = function(e) NULL)
    })

    # FDA 2018 verdict palette (Okabe-Ito, colourblind-safe) + legend labels.
    # Applied as POINT colour on the Curve plot; shape encodes mask state.
    FDA_COLORS <- c(PASS = "#009E73", FAIL_ACC = "#E69F00",
                    FAIL_CV = "#0072B2", FAIL_BOTH = "#D55E00", "NA" = "#999999")
    FDA_LABEL  <- c(PASS = "FDA pass", FAIL_ACC = "Fail: accuracy",
                    FAIL_CV = "Fail: CV", FAIL_BOTH = "Fail: both", "NA" = "Not evaluated")

    # Summary caption under the Curve plot (the classification itself is shown on
    # the POINTS now, not a ribbon). Muted note when the curve has no freq fit.
    output$fda_ribbon_area <- shiny::renderUI({
      if (!isTRUE(has_freq()))
        return(shiny::div(style = "margin:2px 0 6px;font-size:11px;color:#9a9a9a;",
          "FDA 2018 classification needs a frequentist fit \u2014 compute one on the Compute-fits tab."))
      fc <- fda_class(); if (is.null(fc)) return(NULL)
      s <- fc$summary
      g3 <- function(v) if (is.null(v) || is.na(v)) "\u2014" else formatC(v, format = "g", digits = 3)
      status_txt <- switch(s$status,
        OK = sprintf("LLOQ %s \u2013 ULOQ %s", g3(s$lloq_conc), g3(s$uloq_conc)),
        NO_PASSING_LEVELS = "no level passed",
        NO_FREQ_FIT = "no frequentist fit", "no data")
      shiny::div(style = "font-size:11px;color:#666;margin:2px 0 6px;",
        sprintf("FDA 2018 (frequentist curve, cross-plate): %d/%d levels pass \u00b7 %s \u00b7 meets 75%%/\u22656: %s \u00b7 hollow = masked",
                s$n_pass, s$n_total, status_txt, if (isTRUE(s$meets_fda_run)) "yes" else "no"))
    })

    # -- tables ------------------------------------------------------------
    output$fits_table <- DT::renderDT({
      f <- bundle()$fits; shiny::req(nrow(f) > 0)
      f$family <- family_label(f$model_name)
      cols <- intersect(c("family", "is_best", "converged", "eligible",
                          "score_type", "selection_score", "selection_weight",
                          "n_params"), names(f))
      DT::datatable(f[, cols, drop = FALSE], rownames = FALSE,
                    options = list(dom = "t", pageLength = 10))
    })
    output$params_table <- DT::renderDT({
      p <- bundle()$params; shiny::req(nrow(p) > 0)
      DT::datatable(p, rownames = FALSE, options = list(dom = "t"))
    })
    output$samples_table <- DT::renderDT(
      DT::datatable(bundle()$samples, rownames = FALSE,
                    options = list(pageLength = 15)))

    list(curve_id = curve_id, method = method, bundle = bundle)
  })
}
