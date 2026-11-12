# =============================================================================
# linearity_module.R  --  Quality Control > Replicate Linearity
#
# Finds the samples that were measured more than once (duplicates, several
# dilutions, several plates, both fitting methods, several curve sources),
# lets the user pick what goes on each axis, and shows how well the two sets of
# concentrations agree: 1:1 line, regression with confidence band, optional 1:1
# transform of y, optional pcov gate on each axis, and a report.
#
# All logic lives in linearity_report.R (pure functions). This file is the UI,
# the plot, and the reactive wiring. Phase 1: test samples. See that file's
# header for what phase 2 adds (standards, controls, blanks).
# =============================================================================

# ---- plot (pure: pairs + regression -> plotly) ---------------------------------
.lin_esc <- function(x) htmltools::htmlEscape(ifelse(is.na(x), "-", as.character(x)))

# Hover text: every identifying field of both points of each pair.
lin_hover <- function(d) {
  side <- function(p, lab)
    sprintf(paste0("<b>%s</b><br>plate %s · well %s<br>patientid %s · timeperiod %s<br>",
                   "specimen_id %s · sample_dilution %s<br>wavelength %s · method %s · source %s<br>",
                   "concentration %s · pcov %s · %s"),
            lab, .lin_esc(d[[paste0(p, "plate")]]), .lin_esc(d[[paste0(p, "well")]]),
            .lin_esc(d$patientid), .lin_esc(d$timeperiod),
            .lin_esc(.lin_spec(d[[paste0(p, "specimen_id")]], d[[paste0(p, "well")]])), .lin_esc(d[[paste0(p, "dilution")]]),
            .lin_esc(d[[paste0(p, "wavelength")]]), .lin_esc(d[[paste0(p, "method")]]), .lin_esc(d[[paste0(p, "source")]]),
            .lin_fmt(d[[paste0(p, "conc")]], 4), .lin_fmt(d[[paste0(p, "pcov")]], 3),
            LIN_GATE_LABEL[d[[paste0(p, "gate_class")]]])
  paste0(side("x_", "X"), "<br><br>", side("y_", "Y"),
         ifelse((is.finite(d$y_plot) & d$y_plot != d$y_conc) | (is.finite(d$x_plot) & d$x_plot != d$x_conc),
                sprintf("<br><br>After transform: X %s, Y %s", .lin_fmt(d$x_plot, 4), .lin_fmt(d$y_plot, 4)), ""),
         "<br>", ifelse(d$inc, "in the regression", "<i>not in the regression</i>"))
}

# Marker size from the pair's combined precision: sqrt(pcov_x^2 + pcov_y^2),
# 16 px at 0 down to 5 px at/after `cap`.
lin_point_size <- function(px, py, cap = 100, smin = 5, smax = 16) {
  comb <- sqrt(ifelse(is.na(px), cap, px)^2 + ifelse(is.na(py), cap, py)^2)
  smax - (smax - smin) * pmin(comb, cap) / cap
}

lin_make_plot <- function(pr, reg, scale = "log10", color_by_gate = TRUE, size_by_precision = TRUE,
                          xlab = "x", ylab = "y") {
  yp <- if (length(reg$y_plot) == nrow(pr)) reg$y_plot else pr$y_conc
  xp <- if (length(reg$x_plot) == nrow(pr)) reg$x_plot else pr$x_conc
  inc <- if (length(reg$inc) == nrow(pr)) reg$inc else rep(FALSE, nrow(pr))
  show <- is.finite(xp) & is.finite(yp) & (scale != "log10" | (xp > 0 & yp > 0))
  d <- pr[show, , drop = FALSE]; d$x_plot <- xp[show]; d$y_plot <- yp[show]; d$inc <- inc[show]
  tr <- .lin_or(reg$transform, "none")
  log_ax <- identical(scale, "log10")
  p <- plotly::plot_ly()
  if (!nrow(d))
    return(plotly::layout(plotly::add_trace(p, x = 0, y = 0, type = "scatter", mode = "markers", marker = list(opacity = 0), hoverinfo = "skip", showlegend = FALSE),
                          annotations = list(text = "No pairs to show", showarrow = FALSE, x = .5, y = .5, xref = "paper", yref = "paper")))
  lo <- min(c(d$x_plot, d$y_plot)); hi <- max(c(d$x_plot, d$y_plot))
  if (isTRUE(reg$ok)) {
    b <- reg$band
    p <- plotly::add_trace(p, x = c(b$x, rev(b$x)), y = c(b$upr, rev(b$lwr)), type = "scatter", mode = "lines",
                           fill = "toself", fillcolor = "rgba(70,110,180,0.18)", line = list(width = 0),
                           hoverinfo = "skip", name = sprintf("%d%% CI of the line", round(100 * reg$level)))
  }
  p <- plotly::add_trace(p, x = c(lo, hi), y = c(lo, hi), type = "scatter", mode = "lines",
                         line = list(color = "#222222", width = 1.5, dash = "dash"), hoverinfo = "skip", name = "1:1")
  if (isTRUE(reg$ok)) {
    b <- reg$band
    p <- plotly::add_trace(p, x = b$x, y = b$fit, type = "scatter", mode = "lines",
                           line = list(color = "#2c5aa0", width = 2.5), hoverinfo = "skip", name = "regression")
  }
  d$size <- if (size_by_precision) lin_point_size(d$x_pcov, d$y_pcov) else 9
  d$hover <- lin_hover(d)
  d$opac <- ifelse(d$inc, 0.9, 0.3)
  mk <- function(dd, fill, line, name) {
    plotly::add_trace(p, data = dd, x = ~x_plot, y = ~y_plot, type = "scatter", mode = "markers", name = name,
                      text = ~hover, hoverinfo = "text",
                      marker = list(size = dd$size, color = fill, opacity = dd$opac, line = list(color = line, width = 2.5)))
  }
  if (color_by_gate) {
    for (xg in LIN_GATES) for (yg in LIN_GATES) {
      dd <- d[d$x_gate_class == xg & d$y_gate_class == yg, , drop = FALSE]
      if (!nrow(dd)) next
      p <- mk(dd, LIN_GATE_COL[[yg]], LIN_GATE_COL[[xg]],
              sprintf("x %s / y %s (%d)", LIN_GATE_LABEL[[xg]], LIN_GATE_LABEL[[yg]], nrow(dd)))
    }
  } else {
    p <- mk(d, "#5b7fb8", "#ffffff", sprintf("pairs (%d)", nrow(d)))
  }
  ax <- function(title) list(title = title, type = if (log_ax) "log" else "linear", zeroline = FALSE, showline = TRUE)
  eq <- if (isTRUE(reg$ok)) {
    f <- reg$fit_plot
    st <- reg$stats
    paste0(lin_equation(f, scale, switch(tr, none = "y", y = "y'", harmonize = "z_y"), if (identical(tr, "harmonize")) "z_x" else "x"),
           "<br>CCC = ", .lin_fmt(st$ccc, 3), " (", .lin_fmt(st$ccc_lo, 3), " to ", .lin_fmt(st$ccc_hi, 3), ")",
           ", R² = ", .lin_fmt(reg$fit$r2, 3), ", n = ", reg$n,
           "<br>slope ", .lin_fmt(f$b, 3), " (", .lin_fmt(f$b_lo, 3), " to ", .lin_fmt(f$b_hi, 3), ")",
           if (!identical(tr, "none")) " after transform" else "")
  } else .lin_or(reg$reason, "")
  plotly::layout(p, xaxis = ax(xlab), yaxis = ax(ylab), hovermode = "closest",
                 legend = list(orientation = "h", y = -0.22, font = list(size = 10)),
                 annotations = list(list(text = eq, x = 0.01, y = 0.99, xref = "paper", yref = "paper", showarrow = FALSE,
                                         xanchor = "left", yanchor = "top", align = "left", font = list(size = 11),
                                         bgcolor = "rgba(255,255,255,0.8)")))
}

# ---- UI -------------------------------------------------------------------------
linearityUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$style(shiny::HTML(paste0(
      ".lin-card{display:inline-block;min-width:150px;margin:0 8px 8px 0;padding:8px 12px;border:1px solid #cfd8e3;",
      "border-radius:6px;background:#f7f9fc;text-align:left;cursor:pointer;}",
      ".lin-card:hover{border-color:#2c5aa0;background:#eef3fa;}",
      ".lin-card .n{font-size:20px;font-weight:600;color:#2c5aa0;display:block;line-height:1.1;}",
      ".lin-card .t{font-size:11px;color:#555;}",
      ".lin-axis{border-left:4px solid #2c5aa0;padding:4px 10px;margin-bottom:8px;background:#fafbfd;}",
      ".lin-axis.y{border-left-color:#e08a2c;}",
      ".lin-axis h5{margin:2px 0 6px;font-weight:600;}"))),
    shiny::div(class = "help-block", style = "margin:8px 0;",
      shiny::strong("How well do repeated measurements of the same sample agree?"),
      help_icon("qc.linearity.overview", ns),
      " Pairs up estimates of the same sample (duplicates, other dilutions, other plates, the two fitting ",
      "methods, other curve sources), plots one against the other with a 1:1 line, and fits a regression."),
    shiny::checkboxGroupInput(ns("kinds"), shiny::tagList("Include", help_icon("qc.linearity.kinds", ns)),
                              choices = stats::setNames(names(LIN_KINDS), unname(LIN_KINDS)), selected = "sample", inline = TRUE),
    shiny::actionButton(ns("load"), "Find replicate sets", icon = shiny::icon("search"), class = "btn-primary"),
    shiny::uiOutput(ns("kind_ui")),
    shiny::uiOutput(ns("body")))
}

# ---- server ---------------------------------------------------------------------
#' @param scope  reactive -> list(study, experiment, project_id)
#' @param fetch  function(pool, project, study, experiment) -> raw frame (injectable)
#' @param fetch_extra function(pool, project, study, experiment, kinds, samples) -> list(control, standard,
#'   check, scales_inferred): back-calculated controls and standards (injectable)
linearityServer <- function(id, pool, scope, reload_trigger = NULL, calib_dirty = NULL, fetch = lin_fetch,
                            fetch_extra = lin_fetch_extra) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    hi <- function(id) help_icon(id, ns)

    shiny::observeEvent(input$help_show, {
      hid <- input$help_show
      body <- help_modal_body(hid, ns)
      if (is.null(body)) return()
      shiny::showModal(shiny::modalDialog(title = help_modal_title(hid), body, easyClose = TRUE, size = "l", footer = shiny::modalButton("Close")))
    })
    pts_all <- shiny::reactiveVal(NULL)       # everything loaded, all sample types
    chk_txt <- shiny::reactiveVal("")         # how closely controls/standards match the worker

    # The points of one sample type at a time: estimates of a control and of a test
    # sample are never compared with each other. With one type loaded there is no choice.
    cur_kind <- shiny::reactive({
      p <- pts_all()
      if (is.null(p) || !nrow(p)) return("sample")
      kp <- intersect(names(LIN_KINDS), unique(p$kind)); k <- input$kind
      if (!is.null(k) && k %in% kp) k else kp[1]
    })
    pts <- shiny::reactive({
      p <- pts_all(); if (is.null(p)) return(NULL)
      if (length(unique(p$kind)) > 1L) p <- p[p$kind == cur_kind(), , drop = FALSE]
      p
    })

    # A new experiment, a reload or a recompute makes loaded points stale.
    shiny::observeEvent(list(scope(), if (!is.null(reload_trigger)) reload_trigger(),
                             if (!is.null(calib_dirty)) calib_dirty()), { pts_all(NULL); chk_txt("") }, ignoreInit = TRUE)

    shiny::observeEvent(input$load, {
      s <- scope()
      kinds <- intersect(names(LIN_KINDS), input$kinds %||% "sample")
      if (!length(kinds)) kinds <- "sample"
      res <- NULL
      shiny::withProgress(message = "Collecting back-calculated samples...", value = 0.3, {
        res <- tryCatch({
          raw <- if ("sample" %in% kinds) fetch(pool, s$project_id, s$study, s$experiment) else NULL
          parts <- list(lin_prepare(raw, "sample")); ex <- NULL
          extra <- intersect(kinds, c("control", "standard"))
          if (length(extra)) {
            shiny::incProgress(0.3, message = "Back-calculating controls and standards from the stored curves...")
            ex <- fetch_extra(pool, s$project_id, s$study, s$experiment, kinds = extra, samples = raw)
            parts <- c(parts, list(lin_prepare(ex$control, "control"), lin_prepare(ex$standard, "standard")))
          }
          list(pts = do.call(rbind, parts), ex = ex)
        }, error = function(e) e)
      })
      if (inherits(res, "error")) {
        shiny::showNotification(paste("Could not load samples:", conditionMessage(res)), type = "error")
        return()
      }
      pts_all(res$pts)
      chk_txt(if (is.null(res$ex)) "" else lin_backcalc_check_text(res$ex$check))
      if (!is.null(res$ex)) {
        n <- table(factor(res$pts$kind, levels = names(LIN_KINDS)))
        msg <- paste(sprintf("%s: %s estimates", tolower(LIN_KINDS[kinds]), format(as.integer(n[kinds]), big.mark = ",")), collapse = "; ")
        if (isTRUE(res$ex$scales_inferred > 0L))
          msg <- paste0(msg, sprintf(". The response scale could not be measured for %d curve(s); the experiment's usual scale was used.", res$ex$scales_inferred))
        st <- res$ex$control_stages
        if ("control" %in% kinds && !is.null(st)) {
          msg <- paste(msg, lin_control_stages_text(st), sep = "\n")
          chk_txt(paste(chk_txt(), lin_control_stages_text(st), sep = if (nzchar(chk_txt())) " | " else ""))
        }
        shiny::showNotification(msg, type = if (!is.null(st) && "control" %in% kinds && st$with_conc == 0L) "warning" else "message",
                                duration = 20)
      }
    })

    output$kind_ui <- shiny::renderUI({
      p <- pts_all(); if (is.null(p) || !nrow(p)) return(NULL)
      kp <- intersect(names(LIN_KINDS), unique(p$kind))
      if (length(kp) < 2L) return(NULL)
      keep <- shiny::isolate(input$kind)
      shiny::div(style = "margin-top:10px;max-width:360px;",
        shiny::selectInput(ns("kind"), shiny::tagList("Compare estimates of", help_icon("qc.linearity.kinds", ns)),
                           choices = stats::setNames(kp, unname(LIN_KINDS[kp])),
                           selected = if (!is.null(keep) && keep %in% kp) keep else kp[1]))
    })

    sets <- shiny::reactive({ p <- pts(); if (is.null(p)) NULL else lin_sets(p) })
    sets_all <- shiny::reactive({ p <- pts_all(); if (is.null(p)) NULL else lin_sets(p) })

    # ---- the body: built once per load, from what the data holds ----
    output$body <- shiny::renderUI({
      p <- pts(); if (is.null(p)) return(NULL)
      if (!nrow(p))
        return(shiny::div(class = "alert alert-info", style = "margin-top:10px;",
          "No back-calculated estimates were found for the types chosen. Compute fits first",
          if (any(c("control", "standard") %in% (input$kinds %||% character()))) " (controls and standards need a stored best model for the curve)",
          "."))
      st <- sets()
      sm <- lin_set_summary(st)
      first <- sm$type[sm$sets > 0][1]; if (is.na(first)) first <- "custom"
      ds <- lin_preset_sel(p, first)
      ag_n <- sort(table(st$antigen), decreasing = TRUE)
      cards <- lapply(seq_len(nrow(sm)), function(i) {
        shiny::actionLink(ns(paste0("card_", sm$type[i])), class = "lin-card",
          label = shiny::tagList(shiny::span(class = "n", format(sm$sets[i], big.mark = ",")),
                                 shiny::span(class = "t", sm$label[i])))
      })
      sel <- function(id, lab, ch, selected = NULL, ph = "any")
        shiny::selectizeInput(ns(id), lab, choices = ch, selected = selected, multiple = TRUE,
                              options = list(placeholder = ph, plugins = list("remove_button")))
      dims <- function(side, init) {
        shiny::div(class = paste("lin-axis", side),
          shiny::h5(if (side == "x") "X axis" else "Y axis"),
          shiny::fluidRow(
            shiny::column(4, sel(paste0(side, "_method"), "Method", sort(unique(p$method)), init$method)),
            shiny::column(4, sel(paste0(side, "_source"), "Source", sort(unique(ifelse(is.na(p$source), "(none)", p$source))), init$source)),
            shiny::column(4, sel(paste0(side, "_wavelength"), "Wavelength", sort(unique(ifelse(is.na(p$wavelength), "(none)", p$wavelength))), init$wavelength))),
          shiny::fluidRow(
            shiny::column(6, sel(paste0(side, "_plate"), "Plate", sort(unique(p$plate)), init$plate)),
            shiny::column(6, sel(paste0(side, "_dilution"), "Dilution", .lin_sort_dil(p$dilution), init$dilution))))
      }
      ag <- names(ag_n)
      shiny::tagList(
        shiny::h4("1. Replicate sets in this experiment", hi("qc.linearity.sets")),
        shiny::p(class = "text-muted", style = "margin-bottom:6px;",
          "A set is every estimate of the same sample (antigen, feature, patient, timeperiod; for a control, its name; ",
          "for a standard, its level). A set can be in several kinds at once. Click a kind to compare it."),
        shiny::div(cards),
        shiny::div(style = "max-width:900px;", DT::dataTableOutput(ns("tbl_sets"))),
        shiny::hr(),
        shiny::h4("2. Choose what to compare", hi("qc.linearity.sets")),
        shiny::fluidRow(
          shiny::column(3, shiny::selectInput(ns("antigen"), "Antigen", choices = c(ag, "(all)"), selected = ag[1])),
          shiny::column(3, shiny::selectInput(ns("feature"), "Feature", choices = "(all)")),
          shiny::column(3, shiny::selectizeInput(ns("timeperiod"), "Timeperiod", choices = sort(unique(p$timeperiod)), multiple = TRUE,
                                                  options = list(placeholder = "all"))),
          shiny::column(3, shiny::textInput(ns("patient"), "Patient contains", placeholder = "any"))),
        shiny::radioButtons(ns("preset"), "Comparison",
                            choices = stats::setNames(names(LIN_PRESETS), vapply(LIN_PRESETS, function(z) z$label, "")),
                            selected = first, inline = TRUE),
        shiny::uiOutput(ns("preset_detail")),
        shiny::fluidRow(shiny::column(6, dims("x", ds$x)), shiny::column(6, dims("y", ds$y))),
        shiny::fluidRow(
          shiny::column(6, shiny::checkboxGroupInput(ns("match"), "Pair two points only when these are the same",
                                                      choices = LIN_MATCH_CHOICES, selected = ds$match, inline = TRUE)),
          shiny::column(6, shiny::checkboxGroupInput(ns("differ"), "...and these are different",
                                                      choices = LIN_MATCH_CHOICES, selected = ds$differ, inline = TRUE))),
        shiny::uiOutput(ns("pair_count")),
        shiny::hr(),
        shiny::h4("3. Plot and regression", hi("qc.linearity.overview")),
        shiny::fluidRow(
          shiny::column(4,
            shiny::radioButtons(ns("error"), shiny::tagList("Which estimates carry error?", hi("qc.linearity.error_model")),
              choices = stats::setNames(names(LIN_ERROR_MODELS), c("Both (Deming / York)", "Only y (ordinary regression)")), selected = "both"),
            shiny::conditionalPanel(sprintf("input['%s'] == 'both' && input['%s'] != 'weights'", ns("error"), ns("weighting")),
              shiny::selectInput(ns("delta_mode"), shiny::tagList("Error-variance ratio (y / x)", hi("qc.linearity.delta")),
                                 c("From each axis's pcov" = "auto", "Equal: exchangeable repeats (1)" = "equal", "Custom" = "custom")),
              shiny::conditionalPanel(sprintf("input['%s'] == 'custom'", ns("delta_mode")),
                shiny::numericInput(ns("delta"), "delta", 1, min = 0.01, step = 0.1)))),
          shiny::column(4,
            shiny::radioButtons(ns("weighting"), shiny::tagList("Regression uses", hi("qc.linearity.weighting")),
              choices = stats::setNames(names(LIN_WEIGHTINGS), c("All pairs", "Pairs within the pcov gate", "All pairs, weighted by precision")),
              selected = "full"),
            shiny::fluidRow(
              shiny::column(6, shiny::numericInput(ns("x_pcov_max"), "gate: x pcov ≤", 20, min = 0, max = 150, step = 5)),
              shiny::column(6, shiny::numericInput(ns("y_pcov_max"), "gate: y pcov ≤", 20, min = 0, max = 150, step = 5))),
            shiny::tags$small(class = "text-muted", "The two options not chosen are reported as sensitivity analyses.")),
          shiny::column(4,
            shiny::radioButtons(ns("scale"), shiny::tagList("Fit and axes", hi("qc.linearity.scale")), inline = TRUE, choices = c("log10" = "log10", "natural" = "natural"), selected = "log10"),
            shiny::radioButtons(ns("transform"), shiny::tagList("Transform", hi("qc.linearity.transform")),
              choices = stats::setNames(names(LIN_TRANSFORMS), c("None", "Map y onto x (1:1)", "Harmonize both to the latent concentration")), selected = "none"),
            shiny::fluidRow(
              shiny::column(6, shiny::selectInput(ns("level"), shiny::tagList("Interval", hi("qc.linearity.intervals")), c("90%" = 0.9, "95%" = 0.95, "99%" = 0.99), selected = 0.95)),
              shiny::column(6, shiny::numericInput(ns("boot"), shiny::tagList("Bootstrap", hi("qc.linearity.intervals")), 200, min = 0, max = 2000, step = 50))))),
        shiny::fluidRow(
          shiny::column(3, shiny::checkboxInput(ns("color_gate"), "Colour by gate class", TRUE)),
          shiny::column(3, shiny::checkboxInput(ns("size_prec"), "Size by precision", TRUE))),
        shiny::p(class = "text-muted", style = "font-size:12px;",
          "Fill = the y point's gate class, outline = the x point's. Larger = both estimates more precise (lower pcov). ",
          "Faded = not in the regression. Dashed line = 1:1."),
        shinycssloaders::withSpinner(plotly::plotlyOutput(ns("plot"), height = "620px"), type = 4, color = "#337ab7"),
        shiny::uiOutput(ns("reg_summary")),
        shiny::hr(),
        shiny::h4("4. Report and pairs"),
        shiny::uiOutput(ns("report_actions")),
        shiny::uiOutput(ns("report_preview")),
        shiny::h5("Consensus per sample", hi("qc.linearity.consensus")),
        shiny::p(class = "text-muted", style = "font-size:12px;",
          "Needs 'Both' carry error. Each sample's estimates are harmonized to the latent concentration and combined by inverse-variance weighting. ",
          "Q/df is about 1 when pcov accounts for all the disagreement."),
        DT::dataTableOutput(ns("tbl_consensus")),
        shiny::downloadButton(ns("dl_consensus"), "Consensus (.csv)", class = "btn-xs"),
        shiny::h5("Pairs"),
        shiny::div(style = "margin-top:10px;", DT::dataTableOutput(ns("tbl_pairs"))),
        shiny::downloadButton(ns("dl_pairs"), "Pairs (.csv)", class = "btn-xs"))
    })

    # ---- set overview tables ----
    output$tbl_sets <- DT::renderDataTable({
      st <- sets_all(); req(st)
      d <- lin_set_summary_by_antigen(st)
      lab <- c(antigen = "Antigen", kind = "Type", sets = "Sets", LIN_SET_TYPES)
      names(d) <- lab[names(d)]
      DT::datatable(d, rownames = FALSE, selection = "none", options = list(dom = "tp", pageLength = 8))
    })

    # ---- presets ----
    apply_preset <- function(key) {
      p <- pts_f(); if (is.null(p)) p <- pts()
      s <- lin_preset_sel(p, key)
      for (side in c("x", "y")) for (d in LIN_DIMS)
        shiny::updateSelectizeInput(session, paste0(side, "_", d), selected = .lin_or(s[[side]][[d]], character(0)))
      shiny::updateCheckboxGroupInput(session, "match", selected = s$match)
      shiny::updateCheckboxGroupInput(session, "differ", selected = s$differ)
    }
    shiny::observeEvent(input$preset, apply_preset(input$preset), ignoreInit = TRUE)
    for (k in names(LIN_SET_TYPES)) local({
      key <- k
      shiny::observeEvent(input[[paste0("card_", key)]], shiny::updateRadioButtons(session, "preset", selected = key),
                          ignoreInit = TRUE)
    })
    output$preset_detail <- shiny::renderUI({
      k <- shiny::req(input$preset)
      shiny::p(class = "text-muted", style = "margin-top:-6px;", LIN_PRESETS[[k]]$detail)
    })

    # Features follow the antigen.
    shiny::observeEvent(list(pts(), input$antigen), {
      p <- pts(); req(p, input$antigen)
      f <- if (identical(input$antigen, "(all)")) p$feature else p$feature[p$antigen == input$antigen]
      shiny::updateSelectInput(session, "feature", choices = c("(all)", sort(unique(f[!is.na(f)]))), selected = "(all)")
    })

    # ---- the points after the scope filters ----
    pts_f <- shiny::reactive({
      p <- pts(); req(p, input$antigen)
      if (!identical(input$antigen, "(all)")) p <- p[p$antigen == input$antigen, , drop = FALSE]
      if (!is.null(input$feature) && !input$feature %in% c("", "(all)")) p <- p[!is.na(p$feature) & p$feature == input$feature, , drop = FALSE]
      if (length(input$timeperiod)) p <- p[!is.na(p$timeperiod) & p$timeperiod %in% input$timeperiod, , drop = FALSE]
      if (nzchar(.lin_or(input$patient, ""))) p <- p[!is.na(p$patientid) & grepl(input$patient, p$patientid, fixed = TRUE), , drop = FALSE]
      p
    })
    sel_of <- function(side) {
      out <- lapply(stats::setNames(LIN_DIMS, LIN_DIMS), function(d) input[[paste0(side, "_", d)]])
      out[vapply(out, length, integer(1)) > 0]
    }
    pairs <- shiny::reactive({
      p <- pts_f()
      lin_pairs(p, sel_of("x"), sel_of("y"), match = input$match %||% character(), differ = input$differ %||% character())
    })
    output$pair_count <- shiny::renderUI({
      pr <- pairs(); n_sets <- length(unique(pr$set_id))
      if (!nrow(pr))
        return(shiny::div(class = "alert alert-warning", style = "padding:6px 10px;",
          "No pairs match these choices. Loosen what must match, or pick other values for the axes."))
      shiny::div(class = "alert alert-success", style = "padding:6px 10px;",
        sprintf("%s pairs from %s sets.", format(nrow(pr), big.mark = ","), format(n_sets, big.mark = ",")),
        if (isTRUE(attr(pr, "truncated"))) " Capped: narrow the selection to see all pairs.")
    })

    gate_x <- shiny::reactive(.lin_or(input$x_pcov_max, 20))
    gate_y <- shiny::reactive(.lin_or(input$y_pcov_max, 20))
    n_boot <- shiny::reactive(as.integer(.lin_or(input$boot, 200L)))
    delta_spec <- shiny::reactive({
      switch(input$delta_mode %||% "auto", equal = 1, custom = .lin_or(input$delta, 1), "auto")
    })
    reg <- shiny::reactive({
      pr <- pairs()
      lin_regress(pr, scale = input$scale %||% "log10", error = input$error %||% "both",
                  weighting = input$weighting %||% "full", delta = delta_spec(),
                  x_pcov_max = gate_x(), y_pcov_max = gate_y(),
                  level = as.numeric(.lin_or(input$level, 0.95)), transform = input$transform %||% "none", B = n_boot())
    })
    sens <- shiny::reactive({
      r <- reg(); if (!isTRUE(r$ok)) return(NULL)
      lin_sensitivity(pairs(), r, gate_x(), gate_y(), B = min(n_boot(), 100L))
    })
    consensus <- shiny::reactive(lin_consensus(pairs(), reg()))

    axis_label <- function(sel, which) {
      t <- .lin_sel_text(sel)
      sprintf("%s: %s concentration", which, if (identical(t, "any")) "all points" else t)
    }
    output$plot <- plotly::renderPlotly({
      pr <- pairs(); rg <- reg()
      ylab <- axis_label(sel_of("y"), "Y"); xlab <- axis_label(sel_of("x"), "X")
      if (!is.null(rg$tf)) {
        if (identical(rg$tf$kind, "harmonize")) { ylab <- paste0(ylab, " (harmonized, latent scale)"); xlab <- paste0(xlab, " (harmonized, latent scale)") }
        else ylab <- paste0(ylab, " (mapped onto x, 1:1)")
      }
      lin_make_plot(pr, rg, scale = input$scale %||% "log10", color_by_gate = isTRUE(input$color_gate),
                    size_by_precision = isTRUE(input$size_prec), xlab = xlab, ylab = ylab)
    })

    output$reg_summary <- shiny::renderUI({
      rg <- reg(); ss <- sens(); cs <- consensus(); pr <- pairs()
      fnd <- lin_assess(rg, list(sens = ss, pair_types = unique(pr$pair_type),
                                 off_range_share = if (nrow(pr)) mean(pr$x_gate_class != "inDynamicRange" | pr$y_gate_class != "inDynamicRange") else NA_real_))
      sev <- lin_severity(fnd)
      cls <- switch(sev, error = "alert-danger", warn = "alert-warning", "alert-info")
      stat <- NULL
      if (isTRUE(rg$ok)) {
        f <- rg$fit; s <- rg$stats
        row <- function(k, v) shiny::tags$tr(shiny::tags$td(style = "padding:1px 14px 1px 0;color:#555;", k), shiny::tags$td(v))
        stat <- shiny::tagList(
          shiny::tags$p(style = "font-family:monospace;font-size:14px;margin:8px 0;", lin_equation(f, rg$scale)),
          if (!is.null(rg$tf)) shiny::tags$p(style = "font-family:monospace;", rg$tf$formula,
                                              sprintf(" with a = %s, b = %s (the plotted line is the refit)", .lin_fmt(rg$tf$a, 5), .lin_fmt(rg$tf$b, 5))),
          if (!is.null(rg$transform_note)) shiny::tags$p(class = "text-warning", rg$transform_note),
          shiny::tags$h5("Agreement", hi("qc.linearity.agreement")),
          shiny::tags$table(
            row("Concordance (CCC)", sprintf("%s  (%d%% CI %s to %s): %s", .lin_fmt(s$ccc, 3), round(100 * rg$level), .lin_fmt(s$ccc_lo, 3), .lin_fmt(s$ccc_hi, 3), lin_ccc_label(s$ccc))),
            row("Bland-Altman bias", if (rg$scale == "log10") sprintf("%s-fold  (CI %s to %s)", .lin_fmt(10^s$bias, 3), .lin_fmt(10^s$bias_lo, 3), .lin_fmt(10^s$bias_hi, 3))
                                      else sprintf("%s  (CI %s to %s)", .lin_fmt(s$bias), .lin_fmt(s$bias_lo), .lin_fmt(s$bias_hi))),
            row("Limits of agreement", if (rg$scale == "log10") sprintf("%s-fold to %s-fold", .lin_fmt(10^s$loa_lo, 3), .lin_fmt(10^s$loa_hi, 3))
                                        else sprintf("%s to %s", .lin_fmt(s$loa_lo), .lin_fmt(s$loa_hi))),
            row("Trend of difference with level", sprintf("slope %s, p %s", .lin_fmt(s$trend_slope), .lin_p(s$trend_p))),
            row("Correlation", sprintf("r %s, R² %s, Spearman %s", .lin_fmt(s$pearson), .lin_fmt(f$r2), .lin_fmt(s$spearman)))),
          shiny::tags$h5("Fitted line"),
          shiny::tags$table(
            row("Pairs used", sprintf("%d of %d (%d removed by the gate, %d without pcov, %d non-finite)", rg$n, rg$n_pairs, rg$n_gated, rg$n_nopcov, rg$n_invalid)),
            if (!is.na(rg$delta)) row("Error-variance ratio", sprintf("%s (%s)", .lin_fmt(rg$delta, 4), rg$delta_source)),
            row("Intercept a", sprintf("%s  (%d%% CI %s to %s; p(a=0) %s)", .lin_fmt(f$a), round(100 * rg$level), .lin_fmt(f$a_lo), .lin_fmt(f$a_hi), .lin_p(f$p_int0))),
            row("Slope b", sprintf("%s  (%d%% CI %s to %s; p(b=1) %s)", .lin_fmt(f$b), round(100 * rg$level), .lin_fmt(f$b_lo), .lin_fmt(f$b_hi), .lin_p(f$p_slope1))),
            row("Intervals", f$ci_method)),
          if (!is.null(rg$replicate)) shiny::tags$p(class = "text-muted", style = "margin-top:6px;",
            sprintf("Duplicates: within-pair SD %s%s, ICC %s (%d pairs).", .lin_fmt(rg$replicate$sd_within),
                    if (is.na(rg$replicate$cv)) "" else sprintf(" (CV %.1f%%)", 100 * rg$replicate$cv), .lin_fmt(rg$replicate$icc), rg$replicate$n_pairs)),
          if (!is.null(ss)) shiny::tagList(
            shiny::tags$h5("Sensitivity to weighting", hi("qc.linearity.weighting")),
            shiny::tags$table(class = "table table-condensed", style = "max-width:900px;",
              shiny::tags$thead(shiny::tags$tr(lapply(c("Regression uses", "n", "Slope b (CI)", "Intercept a", "CCC (CI)", "Bias"), shiny::tags$th))),
              shiny::tags$tbody(lapply(seq_len(nrow(ss)), function(i) {
                z <- ss[i, ]
                shiny::tags$tr(style = if (z$primary) "font-weight:600;background:#eef3fa;" else NULL,
                  shiny::tags$td(paste0(LIN_WEIGHTINGS[[z$weighting]], if (z$primary) " (shown)" else "")), shiny::tags$td(z$n),
                  shiny::tags$td(sprintf("%s (%s to %s)", .lin_fmt(z$b, 3), .lin_fmt(z$b_lo, 3), .lin_fmt(z$b_hi, 3))),
                  shiny::tags$td(.lin_fmt(z$a, 3)),
                  shiny::tags$td(sprintf("%s (%s to %s)", .lin_fmt(z$ccc, 3), .lin_fmt(z$ccc_lo, 3), .lin_fmt(z$ccc_hi, 3))),
                  shiny::tags$td(if (rg$scale == "log10") sprintf("%s-fold", .lin_fmt(10^z$bias, 3)) else .lin_fmt(z$bias)))
              })))),
          if (!is.null(cs)) shiny::tags$p(class = "text-muted",
            sprintf("Consensus: %d samples combined by inverse-variance weighting after harmonizing (table below, and in the report).", nrow(cs))))
      }
      shiny::tagList(stat,
        if (length(fnd)) shiny::div(class = paste("alert", cls), style = "margin-top:8px;",
          shiny::tags$ul(style = "margin:0;padding-left:18px;", lapply(fnd, function(z)
            shiny::tags$li(shiny::strong(z$message), if (!is.null(z$evidence)) shiny::tags$small(paste0(" — ", z$evidence)))))))
    })

    # ---- report ----
    report <- shiny::reactive({
      p <- pts_f(); s <- scope()
      lin_report_build(list(
        scope = list(project_id = s$project_id, study = s$study, experiment = s$experiment),
        preset = LIN_PRESETS[[.lin_or(input$preset, "custom")]]$label,
        x = sel_of("x"), y = sel_of("y"), match = input$match %||% character(), differ = input$differ %||% character(),
        kind = cur_kind(), backcalc = chk_txt(),
        filters = list(antigen = input$antigen, feature = input$feature, timeperiod = input$timeperiod, patient = input$patient),
        sets = lin_sets(p), pairs = pairs(), reg = reg(), sens = sens(), consensus = consensus(),
        n_gate_inferred = sum(p$gate_inferred)))
    })
    text <- shiny::reactive(lin_report_text(report()))
    output$report_actions <- shiny::renderUI({
      shiny::div(style = "margin:6px 0;",
        shiny::downloadButton(ns("dl_md"), "Report (.md)", class = "btn-xs"),
        shiny::downloadButton(ns("dl_json"), "Report (.json)", class = "btn-xs"),
        shiny::tags$button(type = "button", class = "btn btn-default btn-xs",
          onclick = sprintf("navigator.clipboard.writeText(document.getElementById('%s').innerText)", ns("preview")),
          shiny::icon("copy"), " Copy text"))
    })
    output$report_preview <- shiny::renderUI({
      shiny::tags$details(shiny::tags$summary("Show the report"),
        shiny::tags$pre(id = ns("preview"), style = "max-height:420px;overflow:auto;white-space:pre-wrap;font-size:11px;", text()))
    })
    fname <- function(ext) {
      s <- scope()
      sprintf("replicate_linearity_%s_%s_%s.%s", gsub("[^A-Za-z0-9]+", "_", s$study),
              gsub("[^A-Za-z0-9]+", "_", s$experiment), format(Sys.Date(), "%Y%m%d"), ext)
    }
    output$dl_md   <- shiny::downloadHandler(filename = function() fname("md"), content = function(file) writeLines(text(), file, useBytes = TRUE))
    output$dl_json <- shiny::downloadHandler(filename = function() fname("json"),
      content = function(file) writeLines(as.character(lin_report_json(report())), file, useBytes = TRUE))

    cons_out <- shiny::reactive({
      cs <- consensus(); if (is.null(cs)) return(NULL)
      cs[, setdiff(names(cs), "set_id")]
    })
    output$tbl_consensus <- DT::renderDataTable({
      d <- cons_out()
      if (is.null(d)) return(DT::datatable(data.frame(note = "Choose 'Both' carry error, with a positive slope."), rownames = FALSE, options = list(dom = "t")))
      num <- c("consensus", "z", "se_z", "lo", "hi", "q", "q_ratio")
      DT::formatSignif(DT::datatable(d, rownames = FALSE, filter = "top", options = list(pageLength = 8, scrollX = TRUE, dom = "ltip")), num, 4)
    })
    output$dl_consensus <- shiny::downloadHandler(filename = function() sub("^replicate_linearity", "consensus", fname("csv")),
      content = function(file) utils::write.csv(cons_out() %||% data.frame(), file, row.names = FALSE, na = ""))

    pairs_out <- shiny::reactive({
      pr <- pairs(); rg <- reg()
      if (!nrow(pr)) return(pr)
      pr$x_conc_transformed <- if (length(rg$x_plot) == nrow(pr)) rg$x_plot else NA_real_
      pr$y_conc_transformed <- if (length(rg$y_plot) == nrow(pr)) rg$y_plot else NA_real_
      pr$in_regression <- if (length(rg$inc) == nrow(pr)) rg$inc else NA
      pr[, setdiff(names(pr), c("x_pt_id", "y_pt_id", "x_meas_id", "y_meas_id", "x_curve_id", "y_curve_id", "x_dilution_num", "y_dilution_num"))]
    })
    output$tbl_pairs <- DT::renderDataTable({
      d <- pairs_out(); req(nrow(d) > 0)
      DT::datatable(d, rownames = FALSE, filter = "top", options = list(pageLength = 10, scrollX = TRUE, dom = "ltip"))
    })
    output$dl_pairs <- shiny::downloadHandler(filename = function() fname("csv"),
      content = function(file) utils::write.csv(pairs_out(), file, row.names = FALSE, na = ""))

    list(points = pts, pairs = pairs, regression = reg, report = report, report_text = text)
  })
}
