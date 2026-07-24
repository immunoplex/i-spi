# =============================================================================
# std_curve_module.R  --  the restructured standard-curve subsystem, as a
#                         proper Shiny module (NS/moduleServer)
# -----------------------------------------------------------------------------
# This replaces the flat, sourced std_curver_ui.R. It does NO curve fitting: all
# results come from calib_* via calib_data_access.R, and all (re)computation is
# delegated to the workers via compute_api_client.R. There is ONE visualization
# (built from calib_grid), toggled by method (bayesian | frequentist) rather than
# the old side-by-side bayes-vs-freq machinery.
#
# DEPENDS (sourced elsewhere by app.R, same as the other modules):
#   calib_data_access.R   fetch_calib_bundle(), calib_available_models(),
#                         fetch_curve_lookup(), calib_loq(), family_label/short(),
#                         fetch_antigen_feature_settings(), parse_model_form_list(),
#                         model_list_to_param(), CALIB_FAMILY, CALIB_NK_COLS
#   compute_api_client.R  compute_api_client(), is_terminal_status()
# PACKAGES: shiny, plotly, DT
#
# This is a SKELETON: the reactive wiring, data-access calls, and API calls are
# real; a few domain-specific rendering choices are marked TODO.
# =============================================================================

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
#' @param id module id
stdCurveModuleUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(
      width = 3,
      shiny::wellPanel(
        shiny::h4("Standard curve"),

        # --- curve selection (natural-key navigation) -------------------
        shiny::selectizeInput(ns("study"),      "Study",      choices = NULL),
        shiny::selectizeInput(ns("experiment"), "Experiment", choices = NULL),
        shiny::selectizeInput(ns("antigen"),    "Antigen",    choices = NULL),
        # Multiple curves can share study/experiment/antigen (plate, feature,
        # wavelength, dilution, source); this disambiguates to one curve_id.
        shiny::selectizeInput(ns("curve"),      "Curve",      choices = NULL),

        shiny::tags$hr(),

        # --- method + model ---------------------------------------------
        shiny::radioButtons(ns("method"), "Method",
          choices = c("Bayesian" = "bayesian", "Frequentist" = "frequentist"),
          selected = "bayesian", inline = TRUE),
        # DISPLAY model: which already-fit family to show. Populated from
        # calib_fit (data-driven, up to all five families).
        shiny::selectizeInput(ns("model"), "Displayed model (best pre-selected)",
                              choices = NULL),

        shiny::tags$hr(),

        # --- (re)compute via the workers --------------------------------
        # Models to FIT next: user-controlled list, defaulted from
        # antigen_feature_settings.model_form_list (curveRcore names).
        # Editing here changes only THIS submission; persisting the default back
        # to antigen_feature_settings belongs to the settings/config module.
        shiny::selectizeInput(ns("model_form"), "Models to fit (feature settings)",
                              choices = NULL, multiple = TRUE),
        shiny::selectInput(ns("submit_scope"), "Re-fit scope",
                           choices = c("study", "experiment", "antigen"),
                           selected = "experiment"),
        shiny::actionButton(ns("submit"), "Submit fit job",
                            class = "btn-primary btn-sm"),
        shiny::uiOutput(ns("job_status"))
      )
    ),

    shiny::column(
      width = 9,
      # Compute/status ALWAYS on top; the plot area appears below only once
      # calib_* data exists for the selected curve+method.
      shiny::uiOutput(ns("sc_status")),
      shiny::uiOutput(ns("sc_plot_area"))
    )
  )
}

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
#' @param id     module id
#' @param conn   a DBI connection or pool::Pool (passed straight to data-access)
#' @param api    a compute_api_client() object (defaults to one from env)
#' @param scope  optional reactive returning list(project_id, source) context
#'               for job submission; if NULL, submission uses the selected NK.
#' @return list(curve_id = reactive, method = reactive, bundle = reactive)
stdCurveServer <- function(id, conn, api = compute_api_client(), scope = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    # -- registry pulled once; NK navigation is just filtering in R --------
    lookup <- shiny::reactive(fetch_curve_lookup(conn))

    # Cascade study -> experiment -> antigen -> curve from the lookup NK.
    shiny::observe({
      lk <- lookup()
      shiny::updateSelectizeInput(session, "study",
        choices = sort(unique(lk$study_accession)), server = TRUE)
    })
    shiny::observeEvent(input$study, {
      lk <- lookup(); lk <- lk[lk$study_accession == input$study, ]
      shiny::updateSelectizeInput(session, "experiment",
        choices = sort(unique(lk$experiment_accession)), server = TRUE)
    })
    shiny::observeEvent(list(input$study, input$experiment), {
      lk <- lookup()
      lk <- lk[lk$study_accession == input$study &
               lk$experiment_accession == input$experiment, ]
      shiny::updateSelectizeInput(session, "antigen",
        choices = sort(unique(lk$antigen)), server = TRUE)
    })

    # Curves matching the chosen study/experiment/antigen (may be several).
    matching_curves <- shiny::reactive({
      shiny::req(input$study, input$experiment, input$antigen)
      lk <- lookup()
      lk[lk$study_accession == input$study &
         lk$experiment_accession == input$experiment &
         lk$antigen == input$antigen, ]
    })
    shiny::observeEvent(matching_curves(), {
      mc <- matching_curves()
      # Label each curve by the remaining NK dims that distinguish it.
      labels <- with(mc, paste0(
        "plate=", plateid, " | feat=", feature,
        " | dil=", nominal_sample_dilution, " | \u03bb=", wavelength))
      choices <- setNames(as.character(mc$curve_id), labels)
      shiny::updateSelectizeInput(session, "curve", choices = choices)
    })

    # curve_id carried as character (bigint-safe); data-access binds it as-is.
    curve_id <- shiny::reactive({ shiny::req(input$curve); input$curve })
    method   <- shiny::reactive(input$method)

    # The full NK row for the chosen curve (project_id, feature, source, ...).
    selected_curve_row <- shiny::reactive({
      mc <- matching_curves(); shiny::req(nrow(mc) > 0, input$curve)
      mc[as.character(mc$curve_id) == input$curve, , drop = FALSE][1, ]
    })

    # Per-antigen/feature analysis settings (model_form_list, std conc, pcov
    # threshold, constraints). Drives the "models to fit" default + job params.
    af_settings <- shiny::reactive({
      r <- selected_curve_row(); shiny::req(nrow(r) == 1)
      fetch_antigen_feature_settings(conn,
        project_id = r$project_id, study = input$study, antigen = input$antigen,
        experiment = input$experiment, feature = r$feature)
    })

    # Default the "models to fit" list from settings (already curveRcore names
    # post-migration; parse_model_form_list still tolerates legacy stragglers).
    shiny::observeEvent(af_settings(), {
      s <- af_settings()
      configured <- if (nrow(s)) parse_model_form_list(s$model_form_list) else character(0)
      all5 <- CALIB_FAMILY$model_name
      shiny::updateSelectizeInput(session, "model_form",
        choices = setNames(all5, family_short(all5)), selected = configured)
    })

    # -- model selector: data-driven from calib_fit -----------------------
    shiny::observeEvent(list(curve_id(), method()), {
      models <- calib_available_models(conn, curve_id(), method())
      # Best model sorts first (see calib_available_models); pre-select it.
      shiny::updateSelectizeInput(session, "model",
        choices = setNames(models, family_short(models)),
        selected = if (length(models)) models[1] else NULL)
    })

    # -- the one bundle every panel reads from ----------------------------
    # reload_tick lets a completed job force a refetch without changing inputs.
    reload_tick <- shiny::reactiveVal(0)
    bundle <- shiny::reactive({
      reload_tick()
      shiny::req(curve_id(), method())
      fetch_calib_bundle(conn, curve_id(), method())
    })

    # Is there a fitted calibration to show for this curve+method?
    has_calib <- shiny::reactive({
      b <- bundle(); nrow(b$fit_best) > 0 && nrow(b$grid) > 0
    })

    ns <- session$ns  # needed to build namespaced ids inside renderUI

    # -- status banner (ALWAYS on top, above any plot) --------------------
    # Shows job progress when a fit is running, a compute prompt when nothing is
    # fitted yet, or a compact "fitted" summary once data exists.
    output$sc_status <- shiny::renderUI({
      st  <- job_state()
      running <- !is.null(st) && !is_terminal_status(st)
      if (running || identical(st, "failed")) {
        return(shiny::div(class = "well", style = "background:#fff8e1;",
          shiny::strong(sprintf("Compute job %s: %s",
            if (is.null(job_id())) "?" else job_id(), st))))
      }
      if (!isTRUE(has_calib())) {
        return(shiny::div(class = "well", style = "background:#f5f5f5;",
          shiny::strong("No fitted calibration for this curve/method yet."),
          shiny::br(),
          shiny::span("Set the models to fit in the panel at left and press ",
                      shiny::strong("Submit fit job"),
                      " to run it in the compute worker; results appear here when done.")))
      }
      b <- bundle()
      shiny::div(style = "margin:4px 0 8px;",
        shiny::strong("Fitted: "),
        family_label(b$fit_best$model_name[1]),
        shiny::span(sprintf("  \u00b7  %s", method())),
        if (!is.null(st)) shiny::span(sprintf("  \u00b7  last job: %s", st)))
    })

    # -- plot area: rendered ONLY when calib_* data exists ----------------
    output$sc_plot_area <- shiny::renderUI({
      if (!isTRUE(has_calib())) return(NULL)   # no plots before there is data
      shiny::tabsetPanel(
        shiny::tabPanel("Curve",
          plotly::plotlyOutput(ns("curve_plot"), height = "460px"),
          shiny::uiOutput(ns("diagnostics_panel"))),
        shiny::tabPanel("Model selection", DT::DTOutput(ns("fits_table"))),
        shiny::tabPanel("Parameters",      DT::DTOutput(ns("params_table"))),
        shiny::tabPanel("Back-calculated samples", DT::DTOutput(ns("samples_table")))
      )
    })

    # -- curve plot (single visualization, built from calib_grid) ----------
    output$curve_plot <- plotly::renderPlotly({
      b <- bundle(); shiny::req(nrow(b$grid) > 0)
      g   <- b$grid[order(b$grid$log10_concentration), ]
      loq <- calib_loq(b$diagnostics, "log10")

      p <- plotly::plot_ly(g, x = ~log10_concentration)
      p <- plotly::add_ribbons(p, ymin = ~ci_lower, ymax = ~ci_upper,
                               name = "95% band", line = list(width = 0),
                               opacity = 0.25, hoverinfo = "skip")
      p <- plotly::add_lines(p, y = ~predicted_response, name = "Fitted")
      # LLOQ / ULOQ guide lines (log10 scale to match the axis).
      shapes <- list()
      addv <- function(xval, col) list(type = "line", x0 = xval, x1 = xval,
        y0 = 0, y1 = 1, yref = "paper", line = list(dash = "dot", color = col))
      if (!is.na(loq$lloq)) shapes <- c(shapes, list(addv(loq$lloq, "#B2182B")))
      if (!is.na(loq$uloq)) shapes <- c(shapes, list(addv(loq$uloq, "#B2182B")))
      # TODO: overlay observed standards from xmap_standard (raw points) once a
      # raw-data access module exists; the grid alone has no observed y.
      plotly::layout(p, shapes = shapes,
        xaxis = list(title = "log10(concentration)"),
        yaxis = list(title = "Response (MFI)"),
        legend = list(orientation = "h"))
    })

    # -- diagnostics / LOQ summary ----------------------------------------
    output$diagnostics_panel <- shiny::renderUI({
      b <- bundle(); d <- b$diagnostics
      if (!nrow(d)) return(shiny::helpText("No diagnostics for this curve/method."))
      loq_c <- calib_loq(d, "conc")
      best  <- if (nrow(b$fit_best)) family_label(b$fit_best$model_name[1]) else "\u2014"
      shiny::tagList(
        shiny::tags$b("Best model: "), best, shiny::tags$br(),
        sprintf("LLOQ = %.3g | ULOQ = %.3g (conc)", loq_c$lloq, loq_c$uloq),
        shiny::tags$br(),
        # TODO: surface LOD / RDL / inflection from the 34-col diagnostics row.
        shiny::helpText("LOD, RDL, inflection available in calib_diagnostics.")
      )
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

    # -- submit + poll a worker job ---------------------------------------
    job_id     <- shiny::reactiveVal(NULL)
    job_state  <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$submit, {
      r      <- selected_curve_row()
      s      <- af_settings()
      models <- input$model_form
      if (!length(models)) { job_state("pick at least one model to fit"); return() }
      # params is a passthrough to the worker CLI; `models` is the known flag.
      # pcov_threshold is sent as cdan_cv_threshold (same quantity, confirmed).
      # TODO: map standard_curve_concentration / l_asy_* constraints from
      # af_settings() to the worker's actual CLI flag names.
      res <- tryCatch(
        api$submit_job(
          project_id  = r$project_id,
          study       = input$study,
          experiment  = input$experiment,
          antigen     = input$antigen,
          source      = r$source,
          scope       = input$submit_scope,
          script_type = method(),
          params      = list(models = model_list_to_param(models)),
          cdan_cv_threshold =
            if (nrow(s) && !is.na(s$pcov_threshold)) s$pcov_threshold else NULL),
        error = function(e) { job_state(paste("submit failed:", conditionMessage(e))); NULL })
      if (!is.null(res)) { job_id(res$job_id); job_state("queued") }
    })

    # Poll every 3s until the job reaches a terminal state; then refetch.
    shiny::observe({
      jid <- job_id(); shiny::req(jid)
      if (is_terminal_status(job_state())) return()
      shiny::invalidateLater(3000, session)
      st <- tryCatch(api$get_job(jid), error = function(e) NULL)
      if (!is.null(st)) {
        job_state(st$status)
        if (identical(st$status, "completed")) reload_tick(reload_tick() + 1)
      }
    })

    output$job_status <- shiny::renderUI({
      s <- job_state(); if (is.null(s)) return(NULL)
      jid <- job_id(); if (is.null(jid)) jid <- "?"
      shiny::helpText(sprintf("Job %s: %s", jid, s))
    })

    # Expose current selection so sibling modules can react (study overview,
    # dilutional linearity, etc. will consume the same calib_* boundary).
    list(curve_id = curve_id, method = method, bundle = bundle)
  })
}
