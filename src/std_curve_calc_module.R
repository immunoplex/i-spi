# =============================================================================
# std_curve_calc_module.R  --  the CALCULATE side: submit fits + status.
#
# Split out of the former monolithic std_curve_module.R (pure refactor: behavior
# unchanged). This half OWNS calculating: the fit-engine/scope/target controls,
# scope->curve_id-batch resolution, submit to i-spi-compute, poll-to-completion,
# and the experiment calculation-status table (later: the categorical heatmap).
#
# Contract with the parent / sibling view module (shared reactiveVals):
#   scope        : reactive(list(study, experiment, project_id))  -- app scope
#   calib_dirty  : reactiveVal(int)  -- this module BUMPS it when a job completes
#                  (calib_* changed) so the view re-reads. (Future: mask also bumps.)
#   selected_curve : reactiveVal(list(curve_id, method)|NULL) -- read only (a
#                  heatmap click will WRITE it; view observes). Reserved for the
#                  heatmap build.
# =============================================================================

# Bayesian sampling defaults (post-warmup draws per chain / chains / warmup).
# Single source of truth so a future "Precision resolution" preset can override
# it. Worker built-in is sampling=1000; we default to 1500 (= 6000 draws over 4
# chains) to smooth the precision profile. Only sent for script_type=bayesian.
DEFAULT_BAYES_SAMPLING <- 1500L
DEFAULT_BAYES_CHAINS   <- 4L
DEFAULT_BAYES_WARMUP   <- 1000L

# ---------------------------------------------------------------------------
# TEMPORARY submit/queue diagnostics, rendered into the job-status box under the
# Submit / Check-now buttons. Set to FALSE (or delete this constant, the
# `job_batch`/`job_diag` reactives, and the diag block in output$job_status) once
# the "stuck in queued / never runs" issue is resolved.
#   * standard_for_fit row count for THIS job's curve_ids -- the worker exits
#     BEFORE it ever writes 'running' when this is 0 (worker_curveR.R line 329),
#     so a 0 here with the job stuck in 'queued' pinpoints the empty-view path.
#   * blank_/sample_for_fit counts, the full JobStatus fields, and a service-wide
#     queued/running snapshot (rows-present + 0 running => block is the consumer).
# Mirrors the worker's own schema-qualified view names exactly.
JOB_DIAG_VERBOSE     <- isTRUE(getOption("ispi.calc_job_diag", TRUE))
JOB_DIAG_FIT_SCHEMA  <- getOption("ispi.calib_schema", "madi_results")

stdCurveCalcUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(
      width = 3,
      shiny::wellPanel(
        shiny::h4("Compute fits"),
        shiny::tags$p(shiny::tags$strong("Calculate (submit a fit job)"),
                      style = "margin-bottom:4px;color:#555;"),
        shiny::selectizeInput(ns("model_form"), "Models to fit (feature settings)",
                              choices = NULL, multiple = TRUE),
        shiny::radioButtons(ns("fit_engine"), "Fit engine",
                            choices = c("Bayesian" = "bayesian",
                                        "Frequentist" = "frequentist"),
                            selected = "frequentist", inline = TRUE),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'bayesian'", ns("fit_engine")),
          shiny::tags$div(
            shiny::selectInput(ns("bayes_precision"), "Precision resolution (Bayesian)",
              choices = c("Fast (1000 draws/chain)"      = "1000",
                          "Standard (1500)"              = "1500",
                          "High (3000)"                  = "3000",
                          "Very high (6000)"             = "6000"),
              selected = "1500"),
            shiny::actionLink(ns("bayes_help"), "?",
              style = "font-weight:bold;color:#337ab7;"),
            shiny::tags$span(style = "font-size:11px;color:#787878;",
              " more draws \u2192 smoother precision profile; costs runtime."),
            shiny::tags$div(style = "margin-top:8px;",
              shiny::checkboxInput(ns("include_meas_err"),
                "Include assay measurement error", value = TRUE),
              shiny::actionLink(ns("meas_err_help"), "?",
                style = "font-weight:bold;color:#337ab7;"),
              shiny::tags$div(style = "font-size:11px;color:#787878;",
                "On (recommended): the precision profile reflects assay noise, so ",
                "LLOQ/ULOQ describe real single-reading precision. Off: ",
                "calibration-curve uncertainty only \u2014 useful when a plate has ",
                "few standards/controls, where the measurement-error estimate is ",
                "unreliable."),
              shiny::uiOutput(ns("meas_err_hint"))))),
        shiny::radioButtons(ns("fit_scope"), "Re-fit scope",
                            choices = c("Whole experiment"        = "experiment",
                                        "Single feature/antigen"  = "antigen"),
                            selected = "experiment"),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'antigen'", ns("fit_scope")),
          shiny::selectizeInput(ns("fit_target"), "Feature / antigen to fit",
                                choices = NULL)),
        shiny::actionButton(ns("submit"), "Submit fit job",
                            class = "btn-primary btn-sm"),
        shiny::actionButton(ns("refresh_status"), "Check now",
                            class = "btn-default btn-sm", style = "margin-left:6px;"),
        shiny::uiOutput(ns("job_status"))
      )
    ),
    shiny::column(
      width = 9,
      shiny::div(style = "margin-bottom:12px;",
        shiny::strong("Calculation status (this experiment)"),
        shiny::uiOutput(ns("calc_status_summary")),
        DT::dataTableOutput(ns("calc_status")))
    )
  )
}

stdCurveCalcServer <- function(id, pool, api = compute_api_client(), scope = NULL,
                               calib_dirty = shiny::reactiveVal(0),
                               selected_curve = shiny::reactiveVal(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

    is_valid_sel <- function(x) !is.null(x) && length(x) && !is.na(x[1]) &&
                                nzchar(x[1]) && !(x[1] %in% c("Click here"))
    cur <- shiny::reactive({
      if (!is.null(scope)) scope() else list(study = NULL, experiment = NULL, project_id = NA)
    })

    shiny::observe({
      s <- cur()
      vmsg(">>> calc module cur(): study=", s$study, " exp=", s$experiment, " proj=", s$project_id)
    })

    lookup <- shiny::reactive({
      s <- cur()
      vmsg(">>> lookup eval: proj=", s$project_id, " study=", s$study, " exp=", s$experiment)
      # empty, correctly-shaped frame for the not-ready / error states (no DB call).
      empty_lk <- function() {
        cols <- c("curve_id", CALIB_NK_COLS)          # same columns fetch_curve_lookup_scoped returns
        setNames(data.frame(matrix(nrow = 0, ncol = length(cols))), cols)
      }
      if (!is_valid_sel(s$study) || !is_valid_sel(s$experiment) ||
          is.null(s$project_id) || is.na(s$project_id))
        return(empty_lk())                            # nothing selected yet -> empty, no fetch
      tryCatch({
        out <- fetch_curve_lookup_scoped(pool, project = s$project_id,
                                         study = s$study, experiment = s$experiment)
        vmsg(">>> lookup: matched ", nrow(out))
        out
      }, error = function(e) { vmsg(">>> lookup ERROR: ", conditionMessage(e)); empty_lk() })
    })

    response_lbl <- shiny::reactive({
      s <- cur(); shiny::req(is_valid_sel(s$study), is_valid_sel(s$experiment))
      response_label(response_var_of(fetch_raw_header(pool, project = s$project_id, study = s$study, experiment = s$experiment)))
    })

    # Settings that drive the calc controls -- keyed on the FIT TARGET.
    fit_settings <- shiny::reactive({
      tgt <- input$fit_target
      if (is.null(tgt) || !nzchar(tgt)) return(data.frame())
      fa <- strsplit(tgt, "\u001f", fixed = TRUE)[[1]]
      cs <- cur()
      vmsg(">>> fit scope: proj=", cs$project_id, " study=", cs$study,
              " exp=", cs$experiment, " feature=", fa[1],
              " antigen=", if (length(fa) > 1) fa[2] else NA)
      fetch_antigen_feature_settings(db_pool,
                                     project_id = cs$project_id, study = cs$study, experiment = cs$experiment,
                                     antigen = if (length(fa) > 1) fa[2] else NA, feature = fa[1])
    })

    shiny::observeEvent(list(input$fit_scope, input$fit_target, cur()), {
      cs <- cur()
      tgt <- input$fit_target
      # Determine the scope of the current fit: specific feature/antigen if a target
      # is selected, else the whole experiment.
      feat <- ant <- "__none__"
      if (!is.null(tgt) && nzchar(tgt)) {
        fa   <- strsplit(tgt, "\u001f", fixed = TRUE)[[1]]
        feat <- if (length(fa) >= 1 && nzchar(fa[1])) fa[1] else "__none__"
        ant  <- if (length(fa) >= 2 && nzchar(fa[2])) fa[2] else "__none__"
      }
      vmsg(">>> model scope: proj=", cs$project_id, " study=", cs$study,
              " exp=", cs$experiment, " feature=", feat, " antigen=", ant,
              " | fit_target=", if (is.null(tgt)) "NULL" else tgt)

      s <- tryCatch(
        resolve_settings_scoped(db_pool, project = cs$project_id, study = cs$study,
                                experiment = cs$experiment, feature = feat, antigen = ant,
                                group = "calibration"),
        error = function(e) { vmsg(">>> resolve error: ", conditionMessage(e)); NULL })

      mfl <- if (!is.null(s) && nrow(s)) {
        v <- s$value_text[s$param_name == "model_form_list"]; if (length(v)) v[1] else NA_character_
      } else NA_character_
      vmsg(">>> resolved model_form_list=", mfl)

      configured <- if (length(mfl) == 1 && !is.na(mfl))
        trimws(strsplit(mfl, "[,;]")[[1]]) else character(0)
      configured <- intersect(configured[nzchar(configured)], CALIB_FAMILY$model_name)
      if (!length(configured)) configured <- CALIB_FAMILY$model_name
      shiny::updateSelectizeInput(session, "model_form",
                                  choices  = setNames(configured, family_short(configured)),
                                  selected = configured)
    }, ignoreNULL = FALSE)

    # shiny::observeEvent(lookup(), {
    #   lk <- lookup()
    #   vmsg(">>> fit_target observer FIRED; lookup nrow=", if (is.null(lk)) "NULL" else nrow(lk))
    #   if (is.null(lk) || !nrow(lk)) {
    #     shiny::updateSelectizeInput(session, "fit_target", choices = character(0)); return()
    #   }
    #   pairs <- unique(lk[, c("feature", "antigen")])
    #   pairs <- pairs[order(pairs$feature, pairs$antigen), , drop = FALSE]
    #   one_feature <- length(unique(pairs$feature)) <= 1
    #   vals <- paste(pairs$feature, pairs$antigen, sep = "\u001f")
    #   labs <- if (one_feature) pairs$antigen else sprintf("%s / %s", pairs$feature, pairs$antigen)
    #   shiny::updateSelectizeInput(session, "fit_target",
    #     choices = stats::setNames(vals, labs), selected = character(0))
    # })

    shiny::observeEvent(list(lookup(), input$fit_scope), {
      lk <- lookup()
      if (is.null(lk) || !nrow(lk)) {
        shiny::updateSelectizeInput(session, "fit_target", choices = character(0)); return()
      }
      pairs <- unique(lk[, c("feature", "antigen")])
      pairs <- pairs[order(pairs$feature, pairs$antigen), , drop = FALSE]
      one_feature <- length(unique(pairs$feature)) <= 1
      vals <- paste(pairs$feature, pairs$antigen, sep = "\u001f")
      labs <- if (one_feature) pairs$antigen else sprintf("%s / %s", pairs$feature, pairs$antigen)
      vmsg(">>> updating fit_target with ", length(vals), " choices")
      shiny::updateSelectizeInput(session, "fit_target",
                                  choices = stats::setNames(vals, labs), selected = character(0))
    }, ignoreNULL = FALSE)

    # -- experiment-level calculation status ------------------------------
    calc_status <- shiny::reactive({
      calib_dirty()  # refresh after a submitted job completes (or a mask, later)
      s <- cur(); shiny::req(s$study, s$experiment)
      fetch_calc_status_scoped(pool, project = s$project_id, study = s$study, experiment = s$experiment)
    })

    output$calc_status_summary <- shiny::renderUI({
      cs <- calc_status(); resp <- response_lbl()
      n_curves <- length(unique(cs$curve_id))
      done <- cs[!is.na(cs$method), , drop = FALSE]
      by_m <- table(done$method)
      shiny::tags$small(sprintf(
        "assay response: %s  \u00b7  %d curve set(s) registered  \u00b7  computed: %s",
        resp, n_curves,
        if (length(by_m)) paste(sprintf("%s %d", names(by_m), as.integer(by_m)), collapse = ", ")
        else "none yet"))
    })

    output$calc_status <- DT::renderDataTable({
      cs <- calc_status(); shiny::req(nrow(cs) > 0)
      cs$best_model <- ifelse(is.na(cs$best_model), "\u2014", family_label(cs$best_model))
      cs$method     <- ifelse(is.na(cs$method), "not computed", cs$method)
      cols <- intersect(c("antigen", "plateid", "feature", "source", "wavelength",
                          "method", "best_model", "converged", "eligible",
                          "score_type", "selection_score", "job_status", "finished_at"),
                        names(cs))
      DT::datatable(cs[, cols, drop = FALSE], rownames = FALSE, filter = "top",
                    selection = "none",
                    options = list(scrollX = TRUE, pageLength = 15))
    }, server = TRUE)

    # -- submit + poll a worker job ---------------------------------------
    job_id     <- shiny::reactiveVal(NULL)
    job_state  <- shiny::reactiveVal(NULL)
    job_started_at <- shiny::reactiveVal(NULL)  # wall-clock; drives escalation
    job_detail     <- shiny::reactiveVal(NULL)  # last full JobStatus from the API
    job_checked_at <- shiny::reactiveVal(NULL)  # when we last polled (for display)
    queue_pos      <- shiny::reactiveVal(NULL)  # list(pos,total) while queued
    job_batch      <- shiny::reactiveVal(NULL)  # data.frame(curve_id, ...) we submitted (diag)

    # Escalating poll cadence keyed off elapsed time since the job started:
    #   < 2 min -> 10s,  2-10 min -> 30s,  > 10 min -> 60s.
    poll_interval_ms <- function(elapsed_sec) {
      if (!is.finite(elapsed_sec)) elapsed_sec <- 0
      if (elapsed_sec < 120)      10000L
      else if (elapsed_sec < 600) 30000L
      else                        60000L
    }

    shiny::observeEvent(input$submit, {
      cs <- cur()
      if (!is_valid_sel(cs$study) || !is_valid_sel(cs$experiment)) {
        job_state("select a study and experiment first"); return()
      }
      scope_sel <- input$fit_scope; if (is.null(scope_sel) || !nzchar(scope_sel)) scope_sel <- "experiment"

      feature <- NULL; antigen <- NULL; models <- NULL
      if (length(input$model_form)) models <- model_list_to_param(input$model_form)

      if (scope_sel == "antigen") {
        tgt <- input$fit_target
        if (is.null(tgt) || !nzchar(tgt)) {
          job_state("choose a feature/antigen to fit"); return()
        }
        fa <- strsplit(tgt, "\u001f", fixed = TRUE)[[1]]
        feature <- fa[1]; antigen <- if (length(fa) > 1) fa[2] else NA

      }

      batch <- tryCatch(
        fetch_curve_batch(pool, cs$study, cs$experiment, cs$project_id, feature, antigen),
        error = function(e) { job_state(paste("scope lookup failed:", conditionMessage(e))); NULL })
      if (is.null(batch)) return()
      if (!nrow(batch)) { job_state("no curves match this scope (nothing to fit)"); return() }

      params <- if (!is.null(models)) list(models = models) else list()
      # Bayesian-only: raise post-warmup draws so the precision profile is a
      # stable ratio estimate (Monte-Carlo noise falls ~1/sqrt(draws)). Worker
      # default sampling is 1000; we send 1500 (=6000 draws over 4 chains).
      # Merge, don't overwrite, any params already set. Frequentist ignores these.
      if (identical(input$fit_engine, "bayesian")) {
        samp <- suppressWarnings(as.integer(input$bayes_precision))
        if (is.na(samp)) samp <- DEFAULT_BAYES_SAMPLING
        params$sampling <- as.character(samp)
        params$chains   <- as.character(DEFAULT_BAYES_CHAINS)
        params$warmup   <- as.character(DEFAULT_BAYES_WARMUP)
        # Measurement-error switch (string form the worker parses). Default TRUE.
        params$include_measurement_error <-
          if (isFALSE(input$include_meas_err)) "false" else "true"
      }
      thr <- tryCatch({
        s <- fit_settings()
        if (!is.null(s) && nrow(s) && !is.na(s$pcov_threshold)) s$pcov_threshold else NULL
      }, error = function(e) NULL)

      vmsg(">>> SUBMIT DIAGNOSTIC ==================================")
      vmsg(">>> input$model_form = ", paste(input$model_form, collapse = " | "))
      vmsg(">>> models (after model_list_to_param) = ", paste(unlist(models), collapse = " | "))
      vmsg(">>> scope_sel = ", scope_sel)
      vmsg(">>> params$models = ", paste(unlist(params$models), collapse = " | "))
      vmsg(">>> params keys = ", paste(names(params), collapse = ", "))
      vmsg(">>> ====================================================")
      res <- tryCatch(
        api$submit_job(
          curve_ids            = batch$curve_id,
          multiplate_group_ids = batch$multiplate_group_id,
          script_type          = input$fit_engine,
          params               = params,
          cdan_cv_threshold    = thr),
        error = function(e) { job_state(paste("submit failed:", conditionMessage(e))); NULL })
      if (!is.null(res)) {
        jid <- if (!is.null(res$job_id)) res$job_id else res$id
        job_id(jid)
        job_batch(batch)                    # remember curve_ids for the diagnostic
        job_started_at(Sys.time())
        ng <- length(unique(batch$multiplate_group_id))
        job_state(sprintf("queued: %d curve%s in %d group%s",
                          nrow(batch), if (nrow(batch) == 1) "" else "s",
                          ng, if (ng == 1) "" else "s"))
      }
    })

    # One poll cycle, fully self-contained and error-guarded. An unhandled
    # error inside a Shiny observe stops it PERMANENTLY (which silently
    # freezes the status box), so every fragile call here is wrapped. Shared
    # by the tiered timer below and the manual "Check now" button.
    poll_once <- function() {
      jid <- job_id(); if (is.null(jid)) return(invisible())
      job_checked_at(Sys.time())          # stamp EVERY attempt, even on error
      st <- tryCatch(api$get_job(jid),
                     error = function(e) {
                       job_state(paste("check failed:", conditionMessage(e))); NULL })
      if (!is.null(st)) {
        job_detail(st)
        if (!is.null(st$status)) job_state(st$status)
        # Queue position: the worker consumes ispi:batch:queue FIFO (rpush at
        # submit), so our wait = the QUEUED jobs created before us. Counting
        # status=='queued' (not the raw list index) naturally drops cancelled
        # jobs (worker skips them) and excludes the running one -> effective
        # position. Only meaningful while WE are queued.
        if (identical(st$status, "queued")) {
          queue_pos(tryCatch({
            js   <- api$list_jobs(status = "queued")$jobs
            mine <- .parse_iso(st$created_at)
            if (length(js) && !is.null(mine)) {
              ahead <- 0L
              for (j in js) { cj <- .parse_iso(j$created_at %||% "")
                if (!is.null(cj) && cj < mine) ahead <- ahead + 1L }
              list(pos = ahead + 1L, total = length(js))
            } else NULL
          }, error = function(e) NULL))
        } else queue_pos(NULL)
        if (identical(st$status, "completed")) calib_dirty(calib_dirty() + 1)
      }
      invisible()
    }

    # TEMPORARY: gather the submit/queue diagnostics for the status box. Depends
    # on the poll stamp + detail so it refreshes every tick and on "Check now".
    # Fully error-guarded (returns NA fields) so it can never freeze the box.
    job_diag <- shiny::reactive({
      if (!isTRUE(JOB_DIAG_VERBOSE)) return(NULL)
      job_checked_at(); job_detail(); job_batch()      # refresh triggers
      d <- job_detail()
      cids <- job_batch()$curve_id %||% suppressWarnings(as.integer(unlist(d$curve_ids)))
      cids <- cids[is.finite(cids)]
      out <- list(curve_ids = cids)
      if (length(cids)) {
        idlist <- paste(cids, collapse = ",")
        cnt <- function(v) tryCatch(as.integer(DBI::dbGetQuery(pool, sprintf(
          "SELECT count(*) AS n FROM %s.%s WHERE curve_id IN (%s)",
          JOB_DIAG_FIT_SCHEMA, v, idlist))$n), error = function(e) NA_integer_)
        out$std_rows <- cnt("standard_for_fit")
        out$blk_rows <- cnt("blank_for_fit")
        out$smp_rows <- cnt("sample_for_fit")
      }
      out$n_queued  <- tryCatch(length(api$list_jobs(status = "queued")$jobs),
                                error = function(e) NA_integer_)
      out$n_running <- tryCatch(length(api$list_jobs(status = "running")$jobs),
                                error = function(e) NA_integer_)
      out
    })

    # Tiered poll -- interval grows with elapsed time (see poll_interval_ms).
    # The timer is re-armed BEFORE poll_once() and the terminal/elapsed checks
    # are guarded, so a bad status string or clock error can't kill the loop.
    shiny::observe({
      jid <- job_id(); shiny::req(jid)
      terminal <- tryCatch(is_terminal_status(job_state()), error = function(e) FALSE)
      if (isTRUE(terminal)) return()                 # stop once terminal
      started <- job_started_at() %||% Sys.time()
      elapsed <- tryCatch(as.numeric(difftime(Sys.time(), started, units = "secs")),
                          error = function(e) 0)
      shiny::invalidateLater(poll_interval_ms(elapsed), session)  # re-arm FIRST
      poll_once()
    })

    # Manual refresh -- uses the SAME get_job path (proven to work on demand),
    # so a returning or stale session can always force an update immediately
    # without waiting for the next tick.
    shiny::observeEvent(input$refresh_status, poll_once())

    # Resume-after-login: the compute API is SCOPE-BLIND -- jobs are opaque
    # curve_id batches and list_jobs cannot filter by study/experiment (see
    # app.py; the R client's `study=` arg is dropped by the service). But every
    # JobStatus carries its curve_ids, and curve_ids are globally unique, so a
    # job's batch intersects exactly ONE scope. We reconstruct the job<->scope
    # mapping client-side (as the API prescribes) by matching this scope's
    # curve_ids against the jobs Redis still holds -- recovering BOTH in-flight
    # and recently-finished jobs, with no new table and no API change. The
    # tiered poll above then takes over any non-terminal job automatically.
    .parse_iso <- function(x) {                      # ISO8601 (UTC) -> POSIXct
      if (is.null(x) || !length(x) || !nzchar(x)) return(NULL)
      x2 <- sub("([+-][0-9]{2}):?([0-9]{2})$", "", sub("Z$", "", x))  # drop tz
      t <- suppressWarnings(as.POSIXct(x2, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
      if (is.na(t)) NULL else t
    }
    resumed_for <- shiny::reactiveVal(NULL)
    shiny::observe({
      s <- cur()
      if (!is_valid_sel(s$study) || !is_valid_sel(s$experiment) ||
          is.null(s$project_id) || is.na(s$project_id)) return()
      key <- paste(s$project_id, s$study, s$experiment, sep = "\u001f")
      if (identical(resumed_for(), key)) return()   # act once per scope
      resumed_for(key)
      if (!is.null(job_id())) return()              # don't clobber a session job

      lk <- lookup()
      scope_ids <- if (!is.null(lk) && nrow(lk)) suppressWarnings(as.integer(lk$curve_id)) else integer(0)
      scope_ids <- scope_ids[is.finite(scope_ids)]
      if (!length(scope_ids)) return()

      # First job (list_jobs is newest-first) whose batch touches this scope.
      pick <- function(resp) {
        js <- if (!is.null(resp)) resp$jobs else NULL
        if (is.null(js) || !length(js)) return(NULL)
        for (j in js) {
          cids <- suppressWarnings(as.integer(unlist(j$curve_ids)))
          if (length(cids) && any(cids %in% scope_ids)) return(j)
        }
        NULL
      }
      # Prefer an in-flight job (running, then queued); else the most recent.
      job <- tryCatch(
        pick(api$list_jobs(status = "running")) %||%
        pick(api$list_jobs(status = "queued"))  %||%
        pick(api$list_jobs()),
        error = function(e) NULL)
      if (is.null(job)) return()

      job_id(job$job_id)
      job_state(job$status %||% "unknown")
      job_detail(job)
      job_checked_at(Sys.time())
      job_started_at(.parse_iso(job$started_at %||% job$created_at) %||% Sys.time())
    })

    output$job_status <- shiny::renderUI({
      s <- job_state(); if (is.null(s)) return(NULL)
      jid <- job_id(); if (is.null(jid)) jid <- "?"
      d <- job_detail()
      running <- !is.null(s) && !is_terminal_status(s)
      style <- if (running) "background:#fff8e1;"
               else if (identical(s, "failed") || identical(s, "cancelled")) "background:#fde8e8;"
               else "background:#eef7ee;"
      short <- if (nchar(jid) > 10) paste0(substr(jid, 1, 8), "\u2026") else jid
      chr <- function(x) if (is.null(x) || !length(x) || is.na(x[1])) "" else as.character(x[1])
      rows <- list(shiny::strong(sprintf("Compute job %s: %s", short, s)))
      if (!is.null(d)) {
        prog <- chr(d$progress)
        pct  <- suppressWarnings(as.numeric(d$percentage %||% NA))
        if (nzchar(prog) || is.finite(pct))
          rows <- c(rows, list(shiny::div(sprintf("progress: %s%s", prog,
                   if (is.finite(pct)) sprintf("  (%.0f%%)", pct) else ""))))
        if (running && nzchar(chr(d$eta_display)))
          rows <- c(rows, list(shiny::div(sprintf("ETA: %s", chr(d$eta_display)))))
        if (running && nzchar(chr(d$current_group)))
          rows <- c(rows, list(shiny::div(sprintf("fitting group: %s", chr(d$current_group)))))
        if (nzchar(chr(d$error)))
          rows <- c(rows, list(shiny::div(style = "color:#b71c1c;",
                   sprintf("error: %s", chr(d$error)))))
      }
      qp <- queue_pos()
      if (identical(s, "queued") && !is.null(qp))
        rows <- c(rows, list(shiny::div(style = "color:#1b5e20;font-weight:bold;",
                 sprintf("queue position: %d of %d", qp$pos, qp$total))))
      ck <- job_checked_at()
      if (!is.null(ck))
        rows <- c(rows, list(shiny::div(style = "color:#787878;font-size:11px;margin-top:3px;",
                 sprintf("checked %s", format(ck, "%H:%M:%S")))))

      # TEMPORARY verbose diagnostic (see JOB_DIAG_VERBOSE up top).
      if (isTRUE(JOB_DIAG_VERBOSE)) {
        dg <- job_diag()
        if (!is.null(dg)) {
          n <- function(x) if (is.null(x) || !length(x) || is.na(x[1])) "?" else as.character(x[1])
          .fmt_dur <- function(sec) {
            if (!is.finite(sec)) return("?")
            sec <- round(sec)
            if (sec < 120) paste0(sec, "s")
            else if (sec < 7200) paste0(round(sec / 60), "m")
            else paste0(round(sec / 3600, 1), "h")
          }
          ncid <- length(dg$curve_ids)
          cid_show <- if (ncid) paste(utils::head(dg$curve_ids, 20), collapse = ",") else "(none)"
          if (ncid > 20) cid_show <- paste0(cid_show, ", \u2026 (+", ncid - 20, ")")
          dl <- c(
            sprintf("job_id ............. %s", jid),
            sprintf("status ............. %s", s),
            sprintf("curve_ids (%d) ..... %s", ncid, cid_show),
            sprintf("standard_for_fit ... %s row(s)   <- worker exits before 'running' if 0",
                    n(dg$std_rows)),
            sprintf("blank_for_fit ...... %s row(s)", n(dg$blk_rows)),
            sprintf("sample_for_fit ..... %s row(s)", n(dg$smp_rows)),
            sprintf("service queue ...... %s queued / %s running",
                    n(dg$n_queued), n(dg$n_running)))

          # Liveness (REPORT-ONLY): two clocks. heartbeat = the supervisor/worker
          # loop is spinning; progress = a group actually completed. A legit
          # Bayesian 4-chain group can run ~2h between advances, so we alarm on a
          # stale HEARTBEAT (fast), and only flag stale PROGRESS past a
          # method-aware threshold well above that (never auto-acts).
          if (!is.null(d) && identical(s, "running")) {
            now <- as.numeric(Sys.time())
            age <- function(x) { t <- .parse_iso(chr(x)); if (is.null(t)) NA_real_ else now - as.numeric(t) }
            hb <- age(d$heartbeat_at); pg <- age(d$progress_at)
            is_bayes <- identical(tolower(chr(d$script_type)), "bayesian")
            hb_limit <- 30                       # 6x the 5s supervisor poll
            pg_limit <- if (is_bayes) 9000 else 600   # bayes >2h legit; freq minutes
            verdict <- if (is.finite(hb) && hb > hb_limit)
                         sprintf("STALE (no heartbeat %s) \u2014 loop wedged/dead; RESTART candidate", .fmt_dur(hb))
                       else if (is.finite(pg) && pg > pg_limit)
                         sprintf("STALE (no progress %s) \u2014 a group may be wedged; investigate", .fmt_dur(pg))
                       else if (!is.finite(hb))
                         "unknown (no heartbeat field yet \u2014 supervisor not updated?)"
                       else "HEALTHY"
            dl <- c(dl, sprintf("liveness ........... heartbeat %s ago \u00b7 last progress %s ago \u2192 %s",
                                .fmt_dur(hb), .fmt_dur(pg), verdict))
          }

          if (!is.null(d)) {
            flds <- c("progress", "percentage", "current_group", "eta_display",
                      "created_at", "started_at", "output_path", "error")
            dl <- c(dl, "JobStatus fields:",
                    vapply(flds, function(f) sprintf("  %-14s %s", f, chr(d[[f]])),
                           character(1)))
          }
          rows <- c(rows, list(
            shiny::tags$hr(style = "margin:6px 0;"),
            shiny::tags$div(style = "font-size:10px;color:#a33;font-weight:bold;",
                            "VERBOSE job diagnostic (temporary)"),
            shiny::tags$pre(
              style = "font-size:10px;background:#f6f6f6;padding:6px;border:1px solid #ddd;white-space:pre-wrap;margin-top:3px;",
              paste(dl, collapse = "\n"))))
        }
      }
      do.call(shiny::div, c(list(class = "well", style = style), rows))
    })

    shiny::observeEvent(input$bayes_help, {
      shiny::showModal(shiny::modalDialog(
        title = "Bayesian sampling & the precision profile",
        shiny::p(paste(
          "The Bayesian precision profile is estimated from the model's posterior draws,",
          "and its smoothness is governed by how many draws we keep: the total is",
          "chains \u00d7 sampling, where sampling is the number of post-warmup draws per",
          "chain. Because the %CV plotted is a ratio of posterior quantities, too few",
          "draws make it wobble from point to point \u2014 much of the jaggedness is",
          "Monte-Carlo noise, not real assay behavior. Raising sampling reduces the",
          "noise roughly with the square root of the draw count, so quadrupling draws",
          "roughly halves the wobble. Increase draws through sampling rather than chains,",
          "since chains beyond the worker's core count run sequentially and cost wall",
          "time without adding parallelism. The one thing more draws will NOT fix is the",
          "blow-up at the very low and very high ends of the curve: there the response",
          "is nearly flat, so back-calculated concentration is genuinely ill-conditioned",
          "and the high, unstable %CV at the extremes is real \u2014 it reflects the",
          "assay's detection limits, not a sampling artifact.")),
        easyClose = TRUE, footer = shiny::modalButton("Close")))
    })

    shiny::observeEvent(input$meas_err_help, {
      shiny::showModal(shiny::modalDialog(
        title = "Assay measurement error in the precision profile",
        shiny::p(paste(
          "Back-calculation precision has two sources that add (delta method):",
          "uncertainty in the fitted calibration curve itself (the posterior over",
          "the model parameters), and the assay's measurement noise -- the",
          "variability of a single response reading at a given concentration, which",
          "in immunoassay grows with signal level. Dividing by the curve's local",
          "slope turns response variability into concentration variability, which is",
          "why the profile is U-shaped and blows up toward the flat asymptotes.")),
        shiny::p(paste(
          "ON (default): both terms are included -- the classical assay precision",
          "profile. LLOQ/ULOQ are meaningful only against a profile that reflects",
          "how precisely a real, noisy reading pins down a concentration, so this is",
          "the recommended setting.")),
        shiny::p(paste(
          "OFF: curve/parameter uncertainty only -- an honest lower bound. The",
          "measurement-error term (and especially its concentration dependence) is",
          "hard to estimate and needs several standards and replicated controls",
          "spanning the response range. On a sparse plate that term is a crude",
          "estimate that can dominate a profile resting on a weak noise model; the",
          "curve-only profile reports the precision the data can actually support.",
          "The gap between the two settings is itself diagnostic: a large gap means",
          "the reported precision is being driven by a weakly-identified noise model",
          "-- a signal to add standards/controls rather than to trust either number",
          "blindly. The switch chooses the honest presentation for the data you",
          "have; it does not manufacture precision.")),
        easyClose = TRUE, size = "l", footer = shiny::modalButton("Close")))
    })

    # Non-blocking hint when the experiment's standards are thin (few levels or
    # little replication), where the measurement-error estimate is least
    # trustworthy (understanding doc, sec 4). Uses actual standard counts.
    output$meas_err_hint <- shiny::renderUI({
      if (!identical(input$fit_engine, "bayesian")) return(NULL)
      v <- tryCatch({
        cs <- cur()
        if (!is_valid_sel(cs$study) || !is_valid_sel(cs$experiment)) return(NULL)
        sup <- standards_support(pool, project = cs$project_id, study = cs$study, experiment = cs$experiment)
        standards_support_verdict(sup)
      }, error = function(e) NULL)
      if (is.null(v) || !isTRUE(v$thin)) return(NULL)
      shiny::tags$div(style = "font-size:11px;color:#8a6d3b;background:#fcf8e3;padding:4px 6px;border:1px solid #faebcc;margin-top:4px;",
        sprintf(paste("%d of %d curve(s) here have thin standards (as few as %s levels,",
                      "~%s reps/level). The measurement-error estimate may be crude \u2014",
                      "consider comparing both settings, or adding standards/controls."),
                v$n_thin, v$n_curves,
                if (is.finite(v$worst_levels)) v$worst_levels else "?",
                if (is.finite(v$worst_reps)) formatC(v$worst_reps, format = "f", digits = 1) else "?"))
    })

    invisible(NULL)
  })
}
