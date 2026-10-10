# =============================================================================
# std_curve_weights_module.R  --  the "Precision Weights" tab.
#
# A sibling to "Standard Curve" under Quality Control - Basic (qc_component),
# NOT nested under it -- curveRweights precision-weighting is a fully
# separate job family from calibration, submitted via its OWN script_type
# (weights_bayesian/weights_frequentist) and reading/writing its OWN tables
# (calib_weights/calib_weights_fit), per the i-spi-compute worker's design: a
# weights-fit failure can never affect a calibration job's status or data,
# because it shares no code path, process, or table with it. See
# i-spi-compute/worker/worker_weights.R for the backend half of this contract.
#
# ONE combined module (this file), TWO visible sub-tabs ("Compute weights" /
# "Summary"), same idiom std_curve_subtabs itself uses for its three tabs --
# just with the UI split two ways under a single moduleServer instead of
# three separate module files. app.R builds the outer tabsetPanel and passes
# the SAME id ("sc_weights") to both UI-builder functions below.
#
# Design-completeness gate (the reason this is more than "add a submit
# button"): curveRweights::fit_precision_weights() needs calib_samples to
# already exist for the chosen scope+method, AND at least one design column
# (timeperiod/agroup) to have real, non-degenerate variation. Confirmed this
# session: study MADI_P3_GAPS has good timeperiod data but agroup is NULL for
# every row -- not a hypothetical edge case. When that happens, this module
# offers an Excel download/fill-in/upload round-trip to patch agroup (ONLY
# agroup -- see update_calib_samples_agroup()'s own comment in
# calib_data_access.R for why timeperiod is deliberately not editable here),
# mirroring assay_import_module.R's existing template/upload/validate/commit
# flow rather than inventing a new pattern. Submit stays hidden until an
# explicit human approval checkbox is checked.
#
# Most scope/queue-polling code below is a DELIBERATE, namespaced duplicate of
# the equivalent private helpers inside std_curve_calc_module.R's
# moduleServer (chr0/short_id/approach_label/fetch_queue_view/
# render_queue_block/poll cadence) -- those are closures private to that
# module's own moduleServer call, not globally reusable functions, and this
# codebase's own established convention (see e.g. i-spi-compute's
# open_conn()/plan_parallelism() duplicated across sibling worker scripts) is
# light duplication across sibling modules over a shared-module refactor of
# already-working code.
# =============================================================================

DEFAULT_WEIGHTS_CHAINS      <- 4L
DEFAULT_WEIGHTS_WARMUP      <- 1000L
DEFAULT_WEIGHTS_ITER        <- 4000L
DEFAULT_WEIGHTS_ADAPT_DELTA <- 0.95

WEIGHTS_QUEUE_POLL_MS <- as.integer(getOption("ispi.weights_queue_poll_ms", 15000L))

# Summary tab: cap on how many antigens the precision-weight panel renders at
# once (selectizeInput's maxItems). Each antigen is its own ggplot subplot,
# so this bounds the figure grid to at most ceiling(9/3) = 3 rows of 3.
PANEL_MAX_ANTIGENS <- 9L

# Required calib_samples identity columns for the Excel round-trip (the
# upload must match an EXISTING row's full key on every one of these, plus
# carry agroup -- see validate_agroup_upload()).
WEIGHTS_IDENTITY_COLS <- c("curve_id", "method", "sampleid", "patientid",
                          "timeperiod", "dilution")

# ---------------------------------------------------------------------------
# "Compute weights" sub-tab
# ---------------------------------------------------------------------------
stdCurveWeightsComputeUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(
      width = 4,
      shiny::wellPanel(
        shiny::h4("Compute weights"),
        shiny::tags$p(shiny::tags$strong("curveRweights precision weighting"),
                      help_icon("qc.precision_weights.method", ns),
                      style = "margin-bottom:4px;color:#555;"),
        shiny::radioButtons(ns("weight_method"), "Method to weight",
                            choices = c("Bayesian" = "bayesian",
                                        "Frequentist" = "frequentist"),
                            selected = "bayesian", inline = TRUE),
        shiny::radioButtons(ns("fit_scope"), "Scope",
                            choices = c("Whole experiment"       = "experiment",
                                        "Single feature/antigen" = "antigen"),
                            selected = "experiment"),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'antigen'", ns("fit_scope")),
          shiny::selectizeInput(ns("fit_target"), "Feature / antigen to weight",
                                choices = NULL)),

        shiny::hr(),
        shiny::tags$strong("Design readiness"),
        shiny::tags$p(shiny::tags$small(style = "color:#787878;",
          "See the design table in the main panel →")),
        shiny::uiOutput(ns("design_excel_ui")),
        shiny::uiOutput(ns("design_approval_ui")),

        shiny::hr(),
        shiny::checkboxGroupInput(ns("design_cols"), "Design columns",
                                  choices = c("timeperiod", "agroup"),
                                  selected = c("timeperiod", "agroup")),
        shiny::radioButtons(ns("scale_predictor"), "Scale predictor",
                            choices = c("se (recommended)" = "se",
                                        "pcov (legacy)"     = "pcov"),
                            selected = "se"),
        shiny::tags$details(
          shiny::tags$summary("Advanced MCMC settings"),
          shiny::tags$div(style = "margin-top:6px;",
            shiny::numericInput(ns("chains"), "Chains", DEFAULT_WEIGHTS_CHAINS, min = 1, max = 16),
            shiny::numericInput(ns("warmup"), "Warmup iterations", DEFAULT_WEIGHTS_WARMUP, min = 50),
            shiny::numericInput(ns("iter"), "Total iterations", DEFAULT_WEIGHTS_ITER, min = 100),
            shiny::numericInput(ns("adapt_delta"), "Adapt delta", DEFAULT_WEIGHTS_ADAPT_DELTA,
                                min = 0.5, max = 0.999999, step = 0.01),
            shiny::numericInput(ns("seed"), "Seed (blank = server default)",
                                value = NA, min = 1),
            shiny::tags$small(style = "color:#787878;",
              "Apply to BOTH Bayesian and Frequentist weighting equally -- the ",
              "weights fit is always its own Stan run, regardless of which ",
              "calib_samples method is being read.")
          )
        ),

        shiny::hr(),
        shiny::uiOutput(ns("submit_ui")),
        shiny::actionButton(ns("refresh_status"), "Check now",
                            class = "btn-default btn-sm", style = "margin-left:6px;"),
        shiny::uiOutput(ns("job_status"))
      )
    ),
    shiny::column(
      width = 8,
      shiny::div(style = "margin-bottom:12px;",
        shiny::strong("Design table (timeperiod × agroup sample counts)"),
        shiny::tableOutput(ns("design_crosstab")),
        shiny::uiOutput(ns("design_warning"))),
      shiny::div(style = "margin-bottom:12px;",
        shiny::strong("Computed weights by design cell"),
        shiny::tags$p(shiny::tags$small(style = "color:#787878;",
          "Distribution of w_norm (normalized precision weight) per ",
          "timeperiod × agroup cell, one grid per method. Blank = no ",
          "weights computed yet for that cell/method.")),
        shiny::fluidRow(
          shiny::column(6, shiny::tags$strong("Bayesian"),
                        shiny::tableOutput(ns("weights_dist_bayesian"))),
          shiny::column(6, shiny::tags$strong("Frequentist"),
                        shiny::tableOutput(ns("weights_dist_frequentist"))))),
      shiny::div(style = "margin-bottom:12px;",
        shiny::strong("Weights calculation status (this experiment)"),
        shiny::uiOutput(ns("weights_status_summary")),
        DT::dataTableOutput(ns("weights_status")))
    )
  )
}

# ---------------------------------------------------------------------------
# "Summary" sub-tab (read-only results)
# ---------------------------------------------------------------------------
stdCurveWeightsSummaryUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(
      width = 12,
      shiny::h4("Precision weight panel"),
      shiny::tags$p(shiny::tags$small(style = "color:#787878;",
        "One subplot per antigen (ported from std-curver's ",
        shiny::tags$code("precision_weight_panel_m16.R"),
        "): predicted concentration vs. normalized precision weight, coloured ",
        "by plate, with a LOESS trend and a black-circle pcov_pass gate overlay.")),
      shiny::fluidRow(
        shiny::column(3,
          shiny::radioButtons(ns("panel_method"), "Method",
                              choices = c("Bayesian" = "bayesian",
                                          "Frequentist" = "frequentist"),
                              selected = "bayesian", inline = TRUE)),
        shiny::column(3, shiny::uiOutput(ns("panel_source_ui"))),
        shiny::column(6, shiny::uiOutput(ns("panel_antigens_ui")))
      ),
      # height = "auto": the container takes whatever height renderPlot's own
      # height function emits (1-3+ rows of panels), instead of plotOutput's
      # fixed 400px default -- a fixed container clipped/overlapped the
      # "Weights fit summary" table below it whenever the grid ran >1 row.
      # Same loading-indicator style as the Standard Curve tab's plots/tables
      # (type = 4 "expanding circles in a line", std_curve_view_module.R /
      # std_curve_compare_module.R) -- consistent feedback across all three
      # sections of this tab while the DB fetch / plot build is in flight.
      shinycssloaders::withSpinner(
        shiny::plotOutput(ns("weights_panel_plot"), height = "auto"), type = 4, color = "#337ab7"),
      shiny::hr(),
      shiny::h4("Weights fit summary"),
      shiny::uiOutput(ns("weights_fit_summary_msg")),
      shinycssloaders::withSpinner(
        shiny::tableOutput(ns("weights_fit_summary")), type = 4, color = "#337ab7"),
      shiny::hr(),
      shiny::h4("Per-sample weights"),
      shinycssloaders::withSpinner(
        DT::dataTableOutput(ns("weights_table")), type = 4, color = "#337ab7")
      # Deferred: a weights-vs-timeperiod plot. std_curve_view_module.R has a
      # plotly precision-plot pattern to reuse when this gets added.
    )
  )
}

# ---------------------------------------------------------------------------
# Server (backs BOTH UI halves above, same namespace id)
# ---------------------------------------------------------------------------
stdCurveWeightsServer <- function(id, pool, api = function() compute_api_client(), scope = NULL) {
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

    # --- queue-panel helpers (namespaced duplicate -- see file header) ------
    chr0 <- function(x) if (is.null(x) || !length(x) || is.na(x[1])) "" else as.character(x[1])
    short_id <- function(x) { x <- chr0(x); if (nchar(x) > 10) paste0(substr(x, 1, 8), "…") else x }
    approach_label <- function(x) {
      a <- tolower(chr0(x))
      if (identical(a, "bayesian")) "Bayesian"
      else if (identical(a, "frequentist")) "Frequentist"
      else if (identical(a, "weights_bayesian")) "Weights (Bayesian)"
      else if (identical(a, "weights_frequentist")) "Weights (Frequentist)"
      else if (nzchar(a)) a else "?"
    }
    n_curve_ids <- function(j) {
      n <- suppressWarnings(as.integer(j$n_curves %||% NA))
      if (is.na(n)) n <- length(unlist(j$curve_ids %||% list()))
      if (is.na(n)) 0L else n
    }
    n_curves_phrase <- function(j) {
      k <- n_curve_ids(j); sprintf("%d curve%s", k, if (k == 1L) "" else "s")
    }
    .parse_iso <- function(x) {                      # ISO8601 (UTC) -> POSIXct
      if (is.null(x) || !length(x) || !nzchar(x)) return(NULL)
      x2 <- sub("([+-][0-9]{2}):?([0-9]{2})$", "", sub("Z$", "", x))  # drop tz
      t <- suppressWarnings(as.POSIXct(x2, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
      if (is.na(t)) NULL else t
    }

    fetch_queue_view <- function() {
      running <- tryCatch(api()$list_jobs(status = "running")$jobs, error = function(e) NULL) %||% list()
      queued  <- tryCatch(api()$list_jobs(status = "queued")$jobs,  error = function(e) NULL) %||% list()
      if (length(queued) > 1) {
        key <- vapply(queued, function(j) {
          t <- .parse_iso(chr0(j$created_at)); if (is.null(t)) Inf else as.numeric(t)
        }, numeric(1))
        queued <- queued[order(key)]
      }
      list(running = running, queued = queued, at = Sys.time())
    }

    render_queue_block <- function(qv, mine = NULL) {
      if (is.null(qv)) return(list())
      running <- qv$running %||% list(); queued <- qv$queued %||% list()
      mine <- chr0(mine)
      tag_mine <- function(jid) {
        if (nzchar(mine) && identical(chr0(jid), mine))
          shiny::tags$span(style = "color:#1b5e20;font-weight:bold;", " (yours)")
        else NULL
      }
      out <- list(shiny::div(style = "font-weight:bold;margin-top:2px;", "This compute queue"))
      if (!length(running)) {
        out <- c(out, list(shiny::div(style = "color:#787878;", "No job running.")))
      } else {
        for (j in running) {
          prog <- chr0(j$progress); pct <- suppressWarnings(as.numeric(j$percentage %||% NA))
          eta <- chr0(j$eta_display)
          det <- paste0(
            if (nzchar(prog) || is.finite(pct))
              sprintf(" — %s%s", prog, if (is.finite(pct)) sprintf(" (%.0f%%)", pct) else "") else "",
            if (nzchar(eta)) sprintf(" · ETA %s", eta) else "")
          out <- c(out, list(shiny::div(
            shiny::tags$span(style = "color:#b26a00;font-weight:bold;", "▶ running "),
            sprintf("%s · %s · %s", short_id(j$job_id), approach_label(j$script_type), n_curves_phrase(j)),
            tag_mine(j$job_id), shiny::tags$span(style = "color:#555;", det))))
        }
      }
      out <- c(out, list(shiny::div(style = "margin-top:4px;font-weight:bold;",
                                    sprintf("Queued (%d)", length(queued)))))
      if (!length(queued)) {
        out <- c(out, list(shiny::div(style = "color:#787878;", "No jobs waiting.")))
      } else {
        items <- lapply(queued, function(j) shiny::tags$li(
          sprintf("%s · %s · %s", short_id(j$job_id), approach_label(j$script_type), n_curves_phrase(j)),
          tag_mine(j$job_id)))
        out <- c(out, list(shiny::tags$ol(style = "margin:2px 0 2px 20px;padding:0;", items)))
      }
      out
    }

    queue_view <- shiny::reactive({
      input$submit; input$refresh_status
      shiny::invalidateLater(WEIGHTS_QUEUE_POLL_MS, session)
      fetch_queue_view()
    })

    # --- scope / lookup (copied from std_curve_calc_module.R verbatim) ------
    is_valid_sel <- function(x) !is.null(x) && length(x) && !is.na(x[1]) &&
                                nzchar(x[1]) && !(x[1] %in% c("Click here"))
    cur <- shiny::reactive({
      if (!is.null(scope)) scope() else list(study = NULL, experiment = NULL, project_id = NA)
    })

    lookup <- shiny::reactive({
      s <- cur()
      empty_lk <- function() {
        cols <- c("curve_id", CALIB_NK_COLS)
        setNames(data.frame(matrix(nrow = 0, ncol = length(cols))), cols)
      }
      if (!is_valid_sel(s$study) || !is_valid_sel(s$experiment) ||
          is.null(s$project_id) || is.na(s$project_id))
        return(empty_lk())
      tryCatch(fetch_curve_lookup_scoped(pool, project = s$project_id,
                                         study = s$study, experiment = s$experiment),
               error = function(e) empty_lk())
    })

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
      shiny::updateSelectizeInput(session, "fit_target",
                                  choices = stats::setNames(vals, labs), selected = character(0))
    }, ignoreNULL = FALSE)

    # Current (feature, antigen) target, or NULL/NA for "whole experiment".
    cur_target <- shiny::reactive({
      if (!identical(input$fit_scope, "antigen")) return(list(feature = NULL, antigen = NULL))
      tgt <- input$fit_target
      if (is.null(tgt) || !nzchar(tgt)) return(list(feature = NULL, antigen = NULL))
      fa <- strsplit(tgt, "\u001f", fixed = TRUE)[[1]]
      list(feature = fa[1], antigen = if (length(fa) > 1) fa[2] else NA)
    })

    # --- weights computation status box -------------------------------------
    weights_status <- shiny::reactive({
      design_dirty(); job_checked_at()   # refresh after upload / job completion
      s <- cur(); shiny::req(s$study, s$experiment)
      fetch_weights_status_scoped(pool, project = s$project_id, study = s$study, experiment = s$experiment)
    })

    output$weights_status_summary <- shiny::renderUI({
      ws <- weights_status()
      n_curves <- length(unique(ws$curve_id))
      done <- ws[!is.na(ws$method), , drop = FALSE]
      by_m <- table(done$method)
      shiny::tags$small(sprintf(
        "%d curve set(s) registered · weights computed: %s",
        n_curves,
        if (length(by_m)) paste(sprintf("%s %d", names(by_m), as.integer(by_m)), collapse = ", ")
        else "none yet"))
    })

    output$weights_status <- DT::renderDataTable({
      ws <- weights_status(); shiny::req(nrow(ws) > 0)
      ws$method <- ifelse(is.na(ws$method), "not computed", ws$method)
      ws$phi    <- round(ws$phi, 3); ws$beta1 <- round(ws$beta1, 3)
      cols <- intersect(c("antigen", "plateid", "feature", "source", "wavelength",
                          "method", "phi", "beta1", "interpretation", "design_cols",
                          "computed_at"), names(ws))
      DT::datatable(ws[, cols, drop = FALSE], rownames = FALSE, filter = "top",
                    selection = "none", options = list(scrollX = TRUE, pageLength = 15))
    }, server = TRUE)

    # --- design readiness (timeperiod x agroup cross-tab) -------------------
    design_dirty <- shiny::reactiveVal(0)

    readiness <- shiny::reactive({
      design_dirty()
      s <- cur(); shiny::req(s$study, s$experiment, input$weight_method)
      tgt <- cur_target()
      fetch_design_readiness(pool, project = s$project_id, study = s$study,
                             experiment = s$experiment, method = input$weight_method,
                             feature = tgt$feature, antigen = tgt$antigen)
    })

    has_samples <- shiny::reactive(isTRUE(sum(readiness()$n) > 0))

    # A design column "has variation" when it carries >= 2 distinct non-NA/
    # non-blank values. agroup was found NULL for every row of a real study
    # this session -- that is exactly the degenerate case this must catch.
    varying_cols <- shiny::reactive({
      df <- readiness(); if (!nrow(df)) return(character(0))
      out <- character(0)
      for (col in c("timeperiod", "agroup")) {
        v <- df[[col]]; v <- v[!is.na(v) & nzchar(trimws(as.character(v)))]
        if (length(unique(v)) >= 2) out <- c(out, col)
      }
      out
    })

    output$design_crosstab <- shiny::renderTable({
      df <- readiness(); shiny::req(nrow(df) > 0)
      tab <- tryCatch(stats::xtabs(n ~ timeperiod + agroup, data = df, addNA = TRUE),
                      error = function(e) NULL)
      shiny::req(!is.null(tab))
      as.data.frame.matrix(tab)
    }, rownames = TRUE)

    output$design_warning <- shiny::renderUI({
      df <- readiness()
      if (!nrow(df)) return(shiny::tags$em("No calib_samples for this scope/method yet."))
      if (!length(varying_cols()))
        shiny::tags$div(style = "color:#b02a37;font-weight:bold;margin-top:4px;",
          "⚠ Neither timeperiod nor agroup varies for this scope — ",
          "curveRweights needs at least one to tell real study-design ",
          "differences apart from measurement noise. See the sidebar to ",
          "fill in agroup.",
          help_icon("qc.precision_weights.method", ns))
      else NULL
    })

    # --- computed-weights distribution, same row/column shape as the design
    # table above, one grid per method (pivoted in R from the already-
    # aggregated SQL result -- at most a few dozen rows, trivial to pivot) --
    weights_dist <- shiny::reactive({
      design_dirty(); job_checked_at()
      s <- cur(); shiny::req(s$study, s$experiment)
      tgt <- cur_target()
      fetch_weights_distribution(pool, project = s$project_id, study = s$study,
                                 experiment = s$experiment,
                                 feature = tgt$feature, antigen = tgt$antigen)
    })

    # Pivot one method's rows into a timeperiod x agroup grid of formatted
    # "n=.. x̄=.. (±sd)" cells; blank where no weights exist yet for
    # that cell. Row/column levels come from the FULL design table (not just
    # this method's rows) so both method grids line up with each other and
    # with the design table above, even where one method has gaps.
    weights_dist_grid <- function(method_name) {
      all_df <- readiness()  # for the row/column level universe
      d <- weights_dist()
      d <- d[!is.na(d$method) & d$method == method_name, , drop = FALSE]
      tps <- sort(unique(all_df$timeperiod)); ags <- sort(unique(all_df$agroup))
      if (!length(tps) || !length(ags)) return(NULL)
      grid <- matrix("", nrow = length(tps), ncol = length(ags),
                     dimnames = list(tps, ags))
      if (nrow(d)) {
        for (i in seq_len(nrow(d))) {
          tp <- as.character(d$timeperiod[i]); ag <- as.character(d$agroup[i])
          if (tp %in% tps && ag %in% ags) {
            sd_txt <- if (is.na(d$sd_w_norm[i])) "" else sprintf(" (±%.2f)", d$sd_w_norm[i])
            grid[tp, ag] <- sprintf("n=%d x̄=%.2f%s", d$n[i], d$mean_w_norm[i], sd_txt)
          }
        }
      }
      as.data.frame(grid, check.names = FALSE)
    }

    output$weights_dist_bayesian <- shiny::renderTable({
      g <- weights_dist_grid("bayesian"); shiny::req(!is.null(g)); g
    }, rownames = TRUE)
    output$weights_dist_frequentist <- shiny::renderTable({
      g <- weights_dist_grid("frequentist"); shiny::req(!is.null(g)); g
    }, rownames = TRUE)

    # --- Excel round-trip (only offered when samples exist but no design
    # column varies -- mirrors assay_import_module.R's template/upload flow) --
    rv <- shiny::reactiveValues(issues = NULL, upload = NULL)

    output$design_excel_ui <- shiny::renderUI({
      if (!has_samples()) return(NULL)
      # Triggered on agroup SPECIFICALLY -- it's the only editable column
      # (timeperiod is part of calib_samples' primary key; see
      # update_calib_samples_agroup()'s own comment for why). Checking
      # length(varying_cols()) == 0 here was wrong: it hid this panel the
      # moment timeperiod alone had variation, which is exactly the
      # MADI_P3_GAPS case (timeperiod fine, agroup NULL) this workflow was
      # built for -- agroup missing should offer the fix regardless of
      # whether timeperiod already makes the design technically submittable.
      if ("agroup" %in% varying_cols()) return(NULL)   # agroup already fine, nothing to patch
      ns <- session$ns
      shiny::tagList(
        shiny::tags$p(style = "margin-top:8px;",
          "Download the current samples, fill in ", shiny::tags$code("agroup"),
          " (cohort/treatment-arm), then re-upload."),
        shiny::downloadButton(ns("dl_samples"), "Download calib_samples (Excel)"),
        shiny::fileInput(ns("upload_samples"), "Upload filled file",
                         accept = c(".xlsx", ".xls")),
        shiny::uiOutput(ns("upload_issues_ui")),
        shiny::uiOutput(ns("commit_ui"))
      )
    })

    scope_samples <- shiny::reactive({
      s <- cur(); tgt <- cur_target()
      where <- "cs.method = $1"; params <- list(input$weight_method)
      # Reuse fetch_calib_weights_scoped-style access isn't right here (that's
      # calib_weights, not calib_samples) -- read calib_samples directly via
      # the module-level helper added to calib_data_access.R.
      fetch_calib_samples_for_scope(pool, project = s$project_id, study = s$study,
                                    experiment = s$experiment, method = input$weight_method,
                                    feature = tgt$feature, antigen = tgt$antigen)
    })

    output$dl_samples <- shiny::downloadHandler(
      filename = function() {
        s <- cur()
        sprintf("%s_%s_calib_samples_%s.xlsx", s$study %||% "study",
               s$experiment %||% "exp", input$weight_method %||% "method")
      },
      content = function(file) {
        df <- scope_samples()
        cols <- intersect(c(WEIGHTS_IDENTITY_COLS, "antigen", "feature", "plateid", "agroup"),
                          names(df))
        openxlsx::write.xlsx(df[, cols, drop = FALSE], file)
      }
    )

    shiny::observeEvent(input$upload_samples, {
      req(input$upload_samples)
      rv$upload <- NULL; rv$issues <- NULL
      up <- tryCatch(readxl::read_excel(input$upload_samples$datapath),
                     error = function(e) NULL)
      if (is.null(up)) {
        rv$issues <- data.frame(sheet = "upload", severity = "error", column = NA_character_,
                                message = "Could not read this file as Excel.",
                                stringsAsFactors = FALSE)
        return()
      }
      up <- as.data.frame(up)
      rv$issues <- validate_agroup_upload(up, scope_samples())
      rv$upload <- up
    })

    output$upload_issues_ui <- shiny::renderUI({
      shiny::req(rv$issues)
      iss <- rv$issues
      if (!nrow(iss)) return(shiny::tags$div(style = "color:#2e7d32;", "No issues — ready to upload."))
      DT::DTOutput(session$ns("upload_issues"))
    })
    output$upload_issues <- DT::renderDataTable({
      shiny::req(rv$issues); iss <- rv$issues; shiny::req(nrow(iss) > 0)
      DT::formatStyle(
        DT::datatable(iss, rownames = FALSE, options = list(dom = "tp", pageLength = 10)),
        "severity",
        backgroundColor = DT::styleEqual(c("error", "warning"), c("#f8d7da", "#fff3cd")))
    })

    design_upload_ok <- function(issues) is.null(issues) || !nrow(issues) || !any(issues$severity == "error")

    output$commit_ui <- shiny::renderUI({
      shiny::req(rv$upload)
      if (!design_upload_ok(rv$issues)) return(NULL)
      shiny::actionButton(session$ns("commit_agroup"), "Upload to database", class = "btn-primary")
    })

    shiny::observeEvent(input$commit_agroup, {
      req(rv$upload, design_upload_ok(rv$issues))
      res <- tryCatch({
        keep <- intersect(c(WEIGHTS_IDENTITY_COLS, "agroup"), names(rv$upload))
        update_calib_samples_agroup(pool, rv$upload[, keep, drop = FALSE])
        list(ok = TRUE)
      }, error = function(e) list(ok = FALSE, msg = conditionMessage(e)))
      if (isTRUE(res$ok)) {
        shiny::showNotification("agroup updated.", type = "message", duration = 8)
        rv$upload <- NULL; rv$issues <- NULL
        design_dirty(design_dirty() + 1)
      } else {
        shiny::showNotification(paste("Upload failed:", res$msg), type = "error", duration = NULL)
      }
    })

    # --- approval gate + submit ---------------------------------------------
    output$design_approval_ui <- shiny::renderUI({
      if (!has_samples() || !length(varying_cols())) return(NULL)
      shiny::checkboxInput(session$ns("design_approved"),
        "I've reviewed the timeperiod × agroup table above and it's ready", value = FALSE)
    })

    output$submit_ui <- shiny::renderUI({
      if (!isTRUE(input$design_approved)) return(NULL)
      shiny::actionButton(session$ns("submit"), "Submit weights job", class = "btn-primary btn-sm")
    })

    job_id         <- shiny::reactiveVal(NULL)
    job_state      <- shiny::reactiveVal(NULL)
    job_started_at <- shiny::reactiveVal(NULL)
    job_detail     <- shiny::reactiveVal(NULL)
    job_checked_at <- shiny::reactiveVal(NULL)
    job_api        <- shiny::reactiveVal(NULL)  # client THIS job was submitted through

    shiny::observeEvent(input$submit, {
      s <- cur()
      if (!is_valid_sel(s$study) || !is_valid_sel(s$experiment)) {
        job_state("select a study and experiment first"); return()
      }
      tgt <- cur_target()
      batch <- tryCatch(
        fetch_curve_batch(pool, s$study, s$experiment, s$project_id, tgt$feature, tgt$antigen),
        error = function(e) { job_state(paste("scope lookup failed:", conditionMessage(e))); NULL })
      if (is.null(batch) || !nrow(batch)) { job_state("no curves match this scope"); return() }

      params <- list(
        design          = paste(input$design_cols, collapse = ","),
        scale_predictor = input$scale_predictor,
        chains          = as.character(input$chains),
        warmup          = as.character(input$warmup),
        iter            = as.character(input$iter),
        adapt_delta     = as.character(input$adapt_delta))
      if (!is.null(input$seed) && !is.na(input$seed)) params$seed <- as.character(input$seed)

      res <- tryCatch(
        api()$submit_job(curve_ids = batch$curve_id, multiplate_group_ids = batch$multiplate_group_id,
                         script_type = paste0("weights_", input$weight_method), params = params),
        error = function(e) { job_state(paste("submit failed:", conditionMessage(e))); NULL })
      if (!is.null(res)) {
        job_id(res$job_id %||% res$id)
        job_api(api())
        job_started_at(Sys.time())
        ng <- length(unique(batch$multiplate_group_id))
        job_state(sprintf("queued: %d curve%s in %d group%s",
                          nrow(batch), if (nrow(batch) == 1) "" else "s",
                          ng, if (ng == 1) "" else "s"))
      }
    })

    poll_once <- function() {
      jid <- job_id(); if (is.null(jid)) return(invisible())
      job_checked_at(Sys.time())
      st <- tryCatch(job_api()$get_job(jid), error = function(e) {
        job_state(paste("check failed:", conditionMessage(e))); NULL })
      if (!is.null(st)) {
        job_detail(st)
        if (!is.null(st$status)) job_state(st$status)
        if (identical(st$status, "completed")) design_dirty(design_dirty() + 1)
      }
      invisible()
    }

    poll_interval_ms <- function(elapsed_sec) {
      if (!is.finite(elapsed_sec)) elapsed_sec <- 0
      if (elapsed_sec < 120) 10000L else if (elapsed_sec < 600) 30000L else 60000L
    }

    shiny::observe({
      jid <- job_id(); shiny::req(jid)
      st <- job_state()
      if (isTRUE(st %in% c("completed", "failed", "cancelled"))) return()
      elapsed <- as.numeric(difftime(Sys.time(), job_started_at() %||% Sys.time(), units = "secs"))
      shiny::invalidateLater(poll_interval_ms(elapsed), session)
      poll_once()
    })
    shiny::observeEvent(input$refresh_status, poll_once())

    output$job_status <- shiny::renderUI({
      qv <- queue_view()
      # last-checked stamp (your last poll, else the queue snapshot time) --
      # same convention as std_curve_calc_module.R's sidebar status box.
      ck <- job_checked_at() %||% (if (!is.null(qv)) qv$at else NULL)
      # Cluster THIS status box is actually watching: the job's own frozen
      # client while one is tracked (see job_api), else the live cluster a
      # new submission would go to.
      cluster_lbl <- (job_api() %||% api())$label %||% "default"
      shiny::tagList(
        shiny::div(style = "color:#555;font-size:11px;margin-bottom:3px;",
                   sprintf("Compute cluster: %s", cluster_lbl)),
        if (!is.null(job_state())) shiny::tags$div(shiny::strong("Your job: "), job_state()) else NULL,
        render_queue_block(qv, mine = job_id()),
        if (!is.null(ck)) shiny::div(style = "color:#787878;font-size:11px;margin-top:3px;",
                                     sprintf("checked %s", format(ck, "%H:%M:%S"))) else NULL
      )
    })

    # --- Summary tab results -------------------------------------------------
    weights_fit_rows <- shiny::reactive({
      design_dirty(); job_checked_at()
      s <- cur(); shiny::req(s$study, s$experiment)
      fetch_calib_weights_fit_scoped(pool, project = s$project_id, study = s$study, experiment = s$experiment)
    })

    output$weights_fit_summary_msg <- shiny::renderUI({
      df <- weights_fit_rows()
      if (!nrow(df)) shiny::tags$em("No weights computed yet for this experiment.") else NULL
    })
    output$weights_fit_summary <- shiny::renderTable({
      df <- weights_fit_rows(); shiny::req(nrow(df) > 0)
      df$phi <- round(df$phi, 3); df$beta1 <- round(df$beta1, 3)
      cols <- intersect(c("multiplate_group_id", "method", "antigen", "feature",
                          "design_cols", "scale_predictor", "phi", "beta1",
                          "interpretation", "n_fit", "n_eff", "weight_ratio"), names(df))
      df[, cols, drop = FALSE]
    }, rownames = FALSE)

    # Antigen multi-select + source single-select for the panel, both
    # populated from the scope's curve registry. Built as renderUI (fresh
    # selectInput/selectizeInput per scope change) rather than a static
    # control + update*Input() calls -- update*Input messages sent to a
    # selectize-backed widget (both selectInput's default selectize = TRUE
    # AND selectizeInput) INSIDE A TAB THAT HASN'T BEEN SHOWN YET are
    # silently dropped by selectize.js's lazy client-side init, which is
    # exactly what was happening here: the Summary sub-tab is hidden on
    # first load, so the choices computed when the user picked a study/
    # experiment (while still on "Compute weights") never reached the
    # widget once they switched to "Summary". renderUI sidesteps this
    # entirely -- Shiny (re)runs a suspended output once it becomes visible,
    # so the control is built fresh, with CURRENT choices, the moment it's
    # actually shown. Antigens cap at PANEL_MAX_ANTIGENS (selectize's
    # maxItems) so the figure grid can't grow unbounded -- past that, the
    # user swaps one selection for another rather than piling more on.
    output$panel_source_ui <- shiny::renderUI({
      lk <- lookup()
      srcs <- if (is.null(lk) || !nrow(lk)) character(0) else sort(unique(lk$source))
      shiny::selectInput(session$ns("panel_source"), "Calibration source",
                        choices = srcs, selected = if (length(srcs)) srcs[1] else character(0))
    })

    # Panel target choices: bare antigen names when every antigen in scope has
    # exactly one feature (today's behavior, unchanged); otherwise
    # feature/antigen composite keys -- same "\u001f"-joined convention as
    # `fit_target` above -- so one antigen carrying several analytes (e.g. a
    # combined flow experiment) can be compared side by side instead of
    # pooled into one subplot.
    output$panel_antigens_ui <- shiny::renderUI({
      lk <- lookup()
      if (is.null(lk) || !nrow(lk)) {
        return(shiny::selectizeInput(session$ns("panel_antigens"),
          sprintf("Antigens (up to %d)", PANEL_MAX_ANTIGENS),
          choices = character(0), selected = character(0), multiple = TRUE))
      }
      pairs <- unique(lk[, c("feature", "antigen")])
      pairs <- pairs[order(pairs$feature, pairs$antigen), , drop = FALSE]
      one_feature <- length(unique(pairs$feature)) <= 1
      choices <- if (one_feature) {
        ags <- sort(unique(pairs$antigen)); stats::setNames(ags, ags)
      } else {
        vals <- paste(pairs$feature, pairs$antigen, sep = "\u001f")
        labs <- sprintf("%s / %s", pairs$feature, pairs$antigen)
        stats::setNames(vals, labs)
      }
      default_sel <- if (length(choices) > PANEL_MAX_ANTIGENS)
        choices[seq_len(PANEL_MAX_ANTIGENS)] else choices
      shiny::tagList(
        shiny::selectizeInput(session$ns("panel_antigens"),
          sprintf("Antigens (up to %d)", PANEL_MAX_ANTIGENS),
          choices = choices, selected = default_sel, multiple = TRUE,
          options = list(maxItems = PANEL_MAX_ANTIGENS, plugins = list("remove_button"))),
        if (length(choices) > PANEL_MAX_ANTIGENS)
          shiny::tags$small(style = "color:#787878;",
            sprintf("%d targets available -- remove one to add another.", length(choices)))
        else NULL
      )
    })

    # Selected panel targets parsed back into (antigen, feature); feature is
    # NA for the plain (non-composite) form.
    panel_targets <- shiny::reactive({
      sel <- input$panel_antigens; shiny::req(length(sel) > 0)
      is_composite <- grepl("\u001f", sel, fixed = TRUE)
      parts <- strsplit(sel, "\u001f", fixed = TRUE)
      feat <- ifelse(is_composite, vapply(parts, `[`, character(1), 1), NA_character_)
      ag   <- ifelse(is_composite, vapply(parts, `[`, character(1), 2), sel)
      data.frame(antigen = ag, feature = feat, stringsAsFactors = FALSE)
    })

    panel_weights_data <- shiny::reactive({
      design_dirty(); job_checked_at()
      s <- cur(); shiny::req(s$study, s$experiment, input$panel_method, input$panel_source)
      tg <- panel_targets()
      fetch_weights_panel_data(pool, project = s$project_id, study = s$study,
                               experiment = s$experiment, method = input$panel_method,
                               antigens = unique(tg$antigen), source = input$panel_source)
    })

    panel_fit_data <- shiny::reactive({
      design_dirty(); job_checked_at()
      s <- cur(); shiny::req(s$study, s$experiment, input$panel_method, input$panel_source)
      tg <- panel_targets()
      fetch_weights_panel_fit(pool, project = s$project_id, study = s$study,
                              experiment = s$experiment, method = input$panel_method,
                              antigens = unique(tg$antigen), source = input$panel_source)
    })

    output$weights_panel_plot <- shiny::renderPlot({
      tg <- panel_targets()
      wd <- panel_weights_data(); shiny::req(nrow(wd) > 0)
      fd <- panel_fit_data()
      precision_weight_panel(wd, fd, antigens = tg$antigen, features = tg$feature,
                             ncol = min(3L, nrow(tg)))
    }, height = function() {
      tg <- panel_targets(); n <- max(1L, nrow(tg))
      ncol <- min(3L, n)
      ceiling(n / ncol) * 340
    })

    weights_rows <- shiny::reactive({
      design_dirty(); job_checked_at()
      s <- cur(); shiny::req(s$study, s$experiment)
      fetch_calib_weights_scoped(pool, project = s$project_id, study = s$study, experiment = s$experiment)
    })

    output$weights_table <- DT::renderDataTable({
      df <- weights_rows(); shiny::req(nrow(df) > 0)
      df$sigma <- round(df$sigma, 4); df$w <- round(df$w, 4); df$w_norm <- round(df$w_norm, 4)
      cols <- intersect(c("antigen", "feature", "curve_id", "method", "sampleid",
                          "patientid", "timeperiod", "agroup", "se", "pcov",
                          "sigma", "w", "w_norm"), names(df))
      DT::datatable(df[, cols, drop = FALSE], rownames = FALSE, filter = "top",
                    options = list(scrollX = TRUE, pageLength = 20))
    }, server = TRUE)
  })
}

# ---------------------------------------------------------------------------
# Validate an uploaded "fill in agroup" Excel file against the calib_samples
# rows it's meant to patch. Plain function (no Shiny deps) so it's directly
# testable. Returns an issues data.frame (sheet/severity/column/message),
# same shape as assay_import_contract.R's validator -- empty/0-row means
# clean. Only checks what update_calib_samples_agroup() actually needs:
# the full existing key must be present and must match a real row, and at
# least one row must actually carry a non-blank agroup (otherwise the
# upload accomplishes nothing).
# ---------------------------------------------------------------------------
validate_agroup_upload <- function(uploaded, existing) {
  issue <- function(col, msg, severity = "error")
    data.frame(sheet = "upload", severity = severity, column = col, message = msg,
              stringsAsFactors = FALSE)
  out <- list()

  missing_cols <- setdiff(c(WEIGHTS_IDENTITY_COLS, "agroup"), names(uploaded))
  if (length(missing_cols)) {
    out[[length(out) + 1L]] <- issue(paste(missing_cols, collapse = ", "),
      paste("Missing required column(s):", paste(missing_cols, collapse = ", ")))
    return(do.call(rbind, out))   # can't check row-matching without the key columns
  }

  key <- function(df) do.call(paste, c(df[WEIGHTS_IDENTITY_COLS], sep = "\u001f"))
  uploaded_key <- key(uploaded)
  existing_key <- if (nrow(existing)) key(existing) else character(0)

  dup <- duplicated(uploaded_key)
  if (any(dup))
    out[[length(out) + 1L]] <- issue("curve_id/method/sampleid/patientid/timeperiod/dilution",
      sprintf("%d duplicate row key(s) in the upload.", sum(dup)))

  orphan <- !uploaded_key %in% existing_key
  if (any(orphan))
    out[[length(out) + 1L]] <- issue("curve_id/method/sampleid/patientid/timeperiod/dilution",
      sprintf("%d row(s) don't match any existing calib_samples row for this scope/method -- check for typos in the identity columns.",
              sum(orphan)))

  blank <- is.na(uploaded$agroup) | !nzchar(trimws(as.character(uploaded$agroup)))
  if (all(blank))
    out[[length(out) + 1L]] <- issue("agroup", "agroup is blank on every row -- nothing to upload.")
  else if (any(blank))
    out[[length(out) + 1L]] <- issue("agroup",
      sprintf("%d row(s) still have a blank agroup (allowed -- partial fill-in is fine).",
              sum(blank)), severity = "warning")

  if (!length(out)) data.frame(sheet = character(), severity = character(),
                               column = character(), message = character(),
                               stringsAsFactors = FALSE)
  else do.call(rbind, out)
}
