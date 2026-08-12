# =============================================================================
# study_overview.R  --  Study Overview, rebuilt as a lazy module  [Phase 1]
# -----------------------------------------------------------------------------
# Replaces the former study_overview_ui.R monolith (one giant
# observeEvent(input$study_level_tabs) that eagerly ran preprocess_plate_data()
# -- every pull_* query, including the stale best_* fit paths -- on tab entry,
# which crashed the whole app.
#
# ARCHITECTURE (the point of the rewrite):
#   * A tabsetPanel where EACH view owns its data.
#   * Nothing computes on tab entry. Each pane is a renderUI/render* gated by
#     req(input$view == "<that view>"), so a hidden pane never runs its queries
#     (outputs are suspendWhenHidden = TRUE by default).
#   * Each view's loader is an eventReactive(..., ignoreInit = TRUE) wrapped with
#     bindCache(scope$project, scope$study): first open loads, re-open is instant,
#     changing study invalidates.
#   * Per-pane withSpinner() gives scoped feedback (no global blocking notice).
#   * Data sources per view (Phase 2/3): xmap_* for specimen/bead/CV/arms;
#     annotation_* for labels/order/families; calib_* + curve_lookup for the fit
#     views. NOTHING here reads best_* -- those are stale and deliberately unused.
#
# STATUS: Phase 1 stands up the module + lazy shell and STOPS THE CRASH. Each
# pane is a placeholder marked with the loader/render it will receive. The
# xmap_* views (Phase 2) and the calib_* fit views with a method dimension
# (Phase 3) fill these panes in place, one at a time, with no further structural
# change.
#
# Scope: study-level, all experiments (unchanged from the original).
# Mount:  ui_handler body_tabs -> studyOverviewUI("study_overview")
#         app server           -> studyOverviewServer("study_overview",
#                                    pool = db_pool,
#                                    project = userWorkSpaceID,
#                                    study   = reactive(input$readxMap_study_accession),
#                                    user    = currentuser)
# =============================================================================

# ---- the views, in tab order. label = tab title; value = input$view token;
#      phase = which build fills it; source = data layer it will read. ----------
# Matches the live overview's five sub-tabs (the old CV tab is commented out in
# the app, so it is intentionally omitted here).
.SO_VIEWS <- list(
  list(id = "bcs",       label = "Blanks, Controls & Standards", phase = 2, src = "xmap_*"),
  list(id = "beadflags", label = "High-Aggregate & Low Bead",    phase = 2, src = "xmap_*"),
  list(id = "arms",      label = "Samples by Arm",               phase = 2, src = "xmap_sample + annotation_* (arm order / referent)"),
  list(id = "timepoint", label = "Samples by Timepoint",         phase = 2, src = "xmap_sample + annotation_* (timepoint order)"),
  list(id = "fit",       label = "Sample Estimate Quality",      phase = 3, src = "calib_* + curve_lookup (method: bayesian/frequentist)")
)

# 21-colour palette used by the timepoint / arm grids (from the original overview)
.so_kelly_pal <- c("#f3c300","#875692","#f38400","#a1caf1","#be0032","#c2b280","#848482",
                   "#008856","#e68fac","#0067a5","#f99379","#604e97","#f6a600","#b3446c",
                   "#dcd300","#882d17","#8db600","#654522","#e25822","#2b3d26","lightgrey")

# treat these timeperiod/arm values as "not defined"
.so_undef <- function(x) {
  x <- trimws(as.character(x))
  x[!is.na(x) & nzchar(x) & !tolower(x) %in% c("__none__", "none", "na", "null")]
}

# =============================================================================
# UI
# =============================================================================
studyOverviewUI <- function(id) {
  ns <- shiny::NS(id)
  panes <- lapply(.SO_VIEWS, function(v)
    shiny::tabPanel(
      title = v$label, value = v$id,
      shiny::br(),
      shinycssloaders::withSpinner(shiny::uiOutput(ns(paste0("pane_", v$id))),
                                   type = 4, color = "#2c5aa0")
    ))
  shiny::tagList(
    shiny::uiOutput(ns("header")),
    do.call(shiny::tabsetPanel, c(list(id = ns("view"), type = "tabs"), panes))
  )
}

# =============================================================================
# Server
# =============================================================================
# pool     : a pool::Pool (db_pool)
# project  : reactive/function returning the workspace/project id
# study    : reactive/function returning the selected study_accession
# user     : reactive/function returning the current user
studyOverviewServer <- function(id, pool, project, study, user) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    .val <- function(x) if (is.function(x)) x() else x   # accept reactive or plain

    # Resolved study-level scope; every loader depends on this. A blank/"Click
    # here" study short-circuits every pane to a friendly prompt (no queries).
    scope <- shiny::reactive({
      s <- tryCatch(.val(study), error = function(e) NULL)
      list(
        project = tryCatch(.val(project), error = function(e) NA),
        study   = s,
        user    = tryCatch(.val(user), error = function(e) NA_character_),
        ok      = !is.null(s) && length(s) == 1 && !is.na(s) &&
                  nzchar(trimws(s)) && !identical(s, "Click here")
      )
    })

    output$header <- shiny::renderUI({
      sc <- scope()
      if (!isTRUE(sc$ok))
        return(shiny::div(class = "alert alert-info",
                          "Select a study in the sidebar to see its overview."))
      shiny::h3(sprintf("Study Overview \u2014 %s", sc$study))
    })

    # ---- placeholder card (Phase 1). Phase 2/3 replace each pane body. --------
    .pending <- function(v) {
      shiny::div(
        class = "well",
        shiny::h4(v$label),
        shiny::p(class = "text-muted",
          sprintf("On-demand view (Phase %d). Loads from %s only when this tab is open.",
                  v$phase, v$src)),
        if (identical(v$id, "fit"))
          shiny::p(class = "text-muted",
            "Will read the live calib_* tables via curve_lookup, with a method",
            " selector (bayesian / frequentist) rather than any best_* view.")
      )
    }

    # =========================================================================
    # VIEW: Blanks, Controls & Standards  [Phase 2 -- xmap_*, no best_*]
    # -------------------------------------------------------------------------
    # Lazy + cached: so_nonsample_summspec() runs the first time this tab is
    # opened for a given (project, study) and is reused thereafter. bindCache
    # keys on the scope so switching study invalidates. specimen_type_order is
    # added here (make_summspec doesn't emit it) for the plot's stacking order.
    bcs_data <- shiny::reactive({
      sc <- scope(); shiny::req(sc$ok, input$view == "bcs")
      ss <- so_nonsample_summspec(pool, sc$project, sc$study)
      if (is.null(ss) || !nrow(ss)) return(ss)
      ord <- c(blank = 1L, control = 2L, standard = 3L)
      ss$specimen_type_order <- unname(ord[ss$specimen_type])
      ss
    }) |> shiny::bindCache(scope()$project, scope()$study)

    output$bcs_plot <- shiny::renderPlot({
      ss <- bcs_data(); shiny::req(ss, nrow(ss) > 0)
      make_timeperiod_grid(
        df = ss, x_var = "analyte", y_var = "plate",
        time_var = "specimen_type", count_var = "n",
        time_var_order = "specimen_type_order",
        time_var_palette = c("blank" = "#f3c300", "control" = "#2b3d26",
                             "standard" = "#a1caf1"),
        title_var = "Summary of Non-Sample Specimen Types by Plate and Analyte")
    })

    output$bcs_table <- DT::renderDT({
      ss <- bcs_data(); shiny::req(ss, nrow(ss) > 0)
      tbl <- ss[, c("analyte", "plate", "specimen_type", "n"), drop = FALSE]
      tbl <- tbl[order(tbl$analyte, tbl$plate, tbl$specimen_type), , drop = FALSE]
      DT::datatable(tbl, rownames = FALSE, filter = "top",
                    options = list(pageLength = 15, scrollX = TRUE))
    })

    output$bcs_dl_data <- shiny::downloadHandler(
      filename = function() sprintf("bcs_summary_%s.csv", scope()$study %||% "study"),
      content  = function(file) {
        ss <- bcs_data()
        utils::write.csv(if (is.null(ss)) data.frame() else ss, file, row.names = FALSE)
      })
    output$bcs_dl_plot <- shiny::downloadHandler(
      filename = function() sprintf("bcs_plot_%s.png", scope()$study %||% "study"),
      content  = function(file) {
        ss <- bcs_data(); shiny::req(ss, nrow(ss) > 0)
        p <- make_timeperiod_grid(
          df = ss, x_var = "analyte", y_var = "plate",
          time_var = "specimen_type", count_var = "n",
          time_var_order = "specimen_type_order",
          time_var_palette = c("blank" = "#f3c300", "control" = "#2b3d26",
                               "standard" = "#a1caf1"),
          title_var = "Summary of Non-Sample Specimen Types by Plate and Analyte")
        ggplot2::ggsave(file, p, width = 12, height = 8, dpi = 150)
      })

    .bcs_pane <- function() {
      shiny::tagList(
        shiny::fluidRow(
          shiny::column(6, shiny::downloadButton(ns("bcs_dl_plot"), "Download plot")),
          shiny::column(6, shiny::downloadButton(ns("bcs_dl_data"), "Download data (CSV)"))),
        shiny::br(),
        shiny::plotOutput(ns("bcs_plot"), height = "640px"),
        shiny::hr(),
        DT::DTOutput(ns("bcs_table"))
      )
    }

    # =========================================================================
    # VIEW: Samples by Timepoint  [Phase 2 -- xmap_sample + annotation order]
    # -------------------------------------------------------------------------
    # Guard (per spec): if the timepoint dimension is undefined for the study
    # (all NULL / '__none__' / blank), render a notice and NO figure. Ordering
    # comes from annotation_order via get_order() at the study-level natural key
    # ("__none__"), falling back to natural order -- replacing the stale
    # fetch_study_configuration(param_name='timeperiod_order') read.
    tp_data <- shiny::reactive({
      sc <- scope(); shiny::req(sc$ok, input$view == "timepoint")
      raw <- so_raw_samples(pool, sc$project, sc$study)
      levs <- if (is.null(raw) || !nrow(raw)) character(0) else .so_undef(raw$timeperiod)
      if (!length(levs)) return(list(defined = FALSE))
      summ <- summarise_by_timeperiod(raw)
      if (is.null(summ) || !nrow(summ)) return(list(defined = TRUE, summ = NULL))
      ord <- tryCatch(get_order(pool, sc$project, sc$study, "__none__", "timeperiod"),
                      error = function(e) character(0))
      if (!length(ord)) ord <- unique(summ$timeperiod)
      idx <- stats::setNames(seq_along(ord), ord)
      o <- unname(idx[as.character(summ$timeperiod)])           # NA for values not in order
      if (anyNA(o)) {                                           # append any unordered levels
        extra <- unique(summ$timeperiod[is.na(o)]); base <- length(ord)
        for (i in seq_along(extra)) o[summ$timeperiod == extra[i]] <- base + i
      }
      summ$timeperiod_order <- as.integer(o)
      list(defined = TRUE, summ = summ)
    }) |> shiny::bindCache(scope()$project, scope()$study)

    .tp_plot <- function() {
      d <- tp_data(); shiny::req(isTRUE(d$defined), !is.null(d$summ), nrow(d$summ) > 0)
      make_timeperiod_grid(
        df = d$summ, x_var = "analyte", y_var = "plate", time_var = "timeperiod",
        count_var = "n", time_var_order = "timeperiod_order",
        time_var_palette = .so_kelly_pal,
        title_var = "Number of Samples by Analyte, Plate, and Timepoint")
    }
    output$tp_plot  <- shiny::renderPlot({ .tp_plot() })
    output$tp_table <- DT::renderDT({
      d <- tp_data(); shiny::req(isTRUE(d$defined), !is.null(d$summ), nrow(d$summ) > 0)
      dt <- create_timeperiod_table(d$summ)
      names(dt)[names(dt) == "timeperiod"] <- "timepoint"
      DT::datatable(dt, rownames = FALSE, filter = "top",
                    caption = "Number of Samples by Analyte, Plate, and Timepoint")
    })
    output$tp_dl_plot <- shiny::downloadHandler(
      filename = function() sprintf("timepoint_plot_%s.pdf", scope()$study %||% "study"),
      content  = function(file) ggplot2::ggsave(file, .tp_plot(), device = "pdf",
                                                width = 20, height = 10, units = "in"))
    output$tp_dl_data <- shiny::downloadHandler(
      filename = function() sprintf("timepoint_data_%s.csv", scope()$study %||% "study"),
      content  = function(file) {
        d <- tp_data(); utils::write.csv(if (is.null(d$summ)) data.frame() else d$summ,
                                         file, row.names = FALSE)
      })

    .tp_pane <- function() {
      d <- tp_data()
      if (!isTRUE(d$defined))
        return(shiny::div(class = "alert alert-warning",
          "No timepoint variable is defined for this study, so there is nothing to plot."))
      shiny::tagList(
        shiny::fluidRow(
          shiny::column(6, shiny::downloadButton(ns("tp_dl_plot"), "Download plot")),
          shiny::column(6, shiny::downloadButton(ns("tp_dl_data"), "Download data (CSV)"))),
        shiny::br(),
        shiny::plotOutput(ns("tp_plot"), height = "640px"),
        shiny::hr(),
        DT::DTOutput(ns("tp_table"))
      )
    }

    # =========================================================================
    # VIEW: High-Aggregate & Low Bead  [Phase 2 -- xmap_* incl. raw_sample]
    # -------------------------------------------------------------------------
    # Uses the FULL summspec (adds raw_sample rows) so each specimen carries
    # nlowbead / nhighbeadagg. Three selectors drive the plot; the source
    # selector only appears for standards. make_antigen_plate_bead() returns
    # NULL when nothing fails the thresholds -> we show a notice instead.
    bead_data <- shiny::reactive({
      sc <- scope(); shiny::req(sc$ok, input$view == "beadflags")
      so_full_summspec(pool, sc$project, sc$study)
    }) |> shiny::bindCache(scope()$project, scope()$study)

    output$bead_analyte_ui <- shiny::renderUI({
      ss <- bead_data(); shiny::req(ss, nrow(ss) > 0)
      ch <- sort(unique(ss$analyte[!is.na(ss$analyte)]))
      shinyWidgets::radioGroupButtons(ns("bead_analyte"), "Select Analyte:",
        choices = ch, selected = ch[1], status = "success")
    })
    output$bead_specimen_ui <- shiny::renderUI({
      ss <- bead_data(); shiny::req(ss, nrow(ss) > 0)
      ch <- intersect(c("blank", "control", "standard", "raw_sample"),
                      unique(ss$specimen_type))
      shinyWidgets::radioGroupButtons(ns("bead_specimen"), "Select Specimen Type:",
        choices = ch, selected = ch[1], status = "success")
    })
    output$bead_source_ui <- shiny::renderUI({
      ss <- bead_data()
      shiny::req(ss, nrow(ss) > 0, identical(input$bead_specimen, "standard"))
      ch <- unique(stats::na.omit(ss$std_source))
      if (!length(ch)) return(NULL)
      shinyWidgets::radioGroupButtons(ns("bead_source"), "Select Standard Curve Source:",
        choices = ch, selected = ch[1], status = "success")
    })

    # plot + underlying table (list); plot is NULL when nothing fails thresholds
    .bead_res <- shiny::reactive({
      ss <- bead_data()
      shiny::req(ss, nrow(ss) > 0, input$bead_analyte, input$bead_specimen)
      bd <- ss[ss$specimen_type %in% c("blank", "control", "standard", "raw_sample"), , drop = FALSE]
      if (identical(input$bead_specimen, "standard") && !is.null(input$bead_source))
        bd <- bd[!is.na(bd$std_source) & bd$std_source == input$bead_source, , drop = FALSE]
      bd <- bd[, c("analyte", "antigen", "plate", "specimen_type",
                   "nhighbeadagg", "nlowbead"), drop = FALSE]
      names(bd)[names(bd) == "nhighbeadagg"] <- "HighAggregates"
      names(bd)[names(bd) == "nlowbead"]     <- "LowBeads"
      bd <- dplyr::summarise(
        dplyr::group_by(bd, analyte, antigen, plate, specimen_type),
        LowBeads = sum(LowBeads, na.rm = TRUE),
        HighAggregates = sum(HighAggregates, na.rm = TRUE), .groups = "drop")
      lb <- tidyr::pivot_longer(bd, cols = c("HighAggregates", "LowBeads"),
                                names_to = "Type", values_to = "N_wells")
      lb$antigen <- factor(lb$antigen); lb$plate <- factor(lb$plate)
      ttl <- paste("Bead counts failing thresholds:", input$bead_analyte, input$bead_specimen)
      list(plot = make_antigen_plate_bead(lb, input$bead_specimen, input$bead_analyte, ttl),
           data = lb)
    })

    output$bead_plot <- shiny::renderPlot({
      p <- .bead_res()$plot; shiny::req(!is.null(p)); p
    })
    output$bead_msg <- shiny::renderUI({
      p <- .bead_res()$plot
      if (is.null(p))
        shiny::div(class = "alert alert-info",
          "No failing bead count for this combination of specimen type and analyte.")
      else NULL
    })
    output$bead_dl_plot <- shiny::downloadHandler(
      filename = function() sprintf("bead_failing_%s.pdf", scope()$study %||% "study"),
      content  = function(file) {
        p <- .bead_res()$plot; shiny::req(!is.null(p))
        ggplot2::ggsave(file, p, device = "pdf", width = 20, height = 10, units = "in")
      })
    output$bead_dl_data <- shiny::downloadHandler(
      filename = function() sprintf("bead_failing_%s.csv", scope()$study %||% "study"),
      content  = function(file) utils::write.csv(.bead_res()$data, file, row.names = FALSE))

    .bead_pane <- function() {
      shiny::tagList(
        shiny::fluidRow(
          shiny::column(4, shiny::uiOutput(ns("bead_analyte_ui"))),
          shiny::column(4, shiny::uiOutput(ns("bead_specimen_ui"))),
          shiny::column(4, shiny::uiOutput(ns("bead_source_ui")))),
        shiny::fluidRow(
          shiny::column(6, shiny::downloadButton(ns("bead_dl_plot"), "Download plot")),
          shiny::column(6, shiny::downloadButton(ns("bead_dl_data"), "Download data (CSV)"))),
        shiny::br(),
        shiny::uiOutput(ns("bead_msg")),
        shiny::plotOutput(ns("bead_plot"), height = "640px")
      )
    }

    # =========================================================================
    # VIEW: Samples by Arm  [Phase 2 -- xmap_sample + annotation_level referent]
    # -------------------------------------------------------------------------
    # Guard (per spec): if the arm dimension (agroup) is undefined for the study,
    # render a notice and NO figures. Reference arm comes from annotation_level
    # (is_referent for variable 'agroup'), falling back to the first arm --
    # replacing the stale fetch_study_configuration(param='reference_arm') read.
    # Arms and participant counts are derived from raw_samples (avoids the old
    # fetch_study_arms/fetch_study_participant_arms helpers, which rely on a
    # global `conn`).
    arm_data <- shiny::reactive({
      sc <- scope(); shiny::req(sc$ok, input$view == "arms")
      raw <- so_raw_samples(pool, sc$project, sc$study)
      arms <- if (is.null(raw) || !nrow(raw)) character(0) else .so_undef(unique(raw$agroup))
      if (!length(arms)) return(list(defined = FALSE))
      ref <- tryCatch({
        la <- get_level_annotations(pool, sc$project, sc$study, "__none__", "agroup")
        r <- if (is.null(la) || !nrow(la)) character(0) else la$level[which(la$is_referent %in% TRUE)]
        if (length(r)) as.character(r[1]) else NA_character_
      }, error = function(e) NA_character_)
      arms_sorted <- if (!is.na(ref) && ref %in% arms) c(ref, setdiff(arms, ref)) else arms
      prepared <- prepare_arm_balance_data(raw, arms_sorted)
      pa <- dplyr::summarise(
        dplyr::group_by(raw, experiment_accession, agroup),
        num_patients = dplyr::n_distinct(patientid), .groups = "drop")
      pa$agroup <- trimws(pa$agroup)
      list(defined = TRUE, prepared = prepared, participant = pa,
           ref = ref, study = sc$study)
    }) |> shiny::bindCache(scope()$project, scope()$study)

    .arm_plot <- function() {
      d <- arm_data(); shiny::req(isTRUE(d$defined), !is.null(d$prepared), nrow(d$prepared) > 0)
      make_timeperiod_grid_stacked(
        df = d$prepared, x_var = "analyte", y_var = "plate", time_var = "arm",
        count_var = "proportion", time_var_order = "agroup_order",
        time_var_palette = .so_kelly_pal,
        title_var = paste(d$study, "- Proportion of Samples by Study Arms, Plate, and Analyte"))
    }
    output$arm_plot <- shiny::renderPlot({ .arm_plot() })
    output$arm_table <- DT::renderDT({
      d <- arm_data(); shiny::req(isTRUE(d$defined), !is.null(d$prepared), nrow(d$prepared) > 0)
      pad <- d$prepared[order(d$prepared$agroup_order),
                        c("plate", "analyte", "agroup", "proportion"), drop = FALSE]
      names(pad)[names(pad) == "agroup"] <- "arm"
      DT::datatable(pad, rownames = FALSE, filter = "top",
        caption = "Sample Proportions Across Study Arms Stratified by Analyte and Plate")
    })
    output$arm_distribution <- plotly::renderPlotly({
      d <- arm_data(); shiny::req(isTRUE(d$defined), !is.null(d$participant), nrow(d$participant) > 0)
      plot_study_arm_distribution(patients_arm = d$participant)
    })
    output$arm_dl_plot <- shiny::downloadHandler(
      filename = function() sprintf("arm_balance_%s.pdf", scope()$study %||% "study"),
      content  = function(file) ggplot2::ggsave(file, .arm_plot(), device = "pdf",
                                                width = 20, height = 10, units = "in"))
    output$arm_dl_data <- shiny::downloadHandler(
      filename = function() sprintf("arm_balance_%s.csv", scope()$study %||% "study"),
      content  = function(file) {
        d <- arm_data(); pad <- if (is.null(d$prepared)) data.frame() else
          d$prepared[order(d$prepared$agroup_order), , drop = FALSE]
        utils::write.csv(pad, file, row.names = FALSE)
      })

    .arm_pane <- function() {
      d <- arm_data()
      if (!isTRUE(d$defined))
        return(shiny::div(class = "alert alert-warning",
          "No arm variable is defined for this study, so there is nothing to plot."))
      shiny::tagList(
        shiny::fluidRow(
          shiny::column(6, shiny::downloadButton(ns("arm_dl_plot"), "Download plot")),
          shiny::column(6, shiny::downloadButton(ns("arm_dl_data"), "Download data (CSV)"))),
        shiny::br(),
        shiny::plotOutput(ns("arm_plot"), height = "640px"),
        shiny::hr(),
        shiny::h4("Participant distribution by experiment & arm"),
        plotly::plotlyOutput(ns("arm_distribution"), height = "420px"),
        shiny::hr(),
        DT::DTOutput(ns("arm_table"))
      )
    }

    # =========================================================================
    # VIEW: Sample Estimate Quality  [Phase 3 -- calib_* + curve_lookup]
    # -------------------------------------------------------------------------
    # Reads live fits (NOT best_*): so_fit_glance (crit/source per is_best curve)
    # + so_fit_sample_summary (LOQ/LOD gating counts from calib_samples +
    # calib_diagnostics), per method (bayesian/frequentist -> selector). Merged
    # by prep_analyte_fit_summary; crit is collapsed to Model / No Model here
    # (the model-name vocabulary is gompertz4/logistic4/logistic5/loglogistic5/…,
    # so "any real fit = Model" rather than a hard-coded name list). Shows the
    # "once standard curves are saved" notice when a study has no fits.
    fit_methods <- shiny::reactive({
      sc <- scope(); shiny::req(sc$ok, input$view == "fit")
      so_fit_methods(pool, sc$project, sc$study)
    })
    output$fit_method_ui <- shiny::renderUI({
      m <- fit_methods(); shiny::req(length(m) > 0)
      shinyWidgets::radioGroupButtons(ns("fit_method"), "Method:",
        choices = m, selected = m[1], status = "success")
    })

    fit_preped <- shiny::reactive({
      sc <- scope(); shiny::req(sc$ok, input$view == "fit", input$fit_method)
      glance <- so_fit_glance(pool, sc$project, sc$study, methods = input$fit_method)
      samp   <- so_fit_sample_summary(pool, sc$project, sc$study, input$fit_method)
      if (is.null(samp) || !nrow(samp)) return(NULL)
      if (is.null(glance) || !nrow(glance))
        glance <- data.frame(plateid = character(), antigen = character(),
                             analyte = character(), crit = character(),
                             source = character())
      pd <- prep_analyte_fit_summary(samp, glance)
      pd$crit[!is.na(pd$crit) & pd$crit != "No Model"] <- "Model"   # fitted vs not
      pd
    }) |> shiny::bindCache(scope()$project, scope()$study, input$fit_method)

    output$fit_source_ui <- shiny::renderUI({
      pd <- fit_preped(); shiny::req(!is.null(pd), nrow(pd) > 0)
      ch <- unique(pd$source[!is.na(pd$source)])
      shiny::radioButtons(ns("fit_source"), "Source:", choices = ch, selected = ch[1])
    })
    output$fit_analyte_ui <- shiny::renderUI({
      pd <- fit_preped(); shiny::req(!is.null(pd), input$fit_source)
      pd <- pd[!is.na(pd$source) & pd$source == input$fit_source, , drop = FALSE]
      ch <- unique(pd$analyte[!is.na(pd$analyte)])
      shinyWidgets::radioGroupButtons(ns("fit_analyte"), "Analyte:",
        choices = ch, selected = ch[1], status = "success")
    })

    # plot + proportion table (list(plot, proportion_df)), source-filtered
    .fit_res <- shiny::reactive({
      pd <- fit_preped(); shiny::req(!is.null(pd), input$fit_source, input$fit_analyte)
      pd <- pd[!is.na(pd$source) & pd$source == input$fit_source, , drop = FALSE]
      plot_preped_analyte_fit_summary(preped_data = pd, analyte_selector = input$fit_analyte)
    })
    output$fit_plot <- shiny::renderPlot({ .fit_res()[[1]] })
    output$fit_table <- DT::renderDT({
      pf <- .fit_res()[[2]]; shiny::req(!is.null(pf))
      keep <- intersect(c("plate", "antigen", "analyte", "model_class", "crit",
                          "fit_category", "count", "proportion"), names(pf))
      DT::datatable(pf[, keep, drop = FALSE], rownames = FALSE, filter = "top",
                    caption = "Sample Estimate Quality by Plate and Antigen")
    })
    output$fit_dl_plot <- shiny::downloadHandler(
      filename = function() sprintf("fit_summary_%s.pdf", scope()$study %||% "study"),
      content  = function(file) ggplot2::ggsave(file, .fit_res()[[1]], device = "pdf",
                                                width = 20, height = 10, units = "in"))
    output$fit_dl_data <- shiny::downloadHandler(
      filename = function() sprintf("fit_specimen_%s.csv", scope()$study %||% "study"),
      content  = function(file) utils::write.csv(fit_preped(), file, row.names = FALSE))
    output$fit_dl_prop <- shiny::downloadHandler(
      filename = function() sprintf("fit_proportion_%s.csv", scope()$study %||% "study"),
      content  = function(file) utils::write.csv(.fit_res()[[2]], file, row.names = FALSE))

    .fit_pane <- function() {
      if (!length(fit_methods()))
        return(shiny::div(class = "alert alert-info",
          "Once standard curves are saved, Sample Estimate Quality will be available for inspection."))
      shiny::tagList(
        shiny::fluidRow(
          shiny::column(4, shiny::uiOutput(ns("fit_method_ui"))),
          shiny::column(4, shiny::uiOutput(ns("fit_source_ui"))),
          shiny::column(4, shiny::uiOutput(ns("fit_analyte_ui")))),
        shiny::fluidRow(
          shiny::column(4, shiny::downloadButton(ns("fit_dl_plot"), "Download plot")),
          shiny::column(4, shiny::downloadButton(ns("fit_dl_data"), "Download data")),
          shiny::column(4, shiny::downloadButton(ns("fit_dl_prop"), "Download proportions"))),
        shiny::br(),
        shiny::plotOutput(ns("fit_plot"), height = "800px"),
        shiny::hr(),
        DT::DTOutput(ns("fit_table"))
      )
    }

    # ---- lazy panes -----------------------------------------------------------
    # Each pane renders only when its tab is active AND a study is chosen. Phase 2
    # fills a pane by branching here to its real UI (see "bcs"); the rest still
    # show the placeholder until built.
    lapply(.SO_VIEWS, function(v) {
      output[[paste0("pane_", v$id)]] <- shiny::renderUI({
        shiny::req(input$view == v$id)         # laziness: only the open tab runs
        sc <- scope()
        if (!isTRUE(sc$ok))
          return(shiny::div(class = "alert alert-info",
                            "Choose a study in the sidebar first."))
        if (identical(v$id, "bcs")) return(.bcs_pane())
        if (identical(v$id, "timepoint")) return(.tp_pane())
        if (identical(v$id, "beadflags")) return(.bead_pane())
        if (identical(v$id, "arms")) return(.arm_pane())
        if (identical(v$id, "fit")) return(.fit_pane())
        .pending(v)
      })
    })

    invisible(NULL)
  })
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
