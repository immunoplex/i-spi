# =============================================================================
# annotation_ui.R  --  the "Annotations" tab (config/annotation separation)
# -----------------------------------------------------------------------------
# One module for all annotation authoring at the natural key
# (project_id, study_accession, experiment_accession):
#   * Analytes      — editable grid of antigen & feature facts
#                     (source, virus/bacterial strain, catalog, batch/lot,
#                      reagent acq date, family)
#   * Sample Levels — editable grid of timeperiod & agroup level descriptions
#                     + a single referent level per variable
#   * Display Order — drag-to-order (shinyjqui::orderInput) for antigen,
#                     feature, timeperiod, agroup (comma-separated store)
#
# Reads/writes go through annotation_access.R (source it before this file).
# Requires rhandsontable + shinyjqui (already loaded in global.R).
#
# Mount:
#   UI:     annotation_ui("annotations")
#   server: annotation_server("annotations", db_pool,
#                             project_id = reactive(userWorkSpaceID()),
#                             study      = reactive(input$readxMap_study_accession),
#                             user       = reactive(currentuser()))
# =============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

.ANN_ANALYTE_COLS <- c("source", "virus_bacterial_strain", "catalog_number",
                       "batch_lot", "reagent_acq_date", "family")

# ---- grid builders (universe left-joined with saved annotation) -------------
.ann_build_analyte_grid <- function(pool, p, s, e, type) {
  univ <- tryCatch(list_universe(pool, p, s, e, type), error = function(err) character())
  df <- data.frame(analyte = univ, stringsAsFactors = FALSE)
  for (col in .ANN_ANALYTE_COLS) df[[col]] <- NA_character_
  ann <- tryCatch(get_analyte_annotations(pool, p, s, e, type),
                  error = function(err) data.frame())
  if (nrow(df) && nrow(ann)) {
    idx <- match(df$analyte, ann$analyte)
    for (col in .ANN_ANALYTE_COLS) if (col %in% names(ann)) {
      val <- as.character(ann[[col]][idx])
      df[[col]] <- ifelse(is.na(idx), df[[col]], val)
    }
  }
  df
}

.ann_build_level_grid <- function(pool, p, s, e, variable) {
  univ <- tryCatch(list_universe(pool, p, s, e, variable), error = function(err) character())
  df <- data.frame(level = univ, description = NA_character_, stringsAsFactors = FALSE)
  ann <- tryCatch(get_level_annotations(pool, p, s, e, variable),
                  error = function(err) data.frame())
  if (nrow(df) && nrow(ann)) {
    idx <- match(df$level, ann$level)
    df$description <- ifelse(is.na(idx), NA_character_, as.character(ann$description[idx]))
  }
  df
}

.ann_current_referent <- function(pool, p, s, e, variable) {
  ann <- tryCatch(get_level_annotations(pool, p, s, e, variable),
                  error = function(err) data.frame())
  if (!nrow(ann) || !"is_referent" %in% names(ann)) return("")
  ref <- ann$level[!is.na(ann$is_referent) & as.logical(ann$is_referent)]
  if (length(ref)) as.character(ref[1]) else ""
}


# ---- UI ---------------------------------------------------------------------
annotation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(5, uiOutput(ns("experiment_ui"))),
      column(7, tags$p(tags$small(
        "Annotations are saved per experiment at (project, study, experiment). ",
        "Pick an experiment, edit, and Save each section.")))
    ),
    tabsetPanel(
      id = ns("ann_tabs"),
      tabPanel(
        "Analytes",
        tags$h4("Antigen annotations"),
        rhandsontable::rHandsontableOutput(ns("hot_antigen")),
        tags$div(style = "margin-top:8px;",
                 actionButton(ns("save_antigen"), "Save antigen annotations", class = "btn-primary")),
        tags$hr(),
        tags$h4("Feature annotations"),
        rhandsontable::rHandsontableOutput(ns("hot_feature")),
        tags$div(style = "margin-top:8px;",
                 actionButton(ns("save_feature"), "Save feature annotations", class = "btn-primary"))
      ),
      tabPanel(
        "Sample Levels",
        tags$h4("Timepoints"),
        rhandsontable::rHandsontableOutput(ns("hot_timeperiod")),
        selectInput(ns("ref_timeperiod"), "Referent timepoint", choices = c("(none)" = "")),
        tags$div(style = "margin-top:8px;",
                 actionButton(ns("save_timeperiod"), "Save timepoint annotations", class = "btn-primary")),
        tags$hr(),
        tags$h4("Arms / Groups"),
        rhandsontable::rHandsontableOutput(ns("hot_agroup")),
        selectInput(ns("ref_agroup"), "Referent arm/group", choices = c("(none)" = "")),
        tags$div(style = "margin-top:8px;",
                 actionButton(ns("save_agroup"), "Save arm/group annotations", class = "btn-primary"))
      ),
      tabPanel(
        "Display Order",
        tags$p(tags$small("Drag items to set the display order used across the app.")),
        fluidRow(
          column(6, shinyjqui::orderInput(ns("order_antigen"), "Antigen order",
                                          items = character(), width = "100%", item_class = "primary")),
          column(6, shinyjqui::orderInput(ns("order_feature"), "Feature order",
                                          items = character(), width = "100%", item_class = "primary"))
        ),
        tags$br(),
        fluidRow(
          column(6, shinyjqui::orderInput(ns("order_timeperiod"), "Timepoint order",
                                          items = character(), width = "100%", item_class = "info")),
          column(6, shinyjqui::orderInput(ns("order_agroup"), "Arm/Group order",
                                          items = character(), width = "100%", item_class = "info"))
        ),
        tags$div(style = "margin-top:12px;",
                 actionButton(ns("save_order"), "Save display order", class = "btn-primary"))
      )
    )
  )
}


# ---- server -----------------------------------------------------------------
annotation_server <- function(id, pool, project_id, study, user) {
  .ann_dbg(sprintf("[annotation] annotation_server() WIRED for id='%s'\n", id))
  moduleServer(id, function(input, output, session) {
    .ann_dbg(sprintf("[annotation] module '%s' server INITIALISING\n", id))

    refresh <- reactiveVal(0)
    bump    <- function() refresh(isolate(refresh()) + 1)
    exp     <- reactive({ req(input$experiment); input$experiment })
    scope_ok <- reactive(
      !is.null(project_id()) && !is.null(study()) &&
      !is.null(input$experiment) && nzchar(input$experiment))

    # experiment picker follows project + study. Rendered (not updated) so the
    # choices survive studyParameters_UI re-rendering on study change (a plain
    # updateSelectizeInput fires before the re-render recreates the control and
    # is lost).
    output$experiment_ui <- renderUI({
      p <- project_id(); s <- study()
      .ann_dbg(sprintf("[annotation] experiment_ui render: project=%s study=%s\n",
                  if (is.null(p)) "NULL" else p, if (is.null(s)) "NULL" else s))
      req(p, s)
      exps <- tryCatch(list_experiments(pool, p, s),
                       error = function(e) {
                         .ann_dbg("[annotation] list_experiments error:", conditionMessage(e), "\n")
                         character()
                       })
      .ann_dbg(sprintf("[annotation] -> %d experiment choice(s)\n", length(exps)))
      selectizeInput(session$ns("experiment"), "Experiment",
                     choices  = exps,
                     selected = if (length(exps)) exps[1] else NULL,
                     width = "100%")
    })

    # ---- analyte grids ----
    make_analyte_output <- function(type, out_id) {
      output[[out_id]] <- rhandsontable::renderRHandsontable({
        refresh(); req(scope_ok())
        df <- .ann_build_analyte_grid(pool, project_id(), study(), exp(), type)
        if (!nrow(df)) df <- data.frame(analyte = character(),
          source = character(), virus_bacterial_strain = character(),
          catalog_number = character(), batch_lot = character(),
          reagent_acq_date = character(), family = character(),
          stringsAsFactors = FALSE)
        ht <- rhandsontable::rhandsontable(df, rowHeaders = NULL, stretchH = "all")
        rhandsontable::hot_col(ht, "analyte", readOnly = TRUE)
      })
    }
    make_analyte_output("antigen", "hot_antigen")
    make_analyte_output("feature", "hot_feature")

    save_analyte <- function(type, in_id) {
      req(scope_ok(), input[[in_id]])
      df <- rhandsontable::hot_to_r(input[[in_id]])
      n <- 0L
      for (i in seq_len(nrow(df))) {
        set_analyte_annotation(
          pool, project_id(), study(), exp(), type, df$analyte[i],
          source = df$source[i], virus_bacterial_strain = df$virus_bacterial_strain[i],
          catalog_number = df$catalog_number[i], batch_lot = df$batch_lot[i],
          reagent_acq_date = df$reagent_acq_date[i], family = df$family[i],
          user = user())
        n <- n + 1L
      }
      showNotification(sprintf("Saved %d %s annotation(s).", n, type),
                       type = "message", duration = 4)
      bump()
    }
    observeEvent(input$save_antigen, save_analyte("antigen", "hot_antigen"))
    observeEvent(input$save_feature, save_analyte("feature", "hot_feature"))

    # ---- level grids + referent selectors ----
    make_level_output <- function(variable, out_id, ref_id) {
      output[[out_id]] <- rhandsontable::renderRHandsontable({
        refresh(); req(scope_ok())
        df <- .ann_build_level_grid(pool, project_id(), study(), exp(), variable)
        if (!nrow(df)) df <- data.frame(level = character(),
                                        description = character(), stringsAsFactors = FALSE)
        ht <- rhandsontable::rhandsontable(df, rowHeaders = NULL, stretchH = "all")
        rhandsontable::hot_col(ht, "level", readOnly = TRUE)
      })
      observe({
        refresh(); req(scope_ok())
        univ <- tryCatch(list_universe(pool, project_id(), study(), exp(), variable),
                         error = function(e) character())
        cur  <- .ann_current_referent(pool, project_id(), study(), exp(), variable)
        updateSelectInput(session, ref_id, choices = c("(none)" = "", stats::setNames(univ, univ)),
                          selected = cur)
      })
    }
    make_level_output("timeperiod", "hot_timeperiod", "ref_timeperiod")
    make_level_output("agroup",     "hot_agroup",     "ref_agroup")

    save_level <- function(variable, in_id, ref_id) {
      req(scope_ok(), input[[in_id]])
      df <- rhandsontable::hot_to_r(input[[in_id]])
      for (i in seq_len(nrow(df)))
        set_level_annotation(pool, project_id(), study(), exp(), variable,
                             df$level[i], description = df$description[i], user = user())
      ref <- input[[ref_id]] %||% ""
      set_referent(pool, project_id(), study(), exp(), variable,
                   level = if (nzchar(ref)) ref else NA, user = user())
      showNotification(sprintf("Saved %s levels.", variable), type = "message", duration = 4)
      bump()
    }
    observeEvent(input$save_timeperiod, save_level("timeperiod", "hot_timeperiod", "ref_timeperiod"))
    observeEvent(input$save_agroup,     save_level("agroup",     "hot_agroup",     "ref_agroup"))

    # ---- display order (orderInput) ----
    observe({
      refresh(); req(scope_ok())
      for (dim in c("antigen", "feature", "timeperiod", "agroup")) {
        items <- tryCatch(get_ordered_universe(pool, project_id(), study(), exp(), dim),
                          error = function(e) character())
        shinyjqui::updateOrderInput(session, paste0("order_", dim), items = items)
      }
    })
    observeEvent(input$save_order, {
      req(scope_ok())
      for (dim in c("antigen", "feature", "timeperiod", "agroup")) {
        ord <- input[[paste0("order_", dim)]]
        if (!is.null(ord) && length(ord))
          set_order(pool, project_id(), study(), exp(), dim, ord, user = user())
      }
      showNotification("Saved display order.", type = "message", duration = 4)
      bump()
    })
  })
}
