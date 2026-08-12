# =============================================================================
# assay_import_mount.R  —  11.10 Assay Import Refactor, Phase 3 (+ scope control)
# -----------------------------------------------------------------------------
# Mounts the three assay-import module instances behind the import tab, AND
# restores the shared "Experiment name" control that used to live in the retired
# import_lumifile.R readxMapData UI. That control (a create-enabled selectize,
# id "readxMap_experiment_accession_import") sets the experiment scope for every
# assay tab; the study selector (readxMap_study_accession) still lives in
# ui_handler.R. A study-change observer repopulates the experiment choices.
#
# app.R calls this once in the server body:
#   mount_assay_import(input, output, session, db_pool, scope,
#                      reactive_df_study_exp, experiment_choices_rv)
#   where scope = reactive(list(project_id=, study=, experiment=, user=)).
#
# Source AFTER: assay_import_contract/backend/module + readers + descriptors.
# =============================================================================

mount_assay_import <- function(input, output, session, pool, scope,
                               study_exp, experiment_choices_rv) {
  descriptors <- list(
    bead  = descriptor_bead,
    elisa = descriptor_elisa,
    flow  = descriptor_flow
  )

  exp_ontype <- I("function(str) { if (str.length > 15) { this.setTextboxValue(str.substring(0, 15)); } }")

  output$readxMapData <- renderUI({
    init_choices <- isolate(experiment_choices_rv())
    if (is.null(init_choices) || !length(init_choices))
      init_choices <- c("Click OR Create New" = "Click here")

    tagList(
      wellPanel(
        tags$label(
          `for` = "readxMap_experiment_accession_import",
          style = "display:block;",
          "Experiment name",
          tags$br(),
          tags$small(style = "font-weight:normal;",
                     "Choose an existing experiment, or type a new one (up to 15 characters).")
        ),
        selectizeInput(
          "readxMap_experiment_accession_import",
          label    = NULL,
          choices  = init_choices,
          selected = "Click here",
          multiple = FALSE,
          options  = list(create = TRUE, onType = exp_ontype),
          width    = "100%"
        )
      ),
      do.call(tabsetPanel, c(
        list(id = "assay_import_tabs"),
        lapply(names(descriptors), function(k)
          tabPanel(descriptors[[k]]$label,
                   assay_import_ui(paste0("ai_", k), descriptors[[k]])))
      ))
    )
  })

  # Repopulate experiment choices when the study changes (ported from the
  # retired import_lumifile.R observer). updateSelectizeInput refreshes the
  # control in place so the tabset UI is not rebuilt (module state is preserved).
  observeEvent(input$readxMap_study_accession, {
    req(input$readxMap_study_accession)
    if (identical(input$readxMap_study_accession, "Click here")) return()
    se <- tryCatch(study_exp(), error = function(e) NULL)
    expvec <- character(0)
    if (!is.null(se) && all(c("study_accession", "experiment_accession") %in% names(se))) {
      filt <- se[se$study_accession == input$readxMap_study_accession, , drop = FALSE]
      if (nrow(filt) > 0) {
        nm <- if ("experiment_name" %in% names(filt)) filt$experiment_name else filt$experiment_accession
        expvec <- stats::setNames(filt$experiment_accession, nm)
      }
    }
    choices <- c("Click OR Create New" = "Click here", expvec)
    experiment_choices_rv(choices)
    updateSelectizeInput(session, "readxMap_experiment_accession_import",
                         choices = choices, selected = "Click here",
                         options = list(create = TRUE, onType = exp_ontype))
  }, ignoreNULL = TRUE)

  lapply(names(descriptors), function(k)
    assay_import_server(paste0("ai_", k), pool, descriptors[[k]], scope))

  invisible(NULL)
}
