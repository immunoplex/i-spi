# =============================================================================
# delete_study_components_ui.R
# -----------------------------------------------------------------------------
# UI for the "Delete Study Components" tab (server-side renderUI, matching the
# app's *_ui.R convention; sourced local=TRUE into the server). Placed by
# ui_handler.R via uiOutput("delete_study_components_ui"). Server logic +
# delete/count functions live in delete_study_components.R.
#
# Study comes from the sidebar selection (input$readxMap_study_accession); the
# user chooses the scope: whole study, or a single experiment. Deletion is a
# two-step confirm: Preview (dry-run counts) -> Delete permanently -> shinyalert.
# =============================================================================

output$delete_study_components_ui <- renderUI({
  study <- input$readxMap_study_accession
  proj  <- tryCatch(userWorkSpaceID(), error = function(e) NA)

  if (is.null(study) || identical(study, "Click here") || is.na(study)) {
    return(div(class = "well",
      h4("Choose a study first"),
      p("Select a study in the sidebar, then return here to delete its components.")))
  }

  # Experiments available for this study (from the plate registry).
  exps <- tryCatch(
    DBI::dbGetQuery(db_pool, sprintf(
      "SELECT DISTINCT experiment_accession FROM %s
         WHERE project_id IS NOT DISTINCT FROM $1 AND study_accession = $2
         ORDER BY 1", .tbl("xmap_header")),
      params = list(proj, study))$experiment_accession,
    error = function(e) character(0))

  scope_choices <- c("Whole study (all experiments)" = "__ALL__",
                     stats::setNames(exps, exps))

  tagList(
    h3(sprintf("Delete Study Components \u2014 %s", study)),
    div(class = "alert alert-danger",
      strong("Permanent. This cannot be undone."), br(),
      "Deletes raw plate data, the curve registry, fit results, dilution analysis,",
      " antigen-family annotations, descriptive annotations (analyte / level / order),",
      " and settings overrides for the chosen scope.",
      " Plate-level deletion is intentionally not offered."),
    fluidRow(
      column(6, selectInput("dc_scope", "Scope to delete",
                            choices = scope_choices, selected = "__ALL__")),
      column(6, br(),
             actionButton("dc_preview", tagList(icon("search"), "Preview what will be deleted")))
    ),
    tags$hr(),
    h4("Rows that would be deleted"),
    DT::dataTableOutput("dc_blast"),
    uiOutput("dc_confirm_ui")
  )
})
