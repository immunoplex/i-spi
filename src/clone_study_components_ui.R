# =============================================================================
# clone_study_components_ui.R
# -----------------------------------------------------------------------------
# UI for the "Clone Study" tab (server-side renderUI, matching the app's *_ui.R
# convention; sourced local=TRUE). Placed by ui_handler.R via
# uiOutput("clone_study_components_ui"). Server logic + clone/count functions
# live in clone_study_components.R.
#
# Source study = the sidebar selection (input$readxMap_study_accession); the user
# enters the TARGET project id and study name. Two-step confirm: Preview -> Clone.
# =============================================================================

output$clone_study_components_ui <- renderUI({
  study <- input$readxMap_study_accession
  proj  <- tryCatch(userWorkSpaceID(), error = function(e) NA)

  if (is.null(study) || identical(study, "Click here") || is.na(study)) {
    return(div(class = "well",
      h4("Choose a study first"),
      p("Select a source study in the sidebar, then return here to clone it.")))
  }

  tagList(
    h3(sprintf("Clone Study \u2014 source: %s", study)),
    div(class = "alert alert-info",
      strong("Shallow clone."), br(),
      "Copies the raw plate data, the curve registry, this study\u2019s settings",
      " overrides, and its descriptive annotations (analyte / level / order) into a",
      " NEW project + study. Fitted results are NOT copied \u2014 re-run Compute-fits on",
      " the clone. New curve_ids and multiplate-group ids are generated automatically."),
    fluidRow(
      column(4, numericInput("clone_target_project", "Target project id (16\u201332767)",
                             value = if (is.na(proj)) NA_integer_ else as.integer(proj),
                             min = 16, max = 32767, step = 1)),
      column(4, textInput("clone_target_study", "Target study name (<= 15 chars)", value = "")),
      column(4, br(),
             actionButton("clone_preview", tagList(icon("search"), "Preview what will be cloned")))
    ),
    tags$hr(),
    h4("Rows that would be cloned"),
    DT::dataTableOutput("clone_blast"),
    uiOutput("clone_confirm_ui")
  )
})
