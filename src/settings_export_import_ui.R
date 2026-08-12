# =============================================================================
# settings_export_import_ui.R
# -----------------------------------------------------------------------------
# UI for the scoped settings-cascade Export/Import tab. Replaces the old xlsx
# "Export/Import Study Parameters" machinery (create_parameter_template /
# write_back_config / update_study_config / update_antigen_family_config).
#
# It dumps / reloads the study's OWN calib_settings override rows -- the sparse
# cascade DATA, not resolved values -- via settings_cascade_access.R
# (export_settings_scoped / import_settings_scoped). Server: settings_export_import.R.
# =============================================================================

settingsExportImportUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "help-block", style = "margin:8px 0;",
      shiny::strong("Export / import this study's settings overrides."),
      shiny::br(),
      shiny::tags$small(paste(
        "Export writes only the values this study overrides (the sparse cascade),",
        "so a reload reproduces them exactly without baking in system defaults.",
        "Import applies them to the CURRENT study."))
    ),
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::h4("Export"),
        shiny::uiOutput(ns("export_summary")),
        shiny::div(
          style = "margin-top:6px;",
          shiny::downloadButton(ns("dl_rdata"), "Download .RData"),
          shiny::downloadButton(ns("dl_json"), "Download JSON"))
      ),
      shiny::column(
        6,
        shiny::h4("Import"),
        shiny::fileInput(ns("upload"),
          "Choose a settings export (.RData or .json)",
          accept = c(".RData", ".rdata", ".Rdata", ".json")),
        shiny::checkboxInput(ns("reset_first"),
          "Replace: clear this study's existing overrides first", value = FALSE),
        shiny::actionButton(ns("apply"), "Apply to this study", class = "btn-primary"),
        shiny::uiOutput(ns("import_status"))
      )
    ),
    shiny::hr(),
    shiny::h4("Preview"),
    DT::dataTableOutput(ns("preview"))
  )
}
