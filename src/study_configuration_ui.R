tags$head(
  tags$style(HTML("
    .datatables {
      width: 100% !important;
      margin: 0 !important;
    }
    .dataTables_wrapper {
      width: 100%;
      margin: 0 auto;
      padding: 0 10px;
    }
    .panel-collapse {
      padding: 15px;
    }

  "))
)

# antigen family table editable

render_study_parameters <- reactive({
  selected_study    <- input$readxMap_study_accession
  main_tab_selected <- input$main_tabs

  if (!is.null(selected_study) && selected_study != "Click here") {
    output$studyParameters_UI <- renderUI({
      conditionalPanel(
        condition = "input.readxMap_study_accession != 'Click here'",
        tagList(
          HTML(paste0("<h3>Change ", selected_study,
                      " study settings for ", currentuser(), "</h3>")),
          tabsetPanel(
            id = "study_params_section_tab",
            tabPanel("Calibration Settings", settingsCascadeUI("settings")),
            tabPanel("Annotations",          annotation_ui("annotations")),
            tabPanel("Source Names",         uiOutput("source_alias_ui")),
            tabPanel("Export/Import",        settingsExportImportUI("settings_io")),
            tabPanel("Delete Components",    uiOutput("delete_study_components_ui")),
            tabPanel("Clone Study",          uiOutput("clone_study_components_ui"))
          )
        )
      )
    })
  } else {
    output$studyParameters_UI <- renderUI({
      if (!is.null(main_tab_selected) &&
          main_tab_selected != "home_page" && main_tab_selected != "manage_project_tab") {
        tagList(conditionalPanel(
          condition = "input.readxMap_study_accession == 'Click here'",
          HTML("<h3>Choose or create a study to change study settings.</h3>")))
      }
    })
  }
}) # end render



observe({
  req(input$main_tabs == "study_settings")
 # req(study_level_tabs == "Study Parameters")
  req(input$readxMap_study_accession)
  req(currentuser())
  # capture reactive inputs *outside* later callback
  study_accession <- isolate(input$readxMap_study_accession)
  user <- isolate(currentuser())

  # start async polling
  check_and_render_study_parameters(study_accession, user)

#  Pull actual antigens once rendered again
#   query <- paste0("SELECT DISTINCT antigen FROM madi_results.xmap_sample
#                 WHERE study_accession = '", study_accession, "'")
#
#   sample_df  <- dbGetQuery(conn, query)
#   current_antigens <- unique(sample_df$antigen)
#   # from database
#   study_config <- fetch_study_configuration(db_pool, study = study_accession, project_id = userWorkSpaceID())
#   antigen_order_params <- strsplit(study_config[study_config$param_name == "antigen_order",]$param_character_value, ",")[[1]]
#   # a_order <<- isolate(input$antigen_order)
#   if (!(all(sort(antigen_order_params) == sort(current_antigens)))) {
#       updateOrderInput(session = session,
#                  "antigen_order",
#                  label = "Antigen Order:",
#                  items = current_antigens)
# }


})

study_params_ready <- reactiveVal(FALSE)

# check if ready on database side
observeEvent(study_params_ready(), {
 # req(input$main_tabs == "view_files_tab")
  #req(input$study_level_tabs == "Study Parameters")
  if (study_params_ready()) {
    render_study_parameters()  # safe here, reactive context
    study_params_ready(FALSE)  # reset flag if needed
  }
})
