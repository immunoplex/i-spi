# ui handling
reset_import_values <- function() {
  # Reset all reactive values
  isolate({
    # Single values
    upload_state_value$upload_state <- NULL
    rv_value_button$valueButton <- 0

    # Clear reactiveVal objects
    plate_data(NULL)
    unique_plate_types(NULL)
    availableSheets(NULL)
    inFile(NULL)

    # Reset xPonent specific values
    xponent_plate_data(NULL)
    xponent_meta_data(NULL)
    generated_tabs(NULL)
    lumcsv_reactive(NULL)

  })
}

get_user_projects <- function(conn, current_user) {
  query <- glue::glue("
    SELECT pu.project_id, p.project_name, pak.access_key
    FROM madi_lumi_users.project_users pu
    JOIN madi_lumi_users.projects p ON pu.project_id = p.project_id
    JOIN madi_lumi_users.project_access_keys pak ON pu.project_id = pak.project_id
    WHERE pu.user_id = {dbQuoteLiteral(conn, current_user)} AND pu.is_owner = TRUE
  ")
  dbGetQuery(conn, query)
}

get_user_projects_non_owner <- function(conn, current_user) {
  query <- glue::glue("
    SELECT pu.project_id, p.project_name
    FROM madi_lumi_users.project_users pu
    JOIN madi_lumi_users.projects p ON pu.project_id = p.project_id
    JOIN madi_lumi_users.project_access_keys pak ON pu.project_id = pak.project_id
    WHERE pu.user_id = {dbQuoteLiteral(conn, current_user)} AND pu.is_owner = FALSE
  ")
  dbGetQuery(conn, query)
}

getProjectName <- function(conn, current_user){
  query <- glue::glue(" SELECT project_name, workspace_id FROM madi_results.xmap_users pu WHERE pu.auth0_user = {dbQuoteLiteral(conn, current_user)} ")
  result <- dbGetQuery(conn, query)

  if(nrow(result) > 0){
    name <- result[1, "project_name"]
    id <- result[1, "workspace_id"]
  } else {
    name <- "unknown"
    id <- -1
  }

  return(list(name = name, id = id))
}

# returns a data frame for the experiment in the study and project and return ELISA or bead_assy
# used to determine if it is a bead based assay or not. 
get_exp_assay_type <- function(conn, project_id, study_accession, experiment_accession) {
  query <- glue::glue("SELECT
    project_id,
    study_accession,
    experiment_accession,
    CASE
        WHEN LOWER(TRIM(assay_response_variable)) = 'absorbance' THEN 'ELISA'
        ELSE 'bead_assay'
    END AS assay_type
FROM madi_results.xmap_header
WHERE project_id = {project_id}
  AND study_accession      = '{study_accession}'
  AND experiment_accession = '{experiment_accession}'
GROUP BY
    project_id,
    study_accession,
    experiment_accession,
    assay_type
LIMIT 1;")
  
  result <- dbGetQuery(conn, query)
  return(result)
}

info_context_box <- function(txt) {
  tagList(
    div(
      style = paste0(
        "background-color:#f0f8ff; ",      # very light blue background
        "border:1px solid #4a90e2; ",      # medium‑blue border
        "padding:10px; ",
        "margin-bottom:15px; ",
        "border-radius:5px; "
      ),
      # ---- Heading ----
      tags$h4(
        "Current Bead Count Context",
        style = "margin-top:0; color:#2c5aa0;"
      ),
      # ---- Body text (passed in) ----
      tags$p(
        HTML(txt),
        style = "margin:0; color:#333333; line-height:1.4;"
      )
    )
  )
}

output$project_info <- renderUI({
  tagList(
    div(
      style = paste0(
        "background-color: #666666; padding: 15px; border-radius: 8px; ",
        "margin: 0 auto 20px auto; border: 2px solid #2c3e50; ",
        "width: 90%; max-width: 400px; box-sizing: border-box;"),
  p(
    strong("Project: ", style = "color: white;"),
    span(userProjectName(), style = "color: white;"), br(),
    strong("Project ID: ", style = "color: white;"),
    span(userWorkSpaceID(), style = "color: white;")
  )
  )
  )
})

output$main_study_selector <- renderUI({
  req(study_choices_rv())

  tagList(
    div(
      style = "width: 250px;",
      tags$label(
        `for` = "readxMap_study_accession",
        style = "display: block; padding-left: 15px;",
        "Select Study Name",
        tags$br(),
        tags$small(style = "font-weight: normal;",
                   "To create a new study, type in this box (up to 15 characters)."
        )
      ),

      selectizeInput(
        "readxMap_study_accession",
        label = NULL,
        choices = study_choices_rv(),
        selected = "Click here",
        multiple = FALSE,
        options = list(create = TRUE,
                       onType = I("
      function(str) {
        if (str.length > 15) {
          this.setTextboxValue(str.substring(0, 15));
        }
      }
    ")),
        width = "100%"
      )
    )
  )
})

output$study_sidebar <- renderUI({
  val <- input$readxMap_study_accession

  # treat NULL, empty string, or explicit "Click here" as "no selection"
  enabled <- !is.null(val) && nzchar(trimws(val)) && val != "Click here"

  if (enabled) {
    # enabled menu — id must be 'study_tabs' so clicks update input$study_tabs
    sidebarMenu(
      id = "study_tabs",
      menuItem("Import Plate Data", tabName = "import_tab", icon = icon("file")),
      menuItem("View, Process, and Export Data", tabName = "view_files_tab", icon = icon("dashboard")),
      menuItem("Change Study Settings", tabName = "study_settings", icon = icon("cog")),
      menuItem("Delete Study Data", tabName = "delete_study", icon = icon("delete-left"))
    )
  } else {
    # disabled visual: mimic menu appearance but not interactive
    # You can customize CSS further to grey them out; this is a simple, accessible version.
    tags$div(style = "padding: 10px; color: #888;",
             tags$strong("Study options are disabled"),
             tags$p("Select or create a study to enable these actions."),
             tags$ul(class = "sidebar-menu",
                     tags$li(class = "treeview disabled",
                             tags$a(href = "#", icon("file"), "Import Plate Data")
                     ),
                     tags$li(class = "treeview disabled",
                             tags$a(href = "#", icon("dashboard"), "View, Process, and Export Data")
                     ),
                     tags$li(class = "treeview disabled",
                             tags$a(href = "#", icon("cog"), "Change Study Settings")
                     ),
                     tags$li(class = "treeview disabled",
                             tags$a(href = "#", icon("delete-left"), "Delete Study Data")
                     )
             )
    )
  }
})

# When study is enabled, explicitly clear any selection among study_tabs so that none are selected.
# We use a sentinel string "study_none" that does not correspond to any tabName.
observeEvent(input$readxMap_study_accession, {
  # Reset experiment FIRST before anything else with priorty helps app not crash when study is changed and experiment is a value
  updateSelectInput(session, "readxMap_experiment_accession",
                    choices = c("Click here" = "Click here"),
                    selected = "Click here"
  )
  
  val <- input$readxMap_study_accession
  enabled <- !is.null(val) && nzchar(trimws(val)) && val != "Click here"

  if (enabled) {
    # set selected to a non-existing tabName so none of the items appear selected.
    # This avoids forcing any of the study tabs to display immediately.
    updateTabItems(session, "study_tabs", selected = "study_none")
  } else {
    # when disabled, also clear selection (optional)
    updateTabItems(session, "study_tabs", selected = "study_none")
  }

}, ignoreNULL = FALSE, priority = 10)

output$landing_page_ui <- renderUI({
  cat(input$main_tabs)
  if (is.null(input$main_tabs) || input$main_tabs == "home_page") {   # nothing selected yet
    fluidRow(
      div(style = "padding-left: 50px; padding-right: 50px;",
      tagList(
       # img(src = "I_SPI_logo.png"),
        img(src = "ispi_new.png",
            style = "max-width:100%; height:auto;"), # 40% old logo
        br(),
        br(),
        p(strong("Overview")),
        p("The Interactive Serology Plate Inspector, also known as I-SPI, is an open-source web application designed to streamline quality control
        (QC) and quality assurance (QA) for multiplex immunoassays.
          It provides a unified workflow that supports both automation and user decision making while remaining grounded in
          objective statistical algorithms."),
        p(strong("Quick Start Guide")),
        tags$ol(
          tags$li(
            tags$p("Create or Load a Project"),
            tags$p("To get started, the first step is to create a new project or load an existing project.
            To do this, click Create, Add, and Load Projects in the sidebar.
            Project access keys allowing  you to share projects with others can be found here.")
          ),
          tags$li(
            tags$p("Create or Load a Study"),
            tags$p("Select an existing study or create a new study by typing a new name into the study field in the sidebar.")
          ),
          tags$li(
            tags$p("Import Plate Data"),
            tags$p("To import your data into a study, click Import Plate Data in the sidebar. Plate data can be imported into I-SPI once a study is selected.
                   For more detailed information, guidance, and tips on importing data please visit our ",tags$a(href = "I_SPI_data_upload_guide.pdf","guide on importing data.", target = "_blank"))
          ),
          tags$li(
            tags$p("Change Study Settings"),
            tags$p("If you need to change a study’s settings, click Change Study Settings in the sidebar.")
          ),
          tags$li(
            tags$p("Delete Study Data"),
            tags$p("If you need to remove all ofa study's data, click Delete Study Data in the sidebar.")
          ),
          tags$li(
            tags$p("Conduct Analyses"),
            tags$p("To conduct QA and QC analyses on your study data, click View, Process, and Export Data in the sidebar.")
          )
        ),
        p("The organization of projects, studies and experiments and how data and QC/QA results can be shared is outlined in the figure below."),
        img(src = "research_ISPI_organization_revised.png", style = "max-width: 80%;"),
        br(),
        br(),
        HTML(
          'For more detailed documentation on I-SPI please visit
   <a href="https://immunoplex.org/" target="_blank">Immunoplex</a>.
   All the Immunoplex tools are available through
   <a href="https://github.com/immunoplex/deployment" target="_blank">GitHub</a>.
          To download the source code for I-SPI please visit the <a href = "https://github.com/immunoplex/i-spi", target="_blank"> I-SPI repository</a>.
          We welcome feedback which can be given on the <a href = "https://github.com/immunoplex/i-spi/issues", target ="_blank"> issues page </a> of our repository for I-SPI.'
        ),
        br(),
        br(),
        p(strong("Citing I-SPI")),
        p("Citation information is coming soon.")
      )
    )
    )
  }
})

output$body_tabs <- renderUI({
  current_count <- tab_counter()

  dynamic_tabs <- lapply(seq_len(current_count), function(i) {
    tabItem(
      tabName = paste0("dynamic_tab_", i),
      uiOutput(paste0("dynamic_content_", i))  # placeholder for dynamic content
    )
  })

  do.call(tabItems, c(
    list(
      tabItem(tabName = "view_files_tab", uiOutput("view_stored_experiments_ui")),
      tabItem(tabName = "study_settings", uiOutput("studyParameters_UI")),
      tabItem(tabName = "import_tab", uiOutput("readxMapData")),
      tabItem(tabName = "manage_project_tab", uiOutput("manage_project_ui")),
      tabItem(tabName = "delete_study", uiOutput("delete_study_ui"))
    )
   # dynamic_tabs  # append dynamic tabs here
  ))

})

output$load_ui <- renderUI({
  current_user <- currentuser()
  tabRefreshCounter()$manage_project_tab
  fluidPage(
    tagList(
      #h3("You are in workspace/project:", userProjectName(), "ID: ", userWorkSpaceID() ),
      # br(),
      bsCollapse(
        id = "loadProjectCollapse",
        bsCollapsePanel(
          title = " Load Existing Project Documentation",
          style = "success",
          tagList(
            tags$p("To load a project you own, in the table labeled 'Projects you own: (your username)' select a project
                   and press the button labed 'Load Selected Project' under the table."),
            tags$p("To load a project you have access to, in the table labeled 'Projects you have access to: (your username)' select a project
                   and press the button labed 'Load Selected Project' under the table.")
          )
        )
      ),
      h3(glue::glue("Projects you own: {current_user}")),
      DT::dataTableOutput("userProjectsTable"),
      actionButton("execute_project_button", "Load Selected Project"),
      br(),
      h3(glue::glue("Projects you have access to: {current_user}")),
      DT::dataTableOutput("userProjectsTableNonOwner"),
      actionButton("execute_project_button1", "Load Selected Project")
    )
  )
})

observeEvent(input$execute_project_button, {
  selected_project_id <- input$userProjectsTable_rows_selected
  if (length(selected_project_id) == 1) {
    # Retrieve the project ID of the first selected row
    selected_project_id <- selected_project_id[1]
    # Perform action based on the selected project ID
    user_projects <- get_user_projects(conn, currentuser())
    project_id <- user_projects[selected_project_id, "project_id"]
    # Call your function to execute the project
    load_project(conn, project_id, currentuser())
    current_project_details <- getProjectName(conn, currentuser())
    userWorkSpaceID(current_project_details$id)
    userProjectName(current_project_details$name)
  } else {
    # Notify the user if no project or multiple projects are selected
    showModal(modalDialog(
      title = "Error",
      "Please select one project to execute.",
      easyClose = TRUE
    ))
  }
})

observeEvent(input$execute_project_button1, {
  selected_project_id <- input$userProjectsTableNonOwner_rows_selected
  if (length(selected_project_id) == 1) {
    # Retrieve the project ID of the first selected row
    selected_project_id <- selected_project_id[1]
    # Perform action based on the selected project ID
    user_projects_non_owner <- get_user_projects_non_owner(conn, currentuser())
    project_id <- user_projects_non_owner[selected_project_id, "project_id"]
    # Call your function to execute the project
    load_project(conn, project_id, currentuser())
    current_project_details <- getProjectName(conn, currentuser())
    userWorkSpaceID(current_project_details$id)
    userProjectName(current_project_details$name)
  } else {
    # Notify the user if no project or multiple projects are selected
    showModal(modalDialog(
      title = "Error",
      "Please select one project to execute.",
      easyClose = TRUE
    ))
  }
})

output$userProjectsTable <- DT::renderDataTable({
  tabRefreshCounter()$manage_project_tab
  user_projects <- get_user_projects(conn, currentuser())
  DT::datatable(user_projects, options = list(pageLength = 5))
})

output$userProjectsTableNonOwner <- DT::renderDataTable({
  tabRefreshCounter()$manage_project_tab
  user_projects_non_owner <- get_user_projects_non_owner(conn, currentuser())
  DT::datatable(user_projects_non_owner, options = list(pageLength = 5))
})

output$view_stored_experiments_ui <- renderUI({
  if (input$main_tabs != "home_page" & input$main_tabs != "manage_project_tab" & input$study_tabs == "view_files_tab") {
    if (input$readxMap_study_accession != "Click here") {

 # tabRefreshCounter()$view_files_tab

  req(reactive_df_study_exp())
  # Get data
  df <- reactive_df_study_exp()

 # if (!is.null(input$readxMap_study_accession)) {
    df_filtered <- df[df$study_accession == input$readxMap_study_accession, ]
    experiment_choices <- c(
      "Click here" = "Click here",
      setNames(df_filtered$experiment_accession, df_filtered$experiment_name)
    )
  #}

# if (input$readxMap_study_accession != "Click here") {
   stored_plate_title <- paste("View, Process, and Export", input$readxMap_study_accession, "Data", sep = " ")
   

  tagList(
    fluidPage(
      h3(stored_plate_title),

      # Study Level Content
        tabsetPanel(
          id = "study_level_tabs",
          # Experiment Level Tab
          tabPanel("Experiments",
                   fluidRow(
                    # column(6,
                            selectInput("readxMap_experiment_accession",
                                        "Choose Experiment Name",
                                        choices = experiment_choices,
                                        # choices = c("Click here" = "Click here",
                                        #             setNames(df$experiment_accession,
                                        #                      df()$experiment_name)),
                                        selected = "Click here",
                                        multiple = FALSE
                            ),
                            actionButton(
                              inputId = "refresh_experiments_button",
                              label = "Refresh Experiments",
                              icon = icon("refresh")
                            ),
                    # ),
                     #column(6,
                     conditionalPanel(
                       condition = "input.readxMap_experiment_accession != 'Click here' &&
                       input.study_level_tabs == 'Experiments' && input.study_tabs== 'view_files_tab'",

                      tabsetPanel(
                        id = "basic_advance_tabs",
                        tabPanel(
                         # id = "data_view",
                          title = "Data",
                          # conditionalPanel(
                          #   condition = "input.basic_advance_tabs == 'data_view'",
                            uiOutput("dynamic_data_ui")
                          #)
                        ),
                        tabPanel(
                          id = "basic_qc",
                          title = "Quality Control - Basic",
                          radioGroupButtons(
                            inputId = "qc_component",
                            label = "",
                            choices = c("Bead Count", "Standard Curve",  "Standard Curve Summary"),
                            selected = character(0)
                          ),
                          conditionalPanel(
                            condition = "input.qc_component == 'Bead Count' && output.exp_assay_type_js == 'bead_assay'",
                            uiOutput("bead_count_available_ui"),
                            uiOutput("bead_count_module_ui")
                          ),
                          conditionalPanel(
                            condition = "input.qc_component == 'Bead Count' && output.exp_assay_type_js == 'ELISA'",
                            uiOutput("bead_not_available_ui")
                          ),
                          
                          
                          # conditionalPanel(
                          #   condition = "input.qc_component == 'Standard Curve'",
                          #  uiOutput("sc_fit_module_ui")
                          # ),
                          conditionalPanel(
                            condition = "input.qc_component == 'Standard Curve'",
                            uiOutput("std_curver_ui")
                          ),

                          # conditionalPanel(
                          #   condition = "input.qc_component == 'Standard Curve Summary'",
                          #   uiOutput("standardCurveSummaryUI")
                          # ),
                          conditionalPanel(
                            condition = "input.qc_component == 'Standard Curve Summary'",
                            uiOutput("std_curver_summary_ui")
                          )
                        ),

                        tabPanel(
                          id = "advance_qc",
                          title = "Advanced Diagnostics",
                          radioGroupButtons(
                            inputId = "advanced_qc_component",
                            label = "Advanced QC Phase",
                            choices = c("Dilution Analysis", "Dilutional Linearity",
                                        "Outliers", "Subgroup Detection", "Subgroup Detection Summary"),
                            selected = character(0)
                          ),

                          # Conditional panels for advanced QC only
                          conditionalPanel(
                            condition = "input.advanced_qc_component == 'Dilution Analysis'",
                           uiOutput("dilutionAnalysisUI")
                          ),
                          conditionalPanel(
                            condition = "input.advanced_qc_component == 'Dilutional Linearity'",
                           uiOutput("dilutional_linearity_mod_ui")
                          ),
                          conditionalPanel(
                            condition = "input.advanced_qc_component == 'Outliers'",
                             uiOutput("outlierTab")
                          ),
                          conditionalPanel(
                            condition = "input.advanced_qc_component == 'Subgroup Detection'",
                            uiOutput("subgroupDetectionUI")
                          ),
                          conditionalPanel(
                            condition = "input.advanced_qc_component == 'Subgroup Detection Summary'",
                            uiOutput("subgroup_summary_UI")
                          )
                        )
                      ),
                    )
                   #)
                   ),
          ),
          # Study Overview Tab
          tabPanel("Study Overview",
                   id = "study_overview_tab",
                   uiOutput("study_overview_page")
          ),# end TabsetPanel
        #) # end study level tabs
      ) # end  Study Level Content
      ) # end fluidPage
    ) #end tagList
    } # end test for click here
    else {
      stored_plate_title <- paste("Choose or create a study to View, Process, and Export Data")
    }
  } # end outer if for nav
})

# Data Contents
output$dynamic_data_ui <- renderUI({
  req(input$basic_advance_tabs)
  if (
    input$basic_advance_tabs == "Data" &&
    input$readxMap_study_accession != "Click here" &&
    input$readxMap_experiment_accession != "Click here" &&
    input$study_level_tabs == "Experiments" &&
    input$main_tabs == "view_files_tab"
  ) {
    tagList(
      actionButton(
        inputId = "refresh_data_button",
        label = "Refresh Data",
        icon = icon("refresh")
      ),
      downloadButton(
        outputId = "download_rdata_bundle",
        label = "R Data Bundle",
        icon = icon("download")
      ) |> tagAppendAttributes(
        title = "Downloads an RData file containing plates, standards, blanks, controls, samples, and sample QC dataframes."
      ),
    tabsetPanel(
      id = "dataCollapse",
      tabPanel(
        title = "Plates",
        DT::dataTableOutput("stored_header"),
        downloadButton("download_stored_header"),
        uiOutput("header_actions"),
        uiOutput("split_plate_nominal_UI"),
        uiOutput("wavelength_subtraction_UI")
       # uiOutput("split_button_ui")
      ),
      tabPanel(
        title = "Standards",
        DT::dataTableOutput("swide_standard"),
        downloadButton("download_stored_standard")
      ),
      tabPanel(
        title = "Controls",
        DT::dataTableOutput("swide_control"),
        downloadButton("download_stored_control")
      ),
      tabPanel(
        title = "Blanks",
        DT::dataTableOutput("swide_buffer"),
        downloadButton("download_stored_buffer")
      ),
      tabPanel(
        title = "Samples",
        DT::dataTableOutput("swide_sample"),
        downloadButton("download_stored_sample")
      ),
      tabPanel(
        title = "Samples QC",
        DT::dataTableOutput("stored_best_sample_se"),
        downloadButton("download_stored_best_sample_se")

      )
    )
    )
  } else {
    NULL  # Removes the bsCollapse completely
  }
})


exp_assay_info <- reactive({
  req(input$readxMap_study_accession,
      input$readxMap_experiment_accession,
      userWorkSpaceID())
  
  # Query the DB – returns a data.frame with column "assay_type"
  res <- get_exp_assay_type(
    conn                 = conn,
    project_id           = userWorkSpaceID(),
    study_accession      = input$readxMap_study_accession,
    experiment_accession = input$readxMap_experiment_accession
  )
  assay_type <- if (nrow(res) == 0) NA_character_ else res$assay_type[1]
  
  list(
    assay_type          = assay_type,
    study_accession     = input$readxMap_study_accession,
    experiment_accession= input$readxMap_experiment_accession
  )
})

output$exp_assay_type_js <- renderText({
  info <- exp_assay_info()
  # Default to bead_assay (the most common case) while the query runs
  if (is.null(info$assay_type) || is.na(info$assay_type)) {
    "bead_assay"
  } else {
    info$assay_type
  }
  
})

output$bead_not_available_ui <- renderUI({
  info <- exp_assay_info()
  # Show the box only for ELISA assays
  if (info$assay_type != "ELISA") return(NULL)
  
  txt <- sprintf(
    "Current Study: <strong>%s</strong><br>
     Current Experiment: <strong>%s</strong><br>
     Assay Type: <strong>ELISA</strong> (absorbance‑based).<br>
     Because the assay is absorbance‑based and not bead‑based,
     bead‑count analysis is not available for this experiment.",
    info$study_accession,
    info$experiment_accession
  )
  info_context_box(txt)
})

output$bead_count_available_ui <- renderUI({
  info <- exp_assay_info()
  # Show the box only for bead assays assays
  if (info$assay_type != "bead_assay") return(NULL)
  
  txt <- sprintf(
    "Current Study: <strong>%s</strong><br>
     Current Experiment: <strong>%s</strong><br>
     Assay Type: <strong>Bead Array</strong><br>
     Bead count analysis is avaliable.",
    info$study_accession,
    info$experiment_accession
  )
  info_context_box(txt)
})


  
  
# Keep it alive even when the UI element that uses it is hidden
outputOptions(output, "exp_assay_type_js", suspendWhenHidden = FALSE)


optimization_parsed_boolean <- reactive({
  optimization_refresh() # refresh dependency
  is_optimization_experiment_parsed(input$readxMap_study_accession, input$readxMap_experiment_accession_import, input$read_import_plate_id, input$read_import_plate_number)
})

output$split_button_ui <- renderUI({
  if (!optimization_parsed_boolean()) {
    actionButton("optimize_plates", "Split Optimization Plates")
  } else {
    NULL
  }
})

# In Plates tab
output$split_plate_nominal_UI <- renderUI({
  req(input$stored_header_rows_selected)
  if ((split_by_nominal_dilution())) {
    actionButton("split_plates_nominal", "Split Plate by Nominal Sample Dilution")
  } else {
    NULL
  }
})

output$wavelength_subtraction_UI <- renderUI({
  req(input$stored_header_rows_selected)
  if ((show_wavelength_subtraction())) {
    actionButton("wavelength_subtraction", "Subtract Wavelengths")
  } else {
    NULL
  }
})

observeEvent(input$split_plates_nominal,{
  cat("split plates by nominal_sample dilution")
  header_row_selected <- stored_plates_data$stored_header[input$stored_header_rows_selected,]

  print(header_row_selected)
  split_plate_nominal_sample_dilution(
    study_accession = header_row_selected$study_accession,
    experiment_accession = header_row_selected$experiment_accession,
    plateid = header_row_selected$plateid,
    conn = conn
  )
})

## wavelength subtraction
observeEvent(input$wavelength_subtraction, {
  cat("starting subtraction\n")
  
  showNotification(id = "subtract_wavelength_notify", 
                   HTML("Subtracting wavelengths<span class='dots'>"), 
                   duration = NULL, type = "message")
  
  header_row_selected <- stored_plates_data$stored_header[input$stored_header_rows_selected, ]
  
  args <- list(
    study_accession         = header_row_selected$study_accession,
    experiment_accession    = header_row_selected$experiment_accession,
    plate                   = header_row_selected$plate,
    nominal_sample_dilution = header_row_selected$nominal_sample_dilution,
    wavelengths             = header_row_selected$wavelengths
  )
  
  std_ctrl_buf_join_keys <- c(
    "project_id", "study_accession", "experiment_accession",
    "well", "sampleid", "antigen", "dilution",
    "feature", "source", "stype",
    "nominal_sample_dilution", "plate"
  )
  
  sample_join_keys <- c(
    "project_id", "study_accession", "experiment_accession",
    "well", "sampleid", "patientid", "timeperiod",
    "antigen", "feature", "stype", "dilution"
  )
  
  delta_standard <- do.call(subtract_wavelength_mfi, c(
    list(df = stored_plates_data$stored_standard, join_keys = std_ctrl_buf_join_keys), args))
  
  delta_control <- do.call(subtract_wavelength_mfi, c(
    list(df = stored_plates_data$stored_control, join_keys = std_ctrl_buf_join_keys), args))
  
  delta_buffer <- do.call(subtract_wavelength_mfi, c(
    list(df = stored_plates_data$stored_buffer, join_keys = std_ctrl_buf_join_keys), args))
  
  
  # delta_standard <- do.call(subtract_wavelength_mfi, c(list(df = stored_plates_data$stored_standard), args))
  # delta_control  <- do.call(subtract_wavelength_mfi, c(list(df = stored_plates_data$stored_control),  args))
  # delta_buffer   <- do.call(subtract_wavelength_mfi, c(list(df = stored_plates_data$stored_buffer),   args))
  
  delta_sample <- do.call(subtract_wavelength_mfi, c(
    list(df = stored_plates_data$stored_sample, join_keys = sample_join_keys),
    args
  ))
  
  # for (tbl in c("xmap_standard", "xmap_control", "xmap_buffer", "xmap_sample", "xmap_header")) {
  #   cat("\n---", tbl, "---\n")
  #   col_sql <- glue::glue("
  #   SELECT column_name, column_default, is_nullable
  #   FROM information_schema.columns
  #   WHERE table_schema = 'madi_results'
  #     AND table_name = '{tbl}'
  #   ORDER BY ordinal_position;
  # ")
  #   print(DBI::dbGetQuery(conn, col_sql))
  # }
  
  
  if (!is.null(delta_standard) && nrow(delta_standard) > 0) {
    showNotification(id = "subtract_wavelength_notify",
                     HTML("Subtracting wavelengths.. saving standards<span class='dots'>"),
                     duration = NULL, type = "message")
    insert_delta_sql(conn, "madi_results", "xmap_standard", delta_standard)
  }

  if (!is.null(delta_control) && nrow(delta_control) > 0) {
    showNotification(id = "subtract_wavelength_notify",
                     HTML("Subtracting wavelengths... saving controls<span class='dots'>"),
                     duration = NULL, type = "message")
    insert_delta_sql(conn, "madi_results", "xmap_control", delta_control)
  }

  if (!is.null(delta_buffer) && nrow(delta_buffer) > 0) {
    showNotification(id = "subtract_wavelength_notify",
                     HTML("Subtracting wavelengths... saving blanks<span class='dots'>"),
                     duration = NULL, type = "message")
    insert_delta_sql(conn, "madi_results", "xmap_buffer", delta_buffer)
  }

  if (!is.null(delta_sample) && nrow(delta_sample) > 0) {
    showNotification(id = "subtract_wavelength_notify",
                     HTML("Subtracting wavelengths... saving samples<span class='dots'>"),
                     duration = NULL, type = "message")
    insert_delta_sql(conn, "madi_results", "xmap_sample", delta_sample)
  }

  # Insert delta header
  delta_header <- header_row_selected
  delta_header$experiment_accession <- paste0(header_row_selected$experiment_accession, "|D")
  delta_header$wavelengths <- "delta"
  insert_delta_sql(conn, "madi_results", "xmap_header", delta_header)

  #show_wavelength_subtraction(FALSE)
  
  # Copy antigen family settings from base experiment to delta experiment
  delta_experiment <- paste0(header_row_selected$experiment_accession, "|D")
  base_experiment  <- header_row_selected$experiment_accession
  
  antigen_family_base <- DBI::dbGetQuery(conn, glue::glue("
    SELECT * FROM madi_results.xmap_antigen_family
    WHERE study_accession      = '{header_row_selected$study_accession}'
      AND experiment_accession = '{base_experiment}'
      AND project_id           = {header_row_selected$project_id}
      AND NOT EXISTS (
        SELECT 1 FROM madi_results.xmap_antigen_family tgt
        WHERE tgt.study_accession      = '{header_row_selected$study_accession}'
          AND tgt.experiment_accession = '{delta_experiment}'
          AND tgt.antigen              = xmap_antigen_family.antigen
          AND tgt.project_id           = {header_row_selected$project_id}
      );
  "))
  
  if (nrow(antigen_family_base) > 0) {
    antigen_family_base$experiment_accession <- delta_experiment
    # remove the pkey 
    antigen_family_base <- antigen_family_base[, !names(antigen_family_base) %in% "xmap_antigen_family_id"]
    insert_delta_sql(conn, "madi_results", "xmap_antigen_family", antigen_family_base)
    cat("antigen family rows copied to delta experiment:", nrow(antigen_family_base), "\n")
  } else {
    cat("no antigen family rows to copy\n")
  }
  
  showNotification(id = "subtract_wavelength_notify",
                   "Wavelength subtraction complete!",
                   duration = NULL, type = "message")
  removeNotification(id = "subtract_wavelength_notify")

  cat("all delta inserts complete\n")
})


# observeEvent(input$wavelength_subtraction, {
#   cat("starting subtraction\n")
#   showNotification(id = "subtract_wavelength_notify", HTML("Subtracting Wavelength for the selected plate<span class = 'dots'>"), duration = NULL)
#   header_row_selected <- stored_plates_data$stored_header[input$stored_header_rows_selected, ]
#   
#   args <- list(
#     study_accession         = header_row_selected$study_accession,
#     experiment_accession    = header_row_selected$experiment_accession,
#     plate                   = header_row_selected$plate,
#     nominal_sample_dilution = header_row_selected$nominal_sample_dilution,
#     wavelengths             = header_row_selected$wavelengths
#   )
#   
#   delta_standard <<- do.call(subtract_wavelength_mfi, c(list(df = stored_plates_data$stored_standard), args))
#   delta_control  <<- do.call(subtract_wavelength_mfi, c(list(df = stored_plates_data$stored_control),  args))
#   delta_buffer   <<-  do.call(subtract_wavelength_mfi, c(list(df = stored_plates_data$stored_buffer),   args))
#   
#   sample_join_keys <- c(
#     "project_id",
#     "study_accession", 
#     "experiment_accession",
#     "well",
#     "sampleid",
#     "patientid",
#     "timeperiod", 
#     "antigen",
#     "feature",
#     "stype",
#     "dilution"
#   )
#   
#   delta_sample <<- do.call(subtract_wavelength_mfi, c(
#     list(df = stored_plates_data$stored_sample, join_keys = sample_join_keys),
#     args
#   ))
#   
#   # # --- Insert delta results to DB ---
#   # cat("inserting delta results to DB\n")
#   # 
#   # if (!is.null(delta_standard) && nrow(delta_standard) > 0) {
#   #   update_db(
#   #     operation  = "insert",
#   #     schema     = "madi_results",
#   #     table_name = "xmap_standard",
#   #     data       = delta_standard
#   #   )
#   #   cat("delta_standard inserted:", nrow(delta_standard), "rows\n")
#   # } else {
#   #   cat("delta_standard empty, skipping insert\n")
#   # }
#   # 
#   # if (!is.null(delta_control) && nrow(delta_control) > 0) {
#   #   update_db(
#   #     operation  = "insert",
#   #     schema     = "madi_results",
#   #     table_name = "xmap_control",
#   #     data       = delta_control
#   #   )
#   #   cat("delta_control inserted:", nrow(delta_control), "rows\n")
#   # } else {
#   #   cat("delta_control empty, skipping insert\n")
#   # }
#   # 
#   # if (!is.null(delta_buffer) && nrow(delta_buffer) > 0) {
#   #   update_db(
#   #     operation  = "insert",
#   #     schema     = "madi_results",
#   #     table_name = "xmap_buffer",
#   #     data       = delta_buffer
#   #   )
#   #   cat("delta_buffer inserted:", nrow(delta_buffer), "rows\n")
#   # } else {
#   #   cat("delta_buffer empty, skipping insert\n")
#   # }
#   # 
#   # if (!is.null(delta_sample) && nrow(delta_sample) > 0) {
#   #   update_db(
#   #     operation  = "insert",
#   #     schema     = "madi_results",
#   #     table_name = "xmap_sample",
#   #     data       = delta_sample
#   #   )
#   #   cat("delta_sample inserted:", nrow(delta_sample), "rows\n")
#   # } else {
#   #   cat("delta_sample empty, skipping insert\n")
#   # }
#   # 
#   cat("all delta inserts complete\n")
# 
#   cat("subtract completed\n")
#   removeNotification("subtract_wavelength_notify")
#   
# })
# observeEvent(input$wavelength_subtraction, {
#   cat("starting subtraction")
#   header_row_selected <- stored_plates_data$stored_header[input$stored_header_rows_selected, ]
#   print(header_row_selected)
#   print(str(stored_plates_data$stored_standard))
#   
#   delta_standard <<- subtract_wavelength_mfi(
#     df                   = stored_plates_data$stored_standard,
#     study_accession      = header_row_selected$study_accession,
#     experiment_accession = header_row_selected$experiment_accession,
#     plate              = header_row_selected$plate,
#     nominal_sample_dilution = header_row_selected$nominal_sample_dilution,
#     wavelengths          = header_row_selected$wavelengths
#   )
#   
#   delta_control <<- subtract_wavelength_mfi(
#     df                   = stored_plates_data$stored_control,
#     study_accession      = header_row_selected$study_accession,
#     experiment_accession = header_row_selected$experiment_accession,
#     plate                 = header_row_selected$plate,
#     nominal_sample_dilution = header_row_selected$nominal_sample_dilution,
#     wavelengths          = header_row_selected$wavelengths
#   )
#   
#   delta_buffer <<- subtract_wavelength_mfi(
#     df                   = stored_plates_data$stored_buffer,
#     study_accession      = header_row_selected$study_accession,
#     experiment_accession = header_row_selected$experiment_accession,
#     plate              = header_row_selected$plate,
#     nominal_sample_dilution = header_row_selected$nominal_sample_dilution,
#     wavelengths          = header_row_selected$wavelengths
#   )
#   
#   delta_sample <<- subtract_wavelength_mfi(
#     df                   = stored_plates_data$stored_sample,
#     study_accession      = header_row_selected$study_accession,
#     experiment_accession = header_row_selected$experiment_accession,
#     plate                 = header_row_selected$plate,
#     nominal_sample_dilution = header_row_selected$nominal_sample_dilution,
#     wavelengths          = header_row_selected$wavelengths
#   )
#   
#   cat("subtract completed")
#   
#   # perform_wavelength_subtraction(
#   #   header_row_selected  = header_row_selected,
#   #   delta_standard       = delta_standard,
#   #   delta_control        = delta_control,
#   #   delta_buffer         = delta_buffer,
#   #   delta_sample         = delta_sample,
#   #   conn                 = conn,
#   #   refresh_data_trigger = refresh_data_trigger
#   # )
# })
# 

observeEvent(input$optimize_plates, {
  split_optimization_plates(study_accession = input$readxMap_study_accession, experiment_accession = input$readxMap_experiment_accession )
})


# ReactiveVal to store experiments
reactive_df_study_exp <- reactiveVal()

# Refresh the list of studies and their experiments
# for viewing, and conducing QC/QA
observeEvent({
  list(
    input$main_tabs,
    input$study_tabs,
    input$readxMap_study_accession,
    refresh_experiment_trigger()
  )
}, {
  # Only run when tab is active
  if (!is.null(input$main_tabs) &&
      !is.null(input$study_tabs) &&
      input$main_tabs != "home_page" &&
      input$main_tabs != "manage_project_tab" &&
      input$study_tabs == "view_files_tab") {

    # guard study_accession
    if (!is.null(input$readxMap_study_accession) &&
        nzchar(input$readxMap_study_accession)) {

      select_query <- glue::glue_sql("
        SELECT DISTINCT
          xmap_header.study_accession,
          xmap_header.experiment_accession,
          xmap_header.study_accession AS study_name,
          xmap_header.experiment_accession AS experiment_name,
          xmap_header.workspace_id,
          xmap_users.project_name
        FROM madi_results.xmap_header
        JOIN madi_results.xmap_users
          ON xmap_header.workspace_id = xmap_users.workspace_id
        WHERE xmap_header.workspace_id = {userWorkSpaceID()}
      ;", .con = conn)

      query_result <- dbGetQuery(conn, select_query)
      reactive_df_study_exp(query_result)
    }
  }
}, ignoreNULL = TRUE, ignoreInit = TRUE)


observeEvent(input$refresh_data_button, {
  refresh_data_trigger(refresh_data_trigger() + 1)
})

observeEvent(input$refresh_experiments_button, {
  refresh_experiment_trigger(refresh_experiment_trigger() + 1)
})

observeEvent(input$basic_advance_tabs, {
  if (input$basic_advance_tabs == "basic_qc") {
    updateRadioGroupButtons(session, "advanced_qc_component", selected = character(0))
  }

})

observeEvent(input$advanced_qc_component, {
  req(input$readxMap_study_accession)
  req(input$readxMap_experiment_accession)

  if (input$advanced_qc_component == "Dilutional Linearity" &&
      input$readxMap_study_accession != "" &&
      input$readxMap_study_accession != "Click here" &&
      input$readxMap_experiment_accession != "" &&
      input$readxMap_experiment_accession != "Click here") {

    # Destroy previous module (if exists)
    prev_dil_lin_id <- paste0("dil_lin_mod_", reload_dil_lin_count)
    try(destroyModule(prev_dil_lin_id), silent = TRUE)

    # Increment counter and build new ID
    reload_dil_lin_count <<- reload_dil_lin_count + 1
    new_dil_lin_id <- paste0("dil_lin_mod_", reload_dil_lin_count)

    # Render UI and load module
    output$dilutional_linearity_mod_ui <- renderUI({
      destroyableDilutionalLinearityModuleUI(new_dil_lin_id)
    })

    destroyableDilutionalLinearityServer(
      id = new_dil_lin_id,
      selected_study = reactive(input$readxMap_study_accession),
      selected_experiment = reactive(input$readxMap_experiment_accession),
      currentuser()
    )

  } else {
    # If switching away, destroy any existing SC a module
    try(destroyModule(paste0("dil_lin_mod_", reload_dil_lin_count)), silent = TRUE)
    output$dilutional_linearity_mod_ui <- renderUI({ NULL })
  }
})

observeEvent(input$qc_component, {
  cat("QC component selected:\n")
  print(input$qc_component)

  req(input$readxMap_study_accession)
  req(input$readxMap_experiment_accession)

 # cat("Current open panel(s):", input$standardCurveCollapse, "\n")
  # ----- Bead Count Module -----
  if (input$qc_component == "Bead Count" &&
      input$readxMap_study_accession != "" &&
      input$readxMap_study_accession != "Click here" &&
      input$readxMap_experiment_accession != "" &&
      input$readxMap_experiment_accession != "Click here") {

    # Destroy previous module (if exists)
    prev_bead_mod_id <- paste0("bead_count_mod_", reload_bead_count)
    try(destroyModule(prev_bead_mod_id), silent = TRUE)

    # Increment counter and build new ID
    reload_bead_count <<- reload_bead_count + 1
    new_bead_mod_id <- paste0("bead_count_mod_", reload_bead_count)

    # Render UI and load module
    output$bead_count_module_ui <- renderUI({
      destroyableBeadCountModuleUI(new_bead_mod_id)
    })

    destroyableBeadCountModuleServer(
      id = new_bead_mod_id,
      selected_study = reactive(input$readxMap_study_accession),
      selected_experiment = reactive(input$readxMap_experiment_accession),
      currentuser()
    )

  } else {
    # If switching away, destroy any existing bead module
    try(destroyModule(paste0("bead_count_mod_", reload_bead_count)), silent = TRUE)
    output$bead_count_module_ui <- renderUI({ NULL })
  }
  gc(verbose = TRUE)
})



observeEvent(input$study_level_tabs, {
  if (input$study_level_tabs == "Experiments") {
    updateRadioGroupButtons(session, "qc_component", selected = "Data")
  }
})

load_project <- function(conn, project_id, current_user){
    if (project_id != "") {
      dbBegin(conn)
      tryCatch({
        result <- dbGetQuery(conn, glue::glue("SELECT COUNT(*) as user_count FROM madi_lumi_users.project_users WHERE project_id = {project_id} AND user_id = '{current_user}'"))
        if (result$user_count > 0) {
          # Retrieve the project name for the given project_id
          project_name_query <- glue::glue("SELECT project_name FROM madi_lumi_users.projects WHERE project_id = {project_id}")
          project_name_result <- dbGetQuery(conn, project_name_query)

          if (nrow(project_name_result) > 0) {
            project_name <- project_name_result$project_name[1]

            query_check <- sprintf("SELECT 1 FROM madi_results.xmap_users WHERE auth0_user = %s", dbQuoteLiteral(conn, current_user))
            exists <- dbGetQuery(conn, query_check)

            # Insert or update the user record
            if (nrow(exists) == 0) {

              query_insert <- glue::glue("INSERT INTO madi_results.xmap_users (auth0_user, project_name, workspace_id) VALUES ({dbQuoteLiteral(conn, current_user)}, {dbQuoteLiteral(conn, project_name)}, {project_id})")
              dbExecute(conn, query_insert)
              message("New user inserted in xmap.")
            } else {

              query_update <- glue::glue("UPDATE madi_results.xmap_users SET project_name = {dbQuoteLiteral(conn, project_name)}, workspace_id = {project_id} WHERE auth0_user = {dbQuoteLiteral(conn, current_user)}")
              dbExecute(conn, query_update)

              message("User updated in xmap.")
            }

            dbCommit(conn)
            showNotification(glue::glue("Project {project_id} loaded successfully!"), type = "message")
          } else {
            dbRollback(conn)
            showNotification("Project not found.", type = "error")
          }
        } else {
          dbRollback(conn)
          showNotification("You do not have access to the project you are trying to load.", type = "error")
        }
      }, error = function(e) {
        dbRollback(conn)
        showNotification(glue::glue("Failed to load project '{project_id}'. Error: {e$message}"), type = "error")
      })
    }
  source("import_lumifile.R", local=TRUE)
}

observeEvent(userWorkSpaceID(), {
  reset_import_values()
})

refreshTabUI <- function(tabName) {
  # Get current counters
  current_counters <- tabRefreshCounter()
  # Increment counter for specific tab
  current_counters[[tabName]] <- current_counters[[tabName]] + 1
  # Update counters
  tabRefreshCounter(current_counters)
}

output$manage_project_ui <- renderUI({
  if (input$main_tabs != "home_page") {
  fluidRow(
    column(12,
           # Create Project Section
           h3("Project Management"),
           wellPanel(
             h4("Create New Project"),
             bsCollapse(
               id = "createNewProjectCollapse",
               bsCollapsePanel(
                 title = "Create New Project Documentation",
                 style = "success",
                 tagList(
                   tags$p("To create a new project in the 'Enter Project Name:' field
                          type the name of the new project and press 'Enter'.")
                 )
               )
             ),
             textInput("project_name", "Enter Project Name:"),
             actionButton("create_project", "Enter", class = "btn-success")
           ),
           hr(),
           # Add Project Section
           wellPanel(
             h4("Add New Project"),
             bsCollapse(
               id = "addProjectDocumentation",
               bsCollapsePanel(
                 title = "Create New Project Documentation",
                 style = "success",
                 tagList(
                   tags$p("To add a new project given an existing project ID and access ID,
                          type the project ID and access ID in the respective fields and press 'Add Project'")
                 )
               )
             ),
             textInput("project_id", "Enter Project ID:"),
             textInput("access_id", "Enter Access ID:"),
             actionButton("add_project", "Add Project", class = "btn-success")
           ),
           hr(),
           # Load Project Section
           wellPanel(
             h4("Load Existing Project"),
             uiOutput("load_ui")
           )
    )
  )
  } else {
     NULL
  }
})

