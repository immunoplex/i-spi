#observeEvent(list(input$inLoadedData, input$readxMap_experiment_accession), {
# observeEvent(list(
#      input$readxMap_experiment_accession,
#      input$readxMap_study_accession,
#      input$qc_component,
#      input$study_level_tabs,
#      input$main_tabs), {

fetch_db_samples_bc <- function(study_accession, experiment_accession, project_id) {
  query <- paste0("SELECT * FROM madi_results.xmap_sample
WHERE project_id = ",project_id, "
AND study_accession = '", study_accession,"'
AND experiment_accession = '", experiment_accession,"'
")

  sample_df  <- dbGetQuery(conn, query)
  names(sample_df)[names(sample_df) == "antibody_n"] <- "n"
  # sample_df$plateid <- gsub("[[:punct:][:blank:]]+", ".", basename(gsub("\\", "/", sample_df$plate_id, fixed=TRUE)))
  sample_df$plate_nom <- paste(sample_df$plate, sample_df$nominal_sample_dilution, sep = "-")
  return(sample_df)
}

fetch_db_buffer <- function(study_accession, experiment_accession) {
  query <- paste0("SELECT * FROM madi_results.xmap_buffer
WHERE study_accession = '", study_accession,"'
AND experiment_accession = '", experiment_accession,"'
")
buffer_data <- dbGetQuery(conn, query)
buffer_data$plateid <- gsub("[[:punct:][:blank:]]+", ".", basename(gsub("\\", "/", buffer_data$plate_id, fixed=TRUE)))
names(buffer_data)[names(buffer_data) == "antibody_n"] <- "n"
names(buffer_data)[names(buffer_data) == "antibody_mfi"] <- "mfi"
buffer_data <- distinct(buffer_data[,!(names(buffer_data) %in% c("xmap_buffer_id", "antibody_name","plate_id"))])
return(buffer_data)
}

fetch_db_standards_bc <- function(study_accession, experiment_accession) {
  query <- paste0("SELECT * FROM madi_results.xmap_standard
WHERE study_accession = '", study_accession,"'
AND experiment_accession = '", experiment_accession,"'
")
  standard_df  <- dbGetQuery(conn, query)
  standard_df$plateid <- gsub("[[:punct:][:blank:]]+", ".", basename(gsub("\\", "/", standard_df$plate_id, fixed=TRUE)))
  names(standard_df)[names(standard_df) == "antibody_n"] <- "n"
  names(standard_df)[names(standard_df) == "antibody_mfi"] <- "mfi"
  standard_df <- distinct(standard_df[,!(names(standard_df) %in% c("xmap_standard_id", "antibody_name", "plate_id"))])
  return(standard_df)
}



beadCountModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML("
        .bead-count-collapse-container {
          width: 75vw;
          overflow-x: auto;
        }
      "))),
    br(),
    fluidRow(
      bsCollapse(
        id = ns("bead_count_collapse"),
        bsCollapsePanel(
          title = "Bead Count Analysis Methods",
          tagList(
            tags$p("Use the dropdown menu labeled ‘Plate in Sample Data’ to select a plate from the sample data in the currently selected experiment. Then, choose an antigen from the selected plate to analyze bead counts for that antigen."),
            tags$p("The figure below the dropdown menus displays bead counts on the y-axis for each of the 96 wells in the multiplex bead assay on the x-axis (Matson, Zachary et al). In the figure:"),
            tags$ul(
              tags$li("The red dotted horizontal line represents the lower threshold value."),
              tags$li("The blue dotted horizontal line represents the upper threshold value."),
              tags$li("The default lower and upper thresholds are 35 and 50, respectively."),
              tags$li("The black solid horizontal line represents the average bead count across all wells."),
              tags$li("Red-colored wells indicate a low bead count, while blue-colored wells have sufficient bead counts based on the failed well criterion.")
            ),

            tags$p("The failed well criterion is either wells with bead counts below the upper threshold or wells with bead counts below the upper threshold.
                     To adjust the lower and upper thresholds and modify the failed well criterion, go to the Study Overview tab and navigate to Bead Count Options."),

            tags$p("Below the figure, a table titled ‘Sample Values with Low Bead Counts’ lists samples that meet the low bead count criteria. The table includes:"),
            tags$ul(
              tags$li("bead_count_gc: The gate class of the bead count, indicating whether it is sufficient or low."),
              tags$li("is_low_bead_count: A Boolean column where true indicates a low bead count and false indicates a sufficient bead count.")
            ),

            tags$p("To download the bead count gate class for all samples in the currently selected experiment within the selected study, click the download button below the table."),
            tags$h3("References"),
            tags$p("Matson, Zachary et al. “shinyMBA: a novel R shiny application for quality control of the multiplex bead assay for serosurveillance studies.” Scientific reports vol. 14,1 7442. 28 Mar. 2024, doi:10.1038/s41598-024-57652-4")
          ), #end tagList
          style = "success"
        )

      ),
      mainPanel(
        fluidRow(
          column(6, uiOutput(ns("plateSelection_bead_count_UI"))),
          column(6,uiOutput(ns("sample_data_antigenUI"))),
        ),
        plotlyOutput(ns("beadCountPlot"), width = "75vw"),
        br(),
        div(style = "overflow-x: auto; width: 100%;",tableOutput(ns("sample_low_bead_count_table"))),
        # div(
        #   class = "bead-count-collapse-container",
        # bsCollapse(
        #   id = ns("sample_bead_count_collapse"),
        #   bsCollapsePanel(
        #     title = "Samples with low Bead Counts",
        #     div(style = "overflow-x: auto; width: 100%;",tableOutput(ns("sample_low_bead_count_table"))),
        #     style = "primary"
        #   )
        # )
        # ),
        #div(style = "overflow-x: auto; width: 100%;",tableOutput("sample_low_bead_count_table")),
        br(),
        #uiOutput(ns("download_bead_gating"))
        downloadButton(ns("download_bead_gating"), "Download Bead Count Data")
      )
    )
  )
}

beadCountServer <- function(id, selected_study, selected_experiment,currentuser) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # selected_study <- selected_studyexpplate$study_accession
    # selected_experiment <- selected_studyexpplate$experiment_accession
    # selected_study <- reactive({
    #
    #   req(selected_studyexpplate$study_accession && selected_studyexpplate$study_accession != "")
    #   selected_studyexpplate$study_accession
    # })
    #
    # selected_experiment <- reactive({
    #   req(selected_studyexpplate$experiment_accession && selected_studyexpplate$experiment_accession != "")
    #   selected_studyexpplate$experiment_accession
    # })

   #observeEvent(reload_flag, {


    sample_data_bc <- fetch_db_samples_bc(study_accession = selected_study(), experiment_accession = selected_experiment(),
                                          project_id = userWorkSpaceID())
    sample_data_bc <- obtain_well_number(sample_data_bc, "well")
    cat("Bead Count: Sample Data\n")
    print(names(sample_data_bc))

    #standard_data_curve <- fetch_db_standards_bc(study_accession = selected_study(), experiment_accession = selected_experiment())

    if (!is.null(selected_study()) && length(selected_study()) > 0 &&
        !is.null(selected_experiment()) && length(selected_experiment()) > 0 &&
        !is.null(sample_data_bc) && length(sample_data_bc > 0)){

      # Filter sample data
      sample_data_bc$selected_str <- paste0(sample_data_bc$study_accession, sample_data_bc$experiment_accession)
      sample_data_bc <- sample_data_bc[sample_data_bc$selected_str == paste0(selected_study(), selected_experiment()), ]

      # Summarize sample data
      cat("Viewing sample data")
      print(names(sample_data_bc))
      print(table(sample_data_bc$plateid))
      print(table(sample_data_bc$antigen))
      cat("After summarizing sample data")


      # Rename columns

      sample_data_bc <- dplyr::rename(sample_data_bc, arm_name = agroup)
      sample_data_bc <- dplyr::rename(sample_data_bc, visit_name = timeperiod)


      sample_data_bc$subject_accession <- sample_data_bc$patientid

      print(names(sample_data_bc))


      sample_data_bc <- dplyr::rename(sample_data_bc, value_reported = antibody_mfi)

      arm_choices <- unique(sample_data_bc$arm_name)
      visits <- unique(sample_data_bc$visit_name)

    }

    # if (!is.null(selected_study()) && length(selected_study()) > 0 &&
    #     !is.null(selected_experiment()) && length(selected_experiment()) > 0 &&
    #     !is.null(standard_data_curve) && length(standard_data_curve) > 0){
    #
    #   # Filter sample data
    #   standard_data_curve$selected_str <- paste0(standard_data_curve$study_accession, standard_data_curve$experiment_accession)
    #   standard_data_curve <- standard_data_curve[standard_data_curve$selected_str == paste0(selected_study(), selected_experiment()), ]
    #
    #   # Summarize std curve data data
    #   cat("View Standard Curve data plateid")
    #   print(table(standard_data_curve$plateid))
    #   cat("View Standard Curve data antigen")
    #   print(table(standard_data_curve$antigen))
    #
    #   std_curve_data <- standard_data_curve
    #
    #
    #   # Rename columns
    #
    #   # data <- dplyr::rename(data, arm_name = agroup)
    #   #data <- dplyr::rename(data, visit_name = timeperiod)
    #
    #
    #   std_curve_data$subject_accession <- std_curve_data$patientid
    #
    #   std_curve_data <- calculate_log_dilution(std_curve_data)
    #   cat("Standard Curve data after calculating log dilutions")
    #   print(names(std_curve_data))
    #
    # }

    ## Load study configuration
    study_configuration <- fetch_study_configuration(db_pool, study = selected_study(), project_id = userWorkSpaceID())
    failed_well_criteria <- study_configuration[study_configuration$param_name == "failed_well_criteria",]$param_character_value
    upper_bc_threshold <- study_configuration[study_configuration$param_name == "upper_bc_threshold",]$param_integer_value
    lower_bc_threshold <-  study_configuration[study_configuration$param_name == "lower_bc_threshold",]$param_integer_value
    pct_agg_threshold <- study_configuration[study_configuration$param_name == "pct_agg_threshold",]$param_integer_value
    print(failed_well_criteria)
    print(upper_bc_threshold)
    print(lower_bc_threshold)
    print(pct_agg_threshold)

    output$plateSelection_bead_count_UI <- renderUI({
    #  req(input$readxMap_study_accession, input$readxMap_experiment_accession)
    #  req(selected_study(), selected_experiment())
      req(sample_data_bc$study_accession, sample_data_bc$experiment_accession)
      updateSelectInput(session, ns("plateSelection_bead"), selected = NULL)  # Reset the plateSelection
      bead_plate_data <- sample_data_bc[sample_data_bc$study_accession %in% selected_study() &
                                       sample_data_bc$experiment_accession %in% selected_experiment(), ]

      req(nrow(bead_plate_data) > 0)

      selectInput(ns("plateSelection_bead"),
                  label = "Plate - Sample Dilution(s) in Sample Data",
                  choices = unique(bead_plate_data$plate_nom))
    })

    # sample data antigens
    output$sample_data_antigenUI <- renderUI({
     # req(input$readxMap_study_accession, input$readxMap_experiment_accession)
    #  req(selected_study(), selected_experiment())
      req(sample_data_bc$study_accession, sample_data_bc$experiment_accession)
      req(input$plateSelection_bead)  # Ensure a plate is selected

      updateSelectInput(session, ns("antigenSelectionBead"), selected = NULL)
      # Filter data for the selected study, experiment, and plate
      dat_antigen <- sample_data_bc[
        sample_data_bc$study_accession %in% selected_study() &
          sample_data_bc$experiment_accession %in% selected_experiment() &
          sample_data_bc$plate_nom %in% input$plateSelection_bead,
      ]

      # Ensure there are valid antigen values
      req(nrow(dat_antigen) > 0)

      selectInput(ns("antigenSelectionBead"),
                  label = "Antigen",
                  choices = unique(dat_antigen$antigen))
    })
    output$beadCountPlot <- renderPlotly({
      req(sample_data_bc)
      req(input$plateSelection_bead, input$antigenSelectionBead)
      # req(lower_threshold_rv())
      # req(upper_threshold_rv())
      # req(failed_well_criteria())
      req(study_configuration)
      # req(failed_well_criteria)
      # req(upper_bc_threshold)
      # req(lower_bc_threshold)
      sub_sample_data_bc <- sample_data_bc[sample_data_bc$plate_nom == input$plateSelection_bead & sample_data_bc$antigen == input$antigenSelectionBead,]
      plot_bead_count(df_well = sub_sample_data_bc, lower_threshold = lower_bc_threshold,
                      upper_threshold = upper_bc_threshold , failed_well_criteria =  failed_well_criteria)
      # plot_bead_count(df_well = sample_data_well(), lower_threshold = lower_threshold_rv(),
      #                 upper_threshold = upper_threshold_rv() , failed_well_criteria =  failed_well_criteria())
    })


    # updateSelectInput(session, "plateSelection_bead", selected = unique(sample_data_bc$plateid)[1])
    # updateSelectInput(session, "antigenSelectionBead", selected = unique(sample_data_bc$antigen)[1])

    # Table of Sample data with low bead counts
    output$sample_low_bead_count_table <- renderTable({
      req(sample_data_bc)
     # req(input$plateSelection_bead, input$antigenSelectionBead)
      req(study_configuration)
      sub_sample_data_bc <- sample_data_bc[sample_data_bc$plate_nom == input$plateSelection_bead & sample_data_bc$antigen == input$antigenSelectionBead,]
      bead_count_gc_table <- bead_count_gc(df_well = sub_sample_data_bc, lower_threshold = lower_bc_threshold,
                                           upper_threshold = upper_bc_threshold , failed_well_criteria =  failed_well_criteria)

      bead_count_gc_table <- bead_count_gc_table[bead_count_gc_table$bead_count_gc == "Low Bead Count",]

      return(bead_count_gc_table)
    }, caption = "Sample Values with Low Bead Counts",
    caption.placement = getOption("xtable.caption.placement", "top"))

    # Download Bead Counts for Sample data
   #  output$download_bead_gating <- renderUI({
   #    req(sample_data_bc)
   #  #  req(selected_study())
   #   # req(selected_experiment())
   #   # req(input$readxMap_study_accession, input$readxMap_experiment_accession)
   #    req(study_configuration)
   #    bead_count_gc_table <- bead_count_gc(df_well = sample_data_bc, lower_threshold = lower_bc_threshold,
   #                                         upper_threshold = upper_bc_threshold , failed_well_criteria =  failed_well_criteria)
   #
   #    download_bead_count_data(download_df = bead_count_gc_table, selected_study = selected_study(),
   #                             selected_experiment = selected_experiment())
   #
   #  })
   #
  # })

  output$download_bead_gating <-  downloadHandler(
    filename = function() {
      paste(selected_study(), selected_experiment(), "bead_count_data.csv", sep = "_")
    },
    content = function(file) {
      req(selected_study())
      req(selected_experiment())
      req(sample_data_bc)
      req(study_configuration)

      bead_count_gc_table <- bead_count_gc(df_well = sample_data_bc, lower_threshold = lower_bc_threshold,
                                          upper_threshold = upper_bc_threshold , failed_well_criteria =  failed_well_criteria)


      bead_count_gc_table <-  bead_count_gc_table[bead_count_gc_table$experiment_accession == selected_experiment(),]
      # download data component (data frame)
      write.csv(bead_count_gc_table, file)
    }
  )

  })
  #})
}


# --- Destroyable wrappers ---
destroyableBeadCountModuleUI <- makeModuleUIDestroyable(beadCountModuleUI)
destroyableBeadCountModuleServer <- makeModuleServerDestroyable(beadCountServer)


#
# observeEvent(input$qc_component, {
#
#        req(input$readxMap_study_accession != "Click here",
#            input$readxMap_experiment_accession != "Click here",
#            input$study_level_tabs == "Experiments",
#            input$main_tabs == "view_files_tab")
#
#        cat("in bead count observe Event")
#
#   #req(input$inLoadedData, input$readxMap_experiment_accession)
#
#   if (input$qc_component == "Bead Count") {
#
#     cat("in bead count observe Event panel")
#
#     selected_study <- selected_studyexpplate$study_accession
#     selected_experiment <- selected_studyexpplate$experiment_accession
#
#     sample_data <- stored_plates_data$stored_sample
#     sample_data <- obtain_well_number(sample_data, "well")
#     cat("Bead Count: Sample Data\n")
#     print(names(sample_data))
#
#     standard_data_curve <- stored_plates_data$stored_standard
#     buffer_data <- stored_plates_data$stored_buffer
#
#     # Check if selected study, experiment, and sample data are available
#     if (!is.null(selected_study) && length(selected_study) > 0 &&
#         !is.null(selected_experiment) && length(selected_experiment) > 0 &&
#         !is.null(sample_data) && length(sample_data) > 0){
#
#       # Filter sample data
#       sample_data$selected_str <- paste0(sample_data$study_accession, sample_data$experiment_accession)
#       sample_data <- sample_data[sample_data$selected_str == paste0(selected_study, selected_experiment), ]
#
#       # Summarize sample data
#       cat("Viewing sample data")
#       print(names(sample_data))
#       print(table(sample_data$plateid))
#       print(table(sample_data$antigen))
#       cat("After summarizing sample data")
#
#
#       # Rename columns
#
#       sample_data <- dplyr::rename(sample_data, arm_name = agroup)
#       sample_data <- dplyr::rename(sample_data, visit_name = timeperiod)
#
#
#       sample_data$subject_accession <- sample_data$patientid
#
#       sample_data <- dplyr::rename(sample_data, value_reported = mfi)
#
#       arm_choices <- unique(sample_data$arm_name)
#       visits <- unique(sample_data$visit_name)
#
#     }
#
#     # Check if selected study, experiment, and standard data are available
#     if (!is.null(selected_study) && length(selected_study) > 0 &&
#         !is.null(selected_experiment) && length(selected_experiment) > 0 &&
#         !is.null(standard_data_curve) && length(standard_data_curve) > 0){
#
#       # Filter sample data
#       standard_data_curve$selected_str <- paste0(standard_data_curve$study_accession, standard_data_curve$experiment_accession)
#       standard_data_curve <- standard_data_curve[standard_data_curve$selected_str == paste0(selected_study, selected_experiment), ]
#
#       # Summarize std curve data data
#       cat("View Standard Curve data plateid")
#       print(table(standard_data_curve$plateid))
#       cat("View Standard Curve data antigen")
#       print(table(standard_data_curve$antigen))
#
#       std_curve_data <- standard_data_curve
#
#
#       # Rename columns
#
#       # data <- dplyr::rename(data, arm_name = agroup)
#       #data <- dplyr::rename(data, visit_name = timeperiod)
#
#
#       std_curve_data$subject_accession <- std_curve_data$patientid
#
#       std_curve_data <- calculate_log_dilution(std_curve_data)
#       cat("Standard Curve data after calculating log dilutions")
#       print(names(std_curve_data))
#
#     }
#
#     ## Load study configuration
#     study_configuration <- fetch_study_configuration(study_accession = selected_study , user = currentuser())
#     failed_well_criteria <- study_configuration[study_configuration$param_name == "failed_well_criteria",]$param_character_value
#     upper_bc_threshold <- study_configuration[study_configuration$param_name == "upper_bc_threshold",]$param_integer_value
#     lower_bc_threshold <-  study_configuration[study_configuration$param_name == "lower_bc_threshold",]$param_integer_value
#     pct_agg_threshold <- study_configuration[study_configuration$param_name == "pct_agg_threshold",]$param_integer_value
#     print(failed_well_criteria)
#     print(upper_bc_threshold)
#     print(lower_bc_threshold)
#     print(pct_agg_threshold)
#
#
#
#
#   output$beadCountAnalysisUI <- renderUI({
#    # req(study_configuration)
#     cat("\nrendering bead count UI\n")
#     tagList(
#       br(),
#       fluidRow(
#         bsCollapse(
#           id = "bead_count_collapse",
#           bsCollapsePanel(
#             title = "Bead Count Analysis Methods",
#             tagList(
#               tags$p("Use the dropdown menu labeled ‘Plate in Sample Data’ to select a plate from the sample data in the currently selected experiment. Then, choose an antigen from the selected plate to analyze bead counts for that antigen."),
#               tags$p("The figure below the dropdown menus displays bead counts on the y-axis for each of the 96 wells in the multiplex bead assay on the x-axis (Matson, Zachary et al). In the figure:"),
#               tags$ul(
#                 tags$li("The red dotted horizontal line represents the lower threshold value."),
#                 tags$li("The blue dotted horizontal line represents the upper threshold value."),
#                 tags$li("The default lower and upper thresholds are 35 and 50, respectively."),
#                 tags$li("The black solid horizontal line represents the average bead count across all wells."),
#                 tags$li("Red-colored wells indicate a low bead count, while blue-colored wells have sufficient bead counts based on the failed well criterion.")
#               ),
#
#               tags$p("The failed well criterion is either wells with bead counts below the upper threshold or wells with bead counts below the upper threshold.
#                      To adjust the lower and upper thresholds and modify the failed well criterion, go to the Study Overview tab and navigate to Bead Count Options."),
#
#            tags$p("Below the figure, a table titled ‘Sample Values with Low Bead Counts’ lists samples that meet the low bead count criteria. The table includes:"),
#            tags$ul(
#                 tags$li("bead_count_gc: The gate class of the bead count, indicating whether it is sufficient or low."),
#                 tags$li("is_low_bead_count: A Boolean column where true indicates a low bead count and false indicates a sufficient bead count.")
#            ),
#
#           tags$p("To download the bead count gate class for all samples in the currently selected experiment within the selected study, click the orange download button below the table."),
#           tags$h3("References"),
#           tags$p("Matson, Zachary et al. “shinyMBA: a novel R shiny application for quality control of the multiplex bead assay for serosurveillance studies.” Scientific reports vol. 14,1 7442. 28 Mar. 2024, doi:10.1038/s41598-024-57652-4")
#             ), #end tagList
#             style = "success"
#           )
#
#         ),
#       mainPanel(
#       fluidRow(
#         column(6, uiOutput("plateSelection_bead_count_UI")),
#         column(6,uiOutput("sample_data_antigenUI")),
#         #column(4, uiOutput("failed_well_criteriaUI"))
#       ),
#       # fluidRow(
#       #   column(4, uiOutput("lower_threshold_ui")),
#       #   column(4, uiOutput("upper_threshold_ui"))
#       #  # tableOutput("sample_data_well_view")
#       # ),
#       plotlyOutput("beadCountPlot", width = "75vw"),
#       br(),
#       bsCollapse(
#         id = "sample_bead_count_collapse",
#         bsCollapsePanel(
#           title = "Samples with low Bead Counts",
#           div(style = "overflow-x: auto; width: 100%;",tableOutput("sample_low_bead_count_table")),
#           style = "primary"
#         )
#       ),
#       #div(style = "overflow-x: auto; width: 100%;",tableOutput("sample_low_bead_count_table")),
#       br(),
#       uiOutput("download_bead_gating")
#     )
#     )
#     )
# })
#
#
#   # sample data plates
#   output$plateSelection_bead_count_UI <- renderUI({
#     req(input$readxMap_study_accession, input$readxMap_experiment_accession)
#     req(sample_data$study_accession, sample_data$experiment_accession)
#     updateSelectInput(session, "plateSelection_bead", selected = NULL)  # Reset the plateSelection
#     bead_plate_data <- sample_data[sample_data$study_accession %in% input$readxMap_study_accession &
#                                    sample_data$experiment_accession %in% input$readxMap_experiment_accession, ]
#
#     req(nrow(bead_plate_data) > 0)
#
#     selectInput("plateSelection_bead",
#                 label = "Plate in Sample data",
#                 choices = unique(bead_plate_data$plateid))
#   })
#   # sample data antigens
#   output$sample_data_antigenUI <- renderUI({
#     req(input$readxMap_study_accession, input$readxMap_experiment_accession)
#     req(sample_data$study_accession, sample_data$experiment_accession)
#     req(input$plateSelection_bead)  # Ensure a plate is selected
#
#     updateSelectInput(session, "antigenSelectionBead", selected = NULL)
#     # Filter data for the selected study, experiment, and plate
#     dat_antigen <- sample_data[
#       sample_data$study_accession %in% input$readxMap_study_accession &
#         sample_data$experiment_accession %in% input$readxMap_experiment_accession &
#         sample_data$plateid %in% input$plateSelection_bead,
#     ]
#
#     # Ensure there are valid antigen values
#     req(nrow(dat_antigen) > 0)
#
#     selectInput("antigenSelectionBead",
#                 label = "Select Antigen",
#                 choices = unique(dat_antigen$antigen))
#   })
#
#   # output$failed_well_criteriaUI <- renderUI({
#   #    radioButtons("thresholdCriteria",
#   #                label = "Failed Well Criteria",
#   #                choices = c("Below Upper Threshold" = "upper",
#   #                            "Below Lower Threshold" = "lower"),
#   #                selected = "lower")
#   # })
#
#   # get the sample data with well number
#   # sample_data_well <- reactive({
#   #   req(sample_data)
#   #   req(input$plateSelection_bead, input$antigenSelectionBead)
#   #   sample_data <- sample_data[sample_data$plateid == input$plateSelection_bead & sample_data$antigen == input$antigenSelectionBead,]
#   #   sample_data_with_well_number <- obtain_well_number(sample_data, "well")
#   #   return(sample_data_with_well_number)
#   # })
#
#  #  # Lower threshold
#  #  output$lower_threshold_ui <- renderUI({
#  #    numericInput(inputId = "lower_threshold",
#  #                 label = "Lower Threshold",
#  #                 value = 25)
#  #  })
#  # # upper threshold
#  #  output$upper_threshold_ui <- renderUI({
#  #    numericInput(inputId = "upper_threshold",
#  #                 label = "Upper Threshold",
#  #                 value = 50)
#  #  })
#
#  # Plot bead count plot by wells on selected plate and antigen for sample data
#   output$beadCountPlot <- renderPlotly({
#     req(sample_data)
#     req(input$plateSelection_bead, input$antigenSelectionBead)
#    # req(lower_threshold_rv())
#    # req(upper_threshold_rv())
#   # req(failed_well_criteria())
#     req(study_configuration)
#     # req(failed_well_criteria)
#     # req(upper_bc_threshold)
#     # req(lower_bc_threshold)
#     sub_sample_data <- sample_data[sample_data$plateid == input$plateSelection_bead & sample_data$antigen == input$antigenSelectionBead,]
#     plot_bead_count(df_well = sub_sample_data, lower_threshold = lower_bc_threshold,
#                     upper_threshold = upper_bc_threshold , failed_well_criteria =  failed_well_criteria)
#     # plot_bead_count(df_well = sample_data_well(), lower_threshold = lower_threshold_rv(),
#     #                 upper_threshold = upper_threshold_rv() , failed_well_criteria =  failed_well_criteria())
#   })
#
#
#   # Table of Sample data with low bead counts
#   output$sample_low_bead_count_table <- renderTable({
#     req(input$plateSelection_bead, input$antigenSelectionBead)
#     #req(lower_threshold_rv())
#     #req(upper_threshold_rv())
#     req(study_configuration)
#     #req(failed_well_criteria)
#     # req(upper_bc_threshold)
#     # req(lower_bc_threshold)
#
#     # bead_count_gc_table <- bead_count_gc(df_well = sample_data_well(), lower_threshold = lower_threshold_rv(),
#     #                 upper_threshold = upper_threshold_rv() , failed_well_criteria =  failed_well_criteria())
#     sub_sample_data <- sample_data[sample_data$plateid == input$plateSelection_bead & sample_data$antigen == input$antigenSelectionBead,]
#     bead_count_gc_table <- bead_count_gc(df_well = sub_sample_data, lower_threshold = lower_bc_threshold,
#                      upper_threshold = upper_bc_threshold , failed_well_criteria =  failed_well_criteria)
#
#     bead_count_gc_table <- bead_count_gc_table[bead_count_gc_table$bead_count_gc == "Low Bead Count",]
#
#     return(bead_count_gc_table)
#   }, caption = "Sample Values with Low Bead Counts",
#   caption.placement = getOption("xtable.caption.placement", "top"))
#
#   # Download Bead Counts for Sample data
#   output$download_bead_gating <- renderUI({
#     req(sample_data)
#     req(input$readxMap_study_accession, input$readxMap_experiment_accession)
#     #req(lower_threshold_rv())
#   #  req(upper_threshold_rv())
#     req(study_configuration)
#    # req(failed_well_criteria)
#     # req(lower_bc_threshold)
#     # req(upper_bc_threshold)
#     # get well number
#    # sample_data_well <- obtain_well_number(sample_data, "well")
#
#     bead_count_gc_table <- bead_count_gc(df_well = sample_data, lower_threshold = lower_bc_threshold,
#                                          upper_threshold = upper_bc_threshold , failed_well_criteria =  failed_well_criteria)
#
#     download_bead_count_data(download_df = bead_count_gc_table, selected_study = input$readxMap_study_accession,
#                              selected_experiment = input$readxMap_experiment_accession)
#
#   })
#
#   }
#   else {
#    # removeOutput("beadCountAnalysisUI", session = session)
#     # removeOutput("plateSelection_bead_count_UI", session = session)
#     # removeOutput("sample_data_antigenUI", session = session)
#     # removeOutput("beadCountPlot", session = session)
#     # removeOutput("sample_low_bead_count_table", session = session)
#     # removeOutput("download_bead_gating", session = session)
#
#     output$beadCountAnalysisUI <- renderUI({ NULL })
#     output$plateSelection_bead_count_UI <- renderUI({ NULL })
#     output$sample_data_antigenUI <- renderUI({ NULL })
#     output$beadCountPlot <- renderPlotly({ NULL })
#     output$sample_low_bead_count_table <- renderTable({ NULL })
#     output$download_bead_gating <- renderUI({ NULL })
#   }
# })
