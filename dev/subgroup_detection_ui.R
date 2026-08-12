#observeEvent(list(input$inLoadedData, input$readxMap_experiment_accession), {
#
# subgroupDetectionModuleUI <- function(id) {
#   ns <- NS(id)
#   tagList(
#     fluidRow(
#       br(),
#       column(12,
#              br(),
#              bsCollapse(
#                id = ns("subgroupDetectionMethods"),
#                bsCollapsePanel(
#                  title = "Subgroup Detection Methods",
#                  tagList(
#                    tags$p("Use the dropdown menu titled 'Response type' to choose the desired type of outcome measured.
#                               Currently the options include MFI, Normalized MFI and Arbitrary Units.
#                               Use the dropdown menu to select the desired transformation that is applied to the data.
#                               Select the first and second visits of interest from the dropdown menu titled 'First Visit' and 'Second Visit respectively,  and the antigen of interest from the dropdown titled 'Antigen'.
#                               To change the reference arm selection navigate to the 'Study Overview' tab and to the Set Reference Arm settings."),
#                    tags$p(HTML("Unstandardized assay features are log<sub>2</sub> transformed")),
#                    tags$p(HTML("Vaccinated assay distributions were classified into one or two distributions using a finite mixture model analysis.")),
#                    tags$p(HTML("The difference plot with the two selected visits were classified as
#                                    increasing, no change (an interval centered on zero; between -7.5% and +7.5% of the total range), and decreasing.")),
#                    tags$p(HTML("To determine the optimal number of clusters (k) used for K-Means clustering, the silhouette score and the gap statistic are calculated and the maximum of the 2 are used.
#                               We limit the number of clusters to be at most 4. If there less than 3 subjects, 1 cluster will be used.
#                               Using the result of this algorithm K-Means clustering with k clusters is used.")),
#                    tags$p(HTML("For a given antigen and visit specification in the selected experiment and study, if only one arm is present, that arm will be displayed in the figures.")),
#                    tags$p(HTML("In the assay classification figure, each line connects a point representing a subject from the first visit to the second visit. The color of the line indicates the direction determined by the K-means clustering results.
#                                    Each subject is colored by the classification of being normal or low at the first visit that is selected.
#                                    These colors match the first figure."))
#
#
#                  ), # end tagList
#                  style = "success"
#                ) # end bsCollapsePanel
#              ),
#              mainPanel(
#                uiOutput(ns("parameter_subgroup_dependencies_UI")),
#                fluidRow(
#                  column(3, uiOutput(ns("selectedFeatureUI"))),
#                ),
#                fluidRow(
#                  column(4,uiOutput(ns("response_class"))),
#                  column(4, uiOutput(ns("transformation_type"))),
#                  column(4, uiOutput(ns("selectedAntigenSampleUI"))),
#
#                ),
#                plotlyOutput(ns("density_histogram_UI"),width = "75vw"),
#                br(),
#                uiOutput(ns("download_density_histogram_first_visit")),
#                br(),
#                uiOutput(ns("show_time_dependent_plots"))
#              )# end main Panel
#       ) # end column
#     ) # end fluidRow
#   ) # end outer tag list
# }
#
# subgroupDetectionServer <-  function(id, selected_study, selected_experiment,currentuser) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#
#
#     sample_data_sg <- fetch_db_samples(study_accession = selected_study(), experiment_accession = selected_experiment())
#     standard_data_curve_sg <- fetch_db_standards(study_accession = selected_study(), experiment_accession = selected_experiment())
#     buffer_data_sg <- fetch_db_buffer(study_accession = selected_study(), experiment_accession = selected_experiment())
#
#     output$test <- renderUI({
#       "Testing"
#     })
#
#   })
# }
# destroyableSubgroupDetectionModuleUI <- makeModuleUIDestroyable(subgroupDetectionModuleUI)
# destroyableSubgroupDetectionServer <- makeModuleServerDestroyable(subgroupDetectionServer)


observeEvent(list(
  input$readxMap_experiment_accession,
  input$readxMap_study_accession,
  input$advanced_qc_component,
  input$study_level_tabs,
  input$main_tabs), {

    req(input$advanced_qc_component == "Subgroup Detection",
        input$readxMap_study_accession != "Click here",
        input$readxMap_experiment_accession != "Click here",
        input$study_level_tabs == "Experiments",
        input$main_tabs == "view_files_tab")
  # req(input$inLoadedData, input$readxMap_experiment_accession)

  if (input$advanced_qc_component == "Subgroup Detection") {
    selected_study <- selected_studyexpplate$study_accession
    selected_experiment <- selected_studyexpplate$experiment_accession
    param_group = "standard_curve_options"
    # Load sample data
    #sample_data <- stored_plates_data$stored_sample
    sample_data <- fetch_best_sample_se_all_summary(study_accession = selected_study, experiment_accession = selected_experiment,
                                                    param_user = currentuser(), project_id = userWorkSpaceID(), conn = conn)

    # study_params_is_log_response <<- fetch_study_parameters(study_accession = selected_study,
    #                                        param_user = currentuser(),
    #                                        param_group =param_group,
    #                                        project_id = userWorkSpaceID(),
    #                                        conn = conn)$is_log_response

    # Check if selected study, experiment, and sample data are available
    # if (!is.null(selected_study) && length(selected_study) > 0 &&
    #     !is.null(selected_experiment) && length(selected_experiment) > 0 &&
    #     !is.null(sample_data) && length(sample_data) > 0){
    #
    #   # Filter sample data
    #   sample_data$selected_str <- paste0(sample_data$study_accession, sample_data$experiment_accession)
    #   sample_data <- sample_data[sample_data$selected_str == paste0(selected_study, selected_experiment), ]
    #
    #   # Summarize sample data
    #   cat("Viewing sample dat in subgroup tab ")
    #   print(names(sample_data))
    #   print(table(sample_data$plateid))
    #   print(table(sample_data$antigen))
    #   cat("After summarizing sample data in subgroup tab")
    #
    #
    #   # Rename columns
    #
    #   sample_data <- dplyr::rename(sample_data, arm_name = agroup)
    #   sample_data <- dplyr::rename(sample_data, visit_name = timeperiod)
    #
    #
    #   sample_data$subject_accession <- sample_data$patientid
    #
    #   sample_data <- dplyr::rename(sample_data, value_reported = mfi)
    #
    #   arm_choices <- unique(sample_data$arm_name)
    #   visits <- unique(sample_data$visit_name)
    #
    # }

    # Load Header Data
    header_data <- stored_plates_data$stored_header

    ## Load study configuration
    study_configuration <- fetch_study_configuration(db_pool, study = selected_study, project_id = userWorkSpaceID())
    reference_arm <- study_configuration[study_configuration$param_name == "reference_arm",]$param_character_value
    primary_timeperiod_comparison <- study_configuration[study_configuration$param_name == "primary_timeperiod_comparison",]$param_character_value
    visit1 <- strsplit(primary_timeperiod_comparison, split = ",")[[1]][1]
    visit2 <- strsplit(primary_timeperiod_comparison, split = ",")[[1]][2]
    timeperiod_order <- study_configuration[study_configuration$param_name == "timeperiod_order",]$param_character_value

    output$subgroupDetectionUI <- renderUI({
      req(study_configuration)
      tagList(
        fluidRow(
          br(),
          column(12,
                 br(),
                 bsCollapse(
                   id = "subgroupDetectionMethods",
                   bsCollapsePanel(
                     title = "Subgroup Detection Methods",
                     tagList(
                       tags$p("Use the dropdown menu titled 'Response type' to choose the desired type of outcome measured.
                              Currently the options include MFI, Normalized MFI and Concentration.
                              Use the dropdown menu to select the desired transformation that is applied to the data.
                              Select the first and second visits of interest from the dropdown menu titled 'First Visit' and 'Second Visit respectively,  and the antigen of interest from the dropdown titled 'Antigen'.
                              To change the reference arm selection navigate to the 'Study Overview' tab and to the Set Reference Arm settings."),
                       tags$p(HTML("Unstandardized assay features are log<sub>2</sub> transformed")),
                       tags$p(HTML("Vaccinated assay distributions were classified into one or two distributions using a finite mixture model analysis.")),
                       tags$p(HTML("The difference plot with the two selected visits were classified as
                                   increasing, no change (an interval centered on zero; between -7.5% and +7.5% of the total range), and decreasing.")),
                       tags$p(HTML("To determine the optimal number of clusters (k) used for K-Means clustering, the silhouette score and the gap statistic are calculated and the maximum of the 2 are used.
                              We limit the number of clusters to be at most 4. If there less than 3 subjects, 1 cluster will be used.
                              Using the result of this algorithm K-Means clustering with k clusters is used.")),
                       tags$p(HTML("For a given antigen and visit specification in the selected experiment and study, if only one arm is present, that arm will be displayed in the figures.")),
                       tags$p(HTML("In the assay classification figure, each line connects a point representing a subject from the first visit to the second visit. The color of the line indicates the direction determined by the K-means clustering results.
                                   Each subject is colored by the classification of being normal or low at the first visit that is selected.
                                   These colors match the first figure."))


                     ), # end tagList
                     style = "success"
                   ) # end bsCollapsePanel
                 ),
                 mainPanel(
                   div(
                     style = "background-color: #f0f8ff; border: 1px solid #4a90e2;
                              padding: 10px; margin-bottom: 15px; border-radius: 5px;",
                     tags$h4("Current Subgroup Detection Context", style = "margin-top: 0; color: #2c5aa0;"),
                      uiOutput("parameter_subgroup_dependencies_UI")
                     ),

                   fluidRow(
                     column(3, uiOutput("selectedFeatureUI")),
                     #column(3, uiOutput("reference_arm_from_table_UI"))
                   ),
                   fluidRow(
                     column(4,uiOutput("response_class")),
                     column(4, uiOutput("transformation_type")),
                     # column(3,uiOutput("visit1")),
                     column(4, uiOutput("selectedAntigenSampleUI")),

                   ),

                   # tableOutput("data_form_table"),
                   # textOutput("cluster_value"),
                   plotlyOutput("density_histogram_UI",width = "75vw"),
                   br(),
                   uiOutput("download_density_histogram_first_visit"),
                   br(),
                   # fluidRow(
                   #   column(4,uiOutput("visit2"))
                   # #   #column(4, uiOutput("reference_armSelect_UI")),
                   # #
                   # #   #column(4, uiOutput("transformation_type")),
                   # #
                   # ),
                   uiOutput("show_time_dependent_plots")
                   # plotlyOutput("visit_difference_UI", width = "75vw"),
                   # br(),
                   # plotlyOutput("difference_histogram_UI", width = "75vw"),
                   # br(),
                   # plotlyOutput("assay_classification_UI", width = "75vw")
                 )# end main Panel
          ) # end column
        ) # end fluidRow
      ) # end outer tag list
    })

    # update to ensure if go to subgroup detection and not study overview options.
    observe({
      req(reference_arm_rv())
      updated_reference_arm <- reference_arm_rv()
      if (all(updated_reference_arm$referent == FALSE)) {
        updated_reference_arm[1, "referent"] <- TRUE

        selected_study_accession <- unique(updated_reference_arm$study_accession)
        selected_agroup <- updated_reference_arm[updated_reference_arm$referent == T,]$agroup

        query_update_referent <- paste0("UPDATE madi_results.xmap_arm_reference
                             SET referent = TRUE
                             WHERE study_accession = '", selected_study_accession, "' AND agroup = '", selected_agroup, "';")
        # Set the selected row to TRUE
        dbExecute(conn, query_update_referent)

      } else {
        print("Not all referents are false. Do nothing.")
      }

      reference_arm_selected <- updated_reference_arm[updated_reference_arm$referent == TRUE, "agroup", drop = FALSE]
      reference_arm(as.character(reference_arm_selected))

    })


    output$show_time_dependent_plots <- renderUI({
    # req(input$visit1Selection, input$visit2Selection)
      req(visit1, visit2)
      if (visit1 != visit2) {
        # if (res2row() == 0) {
        #   HTML(paste("<span style='font-size:20px;'> There are no rows for the combination of visit",
        #                            input$input$visit1SelectionSelection, "and visit", input$visit2Selection, "for clustering"))
        #

        if (is.null(difres_reactive())) {

          HTML(paste("<span style='font-size:20px;'> The number of clusters for visit",input$visit1Selection,
                     "and visit", visit2,
                     "must be between 1 and 3 exclusive, which is set in the algorithm.<br></span>"))
        }
        # } else if (nrow(difres_reactive()) == 0) {
        #   HTML(paste("<span style='font-size:20px;'> There are no rows for the combination of visit",
        #              input$input$visit1SelectionSelection, "and visit", input$visit2Selection, "for clustering"))
        # }
        else {
          tagList(
            uiOutput("k_selection_display"),
            br(),
            plotlyOutput("visit_difference_UI", width = "75vw"),
            br(),
            uiOutput("download_visit_difference"),
            br(),
            plotlyOutput("difference_histogram_UI", width = "75vw"),
            br(),
            uiOutput("download_difference_histogram_data"),
            br(),
            plotlyOutput("assay_classification_UI", width = "75vw"),
            br(),
            uiOutput("download_assay_classification_data")
          )
        }
      } else {
        HTML(paste("<span style='font-size:20px;'> The first and second visits must be different to proceed.<br></span>"))
      }
    })

    output$selectedFeatureUI <- renderUI({
      req(sample_data)
      radioButtons("featureSelection",
                   label = "Feature",
                   choices = unique(sample_data$feature)
      )
    })

    # output$response_class <- renderUI({
    #   req(sample_data)
    #   sample_data_response <<- sample_data
    #   selectInput(
    #     inputId = "responseSelection",
    #     label = "Response type:",
    #     choices = c("MFI",
    #                 "Normalized MFI",
    #                 "Arbitrary Units")
    #   )
    # })

    output$response_class <- renderUI({
      req(sample_data)
      req(input$antigenSampleSelection)


      dat <- sample_data[sample_data$antigen == input$antigenSampleSelection,]

      choices <- c("MFI", "Concentration")

      if (any(!is.na(dat$norm_assay_response))) {
        choices <- c("MFI", "Normalized MFI", "Concentration")
      }

      selectInput(
        inputId = "responseSelection",
        label = "Response type:",
        choices = choices
      )
    })

    observeEvent({
        req(sample_data)
        req(input$antigenSampleSelection)
        dat <- sample_data[sample_data$antigen == input$antigenSampleSelection,]
        any(!is.na(dat$norm_assay_response))
      },
      {
        dat <- sample_data[sample_data$antigen == input$antigenSampleSelection,]

        if (!any(!is.na(dat$norm_assay_response)) &&
            identical(input$responseSelection, "Normalized MFI")) {

          updateSelectInput(
            session,
            "responseSelection",
            selected = "MFI"
          )
        }
      },
      ignoreInit = TRUE
    )


    # output$response_class <- renderUI({
    #   req(sample_data)
    #   sample_data_res <- sample_data[sample_data$feature == input$featureSelection,]
    #   selectInput(
    #     inputId = "responseSelection",
    #     label = "Response type:",
    #     choices = setNames(
    #       c(
    #         assay_response_variable,
    #         "Normalized MFI",
    #         "Arbitrary Units"
    #       ),
    #       c(
    #         "Raw Assay Response",
    #         paste0("Normalized ", format_assay_terms(assay_response_variable)),
    #         "Arbitrary Units"
    #       )
    #     )
    #   )
        # choices = c("Raw Assay Response" = assay_response_variable,
        #             norm_choice_label = "Normalized MFI",
        #             "Arbitrary Units" = "Arbitrary Units")


    ## Box noting parameter dependencies
    output$parameter_subgroup_dependencies_UI <- renderUI({
      #req(reference_arm, visit1, visit2, timeperiod_order)
      req(study_configuration)

      HTML(
        paste0(
          "<b>Parameter Dependencies</b><br><br>",

          "<b>Timepoint selection:</b><br>",
          "The first and second timepoints are defined in the study configuration and are currently set to ",
          "<b>", visit1, "</b> (first visit) and <b>", visit2, "</b> (second visit). ",
          "The order of the timepoints matters, and the first visit is used for the finite mixture model.<br><br>",

          "<b>Reference arm:</b><br>",
          "The assay classification uses <b>", reference_arm,
          "</b> as the referent arm, as specified in the study parameters.<br><br>",

          "<b>Timepoint ordering:</b><br>",
          "The timepoints are evaluated according to the configured order: <b>",
          timeperiod_order, "</b>.<br><br>",

          "<b>Response availability:</b><br>",
          "Available response types are determined dynamically based on the selected antigen. ",
          "Normalized MFI is only available when normalized values have been computed for that antigen in the currrent experiment and study."
        )
      )
    })

    # output$parameter_subgroup_dependencies_UI <- renderUI({
    #   # tagList(bsCollapse(
    #   #   id = "param_subgroup_dependencies",
    #   #   bsCollapsePanel(
    #   #     title = "Parameter Dependencies",
    #       HTML(
    #         "The first and second timepoints are set in the subgroup detection section within the study paramaters.
    #         The order of the timepoints matters and the first timepoint is used for the finite mixture model. <br>
    #         The arm that is considered the referent arm in the  assay classification depends on what is set in the study paramaters.
    #         The available response types are determined dynamically based on the data associated with the selected antigen.
    #         The option for normalized MFI is only shown when normalized values have been computed")
    #   #     ),
    #   #     style = "info"
    #   #   )
    #   # ),
    #  # actionButton("to_study_parameters_from_subgroup", label = "Return to Study Parameters")
    #
    # })

    # Switch tabs when click button
    observeEvent(input$to_study_parameters_from_subgroup, {
      updateTabsetPanel(session, inputId = "study_level_tabs", selected = "Study Parameters")
    })
    # output$visit1 <- renderUI({
    #   req(visits)
    #   selectInput(
    #     inputId = "visit1Selection",
    #     label = "First Visit",
    #     choices = visits
    #   )
    # })
    # output$visit2 <- renderUI({
    #   req(visits)
    #   selectInput(
    #     inputId = "visit2Selection",
    #     label = "Second Visit",
    #     choices = visits
    #   )
    # })

    output$k_clustersUI <- renderUI({
      numericInput(
        inputId = "k_clusters",
        label = "Number of centers (k)",
        min = 1,
        max = 3)
    })

    # output$reference_armSelect_UI<- renderUI({
    #   req(arm_choices)
    #   selectInput(
    #     inputId = "referenceArmSelection",
    #     label = "Reference Arm",
    #     choices = arm_choices
    #   )
    # })

    output$reference_arm_from_table_UI <- renderUI({
      req(reference_arm())
      tagList(
        tags$p(reference_arm())
      )
    })

    output$transformation_type <- renderUI(
      selectInput(
        inputId = "transformationTypeSelection",
        label = "Transformation",
        choices =  c("Unstandardized, not scaled","Unstandardized, scaled")
      )
    )

    output$selectedAntigenSampleUI <- renderUI({
      req(sample_data)
      req(header_data)
      req(luminex_features())
      sample_data <- sample_data[!(sample_data$antigen %in% colnames(header_data)), ]
      sample_data <- sample_data[!(sample_data$antigen %in% luminex_features()), ]

      selectInput("antigenSampleSelection",
                  label = "Antigen",
                  choices = unique(sample_data$antigen)
      )
    })

    ## Reactive data form
    data_form_reactive <- reactive({
      req(sample_data)
      req(visit1, visit2, input$responseSelection)
      data_form_df <- create_data_form_df(data = sample_data, t0 = visit1, t1 = visit2, log_assay_outcome = input$responseSelection)
      cat("after data form is created")
      return(data_form_df)
    })

    # data_form_reactive <- reactive({
    #   req(sample_data)
    #   req(visit1, visit2, input$responseSelection)
    #
    #   data_form_df <- create_data_form_df(
    #     data = sample_data,
    #     t0 = visit1,
    #     t1 = visit2,
    #     log_assay_outcome = input$responseSelection
    #   )
    #
    #   n_before <- nrow(data_form_df)
    #   cat("data form_df names")
    #   print(names(data_form_df))
    #   data_form_df <- data_form_df %>%
    #     dplyr::filter(is.finite(log_assay_value))
    #
    #   n_after <- nrow(data_form_df)
    #
    #
    #
    #   if (n_after == 0) {
    #     showNotification(
    #       paste0(
    #         "No finite response values are available for the selected visit combination (",
    #         visit1, "  ", visit2,
    #         ") using ", input$responseSelection, ". ",
    #         "Please choose a different response type or visit combination."
    #       ),
    #       type = "error",
    #       duration = NULL
    #     )
    #     return(NULL)
    #   }
    #
    #   if (n_after < n_before) {
    #     showNotification(
    #       paste0(
    #         n_before - n_after,
    #         " samples with non-finite response values were removed."
    #       ),
    #       type = "warning",
    #       duration = 6
    #     )
    #   }
    #
    #   print("Data FORM Reactive\n")
    #   print(str(data_form_df))
    #   return(data_form_df)
    # })




    # set reference group and filter by selected transformation
    data_form_reference <- reactive({
      req(data_form_reactive())
      req(reference_arm, input$transformationTypeSelection, input$featureSelection, input$antigenSampleSelection)
      data_form_ref_df <- set_reference_arm_transform_type(data_form = data_form_reactive(), arm_ref_group = reference_arm ,
                                                           selected_transformation = input$transformationTypeSelection,
                                                           selected_antigen = input$antigenSampleSelection,
                                                           selected_feature = input$featureSelection)
      return(data_form_ref_df)
    })

    output$data_form_table <- renderTable({
      req(data_form_reference())
      return(data_form_reference())
    })

    ## Compute the finite mixture model
    finite_mixture_model <- reactive({
      req(data_form_reference())
      req(nrow(data_form_reference()) >0)
      req(visit1,visit2)


      mixture_model <- compute_finite_mixture_model(data_form_reference = data_form_reference(),
                                                    t0 = visit1,
                                                    t1 = visit2)
      return(mixture_model)
    })

    output$data_form_table <- renderTable({
      req(finite_mixture_model())
      req(visit1)
      filtered_data <- finite_mixture_model()[finite_mixture_model()$timeperiod == visit1,]
      return(filtered_data)
    })

    # number of unique clusters
    n_unique_clusters_reactive <- reactive({
      req(finite_mixture_model())
      req(visit1)
      # filter the data by visit and obtain the unique clusters
      filtered_data <- finite_mixture_model()[finite_mixture_model()$timeperiod == visit1,]
      unique_clusters <- max(unique(filtered_data$clusters))
      return(unique_clusters)
    })

    output$cluster_value <- renderPrint({
      n_unique_clusters_reactive() # Access the reactive value
    })

    #difres clustering reactive for visit diff, either a list of 2 or a df
    difres_reactive <- reactive({
      req(data_form_reference())
      req(visit1, visit2)
      #input$readxMap_experiment_accession
      req(input$featureSelection, input$antigenSampleSelection)

      id <- showNotification(
        HTML("Loading Subgroup Detection<span class = 'dots'>"),
        type = "message",
        duration = NULL
      )

      on.exit(removeNotification(id), add = TRUE)
      difres <- obtain_difres_clustering(data_form_reference_in = data_form_reference(), t0 = visit1, t1 = visit2,
                                         selected_feature = input$featureSelection,
                                         selected_antigen = input$antigenSampleSelection)
      #if (length(difres) == 2) {
      #   return(difres)
      # } else {
      return(difres)

    })

    ## obtain the method to determine the number of clusters, k
    k_selection_method <- reactive({
      req(difres_reactive())
      cluster_method <- names(difres_reactive()[[2]])
      if (cluster_method == "silhoute") {
        display_method <- "The Silhouette Score was used to select the optimal number of clusters."
      } else if (cluster_method == "gap") {
        display_method <- "The Gap Statistic was used to select the optimal number of clusters."
        # } else if (cluster_method == "silhouette_gap_equal") {
        #     display_method <- "The Silhouette Score and Gap Statistic selected the same number of optimal clusters."
      } else if (cluster_method == "Not_enough_unique_subjects") {
        display_method <- "1 Cluster due to less than 3 subjects (exclusive)"
      } else {
        display_method <- "1 cluster due to the maxinum cluster being above 4"
      }

      return(display_method)
    })

    output$k_selection_display <- renderUI({
      req(k_selection_method())
      tags$span(
        style = "font-size: 18px;",
        k_selection_method()
      )
    })



    # datsub reactive
    datsub_reactive <- reactive({
      req(difres_reactive())
      req(finite_mixture_model())
      req(visit1, visit2)
      datsub <- create_datsub(difres_in = difres_reactive(), daysset_in = finite_mixture_model(),
                              t0 = visit1, t1 = visit2)
      return(datsub)

    })


    ## Plotting

    # Density Histogram
    output$density_histogram_UI <- renderPlotly({
      req(finite_mixture_model())
      req(visit1)
      req(n_unique_clusters_reactive())
      cat("before filtering data in histgoram")
      finite_mixture_model_df_v <- finite_mixture_model()
      filtered_data <- finite_mixture_model()[finite_mixture_model()$timeperiod == visit1,]
      cat("before plotting histogram")
      density_histogram(day0set = filtered_data, n_clusters = n_unique_clusters_reactive())

    })

    # Download density histogram data
    output$download_density_histogram_first_visit <- renderUI({
      req(finite_mixture_model())
      req(visit1)
      req(n_unique_clusters_reactive())
      req(input$antigenSampleSelection)
      req(input$transformationTypeSelection)
      # first_visit_data <- finite_mixture_model()[finite_mixture_model()$visit_name == visit1,]
      #
      # download_first_visit_class(download_df = first_visit_data,
      #                            selected_transformation = input$transformationTypeSelection,
      #                            selected_antigen = input$antigenSampleSelection,
      #                            selected_feature = input$featureSelection)
      button_label <-  paste0("Download ", input$transformationTypeSelection," First Visit Class Data for ",input$antigenSampleSelection, " in ",input$featureSelection)
      downloadButton("download_density_histogram_first_visit_handle", button_label)
    })

    output$download_density_histogram_first_visit_handle <- downloadHandler(
      filename = function() {
        paste0(input$antigenSampleSelection, "_",input$featureSelection, "-", input$transformationTypeSelection, "_first_visit_class_data.csv")
      },
      content = function(file) {
        req(finite_mixture_model())
        req(visit1)
        req(n_unique_clusters_reactive())
        req(input$antigenSampleSelection)
        req(input$transformationTypeSelection)
        first_visit_data <- finite_mixture_model()[finite_mixture_model()$timeperiod == visit1,]


        # download data component (data frame)
        write.csv(first_visit_data, file, row.names = FALSE)
      }
    )



    # Visit Difference
    output$visit_difference_UI <- renderPlotly({
      req(difres_reactive())
      req(visit1, visit2)
      req(input$featureSelection, input$antigenSampleSelection)

      diff_plot <- visit_difference(difres_input = difres_reactive(), t0 = visit1, t1 = visit2,
                                    selected_feature = input$featureSelection,
                                    selected_antigen = input$antigenSampleSelection)

      return(diff_plot)
    })

    # download visit difference plot data
    output$download_visit_difference <- renderUI({
      req(difres_reactive())
      req(visit1, visit2)
      req(input$featureSelection, input$antigenSampleSelection)
      req(input$transformationTypeSelection)

      button_label <-  paste0("Download ", input$transformationTypeSelection," Visit difference (",visit1, " - ",visit2, ") Data for ",input$antigenSampleSelection, " in ", input$featureSelection)
      downloadButton("download_visit_difference_handle", button_label)

      # res <- difres_reactive()[[1]]$res_wide
      # download_visit_difference(download_df = res,
      #                           selected_transformation = input$transformationTypeSelection,
      #                           selected_antigen = input$antigenSampleSelection ,
      #                           selected_feature = input$featureSelection,
      #                           t0 = visit1,
      #                           t1 = visit2
      # )

    })

    output$download_visit_difference_handle <- downloadHandler(
      filename = function() {
        paste0(input$antigenSampleSelection, "_",input$featureSelection, "-", input$transformationTypeSelection, "visit_difference", visit1, "_", visit2, ".csv")
      },
      content = function(file) {
        req(difres_reactive())
        req(visit1, visit2)
        req(input$featureSelection, input$antigenSampleSelection)
        req(input$transformationTypeSelection)
        res <- difres_reactive()[[1]]$res_wide

        # download data component (data frame)
        write.csv(res, file, row.names = FALSE)
      }
    )




    # Difference histogram
    output$difference_histogram_UI <- renderPlotly({
      req(difres_reactive())
      req(input$featureSelection, input$antigenSampleSelection)

      difference_histogram_plot <- difference_histogram(difres = difres_reactive(),
                                                        selected_feature = input$featureSelection,
                                                        selected_antigen = input$antigenSampleSelection )
      return(difference_histogram_plot)
    })

    # download difference histogram data
    output$download_difference_histogram_data <- renderUI({
      req(difres_reactive())
      req(input$featureSelection, input$antigenSampleSelection)
      req(input$transformationTypeSelection)
      button_label <-  paste0("Download ", input$transformationTypeSelection," K-Means Direction Data for ",input$antigenSampleSelection, " in ",input$featureSelection)
      downloadButton("download_difference_histogram_data_handle", button_label)


      # res <- difres_reactive()[[1]]$res_wide
      # download_difference_histogram_data(download_df = res,
      #                                    selected_transformation = input$transformationTypeSelection ,
      #                                    selected_antigen = input$antigenSampleSelection,
      #                                    selected_feature = input$featureSelection)

    })

    output$download_difference_histogram_data_handle <- downloadHandler(
      filename = function() {
        paste0(input$antigenSampleSelection, "_",input$featureSelection, "-",input$transformationTypeSelection , "_visit_difference_kmeans_data.csv")
      },
      content = function(file) {
        req(difres_reactive())
        req(input$featureSelection, input$antigenSampleSelection)
        req(input$transformationTypeSelection)
        res <- difres_reactive()[[1]]$res_wide

        # download data component (data frame)
        write.csv(res, file, row.names = FALSE)
      }
    )


    # Assay Classification
    output$assay_classification_UI <- renderPlotly({
      req(datsub_reactive())
      req(input$responseSelection)
      #input$readxMap_experiment_accession
      req(input$featureSelection, input$antigenSampleSelection)
      req(visit1, visit2)
      assay_classification_plot <- plot_assay_classification(datsub_df = datsub_reactive(),
                                                             selected_antigen = input$antigenSampleSelection,
                                                             selected_feature = input$featureSelection,
                                                             log_assay_outcome = input$responseSelection,
                                                             visit1 = visit1,
                                                             visit2 = visit2)
      return(assay_classification_plot)
    })

    # download assay classification data
    output$download_assay_classification_data <- renderUI({
      req(datsub_reactive())
      req(input$responseSelection)
      req(input$transformationTypeSelection)
      #nput$readxMap_experiment_accession
      req(input$featureSelection, input$antigenSampleSelection)

      button_label <-  paste0("Download ", input$transformationTypeSelection," Visit Plot Data for ",input$antigenSampleSelection, " in ", input$featureSelection)
      downloadButton("downlod_assay_classification_data_handle", button_label)

      # download_assay_classification_data(download_df = datsub_reactive(),
      #                                    selected_transformation = input$transformationTypeSelection,
      #                                    selected_antigen = input$antigenSampleSelection,
      #                                    selected_feature = input$featureSelection)
    })

    output$downlod_assay_classification_data_handle <-  downloadHandler(
      filename = function() {
        clean_input <- trimws(input$transformation_type_selection)
        safe_transformation <- gsub("[, ]+", "_", clean_input)

        #safe_transformation  <- gsub("[, ]+", "_", input$transformation_type_selection)

        paste0(input$antigenSampleSelection, "_",input$featureSelection, "_", safe_transformation, "_visit_plot_data.csv")
      },
      content = function(file) {
        req(datsub_reactive())
        req(input$responseSelection)
        req(input$transformationTypeSelection)

        # download data component (data frame)
        write.csv(datsub_reactive(), file, row.names = FALSE)
      }
    )


  } else {# in Subgroup Detection tab
    output$subgroupDetectionUI <- NULL
    output$parameter_subgroup_dependencies_UI <- renderUI(NULL)
    output$selectedFeatureUI <- renderUI(NULL)
    output$response_class <- renderUI(NULL)
    output$transformation_type <- renderUI(NULL)
    output$selectedAntigenSampleUI <- renderUI(NULL)
    output$density_histogram_UI <- renderPlotly(NULL)
    output$download_density_histogram_first_visit <- renderUI(NULL)
    output$show_time_dependent_plots <- renderUI(NULL)
    output$k_selection_display <- renderUI(NULL)
    output$visit_difference_UI <- renderPlotly(NULL)
    output$download_visit_difference <- renderUI(NULL)
    output$difference_histogram_UI <- renderPlotly(NULL)
    output$download_difference_histogram_data <- renderUI(NULL)
    output$assay_classification_UI <- renderPlotly(NULL)
    output$download_assay_classification_data <- renderUI(NULL)

  }

})



