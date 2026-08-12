#observeEvent(list(input$inLoadedData, input$readxMap_experiment_accession), {
 # req(input$inLoadedData, input$readxMap_experiment_accession)
observeEvent(list(
  input$readxMap_experiment_accession,
  input$readxMap_study_accession,
  input$advanced_qc_component,
  input$study_level_tabs,
  input$main_tabs), {

    req(input$advanced_qc_component == "Subgroup Detection Summary",
        input$readxMap_study_accession != "Click here",
        input$readxMap_experiment_accession != "Click here",
        input$study_level_tabs == "Experiments",
        input$main_tabs == "view_files_tab")

  if (input$advanced_qc_component == "Subgroup Detection Summary") {

    selected_study <- selected_studyexpplate$study_accession
    selected_experiment <- selected_studyexpplate$experiment_accession

    # Load sample data
    #sample_data <- stored_plates_data$stored_sample
    # sample_data <- fetch_best_sample_se_all(study_accession = selected_study, experiment_accession = selected_experiment,
    #                                         project_id = userWorkSpaceID(), conn = conn)
    sample_data <- fetch_best_sample_se_all_summary(study_accession = selected_study, experiment_accession = selected_experiment,
                                                    param_user = currentuser(), project_id = userWorkSpaceID(), conn = conn)

    # # Check if selected study, experiment, and sample data are available
    # if (!is.null(selected_study) && length(selected_study) > 0 &&
    #     !is.null(selected_experiment) && length(selected_experiment) > 0 &&
    #     !is.null(sample_data) && length(sample_data) > 0){
    #
    #   # Filter sample data
    #   sample_data$selected_str <- paste0(sample_data$study_accession, sample_data$experiment_accession)
    #   sample_data <- sample_data[sample_data$selected_str == paste0(selected_study, selected_experiment), ]
    #
    #   # Summarize sample data
    #   cat("Viewing sample dat in subgroup summary tab ")
    #   print(names(sample_data))
    #   print(table(sample_data$plateid))
    #   print(table(sample_data$antigen))
    #   cat("After summarizing sample data in subgroup summary tab")
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
    # filter out columns which are not antigens
    sample_data <- sample_data[!(sample_data$antigen %in% colnames(header_data)), ]
    sample_data <- sample_data[!(sample_data$antigen %in% luminex_features()), ]

    ## Load study configuration
    study_configuration <- fetch_study_configuration(db_pool, study = selected_study, project_id = userWorkSpaceID())

    reference_arm <- study_configuration[study_configuration$param_name == "reference_arm",]$param_character_value
    primary_timeperiod_comparison <- study_configuration[study_configuration$param_name == "primary_timeperiod_comparison",]$param_character_value
    baseline_visit <- strsplit(primary_timeperiod_comparison, split = ",")[[1]][1]
    followup_visit <- strsplit(primary_timeperiod_comparison, split = ",")[[1]][2]
    timeperiod_order <- study_configuration[study_configuration$param_name == "timeperiod_order",]$param_character_value


    output$subgroup_summary_UI <- renderUI({
      req(study_configuration)
      tagList(
        fluidRow(
          column(12,
                 bsCollapse(
                   id = "subgroupSummaryMethods",
                   bsCollapsePanel(
                     title = "Subgroup Summary Methods",
                     tagList(
                       tags$p("Use the dropdown menu titled 'Response type' to choose the desired type of outcome measured.
                              Currently the options include MFI, Normalized MFI and Concentration.
                              Use the dropdown menu to select the desired transformation that is applied to the data.
                              Select the first and second visits of interest from the dropdown menu titled 'First Visit' and 'Second Visit respectively.
                              To change the reference arm selection navigate to the 'Study Overview' tab and to the Set Reference Arm settings.
                              After selecting the desired options click 'Summarize Subgroups'."),
                       tags$p("Once the summarization calculations are complete, a heatmap is displayed.
                              This heatmap clusters the subjects and measures response level into three categories: low, medium, and high with associated values of 1, 2, and 3.
                              To select the number of subgroups slide the slider labeled 'Number of Subgroups' to the desired number of subgroups.
                              The allowed range is 1 to 6 subgroups, or up to the number of subjects if there are fewer than 6 subjects."),
                       tags$p("To download the combined data for all antigens in the currently selected experiment,
                              which is used for the heatmap, click the download button button below the heatmap.")
                     ), # end tagList
                     style = "success"
                   ) # end bsCollapsePanel
                 ),
                 mainPanel(
                   div(
                     style = "background-color: #f0f8ff; border: 1px solid #4a90e2;
                              padding: 10px; margin-bottom: 15px; border-radius: 5px;",
                     tags$h4("Current Subgroup Detection Summary Context", style = "margin-top: 0; color: #2c5aa0;"),
                     uiOutput("parameter_subgroup_summary_dependencies_UI"),
                   ),
                   # fluidRow(
                   #   column(3, uiOutput("feature_selectionUI")),
                   #   column(3, textOutput("study_antigens"))
                   # ),
                   fluidRow(
                     column(3, uiOutput("feature_selectionUI")),
                     column(3,uiOutput("response_class_selection")),
                     column(3, uiOutput("transformation_type_selection")),
                     # column(3, uiOutput("baseline_visit")),
                     # column(3, uiOutput("followup_visit"))
                   ),
                   # fluidRow(
                   #   column(3, uiOutput("reference_arm_selection_UI"))
                   # ),
                   br(),
                   uiOutput("run_summarization_UI"),
                   #  verbatimTextOutput("mixture_model_summary"),
                   # verbatimTextOutput("data_form_reference_summary_view"),
                   # verbatimTextOutput("difres_reactive_summary_view"),
                   #verbatimTextOutput("difres_reactive_summary_view"),
                   br(),
                   # verbatimTextOutput("mixture_model_summary"),
                   # br(),
                   #verbatimTextOutput("combinded_datsub_view"),
                   # tableOutput("my_table"),

                   # Once available it shows.

                   #verbatimTextOutput("categorical_matrix"),
                   #verbatimTextOutput("continuous_matrix"),
                   uiOutput("num_subgroups_option_UI"),
                   #plotOutput("hclust_heatmap", width = "75vw"),
                   plotlyOutput("hclust_heatmap_heatmaply", width = "75vw", height = "100vh"),
                   br(),
                   uiOutput("download_combined_assay_classification_data")
                 ) # end main Panel
          ) # end column
        )# end fluid Row
      ) # end tagList
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

    output$feature_selectionUI <- renderUI({
      req(sample_data)
      radioButtons("feature_selection",
                   label = "Feature",
                   choices = unique(sample_data$feature)
      )
    })

    ## Box noting parameter dependencies
    output$parameter_subgroup_summary_dependencies_UI <- renderUI({
      # tagList(bsCollapse(
      #   id = "param_subgroup_summary_dependencies",
      #   bsCollapsePanel(
      #     title = "Parameter Dependencies",
          HTML(
            "The subgroup summary depends on the first (baseline) and second (followup) timepoints are set in the subgroup detection section within the study paramaters.
             Normalized MFI is only available when normalized values have been computed for all antigens for the selected feature.
             For subject–antigen pairs, concentration values falling outside the standard curve prediction range were rendered in white."
           )
      #     style = "info"
      #   )
      # ),
      # actionButton("to_study_parameters_from_subgroup_summary", label = "Return to Study Parameters")

    })

    # Switch tabs when click button
    observeEvent(input$to_study_parameters_from_subgroup_summary, {
      updateTabsetPanel(session, inputId = "study_level_tabs", selected = "Study Parameters")
    })
    # output$baseline_visit <- renderUI({
    #   req(visits)
    #   selectInput(
    #     inputId = "baselineVisitSelection",
    #     label = "First Visit",
    #     choices = visits
    #   )
    # })
    # output$followup_visit <- renderUI({
    #   req(visits)
    #   selectInput(
    #     inputId = "followupVisitSelection",
    #     label = "Second Visit",
    #     choices = visits
    #   )
    # })

    output$response_class_selection <- renderUI({
      req(sample_data)

      #sample_data_v_class <<- sample_data

      choices <- c("MFI", "Concentration")

      if (any(!is.na(sample_data$norm_assay_response))) {
        choices <- c("MFI", "Normalized MFI", "Concentration")
      }
      selectInput(
        inputId = "responseClassSelection",
        label = "Response type:",
        choices = choices
      )
    })

    output$study_antigens <- renderText({
      req(sample_data)
      return(unique(sample_data$antigens))
    })
    # output$reference_arm_selection_UI <- renderUI({
    #   req(arm_choices)
    #   selectInput(
    #     inputId = "referenceArmSelection_2",
    #     label = "Reference Arm",
    #     choices = arm_choices
    #   )
    # })
    output$transformation_type_selection <- renderUI(
      selectInput(
        inputId = "transformation_type_selection",
        label = "Transformation",
        choices =  c("Unstandardized, not scaled","Unstandardized, scaled")
      )
    )
    output$run_summarization_UI <- renderUI({
      req(baseline_visit, followup_visit)
    #  req(baseline_visit, followup_visit)
      if (baseline_visit != followup_visit) {
        actionButton("run_summarization", "Summarize Subgroups")
      } else {
        HTML(paste0("<span style='font-size:20px;'>The first and second visits must be different to proceed to summarize subgroups."))
      }

    })
    observeEvent(input$run_summarization, {
      # withProgress(message = "Processing Subgroup Summariation", value = 0, {
      cat("Button pressed! Starting summarization...\n")
      ## Reactive data form
      data_form_reactive_summary <- eventReactive(input$run_summarization, {
        req(sample_data)
        req(baseline_visit, followup_visit, input$responseClassSelection)
        data_form_df <- create_data_form_df(data = sample_data,
                                            t0 = baseline_visit,
                                            t1 = followup_visit,
                                            log_assay_outcome = input$responseClassSelection)
        cat("after data form is created")
        return(data_form_df)
      })
      #incProgress(1/6, detail = "Filtered data based on inputs.")

      # set reference group and filter by selected transformation
     # input$referenceArmSelection_2
      data_form_reference_summary <- eventReactive(input$run_summarization, {
        req(data_form_reactive_summary())
        req(reference_arm, input$transformation_type_selection, input$feature_selection)
        unique_antigens <- unique(sample_data$antigen)
        data_form_ref_df_list <- list()
        for (antigen in unique_antigens) {
          data_form_ref_df_list[[antigen]] <- set_reference_arm_transform_type(
            data_form = data_form_reactive_summary(),
            arm_ref_group = reference_arm,#input$referenceArmSelection_2,
            selected_transformation = input$transformation_type_selection,
            selected_antigen = antigen,  # Looping through all antigens
            selected_feature = input$feature_selection
          )
        }
        return(data_form_ref_df_list)
      })
      showNotification("Assigned reference arm and selected transformation for all antigens",
                       type = "message", duration = 3,
                       id = "refrenceArmMessage")

     # incProgress(1/6, detail = "Assigned reference arm and selected transformation for all antigens")

      output$data_form_reference_summary_view <- renderPrint({
        data_form_reference_summary()
      })

      # clustering
      difres_reactive_summary <- eventReactive(input$run_summarization,{
        req(data_form_reference_summary())
        req(baseline_visit, followup_visit)
        req(input$feature_selection)

        notification_id <- showNotification("Begining K-Means Clustering for antigens...",
                                            type = "message",
                                            duration = NULL,
                                            id = "antigen_kmeans_process")

        unique_antigens <- unique(sample_data$antigen)
        n_antigen <- length(unique_antigens)
        difres_list <- list()
        antigen_count <- 1
        for (antigen in unique_antigens) {
          showNotification(paste("Processing K-means clustering for antigen ", antigen_count, "/",n_antigen , "(", antigen, ")"), type = "message", duration = NULL, id = "antigen_kmeans_process")

          difres_list[[antigen]] <- obtain_difres_clustering(data_form_reference_in = data_form_reference_summary()[[antigen]],
                                                             t0 = baseline_visit,
                                                             t1 = followup_visit,
                                                             selected_feature = input$feature_selection,
                                                             selected_antigen = antigen)
          antigen_count <- antigen_count + 1
        }

        return(difres_list)
      })
      showNotification("K-Means clustering completed", type = "message", duration = 3, id = "antigen_kmeans_process")
      shinyjs::delay(2000, removeNotification("antigen_kmeans_process"))


      #incProgress(1/6, detail = "K-Means clustering complete for all antigens")
      output$difres_reactive_summary_view <- renderPrint({
        difres_reactive_summary()
      })


      finite_mixture_model_summary <- eventReactive(input$run_summarization, {
        req(data_form_reference_summary())
        req(baseline_visit, followup_visit)
        unique_antigens <- unique(sample_data$antigen)
        n_antigen <- length(unique_antigens)
        mixture_model_list <- list()
        notification_mixture_model_id <- showNotification("Begining Finite Mixture Models for antigens...",
                                                          type = "message",
                                                          duration = NULL,
                                                          id = "mixtureModelNotification")
        antigen_count <- 1
        for (antigen in unique_antigens){
          showNotification(paste("Processing Finite Mixture Model for antigen", antigen_count, " / ", n_antigen, "(", antigen, ")"), type = "message", duration = NULL, id = "mixtureModelNotification")
          mixture_model_list[[antigen]] <- compute_finite_mixture_model(data_form_reference = data_form_reference_summary()[[antigen]],
                                                                        t0 = baseline_visit,
                                                                        t1 = followup_visit)
          antigen_count <- antigen_count + 1
        }
        return(mixture_model_list)
      })
      showNotification("Finite Mixture Model complete for all antigens.",
                       type = "message", duration = 3,
                       id = "mixtureModelNotification")
      shinyjs::delay(2000, removeNotification("mixtureModelNotification"))

      # incProgress(1/6, detail = "Finite Mixture Model complete for all antigens")


      output$mixture_model_summary <- renderPrint({
        finite_mixture_model_summary()
      })

      # datsub reactive
      datsub_reactive_summary <- eventReactive(input$run_summarization,{
        req(difres_reactive_summary())
        req(finite_mixture_model_summary())
        req(baseline_visit, followup_visit)
        unique_antigens <- unique(sample_data$antigen)
        n_antigen <- length(unique_antigens)
        datsub_list <- list()
        notification_final_df_id <- showNotification(paste("Begining final data processing for all ", n_antigen, " antigens."),
                                                     type = "message",
                                                     duration = NULL,
                                                     id = "appendNotification")
        antigen_count <- 1
        for (antigen in unique_antigens) {
          showNotification(paste("Appending antigen", antigen_count, "/", n_antigen, "(", antigen, ") to final dataframe."), type = "message", duration = NULL, id = "appendNotification")
          datsub <- create_datsub(difres_in = difres_reactive_summary()[[antigen]],
                                  daysset_in = finite_mixture_model_summary()[[antigen]],
                                  t0 = baseline_visit,
                                  t1 = followup_visit)
          datsub$antigen <- antigen

          datsub_list[[antigen]]  <- datsub
          antigen_count <- antigen_count + 1
        }

        combined_datsub <- do.call(rbind, datsub_list)
        #combined_datsub <- do.call(rbind, Filter(Negate(is.null), datsub_list))

        return(combined_datsub)

      })
      # showNotification("Final data processing complete for all antigens.",
      #                  type = "message", duration = NULL,
      #                  id = "appendNotification")
      shinyjs::delay(2000, removeNotification("appendNotification"))

      # output$combinded_datsub_view <- renderPrint({
      #   datsub_reactive_summary()
      # })
      # incProgress(1/6, detail = "Final data processing complete")

      output$download_combined_assay_classification_data<- renderUI({
        req(datsub_reactive_summary())
        req(input$transformation_type_selection)
        req(input$feature_selection)
        # Get the full combined dataset
        # download_df <- datsub_reactive_summary()
        #
        #
        # # download button for the entire dataset
        # download_plot_data <- download_this(
        #   download_df,
        #   output_name = paste0("combined_", input$transformation_type_selection,"_",input$feature_selection, "_visit_plot_data"),
        #   output_extension = ".xlsx",
        #   button_label = paste0("Download ",input$feature_selection, ",", input$transformation_type_selection, " Visit Data"),
        #   button_type = "warning",
        #   icon = "fa fa-save",
        #   class = "hvr-sweep-to-left"
        # )

       # return(download_plot_data)
        button_label <- paste0("Download ",input$feature_selection, ",", input$transformation_type_selection, " Visit Data")
        downloadButton("download_combined_assay_classification_data_handle", button_label)

      })

      output$download_combined_assay_classification_data_handle <- downloadHandler(
        filename = function() {
          paste0("combined_", input$transformation_type_selection,"_",input$feature_selection, "_visit_plot_data.csv")

        },
        content = function(file) {
          req(datsub_reactive_summary())
          req(input$transformation_type_selection)
          req(input$feature_selection)

          # download data component (data frame)
          write.csv(datsub_reactive_summary(), file, row.names = FALSE)
        }
      )


      incProgress(1/6, detail = "Download button rendered")

      cat("Subgroup Summarization Completed")
      # }) #end withProgress

      output$combinded_datsub_view <- renderPrint({
        datsub_reactive_summary()
      })

      classify_set <- eventReactive(input$run_summarization, {
        req(datsub_reactive_summary())
        req(baseline_visit, followup_visit)
        classify_set <- prepare_classify_set(classify_set = datsub_reactive_summary(),
                                             baseline_visit = baseline_visit,
                                             followup_visit = followup_visit)

        classify_set <- create_categorized_subgroups(classify_set = classify_set)
        return(classify_set)
      })

      categorical_heatmap_matrix <- eventReactive(input$run_summarization, {
        req(classify_set())
        req(baseline_visit)
        categorical_heatmap_matrix <- create_catigorical_heatmap_matrix(classify_set = classify_set(),
                                                                        baseline_visit = baseline_visit)
        return(categorical_heatmap_matrix)
      })

      output$categorical_matrix <- renderPrint({
        categorical_heatmap_matrix()
      })

      ## Prepare continous heatmap matrix
      classify_set_visit <- eventReactive(input$run_summarization,{
        req(classify_set())
        classified_set_visit <- create_classifed_set_visit(classify_set())
        return(classified_set_visit)
      })
      # continuous heatmap matrix
      continuous_heatmap_matrix <- eventReactive(input$run_summarization,{
        req(classify_set_visit())
        continuous_matrix <- create_continuous_heatmap_matrix(classify_set_visit())
        return(continuous_matrix)
      })

      output$continuous_matrix  <- renderPrint({
        continuous_heatmap_matrix()
      })

      # baseline symbols
      baseline_symbols <- eventReactive(input$run_summarization,{
        req(baseline_visit)
        req(continuous_heatmap_matrix())
        req(classify_set())
        baseline_symbols <- create_baseline_symbols(classify_set = classify_set(),
                                                    baseline_visit = baseline_visit,
                                                    continuous_heatmap_matrix = continuous_heatmap_matrix())
        return(baseline_symbols)
      })

      # hierarchical clustering
      hierarchical_clustering <- eventReactive(input$run_summarization,{
        req(continuous_heatmap_matrix(), categorical_heatmap_matrix())
        req(baseline_symbols())
        hclust_result <- preform_heirachial_clustering(continuous_heatmap_matrix = continuous_heatmap_matrix(),
                                                       categorical_heatmap_matrix = categorical_heatmap_matrix())
        return(hclust_result)
      })

      # Select the Number of subgroups (range is 1 to 6 or to the number of subjects if less than 6)
      output$num_subgroups_option_UI <- renderUI({
        req(hierarchical_clustering()) # require the result of hierarchical clustering to not show until button completes
        req(sample_data)

        sliderInput(
          inputId = "num_subgroups",
          label = "Number of Subgroups",
          value = 1,
          min = 1,
          max = ifelse(length(unique(sample_data$patientid)) < 6, length(unique(sample_data$patientid)), 6),
          step = 1
        )
      })

      output$hclust_heatmap <- renderPlot({
        req(hierarchical_clustering())
        req(categorical_heatmap_matrix())
        req(baseline_symbols())
        req(input$num_subgroups)

        row_clust <- hierarchical_clustering()[[1]]
        col_clust <- hierarchical_clustering()[[2]]

        row_annotation <- data.frame(Subgroup = factor(cutree(row_clust, input$num_subgroups)))

        subject_feature_heatmap <- plot_heatmap_hclust(heatmap_matrix_in = categorical_heatmap_matrix(),
                                                       num_subgroups_option_selected_in = input$num_subgroups,
                                                       symbols_in = baseline_symbols(),
                                                       row_annotation_in = row_annotation,
                                                       row_clust_in = row_clust,
                                                       col_clust_in = col_clust)
        return(subject_feature_heatmap)
      })


      output$hclust_heatmap_heatmaply <- renderPlotly({
        req(hierarchical_clustering())
        req(categorical_heatmap_matrix())
        req(baseline_symbols())
        req(input$num_subgroups)

        row_clust <- hierarchical_clustering()[[1]]
        col_clust <- hierarchical_clustering()[[2]]
        keep_rows <- hierarchical_clustering()[[3]]

        row_annotation <- data.frame(Subgroup = factor(cutree(row_clust, input$num_subgroups)))

        subject_antigen_heatmap <- plot_heatmap_hclust_heatmaply(heatmap_matrix_in = categorical_heatmap_matrix(),
                                     num_subgroups_option_selected_in = input$num_subgroups,
                                     symbols_in = baseline_symbols(),
                                     row_annotation_in = row_annotation,
                                     row_clust_in = row_clust,
                                     col_clust_in = col_clust,
                                     keep = keep_rows)

        return(subject_antigen_heatmap)
      })
    }) # end observe event for button

  } else {
    output$subgroup_summary_UI <- NULL
    output$parameter_subgroup_summary_dependencies_UI <- renderUI(NULL)
    output$feature_selectionUI <- renderUI(NULL)
    output$study_antigens <- renderText(NULL)
    output$response_class_selection <- renderUI(NULL)
    output$transformation_type_selection <- renderUI(NULL)
    output$run_summarization_UI <- renderUI(NULL)
    output$num_subgroups_option_UI <- renderUI(NULL)
    output$hclust_heatmap_heatmaply <- renderPlotly(NULL)
    output$download_combined_assay_classification_data <- renderUI(NULL)
  }
})
