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
observeEvent(input$antigen_family_table_cell_edit, {
  info <- input$antigen_family_table_cell_edit
  row_num <- info$row
  col_num <- info$col
  new_value <- info$value

  current_data <- antigen_families_rv()
  col_name <- colnames(current_data)[col_num]
  row_id <- current_data$xmap_antigen_family_id[row_num]

  message("Edited column:", col_name, " | New value:", new_value)

  if (col_name == "antigen_family") {
    new_value <- as.character(new_value)
    update_query <- "UPDATE madi_results.xmap_antigen_family
                     SET antigen_family = $1 WHERE xmap_antigen_family_id = $2"

    tryCatch({
      dbExecute(conn, update_query, params = list(new_value, row_id))
      current_data$antigen_family[row_num] <- new_value
      antigen_families_rv(current_data)
      showNotification("antigen_family updated successfully", type = "message")
    }, error = function(e) {
      showNotification(paste("Error updating antigen_family:", e$message), type = "error")
    })

  } else if (col_name == "standard_curve_concentration") {
    new_value <- as.numeric(new_value)
    update_query <- "UPDATE madi_results.xmap_antigen_family
                     SET standard_curve_concentration = $1 WHERE xmap_antigen_family_id = $2"

    tryCatch({
      dbExecute(conn, update_query, params = list(new_value, row_id))
      current_data$standard_curve_concentration[row_num] <- new_value
      antigen_families_rv(current_data)
      showNotification("standard_curve_concentration updated successfully", type = "message")
    }, error = function(e) {
      showNotification(paste("Error updating standard_curve_concentration:", e$message), type = "error")
    })
  } else if (col_name == "antigen_name") {
      new_value <- as.character(new_value)
      update_query <- "UPDATE madi_results.xmap_antigen_family
                     SET antigen_name = $1 WHERE xmap_antigen_family_id = $2"
      tryCatch({
        dbExecute(conn, update_query, params = list(new_value, row_id))
        current_data$antigen_name[row_num] <- new_value
        antigen_families_rv(current_data)
        showNotification("antigen_name updated successfully", type = "message")
      }, error = function(e) {
        showNotification(paste("Error updating antigen_name:", e$message), type = "error")
      })
  } else if (col_name == "virus_bacterial_strain") {
    new_value <- as.character(new_value)
    update_query <- "UPDATE madi_results.xmap_antigen_family
                     SET virus_bacterial_strain = $1 WHERE xmap_antigen_family_id = $2"
    tryCatch({
      dbExecute(conn, update_query, params = list(new_value, row_id))
      current_data$virus_bacterial_strain[row_num] <- new_value
      antigen_families_rv(current_data)
      showNotification("virus_bacterial_strain updated successfully", type = "message")
    }, error = function(e) {
      showNotification(paste("Error updating virus_bacterial_strain:", e$message), type = "error")
    })
  } else if (col_name == "antigen_source") {
    new_value <- as.character(new_value)
    update_query <- "UPDATE madi_results.xmap_antigen_family
                     SET antigen_source = $1 WHERE xmap_antigen_family_id = $2"
    tryCatch({
      dbExecute(conn, update_query, params = list(new_value, row_id))
      current_data$antigen_source[row_num] <- new_value
      antigen_families_rv(current_data)
      showNotification("antigen_source updated successfully", type = "message")
    }, error = function(e) {
      showNotification(paste("Error updating antigen_source:", e$message), type = "error")
    })
  } else if (col_name == "catalog_number") {
    new_value <- as.character(new_value)
    update_query <- "UPDATE madi_results.xmap_antigen_family
                     SET catalog_number = $1 WHERE xmap_antigen_family_id = $2"
    tryCatch({
      dbExecute(conn, update_query, params = list(new_value, row_id))
      current_data$catalog_number[row_num] <- new_value
      antigen_families_rv(current_data)
      showNotification("catalog_number updated successfully", type = "message")
    }, error = function(e) {
      showNotification(paste("Error updating catalog_number:", e$message), type = "error")
    })
  } else if (col_name == "l_asy_min_constraint") {
    new_value <- as.numeric(new_value)
    update_query <- "UPDATE madi_results.xmap_antigen_family
                     SET l_asy_min_constraint = $1 WHERE xmap_antigen_family_id = $2"

    tryCatch({
      dbExecute(conn, update_query, params = list(new_value, row_id))
      current_data$l_asy_min_constraint[row_num] <- new_value
      antigen_families_rv(current_data)
      showNotification("l_asy_min_constraint updated successfully", type = "message")
    }, error = function(e) {
      showNotification(paste("Error updating l_asy_min_constraint:", e$message), type = "error")
    })
  } else if (col_name == "l_asy_max_constraint") {
    new_value <- as.numeric(new_value)
    update_query <- "UPDATE madi_results.xmap_antigen_family
                     SET l_asy_max_constraint = $1 WHERE xmap_antigen_family_id = $2"

    tryCatch({
      dbExecute(conn, update_query, params = list(new_value, row_id))
      current_data$l_asy_max_constraint[row_num] <- new_value
      antigen_families_rv(current_data)
      showNotification("l_asy_max_constraint updated successfully", type = "message")
    }, error = function(e) {
      showNotification(paste("Error updating l_asy_max_constraint:", e$message), type = "error")
    })
   } else if (col_name == "pcov_threshold") {
      new_value <- as.character(new_value)
      update_query <- "UPDATE madi_results.xmap_antigen_family
                       SET pcov_threshold = $1 WHERE xmap_antigen_family_id = $2"
  
      tryCatch({
        dbExecute(conn, update_query, params = list(new_value, row_id))
        current_data$pcov_threshold[row_num] <- new_value
        antigen_families_rv(current_data)
        showNotification("pcov_threshold updated successfully", type = "message")
      }, error = function(e) {
        showNotification(paste("Error updating pcov_threshold:", e$message), type = "error")
      })
    }
})

observeEvent(input$antigen_family_dropdown_edit, {
  info <- input$antigen_family_dropdown_edit
 # info_1 <<- info
  row_num <- info$row
  col_num <- info$col
  new_value <- info$value

  current_data <- antigen_families_rv()
  #current_data2 <<- current_data
  col_name <- colnames(current_data)[col_num]
  row_id <- current_data$xmap_antigen_family_id[row_num]
  #
  if (col_name == "l_asy_constraint_method") {
    new_value <- as.character(new_value)
    update_query <- "
      UPDATE madi_results.xmap_antigen_family
      SET l_asy_constraint_method = $1
      WHERE xmap_antigen_family_id = $2
    "

    tryCatch({
      dbExecute(conn, update_query, params = list(new_value, row_id))
      current_data$l_asy_constraint_method[row_num] <- new_value
      antigen_families_rv(current_data)
      showNotification("l_asy_constraint_method updated successfully", type = "message")
    }, error = function(e) {
      showNotification(paste("Error updating l_asy_constraint_method:", e$message), type = "error")
    })
  } else if (col_name == "model_form_list") {
    new_value <- as.character(new_value)
    update_query <- "
      UPDATE madi_results.xmap_antigen_family
      SET model_form_list = $1
      WHERE xmap_antigen_family_id = $2
    "
    tryCatch({
      dbExecute(conn, update_query, params = list(new_value, row_id))
      current_data$model_form_list[row_num] <- new_value
      antigen_families_rv(current_data)
      showNotification("model_form_list updated successfully", type = "message")
    }, error = function(e) {
      showNotification(paste("Error updating model_form_list:", e$message), type = "error")
    })
  }
})
# observeEvent(input$antigen_family_table_cell_edit, {
#   info <- input$antigen_family_table_cell_edit
#   row_num <- info$row
#   col_num <- info$col
#   new_value <- as.character(info$value)
#   current_data <- antigen_families_rv()
#   row_id <- current_data$xmap_antigen_family_id[row_num]
#
#   update_query <- "UPDATE madi_results.xmap_antigen_family
#                       SET antigen_family = $1
#                       WHERE xmap_antigen_family_id = $2"
#
#   tryCatch({
#     dbExecute(conn, update_query, params = list(new_value, row_id))
#
#     # Update the reactive value with new data
#     current_data$antigen_family[row_num] <- new_value
#     antigen_families_rv(current_data)
#
#     showNotification(
#       "Antigen Family updated successfully",
#       type = "message"
#     )
#   }, error = function(e) {
#     showNotification(
#       paste("Error updating antigen family:", e$message),
#       type = "error"
#     )
#   })
# })

render_study_parameters <- reactive({

#  req(input$readxMap_study_accession)
  #req(study_config)
  # Get selected study
  selected_study <-  input$readxMap_study_accession#ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)
  selected_experiment <- input$readxMap_experiment_accession
  main_tab_selected <- input$main_tabs
  cat("Study in parameters: ")
  cat(selected_study)
  cat(main_tab_selected)
  cat("experiment:")
  cat(selected_experiment)
 if (selected_study != "Click here") {
   cat("in study not click here")
  study_sources <- fetch_study_sources(study_accession = selected_study)
  study_arms <- fetch_study_arms(study_accession = selected_study)
  study_timeperiods <- fetch_study_timeperiods(study_accession = selected_study)

  # study_config <- study_config[study_config$param_group == "antigen_family",]
  # antigen_family_order_params <- study_config[study_config$param_name == "antigen_family_order",]
  # antigen_order_params <- study_config[study_config$param_name == "antigen_order",]
  #
  # antigen_family_order <- strsplit(antigen_family_order_params$param_character_value, ",")[[1]]
  # antigen_order <- strsplit(antigen_order_params$param_character_value, ",")[[1]]
  #
  # antigen_family_df <- fetch_antigen_family_table(selected_study)
  # antigen_family_df$antigen_family <- factor(antigen_family_df$antigen_family, levels = antigen_family_order)
  # antigen_family_df <- antigen_family_df[order(antigen_family_df$antigen_family), , drop = FALSE]
  #
  # antigen_family_df$antigen <- factor(antigen_family_df$antigen, levels = antigen_order)
  # antigen_family_df <- antigen_family_df[order(antigen_family_df$antigen), , drop = FALSE]
  # # Fetch data once
  # antigen_families_rv(antigen_family_df)

  # Fetch data once
  antigen_families_rv(fetch_antigen_family_table(selected_study, userWorkSpaceID(), selected_experiment))

  # Debug output
  #cat("Antigen families data updated for:", selected_study, "\n")

 study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())
  output$studyParameters_UI <- renderUI({
    # if (input$main_tabs != "home_page" & input$main_tabs != "manage_project_tab") {
    # tagList(
    # conditionalPanel(
    #   condition = "input.readxMap_study_accession == 'Click here'",
    #   HTML("<h3>Choose or create a study to change study settings.</h3>")
    # ),

  #  req(selected_study, currentuser())
    conditionalPanel(condition = "input.readxMap_study_accession != 'Click here'",
    tagList(
      HTML(paste0("<h3>Change ", selected_study, " study settings for ", currentuser(), "</h3>")),
      tabsetPanel(
        id = "study_params_section_tab",
        tabPanel(
          "QC Basic Parameters",
          radioGroupButtons(
            inputId = "basic_qc_params",
            label = "",
            choices = c("Plate Label Editor", "Bead Count Parameters", "Standard Curve Parameters" ),
            selected = "Plate Label Editor"
          ),
          conditionalPanel(
            condition = "input.basic_qc_params == 'Plate Label Editor'",
            uiOutput("plate_management_UI")
          ),
          conditionalPanel(
            condition = "input.basic_qc_params == 'Bead Count Parameters'",
            uiOutput("bead_count_config")
          ),
          conditionalPanel(
            condition = "input.basic_qc_params == 'Standard Curve Parameters'",
            uiOutput("standard_curve_config")
          )
        ),
        tabPanel(
          "Advanced Parameters",
          radioGroupButtons(
            inputId = "advanced_qc_params",
            label = "",
            choices = c("Antigen Family Parameters", "Dilution Analysis Parameters", "Subgroup Parameters"),
            selected = "Antigen Family Parameters"
          ),

          conditionalPanel(
            condition = "input.advanced_qc_params == 'Antigen Family Parameters'",
            uiOutput("antigen_family_config")),

          conditionalPanel(
            condition = "input.advanced_qc_params == 'Dilution Analysis Parameters'",
            uiOutput("dilution_analysis_config")),

          conditionalPanel(
            condition = "input.advanced_qc_params == 'Subgroup Parameters'",
            uiOutput("subgroup_config"))

          ),
          tabPanel(
            "Export/Import Study Parameters",
             uiOutput("export_import_parameters")
          )
        ),
        #)
      #uiOutput("plate_management_UI"),
      # uiOutput("bead_count_config"),
      # uiOutput("dilution_analysis_config"),
      # uiOutput("standard_curve_config"),


      # bsCollapse(
      #   id = "advanced_parameters",
      #   bsCollapsePanel(
      #   title = "Advanced Parameters",
      #   uiOutput("antigen_family_config"),
      #   uiOutput("dilution_analysis_config"),
      #   uiOutput("subgroup_config"),
      #   style = "primary")
      # ),
      conditionalPanel(
        condition = "!(input.basic_qc_params == 'Plate Label Editor' && input.study_params_section_tab == 'QC Basic Parameters') 
        && input.study_params_section_tab !== 'Export/Import Study Parameters'",
        actionButton(inputId = "reset_user_config", label = "Reset Study Parameters"),
        uiOutput("user_parameter_download")
      ),
    )
    )
    # } else {
    #   NULL
    # }
  })



  output$antigen_family_config <- renderUI({
    req(study_config)
    req(input$advanced_qc_params == 'Antigen Family Parameters')
    #study_config <- study_config_rv()
    study_config <- study_config[study_config$param_group == "antigen_family",]
    antigen_family_order_params <- study_config[study_config$param_name == "antigen_family_order",]
    antigen_order_params <- study_config[study_config$param_name == "antigen_order",]

    antigen_family_choices <- unique(antigen_families_rv()$antigen_family)
    #antigen_choices <- unique(antigen_families_rv()$antigen)
    query <- paste0("SELECT DISTINCT antigen FROM madi_results.xmap_sample
                 WHERE study_accession = '", selected_study, "'")
    sample_df  <- dbGetQuery(conn, query)
    antigen_choices <- unique(sample_df$antigen)

    antigen_family_order_val <- antigen_family_order_params$param_character_value
    if (!is.null(antigen_family_order_val) && length(antigen_family_order_val) > 0 && !all(is.na(antigen_family_order_val))) {
      default_db_antigen_family_order <- strsplit(antigen_family_order_val, ",")[[1]]
    } else {
      default_db_antigen_family_order <- antigen_family_choices

    }
    # default_db_antigen_family_order <- strsplit(antigen_family_order_params$param_character_value, ",")[[1]]
    # if (!all(default_db_antigen_family_order %in% antigen_family_choices)) {
    #   #if (!default_db_timeperiod_order %in% timeperiod_choices) {
    #   default_db_antigen_family_order <- antigen_family_choices# fallback
    # }


    # db_antigen_order_val <- antigen_order_params$param_character_value
    # if (!is.null(db_antigen_order_val) && length(db_antigen_order_val) > 0 && !all(is.na(db_antigen_order_val))) {
    #   default_db_antigen_order <- strsplit(db_antigen_order_val, ",")[[1]]
    # } else {
    #   default_db_antigen_order <- antigen_choices# fallback
    # }
    db_antigen_order_val <- antigen_order_params$param_character_value
    default_db_antigen_order <- if (!is.null(db_antigen_order_val) &&
                                    length(db_antigen_order_val) > 0 &&
                                    !all(is.na(db_antigen_order_val))) {
      db_order <- strsplit(db_antigen_order_val, ",")[[1]]
      # keep only valid antigens, then append any new ones
      unique(c(db_order[db_order %in% antigen_choices],
               setdiff(antigen_choices, db_order)))
    } else {
      antigen_choices
    }
    # default_db_antigen_order <- strsplit(antigen_order_params$param_character_value, ",")[[1]]
    # if (!all(default_db_antigen_order %in% antigen_choices)) {
    #   #if (!default_db_timeperiod_order %in% timeperiod_choices) {
    #   default_db_antigen_order <- antigen_choices# fallback
    # }
    mainPanel(
    # bsCollapse(
    #   id = "antigen_config_collapse",
    #   bsCollapsePanel(
    #     title = "Antigen Family Parameters",

      #HTML("<h4><strong>Antigen Family</strong></h4>"),
      # HTML(paste0("<h4>Order the antigen family and antigens from most important antigen family and antigen from left to right. </h4>")),
      # orderInput(
      #   inputId = antigen_family_order_params$param_name,
      #   label = antigen_family_order_params$param_label,
      #   items = default_db_antigen_family_order#unique(antigen_families_rv()$antigen_family)
      # ),
      # uiOutput("antigen_family_order_warning"),
      # orderInput(
      #   inputId = antigen_order_params$param_name,
      #   label = antigen_order_params$param_label,
      #   items = default_db_antigen_order# unique(antigen_families_rv()$antigen)
      # ),
      # uiOutput("antigen_order_warning"),
      # tags$table(
      #   tags$tr(
      #     tags$td(
      #       numericInputIcon(
      #         "study_pcov_threshold",
      #         "pCoV Threshold",
      #         value = 15,
      #         min = 0,
      #         max = 100,
      #         step = 1
      #       )
      #     ),
      #     tags$td(
      #       "Set a study-wide precision threshold (0 - 100%) to define acceptable uncertainty in estimated concentrations.
      #  When set it is applied for all antigens. For additional granularity, set the threshold for individual antigens
      #  in the antigen settings."
      #     )
      #   )
      # ),
      tags$table(
        style = "width: 100%; border-collapse: collapse; border: 1px solid #ddd;",
        
        # ---- Antigen Family ----
        tags$tr(
          tags$td(
            style = "border: 1px solid #ddd; padding: 10px; width: 50%; vertical-align: top;",
            orderInput(
              inputId = antigen_family_order_params$param_name,
              label = antigen_family_order_params$param_label,
              items = default_db_antigen_family_order
            ),
            br(),
            uiOutput("antigen_family_order_warning")
          ),
          tags$td(
            style = "border: 1px solid #ddd; padding: 10px; width: 50%; vertical-align: top;",
            HTML("<strong>Antigen Family Order:</strong><br>
              Order antigen families from most to least important, from left to right.")
          )
        ),
        
        # ---- Antigen ----
        tags$tr(
          tags$td(
            style = "border: 1px solid #ddd; padding: 10px; vertical-align: top;",
            orderInput(
              inputId = antigen_order_params$param_name,
              label = antigen_order_params$param_label,
              items = default_db_antigen_order
            ),
            br(),
            uiOutput("antigen_order_warning")
          ),
          tags$td(
            style = "border: 1px solid #ddd; padding: 10px; vertical-align: top;",
            HTML("<strong>Antigen Order:</strong><br>
              Order antigens based on their importance for analysis. Place the most important on the left and the least important on the right.")
          )
        ),
        
        # ---- Threshold ----
        tags$tr(
          tags$td(
            style = "border: 1px solid #ddd; padding: 10px; vertical-align: top;",
            numericInputIcon(
              "study_pcov_threshold",
              "pCoV Threshold",
              value = 15,
              min = 0,
              max = 100,
              step = 1
            )
          ),
          tags$td(
            style = "border: 1px solid #ddd; padding: 10px; vertical-align: top;",
            HTML("<strong>Precision Threshold:</strong><br>
              Set a study-wide precision threshold (0–100%) to define acceptable uncertainty in estimated concentrations.
              When set, it applies to all antigens. For additional granularity, define thresholds for individual antigens
              in the table below.")
          )
        )
      ),
      div(
        style = "width: 100%; padding: 0 15px;",  # Added container styling
        span(
          div(style = "display:inline-block; margin-bottom: 10px;",
              title = "Info",
              icon("info-circle", class = "fa-lg", `data-toggle` = "tooltip",
                   `data-placement` = "right",
                   title = paste("To edit the family of an antigen for", selected_study,
                                 " click on the cell in the Antigen Family column.",
                                 " After making the change, click anywhere outside the cell to save the update"),
                   `data-html` = "true")
          )
        )
      ),
      fluidRow(
        div(
          style = "width: 100%; overflow-x: auto;",
          DTOutput("antigen_family_table")
        )

      ),
      actionButton(inputId = "save_antigen_family_settings",
                   "Save")

    #   style = "primary"
    # ) # end panel
    )
  })


  output$antigen_family_table <- renderDT({
    req(antigen_families_rv())
    cat("Rendering datatable\n")
    
    datatable(antigen_families_rv(),
              options = list(
                pageLength = 50,
                scrollX = TRUE,
                scrollY = "400px",
                autoWidth = TRUE,
                responsive = TRUE,
                order = list(list(0, 'asc')),
                columnDefs = list(
                  list(className = 'dt-center', targets = '_all'),
                  # ---- existing l_asy_constraint_method dropdown ----
                  list(
                    targets = which(colnames(antigen_families_rv()) == "l_asy_constraint_method"),
                    render = JS("
                    function(data, type, row, meta) {
                      var opts = ['default','user_defined','range_of_blanks', 'geometric_mean_of_blanks'];
                      if (type === 'display') {
                        var select = '<select class=\"dt-select\">';
                        for (var i = 0; i < opts.length; i++) {
                          var selected = (data == opts[i]) ? 'selected' : '';
                          select += '<option value=\"' + opts[i] + '\" ' + selected + '>' + opts[i] + '</option>';
                        }
                        select += '</select>';
                        return select;
                      }
                      return data;
                    }
                  ")
                  ),
                  # ---- NEW model_form_list dropdown ----
                  list(
                    targets = which(colnames(antigen_families_rv()) == "model_form_list"),
                    render = JS("
                    function(data, type, row, meta) {
                      var opts = [
                        'Y5,Yd5,Y4,Yd4,Ygomp4',   // default: all models
                        'Y5',
                        'Yd5',
                        'Y4',
                        'Yd4',
                        'Ygomp4'
                      ];
                      var labels = [
                        'Y5, Yd5, Y4, Yd4, Ygomp4',
                        'Y5',
                        'Yd5',
                        'Y4',
                        'Yd4',
                        'Ygomp4'
                      ];
                      if (type === 'display') {
                        var select = '<select class=\"dt-select\">';
                        for (var i = 0; i < opts.length; i++) {
                          var selected = (data == opts[i]) ? 'selected' : '';
                          select += '<option value=\"' + opts[i] + '\" ' + selected + '>' + labels[i] + '</option>';
                        }
                        select += '</select>';
                        return select;
                      }
                      return data;
                    }
                  ")
                  )
                )
              ),
              editable = list(
                target = 'cell',
                disable = list(columns = c(0:4))
              ),
              selection = 'none',
              class = 'cell-border stripe hover',
              # ---- updated callback to handle BOTH dropdowns ----
              callback = JS("
              $(document).on('change', '#antigen_family_table table select', function() {
                var tbl = $('#antigen_family_table table').DataTable();
                var cell = tbl.cell($(this).closest('td'));
                var rowIndex = cell.index().row;
                var colIndex = cell.index().column;
                var value = $(this).val();
                Shiny.setInputValue('antigen_family_dropdown_edit', {
                  row: rowIndex + 1,
                  col: colIndex,
                  value: value,
                  rand: Math.random()
                });
              });
            ")
    ) %>%
      formatStyle(columns = 1:ncol(antigen_families_rv()),
                  backgroundColor = 'white',
                  borderBottom = '1px solid #ddd')
  })

  # output$antigen_family_table <- renderDT({
  #   req(antigen_families_rv())
  #   #req(study_config)
  #   # study_config <- study_config[study_config$param_group == "antigen_family",]
  #   # antigen_family_order_params <- study_config[study_config$param_name == "antigen_family_order",]
  #   # antigen_order_params <- study_config[study_config$param_name == "antigen_order",]
  #   #
  #   # antigen_family_order <- strsplit(antigen_family_order_params$param_character_value, ",")[[1]]
  #   # antigen_order <- strsplit(antigen_order_params$param_character_value, ",")[[1]]
  #   #
  #   #
  #    cat("Rendering datatable\n")
  #   # antigen_family_df <- antigen_families_rv()
  #   # antigen_family_df$antigen_family <- factor(antigen_family_df$antigen_family, levels = antigen_family_order)
  #   # antigen_family_df <- antigen_family_df[order(antigen_family_df$antigen_family), , drop = FALSE]
  #   #
  #   # antigen_family_df$antigen <- factor(antigen_family_df$antigen, levels = antigen_order)
  #   # antigen_family_df <- antigen_family_df[order(antigen_family_df$antigen), , drop = FALSE]
  #   # antigen_families_rv(antigen_family_df)
  # 
  #   datatable(antigen_families_rv(),
  #             options = list(
  #               pageLength = 50,
  #               scrollX = TRUE,
  #               scrollY = "400px",
  #               autoWidth = TRUE,  # Added this
  #               responsive = TRUE, # Added this
  #               order = list(list(0, 'asc')),
  #               columnDefs = list(
  #                 list(className = 'dt-center', targets = '_all'),
  #                 list(
  #                   targets = which(colnames(antigen_families_rv()) == "l_asy_constraint_method"),
  #                   render = JS("
  #                                 function(data, type, row, meta) {
  #                                   var opts = ['default','user_defined','range_of_blanks', 'geometric_mean_of_blanks'];
  #                                   if (type === 'display') {
  #                                     var select = '<select>';
  #                                     for (var i = 0; i < opts.length; i++) {
  #                                       var selected = (data == opts[i]) ? 'selected' : '';
  #                                       select += '<option value=\"' + opts[i] + '\" ' + selected + '>' + opts[i] + '</option>';
  #                                     }
  #                                     select += '</select>';
  #                                     return select;
  #                                   }
  #                                   return data;
  #                                 }
  #                               ")
  #                   )
  #               )
  #             ),
  #             editable = list(
  #               target = 'cell',
  #               disable = list(columns = c(0:4))
  #             ),
  #             selection = 'none',
  #             class = 'cell-border stripe hover',  # Added styling classes
  #             callback = JS("
  #   $(document).on('change', '#antigen_family_table table select', function() {
  #     var tbl = $('#antigen_family_table table').DataTable();
  #     var cell = tbl.cell($(this).closest('td'));
  #     var rowIndex = cell.index().row;
  #     var colIndex = cell.index().column;
  #     var value = $(this).val();
  #     Shiny.setInputValue('antigen_family_dropdown_edit', {
  #       row: rowIndex + 1,
  #       col: colIndex,
  #       value: value,
  #       rand: Math.random()
  #     });
  #   });
  # ")
  #   ) %>%
  #     formatStyle(columns = 1:ncol(antigen_families_rv()),  # Added column styling
  #                 backgroundColor = 'white',
  #                 borderBottom = '1px solid #ddd')
  # })

  output$bead_count_config <- renderUI({
   req(study_config)
   req(input$basic_qc_params == 'Bead Count Parameters')
    # req(study_config_rv())
    # study_config <- study_config_rv()
    min_val_lower_bc <- strsplit(study_config[study_config$param_name == "lower_bc_threshold",]$param_choices_list, ",")[[1]][1]
    min_val_upper_bc <- strsplit(study_config[study_config$param_name == "upper_bc_threshold",]$param_choices_list, ",")[[1]][1]
    failed_well_params <-  study_config[study_config$param_name == "failed_well_criteria",]
    failed_well_params_choices <- strsplit(failed_well_params$param_choices_list, ",")[[1]]
    min_val_pct_agg_threshold <- strsplit(study_config[study_config$param_name == "pct_agg_threshold",]$param_choices_list, ",")[[1]][1]
#

    mainPanel(
#     bsCollapse(
#       id = "bead_count_config_collapse",
#       bsCollapsePanel(
#         title = "Bead Count Parameters",
     # HTML("<h4><strong>Bead Count</strong></h4>"),
      numericInput(inputId = "lower_bc_threshold",
                   label =  study_config[study_config$param_name == "lower_bc_threshold",]$param_label,
                   value = study_config[study_config$param_name == "lower_bc_threshold",]$param_integer_value,
                   min = if (min_val_lower_bc == "NA") NA else min_val_lower_bc),
      numericInput(inputId = "upper_bc_threshold",
                   label =  study_config[study_config$param_name == "upper_bc_threshold",]$param_label,
                   value = study_config[study_config$param_name == "upper_bc_threshold",]$param_integer_value,
                   min = if (min_val_upper_bc == "NA") NA else min_val_upper_bc),

     numericInput(inputId = "pct_agg_threshold",
                  label = study_config[study_config$param_name == "pct_agg_threshold",]$param_label,
                  value = study_config[study_config$param_name == "pct_agg_threshold",]$param_integer_value,
                  min = if (min_val_pct_agg_threshold == "NA") NA else min_val_pct_agg_threshold),


      # failed well criteria
      radioButtons(failed_well_params$param_name,
                   label = failed_well_params$param_label,
                   choices = failed_well_params_choices,
                   selected = study_config[study_config$param_name == "failed_well_criteria",]$param_character_value),

     uiOutput("failed_well_warning"),

     actionButton(inputId = "save_bead_count_params",
                  label = "Save"),


      # style = "primary"
      # )
    )
  })
#
  output$dilution_analysis_config <- renderUI({
    req(study_config)
    req(input$advanced_qc_params == 'Dilution Analysis Parameters')
    # req(study_config_rv())
    # study_config <- study_config_rv()

    study_config <- study_config[study_config$param_group == "dilution_analysis",]
    node_order_params <-  study_config[study_config$param_name == "node_order",]
    node_order_params_choices <- strsplit(node_order_params$param_choices_list, ",")[[1]]
    # -- remove linear region from the choices available to set (new algorithm as of 1/16/2026)
    node_order_params_choices <- node_order_params_choices[node_order_params_choices != "linear_region"]

    valid_gate_class_params <- study_config[study_config$param_name == "valid_gate_class",]
    valid_gate_class_choices <- strsplit(valid_gate_class_params$param_choices_list, ",")[[1]]
    is_binary_gc_params <-  study_config[study_config$param_name == "is_binary_gc",]
    zero_pass_too_diluted_Tx_params <- study_config[study_config$param_name == "zero_pass_diluted_Tx",]
    zero_pass_too_diluted_Tx_choices <- strsplit(zero_pass_too_diluted_Tx_params$param_choices_list, ",")[[1]]
    zero_pass_concentrated_Tx_params <- study_config[study_config$param_name == "zero_pass_concentrated_Tx",]
    zero_pass_concentrated_Tx_choices <- strsplit(zero_pass_concentrated_Tx_params$param_choices_list, ",")[[1]]
    zero_pass_concentrated_diluted_Tx_params <- study_config[study_config$param_name == "zero_pass_concentrated_diluted_Tx",]
    zero_pass_concentrated_diluted_Tx_choices <- strsplit(zero_pass_concentrated_diluted_Tx_params$param_choices_list, ",")[[1]]
    one_pass_acceptable_Tx_params <- study_config[study_config$param_name == "one_pass_acceptable_Tx",]
    one_pass_acceptable_Tx_params_choices <- strsplit(one_pass_acceptable_Tx_params$param_choices_list, ",")[[1]]
    two_plus_pass_acceptable_Tx_params <- study_config[study_config$param_name == "two_plus_pass_acceptable_Tx",]
    two_plus_pass_acceptable_Tx_choices <- strsplit(two_plus_pass_acceptable_Tx_params$param_choices_list, ",")[[1]]

    au_treatment_choices_names = c("Keep all AU measurements",
                "Keep passing AU measurements",
                "Geometric mean of all AU measurements",
                "Geometric mean of passing AU measurements",
                "Replace AU measurements with geometric mean of blank AUs",
                "Exclude AU measurements")
    zero_pass_too_diluted_Tx_choices <- setNames(zero_pass_too_diluted_Tx_choices, au_treatment_choices_names)
    zero_pass_concentrated_Tx_choices <- setNames(zero_pass_concentrated_Tx_choices, au_treatment_choices_names)
    zero_pass_concentrated_diluted_Tx_choices <- setNames(zero_pass_concentrated_diluted_Tx_choices, au_treatment_choices_names)
    one_pass_acceptable_Tx_params_choices <- setNames(one_pass_acceptable_Tx_params_choices, au_treatment_choices_names)
    two_plus_pass_acceptable_Tx_choices <- setNames(two_plus_pass_acceptable_Tx_choices, au_treatment_choices_names)


   default_db_node_order <- strsplit(node_order_params$param_character_value, ",")[[1]]
   ordered_db_node_order_choices <- c(
     default_db_node_order,
     setdiff(node_order_params_choices, default_db_node_order)
   )

   default_valid_gate_class_order <- strsplit(valid_gate_class_params$param_character_value,",")[[1]]
   ordered_db_gate_class_choices <- c(default_valid_gate_class_order,
                                      setdiff(valid_gate_class_choices,default_valid_gate_class_order))
   # if (is.null(default_db_node_order) || identical(default_db_node_order, "null")) {
   #   default_db_node_order <- "linear"
   # }
    # if (!identical(default_db_node_order,node_order_params_choices )) {
    #   default_db_node_order <- node_order_params_choices #strsplit(node_order_params$param_character_value, ",")[[1]] # fallback
    # }

   mainPanel(
    # bsCollapse(
    #   id = "dilution_analysis_config",
    #   bsCollapsePanel(
    #     title = "Dilution Analysis Parameters",
    #  HTML("<h4><strong>Dilution Analysis</strong></h4>"),
      HTML(paste0("<h4>For the decision tree, select the order in which the decisions (and thus the nodes) are created from the type of sample limit selector.
                  Set the limit of detection that is considered passing in the decision tree classification and how the final arbritary units are calculated
                  for the combinations of the number of dilutions that are passing and the sample's concentration status. </h4>")),

     grVizOutput("decision_tree_diagram"),

      selectInput(inputId = "node_order",
                  label = node_order_params$param_label,
                  choices = ordered_db_node_order_choices,#node_order_params_choices,
                  selected = strsplit(node_order_params$param_character_value, ",")[[1]],
                  multiple = TRUE),

      # Valid gate class parameters
      selectInput(inputId = valid_gate_class_params$param_name,
                  label = valid_gate_class_params$param_label,
                  choices = ordered_db_gate_class_choices,#alid_gate_class_choices,
                  selected = strsplit(valid_gate_class_params$param_character_value,",")[[1]],
                  multiple = TRUE),

      # Binary Passing or not for Limits in tree
      checkboxInput(inputId = is_binary_gc_params$param_name,
                    label = is_binary_gc_params$param_label,
                    value = as.logical(toupper(is_binary_gc_params$param_boolean_value))),

    uiOutput("is_bnary_gc_warning"),

      HTML(paste0("<h4> Set decisions on how to treat the final arbritary unit calculations for
                  the associated number of passing dilutions and concentration status.</h4>")),
      fluidRow(
        column(6,
      radioButtons(inputId = zero_pass_too_diluted_Tx_params$param_name,
                   label = zero_pass_too_diluted_Tx_params$param_label,
                   choices = zero_pass_too_diluted_Tx_choices,
                   selected = zero_pass_too_diluted_Tx_params$param_character_value),

      uiOutput("zero_pass_diluted_Tx_warning"),

      radioButtons(inputId = zero_pass_concentrated_Tx_params$param_name,
                  label = zero_pass_concentrated_Tx_params$param_label,
                  choices = zero_pass_concentrated_Tx_choices,
                  selected = zero_pass_concentrated_Tx_params$param_character_value),
      uiOutput("zero_pass_concentrated_Tx_warning"),

      radioButtons(inputId = zero_pass_concentrated_diluted_Tx_params$param_name,
                   label = zero_pass_concentrated_diluted_Tx_params$param_label,
                   choices = zero_pass_concentrated_diluted_Tx_choices,
                   selected = zero_pass_concentrated_diluted_Tx_params$param_character_value),
      uiOutput("zero_pass_concentrated_diluted_Tx_warning")
        ),
      column(6,
      radioButtons(inputId = one_pass_acceptable_Tx_params$param_name,
                   label = one_pass_acceptable_Tx_params$param_label,
                   choices = one_pass_acceptable_Tx_params_choices,
                   selected = one_pass_acceptable_Tx_params$param_character_value),
      uiOutput("one_pass_acceptable_Tx_warning"),

      radioButtons(inputId = two_plus_pass_acceptable_Tx_params$param_name,
                   label = two_plus_pass_acceptable_Tx_params$param_label,
                   choices = two_plus_pass_acceptable_Tx_choices,
                   selected = two_plus_pass_acceptable_Tx_params$param_character_value
                   ),
     uiOutput("two_plus_pass_acceptable_Tx_warning")
      )

    ),
    actionButton(inputId = "save_dilution_analysis_config",
                 label = "Save")

  #   style = "primary"
  # )
    )
})


  output$standard_curve_config <- renderUI({
    req(study_config)
    req(input$basic_qc_params == 'Standard Curve Parameters')
    # req(study_config_rv())
    # study_config <- study_config_rv()
    req(study_sources)
    # get study config parameters
    study_config <- study_config[study_config$param_group == "standard_curve_options",]
    mean_mfi_params <- study_config[study_config$param_name == "mean_mfi",]
    log_mfi_axis_params <- study_config[study_config$param_name == "is_log_mfi_axis",]
    prozone_correction_params <- study_config[study_config$param_name == "applyProzone",]
    blank_options_params <- study_config[study_config$param_name == "blank_option",]
    blank_options_choices <- strsplit(blank_options_params$param_choices_list, ",")[[1]]
    source_options_choices <- study_sources$source
    default_source_params <- study_config[study_config$param_name == "default_source",]

    blank_control_choices_names = c("Ignored" = "ignored", "Included" = "included", "Subtracted 1 x Geometric mean" = "subtracted",
                "Subtract 3 x Geometric Mean" = "subtracted_3x", "Subtracted 10 x Geometric Mean" = "subtracted_10x")
    blank_options_choices <- setNames(blank_options_choices, names(blank_control_choices_names))
    # default_source_choices <- strsplit(default_source_params$param_choices_list, ",")[[1]]

     # check if source has been saved (initially it is not)
    default_db_source <- default_source_params$param_character_value
    cat("Default db source\n")
    print(default_db_source)

    # if (!default_db_source %in% source_options_choices) {
    #   default_db_source <- source_options_choices[1]  # fallback
    # }



    # default_blank_option <- blank_options_params$param_character_value
    # if (!default_blank_option %in% blank_options_choices) {
    #   default_blank_option <- blank_options_params$param_character_value
    # }

    mainPanel(
  # bsCollapse(
  #   id = "standard_curve_config",
  #   bsCollapsePanel(
  #     title = "Standard Curve Parameters",

      # bsCollapse(
      #   id = "standard_curve_parameters_info",
      #   bsCollapsePanel(
      #     title = "Standard Curve Parameters Methods",
      #     tagList(
      #      tags$p("The Prozone correction which is recomended, accounts for the prozone effect in stndard curve data in which
      #             '...the concentration of the analyte becomes so high that it exceeds the capacity of the antibodies in the assay' (Bradley and Bhalla)"),
      #      tags$p("Including the geometric mean of the MFI of the blanks and subtracting the geometric mean of the blanks from each standard are adapted from Sanz et al.")
      #
      #     ), # end taglist
      #     style = "success"
      #   )),


      tags$table(
        border = 1,

        # Row 1: Mean MFI
        tags$tr(
          tags$td(
            switchInput(mean_mfi_params$param_name,
                        label = mean_mfi_params$param_label,
                        value = as.logical(toupper(mean_mfi_params$param_boolean_value))),
            uiOutput("mean_mfi_warning")
          ),
          tags$td(
            "Take the mean of multiple MFI measurements in the standards at same concentration. This is especially helpful if there are repeated MFI measures."
          )
        ),

        # Row 2: Log MFI
        tags$tr(
          tags$td(
            switchInput(log_mfi_axis_params$param_name,
                        label = log_mfi_axis_params$param_label,
                        value = as.logical(toupper(log_mfi_axis_params$param_boolean_value))),
            uiOutput("is_log_mfi_warning")
          ),
          tags$td(
            "Choose whether or not to log transform MFI when fitting standard curves."
          )
        ),

        # Row 3: Prozone correction
        tags$tr(
          tags$td(
            switchInput(prozone_correction_params$param_name,
                        label = prozone_correction_params$param_label,
                        value = as.logical(toupper(prozone_correction_params$param_boolean_value))),
            uiOutput("apply_prozone_warning")
          ),
          tags$td(
            "The Prozone Correction, which is recomended, accounts for the prozone effect in stndard curve data in which
                  '...the concentration of the analyte becomes so high that it exceeds the capacity of the antibodies in the assay' (Bradley and Bhalla)"
          )
        ),

        # Row 4: Blank control options
        tags$tr(
          tags$td(
            radioButtons(blank_options_params$param_name,
                         label = blank_options_params$param_label,
                         choices = blank_options_choices,
                         selected = blank_options_params$param_character_value),
            uiOutput("blank_option_warning")
          ),
          tags$td(
            "Select how blank controls are handled in the standard curve estimation. Options include Ignore, Include geometric mean of blank, or Subtract multiples of the geometric mean.",
            div(style = "display:inline-block; margin-bottom: 10px;",
                title = "Info",
                icon("info-circle", class = "fa-lg", `data-toggle` = "tooltip",
                     `data-placement` = "right",
                     title = paste("To select a method for which the blanks are to be treated for the selected study click one of the methods.
                                 The avaliable methods are 'Ignore', 'Include', 'Subtract Geometric Mean', 'Subtract three times the Geometric Mean' and subtract 10 times the geometric mean.
                                 When Ignore is selected, the blanks are not considered in the standard curve.
                                 When Included is selected, the stimation of the standard curve takes into account the mean of the background
of the values as another point of the standard curve. The median fluorescence intensity and the expected concentration for this new point by analyte is estimated as follows:
                                 MFI: geometric mean of the blank controls.
                                 log dilution: The mininum log dilution - log10(2). This corresponds to the the minimum expected concentration value of the standard points divided by 2 as in the drLumi package.
                                 When subtracted is selected, the geometric mean of the blank controls is subracted from all the standard points. Depending on what level of subtraction is selected,
                                 the geometric mean is multiplied by that factor (1,3, or 10) before the subtraction is applied. After subtraction, if any MFI is below 0 it is set to 0."),
                     `data-html` = "true")
            )
          )
        ),

        # Row 5: Source
        tags$tr(
          tags$td(
            radioButtons(default_source_params$param_name,
                         label = default_source_params$param_label,
                         choices  = source_options_choices,
                         selected = default_db_source),
            uiOutput("default_source_warning")
          ),
          tags$td(
            "Select the default source to use for standard curve calculation."
          )
        )
      ),

  #  HTML("<h4><strong>Standard Curve</strong></h4>"),
    # Mean MFI at each dilution factor
#     switchInput(mean_mfi_params$param_name,
#                 label = mean_mfi_params$param_label,
#                 value = as.logical(toupper(mean_mfi_params$param_boolean_value))),
#
#     uiOutput("mean_mfi_warning"),
#    # Use MFI as log or not
#    switchInput(log_mfi_axis_params$param_name,
#               label = log_mfi_axis_params$param_label,
#               value = as.logical(toupper(log_mfi_axis_params$param_boolean_value))),
#
#    uiOutput("is_log_mfi_warning"),
#
#   # Prozone correction or not
#   switchInput(prozone_correction_params$param_name,
#               label = prozone_correction_params$param_label,
#               value = as.logical(toupper(prozone_correction_params$param_boolean_value))),
#
#   uiOutput("apply_prozone_warning"),
#
#
#
#     # Blank Control Options
#     div(style = "display:inline-block; margin-bottom: 10px;",
#         title = "Info",
#         icon("info-circle", class = "fa-lg", `data-toggle` = "tooltip",
#              `data-placement` = "right",
#              title = paste("To select a method for which the buffers are to be treated for the selected study click one of the methods.
#                                  The avaliable methods are 'Ignore', 'Include', 'Subtract Geometric Mean', 'Subtract three times the Geometric Mean' and subtract 10 times the geometric mean.
#                                  When Ignore is selected, the buffers are not considered in the standard curve.
#                                  When Included is selected, the stimation of the standard curve takes into account the mean of the background
# of the values as another point of the standard curve. The median fluorescence intensity and the expected concentration for this new point by analyte is estimated as follows:
#                                  MFI: geometric mean of the blank controls.
#                                  log dilution: The mininum log dilution - log10(2). This corresponds to the the minimum expected concentration value of the standard points divided by 2 as in the drLumi package.
#                                  When subtracted is selected, the geometric mean of the blank controls is subracted from all the standard points. Depending on what level of subtraction is selected,
#                                  the geometric mean is multiplied by that factor (1,3, or 10) before the subtraction is applied. After subtraction, if any MFI is below 0 it is set to 0."),
#              `data-html` = "true")
#     ),
#     radioButtons(blank_options_params$param_name,
#                  label = blank_options_params$param_label,
#                  choices = blank_options_choices,
#                  selected = blank_options_params$param_character_value),
#
#     uiOutput("blank_option_warning"),
#     # Source - get from loaded data - default to first source
#      radioButtons(default_source_params$param_name,
#                   label = default_source_params$param_label,
#                   choices  = source_options_choices,
#                   selected = default_db_source), # source_options_choices[1]),
#   uiOutput("default_source_warning"),

  actionButton(inputId = "save_standard_curve_config",
               label = "Save")

#   style = "primary")
# )
)

  })

  output$subgroup_config <- renderUI({
    req(study_config)
    req(input$advanced_qc_params == 'Subgroup Parameters')
    # req(study_config_rv())
    # study_config <- study_config_rv()
    req(study_arms)
    req(study_timeperiods)

    # get study config parameters
    study_config <- study_config[study_config$param_group == "subgroup_settings",]
    reference_arm_params <- study_config[study_config$param_name == "reference_arm",]
    reference_arm_choices <- study_arms$agroup
    timeperiod_order_params <- study_config[study_config$param_name == "timeperiod_order",]
    timeperiod_choices <- study_timeperiods$timeperiod
    primary_timeperiod_comparison_params <- study_config[study_config$param_name == "primary_timeperiod_comparison",]

    # Decide to load database values if they are saved after first time it is run and provide fall back
    default_db_reference_arm <- reference_arm_params$param_character_value
    if (is.null(default_db_reference_arm)) {
      default_db_reference_arm <- reference_arm_choices[1]
    } else {
      if (!default_db_reference_arm %in% reference_arm_choices) {
        default_db_reference_arm <- reference_arm_choices[1]  # fallback
      }
    }
   # default_db_timeperiod_order <- timeperiod_order_params$param_character_value
    if(is.null(timeperiod_order_params$character_value)) {
        default_db_timeperiod_order <- timeperiod_choices# fallback
    } else {
      default_db_timeperiod_order <- strsplit(timeperiod_order_params$param_character_value, ",")[[1]]
      if (!all(default_db_timeperiod_order %in% timeperiod_choices)) {
        #if (!default_db_timeperiod_order %in% timeperiod_choices) {
          default_db_timeperiod_order <- timeperiod_choices# fallback
      }
    }

  if (is.null(primary_timeperiod_comparison_params$param_character_value)) {
      default_db_primary_timeperiod_comparison <- timeperiod_choices[1:2]# fallback
  } else {
    default_db_primary_timeperiod_comparison <- strsplit(primary_timeperiod_comparison_params$param_character_value, ",")[[1]]
      #default_db_primary_timeperiod_comparison <- primary_timeperiod_comparison_params$param_character_value
      #if (!default_db_primary_timeperiod_comparison %in% timeperiod_choices) {
    if (!all(default_db_primary_timeperiod_comparison %in% timeperiod_choices)) {
        default_db_primary_timeperiod_comparison <- timeperiod_choices[1:2]# fallback
    }
  }

    mainPanel(
    # bsCollapse(
    #   id = "subgroup_config",
    #   bsCollapsePanel(
    #     title = "Subgroup Parameters",
    #HTML("<h4><strong>Subgroup Parameters</strong></h4>"),
    # Arm control
    radioButtons(inputId = reference_arm_params$param_name,
                 label = reference_arm_params$param_label,
                 choices = reference_arm_choices,
                 selected = default_db_reference_arm),#reference_arm_choices[1]),

    uiOutput("reference_arm_warning"),
    # Order the timeperiods
    orderInput(inputId = timeperiod_order_params$param_name,
               label = timeperiod_order_params$param_label,
               items = default_db_timeperiod_order), #timeperiod_choices),

    uiOutput("timeperiod_order_warning"),

    selectInput(inputId = primary_timeperiod_comparison_params$param_name,
                label = primary_timeperiod_comparison_params$param_label,
                choices = timeperiod_choices,
                selected = default_db_primary_timeperiod_comparison,#timeperiod_choices[1:2],
                multiple = T),

    actionButton(inputId = "save_subgroup_config",
                 label = "Save"),

    # style = "primary")
    # )
    )

  })

 }
  else {
    output$studyParameters_UI <- renderUI({
      if (main_tab_selected != "home_page" & main_tab_selected != "manage_project_tab") {
        tagList(
          conditionalPanel(
            condition = "input.readxMap_study_accession == 'Click here'",
            HTML("<h3>Choose or create a study to change study settings.</h3>")
          )
        )
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
#   study_config <- fetch_study_configuration(study_accession = study_accession, user = currentuser())
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

# observe({
#   req(input$readxMap_study_accession)
#   req(currentuser())
#   if (input$study_level_tabs == "Study Parameters") {
#     cat("Load Study Parameters tab\n")
#
#     study_user_params_nrow <- nrow(fetch_study_configuration(
#       study_accession = input$readxMap_study_accession,
#       user = currentuser()
#     ))
#
#
#     study_accession <-  input$readxMap_study_accession
#     user <-   currentuser()
#
#     check_and_render_study_parameters(study_accession, user)
#
#
#     if (study_user_params_nrow > 0) {
#       render_study_parameters()
#     }
#
#     # config <- fetch_study_configuration(study_accession = input$readxMap_study_accession, user = currentuser())
#     # study_config_rv(config)
#    # render_study_parameters()
#   }
# })

# Detect Changed Values antigen

output$antigen_family_order_warning <- renderUI({
  input$save_antigen_family_settings # once save happens -reset validity of changes
  input$reset_user_config
  antigen_family_choices <- unique(antigen_families_rv()$antigen_family)


  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())

  antigen_study_config <- study_config[study_config$param_group == "antigen_family",]
  antigen_family_order_params <-  antigen_study_config[antigen_study_config$param_name == "antigen_family_order",]
  antigen_family_order_database <- antigen_family_order_params$param_character_value

  if (!is.null(antigen_family_order_database) && length(antigen_family_order_database) > 0) {
    antigen_family_order_database_comparison <- strsplit(antigen_family_order_database, ",")[[1]]
  } else {
    antigen_family_order_database_comparison <- antigen_family_choices

  }
  #antigen_family_order_database_comparison <- strsplit(antigen_family_order_database, ",")[[1]]

  if (!identical(input$antigen_family_order, antigen_family_order_database_comparison)) {
    span(style = "color: #F89406;",
         "Unsaved Changes")
  }
})

output$antigen_order_warning <- renderUI({
  input$save_antigen_family_settings # once save happens -reset validity of changes
  input$reset_user_config
  antigen_choices <- unique(antigen_families_rv()$antigen)
  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())

  antigen_study_config <- study_config[study_config$param_group == "antigen_family",]
  antigen_order_params <-  antigen_study_config[antigen_study_config$param_name == "antigen_order",]
  antigen_order_database <- antigen_order_params$param_character_value

  if (!is.null(antigen_order_database) && length(antigen_order_database) > 0) {
    antigen_order_database_comparison <- strsplit(antigen_order_database, ",")[[1]]
  } else {
    antigen_order_database_comparison <- antigen_choices

  }
 # antigen_order_database_comparison <- strsplit(antigen_order_database, ",")[[1]]
  if (!identical(input$antigen_order,antigen_order_database_comparison)) {
    span(style = "color: #F89406;",
         "Unsaved Changes")
  }
})
# observeEvent({
#   req(input$readxMap_study_accession)
#   # Get selected study
#   selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)
#
#   study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())
#
#   study_config <- study_config[study_config$param_group == "antigen_family",]
#   antigen_family_order_params <- study_config[study_config$param_name == "antigen_family_order",]
#   antigen_order_params <- study_config[study_config$param_name == "antigen_order",]
#
#   print(input$antigen_order)
# if (!identical(input$antigen_order, antigen_order_params$param_character_value)) {
#   cat("unsaved changes")
#   showFeedbackWarning(session = session, "antigen_order", text = "Unsaved changes")
# } else {
#   hideFeedback(session = session, "antigen_order")
# }
# })

# Bead Count  Detect Changed Values
## Observe for non radio buttons and orderInputs
observe({
  req(input$main_tabs == "view_files_tab")
#  req(study_level_tabs == "Study Parameters")
  req(input$readxMap_study_accession)
   input$save_bead_count_params # once save happens -reset validity of changes
   input$reset_user_config
    # Get selected study
    selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

    study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())

    bead_count_study_config <- study_config[study_config$param_group == "bead_count",]
    lower_threshold_param <- bead_count_study_config[bead_count_study_config$param_name == "lower_bc_threshold",]
    bead_count_database_lower_threshold_val <- lower_threshold_param$param_integer_value

    if (!identical(input$lower_bc_threshold, bead_count_database_lower_threshold_val)) {
      showFeedbackWarning("lower_bc_threshold", text = "Unsaved Changes")
    } else {
      hideFeedback("lower_bc_threshold")
    }

    bead_count_upper_threshold_param <- bead_count_study_config[bead_count_study_config$param_name == "upper_bc_threshold",]
    bead_count_database_upper_threshold_val <- bead_count_upper_threshold_param$param_integer_value
    if (!identical(input$upper_bc_threshold, bead_count_database_upper_threshold_val)) {
      showFeedbackWarning("upper_bc_threshold", text = "Unsaved Changes")
    } else {
      hideFeedback("upper_bc_threshold")
    }

    bead_count_pct_agg_threshold_param <- bead_count_study_config[bead_count_study_config$param_name == "pct_agg_threshold",]
    bead_count_database_pct_agg_threshold_val <- bead_count_pct_agg_threshold_param$param_integer_value
    if (!identical(input$pct_agg_threshold, bead_count_database_pct_agg_threshold_val)) {
      showFeedbackWarning("pct_agg_threshold", text = "Unsaved Changes")
    } else {
      hideFeedback("pct_agg_threshold")
    }

    # bead_count_failed_well_crieria <-  bead_count_study_config[bead_count_study_config$param_name == "failed_well_criteria",]
    # bead_count_database_failed_well_criteria <- bead_count_failed_well_crieria$param_character_value
    # if (!identical(input$failed_well_criteria, bead_count_database_failed_well_criteria)) {
    #   showFeedbackWarning("failed_well_criteria", text = "Unsaved Changes")
    # } else {
    #   hideFeedback("failed_well_criteria")
    # }

})

## Bead Count radioButtons observe Changed Values
output$failed_well_warning <- renderUI({
  input$save_bead_count_params # once save happens -reset validity of changes
  input$reset_user_config
  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())

  bead_count_study_config <- study_config[study_config$param_group == "bead_count",]
  bead_count_failed_well_crieria <-  bead_count_study_config[bead_count_study_config$param_name == "failed_well_criteria",]
  bead_count_database_failed_well_criteria <- bead_count_failed_well_crieria$param_character_value
  if (input$failed_well_criteria != bead_count_database_failed_well_criteria) {
    span(style = "color: #F89406;",
         "Unsaved Chamges")
  }
})

## Dilution Analysis Diagrams
decision_tree_reactive_diagram <- reactive({
  input$save_dilution_analysis_config # refresh when the dilution analysis params are saved.
  input$reset_user_config

  req(input$readxMap_study_accession)
  req(currentuser())

  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())


  is_binary <- study_config[study_config$param_name == "is_binary_gc",]$param_boolean_value
  node_order <- strsplit(study_config[study_config$param_name == "node_order",]$param_character_value, ",")[[1]]
  sufficient_gc <- strsplit(study_config[study_config$param_name == "valid_gate_class",]$param_character_value, ",")[[1]]

  replacements <- c(limits_of_detection = "gate", linear_region = "linear", limits_of_quantification = "quantifiable")
  node_order <- ifelse(node_order %in% names(replacements), replacements[node_order], node_order)

  # replacements_gate <- c(Between_Limits_of_Detection = "Between_Limits", Above_Upper_Limit_of_Detection = "Above_Upper_Limit", Below_Lower_Limit_of_Detection = "Below_Lower_Limit")
  # sufficient_gc <- ifelse(sufficient_gc %in% names(replacements_gate), replacements_gate[sufficient_gc], sufficient_gc)

  # Create truth table inline
  truth_table <- create_truth_table(
    binary_gate = is_binary,
    exclude_linear = FALSE,
    exclude_quantifiable = FALSE,
    exclude_gate = FALSE
  )

  # Create decision tree
  decision_tree <- create_decision_tree_tt(
    truth_table = truth_table,
    binary_gate = is_binary,
    sufficient_gc_vector = sufficient_gc,
    node_order = node_order
  )

  decision_tree
})


# decision_tree_reactive_diagram <- reactive({
#   req(decision_tree_cache())
#   decision_tree_cache()
# })

## Decision Tree Plot
output$decision_tree_diagram <- renderGrViz({
  req(decision_tree_reactive_diagram())

  decision_tree <- decision_tree_reactive_diagram()

  dot_string <- paste(
    "digraph tree {",
    paste(get_edges(decision_tree), collapse = "; "),
    "}",
    sep = "\n"
  )
  grViz(dot_string)
  #decision_tree$ToDiagrammeRGraph()
})

## Dilution Analysis - Detect Change Parameters
observe({
  req(input$readxMap_study_accession != "Click here")
 # req(input$main_tabs == "view_files_tab")
  #req(input$study_level_tabs)
  #req(input$study_level_tabs == "Study Parameters")
  req(input$readxMap_study_accession)
  req(input$node_order)
  input$save_dilution_analysis_config # once save happens -reset validity of changes
  input$reset_user_config

  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())
  study_config_v <- study_config

  dilution_analysis_study_config <- study_config[study_config$param_group == "dilution_analysis",]
  node_order_param <- dilution_analysis_study_config[dilution_analysis_study_config$param_name == "node_order",]
  node_order_param_val <- node_order_param$param_character_value

  node_order_vector <- paste(input$node_order, collapse = ",")

  #node_order_param_val_vector <- strsplit(node_order_param_val, ",")[[1]]
 #check <<- identical(strsplit(node_order_param_val, ",")[[1]], node_order)

  if (!identical(node_order_vector, node_order_param_val)) {
    showFeedbackWarning("node_order", text = "Unsaved Changes")
  } else {
    hideFeedback("node_order")
  }
  # Passing Limit of Detection
  valid_gate_class_param <- dilution_analysis_study_config[dilution_analysis_study_config$param_name == "valid_gate_class",]
  valid_gate_class_param_val <- valid_gate_class_param$param_character_value
  valid_gate_class_param_val_vector <- strsplit(valid_gate_class_param_val, ",")[[1]]

  if (!identical(input$valid_gate_class, valid_gate_class_param_val_vector)) {
    showFeedbackWarning("valid_gate_class", text = "Unsaved Changes")
  } else {
    hideFeedback("valid_gate_class")
  }
})

## Dilution Analysis monitor changes for non select Inputs
output$is_bnary_gc_warning <- renderUI({
  input$save_dilution_analysis_config # once save happens -reset validity of changes
  input$reset_user_config

  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())

  dilution_analysis_study_config <- study_config[study_config$param_group == "dilution_analysis",]
  dilution_analysis_passing_limit_of_d <-  dilution_analysis_study_config[dilution_analysis_study_config$param_name == "is_binary_gc",]
  dilution_analysis_passing_limit_of_d_val <- as.logical(toupper(dilution_analysis_passing_limit_of_d$param_boolean_value))
  if (input$is_binary_gc != dilution_analysis_passing_limit_of_d_val) {
    span(style = "color: #F89406;",
         "Unsaved Changes")
  }
})
# passing dilution - concentration status
output$zero_pass_diluted_Tx_warning <- renderUI({
  input$save_dilution_analysis_config # once save happens -reset validity of changes
  input$reset_user_config
  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())

  dilution_analysis_study_config <- study_config[study_config$param_group == "dilution_analysis",]
  zero_pass_diluted_TX <-  dilution_analysis_study_config[dilution_analysis_study_config$param_name == "zero_pass_diluted_Tx",]
  dilution_analysis_zero_pass_diluted_TX_val<- zero_pass_diluted_TX$param_character_value
  if (input$zero_pass_diluted_Tx != dilution_analysis_zero_pass_diluted_TX_val) {
    span(style = "color: #F89406;",
         "Unsaved Changes")
  }
})

output$zero_pass_concentrated_Tx_warning <- renderUI({
  input$save_dilution_analysis_config # once save happens -reset validity of changes
  input$reset_user_config

  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())

  dilution_analysis_study_config <- study_config[study_config$param_group == "dilution_analysis",]
  zero_pass_concentrated_Tx <-  dilution_analysis_study_config[dilution_analysis_study_config$param_name == "zero_pass_concentrated_Tx",]
  dilution_analysis_zero_pass_concentrated_Tx_val<- zero_pass_concentrated_Tx$param_character_value
  if (input$zero_pass_concentrated_Tx != dilution_analysis_zero_pass_concentrated_Tx_val) {
    span(style = "color: #F89406;",
         "Unsaved Changes")
  }
})

output$zero_pass_concentrated_diluted_Tx_warning <- renderUI({
  input$save_dilution_analysis_config # once save happens -reset validity of changes
  input$reset_user_config

  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())

  dilution_analysis_study_config <- study_config[study_config$param_group == "dilution_analysis",]
  zero_pass_concentrated_diluted_Tx <-  dilution_analysis_study_config[dilution_analysis_study_config$param_name == "zero_pass_concentrated_diluted_Tx",]
  dilution_analysis_zero_pass_concentrated_diluted_Tx_val <- zero_pass_concentrated_diluted_Tx$param_character_value
  if (input$zero_pass_concentrated_diluted_Tx != dilution_analysis_zero_pass_concentrated_diluted_Tx_val) {
    span(style = "color: #F89406;",
         "Unsaved Changes")
  }
})

output$one_pass_acceptable_Tx_warning <- renderUI({
  input$save_dilution_analysis_config # once save happens -reset validity of changes
  input$reset_user_config

  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())

  dilution_analysis_study_config <- study_config[study_config$param_group == "dilution_analysis",]
  one_pass_acceptable_Tx <-  dilution_analysis_study_config[dilution_analysis_study_config$param_name == "one_pass_acceptable_Tx",]
  dilution_analysis_one_pass_acceptable_Tx_val <- one_pass_acceptable_Tx$param_character_value
  if (input$one_pass_acceptable_Tx != dilution_analysis_one_pass_acceptable_Tx_val) {
    span(style = "color: #F89406;",
         "Unsaved Changes")
  }
})

output$two_plus_pass_acceptable_Tx_warning <- renderUI({
  input$save_dilution_analysis_config # once save happens -reset validity of changes
  input$reset_user_config

  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())

  dilution_analysis_study_config <- study_config[study_config$param_group == "dilution_analysis",]
  two_plus_pass_acceptable_Tx <-  dilution_analysis_study_config[dilution_analysis_study_config$param_name == "two_plus_pass_acceptable_Tx",]
  dilution_analysis_two_plus_pass_acceptable_Tx_val <- two_plus_pass_acceptable_Tx$param_character_value
  if (input$two_plus_pass_acceptable_Tx != dilution_analysis_two_plus_pass_acceptable_Tx_val) {
    span(style = "color: #F89406;",
         "Unsaved Changes")
  }
})

## Monitor changes for Standard Curve Parameters
# mean mfi (aggrigate or not)
output$mean_mfi_warning <- renderUI({
  input$save_standard_curve_config # monitor save button
  input$reset_user_config

  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())

  standard_curve_study_config <- study_config[study_config$param_group == "standard_curve_options",]
  mean_mfi_param <-  standard_curve_study_config[standard_curve_study_config$param_name == "mean_mfi",]
  mean_mfi_param_val <- as.logical(toupper(mean_mfi_param$param_boolean_val))
  if (input$mean_mfi != mean_mfi_param_val) {
    span(style = "color: #F89406;",
         "Unsaved Changes")
  }
})

output$is_log_mfi_warning <- renderUI({
  input$save_standard_curve_config # monitor save button
  input$reset_user_config

  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())

  standard_curve_study_config <- study_config[study_config$param_group == "standard_curve_options",]
  log_mfi_axis_param <-  standard_curve_study_config[standard_curve_study_config$param_name == "is_log_mfi_axis",]
  log_mfi_axis_param_val <- as.logical(toupper(log_mfi_axis_param$param_boolean_val))
  if (input$is_log_mfi_axis != log_mfi_axis_param_val) {
    span(style = "color: #F89406;",
         "Unsaved Changes")
  }
})

output$apply_prozone_warning <- renderUI({
  input$save_standard_curve_config # monitor save button
  input$reset_user_config

  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())

  standard_curve_study_config <- study_config[study_config$param_group == "standard_curve_options",]
  prozone_correction_param <-  standard_curve_study_config[standard_curve_study_config$param_name == "applyProzone",]
  prozone_correction_param_val <- as.logical(toupper(prozone_correction_param$param_boolean_val))
  if (input$applyProzone != prozone_correction_param_val) {
    span(style = "color: #F89406;",
         "Unsaved Changes")
  }
})


#blank control options
output$blank_option_warning <- renderUI({
  input$save_standard_curve_config # monitor save button
  input$reset_user_config

  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())

  standard_curve_study_config <- study_config[study_config$param_group == "standard_curve_options",]
  blank_controls_param <-  standard_curve_study_config[standard_curve_study_config$param_name == "blank_option",]
  blank_option_val <- blank_controls_param$param_character_val
  if (input$blank_option != blank_option_val) {
    span(style = "color: #F89406;",
         "Unsaved Changes")
  }
})

output$default_source_warning <- renderUI({
  input$save_standard_curve_config # monitor save button
  input$reset_user_config

  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())

  standard_curve_study_config <- study_config[study_config$param_group == "standard_curve_options",]
  source_params <-  standard_curve_study_config[standard_curve_study_config$param_name == "default_source",]
  source_params_val <- source_params$param_character_val
  if (!identical(input$default_source, source_params_val)) {
    span(style = "color: #F89406;",
         "Unsaved Changes")
  }
})

### Detect changes for subgroup parameters
observe({
 # req(input$main_tabs == "view_files_tab")
  req(input$readxMap_study_accession)
  req(input$primary_timeperiod_comparison)
  input$save_subgroup_config # once save happens -reset validity of changes
  input$reset_user_config

  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())

  subgroup_study_config <- study_config[study_config$param_group == "subgroup_settings",]
  primary_timeperiod_comparison_param <- subgroup_study_config[subgroup_study_config$param_name == "primary_timeperiod_comparison",]
  primary_timeperiod_comparison_val <- primary_timeperiod_comparison_param$param_character_value
  #comparison <<-  input$primary_timeperiod_comparison
 # primary_timeperiod_order_database_comparison <<- strsplit(primary_timeperiod_comparison_val, ",")[[1]]
  #primary_timeperiod_order_database_comparison <-   paste(input$primary_timeperiod_comparison_val, collapse = ",")

  primary_timeperiod_comparison_vector <- paste(input$primary_timeperiod_comparison, collapse = ",")

  if (!identical(primary_timeperiod_comparison_vector,primary_timeperiod_comparison_val)) {
    showFeedbackWarning("primary_timeperiod_comparison", text = "Unsaved Changes")
  } else {
    hideFeedback("primary_timeperiod_comparison")
  }
})

output$reference_arm_warning <- renderUI({
  input$save_subgroup_config # monitor save button
  input$reset_user_config

  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())

  subgroup_study_config <- study_config[study_config$param_group == "subgroup_settings",]
  reference_arm_params <-  subgroup_study_config[subgroup_study_config$param_name == "reference_arm",]
  reference_arm_params_val <- reference_arm_params$param_character_value
  if (!identical(input$reference_arm, reference_arm_params_val)) {
    span(style = "color: #F89406;",
         "Unsaved Changes")
  }
})

output$timeperiod_order_warning <- renderUI({
  input$save_subgroup_config # monitor save button
  input$reset_user_config

  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())

  subgroup_study_config <- study_config[study_config$param_group == "subgroup_settings",]
  timeperiod_order_params <-  subgroup_study_config[subgroup_study_config$param_name == "timeperiod_order",]
  timeperiod_order_params_val <- timeperiod_order_params$param_character_value
  if (!is.null(timeperiod_order_params_val) && length(timeperiod_order_params_val) > 0) {
    timeperiod_order_database_comparison <- strsplit(timeperiod_order_params_val, ",")[[1]]
  } else {
    timeperiod_order_database_comparison <- input$timeperiod_order
  }

  if (!identical(input$timeperiod_order, timeperiod_order_database_comparison)) {
    span(style = "color: #F89406;",
         "Unsaved Changes")
  }
})

# Export last saved user settings from the database
output$user_parameter_download <- renderUI({
  req(input$main_tabs == "view_files_tab")
  req(input$readxMap_study_accession)
  req(currentuser())
  download_user_parameters(study_accession = input$readxMap_study_accession, user = currentuser())
})

output$user_parameter_download <- renderUI({
  req(input$readxMap_study_accession)
  req(currentuser())
  button_label <- paste0("Download ", input$readxMap_study_accession, " Configuration for ", currentuser())
  downloadButton("user_parameter_download_handle", button_label)
})


output$user_parameter_download_handle <-  downloadHandler(
  filename = function() {
    paste(input$readxMap_study_accession, "study_config", currentuser(), ".csv", sep = "_")
  },
  content = function(file) {
    req(input$main_tabs == "study_settings")
    req(input$readxMap_study_accession)
    req(currentuser())

    user_config_table <- download_user_parameters(study_accession = input$readxMap_study_accession, user = currentuser())


    # download data component (data frame)
    write.csv(user_config_table, file)
  }
)


### Update Database Buttons
observeEvent(input$save_bead_count_params, {
  req(currentuser())
  req(input$readxMap_study_accession)
  req(input$lower_bc_threshold)
  req(input$upper_bc_threshold)
  req(input$failed_well_criteria)
  cat("updating bead count params")
  cat(currentuser())
  cat(input$readxMap_study_accession)
  cat(input$lower_bc_threshold)
  cat(input$upper_bc_threshold)
  cat(input$failed_well_criteria)

# Update the lower bead count
  update_query <- paste0("UPDATE madi_results.xmap_study_config
  SET param_integer_value = ", as.numeric(input$lower_bc_threshold), "
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'lower_bc_threshold'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_query)

  update_upper_threshold_query <- paste0("UPDATE madi_results.xmap_study_config
  SET param_integer_value = ", as.numeric(input$upper_bc_threshold), "
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'upper_bc_threshold'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_upper_threshold_query)

  update_pct_agg_threshold_query <-  paste0("UPDATE madi_results.xmap_study_config
  SET param_integer_value = ", as.numeric(input$pct_agg_threshold), "
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'pct_agg_threshold'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_pct_agg_threshold_query)

  update_failed_well_query <- paste0("UPDATE madi_results.xmap_study_config
  SET param_character_value = '", input$failed_well_criteria, "'
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'failed_well_criteria'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_failed_well_query)

  showNotification("Bead Count parameters updated successfully", type = "message")

})

observeEvent(input$save_antigen_family_settings, {
  req(currentuser())
  req(input$readxMap_study_accession)
  cat(input$antigen_family_order)
  cat(input$antigen_order)

  req(input$antigen_family_order)
  req(input$antigen_order)
  req(input$study_pcov_threshold)
  cat("Antigen Family settings saved")

  antigen_family_order_str <- paste0("'", paste(input$antigen_family_order, collapse = ","), "'")
  antigen_order_str <- paste0("'", paste(input$antigen_order, collapse = ","), "'")

  update_query <- paste0("UPDATE madi_results.xmap_study_config
  SET param_character_value = ", antigen_family_order_str, "
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'antigen_family_order'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_query)

  update_antigen_order <- paste0("UPDATE madi_results.xmap_study_config
  SET param_character_value = ", antigen_order_str, "
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'antigen_order'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_antigen_order)
  
  update_study_pcov_threshold <- paste0("UPDATE madi_results.xmap_antigen_family
                                        SET pcov_threshold = ",input$study_pcov_threshold, "
                                        WHERE study_accession = '",input$readxMap_study_accession,"'
                                        AND project_id = ", userWorkSpaceID(), "
                                        AND l_asy_constraint_method is not null;"
                                        )
  dbExecute(conn, update_study_pcov_threshold)
  
  antigen_families_rv(
    fetch_antigen_family_table(
      input$readxMap_study_accession,
      userWorkSpaceID()
    )
  )

  showNotification("Antigen Family Parameters updated successfully", type = "message")

})

observeEvent(input$save_dilution_analysis_config, {
  req(currentuser())
  req(input$readxMap_study_accession)
  req(input$node_order)
  req(input$valid_gate_class)
  req(input$zero_pass_diluted_Tx)
  req(input$zero_pass_concentrated_Tx)
  req(input$zero_pass_concentrated_diluted_Tx)
  req(input$one_pass_acceptable_Tx)
  req(input$two_plus_pass_acceptable_Tx)
  cat("Saving dilution analysis configurations")

  node_order_str <- paste0("'", paste(input$node_order, collapse = ","), "'")

  valid_gc_str <- paste0("'", paste(input$valid_gate_class, collapse = ","), "'")

  update_sample_limit <-  paste0("UPDATE madi_results.xmap_study_config
  SET param_character_value = ", node_order_str, "
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'node_order'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_sample_limit)

  # passing limit of detection
  update_valid_gate_class <-  paste0("UPDATE madi_results.xmap_study_config
  SET param_character_value = ", valid_gc_str, "
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'valid_gate_class'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_valid_gate_class)

  update_is_binary_gc <-  paste0("UPDATE madi_results.xmap_study_config
  SET param_boolean_value = ", input$is_binary_gc, "
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'is_binary_gc'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_is_binary_gc)

  update_zero_pass_diluted_Tx <-  paste0("UPDATE madi_results.xmap_study_config
  SET param_character_value = '", input$zero_pass_diluted_Tx, "'
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'zero_pass_diluted_Tx'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_zero_pass_diluted_Tx)

  update_zero_pass_concentrated_Tx <-  paste0("UPDATE madi_results.xmap_study_config
  SET param_character_value = '", input$zero_pass_concentrated_Tx, "'
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'zero_pass_concentrated_Tx'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_zero_pass_concentrated_Tx)

  update_zero_pass_concentrated_diluted_Tx <-  paste0("UPDATE madi_results.xmap_study_config
  SET param_character_value = '", input$zero_pass_concentrated_diluted_Tx, "'
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'zero_pass_concentrated_diluted_Tx'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn,update_zero_pass_concentrated_diluted_Tx )

  update_one_pass_acceptable_Tx <-  paste0("UPDATE madi_results.xmap_study_config
  SET param_character_value = '", input$one_pass_acceptable_Tx, "'
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'one_pass_acceptable_Tx'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn,update_one_pass_acceptable_Tx)

  update_two_plus_pass_acceptable_Tx <-  paste0("UPDATE madi_results.xmap_study_config
  SET param_character_value = '", input$two_plus_pass_acceptable_Tx, "'
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'two_plus_pass_acceptable_Tx'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn,update_two_plus_pass_acceptable_Tx)

  showNotification("Dilution Analysis Parameters updated successfully", type = "message")


})

observeEvent(input$save_standard_curve_config, {
  req(currentuser())
  req(input$readxMap_study_accession)
  req(input$blank_option)
  req(input$default_source)
  cat("in save standard curve")

  update_mean_mfi <- paste0("UPDATE madi_results.xmap_study_config
  SET param_boolean_value = ", input$mean_mfi, "
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'mean_mfi'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_mean_mfi)

  update_blank_option <-  paste0("UPDATE madi_results.xmap_study_config
  SET param_character_value = '", input$blank_option, "'
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'blank_option'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_blank_option)

  update_is_log_mfi_axis <-  paste0("UPDATE madi_results.xmap_study_config
  SET param_boolean_value = ", input$is_log_mfi_axis, "
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'is_log_mfi_axis'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_is_log_mfi_axis)

  update_prozone_correction <-  paste0("UPDATE madi_results.xmap_study_config
  SET param_boolean_value = ", input$applyProzone, "
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'applyProzone'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_prozone_correction)


  update_source <- paste0("UPDATE madi_results.xmap_study_config
  SET param_character_value = '", input$default_source, "'
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'default_source'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_source)

  showNotification("Standard Curve Parameters updated successfully", type = "message")

})

observeEvent(input$save_subgroup_config, {
  req(currentuser())
  req(input$readxMap_study_accession)
  req(input$reference_arm)
  print(input$timeperiod_order)
  req(input$primary_timeperiod_comparison)
  cat("Saving Subgroup configurations")

  timeperiod_order_str <- paste0("'", paste(input$timeperiod_order, collapse = ","), "'")
  primary_timeperiod_comparison_str <- paste0("'", paste(input$primary_timeperiod_comparison, collapse = ","), "'")

  update_reference_arm <- paste0("UPDATE madi_results.xmap_study_config
  SET param_character_value = '", input$reference_arm, "'
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'reference_arm'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_reference_arm)

  update_timeperiod_order <- paste0("UPDATE madi_results.xmap_study_config
  SET param_character_value = ", timeperiod_order_str, "
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'timeperiod_order'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_timeperiod_order)

  update_timeperiod_comparsion <- paste0("UPDATE madi_results.xmap_study_config
  SET param_character_value = ", primary_timeperiod_comparison_str, "
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'primary_timeperiod_comparison'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_timeperiod_comparsion)

  showNotification("Subgroup Parameters updated successfully", type = "message")


})

# Reset the configuration for a user for the selected study
observeEvent(input$reset_user_config, {
  req(currentuser())
  req(input$readxMap_study_accession)
  # 1st delete user's configuration
  delete_user_config <- paste0("DELETE FROM madi_results.xmap_study_config
  WHERE study_accession = '", input$readxMap_study_accession, "'
  AND param_user = '", currentuser(),"';")

  dbExecute(conn, delete_user_config)

  initial_source <- unique(stored_plates_data$stored_standard$source)[1]

  # Add back fresh study configuration
  intitialize_study_configurations(study_accession = input$readxMap_study_accession, user = currentuser(), initial_source = initial_source)

  #study_config <- fetch_study_configuration(study_accession = input$readxMap_study_accession, user = currentuser())


  showNotification(paste0("Successfully reset study configuration for ", currentuser()), type = "message")

})


output$export_import_parameters <- renderUI({
  
  div(class = "card p-3",
      
      div(class = "mb-3",
          p(
            "Researchers can export study configuration settings to share with collaborators.",
            "You can import their study configuration file to apply the same study settings to your profile."
          )
      ),
      # Export
      div(class = "mb-3",
          br(),
          downloadButton(
            "download_parameters",
            "Export Study Configuration",
            class = "btn-success",
            icon = icon("file-export")
          )
      ),
      
      hr(),
      
      strong("Select a study configuration file to import"),
      
      fluidRow(
        column(
          width = 8,
          fileInput(
            "import_parameters",
            NULL,
            accept = c(".xlsx", ".xls")
          )
        )),
        fluidRow(
          column(
            width = 12,
            div(class = "mt-3",
                uiOutput("config_preview_ui")
            )
          )
        ), 
        fluidRow(
          column(width = 4,
                 uiOutput("config_validation_msg"))
        ), 
        fluidRow(
        column(
          width = 4,
          actionButton(
            "load_parameters",
            "Load Study Configuration",
            icon = icon("upload"),
            class = "btn-primary w-100",
            disabled = TRUE
          )
        )
      ),
      br(),
      div(class = "mt-3",
          uiOutput("config_upload_status")
      )
  )
})

# reset message
observeEvent(input$import_parameters, {
  df <- readxl::read_excel(
    input$import_parameters$datapath,
    sheet = 1,
  )
  df <- df[, !(names(df) %in% "param_user")]
  
  antigen_df <- tryCatch(
    readxl::read_excel(
      input$import_parameters$datapath,
      sheet = "Antigen_Parameters"
    ),
    error = function(e) NULL
  )
  
  # store previews
  config_preview(df)
  config_preview_antigen(antigen_df)
  
  config_upload_state(list(
    is_uploaded = FALSE,
    upload_time = NULL,
    user = NULL
  ))
  
})

# output$export_import_parameters <- renderUI({
#   tagList(
#     # ---- Export button ------------------------------------------------
#     downloadButton(
#       outputId = "download_parameters",
#       label    = "Export parameters to Excel",
#       class    = "btn-success",
#       icon     = icon("file-export")
#     ),
#     
#     # ---- File chooser -------------------------------------------------
#     fileInput(
#       inputId = "import_parameters",
#       label   = "Select parameter file to import",
#       accept  = c(".xlsx", ".xls", ".csv")
#     ),
#     
#     actionButton(
#       inputId = "load_parameters",
#       label   = "Load selected file",
#       icon    = icon("upload"),
#       class   = "btn-primary",
#       disabled = TRUE            # start disabled – we enable it when a file is chosen
#     ),
#     uiOutput("config_upload_status")
#   )
#     
# })

valid_study_config <- reactive({
  req(config_preview())
  
  df <- config_preview()
  
  expected_study <- input$readxMap_study_accession
  expected_project  <- userWorkSpaceID()
  
  all(
    df$study_accession == expected_study,
    df$project_id == expected_project
  )
})

valid_antigen_config <- reactive({
  req(config_preview_antigen())
  
  df <- config_preview_antigen()
  
  expected_study   <- input$readxMap_study_accession
  expected_project <- userWorkSpaceID()
  
  all(
    df$study_accession == expected_study,
    df$project_id == expected_project
  )
})

observe({
  toggleState(
    "load_parameters",
    condition = !is.null(input$import_parameters) && isTRUE(valid_study_config() && isTRUE(valid_antigen_config()))
  )
})

output$config_validation_msg <- renderUI({
  req(config_preview())
  
  df <- config_preview()
  
  expected_study   <- input$readxMap_study_accession
  expected_project <- userWorkSpaceID()
  
  # study mismatch
  if (!all(df$study_accession == expected_study)) {
    return(
      div(class = "text-warning",
          icon("exclamation-circle"),
          paste("Study mismatch:", unique(df$study_accession))
      )
    )
  }
  
  # Project mismatch
  if (!all(df$project_id == expected_project)) {
    return(
      div(class = "text-warning",
          icon("exclamation-circle"),
          paste("Project mismatch:", unique(df$project_id))
      )
    )
  }
  
  # Match
  div(class = "text-muted",
      icon("check"),
      "Configuration matches the current study and project"
  )
})

output$config_validation_msg_antigen <- renderUI({
  req(config_preview_antigen())
  
  df <- config_preview_antigen()
  
  expected_study   <- input$readxMap_study_accession
  expected_project <- userWorkSpaceID()
  
  if (!all(df$study_accession == expected_study)) {
    return(
      div(class = "text-warning",
          icon("exclamation-circle"),
          paste("Antigen study mismatch:", unique(df$study_accession))
      )
    )
  }
  
  if (!all(df$project_id == expected_project)) {
    return(
      div(class = "text-warning",
          icon("exclamation-circle"),
          paste("Antigen project mismatch:", unique(df$project_id))
      )
    )
  }
  
  div(class = "text-muted",
      icon("check"),
      "Antigen configuration matches study and project"
  )
})

observeEvent(input$load_parameters, {
  req(input$import_parameters)   # safety
  
  # Determine file type by extension
  ext <- tools::file_ext(input$import_parameters$name)
  
  imported <- switch(
    tolower(ext),
    xlsx = read_excel(input$import_parameters$datapath, sheet = 1),
    xls  = read_excel(input$import_parameters$datapath, sheet = 1),
    csv  = read.csv(input$import_parameters$datapath,
                    stringsAsFactors = FALSE,
                    na.strings = c("", "NA")),
    {
      showNotification("Unsupported file type.", type = "error")
      return(NULL)
    }
  )
  
  imported_clean <- imported %>%
    mutate(
      parameter = stringr::str_trim(parameter),
      parameter = stringr::str_remove(parameter, ":$"),
      project_id = as.integer(project_id)   
    )
  
  imported_antigen_config <- switch(
    tolower(ext),
    xlsx = read_excel(input$import_parameters$datapath, sheet = "Antigen_Parameters"),
    xls  = read_excel(input$import_parameters$datapath, sheet = "Antigen_Parameters"),
    csv  = read.csv(input$import_parameters$datapath,
                    stringsAsFactors = FALSE,
                    na.strings = c("", "NA")),
    {
      showNotification("Unsupported file type.", type = "error")
      return(NULL)
    }
  )
  
  # preview_df <- imported_clean[, !(names(imported_clean) %in% "param_user")]
  # 
  # config_preview(preview_df)
  # 
  final_config <- write_back_config(imported_clean, conn)
  
  # update the user to be the current user
  final_config$param_user <- currentuser()
  

  # update the db 
  update_study_config(final_config, conn)
  
  update_antigen_family_config(imported_antigen_config, conn)
  
  config_upload_state(list(
    is_uploaded = TRUE,
    upload_time = Sys.time(),
    user = currentuser()
  ))
  
})

# output$config_preview_ui <- renderUI({
#   req(config_preview())
#   
#   tagList(
#     strong("Preview of uploaded configuration:"),
#     DT::dataTableOutput("config_preview_table")
#   )
# })
output$config_preview_ui <- renderUI({
  req(config_preview())
  
  tagList(
    
    # ---- PARAMETERS ----
    strong("Preview of uploaded configuration:"),
    DT::dataTableOutput("config_preview_table"),
    uiOutput("config_validation_msg"),
    
    br(),
    
    # ---- ANTIGEN ----
    if (!is.null(config_preview_antigen())) tagList(
      strong("Preview of antigen configuration:"),
      DT::dataTableOutput("config_preview_table_antigen"),
      uiOutput("config_validation_msg_antigen")
    )
  )
})

output$config_preview_table <- DT::renderDataTable({
  req(config_preview())
  
  config_preview()
}, options = list(
  pageLength = 5,
  scrollX = TRUE
))

output$config_preview_table_antigen <- DT::renderDataTable({
  req(config_preview_antigen())
  config_preview_antigen()
}, options = list(
  pageLength = 5,
  scrollX = TRUE
))

output$config_upload_status <- renderUI({
  state <- config_upload_state()
  if (state$is_uploaded) {
    div(
      class = "alert alert-success",
      icon("check-circle"),
      paste("Study Configuration uploaded by", state$user, "at",
            format(state$upload_time, "%Y-%m-%d %H:%M:%S"))
    )
  }
})
  



output$download_parameters <- downloadHandler(
  filename = function() {
    paste0(input$readxMap_study_accession, "_", "_config_template.xlsx")
  },
  content = function(file) {
    create_parameter_template(
      conn            = conn,
      param_user      = currentuser(),   # ← adjust if you want a reactive input
      project_id     = userWorkSpaceID(),
      study_accession =  input$readxMap_study_accession,
      output_file     = file
    )
    
  }
)
# --------------------------------------------------------------
#  create_parameter_template()
# --------------------------------------------------------------
#' Export MADI study parameters to an Excel workbook
#'
#' @param con            A DBI connection object.  If NULL the function
#'                       will open a new ODBC connection using the
#'                       arguments supplied in `dsn`, `uid`, `pwd`,
#'                       `database`.  The connection is closed automatically.
#' @param dsn            ODBC DSN name (used only if `con == NULL`).
#' @param uid            Database user name (used only if `con == NULL`).
#' @param pwd            Database password (used only if `con == NULL`).
#' @param database       Database name (optional, depends on driver).
#' @param param_user     Email address of the user whose parameters you want.
#' @param study_accession Study accession (e.g. "MADI_01").
#' @param output_file    Full path (including .xlsx) where the workbook will be saved.
#' @param delimiter      Character that separates items in `param_choices_list`
#'                       (default = comma).  Only needed for the **Choices**
#'                       sheet.
#' @return invisible(path to the written file) – also prints a short summary.
#' @examples
#' ## Use an existing DBI connection
#' con <- DBI::dbConnect(odbc::odbc(), dsn = "MyDSN")
#' create_parameter_template(
#'   con = con,
#'   param_user = "seamus.owen.stein@dartmouth.edu",
#'   study_accession = "MADI_01",
#'   output_file = "MADI_01_parameters.xlsx"
#' )
#' DBI::dbDisconnect(con)
#' --------------------------------------------------------------

create_parameter_template <- function(conn,
                                      param_user,
                                      project_id,
                                      study_accession,
                                      output_file,
                                      delimiter = ",") {
  
  # ── 1. Query ────────────────────────────────────────────────────
  sql <- glue::glue("
  SELECT
      param_user,
      project_id,
      study_accession,
      param_name,
      param_label           AS parameter,
      param_group,
      param_data_type,
      param_choices_list,
      CASE
          WHEN UPPER(TRIM(param_data_type)) = 'BOOLEAN'
               THEN CAST(param_boolean_value AS VARCHAR)
          WHEN UPPER(TRIM(param_data_type)) IN
               ('INTEGER','NUMERIC','FLOAT','DOUBLE','DECIMAL')
               THEN CAST(param_integer_value AS VARCHAR)
          WHEN UPPER(TRIM(param_data_type)) IN
               ('STRING','CHAR','TEXT','CATEGORICAL')
               THEN param_character_value
          ELSE NULL
      END AS param_value
  FROM   madi_results.xmap_study_config
  WHERE  project_id = {project_id}
     AND param_user = '{param_user}'
     AND study_accession = '{study_accession}';
  ")
  
  # ── 1b. Antigen family query ─────────────────────────────────────
  sql_antigen <- glue::glue("
          SELECT
              xmap_antigen_family_id,
              study_accession,
              project_id,
              experiment_accession,
              antigen,
              feature,
              antigen_family,
              standard_curve_concentration,
              antigen_name,
              virus_bacterial_strain,
              antigen_source,
              catalog_number,
              l_asy_min_constraint,
              l_asy_max_constraint,
              l_asy_constraint_method,
              model_form_list,
              pcov_threshold
          FROM madi_results.xmap_antigen_family
          WHERE study_accession = '{study_accession}'
            AND project_id = {project_id}
            AND l_asy_constraint_method IS NOT NULL;
          ")
  
  antigen_df <- DBI::dbGetQuery(conn, sql_antigen)
  params_df <- DBI::dbGetQuery(conn, sql)
  
  if (nrow(params_df) == 0) {
    stop("No parameters returned from database.")
  }
  
  # ── 2. Classify ─────────────────────────────────────────────────
  params_df <- params_df %>%
    dplyr::mutate(
      data_type_upper = toupper(trimws(param_data_type)),
      is_numeric      = data_type_upper %in% c("INTEGER","NUMERIC","FLOAT","DOUBLE","DECIMAL"),
      is_boolean      = data_type_upper == "BOOLEAN"
    )
  
  # ── 3. Safe value parsing ───────────────────────────────────────
  bad_tokens <- c("NULL","NA","INF","-INF","NAN","INFINITY","-INFINITY","")
  
  params_df <- params_df %>%
    dplyr::mutate(
      param_value_clean = trimws(param_value),
      param_value_clean = ifelse(
        toupper(param_value_clean) %in% bad_tokens,
        NA,
        param_value_clean
      ),
      param_value_num = suppressWarnings(as.numeric(param_value_clean))
    )
  
  # Safe assignment (no type conflicts)
  params_df$param_value_final <- params_df$param_value_clean
  idx <- params_df$is_numeric & !is.na(params_df$param_value_num)
  params_df$param_value_final[idx] <- params_df$param_value_num[idx]
  
  # ── 4. Clean choices (STRICT) ───────────────────────────────────
  params_df <- params_df %>%
    dplyr::mutate(
      choices_clean = trimws(param_choices_list),
      choices_clean = ifelse(
        toupper(choices_clean) %in% c("", "NA", "NULL"),
        NA,
        choices_clean
      )
    )
  
  dropdown_df <- params_df %>%
    dplyr::filter(
      !is_numeric,
      !is.na(choices_clean)
    ) %>%
    dplyr::mutate(
      resolved_choices = dplyr::case_when(
        is_boolean & !is.na(choices_clean) ~ choices_clean,
        is_boolean ~ "TRUE,FALSE",
        TRUE ~ choices_clean
      )
    )
  
  choices_long <- dropdown_df %>%
    dplyr::select(parameter, resolved_choices) %>%
    dplyr::distinct() %>%
    dplyr::mutate(choice = strsplit(resolved_choices, delimiter, fixed = TRUE)) %>%
    tidyr::unnest(choice) %>%
    dplyr::mutate(
      choice = trimws(choice),
      choice_upper = toupper(choice)
    ) %>%
    dplyr::filter(
      !is.na(choice),
      nchar(choice) > 0,
      !(choice_upper %in% c("NA","NULL","INF","-INF","NAN","INFINITY","-INFINITY"))
    ) %>%
    dplyr::select(parameter, choice)
  
  # OPTIONAL: require at least 2 valid choices
  choices_long <- choices_long %>%
    dplyr::group_by(parameter) %>%
    dplyr::filter(dplyr::n_distinct(choice) >= 2) %>%
    dplyr::ungroup()
  
  if (nrow(choices_long) > 0) {
    choices_wide <- choices_long %>%
      dplyr::group_by(parameter) %>%
      dplyr::mutate(row = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(names_from = parameter, values_from = choice) %>%
      dplyr::select(-row)
  } else {
    choices_wide <- data.frame()
  }
  
  params_with_dropdowns <- params_df %>%
    dplyr::group_by(parameter) %>%
    dplyr::summarise(
      is_numeric_any = any(is_numeric),
      has_choices = any(!is.na(choices_clean))
    ) %>%
    dplyr::filter(
      !is_numeric_any,   #  NEVER allow numeric params
      has_choices
    ) %>%
    dplyr::pull(parameter)
  
  
  # ── 5. Prepare export ───────────────────────────────────────────
  export_cols <- c("param_user","project_id","study_accession",
                   "parameter")
  
  export_df <- params_df %>%
    dplyr::select(dplyr::all_of(export_cols)) %>%
    dplyr::mutate(dplyr::across(everything(), as.character))
  
  export_df$param_value <- params_df$param_value_final
  
  # ── 6. Workbook ─────────────────────────────────────────────────
  wb <- openxlsx::createWorkbook()
  
  header_style <- openxlsx::createStyle(
    textDecoration = "bold", border = "Bottom", fgFill = "#DCE6F1"
  )
  
  
  # Parameters sheet
  openxlsx::addWorksheet(wb, "Parameters")
  openxlsx::writeData(wb, "Parameters", export_df, headerStyle = header_style)
  # Antigen paramters
  openxlsx::addWorksheet(wb, "Antigen_Parameters")
  
  if (nrow(antigen_df) > 0) {
    openxlsx::writeData(
      wb,
      "Antigen_Parameters",
      antigen_df,
      headerStyle = header_style
    )
  }
  # Choices sheet
  openxlsx::addWorksheet(wb, "Choices")
  if (nrow(choices_wide) > 0) {
    openxlsx::writeData(wb, "Choices", choices_wide, headerStyle = header_style)
  }
  
  param_value_col <- ncol(export_df)
  
  # ── 7. Dropdown validation ONLY ─────────────────────────────────
  for (i in seq_len(nrow(params_df))) {
    row_idx <- i + 1
    p_label <- params_df$parameter[i]
    
    if (p_label %in% params_with_dropdowns && nrow(choices_wide) > 0) {
      cidx <- which(colnames(choices_wide) == p_label)
      
      if (length(cidx) == 1) {
        n <- sum(!is.na(choices_wide[[cidx]]))
        
        if (n >= 2) {
          cl <- openxlsx::int2col(cidx)
          
          openxlsx::dataValidation(
            wb,
            sheet = "Parameters",
            cols  = param_value_col,
            rows  = row_idx,
            type  = "list",
            value = paste0("Choices!$", cl, "$2:$", cl, "$", n + 1),
            allowBlank = TRUE
          )
        }
      }
    }
  }
  
  # ── 8. Save ─────────────────────────────────────────────────────
  openxlsx::saveWorkbook(wb, file = output_file, overwrite = TRUE)
  
  message("Wrote ", nrow(params_df), " parameters to: ", output_file)
  invisible(output_file)
}



write_back_config <- function(imported, conn) {
  
  # -------------------------------
  # 1. Pull metadata from DB
  # -------------------------------
  config_tbl <- dbGetQuery(conn, "
    SELECT xmap_study_config_id, study_accession, param_group,
           param_name, param_label, param_data_type, param_char_len,
           param_control_type, param_choices_list,
           param_user, project_id
    FROM madi_results.xmap_study_config
  ")
  
  # -------------------------------
  # 2. Clean labels + enforce types
  # -------------------------------
  imported_clean <- imported %>%
    mutate(
      parameter  = str_trim(parameter),
      parameter  = str_remove(parameter, ":$"),
      project_id = as.integer(project_id)
    )
  
  if (any(is.na(imported_clean$project_id))) {
    stop("project_id contains non-numeric values after coercion")
  }
  
  config_tbl <- config_tbl %>%
    mutate(
      param_label = str_trim(param_label),
      param_label = str_remove(param_label, ":$")
    )
  
  # -------------------------------
  # 3. Join
  # -------------------------------
  joined <- imported_clean %>%
    left_join(
      config_tbl,
      by = c(
        "parameter" = "param_label",
        "study_accession",
        "param_user",
        "project_id"
      )
    )
  
  if (any(is.na(joined$param_name))) {
    bad <- joined %>% filter(is.na(param_name))
    stop(paste0(
      "Some parameters did not match DB config:\n",
      paste(bad$parameter, collapse = "\n")
    ))
  }
  
  # -------------------------------
  # 4. TYPE VALIDATION ONLY
  # -------------------------------
  joined <- joined %>%
    mutate(
      # integer check
      param_integer_value = case_when(
        param_data_type == "numeric" ~ suppressWarnings(as.numeric(param_value)),
        TRUE ~ NA_integer_
      ),
      
      # boolean check
      param_boolean_value = case_when(
        param_data_type == "boolean" ~ case_when(
          tolower(param_value) %in% c("true", "false") ~ tolower(param_value) == "true",
          TRUE ~ NA
        ),
        TRUE ~ NA
      ),
      
      # character fallback
      param_character_value = case_when(
        param_data_type %in% c("string", "categorical","character") ~ param_value,
        TRUE ~ NA_character_
      )
    )
  

  #  strict integer validation
  if (any(joined$param_data_type == "numeric" & is.na(joined$param_integer_value))) {
    bad <- joined %>%
      filter(param_data_type == "numeric" & is.na(param_integer_value))
    
    stop(paste0(
      "Invalid integer values:\n",
      paste(bad$parameter, bad$param_value, collapse = "\n")
    ))
  }
  
  #  strict boolean validation
  if (any(joined$param_data_type == "boolean" & is.na(joined$param_boolean_value))) {
    bad <- joined %>%
      filter(param_data_type == "boolean" & is.na(param_boolean_value))
    
    stop(paste0(
      "Invalid boolean values (must be true/false):\n",
      paste(bad$param_label, bad$param_value, collapse = "\n")
    ))
  }
  
  # -------------------------------
  # 5. Final structure
  # -------------------------------
  names(joined)[names(joined) == "parameter"] <- "param_label"
  
  final <- joined %>%
    select(
      xmap_study_config_id,
      study_accession,
      param_group,
      param_name,
      param_label,
      param_data_type,
      param_char_len,
      param_control_type,
      param_choices_list,
      param_integer_value,
      param_boolean_value,
      param_character_value,
      param_user,
      project_id
    )
  
  return(final)
}

update_study_config <- function(df, conn) {
  
  DBI::dbWriteTable(
    conn,
    "temp_config",
    df,
    temporary = TRUE,
    overwrite = TRUE
  )
  
  DBI::dbExecute(conn, "
    UPDATE madi_results.xmap_study_config AS t
    SET
      param_integer_value   = tmp.param_integer_value,
      param_boolean_value   = tmp.param_boolean_value,
      param_character_value = tmp.param_character_value,
      param_user            = tmp.param_user
    FROM temp_config AS tmp
    WHERE t.xmap_study_config_id = tmp.xmap_study_config_id
  ")
}

update_antigen_family_config <- function(df, conn) {
  num_cols <- c(
    "xmap_antigen_family_id",
    "project_id",
    "standard_curve_concentration",
    "l_asy_min_constraint",
    "l_asy_max_constraint",
    "pcov_threshold"
  )
  
  char_cols <- c(
    "study_accession", "experiment_accession", "antigen", "feature",
    "antigen_family", "antigen_name", "virus_bacterial_strain",
    "antigen_source", "catalog_number",
    "l_asy_constraint_method", "model_form_list"
  )
  
  # only mutate columns that exist
  num_cols  <- intersect(num_cols, names(df))
  char_cols <- intersect(char_cols, names(df))
  
  df[num_cols]  <- lapply(df[num_cols], as.numeric)
  df[char_cols] <- lapply(df[char_cols], as.character)
  
  # special case: integer
  if ("project_id" %in% names(df)) {
    df$project_id <- as.integer(df$project_id)
  }
  
  # --- 1. Write to temp table ---
  temp_table <- paste0("tmp_antigen_", as.integer(Sys.time()))
  
  DBI::dbWriteTable(
    conn,
    name = temp_table,
    value = df,
    temporary = TRUE,
    overwrite = TRUE
  )
  
  # --- 2. Single UPDATE using join ---
  sql <- glue::glue("
    UPDATE madi_results.xmap_antigen_family AS target
    SET
      experiment_accession        = src.experiment_accession,
      antigen                    = src.antigen,
      feature                    = src.feature,
      antigen_family             = src.antigen_family,
      standard_curve_concentration = src.standard_curve_concentration,
      antigen_name               = src.antigen_name,
      virus_bacterial_strain     = src.virus_bacterial_strain,
      antigen_source             = src.antigen_source,
      catalog_number             = src.catalog_number,
      l_asy_min_constraint       = src.l_asy_min_constraint,
      l_asy_max_constraint       = src.l_asy_max_constraint,
      l_asy_constraint_method    = src.l_asy_constraint_method,
      model_form_list            = src.model_form_list,
      pcov_threshold             = src.pcov_threshold
    FROM {temp_table} AS src
    WHERE
      target.xmap_antigen_family_id = src.xmap_antigen_family_id
      AND target.study_accession    = src.study_accession
      AND target.project_id         = src.project_id;
  ")
  
  DBI::dbExecute(conn, sql)
  
  message("Updated antigen family table (set-based).")
}

## Clear Study Configuration
# observeEvent(input$main_tabs, {
#   if (!is.null(input$main_tabs) && input$main_tabs != "view_files_tab") {
#     message("Not Viewing stored files")
#     cat("xmap experiment: ")
#     print(input$readxMap_experiment_accession)
#    updateSelectInput(session, "readxMap_experiment_accession", selected = "Click here")
#    cat("xmap experiment after update:\n ")
#    print(input$readxMap_experiment_accession)
#     # clear outputs
#     output$studyParameters_UI <- NULL
#     output$antigen_family_config <- NULL
#     output$antigen_family_table <- NULL
#     output$bead_count_config <- NULL
#     output$dilution_analysis_config <- NULL
#     output$standard_curve_config <- NULL
#     output$subgroup_config <- NULL
#     output$antigen_family_order_warning <- NULL
#     output$antigen_order_warning <- NULL
#     output$failed_well_warning <- NULL
#     output$decision_tree_diagram <- NULL
#     output$is_bnary_gc_warning <- NULL
#     output$zero_pass_diluted_Tx_warning <- NULL
#     output$zero_pass_concentrated_Tx_warning <- NULL
#     output$zero_pass_concentrated_diluted_Tx_warning <- NULL
#     output$one_pass_acceptable_Tx_warning <- NULL
#     output$two_plus_pass_acceptable_Tx_warning <- NULL
#     output$mean_mfi_warning <- NULL
#     output$is_log_mfi_warning <- NULL
#     output$apply_prozone_warning <- NULL
#     output$blank_option_warning <- NULL
#     output$default_source_warning <- NULL
#     output$reference_arm_warning <- NULL
#     output$timeperiod_order_warning <- NULL
#     #output$user_parameter_download <- NULL
#
#
#
#   }
# })
