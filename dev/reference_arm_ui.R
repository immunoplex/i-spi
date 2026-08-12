# tags$head(
#   tags$style(HTML("
#     .datatables {
#       width: 100% !important;
#       margin: 0 !important;
#     }
#     .dataTables_wrapper {
#       width: 100%;
#       margin: 0 auto;
#       padding: 0 10px;
#     }
#     .panel-collapse {
#       padding: 15px;
#     }
#   "))
# )
#
# standard_curve_table_list <- list()
#
# data_summary <- list()
#
# std_curve_tab_active <- reactiveVal(FALSE)
#
#
# fetch_reference_arm_table <- function(study_accession) {
#   if (study_accession == "reset") {
#     result_df <- NULL
#   } else {
#     query <- paste0("
#       SELECT xmap_arm_reference_id, study_accession, agroup, referent
#       FROM madi_results.xmap_arm_reference
#       WHERE study_accession = '", study_accession, "'")
#
#     # Run the query and fetch the result as a data frame
#     result_df <- dbGetQuery(conn, query)
#   }
#   #   if (nrow(result_df) == 0 || is.null(result_df)) {
#   #     # check if result df is null or 0 rows and if so run additional query insert.
#   #     insert_query <- paste0("INSERT INTO madi_results.xmap_antigen_family(
#   #               	study_accession, antigen, antigen_family)
#   #               	SELECT DISTINCT '",study_accession,"', antigen, 'All Antigens'
#   #               	FROM madi_results.xmap_sample
#   #               	WHERE study_accession = '", study_accession, "'
#   #               	ORDER BY antigen;")
#   #     dbExecute(conn, insert_query)
#   #     result_df <- dbGetQuery(conn, query)
#   #   }
#   # }
#   return(result_df)
# }
#
# observeEvent(input$reference_arm_table_cell_edit, {
#   info <- input$reference_arm_table_cell_edit
#   row_num <- info$row
#   col_num <- info$col
#   new_value <- as.character(info$value)
#
#   if (!(tolower(new_value) %in% c("true", "false"))) {
#     showNotification("Please enter true or false (case insensitive)",
#                      type = "error"
#     )
#     return()
#   }
#   current_data <- reference_arm_rv() # antigen_families_rv()
#   row_id <- current_data$xmap_arm_reference_id[row_num]
#
#   update_query <- "UPDATE madi_results.xmap_arm_reference
#                       SET referent = $1
#                       WHERE xmap_arm_reference_id = $2"
#
#   tryCatch({
#     dbExecute(conn, update_query, params = list(new_value, row_id))
#
#     # Update the reactive value with new data
#     current_data$referent[row_num] <- new_value
#     reference_arm_rv(current_data)
#
#     showNotification(
#       "Referent arm updated successfully",
#       type = "message"
#     )
#   }, error = function(e) {
#     showNotification(
#       paste("Error updating reference arm:", e$message),
#       type = "error"
#     )
#   })
# })
#
# # The main rendering logic wrapped in a reactive expression
# render_reference_arm <- reactive({
#   req(input$readxMap_study_accession)
#
#   # Get selected study
#   selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)
#
#   # Fetch data once
#   reference_arm_rv(fetch_reference_arm_table(selected_study))
#
#   # Debug output
#   cat("Reference arm data updated for:", selected_study, "\n")
#   print(head(reference_arm_rv()))
#
#
#   # Render the data table
#   output$reference_arm_UI <- renderUI({
#     req(reference_arm_rv(), selected_study)
#     tagList(
#       fluidRow(
#         div(
#           style = "width: 100%; padding: 0 15px;",  # Added container styling
#           span(
#             div(style = "display:inline-block; margin-bottom: 10px;",
#                 title = "Info",
#                 icon("info-circle", class = "fa-lg", `data-toggle` = "tooltip",
#                      `data-placement` = "right",
#                      title = paste("To edit the reference arm for", selected_study,
#                                    " select the radio button in the referent column for desired reference arm. "),
#                      `data-html` = "true")
#             )
#           ),
#           br(),
#           div(
#             style = "width: 100%; overflow-x: auto;",  # Added wrapper for table
#             DTOutput("reference_arm_table")
#           )
#         )
#       )
#     )
#   })
#
#   createRadioButtons <- function(name, selected, row_id) {
#     as.character(
#       tags$input(
#         type = "radio",
#         name = name,
#         value = row_id,
#         checked = ifelse(selected, "checked", NA),
#         onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", name, row_id)
#       )
#     )
#   }
#
#   table_with_radios <- reactive({
#     req(input$readxMap_study_accession)
#
#     # Get selected study
#     selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)
#
#     reference_arm_rv(fetch_reference_arm_table(selected_study))
#     df <- reference_arm_rv()
#     # if (!any(df$referent) == FALSE) {
#     #   df$referent[1] <- TRUE
#     # }
#     df$referent <- as.integer(df$referent)
#
#     df$referent <- mapply(createRadioButtons, "referent_select", df$referent, seq_len(nrow(df)))
#     df
#   })
#
#   # And modify your renderDT to be more responsive:
#   output$reference_arm_table <- renderDT({
#     req(reference_arm_rv())
#     cat("Rendering datatable\n")
#
#     # radioButtons("selected_row", "Select Row:",
#     #              choices = 1:nrow(reference_arm_rv()),
#     #              selected = 1)
#
#     # selected_row <- if (any(reference_arm_rv()$referent)) {
#     #   which(reference_arm_rv()$referent == TRUE)[1] - 1  # JavaScript is 0-based
#     # } else {
#     #   0  # Default to the first row
#     # }
#
#     #for (col in c("referent")) {
#     #   reference_arm_rv()[[col]] <- sprintf(
#     #     '<input type="radio" name="%s" value="%s" onclick="updateSelection(\'%s\', \'%s\')"/>',
#     #     col, reference_arm_rv()[['Month']], col, reference_arm_rv()[['Month']]
#     #   )
#     # }
#     # m <- reference_arm_rv()
#     #  m$referent <- sprintf(
#     #   '<input type="radio" name="referent" value="%s" onclick="updateSelection(\'%s\')"/>',
#     #   m$reference_arm_id, m$reference_arm_id
#     # )
#     # reference_arm_rv(
#     #   transform(
#     #     reference_arm_rv(),
#     #     referent = mapply(createRadioButtons, "referent_select", referent, seq_len(nrow(reference_arm_rv())))
#     #   )
#     # )
#
#     datatable(table_with_radios(),
#               escape = F,
#               options = list(
#                 pageLength = 50,
#                 scrollX = TRUE,
#                 scrollY = "400px",
#                 autoWidth = TRUE,  # Added this
#                 responsive = TRUE, # Added this
#                 order = list(list(0, 'asc')),
#                 columnDefs = list(
#                   list(className = 'dt-center', targets = '_all')
#                   # list(targets = 4, render = JS(
#                   #   "function(data, type, row, meta) {",
#                   #   "  return '<input type=\"radio\" name=\"selected_row\" value=\"' + data + '\"' + (meta.row === 0 ? ' checked' : '') + ' />';",
#                   #   "}"
#                   # ))
#
#                   # list(targets = 4, createdCell = JS(
#                   #   "function(td, cellData, rowData, row, col) {",
#                   #   "  var rowIndex = row;",
#                   #   "  var selectedRow = ", selected_row, ";",
#                   #   "  var checked = (rowIndex === selectedRow) ? 'checked' : '';",
#                   #   "  var radioButton = '<input type=\"radio\" name=\"referenceArm\" value=\"' + rowIndex + '\" ' + checked + ' onclick=\"Shiny.setInputValue(\'referenceArm\', this.value, {priority: \'event\'});\" />';",
#                   #   "  $(td).html(radioButton);",
#                   #   "}")
#
#
#                   # "function(td, cellData, rowData, row, col) {",
#                   # "  var rowIndex = row;",
#                   # "  var selectedRow = ", selected_row, ";",
#                   # "  $(td).html('<input type=\"radio\" name=\"referenceArm\" ' + (rowIndex === selectedRow ? 'checked' : '') + ' />');",
#                   # "}")
#
#
#
#
#                   # "function(td, cellData, rowData, row, col) {",
#                   # "  $(td).html('<input type=\"radio\" name=\"referenceArm\" ' + (cellData === true ? 'checked' : '') + ' />');",
#                   # "}")
#                   # "function(td, cellData, rowData, row, col) {",
#                   # "  if (cellData === true) {",
#                   # "    $(td).html('<input type=\"radio\" checked />');",
#                   # "  } else {",
#                   # "    $(td).html('<input type=\"radio\" />');",
#                   # "  }",
#                   # "}")
#                 )
#
#               ),
#               editable = list(
#                 target = 'cell',
#                 disable = list(columns = c(0:3))
#               ),
#               selection = 'none',
#               class = 'cell-border stripe hover'  # Added styling classes
#     ) %>%
#       formatStyle(columns = 1:ncol(reference_arm_rv()),  # Added column styling
#                   backgroundColor = 'white',
#                   borderBottom = '1px solid #ddd')
#   })
# })
#
# # observe({
# #   # Only run once after the UI is fully rendered
# #   updateRadioButtons(session, "referent_select", selected = 1)
# # })
#
# observe({
#   selected_value <- input$referent_select
#   if (is.null(selected_value)) {
#     print("none selected yet")
#    # updateRadioButtons(session, "referent_select", selected = 1)
#   } else {
#    print(selected_value)
#   }
# })
#
#
# observe({
#   req(reference_arm_rv())
#   updated_reference_arm <- reference_arm_rv()
#   if (all(updated_reference_arm$referent == FALSE)) {
#     updated_reference_arm[1, "referent"] <- TRUE
#
#     selected_study_accession <- unique(updated_reference_arm$study_accession)
#     selected_agroup <- updated_reference_arm[updated_reference_arm$referent == T,]$agroup
#
#     query_update_referent <- paste0("UPDATE madi_results.xmap_arm_reference
#                              SET referent = TRUE
#                              WHERE study_accession = '", selected_study_accession, "' AND agroup = '", selected_agroup, "';")
#     # Set the selected row to TRUE
#     dbExecute(conn, query_update_referent)
#
#     reference_arm_selected <- updated_reference_arm[updated_reference_arm$referent == TRUE, "agroup", drop = FALSE]
#     reference_arm(reference_arm_selected)
#
#   } else {
#     print("Not all referents are false. Do nothing.")
#   }
#
# })
#
# observeEvent(input$referent_select, {
#   req(reference_arm_rv())
#   if (is.data.frame(reference_arm_rv())) {
#     updated_reference_arm <- reference_arm_rv()
#
#     updated_reference_arm$referent <- FALSE
#     updated_reference_arm[input$referent_select, "referent"] <- TRUE
#     #reference_arm_rv(updated_reference_arm)
#     selected_study_accession <- unique(updated_reference_arm$study_accession)
#     selected_agroup <- updated_reference_arm[updated_reference_arm$referent == T,]$agroup
#
#     query <- paste0("UPDATE madi_results.xmap_arm_reference SET referent = FALSE WHERE study_accession = '", selected_study_accession, "';")
#
#     dbExecute(conn, query)
#
#     query_2 <- paste0("UPDATE madi_results.xmap_arm_reference
#                              SET referent = TRUE
#                              WHERE study_accession = '", selected_study_accession, "' AND agroup = '", selected_agroup, "';")
#      # Set the selected row to TRUE
#      dbExecute(conn, query_2)
#
#
#
#     reference_arm_selected <- updated_reference_arm[updated_reference_arm$referent == TRUE, "agroup", drop = FALSE]
#     reference_arm_selected <- as.character(reference_arm_selected)
#     reference_arm(reference_arm_selected)
#   } else {
#     reference_arm(NULL)  # Reset if it is not a df
#   }
# })
#
# # observe({
# #   if (is.null(input$referent_select)) {
# #     # Set the referent_select value to 1 on load
# #     shinyjs::runjs("$('#referent_select').val(1).trigger('change');")
# #   }
# #  # pr
# #   #selected_value <- input$referent_select
# #   print(selected_value)  # This will print the selected value in the console
# # })
#
# # observeEvent(reference_arm_rv(), {
# #   df <- reference_arm_rv()
# #
# #   # Find the first TRUE value index
# #   selected_row <- which(df$referent)[1]
# #
# #   # Set the input value in Shiny
# #   if (!is.null(selected_row)) {
# #     shinyjs::runjs(sprintf("Shiny.setInputValue('referent_select', %d, {priority: 'event'})", selected_row))
# #   }
# # }, once = TRUE)
# # Extract the reference arm
# # observeEvent(input$referenceArm, {
# #   req(reference_arm_rv())  # Ensure data is available
# #  # req(input$referenceArm)
# #
# #   # If no radio button is clicked, default to the first row convert back to R index.
# #   selected_row <- if (is.null(input$referenceArm)) 1 else as.integer(input$referenceArm) + 1
# #
# #   # Ensure valid selection
# #   if (!is.null(selected_row) && selected_row > 0 && selected_row <= nrow(reference_arm_rv())) {
# #
# #     # Get study_accession and agroup for the selected row
# #     selected_study_accession <- reference_arm_rv()$study_accession[selected_row]
# #     selected_agroup <- reference_arm_rv()$agroup[selected_row]
# #
# #     # Prevent unnecessary updates
# #     current_selected <- reference_arm_rv()[reference_arm_rv()$referent == TRUE, "agroup", drop = TRUE]
# #
# #     if (length(current_selected) == 0 || current_selected != selected_agroup) {
# #
# #       isolate({
# #         # Set all referent values to FALSE
# #         dbExecute(conn, paste0("UPDATE madi_results.xmap_arm_reference
# #                          SET referent = FALSE
# #                          WHERE study_accession = '", selected_study_accession, "';"))
# #
# #         # Set the selected row to TRUE
# #         dbExecute(conn, "UPDATE madi_results.xmap_arm_reference
# #                          SET referent = TRUE
# #                          WHERE study_accession = '", selected_study_accession, "AND agroup = '", selected_agroup, ";")
# #
# #       })
# #
# #       # Reload updated data
# #       updated_data <- dbGetQuery(conn, "SELECT xmap_arm_reference_id, study_accession, agroup, referent
# #                                         FROM madi_results.xmap_arm_reference
# #                                         WHERE study_accession = '",selected_study_accession, "';")
# #
# #
# #       # Update the reactive value
# #       reference_arm_rv(updated_data)
# #
# #       # Update the reference arm selection
# #       reference_arm_selected <- updated_data[updated_data$referent == TRUE, "agroup", drop = FALSE]
# #       reference_arm(as.character(reference_arm_selected))
# #     }
# #   }
# # })
#
# # Observer to update reference arm selection and modify the database
# # observeEvent(input$referenceArm, {
# #   req(reference_arm_rv())  # Ensure data is available
# #
# #   # Get the newly selected row index from the radio button
# #   selected_row <<- as.integer(input$referenceArm)
# #
# #   # Ensure the selected row exists
# #   if (!is.null(selected_row) && selected_row > 0 && selected_row <= nrow(reference_arm_rv())) {
# #
# #     # Get the study_accession and agroup for the selected row
# #     selected_study_accession <- reference_arm_rv()$study_accession[selected_row]
# #     selected_agroup <- reference_arm_rv()$agroup[selected_row]
# #
# #     # Check if the selection has changed to prevent unnecessary updates
# #     current_selected <- reference_arm_rv()[reference_arm_rv()$referent == TRUE, "agroup", drop = TRUE]
# #
# #     if (length(current_selected) == 0 || current_selected != selected_agroup) {
# #
# #       isolate({
# #         # Set all referent values to FALSE for the study
# #         dbExecute(conn, "UPDATE madi_results.xmap_arm_reference
# #                          SET referent = FALSE
# #                          WHERE study_accession = ?",
# #                   params = list(selected_study_accession))
# #
# #         # Set the selected row to TRUE
# #         dbExecute(conn, "UPDATE madi_results.xmap_arm_reference
# #                          SET referent = TRUE
# #                          WHERE study_accession = ? AND agroup = ?",
# #                   params = list(selected_study_accession, selected_agroup))
# #       })
# #
# #       # Reload the updated data from the database
# #       updated_data <- dbGetQuery(conn, "SELECT xmap_arm_reference_id, study_accession, agroup, referent
# #                                         FROM madi_results.xmap_arm_reference
# #                                         WHERE study_accession = ?",
# #                                  params = list(selected_study_accession))
# #
# #       # Update the reactive value
# #       reference_arm_rv(updated_data)
# #
# #       # Update the reference arm selection
# #       reference_arm_selected <- updated_data[updated_data$referent == TRUE, "agroup", drop = FALSE]
# #       reference_arm(as.character(reference_arm_selected))
# #     }
# #   }
# # })
#
#
# # observe({
# #   req(reference_arm_rv())
# #   for (i in 1:nrow(reference_arm_rv())) {
# #     if (input[[paste0("row_", i, "_true")]]) {
# #       reference_arm_rv()$referent[i] <- TRUE
# #     } else if (input[[paste0("row_", i, "_false")]]) {
# #       reference_arm_rv()$referent[i] <- FALSE
# #     }
# #   }
# #
# #   # Access row(s) where IsImportant is TRUE
# #   true_rows <- reference_arm_rv()[reference_arm_rv()$referentt == TRUE, ]
# #   cat(true_rows)
# # })
#
#
# # observe({
# #   updateRadioButtons(session, "selected_row", selected = 1)
# # })
#
# # observe({
# #   req(reference_arm_rv())  # Ensure reference_arm_rv() is available
# #   req(input$selected_row)
# #   if (is.data.frame(reference_arm_rv())) {
# #     # Get the selected row from the radio buttons
# #     selected_row_index <<- input$selected_row  # Get the selected radio button value (row index)
# #
# #   #   if (!is.null(selected_row_index)) {
# #   #     # Create a copy of the reference_arm_rv() data
# #   #     updated_df <- reference_arm_rv()
# #   #
# #   #     # Set all rows in 'referent' column to FALSE first
# #   #     updated_df$referent <- FALSE
# #   #
# #   #     # Set 'referent' to TRUE for the selected row
# #   #     updated_df[selected_row_index, "referent"] <- TRUE
# #   #
# #   #     cat("updated_df")
# #   #     print(updated_df)
# #   #     # Update the data with the new values
# #   #     reference_arm_rv(updated_df)
# #   #
# #   #     # # Optionally update other UI elements if needed
# #   #     reference_arm(as.character(updated_df[updated_df$referent == TRUE, "agroup", drop = FALSE]))
# #   #   }
# #   # } else {
# #   #   reference_arm(NULL)  # Reset if it is not a df
# #   }
# # })
# #
# #
#
# # observeEvent(input$referent_select, {
# #   df <- reference_arm_rv()
# #   df$referent <- FALSE  # Reset all to FALSE
# #   df$referent[input$referent_select] <- TRUE  # Set selected row to TRUE
# #   reference_arm_rv(df)  # Update reactive values
# # })
#
# #
# # observe({
# #   req(reference_arm_rv())
# #   if (is.data.frame(reference_arm_rv())) {
# #     reference_arm_selected <- reference_arm_rv()[reference_arm_rv()$referent == TRUE, "agroup", drop = FALSE]
# #     reference_arm_selected <- as.character(reference_arm_selected)
# #     reference_arm(reference_arm_selected)
# #   } else {
# #     reference_arm(NULL)  # Reset if it is not a df
# #   }
# # })
#
#
#
# # Handle initial page load
# observe({
#   req(input$readxMap_study_accession)
#   if (input$study_level_tabs == "Study Overview") {
#     cat("Initial load of Study Overview tab\n")
#     render_reference_arm()
#   }
# })
#
# # Handle tab changes
# observeEvent(input$study_level_tabs, {
#   req(input$readxMap_study_accession)
#   if (input$study_level_tabs == "Study Overview") {
#     cat("Tab changed to Study Overview\n")
#     render_reference_arm()
#   }
# })
#
# # Handle study selection changes
# observeEvent(input$readxMap_study_accession, {
#   if (input$study_level_tabs == "Study Overview") {
#     cat("Study selection changed\n")
#     render_reference_arm()
#   }
# })
#
#
#
#
#
# # observeEvent(input$study_level_tabs, {
# #   if (input$study_level_tabs == "Study Overview") {
# #
# #     selected_study <- selected_studyexpplate$study_accession
# #     selected_experiment <- selected_studyexpplate$experiment_accession
# #
# #     # Load sample data
# #     sample_data <- stored_plates_data$stored_sample
# #     # Check if selected study, experiment, and sample data are available
# #     if (!is.null(selected_study) && length(selected_study) > 0 &&
# #         !is.null(selected_experiment) && length(selected_experiment) > 0 &&
# #         !is.null(sample_data) && length(sample_data) > 0){
# #
# #       # Filter sample data
# #       sample_data$selected_str <- paste0(sample_data$study_accession, sample_data$experiment_accession)
# #       sample_data <- sample_data[sample_data$selected_str == paste0(selected_study, selected_experiment), ]
# #
# #       # Summarize sample data
# #       cat("Viewing sample dat in subgroup tab ")
# #       print(names(sample_data))
# #       print(table(sample_data$plateid))
# #       print(table(sample_data$antigen))
# #       cat("After summarizing sample data in subgroup tab")
# #
# #
# #       # Rename columns
# #
# #       sample_data <- dplyr::rename(sample_data, arm_name = agroup)
# #       sample_data <- dplyr::rename(sample_data, visit_name = timeperiod)
# #
# #
# #       sample_data$subject_accession <- sample_data$patientid
# #
# #       sample_data <- dplyr::rename(sample_data, value_reported = mfi)
# #
# #       arm_choices <- unique(sample_data$arm_name)
# #       cat("after arm choices ")
# #       visits <- unique(sample_data$visit_name)
# #
# #     }
# #
# #     cat("Loaded Sample data")
# #
# #     output$reference_arm_UI <- renderUI(
# #      # "select reference arm"
# #        uiOutput("select_reference_arm")
# #     )
# #
# #     output$sample_data_table <- renderTable({
# #       req(sample_data)
# #       sample_data
# #     })
# #
# #
# #     fetch_arms <- function(study_accession) {
# #       if (study_accession == "reset") {
# #         result_df <- NULL
# #       } else {
# #         query <- paste0("SELECT study_accession, agroup
# #                         FROM madi_results.xmap_sample
# #                         WHERE study_accession = '", study_accession, "'
# #                         GROUP BY study_accession, agroup;")
# #
# #         # Run the query and fetch the result as a data frame
# #         result_df <- dbGetQuery(conn, query)
# #
# #       }
# #       return(result_df)
# #     }
# #
# #     arm_choices_reactive <- reactive({
# #       req(input$readxMap_study_accession)
# #            # Get selected study
# #            selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)
# #            fetch_arms(selected_study)
# #     })
# #
# #     output$select_reference_arm <- renderUI({
# #       choices <- arm_choices_reactive()$agroup # Get arm choices dynamically
# #       selectInput(
# #         inputId = "selectReferenceArm",
# #         label = "Reference Arm",
# #         choices = choices,
# #         selected = reference_arm()
# #       )
# #     })
# #     observeEvent(input$selectReferenceArm, {
# #       reference_arm(input$selectReferenceArm)
# #     })
# #
# #     # output$select_reference_arm<- renderUI({
# #     #   req(arm_choices)
# #     #   selectInput(
# #     #     inputId = "selectReferenceArm",
# #     #     label = "Reference Arm",
# #     #     choices = arm_choices
# #     #   )
# #     # })
# #   } # end if in tab
# # })
