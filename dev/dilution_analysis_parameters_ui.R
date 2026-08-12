# # tags$head(
# #   tags$style(HTML("
# #     .datatables {
# #       width: 100% !important;
# #       margin: 0 !important;
# #     }
# #     .dataTables_wrapper {
# #       width: 100%;
# #       margin: 0 auto;
# #       padding: 0 10px;
# #     }
# #     .panel-collapse {
# #       padding: 15px;
# #     }
# #   "))
# # )
#
# tags$style(HTML('
#   #hansomeContainer {
#     height: 500px;  /* Adjust this height as needed */
#     overflow-y: auto; /* Make sure scrolling is allowed if content overflows */
#   }
# '))
#
# # tags$head(HTML("
# #     .handsontable {
# #       border-collapse: collapse;
# #       width: 100%;
# #     }
# #     .handsontable th, .handsontable td {
# #       border: 1px solid #ddd;
# #       padding: 8px;
# #       text-align: center;
# #     }
# #     .handsontable tr:nth-child(even) {
# #       background-color: #f2f2f2;
# #     }
# #     .handsontable tr:hover {
# #       background-color: #ddd;
# #     }
# #     .handsontable .htCenter {
# #       text-align: center;
# #     }
# #     .handsontable .htMiddle {
# #       vertical-align: middle;
# #     }
# #   "))
#
# standard_curve_table_list <- list()
#
# data_summary <- list()
#
# std_curve_tab_active <- reactiveVal(FALSE)
#
# fetch_dilution_parameters <- function(study_accession) {
#   if (study_accession == "reset") {
#     return(NULL)
#   }
#
#   query <- paste0("
#     SELECT xmap_dilution_parameters_id, study_accession, node_order, valid_gate_class, is_binary_gate
#     FROM madi_results.xmap_dilution_parameters
#     WHERE study_accession = '", study_accession, "'")
#
#   result_df <- dbGetQuery(conn, query)
#
#   if (nrow(result_df) == 0 || is.null(result_df)) {
#     insert_query <- paste0("
#       INSERT INTO madi_results.xmap_dilution_parameters (
#         study_accession, node_order, valid_gate_class, is_binary_gate
#       )
#       SELECT DISTINCT
#         s.study_accession,
#         'linear' AS node_order,
#         'Between_Limits' AS valid_gate_class,
#          FALSE AS is_binary_gate
#       FROM madi_results.xmap_sample s
#       WHERE s.study_accession = '", study_accession, "'
#       LIMIT 1;")
#
#     dbExecute(conn, insert_query)
#
#     result_df <- dbGetQuery(conn, query)
#   }
#
#   return(result_df)
# }
#
# # fetch_dilution_parameters <- function(study_accession) {
# #   if (study_accession == "reset") {
# #     result_df <- NULL
# #   } else {
# #     query <- paste0("
# #       SELECT xmap_dilution_parameters_id, study_accession, node_order, valid_gate_class
# #       FROM madi_results.xmap_dilution_parameters
# #       WHERE study_accession = '", study_accession, "'")
# #
# #     # Run the query and fetch the result as a data frame
# #     result_df <- dbGetQuery(conn, query)
# #
# #     if (nrow(result_df) == 0 || is.null(result_df)) {
# #       # If no entry, insert default values for this study
# #       insert_query <- paste0("
# #         INSERT INTO madi_results.xmap_dilution_parameters (
# #           study_accession, node_order, valid_gate_class
# #         )
# #         SELECT DISTINCT
# #           '", study_accession, "' AS study_accession,
# #           'linear' AS node_order,
# #           'Between_Limits' AS valid_gate_class
# #         FROM madi_results.xmap_sample s
# #         WHERE s.study_accession = '", study_accession, "'
# #         LIMIT 1;")  # Ensures only one row is inserted
# #
# #       dbExecute(conn, insert_query)
# #
# #       # Re-fetch the inserted row
# #       result_df <- dbGetQuery(conn, query)
# #     } else {
# #       # Update the existing records only if node_order is not 'linear'
# #       update_query <- paste0("
# #         UPDATE madi_results.xmap_dilution_parameters
# #         SET node_order = 'linear', valid_gate_class = 'Between_Limits'
# #         WHERE study_accession = '", study_accession, "'
# #           AND node_order != 'linear';")
# #
# #       dbExecute(conn, update_query)
# #
# #       # Add new dilution parameters if not already present
# #       insert_query <- paste0("
# #         INSERT INTO madi_results.xmap_dilution_parameters (
# #           study_accession, node_order, valid_gate_class
# #         )
# #         SELECT DISTINCT
# #           s.study_accession,
# #           'linear' AS node_order,
# #           'Between_Limits' AS valid_gate_class
# #         FROM madi_results.xmap_sample s
# #         WHERE s.study_accession = '", study_accession, "'
# #           AND NOT EXISTS (
# #             SELECT 1 FROM madi_results.xmap_dilution_parameters d
# #             WHERE d.study_accession = s.study_accession
# #               AND d.node_order = 'linear'  -- Ensure we add only distinct nodes
# #           );")
# #
# #       dbExecute(conn, insert_query)
# #
# #       # Re-fetch updated data
# #       result_df <- dbGetQuery(conn, query)
# #     }
# #   }
# #
# #   return(result_df)
# # }
#
# # observeEvent(input$dilution_parameters_table_cell_edit, {
# #   # Get the information about the edit event
# #   info <- input$dilution_parameters_table_cell_edit
# #
# #   # Extract the edited row, column, and new value
# #   row_num <- info$row
# #   col_num <- info$col
# #   new_value <- as.character(info$value)  # New value entered in the cell
# #
# #   # Convert the rhandsontable to a data frame
# #   edited_df <<- hot_to_r(input$dilution_parameters_table)
# #
# #   # Update the relevant row/column in the data frame
# #   edited_df[row_num, col_num] <- new_value
# #
# #   # Update the reactive value with the modified data frame
# #   dilution_analysis_params_rv(edited_df)
# #
# #   # Optionally, notify the user
# #   showNotification("Table updated successfully!", type = "message")
# #
# #   # If necessary, update the database here (this step is optional)
# #   # For example, you can execute a SQL update query here based on the modified value.
# # })
#
# # observeEvent(input$dilution_parameters_table_cell_edit, {
# #   info <- input$dilution_parameters_table_cell_edit
# #   row_num <- info$row
# #   col_num <- info$col
# #   new_value <- as.character(info$value)
# #   current_data <- antigen_families_rv()
# #   row_id <- current_data$xmap_antigen_family_id[row_num]
# #
# #   update_query <- "UPDATE madi_results.xmap_antigen_family
# #                       SET antigen_family = $1
# #                       WHERE xmap_antigen_family_id = $2"
# #
# #   # tryCatch({
# #   #   dbExecute(conn, update_query, params = list(new_value, row_id))
# #   #
# #   #   # Update the reactive value with new data
# #   #   current_data$antigen_family[row_num] <- new_value
# #   #   antigen_families_rv(current_data)
# #   #
# #   #   showNotification(
# #   #     "Antigen Family updated successfully",
# #   #     type = "message"
# #   #   )
# #   # }, error = function(e) {
# #   #   showNotification(
# #   #     paste("Error updating antigen family:", e$message),
# #   #     type = "error"
# #   #   )
# #   # })
# # })
#
# # render Dilution parameters
# render_dilution_parameters <- reactive({
#   req(input$readxMap_study_accession)
#
#   # Get selected study
#   selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)
#
#   # Fetch data once
#   dilution_analysis_params_rv(fetch_dilution_parameters(selected_study))
#
#   # Debug output
#   cat("Dilution Parameters obtained for:", selected_study, "\n")
#   print(head(dilution_analysis_params_rv()))
#
#   # Render the instructions
#   # output$antigen_family_instructions <- renderUI({
#   #   req(selected_study)
#   #   HTML(paste("<span style='font-size: 1.5em;'>To edit the family of an antigen for",
#   #              selected_study,
#   #              "click on the cell in the Antigen Family column.",
#   #              "After making the change, click anywhere outside the cell to save the update.<br></span>"))
#   # })
#
# output$dilution_paramatersUI <- renderUI({
#  req(dilution_analysis_params_rv(), selected_study)
#   tagList(
#     fluidRow(
#      "Set parameters",
#      div(id = "hansomeContainer",rHandsontableOutput("dilution_parameters_table"))
#     )
#   )
# })
#
# output$dilution_parameters_table <- renderRHandsontable({
#   node_order_choices <- c(
#     "gate", "linear", "quantifiable",
#     "gate, linear", "linear, gate",
#     "gate, quantifiable", "quantifiable, gate",
#     "linear, quantifiable", "quantifiable, linear",
#     "gate, linear, quantifiable", "gate, quantifiable, linear",
#     "linear, gate, quantifiable", "linear, quantifiable, gate",
#     "quantifiable, gate, linear", "quantifiable, linear, gate"
#   )
#
#   valid_gate_class_choices <- c("Between_Limits", "Above_Upper_Limit", "Below_Lower_Limit")
#
#   table <- dilution_analysis_params_rv()
#   table_v <- table  # Optional: only if needed globally
#
#   table[] <- lapply(table, as.character)
#   table$is_binary_gate <- as.logical(table$is_binary_gate)  # Ensure correct type for check box
#
#   rh <- rhandsontable(table, useTypes = TRUE, stretchH = "all", rowHeaders = NULL) %>%
#     hot_col("node_order", type = "dropdown", source = node_order_choices) %>%
#     hot_col("valid_gate_class", type = "dropdown", source = valid_gate_class_choices) %>%
#     hot_col("xmap_dilution_parameters_id", readOnly = TRUE ) %>%
#     hot_col("is_binary_gate", type = "checkbox")
#
#   rh %>% hot_table(width = "100%", height = "auto")
#   rh
#
# })
#
# observe({
#   table <- dilution_analysis_params_rv()
#   last_saved_dilution_params(table)
# })
#
#
# observeEvent(input$dilution_parameters_table, {
#   req(dilution_analysis_params_rv())
#   old_df <- last_saved_dilution_params()
#
#   table <- input$dilution_parameters_table
#   if (!is.null(table)) {
#     new_df <- hot_to_r(table)  # Converts the table to a data frame
#
#     new_df <- new_df[order(new_df$xmap_dilution_parameters_id), ]
#     old_df <- old_df[order(old_df$xmap_dilution_parameters_id), ]
#
#     # Identify changed rows
#     changed_rows <- which(
#       new_df$node_order != old_df$node_order |
#         new_df$valid_gate_class != old_df$valid_gate_class |
#         new_df$is_binary_gate != old_df$is_binary_gate
#     )
#
#
#     ## Update database
#     if (length(changed_rows) > 0) {
#       for (i in changed_rows) {
#         id <- new_df$xmap_dilution_parameters_id[i]
#         node_order <- new_df$node_order[i]
#         gate_class <- new_df$valid_gate_class[i]
#         is_binary_gate <- new_df$is_binary_gate[i]
#
#         DBI::dbExecute(conn, "
#         UPDATE madi_results.xmap_dilution_parameters
#         SET node_order = $1,
#             valid_gate_class = $2,
#             is_binary_gate = $4
#         WHERE xmap_dilution_parameters_id = $3
#       ",
#                        params = list(node_order, gate_class, id, is_binary_gate))
#       }
#
#       # update the data set to the reactiveVal for the next comparison
#       last_saved_dilution_params(new_df)
#
#       showNotification(
#         paste("Dilution Parameters updated successfully."),
#         type = "message")
#     }
#
#   }
# })
#
# # observe({
# #   table <- input$dilution_parameters_table
# #   if (!is.null(table)) {
# #     df <- hot_to_r(table)  # Converts the table to a data frame
# #     df_view <<- df
# #     # You can now use 'df' as needed, e.g., for further processing
# #
# #   }
# # })
#
#
#
#
#
# # output$dilution_parameters_table <- renderDT({
# #   req(dilution_analysis_params_rv())
# #   cat("Rendering datatable\n")
# #   node_order_choices <- c(
# #     "gate",
# #     "linear",
# #     "quantifiable",
# #     "gate, linear",
# #     "linear, gate",
# #     "gate, quantifiable",
# #     "quantifiable, gate",
# #     "linear, quantifiable",
# #     "quantifiable, linear",
# #     "gate, linear, quantifiable",
# #     "gate, quantifiable, linear",
# #     "linear, gate, quantifiable",
# #     "linear, quantifiable, gate",
# #     "quantifiable, gate, linear",
# #     "quantifiable, linear, gate"
# #   )
# #   valid_gate_class_choices <- c("Between_Limits", "Above_Upper_Limit", "Below_Lower_Limit")
# #
# #   datatable(dilution_analysis_params_rv(),
# #             options = list(
# #               pageLength = 50,
# #               scrollX = TRUE,
# #               scrollY = "400px",
# #               autoWidth = TRUE,  # Added this
# #               responsive = TRUE, # Added this
# #               order = list(list(0, 'asc')),
# #               columnDefs = list(
# #                 list(className = 'dt-center', targets = '_all')
# #               )
# #             ),
# #             editable = list(
# #               target = 'cell',
# #               disable = list(columns = c(0:1)),
# #               columns = list(
# #                 node_order = list(type = "select", options = node_order_choices),
# #                 valid_gate_class = list(type = "select", options = valid_gate_class_choices)
# #               )
# #             ),
# #             selection = 'none',
# #             class = 'cell-border stripe hover'  # Added styling classes
# #   ) %>%
# #     formatStyle(columns = 1:ncol(dilution_analysis_params_rv()),  # Added column styling
# #                 backgroundColor = 'white',
# #                 borderBottom = '1px solid #ddd')
# # })
#
# })
#
# # observeEvent(input$dilution_parameters_table, {
# #   edited_df <- hot_to_r(input$dilution_parameters_table)
# #   dilution_analysis_params_rv(edited_df)
# # })
#
# # Handle initial page load
# observe({
#   req(input$readxMap_study_accession)
#   if (input$study_level_tabs == "Study Overview") {
#     cat("Initial load of Study Overview tab\n")
#     render_dilution_parameters()
#   }
# })
#
# # Handle tab changes
# observeEvent(input$study_level_tabs, {
#   req(input$readxMap_study_accession)
#   if (input$study_level_tabs == "Study Overview") {
#     cat("Tab changed to Study Overview\n")
#     render_dilution_parameters()
#   }
# })
#
# # Handle study selection changes
# observeEvent(input$readxMap_study_accession, {
#   if (input$study_level_tabs == "Study Overview") {
#     cat("Study selection changed\n")
#     render_dilution_parameters()
#   }
# })
#
