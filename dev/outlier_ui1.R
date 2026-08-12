hidden <- function(x) {
  tags$div(style = "display: none;", x)
}

useShinyjs()
tags$head(
  tags$script("
    Shiny.addCustomMessageHandler('triggerDownload', function(message) {
      var link = document.getElementById(message);
      if(link) link.click();
    });
  ")
)


current_data <- reactiveValues(
  study = NULL,
  experiment = NULL,
  comparisons_data = NULL,
  ui_loaded = FALSE
)

checkExistingValueTypes <- function(study, experiment, workspace_id) {
  query <- "
    SELECT DISTINCT value_type, job_status
    FROM madi_lumi_reader_outliers.main_context
    WHERE study = $1
    AND experiment = $2
    AND workspace_id = $3
    ORDER BY value_type"  # Added ORDER BY for consistent results

  existing_data <- dbGetQuery(conn, query,
                              params = list(study, experiment, workspace_id))
  return(existing_data)
}



outlierExists <- function(study, experiment, workspace_id, value_type){

  check_query <- "SELECT id FROM madi_lumi_reader_outliers.main_context
                  WHERE study = $1 AND experiment = $2 AND workspace_id = $3 AND value_type = $4"

  context_id <- dbGetQuery(conn, check_query, params = list(study, experiment, workspace_id, value_type))

  return (context_id)

}


preLoadData <- function(context_id) {

  context_id <- context_id$id[1]

  comparisons_query <- "SELECT * FROM madi_lumi_reader_outliers.comparisons
                          WHERE context_id = $1"
  comparisons_data <- dbGetQuery(conn, comparisons_query, params = list(context_id))

  #outliers_query <- "SELECT * FROM madi_lumi_reader_outliers.outliers
  # WHERE comparison_id IN (SELECT id FROM madi_lumi_reader_outliers.comparisons WHERE context_id = $1)"
  #outliers_data <- dbGetQuery(con, outliers_query, params = list(context_id))

  return (comparisons_data)
}


output$outlierTab <- renderUI({
  tagList(
    uiOutput("outlierTypeUI"),  # New UI for type selection
    uiOutput("antigenSelectorUI"),
    uiOutput("visit1SelectorUI"),
    uiOutput("visit2SelectorUI"),
    jqui_resizable(
      plotlyOutput("plot", height = "100vh"),
      options = list(
        handles = "all",  # allows resizing from all sides
        minHeight = 200,  # minimum height in pixels
        minWidth = 200    # minimum width in pixels
      )
    ),
    br(),

    fluidRow(
      column(width = 12,
             div(style = "display: flex; gap: 10px; align-items: center;",
                 actionButton(inputId = "loadOutlierTable", label = "Load Outliers"),
                 downloadButton(outputId = "antigenOutlierDownload", label = "Download Outliers CSV"),
                 fileInput(inputId = "antigenOutlierUpload",
                           label = "Upload Modified CSV",
                           accept = c('text/csv',
                                      'text/comma-separated-values,text/plain',
                                      '.csv'),
                           buttonLabel = "Upload Outliers CSV")
             )
      )
    ),
    jqui_resizable(
      DTOutput("outlier_table",height = "100vh"),
      options = list(
        handles = "all",  # allows resizing from all sides
        minHeight = 200,  # minimum height in pixels
        minWidth = 200    # minimum width in pixels
      )
    )

)
})

# Separate UI rendering function
createOutlierTypeUI <- function(existing_types, job_status) {


  outlierUIRefresher()

  btn_style <- "background-color: #7DAF4C; border-color: #91CF60; color: white;"
  all_types <- c("MFI", "Norm MFI", "Antibody AU")
  calculated_types <- if(nrow(existing_types) > 0) existing_types$value_type else c()
  remaining_types <- setdiff(all_types, calculated_types)

  createStatusBadge <- function(type) {
    if (nrow(existing_types) > 0) {
      type_status <- existing_types$job_status[existing_types$value_type == type]
      if (length(type_status) > 0) {
        status_style <- switch(type_status,
                               "pending" = "background-color: #FFA500; color: white;",
                               "completed" = "background-color: #28a745; color: white;",
                               "failed" = "background-color: #dc3545; color: white;"
        )

        status_text <- switch(type_status,
                              "pending" = tagList(tags$i(class = "fa fa-spinner fa-spin"), " Running..."),
                              "completed" = tagList(tags$i(class = "fa fa-check"), " Completed"),
                              "failed" = tagList(tags$i(class = "fa fa-times"), " Failed")
        )

        span(
          class = "badge",
          style = paste("padding: 3px 8px; border-radius: 10px; margin-left: 10px;", status_style),
          status_text
        )
      }
    }
  }

  fluidRow(
    column(width = 6,
           wellPanel(
             h4("Calculated Outliers"),
             if(length(calculated_types) > 0) {
               tagList(
                 div(
                   style = "margin-bottom: 10px;",
                   tags$label("Select type to view:"),
                   # Single radioButtons with all choices
                   div(
                     style = "margin-top: 10px;",
                     lapply(calculated_types, function(type) {
                       div(
                         style = "display: flex; align-items: center; margin: 5px 0;",
                         div(
                           style = "margin-right: 10px;",
                           radioButtons(
                             inputId = "outlierType",
                             label = NULL,
                             choices = setNames(type, type),
                             selected = if(type == calculated_types[1]) type else NULL
                           )
                         ),
                         createStatusBadge(type)
                       )
                     })
                   )
                 ),
                 div(
                   style = "display: flex; gap: 10px;",
                   actionButton("loadOutliers", "Load Selected Outliers", style = btn_style),
                   actionButton("deleteOutliers", "Delete Outlier Data",
                                style = paste(btn_style, "background-color: #dc3545; color: white;")),  # Red delete button
                   downloadButton("downloadOutliers", "Download Outliers", style = btn_style)
                 )
               )
             } else {
               p("No outliers calculated yet")
             }
           )
    ),
    # Second Panel - Calculate New Outliers
    column(width = 6,
           wellPanel(
             h4("Calculate New Outliers"),
             if(length(remaining_types) > 0) {
               tagList(
                 # Radio buttons with status badge
                 div(
                   style = "display: flex; align-items: center;",
                   div(
                     style = "flex-grow: 1;",
                     radioButtons("newOutlierType",
                                  "Select type to calculate:",
                                  choices = remaining_types)
                   ),
                 ),
                 conditionalPanel(
                   condition = "input.newOutlierType == 'Antibody AU'",
                   numericInput("multiplier",
                                "Multiplier:",
                                value = 1000,
                                min = 0,
                                max = 1000000)
                 ),
                 actionButton("calculateOutliers",
                              "Calculate Outliers",
                              style = btn_style)
               )
             } else {
               p("All outlier types have been calculated")
             }
           )
    )
  )
}



# Main observer - was in loaded data
observeEvent(input$advanced_qc_component, {
  if (input$advanced_qc_component == "Outliers") {
    observe({

    outlierUIRefresher()
    selected_study <- selected_studyexpplate$study_accession
    selected_experiment <- selected_studyexpplate$experiment_accession
    workspace_id <- userWorkSpaceID()

    existing_types <- checkExistingValueTypes(selected_study,
                                              selected_experiment,
                                              workspace_id)
    output$outlierTypeUI <- renderUI({
      createOutlierTypeUI(existing_types, outlierJobStatus())
    })
    })
  }
})



observeEvent(input$loadOutliers, {
  req(input$outlierType)

  progressSweetAlert(
    session = session,
    id = "loadingModal",
    title = "Loading Data",
    display_pct = TRUE,
    value = 0
  )

  selected_study <- selected_studyexpplate$study_accession
  selected_experiment <- selected_studyexpplate$experiment_accession

  # Update progress
  updateProgressBar(session = session, id = "loadingModal", value = 30)

  context_id <- outlierExists(selected_study, selected_experiment, userWorkSpaceID(), input$outlierType)

  if (nrow(context_id) > 0) {
    current_data$comparisons_data <- preLoadData(context_id)

    # Update progress
    updateProgressBar(session = session, id = "loadingModal", value = 70)

    if (!is.null(current_data$comparisons_data) && nrow(current_data$comparisons_data) > 0) {
      # Render the UI components after successful data load
      renderUIComponents()
    } else {
      current_data$comparisons_data <- data.frame()
      showNotification("No data found for the selected criteria", type = "warning")
    }
  }

  # Update progress and close modal
  updateProgressBar(session = session, id = "loadingModal", value = 100)
  closeSweetAlert(session = session)
})


renderUIComponents <- function() {
  output$antigenSelectorUI <- renderUI({
    req(current_data$comparisons_data)
    fluidRow(
      column(9,
             radioGroupButtons(
               inputId = "antigenSelector",
               label = "Select Antigen:",
               choices = unique(current_data$comparisons_data$antigen),
               selected = unique(current_data$comparisons_data$antigen)[1],
               status = "success"
             )
      )
    )
  })

  output$visit1SelectorUI <- renderUI({
    req(input$antigenSelector, current_data$comparisons_data)
    visits_1 <- unique(current_data$comparisons_data[current_data$comparisons_data$antigen == input$antigenSelector, ]$visit_1)
    selectInput("visit1Selector",
                label = "Select Visit 1:",
                choices = visits_1)
  })

  output$visit2SelectorUI <- renderUI({
    req(input$antigenSelector, current_data$comparisons_data)
    visits_2 <- unique(current_data$comparisons_data[current_data$comparisons_data$antigen == input$antigenSelector, ]$visit_2)
    selectInput("visit2Selector",
                label = "Select Visit 2:",
                choices = visits_2)
  })

  output$plot <- renderPlotly({
    req(input$antigenSelector, input$visit1Selector, input$visit2Selector, current_data$comparisons_data)

    selected_plot_data <- current_data$comparisons_data[
      current_data$comparisons_data$antigen == input$antigenSelector &
        current_data$comparisons_data$visit_1 == input$visit1Selector &
        current_data$comparisons_data$visit_2 == input$visit2Selector,
    ]

    if (nrow(selected_plot_data) > 0) {
      plot_object <- unserialize(selected_plot_data$serialized_plot[[1]])
      plotly::ggplotly(plot_object)
    }
  })
}



observe({
  # Watch for tab changes -was in loaded data
  if (!is.null(input$qc_component) && input$qc_component != "Outliers") {
    # Reset all the data
    current_data$study <- NULL
    current_data$experiment <- NULL
    current_data$comparisons_data <- NULL
    current_data$ui_loaded <- FALSE

    # Clear all outputs
    output$outlierTypeUI <- renderUI({ NULL })
    output$antigenSelectorUI <- renderUI({ NULL })
    output$visit1SelectorUI <- renderUI({ NULL })
    output$visit2SelectorUI <- renderUI({ NULL })
    output$plot <- renderPlotly({ NULL })
    output$outlier_table <- renderDT({ NULL })
  }
})


# Also watch for study/experiment changes
observe({
  study <- selected_studyexpplate$study_accession
  experiment <- selected_studyexpplate$experiment_accession

  if (!identical(study, current_data$study) ||
      !identical(experiment, current_data$experiment)) {
    # Reset everything
    current_data$study <- study
    current_data$experiment <- experiment
    current_data$comparisons_data <- NULL
    current_data$ui_loaded <- FALSE

    # Clear all outputs
    output$outlierTypeUI <- renderUI({ NULL })
    output$antigenSelectorUI <- renderUI({ NULL })
    output$visit1SelectorUI <- renderUI({ NULL })
    output$visit2SelectorUI <- renderUI({ NULL })
    output$plot <- renderPlotly({ NULL })
    output$outlier_table <- renderDT({ NULL })
  }
})

observeEvent(input$deleteOutliers, {
  # Show confirmation dialog
  showModal(modalDialog(
    title = "Confirm Deletion",
    "Are you sure you want to delete all outlier data for this context?",
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirmDelete", "Delete",
                   class = "btn-danger")
    )
  ))
})

observeEvent(input$confirmDelete, {
  # Get context_id
  selected_study <- selected_studyexpplate$study_accession
  selected_experiment <- selected_studyexpplate$experiment_accession

  context_id <- outlierExists(selected_study, selected_experiment,
                              userWorkSpaceID(), input$outlierType)

  if (!is.null(context_id)) {
    tryCatch({
      # Split the deletion into separate queries
      # 1. Delete from outliers
      query1 <- sprintf("
        DELETE FROM madi_lumi_reader_outliers.outliers
        WHERE comparison_id IN (
          SELECT id
          FROM madi_lumi_reader_outliers.comparisons
          WHERE context_id = %d
        )", context_id$id)

      # 2. Delete from comparisons
      query2 <- sprintf("
        DELETE FROM madi_lumi_reader_outliers.comparisons
        WHERE context_id = %d", context_id$id)

      # 3. Delete from main_context
      query3 <- sprintf("
        DELETE FROM madi_lumi_reader_outliers.main_context
        WHERE id = %d", context_id$id)

      # Execute queries in order
      dbExecute(conn, query1)
      dbExecute(conn, query2)
      dbExecute(conn, query3)

      # Close modal and show success message
      removeModal()
      showNotification("Outlier data successfully deleted", type = "message")

    }, error = function(e) {
      # Error handling
      removeModal()
      showNotification(paste("Error deleting outlier data:", e$message),
                       type = "error")
    })
  } else {
    removeModal()
    showNotification("No outlier data found to delete", type = "warning")
  }
  outlierUIRefresher(outlierUIRefresher()+1)
})




observeEvent(input$outlierDownload, {
  req(input$outlierType)

  # Initialize logging
  log_msg <- function(msg) {
    message(paste0(Sys.time(), " - ", msg))
  }

  log_msg("Download initiated")
  log_msg(paste("Value type:", input$outlierType))
  log_msg(paste("Study:", selected_studyexpplate$study_accession))

  # Show loading modal
  showModal(modalDialog(
    title = "Preparing Download",
    "Your data is being prepared. This may take a few moments.",
    footer = NULL,
    easyClose = FALSE,
    fade = TRUE,
    tags$div(class = "progress",
             tags$div(id = "progress-bar-modal",
                      class = "progress-bar progress-bar-striped progress-bar-animated",
                      role = "progressbar",
                      style = "width: 100%",
                      "Loading..."))
  ))

  # Get current selections
  selected_study <- selected_studyexpplate$study_accession
  selected_experiment <- selected_studyexpplate$experiment_accession
  selected_value_type <- input$outlierType


  tryCatch({
    log_msg("Querying context_id")

    context_query <- "
      SELECT id
      FROM madi_lumi_reader_outliers.main_context
      WHERE workspace_id = $1
      AND study = $2
      AND experiment = $3
      AND value_type = $4"

    context_id <- dbGetQuery(conn,
                             context_query,
                             params = list(userWorkSpaceID(),
                                           selected_study,
                                           selected_experiment,
                                           selected_value_type))

    log_msg(paste("Context ID query result rows:", nrow(context_id)))

    if(nrow(context_id) > 0) {
      log_msg("Querying outliers data")

      outliers_query <- "
        SELECT o.*, c.antigen, c.visit_1, c.visit_2
        FROM madi_lumi_reader_outliers.outliers o
        JOIN madi_lumi_reader_outliers.comparisons c ON o.comparison_id = c.id
        WHERE c.context_id = $1"

      outlier_data <- dbGetQuery(conn,
                                 outliers_query,
                                 params = list(context_id$id[1]))

      log_msg(paste("Outlier data rows retrieved:", nrow(outlier_data)))

      if(nrow(outlier_data) > 0) {
        # Create filename
        file_name <- paste0("outliers_", selected_study, "_",
                            selected_value_type, "_",
                            format(Sys.time(), "%Y%m%d_%H%M%S"))

        # Create title
        title_name <- paste("Outliers -", selected_study, "-", selected_value_type)

        # Render temporary datatable with download buttons
        output$temp_download_table <- renderDT({
          datatable(
            outlier_data,
            extensions = 'Buttons',
            options = list(
              dom = 'Bt',  # Only show buttons
              buttons = list(
                list(
                  extend = 'csv',
                  title = title_name,
                  filename = file_name,
                  exportOptions = list(modifier = list(page = "all"))
                )
              ),
              pageLength = -1  # Show all rows
            ),
            style = 'none'  # Hide the table
          )
        })

        # Insert hidden table
        insertUI(
          selector = "body",
          where = "beforeEnd",
          ui = tags$div(
            id = "hiddenTableDiv",
            style = "display: none;",
            DTOutput("temp_download_table")
          ),
          immediate = TRUE
        )

        # Trigger CSV download button click
        shinyjs::delay(100, {
          shinyjs::runjs('
            var buttons = document.querySelectorAll("#temp_download_table button.buttons-csv");
            if (buttons.length > 0) {
              buttons[0].click();
              // Remove the hidden div after triggering download
              setTimeout(function() {
                var elem = document.getElementById("hiddenTableDiv");
                if(elem) elem.remove();
              }, 1000);
            }
          ')
        })

        log_msg("Download triggered via DataTables")

      } else {
        log_msg("No outlier data found")
        showModal(modalDialog(
          title = "No Data",
          paste0("No outliers found for ", selected_value_type),
          easyClose = TRUE
        ))
      }
    } else {
      log_msg("No context found")
      showModal(modalDialog(
        title = "No Data",
        paste0("No outlier analysis found for ", selected_value_type),
        easyClose = TRUE
      ))
    }

  }, error = function(e) {
    log_msg(paste("Error occurred:", e$message))
    showModal(modalDialog(
      title = "Error",
      paste("An error occurred while preparing the download: ", e$message),
      easyClose = TRUE
    ))
  }, finally = {
    log_msg("Download process completed")
    removeModal()
  })
})


output$downloadOutliers <- downloadHandler(
  filename = function() {
    paste0("outliers_", selected_studyexpplate$study_accession, "_",
           input$outlierType, "_",
           format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
  },
  content = function(file) {
    # Show loading message
    withProgress(
      message = 'Preparing download...',
      value = 0,
      {
        # Get context ID
        incProgress(0.2, detail = "Getting context...")
        context_query <- "
          SELECT id
          FROM madi_lumi_reader_outliers.main_context
          WHERE workspace_id = $1
          AND study = $2
          AND experiment = $3
          AND value_type = $4"

        context_id <- dbGetQuery(conn,
                                 context_query,
                                 params = list(userWorkSpaceID(),
                                               selected_studyexpplate$study_accession,
                                               selected_studyexpplate$experiment_accession,
                                               input$outlierType))

        if(nrow(context_id) > 0) {
          # Get outliers data
          incProgress(0.4, detail = "Retrieving outliers...")
          outliers_query <- "
            SELECT o.*, c.antigen, c.visit_1, c.visit_2
            FROM madi_lumi_reader_outliers.outliers o
            JOIN madi_lumi_reader_outliers.comparisons c ON o.comparison_id = c.id
            WHERE c.context_id = $1"

          outlier_data <- dbGetQuery(conn,
                                     outliers_query,
                                     params = list(context_id$id[1]))

          # Write to file
          incProgress(0.8, detail = "Writing file...")
          write.csv(outlier_data, file, row.names = FALSE)

          incProgress(1, detail = "Complete")
        } else {
          # Write empty file with message if no data
          write.csv(data.frame(message = "No data found"), file, row.names = FALSE)
        }
      }
    )
  },
  contentType = "text/csv"
)

# First, create a reactive value at a higher scope (outside the observeEvent)
outliers_reactive_data <- reactiveVal(NULL)

observeEvent(input$loadOutlierTable, {
  req(input$antigenSelector, current_data$comparisons_data, input$visit1Selector, input$visit2Selector)

  progressSweetAlert(
    session = session,
    id = "outlierLoadingModal",
    title = "Loading Outliers",
    display_pct = TRUE,
    value = 0
  )

  selected_antigen <- input$antigenSelector
  selected_study <- selected_studyexpplate$study_accession
  selected_experiment <- selected_studyexpplate$experiment_accession
  visit1_name <- input$visit1Selector
  visit2_name <- input$visit2Selector
  context_id <- outlierExists(selected_study, selected_experiment, userWorkSpaceID(), input$outlierType)

  if (nrow(context_id) > 0) {
    updateProgressBar(session = session, id = "outlierLoadingModal", value = 30)

    outliers_query <- "
      SELECT
        o.id,
        o.subject_accession,
        o.antigen,
        o.gate_class_1,
        o.gate_class_2,
        o.visit_1 as visit_1_value,
        o.visit_2 as visit_2_value,
        o.hample_outlier,
        o.bagplot_outlier,
        o.kde_outlier,
        o.lab_confirmed,
        mc.study,
        mc.experiment
      FROM madi_lumi_reader_outliers.outliers o
      JOIN madi_lumi_reader_outliers.main_context mc ON o.context_id = mc.id
      WHERE o.comparison_id IN (
        SELECT id FROM madi_lumi_reader_outliers.comparisons
        WHERE context_id = $1
        AND antigen = $2
        AND visit_1 = $3
        AND visit_2 = $4
      )
      AND o.feature = $5"

    outliers_data <- dbGetQuery(conn, outliers_query,
                                params = list(
                                  context_id$id[1],
                                  selected_antigen,
                                  visit1_name,
                                  visit2_name,
                                  selected_experiment
                                ))


    # Create visit column names using the selector values
    visit1_col_name <- paste("Visit", visit1_name)
    visit2_col_name <- paste("Visit", visit2_name)
    gate_class1_col_name <- paste("Gate Class", visit1_name)
    gate_class2_col_name <- paste("Gate Class", visit2_name)

    formatted_data <- outliers_data %>%
      select(
        ID = id,
        Study = study,
        Experiment = experiment,
        Subject = subject_accession,
        Antigen = antigen,
        !!sym(gate_class1_col_name) := gate_class_1,
        !!sym(gate_class2_col_name) := gate_class_2,
        !!sym(visit1_col_name) := visit_1_value,
        !!sym(visit2_col_name) := visit_2_value,
        `Hampel Outlier` = hample_outlier,
        `Bagplot Outlier` = bagplot_outlier,
        `KDE Outlier` = kde_outlier,
        `Lab Confirmed` = lab_confirmed
      )

    # Store the formatted data in reactive value
    outliers_reactive_data(formatted_data)

    updateProgressBar(session = session, id = "outlierLoadingModal", value = 100)
    closeSweetAlert(session = session)


    output$outlier_table <- renderDT({
      req(outliers_reactive_data())
      datatable(outliers_reactive_data(),
                options = list(
                  pageLength = 10,
                  autoWidth = TRUE,
                  scrollX = TRUE,  # Enable horizontal scrolling
                  scrollY = "400px",  # Enable vertical scrolling with fixed height
                  order = list(list(0, 'asc')),
                  columnDefs = list(
                    list(className = 'dt-center', targets = '_all')
                  )
                ),
                editable = list(
                  target = 'cell',
                  disable = list(columns = c(0:11)), # Enable editing only for Lab Confirmed
                  type = 'select',
                  options = list(values = c('TRUE', 'FALSE'))
                ),
                selection = 'none'
      ) %>%
        formatStyle(
          'Lab Confirmed',
          backgroundColor = styleEqual(
            c(TRUE, FALSE),
            c('#a8f0a8', '#f0a8a8')
          )
        ) %>%
        formatStyle(
          'Hampel Outlier',
          backgroundColor = styleEqual(
            c(TRUE, FALSE),
            c('#a8f0a8', '#f0a8a8')
          )
        ) %>%
        formatStyle(
          'Bagplot Outlier',
          backgroundColor = styleEqual(
            c(TRUE, FALSE),
            c('#a8f0a8', '#f0a8a8')
          )
        ) %>%
        formatStyle(
          'KDE Outlier',
          backgroundColor = styleEqual(
            c(TRUE, FALSE),
            c('#a8f0a8', '#f0a8a8')
          )
        )
    })


    observeEvent(input$outlier_table_cell_edit, {
      info <- input$outlier_table_cell_edit
      row_num <- info$row
      col_num <- info$col
      new_value <- as.logical(info$value)
      current_data <- outliers_reactive_data()
      row_id <- current_data$ID[row_num]

      update_query <- "UPDATE madi_lumi_reader_outliers.outliers
                      SET lab_confirmed = $1
                      WHERE id = $2"

      tryCatch({
        dbExecute(conn, update_query, params = list(new_value, row_id))

        # Update the reactive value with new data
        current_data$`Lab Confirmed`[row_num] <- new_value
        outliers_reactive_data(current_data)

        showNotification(
          "Lab confirmation status updated successfully",
          type = "message"
        )
      }, error = function(e) {
        showNotification(
          paste("Error updating lab confirmation:", e$message),
          type = "error"
        )
      })
    })

  } else {
    closeSweetAlert(session = session)
    showModal(modalDialog(
      title = "No Data",
      "No outliers data found for the selected antigen.",
      easyClose = TRUE,
      footer = NULL
    ))
  }

  # Update the download handler to include all columns
  output$antigenOutlierDownload <- downloadHandler(
    filename = function() {
      paste("antigen_outliers_", selected_antigen, "_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(outliers_reactive_data(), file, row.names = FALSE)
    }
  )

})

# Add upload handler
observeEvent(input$antigenOutlierUpload, {
  req(input$antigenOutlierUpload)

  selected_study <- selected_studyexpplate$study_accession
  selected_experiment <- selected_studyexpplate$experiment_accession
  workspace_id <- userWorkSpaceID()
  selected_antigen <- input$antigenSelector

  progressSweetAlert(
    session = session,
    id = "antigenOutlierUploadModal",
    title = "Processing Antigen Outliers Upload",
    display_pct = TRUE,
    value = 0
  )

  tryCatch({
    # Read uploaded CSV with all columns
    uploaded_antigen_data <- read.csv(input$antigenOutlierUpload$datapath,
                                      stringsAsFactors = FALSE,
                                      check.names = FALSE)  # Preserve exact column names

    updateProgressBar(session = session, id = "antigenOutlierUploadModal", value = 30)

    # Validate that required columns exist
    required_cols <- c("ID", "Lab Confirmed")
    if (!all(required_cols %in% colnames(uploaded_antigen_data))) {
      stop("CSV must contain 'ID' and 'Lab Confirmed' columns")
    }

    # Prepare batch update query
    update_query <- "
      WITH updated_antigen_values (id, lab_confirmed) AS (
        VALUES %s
      )
      UPDATE madi_lumi_reader_outliers.outliers AS o
      SET lab_confirmed = uv.lab_confirmed
      FROM updated_antigen_values uv
      WHERE o.id = uv.id"

    # Create values string for batch update
    values_string <- paste(
      sprintf("(%d, %s)",
              uploaded_antigen_data$ID,
              ifelse(uploaded_antigen_data$`Lab Confirmed`, 'TRUE', 'FALSE')),
      collapse = ", "
    )

    update_query <- sprintf(update_query, values_string)

    updateProgressBar(session = session, id = "antigenOutlierUploadModal", value = 60)

    # Execute update
    dbExecute(conn, update_query)

    # Refresh the table data
    context_id <- outlierExists(selected_study, selected_experiment,
                                userWorkSpaceID(), input$outlierType)

    outliers_query <- "
      SELECT
        o.id,
        o.subject_accession,
        o.antigen,
        o.feature,
        o.gate_class_1,
        o.gate_class_2,
        o.visit_1 as visit_1_value,
        o.visit_2 as visit_2_value,
        o.hample_outlier,
        o.bagplot_outlier,
        o.kde_outlier,
        o.lab_confirmed,
        mc.study,
        mc.experiment
      FROM madi_lumi_reader_outliers.outliers o
      JOIN madi_lumi_reader_outliers.main_context mc ON o.context_id = mc.id
      WHERE o.comparison_id IN (
        SELECT id FROM madi_lumi_reader_outliers.comparisons
        WHERE context_id = $1 AND antigen = $2
      )"


    new_antigen_data <- dbGetQuery(conn, outliers_query,
                                   params = list(context_id$id[1], selected_antigen))

    # Update reactive data with new values
    # In your upload handler:
    outliers_reactive_data(format_antigen_outliers_data(
      new_antigen_data,
      input$visit1Selector,
      input$visit2Selector
    ))

    updateProgressBar(session = session, id = "antigenOutlierUploadModal", value = 100)
    closeSweetAlert(session = session)

    showNotification(
      "Antigen outliers data updated successfully from CSV",
      type = "message"
    )

  }, error = function(e) {
    closeSweetAlert(session = session)
    showNotification(
      paste("Error processing antigen outliers CSV:", e$message),
      type = "error",
      duration = NULL
    )
    print("Error details:")
    print(e)
  })
})



# Helper function to format antigen outliers data consistently
format_antigen_outliers_data <- function(data, visit1_name, visit2_name) {
  visit1_col_name <- paste("Visit", visit1_name)
  visit2_col_name <- paste("Visit", visit2_name)
  gate_class1_col_name <- paste("Gate Class", visit1_name)
  gate_class2_col_name <- paste("Gate Class", visit2_name)

  data %>%
    select(
      ID = id,
      Study = study,
      Experiment = experiment,
      Subject = subject_accession,
      Antigen = antigen,
      Feature = feature,
      !!sym(gate_class1_col_name) := gate_class_1,
      !!sym(gate_class2_col_name) := gate_class_2,
      !!sym(visit1_col_name) := visit_1_value,
      !!sym(visit2_col_name) := visit_2_value,
      `Hampel Outlier` = hample_outlier,
      `Bagplot Outlier` = bagplot_outlier,
      `KDE Outlier` = kde_outlier,
      `Lab Confirmed` = lab_confirmed
    )
}


output$processingIndicator <- renderUI({
  if(processing_status()) {
    div(
      style = "display: inline-block; margin-left: 10px;",
      tags$i(class = "fa fa-spinner fa-spin"),
      "Calculating outliers..."
    )
  }
})


observeEvent(input$calculateOutliersXXXXXX, {
  # Input validation
  req(input$newOutlierType,
      input$multiplier,
      selected_studyexpplate$study_accession,
      selected_studyexpplate$experiment_accession,
      stored_plates_data$stored_sample,
      userWorkSpaceID())

  # Create progress object
  progress <- shiny::Progress$new()
  on.exit(progress$close())

  progress$set(message = "Starting calculation...", value = 0)

  # Get database connection
  conn <- get_db_connection()
  on.exit({ if (!is.null(conn)) dbDisconnect(conn) }, add = TRUE)

  # Direct call to computeOutliers
  result <- computeOutliers(
    selected_type = input$newOutlierType,
    multiplier = input$multiplier,
    selected_study = selected_studyexpplate$study_accession,
    selected_experiment = selected_studyexpplate$experiment_accession,
    sample_data_outlier = stored_plates_data$stored_sample,
    userWorkSpaceID = userWorkSpaceID(),
    conn = conn
  )

  # Show result notification
  if (isTRUE(result)) {
    outlierUIRefresher(outlierUIRefresher()+1)
    showNotification(
      "Calculation completed successfully!",
      type = "message",
      duration = 5
    )
  } else {
    outlierUIRefresher(outlierUIRefresher()+1)
    showNotification(
      "Calculation failed! Check the console for errors.",
      type = "error",
      duration = 5
    )
  }
})

observeEvent(input$calculateOutliers, {
  session <- getDefaultReactiveDomain()

  if (processing_status()) {
    showNotification(
      "Calculation still in progress. Please wait...",
      type = "warning",
      duration = 5
    )
    return()
  }

  req(input$newOutlierType, input$multiplier,
      selected_studyexpplate$study_accession,
      selected_studyexpplate$experiment_accession,
      stored_plates_data$stored_sample,
      userWorkSpaceID())

  # Capture all inputs before future
  input_data <- list(
    selected_type = isolate(input$newOutlierType),
    multiplier = isolate(input$multiplier),
    selected_study = isolate(selected_studyexpplate$study_accession),
    selected_experiment = isolate(selected_studyexpplate$experiment_accession),
    sample_data = isolate(stored_plates_data$stored_sample),
    current_workspace_id = isolate(userWorkSpaceID())
  )


  processing_status(TRUE)

  # Create progress object
  progress <- Progress$new(session = session)
  progress_closed <- FALSE

  context_id <- initialize_context(
      conn,
      input_data$current_workspace_id,
      input_data$selected_study,
      input_data$selected_experiment,
      input_data$selected_type
    )


    outlierUIRefresher(outlierUIRefresher()+1)


  # Modified cleanup
  on.exit({
    processing_status(FALSE)
    if (!progress_closed && !is.null(progress)) {
      try(progress$close(), silent = TRUE)
      progress_closed <- TRUE
    }
  })

  progress$set(message = "Starting calculation...", value = 0)

  future_promise({
    tryCatch({
      # Create new connection inside future
      message("Creating new database connection in future...")
      future_conn <- get_db_connection()

      on.exit({
        message("Closing future database connection...")
        if (exists("future_conn") && !is.null(future_conn)) {
          try(DBI::dbDisconnect(future_conn), silent = TRUE)
        }
      })

      message("Starting future execution...")
      message(paste("Input values:",
                    "\n- selected_type:", input_data$selected_type,
                    "\n- multiplier:", input_data$multiplier,
                    "\n- study:", input_data$selected_study,
                    "\n- experiment:", input_data$selected_experiment))

      result <- computeOutliers(
        selected_type = input_data$selected_type,
        multiplier = input_data$multiplier,
        selected_study = input_data$selected_study,
        selected_experiment = input_data$selected_experiment,
        sample_data_outlier = input_data$sample_data,
        userWorkSpaceID = input_data$current_workspace_id,
        conn = future_conn
      )

      message("Future execution completed")
      return(result)
    }, error = function(e) {
      message("Error in computation: ", e$message)
      return(FALSE)
    })
  }) %>%
    then(
      onFulfilled = function(result) {
        if (!is.null(session) && !session$isClosed()) {
          try({
            if (!progress_closed) {
              progress$set(message = "Completed", value = 1)
              progress$close()
              progress_closed <- TRUE
            }

            processing_status(FALSE)

            if (isTRUE(result)) {
              showNotification(
                "Calculation completed successfully!",
                type = "message",
                duration = 5,
                session = session
              )
            } else {
              showNotification(
                "Calculation failed! Check the console for errors.",
                type = "warning",
                duration = 5,
                session = session
              )
            }
          }, silent = TRUE)
        }
      }
    ) %>%
    catch(function(error) {
      if (!is.null(session) && !session$isClosed()) {
        try({
          if (!progress_closed) {
            progress$set(message = "Error occurred", value = 1)
            progress$close()
            progress_closed <- TRUE
          }

          processing_status(FALSE)

          message("Error in future:", error$message)
          showNotification(
            paste("Error:", error$message),
            type = "warning",
            duration = 5,
            session = session
          )
        }, silent = TRUE)
      }
    })

  # Update progress
  if (!progress_closed) {
    progress$set(message = "Processing outliers...", value = 0.1)
  }
})
