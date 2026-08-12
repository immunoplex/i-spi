
fetch_study_header <- function(selected_study) {
#   header_query <- glue::glue_sql("SELECT experiment_accession, plate_id, plateid, plate, nominal_sample_dilution
# FROM madi_results.xmap_header
# WHERE study_accession = {selected_study}",
#                                   .con = conn)

#   header_query <- glue::glue_sql("SELECT
#     h.experiment_accession,
#     h.plate_id,
#     h.plateid,
#     h.plate,
#     h.nominal_sample_dilution,
#     STRING_AGG( DISTINCT s.source, ',') AS standard_curve_sources
# FROM
#     madi_results.xmap_header as h
# JOIN
#     madi_results.xmap_standard as s
#     ON h.experiment_accession = s.experiment_accession AND h.plate_id = s.plate_id
# WHERE
#     h.study_accession = {selected_study}
#     AND s.study_accession = {selected_study}
# GROUP BY
#     h.experiment_accession,
#     h.plate_id,
#     h.plateid,
#     h.plate,
#     h.nominal_sample_dilution;
# ", .con = conn)
  header_query <- glue::glue_sql("
  SELECT
      h.experiment_accession,
      h.plate_id,
      h.plateid,
      h.plate,
      h.nominal_sample_dilution,
      STRING_AGG(DISTINCT s.source, ',') AS standard_curve_sources
  FROM
      madi_results.xmap_header AS h
  JOIN
      madi_results.xmap_standard AS s
      ON h.experiment_accession = s.experiment_accession
      AND REGEXP_REPLACE(TRIM(h.plate_id), '\\s+', ' ', 'g')
        = REGEXP_REPLACE(TRIM(s.plate_id), '\\s+', ' ', 'g')
  WHERE
      h.study_accession = {selected_study}
      AND s.study_accession = {selected_study}
  GROUP BY
      h.experiment_accession,
      h.plate_id,
      h.plateid,
      h.plate,
      h.nominal_sample_dilution;
", .con = conn)

  study_header <- dbGetQuery(conn, header_query)
  return(study_header)
}


# given a study, experiment, original_plate name and edited_plate
# rename original_plate to the edited_plate for header, control,
# buffer, standard, and sample tables if there are rows.
# update_plate_names <- function(selected_study, selected_experiment, original_plate, edited_plate) {
#   # check if data is in header for original plate (this should be true)
#   original_header_query <- glue::glue_sql("SELECT *
# 	FROM madi_results.xmap_header
#   	WHERE study_accession = {selected_study}
# 	AND experiment_accession = {selected_experiment}
# 	AND plate_id = {original_plate}", .con = conn)
#
#   original_header <- dbGetQuery(conn, original_header_query)
#   if (nrow(original_header) > 0) {
#     cat("\nheader has at least 1 row\n")
#   #  print(head(original_header))
#   } else {
#     cat("\nno data in header for in experiment\n")
#   }
#
#   ## check if data is in the Control
#   original_control_query <- glue::glue_sql("SELECT *
# 	FROM madi_results.xmap_control
# 	WHERE study_accession = {selected_study}
# 	AND experiment_accession = {selected_experiment}
# 	AND plate_id = {original_plate}", .con = conn)
#
#   original_control <- dbGetQuery(conn, original_control_query)
#   if (nrow(original_control) > 0) {
#     cat("\ncontrol has at least 1 row\n")
#   } else {
#     cat("\nno controls in experiment\n")
#   }
#
#   ## Buffer
#   original_buffer_query <- glue::glue_sql("SELECT *
# 	FROM madi_results.xmap_buffer
# 	WHERE study_accession = {selected_study}
# 	AND experiment_accession = {selected_experiment}
# 	AND plate_id = {original_plate}", .con = conn)
#
#   original_buffer <- dbGetQuery(conn, original_buffer_query)
#   if (nrow(original_buffer) > 0) {
#     cat("\nbuffer was at least 1 row\n")
#   #  print(head(original_buffer))
#   } else {
#     cat("\nno data in buffer for plate in experiment.\n")
#   }
#
#   ## Standards (standard curve data)
#   original_standards_query <- glue::glue_sql("SELECT *
# FROM madi_results.xmap_standard
# 	WHERE study_accession = {selected_study}
# 	AND experiment_accession = {selected_experiment}
# 	AND plate_id = {original_plate}", .con = conn)
#
#   original_standards <- dbGetQuery(conn, original_standards_query)
#   if (nrow(original_standards) > 0) {
#     cat("\nstabdards had at least 1 row")
#   } else {
#     cat("no data in standards for plate in experiment")
#   }
#
#   ## Samples
#   original_sample_query <- glue::glue_sql("SELECT *
# 	FROM madi_results.xmap_sample
# 	WHERE study_accession = {selected_study}
# 	AND experiment_accession = {selected_experiment}
# 	AND plate_id = {original_plate}", .con = conn)
#
#   original_samples <- dbGetQuery(conn, original_sample_query)
#   if (nrow(original_samples) > 0) {
#     cat("\nsamples had at least 1 row")
#   } else {
#     cat("no data in samples for experiment")
#   }
#
#
# } # end update plate name function

## Function to update plateid column in header.
#plate_id is used as unique id and is not changed
update_plateid <- function(selected_study, selected_experiment,selected_plate_id, edited_plateid) {
  # check if data is in header for original plate (this should be true)
    original_header_query <- glue::glue_sql("SELECT *
  	FROM madi_results.xmap_header
    	WHERE study_accession = {selected_study}
  	AND experiment_accession = {selected_experiment}
  	AND plate_id = {selected_plate_id}", .con = conn)

    original_header <- dbGetQuery(conn, original_header_query)
    if (nrow(original_header) > 0) {
          cat("\nheader has at least 1 row\n")
         print(head(original_header))
         update_plateid_query <- glue::glue_sql("UPDATE madi_results.xmap_header
                                          SET plateid = {edited_plateid}
                                          WHERE study_accession = {selected_study}
                                            AND experiment_accession = {selected_experiment}
                                            AND plate_id = {selected_plate_id}", .con = conn)
         print(update_plateid_query)
         dbExecute(conn, update_plateid_query)

        } else {
          cat("\nno data in header for in experiment\n")
        }
}

# Update the plate number variable ("plate_n)"
update_plate_var <- function(selected_study, selected_experiment, selected_plate_id, edited_plate) {
  # check if data is in header for original plate (this should be true)
  original_header_query <- glue::glue_sql("SELECT *
  	FROM madi_results.xmap_header
    	WHERE study_accession = {selected_study}
  	AND experiment_accession = {selected_experiment}
  	AND plate_id = {selected_plate_id}", .con = conn)

  original_header <- dbGetQuery(conn, original_header_query)
  if (nrow(original_header) > 0) {
    print(head(original_header))
    update_plate_query <- glue::glue_sql("UPDATE madi_results.xmap_header
                                          SET plate = {edited_plate}
                                          WHERE study_accession = {selected_study}
                                            AND experiment_accession = {selected_experiment}
                                            AND plate_id = {selected_plate_id}", .con = conn)
    print(update_plate_query)
    dbExecute(conn, update_plate_query)
  } else {
    cat("\nno data in header for in experiment\n")
  }
}

# update the sample dilution factor variable in the header
update_nominal_sample_dilution <- function(selected_study, selected_experiment, selected_plate_id, edited_nominal_sample_dilution) {
  # check if data is in header for original plate (this should be true)
  original_header_query <- glue::glue_sql("SELECT *
  	FROM madi_results.xmap_header
    	WHERE study_accession = {selected_study}
  	AND experiment_accession = {selected_experiment}
  	AND plate_id = {selected_plate_id}", .con = conn)

  original_header <- dbGetQuery(conn, original_header_query)
  if (nrow(original_header) > 0) {
    print(head(original_header))
      update_sample_dilution_query <- glue::glue_sql("UPDATE madi_results.xmap_header
                                                      SET nominal_sample_dilution = {edited_nominal_sample_dilution}
                                                      WHERE study_accession = {selected_study}
                                                      AND experiment_accession = {selected_experiment}
                                                      AND plate_id = {selected_plate_id}", .con = conn)
      print(update_sample_dilution_query)
      dbExecute(conn, update_sample_dilution_query)



  } else {
    cat("\nno data in header for in experiment\n")
    }
}

# update the standard curve source for a plate
update_plate_standard_curve_source <- function(selected_study, selected_experiment, selected_plate_id, selected_source, edited_source) {
  # check if there is data to update
  original_standards_query <- glue::glue_sql("SELECT *
	FROM madi_results.xmap_standard
	WHERE study_accession = {selected_study}
	AND experiment_accession = {selected_experiment}
	AND plate_id = {selected_plate_id}
	AND source = {selected_source}
	", .con = conn)

 original_standards <- dbGetQuery(conn, original_standards_query)
 if (nrow(original_standards) > 0) {
   update_standard_curve_source_plate_query <- glue::glue_sql("UPDATE madi_results.xmap_standard
SET source = {edited_source}
WHERE study_accession = {selected_study}
  AND experiment_accession = {selected_experiment}
  AND plate_id = {selected_plate_id}
  AND source = {selected_source};", .con = conn)

   dbExecute(conn, update_standard_curve_source_plate_query)
 } else {
  cat("\nno data in standards for experiment\n")
 }
}

#observeEvent(input$study_level_tabs, {

  # req(input$readxMap_study_accession != "Click here",
  #     input$study_level_tabs == "Study Overview",
  #     input$main_tabs == "view_files_tab")


  output$plate_management_UI <- renderUI({
    # req(input$readxMap_study_accession != "Click here",
    #     input$study_level_tabs == "Study Overview",
    #     input$main_tabs == "view_files_tab")
    req(input$readxMap_study_accession, input$readxMap_study_accession != "Click here")

    fluidPage(
      # bsCollapse(
      #   id = "plate_management_collapse",
      #   bsCollapsePanel(
      #     title = "Plate Label Editor",


      bsCollapse(
        id = "plate_management_instructions",
        bsCollapsePanel(
          title = "Plate Label Editor Instructions",
          tagList(
            tags$ol("1. Select a plate from an experiment by clicking the desired row in the table."),
            tags$ol("2. Rename that plate by typing its new name in the Edit Plate ID field."),
            tags$ol("3. If desired, reset your edits before saving by clicking Reset Current Edits button."),
            tags$h4("Plate Naming Rules"),
            tags$ol("1. New plate name cannot be the name of another existing plate in the study."),
            tags$ol("2. Plate name cannot not start with hyphen (-). If done, reset edits."),
            tags$p("Note: The selected experiment name and original plate fields are locked and can't be changed by design.")



          ), # end taglist
          style = "success"
        )),
       DTOutput("plate_header"),
       # selectInput("selected_experiment_row", "Selected Experiment", choices = NULL),
       # selectInput("selected_plateid_to_edit", "Original Plate", choices = NULL),
       # uiOutput("output_edit_plateID_UI"),
       # uiOutput("resetToCurrentPlate")

       fluidRow(
         column(12,
                tags$div(style = "display: flex; gap: 20px;",
                         tags$div(style = "flex: 1;",
                          selectInput("selected_experiment_row", "Selected Experiment", choices = NULL)
                          ),

               # tags$div(style = "display: flex; gap: 20px;",
                         tags$div(style = "flex: 1;",
                                  selectInput("selected_plate_id", "Selected plate_id", choices = NULL)
                         ),
                        tags$div(style = "flex: 1;",
                                          br(),
                                          br(),
                                          uiOutput("resetToCurrentPlate")
                        ), # end div flex reset button
                      tags$div(style = "flex: 1",
                               br(),
                               br(),
                               NULL  # [11.8] plate delete removed -> Delete Study Components tab
                      )
                )
         )

       ),
      fluidRow(
        column(12,
               tags$div(style = "display: flex; gap: 20px;",
                        tags$div(style = "flex: 1;",
                                 selectInput("selected_plateid_to_edit", "Original plateid", choices = NULL),
                                 uiOutput("output_edit_plateID_UI")
                        ), # end first grouped div
                        tags$div(style = "flex: 1;",
                                 selectInput("original_plate_to_edit", "Original plate", choices = NULL),
                                 uiOutput("edit_plateUI")
                        ), # end second grouped div
                        tags$div(style = "flex: 1;",
                                selectInput("original_nominal_sample_dilution", "Original Sample Dilution Factor",choices = NULL),
                                uiOutput("edit_nominal_sample_dilutionUI")
                        ),# end third grouped div
                        tags$div(style = "flex: 1;",
                                 selectInput("original_sc_source", "Original Standard Curve Source", choices = NULL),
                                 uiOutput("edit_sc_sourceUI"))
               ) # end div outer flex tag
        )
      ),
      fluidRow(
        column(12,
               tags$div(style = "display: flex;",
               #          tags$div(style = "flex: 1;",
               #              uiOutput("resetToCurrentPlate")
               #          ), # end div flex reset button
                        tags$div(style = "flex: 1;",
                                 uiOutput("rename_plateid_UI")
                        ), # end div rename plateid
                        tags$div(style = "flex: 1;",
                                 uiOutput("rename_plate_UI")
                        ), # end div rename plate column
                        tags$div(style = "flex: 1;",
                                 uiOutput("update_nominal_sample_dilutionUI")
                        ), # end div update sample dilution factor
                        tags$div(style = "flex: 1;",
                          uiOutput("rename_plate_sc_source")
                        )
               ) # end div outer flex tag
        ) # end column
      )
      # fluidRow(
      #   column(12,
      #          tags$div(style = "display: flex;",
      #           tags$div(style = "flex: 1;",
      #                    actionButton("delete_plate", label = "Delete Selected Plate"))
      #         ) # end div outer flex tag
      #   )
      # ),

      #  style = "primary" )
      # )
    )
    #)
  })

  output$output_edit_plateID_UI <- renderUI({
    # if (input$edit_plate == "-1") {
   textInput("edit_plateid", "Edit plateid", value = "")

  })

  output$edit_plateUI <- renderUI({
    textInput("edit_plate", "Edit plate", value = "plate_")
  })

  output$edit_nominal_sample_dilutionUI <- renderUI({
    numericInput("edit_sample_dil_factor", "Edit Sample Dilution Factor", value = NULL)
  })

  output$edit_sc_sourceUI <- renderUI({
    textInput("edit_sc_source", "Edit Standard Curve Source", value = NULL)
  })



  output$resetToCurrentPlate <- renderUI({
    actionButton("reset_plate_edits", label = "Reset Current Edits")
  })

  # button for plateid rename
  output$rename_plateid_UI <- renderUI({
    req(input$edit_plateid != "-1")
    req(input$edit_plateid != "")
    req(input$edit_plateid != input$selected_plateid_to_edit)
    actionButton("rename_plateid", "Rename Selected plateid")
  })
  # button for plate variable rename
  output$rename_plate_UI <- renderUI({
    req(input$edit_plate != "-1")
    req(input$edit_plate != "")
    req(input$edit_plate != input$original_plate_to_edit)
    #req(grepl("^plate_\\d+$", input$edit_plate))
    req(grepl("^plate_([1-9]|[1-9][0-9])[a-z]?$", input$edit_plate))

    actionButton("rename_plate_var", "Rename Selected 'plate'")
  })
  ## button to update nominal_sample_dilution
  output$update_nominal_sample_dilutionUI <- renderUI({
    req(input$edit_sample_dil_factor > 0)
    req(input$edit_sample_dil_factor != input$original_nominal_sample_dilution)
    actionButton("update_nominal_sample_dilution", "Update Sample Dilution Factor")
  })
  ## button for rename standard curve source for plate
  output$rename_plate_sc_source <- renderUI({
    req(input$edit_sc_source != "-1")
    req(input$original_sc_source != input$edit_sc_source)
    actionButton("rename_plate_sc_source", "Rename Standard Curve Source")
  })


   output$plate_header <- renderDT({

     plate_df <- fetch_study_header(selected_study = input$readxMap_study_accession)
     datatable(plate_df, selection = "single", filter = 'top')  # Enable single-row selection
   })

   observeEvent(input$plate_header_rows_selected, {
     enable("selected_experiment_row")
     enable("selected_plate_id")
     enable("selected_plateid_to_edit")
     enable("original_plate_to_edit")
     enable("original_nominal_sample_dilution")
     enable("original_sc_source")
     selected_row_index <- input$plate_header_rows_selected
     if (length(selected_row_index)) {
       plate_df <- fetch_study_header(selected_study = input$readxMap_study_accession)
       selected_row_data <- plate_df[selected_row_index, ]

       # Update select boxes with the selected values
       updateSelectInput(session, "selected_experiment_row",
                         choices = unique(plate_df$experiment_accession),
                         selected = selected_row_data$experiment_accession)
       updateSelectInput(session, "selected_plate_id",
                         choices = unique(plate_df$plate_id),
                         selected = selected_row_data$plate_id)

       updateSelectInput(session, "selected_plateid_to_edit",
                         choices = unique(plate_df$plateid),
                         selected = selected_row_data$plateid)

       updateSelectInput(session, "original_plate_to_edit",
                         choices = selected_row_data$plate,
                         selected = selected_row_data$plate)

       updateSelectInput(session, "original_nominal_sample_dilution",
                         choices = selected_row_data$nominal_sample_dilution,
                         selected = selected_row_data$nominal_sample_dilution)

       updateSelectInput(session, "original_sc_source",
                         choices = strsplit(selected_row_data$standard_curve_sources, ",")[[1]],
                         selected = strsplit(selected_row_data$standard_curve_sources, ",")[[1]][1])


       disable("selected_experiment_row")
       disable("selected_plate_id")
       disable("selected_plateid_to_edit")
       disable("original_plate_to_edit")
       disable("original_nominal_sample_dilution")

       updateTextInput(session, "edit_plateid", value = selected_row_data$plateid)
       if (is.null(selected_row_data$plate) || is.na(selected_row_data$plate) || selected_row_data$plate == "") {
         updateTextInput(session, "edit_plate", value = "plate_")
       }else {
        updateTextInput(session, "edit_plate", value = selected_row_data$plate)
       }

       updateNumericInput(session, "edit_sample_dil_factor", value = selected_row_data$nominal_sample_dilution)

       updateTextInput(session, "edit_sc_source", value = strsplit(selected_row_data$standard_curve_sources, ",")[[1]][1])
     }
   })

   # update based on what is selected
   observeEvent(input$original_sc_source, {
     updateTextInput(session, "edit_sc_source", value = input$original_sc_source)
   })



   observe({
     if (!identical(input$selected_plateid_to_edit, input$edit_plateid)) {
       showFeedbackWarning("edit_plateid", text = "Unsaved Changes")
     } else {
       hideFeedback("edit_plateid")
     }
     if (input$edit_plateid == "-1" || is.null(input$edit_plateid)) {
       disable("edit_plateid")
     } else {
       enable("edit_plateid")
     }

     if (!identical(input$original_plate_to_edit, input$edit_plate)) {
       showFeedbackWarning("edit_plate", text = "Unsaved Changes")
     } else {
       hideFeedback("edit_plate")
     }
     if (input$edit_plate == "-1" || is.null(input$edit_plate)) {
       disable("edit_plate")
     } else {
       enable("edit_plate")
     }

     if (input$edit_sc_source == "-1" || is.null(input$edit_sc_source)) {
       disable("edit_sc_source")
     } else {
       enable("edit_sc_source")
     }

     if (!identical(input$original_sc_source, input$edit_sc_source)) {
       showFeedbackWarning("edit_sc_source", text = "Unsaved Changes")
     } else {
       hideFeedback("edit_sc_source")
     }

   #  if (input$original_nominal_sample_dilution != input$edit_sample_dil_factor) {
     if (!is.null(input$original_nominal_sample_dilution) &&
         !is.null(input$edit_sample_dil_factor) &&
         !is.na(input$original_nominal_sample_dilution) &&
         !is.na(input$edit_sample_dil_factor) &&
         input$edit_sample_dil_factor != -1 &&
         input$original_nominal_sample_dilution != input$edit_sample_dil_factor) {
       showFeedbackWarning("edit_sample_dil_factor", text = "Unsaved Changes")
     } else {
       hideFeedback("edit_sample_dil_factor")
     }
    # if (input$edit_sample_dil_factor == -1 || is.null(input$edit_sample_dil_factor)) {
     if (is.null(input$edit_sample_dil_factor) || isTRUE(input$edit_sample_dil_factor == -1)) {
       disable("edit_sample_dil_factor")
     } else {
       enable("edit_sample_dil_factor")
     }

   })

#})


# enforce rules. Cannot start with -
# Cannot be an old plate that is not the one editing.
observe({
  req(input$edit_plateid)
  req(input$edit_plate)
  req(input$edit_sample_dil_factor)

  if (startsWith(input$edit_plateid, "-")) {
    updateTextInput(session, "edit_plateid", value = "-1")
  }
  plate_df <- fetch_study_header(selected_study = input$readxMap_study_accession)
  stored_plateids <- plate_df$plateid

  conflicting_plateids <- setdiff(stored_plateids, input$selected_plateid_to_edit)
  if (input$edit_plateid %in% conflicting_plateids) {
    showNotification("Cannot edit a plateid that already exists.")
    updateTextInput(session, "edit_plateid", value = "-1")
  }

  # # Plate must begin with plate_
  # no_space_plate <- gsub("\\s+", "", input$edit_plate)
  # # must have the word plate
  # no_space_plate <- sub("^(p|pl|pla|plat)$", "plate", no_space_plate)
  # if ((!grepl("_", no_space_plate)) && input$edit_plate != "-1") {
  #   no_space_plate <- paste0(no_space_plate, "_")
  # }
  # if (input$edit_plate != no_space_plate) {
  #  updateTextInput(session, "edit_plate", value = no_space_plate)
  # }
  # sample dilution factors can only be between 0 and 1,000,000  (10^6) inclusive
   if (input$edit_sample_dil_factor < 0 || input$edit_sample_dil_factor > 1000000) {
     updateNumericInput(session, "edit_sample_dil_factor", value = -1)
   }

  # if (input$edit_plate != "") {
  #   updateTextInput(session, "edit_plate", value = input$original_plate_to_edit)
  # }
  ## Get stored plate variable (Plates can be repeated at different serum dilutions so do not run code)
  # stored_plates <- plate_df$plate
  # conflicting_plates <- setdiff(stored_plates, input$original_plate_to_edit)
  # if (input$edit_plate %in% conflicting_plates) {
  #   showNotification("Cannot edit a plate that already exists.")
  #   updateTextInput(session, "edit_plate", value = "-1")
  # }

  # if((!grepl("^plate_\\d+$", input$edit_plate)) && input$edit_plate != "-1") {
  #   showNotification("plate must be in the form plate_number")
  #   updateTextInput(session, "edit_plate", value = "-1")
  # }
})

observeEvent(input$edit_plate, {
  if (input$edit_plate == "-1") {
    return()
  }
  if (input$edit_plate != "") {
    no_space_plate <- gsub("\\s+", "", input$edit_plate)

    if (grepl("^p(l(a(t)?)?)?$", no_space_plate)) {
      no_space_plate <- "plate_"
    }

    if (!grepl("_", no_space_plate)) {
      no_space_plate <- paste0(no_space_plate, "_")
    }

    if (input$edit_plate != no_space_plate) {
      updateTextInput(session, "edit_plate", value = no_space_plate)
    }
 } else {
   updateTextInput(session, "edit_plate", value = "plate_")
  }
})

observe({
#  req(input$edit_plateid)
 input$plate_header_rows_selected
  selected <-  input$plate_header_rows_selected
  if (length(selected)) {
    cat("Selected row index:", selected, "\n")
  } else {
    cat("No row selected\n")

    enable("selected_experiment_row")
    enable("selected_plate_id")
    enable("selected_plateid_to_edit")
    enable("original_plate_to_edit")
    enable("original_nominal_sample_dilution")
    enable("edit_sc_source")

    updateSelectInput(session, "selected_experiment_row",
                     choices = character(0),
                     selected = NULL)
    updateSelectInput(session, "selected_plate_id",
                      choices = character(0),
                      selected = NULL)

    updateSelectInput(session, "selected_plateid_to_edit",
                      choices = character(0),
                      selected = NULL)

    updateSelectInput(session, "original_plate_to_edit",
                      choices = character(0),
                      selected = NULL)

    updateSelectInput(session, "original_nominal_sample_dilution",
                      choices = character(0),
                      selected = NULL)

    updateSelectInput(session, "original_sc_source",
                      choices = character(0),
                      selected = NULL)

    updateTextInput(session, "edit_plateid", value = "-1")
    hideFeedback("edit_plateid")
    updateTextInput(session, "edit_plate", value = "-1")
    hideFeedback("edit_plate")
    updateNumericInput(session, "edit_sample_dil_factor", value = -1)
    updateTextInput(session, "edit_sc_source", value = "-1")

    disable("selected_experiment_row")
    disable("selected_plate_id")
    disable("selected_plateid_to_edit")
    disable("original_plate_to_edit")
    disable("original_nominal_sample_dilution")
    disable("edit_sc_source")


  }



  cat("current values:\n")
   print(input$selected_experiment_row)
   print(input$selected_plateid_to_edit)
   print(input$edit_plateid)
   cat("edited plate col\n")
   print(input$edit_plate)
})

observeEvent(input$reset_plate_edits, {
  # req(input$selected_plateid_to_edit)
  # req(input$original_plate_to_edit)
  # req(input$original_nominal_sample_dilution)
  updateTextInput(session, "edit_plateid", value = input$selected_plateid_to_edit)
  updateTextInput(session, "edit_plate", value = input$original_plate_to_edit)
  updateNumericInput(session,"edit_sample_dil_factor", value = input$original_nominal_sample_dilution)
  updateTextInput(session, "edit_sc_source", value = input$original_sc_source)

})


## Modal for rename plateid
observeEvent(input$rename_plateid, {
showModal(
  modalDialog(
    title = paste0("Confirm plateid Rename: ", input$readxMap_study_accession, " - " ,input$selected_experiment_row),
    paste("Are you sure you want to rename the plateid ",
           input$selected_plateid_to_edit, "to", input$edit_plateid, "?"),
    footer = tagList(
      actionButton("confirm_plateid_edit", "Confirm"),
      modalButton("Cancel")
    ),
    easyClose = TRUE
  )
)
})

# Confirm plateid name update
observeEvent(input$confirm_plateid_edit, {
  cat("\nPRESSED CONFIRM for plateid \n")

  update_plateid(selected_study = input$readxMap_study_accession,
                 selected_experiment = input$selected_experiment_row,
                 selected_plate_id = input$selected_plate_id ,
                 edited_plateid = input$edit_plateid)
  # update_plate_names(selected_study = input$readxMap_study_accession,
  #                    selected_experiment = input$selected_experiment_row,
  #                    original_plate = input$selected_plateid_to_edit,
  #                    edited_plate = input$edit_plateid)
  removeModal() # remove once click confirm
  showNotification("plateid Name Updated")

  # RELOAD plate_header data
  updated_plate_df <- fetch_study_header(selected_study = input$readxMap_study_accession)

  # Reset UI based on the updated data
  updateSelectInput(session, "selected_plateid_to_edit",
                    choices = updated_plate_df$plateid,
                    selected = input$edit_plateid)

  updateTextInput(session, "edit_plateid", value = NULL)

  # Refresh datatable
  output$plate_header <- renderDT({
    datatable(updated_plate_df, selection = "single", filter = 'top')
  })

})

## Modal for rename selected 'plate' column
observeEvent(input$rename_plate_var, {
  showModal(
    modalDialog(
      title = paste0("Confirm plate Rename: ", input$readxMap_study_accession, " - " ,input$selected_experiment_row),
      paste("Are you sure you want to rename the plate ",
            input$original_plate_to_edit, "to", input$edit_plate, "?"),
      footer = tagList(
        actionButton("confirm_plate_edit", "Confirm"),
        modalButton("Cancel")
      ),
      easyClose = TRUE
    )
  )
})
## confirm plate col rename
observeEvent(input$confirm_plate_edit, {
  cat("\nPRESSED CONFIRM for plate \n")
  ## update here
  update_plate_var(selected_study = input$readxMap_study_accession,
                   selected_experiment = input$selected_experiment_row,
                   selected_plate_id = input$selected_plate_id,
                   edited_plate = input$edit_plate)
  removeModal() # remove once click confirm
  showNotification("Plate Name Updated")

  # RELOAD plate_header data
  updated_plate_df <- fetch_study_header(selected_study = input$readxMap_study_accession)

  # Reset UI based on the updated data
  updateSelectInput(session, "original_plate_to_edit",
                    choices = updated_plate_df$plate,
                    selected = input$edit_plate)

  updateTextInput(session, "edit_plate", value = NULL)

  # Refresh datatable
  output$plate_header <- renderDT({
    datatable(updated_plate_df, selection = "single", filter = 'top')
  })

})

## Modal for updating the sample dilution factor
observeEvent(input$update_nominal_sample_dilution, {
  showModal(
    modalDialog(
      title = paste0("Confirm Sample Dilution Factor Update: ", input$readxMap_study_accession, " - " ,input$selected_experiment_row),
      paste("Are you sure you want to update the sample serum dilution factor from ",
            input$original_nominal_sample_dilution, "to", input$edit_sample_dil_factor, "?"),
      footer = tagList(
        actionButton("confirm_sample_dil_edit", "Confirm"),
        modalButton("Cancel")
      ),
      easyClose = TRUE
    )
  )
})

# confirm update for sample dilution factor
observeEvent(input$confirm_sample_dil_edit, {
  cat("Pressed confirm sample dilution factor edit")
  # do update
  update_nominal_sample_dilution(selected_study = input$readxMap_study_accession,
                                selected_experiment = input$selected_experiment_row,
                                selected_plate_id = input$selected_plate_id,
                                edited_nominal_sample_dilution = input$edit_sample_dil_factor)
  removeModal() # remove once click confirm
  showNotification("Sample Dilution Factor Updated")

  # RELOAD plate_header data
  updated_plate_df <- fetch_study_header(selected_study = input$readxMap_study_accession)

  # Reset UI based on the updated data
  updateSelectInput(session, "original_nominal_sample_dilution",
                    choices = updated_plate_df$nominal_sample_dilution,
                    selected = input$edit_sample_dil_factor)

  updateTextInput(session, "edit_sample_dil_factor", value = NULL)

  # Refresh datatable
  output$plate_header <- renderDT({
    datatable(updated_plate_df, selection = "single", filter = 'top')
  })

})

## modal for updating plate standard curve source
observeEvent(input$rename_plate_sc_source, {
  showModal(
    modalDialog(
      title = paste0("Confirm Standard Curve Source Update: ", input$readxMap_study_accession, " - " ,input$selected_experiment_row),
      paste("Are you sure you want to rename the standard curve source in the standards from ",
            input$original_sc_source, "to", input$edit_sc_source, "?"),
      footer = tagList(
        actionButton("confirm_standard_curve_source_edit", "Confirm"),
        modalButton("Cancel")
      ),
      easyClose = TRUE
    )
  )
})

## confirm update for plate-specific standard curve source
observeEvent(input$confirm_standard_curve_source_edit, {
  cat("Pressed confirm standard curve source")
  # do update
  update_plate_standard_curve_source(selected_study = input$readxMap_study_accession,
                                   selected_experiment = input$selected_experiment_row ,
                                   selected_plate_id = input$selected_plate_id,
                                   selected_source = input$original_sc_source,
                                   edited_source = input$edit_sc_source)
  removeModal() # remove once click confirm
  showNotification("Plate Standard Curve Source Updated")

  # RELOAD plate_header data
  updated_plate_df <- fetch_study_header(selected_study = input$readxMap_study_accession)

  # Reset UI based on the updated data
  updateSelectInput(session, "original_sc_source",
                    choices = strsplit(updated_plate_df$standard_curve_sources, ",")[[1]],
                    selected = strsplit(updated_plate_df$standard_curve_sources, ",")[[1]][1])

  updateTextInput(session, "edit_sc_source", value = NULL)

  # Refresh datatable
  output$plate_header <- renderDT({
    datatable(updated_plate_df, selection = "single", filter = 'top')
  })

})



# --- [11.8] removed -> consolidated in delete_study_components(.R/_ui.R) ---


