# =============================================================================
# plate_ops.R  --  the LIVE remnant carved out of load_previous_stored_data.R.
# Everything else in that file (the wide-format loader, the swide_* renderers,
# the RData bundle, and the unmounted stored_plates_ui master view) is dead or
# superseded by data_tab_module.R (long-format Data tab + export) and by the
# tab structure in ui_handler.R. Wide-format data is no longer supported.
#
# What remains here, still sourced into the gated server block:
#   * observeEvent(stored_header_rows_selected) -- plate selection + split/
#     wavelength-subtraction detection (the plate-operations slice).
#   * observeEvent(readxMap_study_accession)    -- study-config initialization.
#   * validators: check_nsample_plate, count_n_std_curve_plates,
#     count_n_dilutions_per_antigen, validate_plate_data (used by live reactives).
# stored_plates_data is now populated by dataTabServer (raw long frames), which
# preserves the contract these observers rely on.
# =============================================================================
observeEvent(input$stored_header_rows_selected, {
  print("in the observeEvent for header_rows_selected")
  selected_studyexpplate$study_accession <- input$readxMap_study_accession
  print(paste("selected study:", selected_studyexpplate$study_accession))
  selected_studyexpplate$experiment_accession <- input$readxMap_experiment_accession
  print(paste("selected experiment:", selected_studyexpplate$experiment_accession))
  selected_studyexpplate$plateid <- stored_plates_data$stored_header[input$stored_header_rows_selected, c("plateid")]
  print(paste("selected plateid:", selected_studyexpplate$plateid))
  selected_studyexpplate$plate <- stored_plates_data$stored_header[input$stored_header_rows_selected, c("plate")]
  print(paste("selected plate:", selected_studyexpplate$plate))
  selected_studyexpplate$nominal_sample_dilution <- stored_plates_data$stored_header[input$stored_header_rows_selected, c("nominal_sample_dilution")]
  print(paste("selected nominal sample dilution", selected_studyexpplate$nominal_sample_dilution))
  selected_studyexpplate$wavelengths <- stored_plates_data$stored_header[input$stored_header_rows_selected, c("wavelengths")]
  print(paste("selected wavelengths", selected_studyexpplate$wavelengths))
  # header_row_selected <- stored_plates_data$stored_header[input$stored_header_rows_selected,]
  # print(header_row_selected)

  plateid <- stored_plates_data$stored_header[
    input$stored_header_rows_selected, "plateid"
  ]
  
  plate_id <- stored_plates_data$stored_header[
    input$stored_header_rows_selected, "plate_id"
  ]

  # --- Check if already split ---
  already_split_sql <- glue::glue("
    SELECT EXISTS (
      SELECT 1
      FROM madi_results.xmap_sample
      WHERE study_accession = '{input$readxMap_study_accession}'
        AND experiment_accession = '{input$readxMap_experiment_accession}'
        AND plateid = '{plateid}'
        AND nominal_sample_dilution IS NOT NULL
        AND nominal_sample_dilution NOT LIKE '%|%'
    ) AS already_split;
  ")
  already_split <- DBI::dbGetQuery(conn, already_split_sql)$already_split


  selected_nominal_dilutions <- strsplit(selected_studyexpplate$nominal_sample_dilution, "\\|")[[1]]
  if (!already_split && length(selected_nominal_dilutions) > 1) {
       split_by_nominal_dilution(TRUE)
  } else {
    split_by_nominal_dilution(FALSE)
  }
  
  ## wavelength check
  wl_parts <- strsplit(selected_studyexpplate$wavelengths, "\\|")[[1]]
  if (length(wl_parts) == 2) {
    delta_experiment <- paste0(input$readxMap_experiment_accession, "|D")
    
    already_subtracted_sql <- glue::glue("
      SELECT EXISTS (
        SELECT 1
        FROM madi_results.xmap_header
        WHERE study_accession     = '{input$readxMap_study_accession}'
          AND experiment_accession = '{delta_experiment}'
          AND plate_id              = '{plate_id}'
      ) AS already_subtracted;
    ")
    already_subtracted <- DBI::dbGetQuery(conn, already_subtracted_sql)$already_subtracted
    
    if (!already_subtracted) {
      show_wavelength_subtraction(TRUE)
    } else {
      show_wavelength_subtraction(FALSE)
    }
  } else {
    show_wavelength_subtraction(FALSE)
  }
  


  # output$selected_plate_text = renderText({
  #   paste0("Selected Plate: ", selected_studyexpplate$plateid)
  # })

  ## identify if the standard curve data is present and store
  # check_standard <- stored_plates_data$stored_standard[ ,"plateid"]
  # selected_studyexpplate$nrows_standard <- length(check_standard[check_standard == selected_studyexpplate$plateid])
  # print(paste(selected_studyexpplate$plateid," nrows_standard:", selected_studyexpplate$nrows_standard))
  #
  # ## identify if the 4 parameter standard curve is calculated and store
  # check_fits <- stored_plates_data$stored_fits[ ,"plateid"]
  # selected_studyexpplate$nrows_fits <- length(check_fits[check_fits == selected_studyexpplate$plateid])
  # print(paste(selected_studyexpplate$plateid," nrows_fits:", selected_studyexpplate$nrows_fits))
  #
  # ## identify if the buffer data is available and store
  # check_buffer <- stored_plates_data$stored_buffer[ ,"plateid"]
  # selected_studyexpplate$nrows_buffer <- length(check_buffer[check_buffer == selected_studyexpplate$plateid])
  # print(paste(selected_studyexpplate$plateid," nrows_buffer:", selected_studyexpplate$nrows_buffer))
  #
  # ## identify if the control data is available and store
  # check_control <- stored_plates_data$stored_control[ , c("plateid")]
  # selected_studyexpplate$nrows_control <- length(check_control[check_control == selected_studyexpplate$plateid])
  # print(paste(selected_studyexpplate$plateid," nrows_control:", selected_studyexpplate$nrows_control))
})

observeEvent(input$readxMap_study_accession, {

  if (input$readxMap_study_accession != "Click here") {
  initial_source <- obtain_initial_source(input$readxMap_study_accession)
  # std <<- stored_plates_data$stored_standard
  # initial_source <<- unique(stored_plates_data$stored_standard$source)[1]

  # Initialize study parameters for a user and study
  study_user_params_nrow <- nrow(fetch_study_configuration(study_accession = input$readxMap_study_accession
                                                           , user = currentuser()))
  if (study_user_params_nrow == 0) {
    intitialize_study_configurations(study_accession = input$readxMap_study_accession,
                                     user = currentuser(), initial_source = initial_source)

  }
  }
})


check_nsample_plate <- function(df, plate_column){
  # Count the number of samples per plate
  plate_sample_counts <- table(df[[plate_column]])

  # all plates must have 2 samples
  min_n_per_plate <- 2
  all_samples_valid <- all(plate_sample_counts >= min_n_per_plate)
  # cat("Total rows in data frame:", nrow(df), "\n")
  # cat("Plate sample counts:", plate_sample_counts, "\n")

  return(all_samples_valid)
}

count_n_std_curve_plates <- function(study_accession, experiment_accession){
  num_plates_query <- paste0("SELECT plateid
  	FROM madi_results.xmap_standard_fits
  	WHERE study_accession = '",study_accession,"' and experiment_accession = '",experiment_accession ,"'
  	GROUP BY plateid;")


  # Run the query and fetch the result as a data frame
  plate_list <- dbGetQuery(conn, num_plates_query)
  cat("Previously stored plate list")
  print(plate_list)

  num_plates <- nrow(plate_list)
  # cat("num_plates")
  #print(num_plates)

  return(num_plates)

}

count_n_dilutions_per_antigen <- function(study_accession, experiment_accession) {

  n_dilutions_query <- paste0("SELECT antigen, COUNT(DISTINCT dilution) AS num_dilutions
  FROM madi_results.xmap_standard
 	WHERE study_accession = '",study_accession,"' and experiment_accession = '",experiment_accession ,"'
  	GROUP BY antigen;")
  # print(n_dilutions_query)

  n_dilution_per_antigen_df <- dbGetQuery(conn, n_dilutions_query)
  return(n_dilution_per_antigen_df)
  # record_count <<- table(n_dilution_per_antigen_df$antigen)
  #
  # # Find antigens with fewer than 5 records
  # if (any(record_count < 5)) {
  #   # Return FALSE if there are antigens with fewer than 5 records
  #   result <- FALSE
  # } else {
  #   # Return TRUE if all antigens have at least 5 records
  #   result <- TRUE
  # }
  # return(result)
}

validate_plate_data <- function(stored_plates_data){

  all_checks_pass <- T
  # Check for stored_sample
  if (!check_nsample_plate(stored_plates_data$stored_sample, "plate_id")) {
    all_checks_pass <- FALSE
  }

  # Check for stored_buffer
  if (!check_nsample_plate(stored_plates_data$stored_buffer, "plateid")) {
    all_checks_pass <- FALSE
  }

  # Check for stored_control
  if (!check_nsample_plate(stored_plates_data$stored_control, "plateid")) {
    all_checks_pass <- FALSE
  }

  # Check for stored_standard
  if (!check_nsample_plate(stored_plates_data$stored_standard, "plateid")) {
    all_checks_pass <- FALSE
  }
  return(all_checks_pass)
}


