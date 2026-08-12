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
# --- [11.7] moved to derived_experiments.R ---

# observeEvent(input$readxMap_study_accession, {
#
#   if (input$readxMap_study_accession != "Click here") {
#   initial_source <- obtain_initial_source(input$readxMap_study_accession)
#   }
# })


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


