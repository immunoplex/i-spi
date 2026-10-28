# =============================================================================
# derived_experiments.R
# -----------------------------------------------------------------------------
# Section 11.7: consolidated home for the two DERIVED-EXPERIMENT generators that
# surface on the Data tab (buttons placed by data_tab_module.R uiOutputs):
#   * ELISA wavelength subtraction    -> moved to wavelength_subtraction_module.R (Data > Raw inputs)
#   * split by nominal sample dilution -> single-dilution curve sets (same exp)
#
# Consolidated from elisa_wavelength_subtraction.R, split_plates_nominal_sample_
# dilution.R, ui_handler.R (renderUIs + observers) and plate_ops.R (eligibility).
# Both paths previously wrote xmap_* but NEVER registered curve_lookup, so the
# derived curves were invisible to the calib pipeline -- fixed via
# register_derived_curves() (idempotent; reuses curve_lookup_functions.R). Each
# operation now runs in ONE transaction on the single app connection `conn` and
# triggers a Data-tab refresh on success.
# =============================================================================



get_insertable_cols <- function(conn, schema, table_name) {
  col_sql <- glue::glue("
    SELECT column_name
    FROM information_schema.columns
    WHERE table_schema = '{schema}'
      AND table_name   = '{table_name}'
      AND column_name NOT IN (
        SELECT column_name 
        FROM information_schema.columns c
        WHERE c.table_schema = '{schema}'
          AND c.table_name   = '{table_name}'
          AND pg_get_serial_sequence('{schema}.' || '{table_name}', c.column_name) IS NOT NULL
      )
    ORDER BY ordinal_position;
  ")
  DBI::dbGetQuery(conn, col_sql)$column_name
}

insert_delta_sql <- function(conn, schema, table_name, df) {
  
  # Rename R columns back to DB column names
  names(df)[names(df) == "mfi"] <- "antibody_mfi"
  names(df)[names(df) == "n"]   <- "antibody_n"
  
  # Dynamically get insertable columns from DB
  insert_cols <- get_insertable_cols(conn, schema, table_name)
  cat("DB cols for", table_name, ":\n", paste(insert_cols, collapse = ", "), "\n")
  cat("df cols:\n", paste(names(df), collapse = ", "), "\n")
  
  # Only use cols that exist in both DB and dataframe
  use_cols <- intersect(insert_cols, names(df))
  cat("inserting into", table_name, "- cols:", paste(use_cols, collapse = ", "), "\n")
  
  col_list <- paste(use_cols, collapse = ", ")
  
  # Build VALUES rows
  rows <- apply(df[, use_cols, drop = FALSE], 1, function(row) {
    vals <- sapply(row, function(v) {
      if (is.na(v)) "NULL"
      else paste0("'", gsub("'", "''", as.character(v)), "'")
    })
    paste0("(", paste(vals, collapse = ", "), ")")
  })
  
  values_sql <- paste(rows, collapse = ",\n")
  
  sql <- glue::glue("
    INSERT INTO {schema}.{table_name} ({col_list})
    VALUES {values_sql};
  ")
  
  DBI::dbExecute(conn, sql)
}
# ELISA wavelength subtraction now lives on Data > Raw inputs > Subtract Wavelengths
# (wavelength_subtraction_module.R): user-named experiment, per-plate and per-wavelength
# choices, replacing the old single-plate Plates-tab button and its '<exp>|D' SQL.




split_plate_nominal_sample_dilution <- function(
    study_accession,
    experiment_accession,
    plateid,
    conn
) {

  showNotification(id = "split_plate_notification", HTML("Splitting plate by nominal sample dilution<span class = 'dots'>"), duration = NULL)
  cat("Splitting plate by nominal sample dilution\n")
  cat("Study:", study_accession, "\n")
  cat("Experiment:", experiment_accession, "\n")
  cat("Plate:", plateid, "\n")

  # -----------------------------
  # Excluded antigens
  # -----------------------------
  exclude_antigens <- c("Well", "Type", "Description", "Region", "Gate", "Total", "% Agg Beads", "Sampling Errors", "Rerun Status",
                        "Device Error", "Plate ID", "Regions Selected", "RP1 Target", "Platform Heater Target", "Platform Temp (°C)",
                        "Bead Map", "Bead Count", "Sample Size (µl)", "Sample Timeout (sec)", "Flow Rate (µl/min)", "Air Pressure (psi)",
                        "Sheath Pressure (psi)", "Original DD Gates", "Adjusted DD Gates", "RP1 Gates", "User", "Access Level", "Acquisition Time",
                        "acquisition_time", "Reader Serial Number", "Platform Serial Number", "Software Version", "LXR Library", "Reader Firmware",
                        "Platform Firmware", "DSP Version", "Board Temp (°C)", "DD Temp (°C)", "CL1 Temp (°C)", "CL2 Temp (°C)", "DD APD (Volts)",
                        "CL1 APD (Volts)", "CL2 APD (Volts)", "High Voltage (Volts)", "RP1 PMT (Volts)", "DD Gain", "CL1 Gain", "CL2 Gain", "RP1 Gain")

  exclude_antigens_sql <- paste0("'", paste(exclude_antigens, collapse = "', '"), "'")

  # -----------------------------
  # Get nominal dilutions
  # -----------------------------
  print("PLate id check nominal\n")
  print(plateid)
  get_nominal <- glue::glue("
    SELECT DISTINCT nominal_sample_dilution
    FROM madi_results.xmap_header
    WHERE study_accession = '{study_accession}'
      AND experiment_accession = '{experiment_accession}'
      AND plateid = '{plateid}'
      AND nominal_sample_dilution IS NOT NULL;
  ")

  nominal_vals <- DBI::dbGetQuery(conn, get_nominal)$nominal_sample_dilution
  nominal_vals <- strsplit(nominal_vals, "\\|")[[1]]
  if (length(nominal_vals) <= 1) {
    showNotification("Plate has one or zero nominal dilutions — nothing to split.")
    return(invisible(NULL))
  }

  # -----------------------------
  # Helper: duplicate rows ONCE
  # -----------------------------
  duplicate_by_nominal <- function(table, extra_where = "") {

    # Get column names dynamically
    col_sql <- glue::glue("
      SELECT column_name
      FROM information_schema.columns
      WHERE table_schema = 'madi_results'
        AND table_name = '{table}'
      ORDER BY ordinal_position;
    ")

    cols <- DBI::dbGetQuery(conn, col_sql)$column_name

    # obtain primary key for the table
      pk_sql <- glue::glue("
    SELECT kcu.column_name
    FROM information_schema.table_constraints tc
    JOIN information_schema.key_column_usage kcu
      ON tc.constraint_name = kcu.constraint_name
     AND tc.table_schema = kcu.table_schema
    WHERE tc.constraint_type = 'PRIMARY KEY'
      AND tc.table_schema = 'madi_results'
      AND tc.table_name = '{table}';
  ")

    pk_cols <- DBI::dbGetQuery(conn, pk_sql)$column_name

    base_cols <- setdiff(cols, c(pk_cols,"nominal_sample_dilution"))

    select_cols <- paste(base_cols, collapse = ", ")
    insert_cols <- paste(c(base_cols, "nominal_sample_dilution"), collapse = ", ")

    for (dil in nominal_vals) {

      # on the mixed plate samples at one sample dilution will not be split into other sample dilutions.
      dilution_clause <- if (table == "xmap_sample") {
        glue("AND src.dilution::text = '{dil}'")
      } else {
        ""
      }

      sql <- glue::glue("
  INSERT INTO madi_results.{table} ({insert_cols})
  SELECT
    {select_cols},
    '{dil}' AS nominal_sample_dilution
  FROM madi_results.{table} src
  WHERE src.study_accession = '{study_accession}'
    AND src.experiment_accession = '{experiment_accession}'
    AND src.plateid = '{plateid}'
    AND src.nominal_sample_dilution LIKE '%|%'
    {extra_where}
    {dilution_clause}
    AND NOT EXISTS (
      SELECT 1
      FROM madi_results.{table} tgt
      WHERE tgt.study_accession = src.study_accession
        AND tgt.experiment_accession = src.experiment_accession
        AND tgt.plateid = src.plateid
        AND tgt.nominal_sample_dilution = '{dil}'
    );
")



      DBI::dbExecute(conn, sql)
    }
  }


  duplicate_by_nominal("xmap_header")

  # -----------------------------
  # SAMPLE
  # -----------------------------
  duplicate_by_nominal(
    "xmap_sample",
    glue("AND antigen NOT IN ({exclude_antigens_sql})"))

  # -----------------------------
  # STANDARD
  # -----------------------------
  duplicate_by_nominal(
    "xmap_standard",
    glue("AND antigen NOT IN ({exclude_antigens_sql})"))

  # -----------------------------
  # CONTROL
  # -----------------------------
  duplicate_by_nominal("xmap_control",
                       glue("AND antigen NOT IN ({exclude_antigens_sql})"))

  # -----------------------------
  # BUFFER / BLANK
  # -----------------------------
  duplicate_by_nominal("xmap_buffer",
                       glue("AND antigen NOT IN ({exclude_antigens_sql})"))

  showNotification(id = "split_plate_notification", "Plate successfully split by nominal sample dilution.", duration = NULL)
  removeNotification(id = "split_plate_notification")
  cat("Split completed.\n")
}



# Register curve_lookup rows for a derived scope -- the step both generators
# were missing. Idempotent (register_curve_lookup -> INSERT ... ON CONFLICT DO
# NOTHING). Runs on the passed (transaction) connection; sees its own
# uncommitted xmap_standard rows within the txn.
register_derived_curves <- function(conn, project, study, experiment) {
  std <- DBI::dbGetQuery(conn, glue::glue_sql(
    "SELECT study_accession, experiment_accession, plateid, plate,
            nominal_sample_dilution, source, wavelength, antigen, feature
       FROM madi_results.xmap_standard
      WHERE project_id = {project}
        AND study_accession = {study}
        AND experiment_accession = {experiment}",
    .con = conn))
  if (is.null(std) || nrow(std) == 0) {
    cat("  [derived] no standards for", experiment, "-- no curves registered\n")
    return(invisible(list(success = TRUE, rows_inserted = 0L, message = "no standards")))
  }
  register_curve_lookup(conn = conn, standards_df = std, project_id = project)
}





## Split by nominal sample dilution -- transactional + curve_lookup + refresh
observeEvent(input$split_plates_nominal, {
  hr    <- stored_plates_data$stored_header[input$stored_header_rows_selected, ]
  study <- hr$study_accession
  exp   <- hr$experiment_accession
  proj  <- tryCatch(userWorkSpaceID(), error = function(e) NA)
  ok <- tryCatch({
    pool::poolWithTransaction(db_pool, function(conn) {
      split_plate_nominal_sample_dilution(
        study_accession = study, experiment_accession = exp,
        plateid = hr$plateid, conn = conn)
      register_derived_curves(conn, project = proj, study = study, experiment = exp)
    })
    TRUE
  }, error = function(e) {
    showNotification(paste("Split failed (rolled back):", conditionMessage(e)),
                     type = "error", duration = NULL); FALSE
  })
  if (isTRUE(ok)) {
    reload_trigger(reload_trigger() + 1)
    refresh_experiment_trigger(refresh_experiment_trigger() + 1)
    showNotification("Plate split by dilution; curves registered.", type = "message")
  }
})

## --- Data-tab buttons (placed by data_tab_module.R uiOutputs) ---
output$split_plate_nominal_UI <- renderUI({
  req(input$stored_header_rows_selected)
  if ((split_by_nominal_dilution())) {
    actionButton("split_plates_nominal", "Split Plate by Nominal Sample Dilution")
  } else {
    NULL
  }
})


## --- Eligibility: on plate selection, decide which buttons to show ---
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
