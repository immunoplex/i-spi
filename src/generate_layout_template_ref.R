#' =============================================================================
#' LAYOUT TEMPLATE GENERATION
#' =============================================================================

#' Extract plate numbers from a data frame using priority-based logic
#'
#' This consolidates the duplicate plate number extraction logic that was
#' previously in both assign_plate_numbers() and generate_layout_template().
#'
#' @param df Data frame with potential plate identifier columns
#' @param priority_cols Character vector of column names to try in order
#' @param prefix Prefix for fallback plate numbers (default: "plate_")
#' @return Character vector of plate numbers (one per row)
#'
extract_plate_numbers_from_df <- function(df,
                                          priority_cols = c("plateid", "source_file", "plate_filename", "plate"),
                                          prefix = "plate_") {
  n_rows <- nrow(df)
  plate_numbers <- rep(NA_character_, n_rows)
  extraction_sources <- rep(NA_character_, n_rows)

  # Try each priority column in order

  for (col in priority_cols) {
    if (col %in% names(df)) {
      for (i in seq_len(n_rows)) {
        if (is.na(plate_numbers[i])) {
          extracted <- extract_plate_number(df[[col]][i])
          if (!is.na(extracted)) {
            plate_numbers[i] <- extracted
            extraction_sources[i] <- col
          }
        }
      }
    }
  }

  # Fallback: assign sequential numbers to any remaining NAs
  na_indices <- which(is.na(plate_numbers))
  if (length(na_indices) > 0) {
    # Find the max number already extracted to continue from there
    existing_nums <- as.numeric(gsub(paste0("^", prefix), "",
                                     plate_numbers[!is.na(plate_numbers)]))
    existing_nums <- existing_nums[!is.na(existing_nums)]
    next_num <- if (length(existing_nums) > 0) max(existing_nums) + 1 else 1

    for (i in na_indices) {
      plate_numbers[i] <- paste0(prefix, next_num)
      extraction_sources[i] <- "fallback"
      next_num <- next_num + 1
    }
  }

  # # Handle duplicates
  # plate_numbers <- resolve_duplicate_plate_numbers(plate_numbers, prefix)

  list(
    plate_numbers = plate_numbers,
    sources = extraction_sources
  )
}


#' Resolve duplicate plate numbers by incrementing
#'
#' @param plate_numbers Character vector of plate numbers
#' @param prefix Prefix used for plate numbers
#' @return Character vector with duplicates resolved
#'
resolve_duplicate_plate_numbers <- function(plate_numbers, prefix = "plate_") {
  seen <- c()
  result <- plate_numbers

  for (i in seq_along(result)) {
    if (result[i] %in% seen) {
      # Extract numeric part and find next available
      base_num <- as.numeric(gsub(paste0("^", prefix), "", result[i]))
      if (is.na(base_num)) base_num <- 1

      while (paste0(prefix, base_num) %in% seen) {
        base_num <- base_num + 1
      }
      result[i] <- paste0(prefix, base_num)
    }
    seen <- c(seen, result[i])
  }

  result
}


#' Parse all descriptions in a plate data frame at once
#'
#' This consolidates the multiple separate sapply() calls to
#' parse_description_with_defaults() for each field (subject_id,
#' dilution_factor, timepoint, groupa, groupb) into a single pass.
#'
#' @param plate_data Data frame with Type and Description columns
#' @param delimiter Delimiter used in Description field
#' @param element_order Named vector mapping element names to positions
#' @param bcs_element_order Element order for Blanks/Controls/Standards
#' @param use_defaults Whether to apply defaults for missing values
#' @return Data frame with parsed columns added
#'
parse_all_descriptions <- function(plate_data,
                                   delimiter = "_",
                                   element_order = c("PatientID", "TimePeriod", "DilutionFactor"),
                                   bcs_element_order = c("Source", "DilutionFactor"),
                                   use_defaults = TRUE) {

  n_rows <- nrow(plate_data)

  # Get element positions
  xpatientid <- get_element_position("PatientID", element_order)
  xtimeperiod <- get_element_position("TimePeriod", element_order)
  xdilutionfactor <- get_element_position("DilutionFactor", element_order)
  xgroupa <- get_element_position("SampleGroupA", element_order)
  xgroupb <- get_element_position("SampleGroupB", element_order)
  bcssource <- get_element_position("Source", bcs_element_order)
  bcsdilutionfactor <- get_element_position("DilutionFactor", bcs_element_order)

  # Initialize result columns
  result <- data.frame(
    subject_id = character(n_rows),
    specimen_dilution_factor = numeric(n_rows),
    timepoint_tissue_abbreviation = character(n_rows),
    specimen_source = character(n_rows),
    groupa = character(n_rows),
    groupb = character(n_rows),
    stringsAsFactors = FALSE
  )

  # Process each row based on specimen type
  for (i in seq_len(n_rows)) {
    type_val <- plate_data$Type[i]
    desc_val <- plate_data$Description[i]
    type_char <- if (is.na(type_val)) NA_character_ else substr(type_val, 1, 1)

    if (is.na(type_char)) {
      # Empty row
      result$subject_id[i] <- ""
      result$specimen_dilution_factor[i] <- 1
      result$timepoint_tissue_abbreviation[i] <- ""
      result$specimen_source[i] <- ""
      result$groupa[i] <- ""
      result$groupb[i] <- ""

    } else if (type_char == "X") {
      # Sample row - parse with full element order
      if (use_defaults) {
        parsed <- parse_description_with_defaults(
          description = desc_val,
          delimiter = delimiter,
          element_order = element_order,
          optional_elements = c("SampleGroupA", "SampleGroupB")
        )
        result$subject_id[i] <- parsed$subject_id
        result$specimen_dilution_factor[i] <- parsed$dilution_factor
        result$timepoint_tissue_abbreviation[i] <- parsed$timeperiod
        result$groupa[i] <- parsed$groupa
        result$groupb[i] <- parsed$groupb
      } else {
        result$subject_id[i] <- str_split_i(desc_val, delimiter, xpatientid)
        result$specimen_dilution_factor[i] <- as.numeric(str_split_i(desc_val, delimiter, xdilutionfactor))
        result$timepoint_tissue_abbreviation[i] <- str_split_i(desc_val, delimiter, xtimeperiod)
        result$groupa[i] <- if (!is.na(xgroupa)) str_split_i(desc_val, delimiter, xgroupa) else ""
        result$groupb[i] <- if (!is.na(xgroupb)) str_split_i(desc_val, delimiter, xgroupb) else ""
      }
      result$specimen_source[i] <- "sample"

    } else if (type_char %in% c("C", "B", "S")) {
      # Control, Blank, or Standard
      result$subject_id[i] <- if (type_char == "B") "1" else substr(type_val, 2, nchar(type_val))

      # Parse dilution factor from BCS description
      dilution_val <- str_split_i(desc_val, delimiter, bcsdilutionfactor)
      result$specimen_dilution_factor[i] <- if (!is.na(suppressWarnings(as.numeric(dilution_val)))) {
        as.numeric(dilution_val)
      } else {
        1
      }

      result$timepoint_tissue_abbreviation[i] <- ""
      result$specimen_source[i] <- str_split_i(desc_val, delimiter, bcssource)
      result$groupa[i] <- ""
      result$groupb[i] <- ""

    } else {
      # Unknown type
      result$subject_id[i] <- "0"
      result$specimen_dilution_factor[i] <- 1
      result$timepoint_tissue_abbreviation[i] <- ""
      result$specimen_source[i] <- ""
      result$groupa[i] <- ""
      result$groupb[i] <- ""
    }
  }

  result
}


#' Apply default values to sample rows in plate_well_map
#'
#' Centralizes the default value application logic that was scattered
#' across multiple locations in the original code.
#'
#' @param plate_well_map Data frame to modify
#' @param defaults Named list of default values
#' @param verbose Whether to print messages about applied defaults
#' @return Modified data frame with defaults applied
#'
apply_sample_defaults <- function(plate_well_map,
                                  defaults = list(
                                    subject_id = "1",
                                    specimen_dilution_factor = 1,
                                    timepoint_tissue_abbreviation = "T0",
                                    groupa = "Unknown",
                                    groupb = "Unknown"
                                  ),
                                  verbose = TRUE) {

  sample_mask <- plate_well_map$specimen_type == "X"

  if (verbose) {
    cat("╔══════════════════════════════════════════════════════════╗\n")
    cat("║  Applying default values to sample rows                  ║\n")
    cat("╚══════════════════════════════════════════════════════════╝\n")
  }

  for (col_name in names(defaults)) {
    if (col_name %in% names(plate_well_map)) {
      default_val <- defaults[[col_name]]

      # Identify rows needing defaults
      if (is.numeric(default_val)) {
        needs_default <- sample_mask & (
          is.na(plate_well_map[[col_name]]) |
            is.nan(plate_well_map[[col_name]])
        )
      } else {
        needs_default <- sample_mask & (
          is.na(plate_well_map[[col_name]]) |
            plate_well_map[[col_name]] == ""
        )
      }

      n_defaults <- sum(needs_default, na.rm = TRUE)
      if (n_defaults > 0) {
        plate_well_map[[col_name]][needs_default] <- default_val
        if (verbose) {
          cat("  → Set", n_defaults, "blank", col_name, "values to '",
              as.character(default_val), "'\n", sep = "")
        }
      }
    }
  }

  if (verbose) {
    cat("╚══════════════════════════════════════════════════════════╝\n")
  }

  plate_well_map
}



# assign_plate_numbers()
#' Extract and assign plate numbers to header list (REFACTORED)
#'
#' Now uses the shared extract_plate_numbers_from_df() function instead of
#' duplicating the extraction logic.
#'
#' @param header_list List of header data frames
#' @return Modified header list with plate numbers assigned
#'
assign_plate_numbers <- function(header_list) {

  # Convert list to data frame for unified processing
  header_df <- do.call(rbind, lapply(seq_along(header_list), function(i) {
    df <- header_list[[i]]
    df$list_index <- i
    df$file_name_from_list <- names(header_list)[i]
    df
  }))

  # Use shared extraction function
  extraction_result <- extract_plate_numbers_from_df(
    header_df,
    priority_cols = c("plateid", "file_name", "source_file"),
    prefix = "plate_"
  )

  # Apply results back to header_list
  for (i in seq_along(header_list)) {
    plate_num <- extraction_result$plate_numbers[i]
    source <- extraction_result$sources[i]
    file_name <- names(header_list)[i]

    header_list[[i]]$plate <- plate_num

    if (is.na(header_list[[i]]$plateid[1]) || header_list[[i]]$plateid[1] == "") {
      header_list[[i]]$plateid <- plate_num
    }

    # Log extraction result
    if (source == "fallback") {
      cat("  ⚠️  ", file_name, "→ Could not extract plate number, using fallback:", plate_num, "\n")
    } else {
      cat("  ✓ ", file_name, " → ", plate_num, " (from ", source, ")\n", sep = "")
    }
  }

  header_list
}


# REFACTORED generate_layout_template()
#' Generate Layout Template with Description Validation (REFACTORED)
#'
#' Creates an Excel layout template for batch plate uploads.
#'
#' Key improvements:
#' - Uses shared extract_plate_numbers_from_df() instead of duplicate logic
#' - Uses parse_all_descriptions() for single-pass description parsing
#' - Uses apply_sample_defaults() for centralized default handling
#'
#' @param all_plates Combined plate data from all uploaded files
#' @param study_accession Study accession ID
#' @param experiment_accession Experiment accession ID
#' @param n_wells Number of wells on the plate (96, 384, etc.)
#' @param header_list List of header metadata from each plate file
#' @param output_file Path to save the Excel template
#' @param project_id Project identifier
#' @param description_status List from check_description_elements() - if NULL, will be computed
#' @param delimiter Character used to separate Description elements (default: "_")
#' @param element_order Order of elements in Description for samples
#' @param bcs_element_order Order of elements for Blanks/Controls/Standards
#'
#' @return Writes Excel file to output_file path
#'

generate_layout_template <- function(all_plates,
                                        study_accession,
                                        experiment_accession,
                                        n_wells,
                                        header_list,
                                        output_file,
                                        project_id = userWorkSpaceID(),
                                        description_status = NULL,
                                        delimiter = NULL,
                                        element_order = NULL,
                                        bcs_element_order = NULL,
                                        assay_response_long_override = NULL,
                                        feature_value = NULL) {

  wb <- createWorkbook()
  bold_style <- createStyle(textDecoration = "bold")
  italic_style <- createStyle(textDecoration = "italic")

  # ==========================================================================
  # STEP 1: Resolve configuration parameters
  # ==========================================================================
  config <- resolve_layout_config(
    all_plates = all_plates,
    description_status = description_status,
    delimiter = delimiter,
    element_order = element_order,
    bcs_element_order = bcs_element_order
  )

  # Resolve feature value: explicit parameter > input$feature_value > NA
  input_feature_value <- if (!is.null(feature_value) && nzchar(feature_value)) {
    feature_value
  } else if (exists("input") && !is.null(input$feature_value)) {
    input$feature_value
  } else {
    NA_character_
  }

  description_status <- config$description_status
  description_delimiter <- config$delimiter
  XElementOrder <- config$element_order
  BCSElementOrder <- config$bcs_element_order

  cat("Using delimiter:", description_delimiter, "\n")
  cat("Using XElementOrder:", paste(XElementOrder, collapse = ", "), "\n")
  cat("Using BCSElementOrder:", paste(BCSElementOrder, collapse = ", "), "\n\n")

  # ==========================================================================
  # STEP 2: Process plate_id from headers
  # ==========================================================================
  plate_id <- build_plate_id_df(
    header_list = header_list,
    study_accession = study_accession,
    experiment_accession = experiment_accession,
    n_wells = n_wells,
    project_id = project_id
  )

  # ==========================================================================
  # STEP 3: Build antigen list
  # ==========================================================================
  antigen_df <- build_antigen_df(
    all_plates = all_plates,
    study_accession = study_accession,
    experiment_accession = experiment_accession,
    project_id = project_id # add project id
  )

  # ==========================================================================
  # STEP 4: Build plates_map
  # ==========================================================================
  plate_well_map <- build_plates_map(
    all_plates = all_plates,
    plate_id = plate_id,
    header_list = header_list,
    study_accession = study_accession,
    experiment_accession = experiment_accession,
    n_wells = n_wells,
    project_id = project_id,
    description_status = description_status,
    delimiter = description_delimiter,
    element_order = XElementOrder,
    bcs_element_order = BCSElementOrder,
    feature_value = input_feature_value
  )

  # ==========================================================================
  # STEP 5: Build subject_groups
  # ==========================================================================
  subject_groups <- build_subject_groups(
    plate_well_map = plate_well_map,
    study_accession = study_accession,
    description_status = description_status,
    delimiter = description_delimiter,
    element_order = XElementOrder
  )

  # ==========================================================================
  # STEP 6: Build timepoint sheet
  # ==========================================================================
  timepoint <- build_timepoint_df(
    plate_well_map = plate_well_map,
    study_accession = study_accession
  )

  # ==========================================================================
  # STEP 7: Join nominal sample dilution
  # ==========================================================================
  nsd_result <- calculate_nominal_sample_dilution(
    plate_well_map,
    project_id,
    study_accession,
    experiment_accession
  )

  plate_id <- plate_id %>%
    left_join(nsd_result, by = c("project_id", "study_name", "experiment_name", "plateid"))

  plate_well_map <- plate_well_map %>%
    left_join(nsd_result, by = c("project_id", "study_name", "experiment_name", "plateid"))

  # Reorder columns
  plate_id <- reorder_plate_id_columns(plate_id)
  plate_well_map <- reorder_plates_map_columns(plate_well_map)

  # ==========================================================================
  # STEP 7.5: Build assay_response_long sheet
  # ==========================================================================
  if (!is.null(assay_response_long_override)) {
    # xPONENT pathway: use pre-built long-format data (preserves bead counts)
    cat("\n  → Using pre-built assay_response_long (xPONENT pathway)\n")
    assay_response_long <- assay_response_long_override

    # Add context columns
    assay_response_long$project_id       <- project_id
    assay_response_long$study_name       <- study_accession
    assay_response_long$experiment_name  <- experiment_accession

    # Override feature with user-supplied value (replaces hardcoded "Net_MFI")
    assay_response_long$feature          <- input_feature_value

    # Select and order final columns
    final_cols <- c("project_id", "study_name", "experiment_name",
                    "plateid", "well", "antigen", "feature",
                    "assay_response", "assay_bead_count")
    available_final <- intersect(final_cols, names(assay_response_long))
    assay_response_long <- assay_response_long[, available_final, drop = FALSE]

    cat("  → assay_response_long:", nrow(assay_response_long), "rows,",
        ncol(assay_response_long), "cols\n")
  } else {
    # Raw File pathway: derive from wide-format plate data
    assay_response_long <- create_assay_response_long(
      all_plates = all_plates,
      plate_id = plate_id,
      antigen_list = antigen_df,
      project_id = project_id,
      study_name = study_accession,
      experiment_name = experiment_accession
    )
  }

  # ==========================================================================
  # STEP 8: Write workbook
  # ==========================================================================
  cell_valid_table <- tibble(
    l_asy_constraint_method = c("default", "user_defined", "range_of_blanks", "geometric_mean_of_blanks")
  )

  workbook <- list(
    plate_id = plate_id,
    subject_groups = subject_groups,
    timepoint = timepoint,
    antigen_list = antigen_df,
    plates_map = plate_well_map,
    assay_response_long = assay_response_long,  # <-- NEW
    cell_valid = cell_valid_table
  )

  write_workbook_sheets_v2(wb, workbook, bold_style, italic_style)

  # ==========================================================================
  # STEP 9: Validation and README
  # ==========================================================================
  add_validation_readme(
    wb = wb,
    plate_well_map = plate_well_map,
    subject_groups = subject_groups,
    timepoint = timepoint,
    description_status = description_status,
    study_accession = study_accession,
    experiment_accession = experiment_accession
  )

  # Save the workbook
  saveWorkbook(wb, output_file, overwrite = TRUE)
  cat("\n✓ Layout template saved to:", output_file, "\n")
  cat("  → Includes assay_response_long sheet with",
      if(!is.null(assay_response_long)) nrow(assay_response_long) else 0,
      "rows\n")
}



# HELPER FUNCTIONS FOR generate_layout_template()

#' Resolve configuration parameters for layout generation
resolve_layout_config <- function(all_plates, description_status, delimiter,
                                  element_order, bcs_element_order) {

  # Handle description_status
  if (is.null(description_status)) {
    check_delimiter <- if (!is.null(delimiter)) delimiter else
      if (exists("input") && !is.null(input$description_delimiter)) input$description_delimiter else "_"

    description_status <- check_description_elements(
      plate_data = all_plates,
      delimiter = check_delimiter,
      required_elements = 3
    )

    cat("\n╔══════════════════════════════════════════════════════════╗\n")
    cat("║  Description Status (computed)                           ║\n")
    cat("╚══════════════════════════════════════════════════════════╝\n")
    cat("  → Has content:", description_status$has_content, "\n")
    cat("  → Has sufficient elements:", description_status$has_sufficient_elements, "\n")
    cat("  → Min elements found:", description_status$min_elements_found, "\n")
    cat("╚══════════════════════════════════════════════════════════╝\n\n")
  }

  # Get delimiter
  resolved_delimiter <- if (!is.null(delimiter)) {
    delimiter
  } else if (description_status$has_content && exists("input") && !is.null(input$description_delimiter)) {
    input$description_delimiter
  } else {
    "_"
  }

  # Get element orders
  resolved_element_order <- if (!is.null(element_order)) {
    element_order
  } else if (description_status$has_sufficient_elements && exists("input") && !is.null(input$XElementOrder)) {
    input$XElementOrder
  } else {
    c("PatientID", "TimePeriod", "DilutionFactor")
  }

  resolved_bcs_element_order <- if (!is.null(bcs_element_order)) {
    bcs_element_order
  } else if (description_status$has_content && exists("input") && !is.null(input$BCSElementOrder)) {
    input$BCSElementOrder
  } else {
    c("Source", "DilutionFactor")
  }

  list(
    description_status = description_status,
    delimiter = resolved_delimiter,
    element_order = resolved_element_order,
    bcs_element_order = resolved_bcs_element_order
  )
}


#' Build plate_id data frame from headers (REFACTORED)
#'
#' Uses shared extract_plate_numbers_from_df() instead of duplicate logic
build_plate_id_df <- function(header_list, study_accession, experiment_accession,
                              n_wells, project_id) {

  cat("\n=== BUILDING PLATE_ID DATA FRAME ===\n")

  # Combine header list
  plate_id <- do.call(rbind, header_list)

  cat("Header columns before processing:", paste(names(plate_id), collapse = ", "), "\n")

  # Add basic fields
  plate_id$study_name <- study_accession
  plate_id$experiment_name <- experiment_accession
  plate_id$number_of_wells <- n_wells
  plate_id$project_id <- project_id

  # Clean plateid from source_file if present
  if ("source_file" %in% names(plate_id) && !all(is.na(plate_id$source_file))) {
    plate_id$plateid <- clean_plate_id(plate_id$source_file)
  }

  # Handle plate_filename
  if ("file_name" %in% names(plate_id)) {
    names(plate_id)[names(plate_id) == "file_name"] <- "plate_filename"
    cat("  ✓ Renamed 'file_name' to 'plate_filename'\n")
  } else if ("source_file" %in% names(plate_id)) {
    plate_id$plate_filename <- plate_id$source_file
    cat("  ✓ Created plate_filename from source_file\n")
  } else {
    plate_id$plate_filename <- names(header_list)
    cat("  ✓ Created plate_filename from header_list names\n")
  }

  # REFACTORED: Use shared extraction function instead of duplicate loops
  cat("  → Extracting plate numbers...\n")
  extraction_result <- extract_plate_numbers_from_df(
    plate_id,
    priority_cols = c("plateid", "source_file", "plate_filename", "plate"),
    prefix = "plate_"
  )

  plate_id$plate_number <- extraction_result$plate_numbers

  # Log results
  cat("  ✓ Plate numbers extracted/assigned:\n")
  for (i in seq_len(nrow(plate_id))) {
    source_name <- if ("source_file" %in% names(plate_id)) plate_id$source_file[i] else
      if ("plate_filename" %in% names(plate_id)) plate_id$plate_filename[i] else
        paste0("Row ", i)
    cat("      ", source_name, " → ", plate_id$plate_number[i],
        " (from ", extraction_result$sources[i], ")\n", sep = "")
  }

  # Create plate_id column
  if ("plate_filename" %in% names(plate_id)) {
    plate_id$plate_id <- clean_plate_id(plate_id$plate_filename)
    cat("  ✓ Created plate_id from plate_filename\n")
  } else if ("source_file" %in% names(plate_id)) {
    plate_id$plate_id <- clean_plate_id(plate_id$source_file)
    cat("  ✓ Created plate_id from source_file\n")
  }

  # Set plateid if not present
  if (!"plateid" %in% names(plate_id) || is.na(plate_id$plateid[1]) || plate_id$plateid[1] == "") {
    plate_id$plateid <- plate_id$plate_number
    cat("  ✓ Created plateid from plate_number\n")
  }

  cat("Columns after processing:", paste(names(plate_id), collapse = ", "), "\n")

  required_cols <- c("project_id", "study_name", "experiment_name", "number_of_wells",
                     "plate_number", "plateid", "plate_id", "plate_filename",
                     "acquisition_date", "reader_serial_number", "rp1_pmt_volts", "rp1_target")
  
  missing <- setdiff(required_cols, names(plate_id))
  if (length(missing) > 0) {
    cat("  ⚠ Missing columns in plate_id (filling with NA):", paste(missing, collapse = ", "), "\n")
    for (mc in missing) {
      plate_id[[mc]] <- NA_character_
    }
  }
  
  plate_id <- plate_id[, required_cols, drop = FALSE]
  cat("Final plate_id dimensions:", nrow(plate_id), "rows x", ncol(plate_id), "cols\n")
  cat("=====================================\n\n")

  plate_id
}


#' Build antigen data frame
build_antigen_df <- function(all_plates, study_accession, experiment_accession, project_id) {

  metadata_cols <- c("source_file", "Well", "Type", "Description",
                     "% Agg Beads", "Sampling Errors", "Acquisition Time", "plateid")
  antigen_names <- setdiff(names(all_plates), metadata_cols)

  tibble(
    project_id = rep(project_id, length(antigen_names)),
    study_name = rep(study_accession, length(antigen_names)),
    experiment_name = rep(experiment_accession, length(antigen_names)),
    antigen_label_on_plate = antigen_names,
    antigen_abbreviation = clean_antigen_label(antigen_names),
    standard_curve_max_concentration = 100000,
    l_asy_constraint_method = "default",
    l_asy_min_constraint = 0,
    l_asy_max_constraint = 0,
    antigen_family = NA_character_,
    antigen_name = NA_character_,
    virus_bacterial_strain = NA_character_,
    antigen_source = NA_character_,
    catalog_number = NA_character_
  )
}


#' Build plates_map data frame (REFACTORED)
#'
#' Uses parse_all_descriptions() for single-pass description parsing
#'
build_plates_map <- function(all_plates, plate_id, header_list, study_accession,
                             experiment_accession, n_wells, project_id,
                             description_status, delimiter, element_order, bcs_element_order,
                             feature_value = NA_character_) {

  # Use plateid (unique run identifier) for expansion, NOT plate_number
  # This ensures each unique plate run gets its own set of wells
  plateids <- unique(plate_id$plateid)

  cat("\n=== BUILD_PLATES_MAP ===\n")
  cat("  Unique plateids:", length(plateids), "\n")
  for (pid in plateids) {
    cat("    - ", pid, "\n", sep="")
  }

  well_list <- generate_well_list(n_wells)

  # Build header_df for merge
  header_df <- build_header_df_for_merge(header_list)

  # Merge all_plates with header info
  in_plates <- merge(all_plates, header_df, by = "source_file")
  in_plates$study_name <- study_accession
  in_plates$experiment_name <- experiment_accession

  # ==========================================================================
  # FIXED: Create plate-well map using plateid instead of plate_number
  # ==========================================================================
  plate_well_map <- expand_grid(
    study_name = study_accession,
    plateid = plateids,  # CHANGED: use plateid for unique run identity
    well = well_list
  )
  plate_well_map$experiment_name <- experiment_accession

  cat("  Expanded grid rows:", nrow(plate_well_map), "\n")
  cat("    (", length(plateids), " plateids × ", length(well_list), " wells)\n", sep="")

  # ==========================================================================
  # FIXED: Join plate_number and plate_id from plate_id table
  # ==========================================================================
  plate_id_lookup <- plate_id[, c("plateid", "plate_number", "plate_id")]
  plate_id_lookup <- unique(plate_id_lookup)

  plate_well_map <- dplyr::left_join(
    plate_well_map,
    plate_id_lookup,
    by = "plateid"
  )

  cat("  After joining plate_id_lookup:\n")
  cat("    - plate_number values:", paste(unique(plate_well_map$plate_number), collapse=", "), "\n")

  # ==========================================================================
  # Join with plate data using plateid as the key
  # ==========================================================================
  # First, ensure in_plates has plateid
  if (!"plateid" %in% names(in_plates)) {
    # plateid should come from all_plates (set by clean_plate_id in process_experiment_files)
    cat("  WARNING: plateid not found in in_plates\n")
  }

  plate_well_map <- dplyr::left_join(
    plate_well_map, in_plates,
    by = c("study_name" = "study_name", "experiment_name" = "experiment_name",
           "plateid" = "plateid", "well" = "Well")  # CHANGED: join on plateid, not plate_number
  )

  cat("  After joining in_plates: ", nrow(plate_well_map), " rows\n", sep="")

  # Set specimen_type
  plate_well_map$specimen_type <- case_when(
    is.na(plate_well_map$Type) ~ "",
    substr(plate_well_map$Type, 1, 1) == "X" ~ "X",
    substr(plate_well_map$Type, 1, 1) == "C" ~ "C",
    substr(plate_well_map$Type, 1, 1) == "B" ~ "B",
    substr(plate_well_map$Type, 1, 1) == "S" ~ "S",
    TRUE ~ ""
  )

  # REFACTORED: Parse all descriptions in a single pass
  use_defaults <- !description_status$has_content || !description_status$has_sufficient_elements

  if (use_defaults) {
    cat("\n╔══════════════════════════════════════════════════════════╗\n")
    cat("║  Parsing descriptions with defaults (single pass)        ║\n")
    cat("╚══════════════════════════════════════════════════════════╝\n")
  }

  parsed <- parse_all_descriptions(
    plate_data = plate_well_map,
    delimiter = delimiter,
    element_order = element_order,
    bcs_element_order = bcs_element_order,
    use_defaults = use_defaults
  )

  # Apply parsed values
  plate_well_map$subject_id <- parsed$subject_id
  plate_well_map$specimen_dilution_factor <- parsed$specimen_dilution_factor
  plate_well_map$timepoint_tissue_abbreviation = gsub("[[:space:][:cntrl:]]", "", parsed$timepoint_tissue_abbreviation)
  plate_well_map$specimen_source <- parsed$specimen_source

  if (use_defaults) {
    cat("  ✓ Parsed all descriptions with defaults applied\n")
    cat("╚══════════════════════════════════════════════════════════╝\n\n")
  }

  # Set biosample_id_barcode
  plate_well_map$biosample_id_barcode <- case_when(
    is.na(plate_well_map$Type) ~ "",
    substr(plate_well_map$Type, 1, 1) == "X" ~ substr(plate_well_map$Type, 2, nchar(plate_well_map$Type)),
    substr(plate_well_map$Type, 1, 1) == "C" ~ substr(plate_well_map$Type, 2, nchar(plate_well_map$Type)),
    substr(plate_well_map$Type, 1, 1) == "B" ~ "",
    substr(plate_well_map$Type, 1, 1) == "S" ~ substr(plate_well_map$Type, 2, nchar(plate_well_map$Type)),
    TRUE ~ ""
  )

  plate_well_map$feature <- feature_value
  plate_well_map$project_id <- project_id

  # REFACTORED: Apply defaults using centralized function
  if (use_defaults) {
    plate_well_map <- apply_sample_defaults(plate_well_map)
  }

  cat("=========================\n\n")

  plate_well_map
}

#' Build header_df for merge operation
#'
#' Note: plateid is NOT included here because it already exists in all_plates
#' (set via clean_plate_id(source_file) in process_experiment_files)
build_header_df_for_merge <- function(header_list) {

  cat("Creating header_df from header_list...\n")
  header_df_raw <- do.call(rbind, header_list)
  cat("  Available columns:", paste(names(header_df_raw), collapse = ", "), "\n")

  # Build with available columns - always include source_file
  # Note: plateid comes from all_plates (via clean_plate_id), not from header_df
  header_df_cols <- c("source_file")

  # plate_id should come from header if available
  if ("plate_id" %in% names(header_df_raw)) {
    header_df_cols <- c(header_df_cols, "plate_id")
  }

  # Handle plate column for the join key
  if ("plate" %in% names(header_df_raw)) {
    # Re-extract to ensure consistency
    extraction <- extract_plate_numbers_from_df(
      header_df_raw,
      priority_cols = c("plate", "source_file"),
      prefix = "plate_"
    )
    header_df_raw$plate <- extraction$plate_numbers
    header_df_cols <- c(header_df_cols, "plate")
  } else if ("plate_number" %in% names(header_df_raw)) {
    header_df_raw$plate <- header_df_raw$plate_number
    header_df_cols <- c(header_df_cols, "plate")
  } else {
    extraction <- extract_plate_numbers_from_df(
      header_df_raw,
      priority_cols = c("source_file"),
      prefix = "plate_"
    )
    header_df_raw$plate <- extraction$plate_numbers
    header_df_cols <- c(header_df_cols, "plate")
  }

  header_df <- header_df_raw[, unique(header_df_cols), drop = FALSE]
  cat("  Final header_df columns:", paste(names(header_df), collapse = ", "), "\n\n")

  header_df
}


#' Build subject_groups data frame
build_subject_groups <- function(plate_well_map, study_accession, description_status,
                                 delimiter, element_order) {

  # Pre-fill NA subject_ids
  sample_mask <- plate_well_map$specimen_type == "X"
  na_subject <- sample_mask & (is.na(plate_well_map$subject_id) | plate_well_map$subject_id == "")
  if (any(na_subject, na.rm = TRUE)) {
    plate_well_map$subject_id[na_subject] <- "1"
    cat("  → Pre-filled", sum(na_subject, na.rm = TRUE), "NA/empty subject_id values with '1'\n")
  }

  if (!any(sample_mask, na.rm = TRUE)) {
    cat("  → No sample rows found, creating default subject_groups\n")
    return(data.frame(
      study_name = study_accession,
      subject_id = "1",
      groupa = "Unknown",
      groupb = "Unknown",
      stringsAsFactors = FALSE
    ))
  }

  # Create subject_id_dat
  subject_id_dat <- data.frame(
    study_name = plate_well_map$study_name[sample_mask],
    subject_id = plate_well_map$subject_id[sample_mask],
    Description = plate_well_map$Description[sample_mask],
    stringsAsFactors = FALSE
  )
  rownames(subject_id_dat) <- NULL

  cat("  → Created subject_id_dat with", nrow(subject_id_dat), "sample rows\n")

  # Parse groups - using parse_all_descriptions would be overkill here
  # since we only need groupa/groupb
  use_defaults <- !description_status$has_content || !description_status$has_sufficient_elements

  if (use_defaults) {
    subject_id_dat$groupa <- sapply(subject_id_dat$Description, function(desc) {
      parsed <- parse_description_with_defaults(
        description = desc,
        delimiter = delimiter,
        element_order = element_order,
        optional_elements = c("SampleGroupA", "SampleGroupB")
      )
      parsed$groupa
    })

    subject_id_dat$groupb <- sapply(subject_id_dat$Description, function(desc) {
      parsed <- parse_description_with_defaults(
        description = desc,
        delimiter = delimiter,
        element_order = element_order,
        optional_elements = c("SampleGroupA", "SampleGroupB")
      )
      parsed$groupb
    })
  } else {
    xgroupa <- get_element_position("SampleGroupA", element_order)
    xgroupb <- get_element_position("SampleGroupB", element_order)

    subject_id_dat$groupa <- if (!is.na(xgroupa)) {
      str_split_i(subject_id_dat$Description, delimiter, xgroupa)
    } else ""

    subject_id_dat$groupb <- if (!is.na(xgroupb)) {
      str_split_i(subject_id_dat$Description, delimiter, xgroupb)
    } else ""
  }

  # Apply defaults to empty values
  subject_id_dat$subject_id[is.na(subject_id_dat$subject_id) | subject_id_dat$subject_id == ""] <- "1"
  subject_id_dat$groupa[is.na(subject_id_dat$groupa) | subject_id_dat$groupa == ""] <- "Unknown"
  subject_id_dat$groupb[is.na(subject_id_dat$groupb) | subject_id_dat$groupb == ""] <- "Unknown"

  # Get unique combinations
  subject_groups_cols <- subject_id_dat[, c("study_name", "subject_id", "groupa", "groupb"), drop = FALSE]
  rownames(subject_groups_cols) <- NULL
  subject_groups <- unique(subject_groups_cols)
  rownames(subject_groups) <- NULL

  cat("  → Created subject_groups with", nrow(subject_groups), "unique subjects\n")

  subject_groups
}


#' Build timepoint data frame
#'
#' Ensures uniqueness on natural key: study_name, timepoint_tissue_abbreviation,
#' tissue_type, tissue_subtype
build_timepoint_df <- function(plate_well_map, study_accession) {

  timepoints <- unique(plate_well_map[plate_well_map$specimen_type == "X", ]$timepoint_tissue_abbreviation)
  timepoints <- timepoints[!is.na(timepoints) & timepoints != ""]

  if (length(timepoints) == 0) {
    timepoints <- "T0"
    cat("  → No timepoints found, using default 'T0'\n")
  }

  timepoint <- expand_grid(
    study_name = study_accession,
    timepoint_tissue_abbreviation = gsub("[[:space:][:cntrl:]]", "", timepoints)
  )
  timepoint$tissue_type <- "blood"
  timepoint$tissue_subtype <- "serum"
  timepoint$description <- NA_character_
  timepoint$min_time_since_day_0 <- NA_character_
  timepoint$max_time_since_day_0 <- NA_character_
  timepoint$timepoint_unit <- NA_character_

  # Ensure uniqueness on natural key
  natural_key <- c("study_name", "timepoint_tissue_abbreviation", "tissue_type", "tissue_subtype")
  timepoint <- timepoint %>%
    dplyr::distinct(across(all_of(natural_key)), .keep_all = TRUE)

  cat("  → Created timepoint with", nrow(timepoint), "unique entries\n")

  timepoint
}


#' Calculate nominal sample dilution
calculate_nominal_sample_dilution <- function(plate_well_map, project_id,
                                              study_accession, experiment_accession) {

  natural_key <- c("project_id", "study_name", "experiment_name", "plateid")

  plate_well_map %>%
    dplyr::filter(specimen_type == "X") %>%
    dplyr::group_by(across(all_of(natural_key))) %>%
    dplyr::summarise(
      nominal_sample_dilution = paste(unique(specimen_dilution_factor), collapse = "|"),
      .groups = "drop"
    ) %>%
    dplyr::mutate(nominal_sample_dilution = as.character(nominal_sample_dilution))
}


#' Reorder plate_id columns
reorder_plate_id_columns <- function(plate_id) {
  leader_names <- c("project_id", "study_name", "experiment_name", "plate_number", "nominal_sample_dilution")
  other_names <- names(plate_id)[!(names(plate_id) %in% leader_names)]
  plate_id[, c(leader_names, other_names)]
}


#' Reorder plates_map columns
reorder_plates_map_columns <- function(plate_well_map) {
  plate_well_map[, c("project_id", "study_name", "plate_number", "nominal_sample_dilution",
                     "well", "specimen_type", "specimen_source", "specimen_dilution_factor",
                     "experiment_name", "feature", "subject_id", "biosample_id_barcode",
                     "timepoint_tissue_abbreviation", "plateid")]
}


#' Write all workbook sheets
write_workbook_sheets <- function(wb, workbook, bold_style, italic_style) {

  # Sheet-specific example data
  # plate_id columns after reorder: project_id, study_name, experiment_name, plate_number,
  # nominal_sample_dilution, number_of_wells, plateid, plate_id, plate_filename,
  # acquisition_date, reader_serial_number, rp1_pmt_volts, rp1_target
  examples <- list(
    plate_id = list(
      rows = list(c("prj_123456", "Vietnam342", "IgG_total", "plate_1", "2000|4000",
                    96, "plate_1", "plaque_1_IgG_15092025",
                    "C:/Users/d78039e/GeiselMed/vaccineVietnam342/IgG_total/plaque_1_IgG_15092025.xlsx",
                    "2025-09-15", "LX12345678", 450, 1000)),
      data_start = 3
    ),
    subject_groups = list(
      rows = list(c("Vietnam342", "P58732", "TdaP", "wP")),
      data_start = 3
    ),
    timepoint = list(
      rows = list(
        c("Vietnam342", "T0", "blood", "serum", "initial_visit", 1, 1, "day"),
        c("Vietnam342", "T1", "blood", "serum", "maternal_serum_at_birth", 3, 6, "month"),
        c("Vietnam342", "C1", "blood", "cord", "cord blood", 3, 6, "month")
      ),
      data_start = 5
    ),
    antigen_list = list(
      rows = list(c("Vietnam342", "IgG_total", "FHA (27)", "FHA", 100000, "unconstrained", 1, 10,
                    "Pertussis", "Filamentous hemagglutinin adhesin ", "Bordetella pertussis strain Tohama I",
                    "SinoBiological", "FHA123456789")),
      data_start = 3,
      extra_header = list(row = 1, col = 7, text = "user defined constraint")
    ),
    plates_map = list(
      rows = list(c("prj_123456", "Vietnam342", "plate_1", "2000|4000", "A1", "X", "sample", 2000,
                    "IgG_total", "IgG", "P58732", "B563429", "T0", "plate_1", "plaque_1_IgG_15092025")),
      data_start = 3
    )
  )

  for (nm in names(workbook)) {
    addWorksheet(wb, nm)

    if (nm %in% names(examples)) {
      ex <- examples[[nm]]

      # Write "Example:" label
      writeData(wb, nm, "Example:", startRow = 1, startCol = 1)

      # Write extra header if present
      if (!is.null(ex$extra_header)) {
        writeData(wb, nm, ex$extra_header$text, startRow = ex$extra_header$row, startCol = ex$extra_header$col)
      }

      # Write example rows
      for (i in seq_along(ex$rows)) {
        writeData(wb, nm, t(ex$rows[[i]]), startRow = 1 + i, startCol = 1, colNames = FALSE)
      }

      # Write real data
      writeData(wb, nm, workbook[[nm]], startRow = ex$data_start, startCol = 1)

      # Apply styles
      addStyle(wb, nm, style = bold_style, rows = ex$data_start,
               cols = 1:ncol(workbook[[nm]]), gridExpand = TRUE)
      addStyle(wb, nm, style = italic_style, rows = 1:(ex$data_start - 1),
               cols = 1:ncol(workbook[[nm]]), gridExpand = TRUE)

    } else if (nm == "cell_valid") {
      writeData(wb, nm, workbook[[nm]], startRow = 1, startCol = 1)
      addStyle(wb, nm, style = bold_style, rows = 1, cols = 1:ncol(workbook[[nm]]), gridExpand = TRUE)
    } else {
      writeData(wb, nm, workbook[[nm]])
      addStyle(wb, nm, style = bold_style, rows = 1, cols = 1:ncol(workbook[[nm]]), gridExpand = TRUE)
    }
  }
}


#' Add validation README sheet
add_validation_readme <- function(wb, plate_well_map, subject_groups, timepoint,
                                  description_status, study_accession, experiment_accession) {

  validation_issues <- list()
  validation_warnings <- list()
  needs_readme <- FALSE

  # Check 1: Description content/sufficiency
  if (!description_status$has_content) {
    needs_readme <- TRUE
    validation_issues <- c(validation_issues, list(
      "DESCRIPTION FIELD BLANK:",
      "  All Description fields in your plate data are blank.",
      "  Default values have been applied to ALL sample wells."
    ))
  } else if (!description_status$has_sufficient_elements) {
    needs_readme <- TRUE
    validation_issues <- c(validation_issues, list(
      "DESCRIPTION FIELD INSUFFICIENT:",
      paste0("  Description field has only ", description_status$min_elements_found,
             " elements (expected ", description_status$required_elements, ")."),
      "  Some fields may have incorrect or default values."
    ))
  }

  # Check 2: Specimen dilution factor
  sample_rows <- plate_well_map[plate_well_map$specimen_type == "X", ]
  if (nrow(sample_rows) > 0) {
    dilution_values <- sample_rows$specimen_dilution_factor
    na_dilutions <- sum(is.na(dilution_values), na.rm = TRUE)
    default_dilutions <- sum(dilution_values == 1, na.rm = TRUE)
    total_samples <- nrow(sample_rows)

    if (na_dilutions > 0) {
      needs_readme <- TRUE
      validation_warnings <- c(validation_warnings, list(
        "SPECIMEN DILUTION FACTOR - MISSING VALUES:",
        paste0("  ", na_dilutions, " of ", total_samples, " sample wells have NA/missing dilution factors."),
        "  These have been set to default value of 1."
      ))
    }

    if (default_dilutions == total_samples && total_samples > 0 &&
        (!description_status$has_content || !description_status$has_sufficient_elements)) {
      validation_warnings <- c(validation_warnings, list(
        "SPECIMEN DILUTION FACTOR - ALL DEFAULTS:",
        paste0("  All ", total_samples, " sample wells have dilution factor = 1 (default)."),
        "  This may indicate missing data."
      ))
    }
  }

  # Check 3: Timepoint linkage
  plates_map_timepoints <- unique(plate_well_map[plate_well_map$specimen_type == "X",
                                                  "timepoint_tissue_abbreviation"])
  plates_map_timepoints <- plates_map_timepoints[!is.na(plates_map_timepoints) & plates_map_timepoints != ""]

  timepoint_sheet_values <- unique(timepoint$timepoint_tissue_abbreviation)
  timepoint_sheet_values <- timepoint_sheet_values[!is.na(timepoint_sheet_values) & timepoint_sheet_values != ""]

  in_plates_not_timepoint <- setdiff(plates_map_timepoints, timepoint_sheet_values)
  in_timepoint_not_plates <- setdiff(timepoint_sheet_values, plates_map_timepoints)

  if (length(in_plates_not_timepoint) > 0) {
    needs_readme <- TRUE
    validation_issues <- c(validation_issues, list(
      "TIMEPOINT MISMATCH - MISSING FROM TIMEPOINT SHEET:",
      paste0("  The following timepoints are in PLATES_MAP but not in TIMEPOINT sheet:"),
      paste0("    • ", paste(in_plates_not_timepoint, collapse = ", "))
    ))
  }

  if (length(in_timepoint_not_plates) > 0) {
    needs_readme <- TRUE
    validation_warnings <- c(validation_warnings, list(
      "TIMEPOINT MISMATCH - UNUSED IN PLATES_MAP:",
      paste0("  The following timepoints are in TIMEPOINT sheet but not used in PLATES_MAP:"),
      paste0("    • ", paste(in_timepoint_not_plates, collapse = ", "))
    ))
  }

  # Check 4: Subject ID validation
  if (nrow(sample_rows) > 0) {
    subject_ids <- sample_rows$subject_id
    default_subject_count <- sum(subject_ids == "1", na.rm = TRUE)
    na_subject_count <- sum(is.na(subject_ids) | subject_ids == "", na.rm = TRUE)

    if (na_subject_count > 0) {
      needs_readme <- TRUE
      validation_warnings <- c(validation_warnings, list(
        "SUBJECT ID - MISSING VALUES:",
        paste0("  ", na_subject_count, " sample wells have missing subject IDs.")
      ))
    }

    if (default_subject_count == nrow(sample_rows) && nrow(sample_rows) > 1 &&
        (!description_status$has_content || !description_status$has_sufficient_elements)) {
      validation_warnings <- c(validation_warnings, list(
        "SUBJECT ID - ALL DEFAULTS:",
        paste0("  All ", nrow(sample_rows), " sample wells have subject_id = '1' (default).")
      ))
    }
  }

  # Check 5: Group validation
  if (nrow(subject_groups) > 0) {
    unknown_groupa <- sum(subject_groups$groupa == "Unknown", na.rm = TRUE)
    unknown_groupb <- sum(subject_groups$groupb == "Unknown", na.rm = TRUE)

    if ((unknown_groupa > 0 || unknown_groupb > 0) &&
        (!description_status$has_content || !description_status$has_sufficient_elements)) {
      validation_warnings <- c(validation_warnings, list(
        "GROUP ASSIGNMENTS - DEFAULTS APPLIED:",
        paste0("  ", unknown_groupa, " subjects have groupa = 'Unknown'"),
        paste0("  ", unknown_groupb, " subjects have groupb = 'Unknown'")
      ))
    }
  }

  # Build README if needed
  if (needs_readme || length(validation_issues) > 0 || length(validation_warnings) > 0) {

    readme_lines <- c(
      "╔══════════════════════════════════════════════════════════════════════════════╗",
      "║  IMPORTANT: LAYOUT TEMPLATE VALIDATION REPORT                               ║",
      "╚══════════════════════════════════════════════════════════════════════════════╝",
      "",
      paste0("Generated: ", Sys.time()),
      paste0("Study: ", study_accession),
      paste0("Experiment: ", experiment_accession),
      ""
    )

    if (length(validation_issues) > 0) {
      readme_lines <- c(readme_lines,
                        "═══════════════════════════════════════════════════════════════════════════════",
                        "⚠️  CRITICAL ISSUES REQUIRING ATTENTION:",
                        "═══════════════════════════════════════════════════════════════════════════════",
                        "", unlist(validation_issues), "")
    }

    if (length(validation_warnings) > 0) {
      readme_lines <- c(readme_lines,
                        "═══════════════════════════════════════════════════════════════════════════════",
                        "⚡ WARNINGS:",
                        "═══════════════════════════════════════════════════════════════════════════════",
                        "", unlist(validation_warnings), "")
    }

    if (!description_status$has_content || !description_status$has_sufficient_elements) {
      readme_lines <- c(readme_lines,
                        "═══════════════════════════════════════════════════════════════════════════════",
                        "DEFAULT VALUES APPLIED:",
                        "═══════════════════════════════════════════════════════════════════════════════",
                        "",
                        "The following default values were used where data was missing:",
                        "  • Subject ID: '1'",
                        "  • Sample Dilution Factor: 1",
                        "  • Timepoint: 'T0'",
                        "  • Groups (groupa, groupb): 'Unknown'",
                        "")
    }

    readme_lines <- c(readme_lines,
                      "═══════════════════════════════════════════════════════════════════════════════",
                      "REQUIRED ACTIONS BEFORE UPLOADING:",
                      "═══════════════════════════════════════════════════════════════════════════════",
                      "",
                      "1. PLATES_MAP sheet: Verify subject_id, specimen_dilution_factor, timepoint",
                      "2. SUBJECT_GROUPS sheet: Verify subject_id matches PLATES_MAP, update groups",
                      "3. TIMEPOINT sheet: Ensure all timepoints used in PLATES_MAP are listed",
                      "",
                      "═══════════════════════════════════════════════════════════════════════════════")

    readme_data <- data.frame(Notes = readme_lines, stringsAsFactors = FALSE)

    addWorksheet(wb, "README_IMPORTANT")
    writeData(wb, "README_IMPORTANT", readme_data, startRow = 1, startCol = 1, colNames = FALSE)

    # Apply styles
    warning_style <- createStyle(fontColour = "#8B0000", textDecoration = "bold")
    addStyle(wb, "README_IMPORTANT", style = warning_style, rows = 1:3, cols = 1, gridExpand = TRUE)

    section_style <- createStyle(fontColour = "#000080", textDecoration = "bold")
    section_rows <- which(grepl("^═══", readme_lines))
    if (length(section_rows) > 0) {
      addStyle(wb, "README_IMPORTANT", style = section_style, rows = section_rows, cols = 1, gridExpand = TRUE)
    }

    setColWidths(wb, "README_IMPORTANT", cols = 1, widths = 85)

    cat("\n╔══════════════════════════════════════════════════════════╗\n")
    cat("║  Added README_IMPORTANT sheet with validation report     ║\n")
    cat("╚══════════════════════════════════════════════════════════╝\n")
    cat("  Issues found:", length(validation_issues), "\n")
    cat("  Warnings found:", length(validation_warnings), "\n")
  }
}
