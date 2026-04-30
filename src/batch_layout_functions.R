reset_batch_reactives <- function() {
  cat("\n╔══════════════════════════════════════════════════════════╗\n")
  cat("║         RESETTING ALL BATCH REACTIVES                    ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n")

  # Log what's being cleared for debugging
  if (!is.null(batch_plate_data())) {
    cat("  ✓ Clearing batch_plate_data:", nrow(batch_plate_data()), "rows\n")
  }

  if (!is.null(batch_metadata())) {
    cat("  ✓ Clearing batch_metadata:", nrow(batch_metadata()), "rows\n")
  }

  if (length(bead_array_header_list()) > 0) {
    cat("  ✓ Clearing bead_array_header_list:", length(bead_array_header_list()), "items\n")
  }

  if (length(bead_array_plate_list()) > 0) {
    cat("  ✓ Clearing bead_array_plate_list:", length(bead_array_plate_list()), "items\n")
  }

  if (length(layout_template_sheets()) > 0) {
    cat("  ✓ Clearing layout_template_sheets:", length(layout_template_sheets()), "sheets\n")
  }

  # CRITICAL: Set all reactives to NULL/empty - this triggers UI invalidation
  batch_plate_data(NULL)
  batch_metadata(NULL)

  # IMPORTANT: Use list() not NULL for layout_template_sheets
  # because length(NULL) is 0 but length(list()) is also 0
  # and the reactive checks depend on this
  layout_template_sheets(list())
  inLayoutFile(NULL)
  avaliableLayoutSheets(NULL)

  # Bead array data
  bead_array_header_list(list())
  bead_array_plate_list(list())

  # DESCRIPTION STATUS
  description_status(list(
    has_content = TRUE,
    has_sufficient_elements = TRUE,
    min_elements_found = 0,
    required_elements = 3,
    checked = FALSE,
    message = NULL
  ))
  cat("  ✓ Reset description_status to default\n")

  # VALIDATION STATE
  batch_validation_state(list(
    is_validated = FALSE,
    is_uploaded = FALSE,
    validation_time = NULL,
    upload_time = NULL,
    metadata_result = NULL,
    bead_array_result = NULL,
    upload_in_progress = FALSE
  ))
  cat("  ✓ Reset batch_validation_state\n")

  # PROCESSING STATE - Reset hashes to allow new uploads
  batch_processing_state(list(
    is_processing = FALSE,
    last_layout_hash = NULL,
    last_validation_hash = NULL,
    last_upload_hash = NULL
  ))
  cat("  ✓ Reset batch_processing_state\n")

  # RELEASE PROCESSING LOCK
  layout_processing_lock(FALSE)
  cat("  ✓ Released layout_processing_lock\n")

  layout_upload_state(list(
    is_uploaded = FALSE,
    upload_time = NULL,
    current_study = NULL,
    current_experiment = NULL,
    processing = FALSE
  ))
  cat("  ✓ Reset layout_upload_state\n")

  cat("  ✓ All reactives reset to NULL/empty\n")
  cat("╚══════════════════════════════════════════════════════════╝\n\n")
}

#' Reset all batch-related UI elements
#'
#' This function resets file inputs, clears dynamic UI outputs, and ensures
#' the UI reflects a clean state when switching studies or experiments.
#'
#' @param session The Shiny session object
#' @param include_experiment_files Logical, whether to reset experiment file input
#' @param include_layout_file Logical, whether to reset layout file input
#'
reset_batch_ui <- function(session,
                           include_experiment_files = TRUE,
                           include_layout_file = TRUE) {

  cat("\n╔══════════════════════════════════════════════════════════╗\n")
  cat("║         RESETTING BATCH UI ELEMENTS                      ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n")

  # Reset blank handling dropdown
  tryCatch({
    updateSelectInput(
      session = session,
      inputId = "batch_blank",
      selected = "empty_well"
    )
    cat("  ✓ Reset batch_blank selector\n")
  }, error = function(e) {
    cat("  ⚠ Could not reset batch_blank:", conditionMessage(e), "\n")
  })

  # Reset wells selector
  tryCatch({
    shinyWidgets::updateRadioGroupButtons(
      session = session,
      inputId = "n_wells_on_plate",
      selected = 96
    )
    cat("  ✓ Reset n_wells_on_plate to 96\n")
  }, error = function(e) {
    cat("  ⚠ Could not reset n_wells_on_plate:", conditionMessage(e), "\n")
  })

  # Reset description delimiter
  tryCatch({
    shinyWidgets::updateRadioGroupButtons(
      session = session,
      inputId = "description_delimiter",
      selected = "_"
    )
    cat("  ✓ Reset description_delimiter to '_'\n")
  }, error = function(e) {
    cat("  ⚠ Could not reset description_delimiter:", conditionMessage(e), "\n")
  })

  # Reset optional elements checkboxes
  tryCatch({
    shinyWidgets::updateCheckboxGroupButtons(
      session = session,
      inputId = "optional_elements",
      selected = c("SampleGroupA", "SampleGroupB")
    )
    cat("  ✓ Reset optional_elements checkboxes\n")
  }, error = function(e) {
    cat("  ⚠ Could not reset optional_elements:", conditionMessage(e), "\n")
  })

  # Reset feature value text input
  tryCatch({
    updateTextInput(
      session = session,
      inputId = "feature_value",
      value = "Up to 15 chars"
    )
    cat("  ✓ Reset feature_value input\n")
  }, error = function(e) {
    cat("  ⚠ Could not reset feature_value:", conditionMessage(e), "\n")
  })

  cat("╚══════════════════════════════════════════════════════════╝\n\n")
}

# Output for Description content status (for conditional panels)
output$descriptionHasContent <- reactive({
  status <- description_status()
  return(isTRUE(status$has_content))
})
outputOptions(output, "descriptionHasContent", suspendWhenHidden = FALSE)

# Output for Description sufficient elements status (for conditional panels)
output$descriptionHasSufficientElements <- reactive({
  status <- description_status()
  return(isTRUE(status$has_sufficient_elements))
})
outputOptions(output, "descriptionHasSufficientElements", suspendWhenHidden = FALSE)

output$layoutIsUploaded <- reactive({
  state <- layout_upload_state()
  return(isTRUE(state$is_uploaded))
})
outputOptions(output, "layoutIsUploaded", suspendWhenHidden = FALSE)

# Clear any leftover temp directories from previous sessions
onSessionStart <- function() {
  tmpdir <- file.path(tempdir(), "uploaded_batch")
  if (dir.exists(tmpdir)) {
    unlink(tmpdir, recursive = TRUE)
    cat("Cleared temp directory from previous session\n")
  }
}

#' Clean and standardize plate_id from file path
#'
#' This function processes a file path to create a standardized plate_id by:
#'
#' a. Replacing all whitespace with single periods (consecutive whitespace characters
#'    are replaced with a single period)
#' b. Converting all characters to uppercase
#' c. Removing the file type suffix (extension)
#' d. Replacing all slashes (forward and back) with pipes
#' e. Replacing all instances of a space or period and other punctuation followed by
#'    another space or period with a single period
#' f. Removing the first drive letter or identifier (e.g., "C:", "/mnt", etc.)
#'
#' @param filepath A character string containing the file path to process
#'
#' @return A character string with the cleaned plate_id
#'
#' @examples
#' clean_plate_id("C:\\Users\\BIO-PLEX\\OneDrive - Université Libre de Bruxelles\\Documents - GRP_IMI_ERASME\\INCENTIVE STUDY\\Systems serology\\India\\FcgR\\FcgR2a plate1 rerun 091225.rbx")
#' # Returns: "USERS|BIO-PLEX|ONEDRIVE.-.UNIVERSITÉ.LIBRE.DE.BRUXELLES|DOCUMENTS.GRP_IMI_ERASME|INCENTIVE.STUDY|SYSTEMS.SEROLOGY|INDIA|FCGR|FCGR2A.PLATE1.RERUN.091225"
#'
#' @export
clean_plate_id <- function(filepath) {
  # Handle NULL or empty input
  if (is.null(filepath) || length(filepath) == 0) {
    return(NA_character_)
  }

  # Handle NA
  if (length(filepath) == 1 && is.na(filepath)) {
    return(NA_character_)
  }

  # Handle empty string
  if (length(filepath) == 1 && filepath == "") {
    return(NA_character_)
  }

  # Vectorize if needed
  if (length(filepath) > 1) {
    return(sapply(filepath, clean_plate_id, USE.NAMES = FALSE))
  }

  result <- filepath

  # Step c: Remove file extension (do this early to avoid issues with period handling)
  # Match common extensions including .xlsx, .xls, .rbx, .csv, .txt, etc.
  result <- sub("\\.[a-zA-Z0-9]+$", "", result)

  # Step d: Replace all slashes (forward and back) with pipes
  result <- gsub("[/\\\\]", "|", result)

  # Step f: Remove the first drive letter or identifier
  # Handle Windows drive letters (e.g., "C:|", "D:|")
  result <- sub("^[A-Za-z]:\\|", "", result)
  # Handle Unix-style root paths that might start with a pipe after slash conversion
  result <- sub("^\\|", "", result)
  # Handle network paths (e.g., "||server" from "\\server")
  result <- sub("^\\|+", "", result)
  # Handle common mount points (e.g., "mnt|", "home|", "var|")
  result <- sub("^(mnt|home|var|tmp|opt)\\|", "", result, ignore.case = TRUE)

  # Step b: Convert to uppercase
  result <- toupper(result)

  # Step a: Replace all whitespace (including consecutive) with single periods
  # This handles spaces, tabs, newlines, etc.
  result <- gsub("[[:space:]]+", ".", result)

  # Step e: Replace punctuation sequences with single period
  # Keep: letters (including accented), numbers, underscores, hyphens, pipes, and single periods
  # Replace: other punctuation that isn't followed by a hyphen (to preserve " - " becoming ".-.")

  # First pass: Convert punctuation (except | _ - .) to periods
  result <- gsub("([^|_.a-zA-Z0-9À-ÿ-])", ".", result)

  # This loop handles nested patterns like ".-.-."
  prev_result <- ""
  while (prev_result != result) {
    prev_result <- result
    # Replace period + any punctuation (except |) + period with single period
    result <- gsub("\\.[^|a-zA-Z0-9À-ÿ]+\\.", ".", result)
    # Replace multiple consecutive periods with single period
    result <- gsub("\\.{2,}", ".", result)
  }

  # Remove leading/trailing periods and pipes
  result <- gsub("^[|.]+|[|.]+$", "", result)

  # Remove periods immediately before or after pipes
  result <- gsub("\\|\\.", "|", result)
  result <- gsub("\\.\\|", "|", result)

  # Remove any empty pipe segments (consecutive pipes)
  result <- gsub("\\|{2,}", "|", result)

  return(result)
}

add_clean_plate_id_to_identifiers <- function(identifiers_df) {

  if ("source_file" %in% names(identifiers_df) && !all(is.na(identifiers_df$source_file))) {
    identifiers_df$plateid <- clean_plate_id(identifiers_df$source_file)
  }

  if ("file_name" %in% names(identifiers_df) && !all(is.na(identifiers_df$file_name))) {
    identifiers_df$plate_id <- clean_plate_id(identifiers_df$file_name)
  }

  return(identifiers_df)
}

#' Apply clean_plate_id to a data frame column
#'
#' @param df Data frame
#' @param source_col Column containing the file path (default: "source_file")
#' @param target_col Column to store cleaned plate_id (default: "plate_id")
#' @return Data frame with cleaned plate_id column
#' @export
add_clean_plate_id <- function(df, source_col = "source_file", target_col = "plate_id") {
  if (!source_col %in% names(df)) {
    warning(paste0("Column '", source_col, "' not found. Returning unchanged."))
    return(df)
  }
  df[[target_col]] <- clean_plate_id(df[[source_col]])
  return(df)
}


#' Apply clean_plate_id using file_name column
#' @export
add_clean_plate_id_from_filename <- function(df) {
  add_clean_plate_id(df, source_col = "file_name", target_col = "plate_id")
}


#' Apply clean_plate_id using plate_filename column
#' @export
add_clean_plate_id_from_plate_filename <- function(df) {
  add_clean_plate_id(df, source_col = "plate_filename", target_col = "plate_id")
}


#' Test the clean_plate_id function
#'
#' Run tests to verify the function works correctly
#'
#' @return Invisible NULL, prints test results
#' @examples
#' test_clean_plate_id()
#'
test_clean_plate_id <- function() {
  cat("Testing clean_plate_id function:\n")
  cat(strrep("=", 80), "\n\n")

  # Test cases
  tests <- list(
    list(
      input = "C:\\Users\\BIO-PLEX\\OneDrive - Université Libre de Bruxelles\\Documents - GRP_IMI_ERASME\\INCENTIVE STUDY\\Systems serology\\India\\FcgR\\FcgR2a plate1 rerun 091225.rbx",
      expected = "USERS|BIO-PLEX|ONEDRIVE.-.UNIVERSITÉ.LIBRE.DE.BRUXELLES|DOCUMENTS.GRP_IMI_ERASME|INCENTIVE.STUDY|SYSTEMS.SEROLOGY|INDIA|FCGR|FCGR2A.PLATE1.RERUN.091225",
      description = "Full Windows path with spaces, special chars, and extension"
    ),
    list(
      input = "/home/user/data/plate 1.xlsx",
      expected = "USER|DATA|PLATE.1",
      description = "Unix path with space in filename"
    ),
    list(
      input = "D:/Projects/Study  Data/file   name.csv",
      expected = "PROJECTS|STUDY.DATA|FILE.NAME",
      description = "Multiple consecutive spaces"
    ),
    list(
      input = "\\\\server\\share\\folder\\file.txt",
      expected = "SERVER|SHARE|FOLDER|FILE",
      description = "Network UNC path"
    ),
    list(
      input = "/mnt/data/experiment/plate.xlsx",
      expected = "DATA|EXPERIMENT|PLATE",
      description = "Unix path with /mnt prefix"
    ),
    list(
      input = "plate1.xlsx",
      expected = "PLATE1",
      description = "Simple filename only"
    ),
    list(
      input = NA,
      expected = NA_character_,
      description = "NA input"
    ),
    list(
      input = "",
      expected = NA_character_,
      description = "Empty string"
    ),
    list(
      input = "C:\\Users\\Name\\File - Copy (2).xlsx",
      expected = "USERS|NAME|FILE.-.COPY.2",
      description = "Filename with parentheses and dash"
    )
  )

  passed <- 0
  failed <- 0

  for (test in tests) {
    result <- clean_plate_id(test$input)

    # Handle NA comparison properly
    if (is.na(test$expected) && is.na(result)) {
      match <- TRUE
    } else if (is.na(test$expected) || is.na(result)) {
      match <- FALSE
    } else {
      match <- result == test$expected
    }

    status <- if (match) "✓ PASS" else "✗ FAIL"

    if (match) {
      passed <- passed + 1
    } else {
      failed <- failed + 1
    }

    cat(sprintf("%s: %s\n", status, test$description))
    cat(sprintf("  Input:    %s\n", ifelse(is.na(test$input), "NA", test$input)))
    cat(sprintf("  Expected: %s\n", ifelse(is.na(test$expected), "NA", test$expected)))
    cat(sprintf("  Got:      %s\n", ifelse(is.na(result), "NA", result)))
    cat("\n")
  }

  cat(strrep("=", 80), "\n")
  cat(sprintf("Results: %d passed, %d failed\n", passed, failed))

  invisible(NULL)
}


#' Check Description field content and element count
#'
#' This function examines the Description column in plate data to determine
#' if it has content and whether it contains sufficient elements for parsing
#' into subject ID, timeperiod, dilution factor, and optional group fields.
#'
#' @param plate_data Data frame containing plate data with a Description column
#' @param delimiter Character used to separate elements in Description (default: "_")
#' @param required_elements Minimum number of elements required (default: 3 for PatientID, TimePeriod, DilutionFactor)
#'
#' @return List with:
#'   - has_content: TRUE if at least one sample has a non-empty Description
#'   - has_sufficient_elements: TRUE if all non-empty Descriptions have enough elements
#'   - min_elements_found: The minimum number of elements found in any Description
#'   - required_elements: The number of elements required
#'   - message: Warning message if applicable, NULL otherwise
#'
#' @examples
#' # Check a plate with proper descriptions
#' check_description_elements(plate_data, delimiter = "_", required_elements = 3)
#'
check_description_elements <- function(plate_data, delimiter = "_", required_elements = 3) {
  # Filter to only sample rows (Type starts with X)
  sample_rows <- plate_data[grepl("^X", plate_data$Type), ]

  if (nrow(sample_rows) == 0) {
    return(list(
      has_content = TRUE,  # No samples, so not applicable
      has_sufficient_elements = TRUE,
      min_elements_found = NA,
      required_elements = required_elements,
      message = NULL
    ))
  }

  # Check Description column
  descriptions <- sample_rows$Description

  # Check if Description column exists
  if (!"Description" %in% names(plate_data)) {
    return(list(
      has_content = FALSE,
      has_sufficient_elements = FALSE,
      min_elements_found = 0,
      required_elements = required_elements,
      message = "Description column is missing from the plate data."
    ))
  }

  # Check for blank/empty descriptions
  empty_descriptions <- is.na(descriptions) | trimws(descriptions) == "" | descriptions == "NA"

  if (all(empty_descriptions)) {
    return(list(
      has_content = FALSE,
      has_sufficient_elements = FALSE,
      min_elements_found = 0,
      required_elements = required_elements,
      message = "All Description fields are blank. The layout template will need to be manually updated with subject IDs, groups, timepoints, and dilution factors after creation."
    ))
  }

  # Count elements in non-empty descriptions
  non_empty_descriptions <- descriptions[!empty_descriptions]

  # Escape delimiter for regex if needed
  delimiter_pattern <- switch(delimiter,
                              "|" = "\\|",
                              "." = "\\.",
                              delimiter
  )

  element_counts <- sapply(non_empty_descriptions, function(desc) {
    # Split by delimiter and count non-empty elements
    elements <- strsplit(as.character(desc), delimiter_pattern)[[1]]
    elements <- elements[trimws(elements) != ""]
    length(elements)
  })

  min_elements <- min(element_counts, na.rm = TRUE)

  # Check if sufficient elements exist
  has_sufficient <- min_elements >= required_elements

  message <- NULL
  if (!has_sufficient) {
    message <- sprintf(
      "Description field has insufficient elements (found %d, need %d). Some fields will use default values. The layout template may need manual updates for subject IDs, groups, timepoints, and/or dilution factors.",
      min_elements,
      required_elements
    )
  }

  return(list(
    has_content = TRUE,
    has_sufficient_elements = has_sufficient,
    min_elements_found = min_elements,
    required_elements = required_elements,
    message = message
  ))
}


#' Apply default values to plates_map when Description is insufficient
#'
#' This function fills in default values for plates_map columns when the
#' Description field is blank or has insufficient elements to parse.
#'
#' @param plates_map Data frame representing the plates map
#' @param description_status List from check_description_elements function
#'
#' @return Modified plates_map with default values applied where needed
#'
#' @details
#' Default values applied:
#'   - subject_id: "1"
#'   - specimen_dilution_factor: 1
#'   - timeperiod_tissue_abbreviation: "T0"
#'   - groupa: "Unknown"
#'   - groupb: "Unknown"
#'
apply_default_values_to_plates_map <- function(plates_map, description_status) {
  if (is.null(plates_map) || nrow(plates_map) == 0) {
    return(plates_map)
  }

  # Only apply defaults for sample rows (specimen_type == "X")
  sample_mask <- plates_map$specimen_type == "X"

  if (!description_status$has_content || !description_status$has_sufficient_elements) {
    cat("╔══════════════════════════════════════════════════════════╗\n")
    cat("║  Applying default values due to insufficient Description ║\n")
    cat("╚══════════════════════════════════════════════════════════╝\n")

    # Set default subject_id to "1"
    if ("subject_id" %in% names(plates_map)) {
      na_subject <- sample_mask & (is.na(plates_map$subject_id) | plates_map$subject_id == "")
      if (any(na_subject)) {
        plates_map$subject_id[na_subject] <- "1"
        cat("  → Set", sum(na_subject), "blank subject_id values to '1'\n")
      }
    }

    # Set default specimen_dilution_factor to 1
    if ("specimen_dilution_factor" %in% names(plates_map)) {
      na_dilution <- sample_mask & (is.na(plates_map$specimen_dilution_factor) | plates_map$specimen_dilution_factor == "")
      if (any(na_dilution)) {
        plates_map$specimen_dilution_factor[na_dilution] <- 1
        cat("  → Set", sum(na_dilution), "blank specimen_dilution_factor values to 1\n")
      }
    }

    # Set default timeperiod_tissue_abbreviation to "T0"
    if ("timeperiod_tissue_abbreviation" %in% names(plates_map)) {
      na_timeperiod <- sample_mask & (is.na(plates_map$timeperiod_tissue_abbreviation) | plates_map$timeperiod_tissue_abbreviation == "")
      if (any(na_timeperiod)) {
        plates_map$timeperiod_tissue_abbreviation[na_timeperiod] <- "T0"
        cat("  → Set", sum(na_timeperiod), "blank timeperiod_tissue_abbreviation values to 'T0'\n")
      }
    }

    # Set default groupa if exists
    if ("groupa" %in% names(plates_map)) {
      na_groupa <- sample_mask & (is.na(plates_map$groupa) | plates_map$groupa == "")
      if (any(na_groupa)) {
        plates_map$groupa[na_groupa] <- "Unknown"
        cat("  → Set", sum(na_groupa), "blank groupa values to 'Unknown'\n")
      }
    }

    # Set default groupb if exists
    if ("groupb" %in% names(plates_map)) {
      na_groupb <- sample_mask & (is.na(plates_map$groupb) | plates_map$groupb == "")
      if (any(na_groupb)) {
        plates_map$groupb[na_groupb] <- "Unknown"
        cat("  → Set", sum(na_groupb), "blank groupb values to 'Unknown'\n")
      }
    }

    cat("╚══════════════════════════════════════════════════════════╝\n")
  }

  return(plates_map)
}


#' Parse Description field with fallback to defaults
#'
#' This function attempts to parse the Description field into components.
#' If parsing fails or the field is empty, it returns default values.
#'
#' @param description Character string containing the Description value
#' @param delimiter Character used to separate elements
#' @param element_order Character vector specifying the order of elements
#' @param optional_elements Character vector of optional elements to include
#'
#' @return Named list with parsed values or defaults:
#'   - subject_id: Subject/patient ID (default: "1")
#'   - groupa: First group assignment (default: "Unknown")
#'   - groupb: Second group assignment (default: "Unknown")
#'   - timeperiod: Timepoint/tissue (default: "T0")
#'   - dilution_factor: Specimen dilution (default: 1)
#'
parse_description_with_defaults <- function(description,
                                            delimiter = "_",
                                            element_order = c("PatientID", "SampleGroupA", "SampleGroupB", "TimePeriod", "DilutionFactor"),
                                            optional_elements = c("SampleGroupA", "SampleGroupB")) {

  # Default values
  defaults <- list(
    subject_id = "1",
    groupa = "Unknown",
    groupb = "Unknown",
    timeperiod = "T0",
    dilution_factor = 1
  )

  # Check if description is empty
  if (is.na(description) || trimws(description) == "" || description == "NA") {
    return(defaults)
  }

  # Escape delimiter for regex
  delimiter_pattern <- switch(delimiter,
                              "|" = "\\|",
                              "." = "\\.",
                              delimiter
  )

  # Split description
  elements <- strsplit(as.character(description), delimiter_pattern)[[1]]
  elements <- trimws(elements)
  elements <- elements[elements != ""]

  # If no elements, return defaults
  if (length(elements) == 0) {
    return(defaults)
  }

  # Build result list
  result <- defaults

  # Map elements to fields based on order
  # Filter element_order to only include elements we're expecting
  active_elements <- element_order[element_order %in% c("PatientID", "SampleGroupA", "SampleGroupB", "TimePeriod", "DilutionFactor")]

  # If optional elements are not in the list, remove them from active_elements
  active_elements <- active_elements[active_elements %in% c("PatientID", "TimePeriod", "DilutionFactor") |
                                       active_elements %in% optional_elements]

  for (i in seq_along(active_elements)) {
    if (i <= length(elements)) {
      field <- active_elements[i]
      value <- elements[i]

      if (field == "PatientID") {
        result$subject_id <- value
      } else if (field == "SampleGroupA") {
        result$groupa <- value
      } else if (field == "SampleGroupB") {
        result$groupb <- value
      } else if (field == "TimePeriod") {
        result$timeperiod <- value
      } else if (field == "DilutionFactor") {
        # Try to parse as numeric, fall back to default if not
        numeric_val <- suppressWarnings(as.numeric(value))
        if (!is.na(numeric_val)) {
          result$dilution_factor <- numeric_val
        }
      }
    }
  }

  return(result)
}


#' Validate Description elements for batch upload
#'
#' Validates that the Description field in the batch plate data has
#' sufficient information for creating the layout template.
#'
#' @param batch_plate_data Combined plate data from all uploaded files
#' @param delimiter Delimiter used in Description field
#' @param required_elements Minimum number of elements required
#'
#' @return List with validation result and messages
#'
validate_batch_description <- function(batch_plate_data, delimiter = "_", required_elements = 3) {

  result <- list(
    valid = TRUE,
    has_warnings = FALSE,
    messages = c()
  )

  # Get unique source files
  source_files <- unique(batch_plate_data$source_file)

  for (file in source_files) {
    file_data <- batch_plate_data[batch_plate_data$source_file == file, ]

    desc_check <- check_description_elements(
      plate_data = file_data,
      delimiter = delimiter,
      required_elements = required_elements
    )

    if (!desc_check$has_content) {
      result$has_warnings <- TRUE
      result$messages <- c(result$messages,
                           sprintf("File '%s': Description field is blank. Default values will be used.", file)
      )
    } else if (!desc_check$has_sufficient_elements) {
      result$has_warnings <- TRUE
      result$messages <- c(result$messages,
                           sprintf("File '%s': Description has only %d elements (need %d). Some defaults will be applied.",
                                   file, desc_check$min_elements_found, required_elements)
      )
    }
  }

  return(result)
}


# read in the folder where the files are and the common word
# Usually the common word is 'plate'
read_bind_xlsx <- function (folder, x){
  files <- list.files(folder, pattern=x, full.names=TRUE)
  files <- files[!grepl("layout", basename(files), ignore.case = TRUE)]
  names(files) <- basename(files)

  files %>%
    #set_names(~basename(.))%>%
    map_dfr(read_excel,sheet=1, skip=7, col_types="text",.id="source_file")
}

#' Process a single plate file and extract header and plate data
#' @param file_path Path to the Excel file
#' @param file_name Original filename
#' @return List with header, plate data, and metadata
process_single_plate_file <- function(file_path, file_name) {
  # Read full sheet to find where "Well" row is
  bead_array_plate <- openxlsx::read.xlsx(file_path, colNames = FALSE, sheet = 1)
  well_row <- which(bead_array_plate[, 1] == "Well")

  if (length(well_row) == 0) {
    warning(paste("No 'Well' row found in:", file_name))
    return(NULL)
  }

  # Header block (rows before "Well")
  header_info <- openxlsx::read.xlsx(
    file_path,
    rows = 1:(well_row - 1),
    colNames = FALSE,
    sheet = 1
  )
  header_info <- parse_metadata_df(header_info)

  # Plate block (data starting from "Well" row)
  plate_data <- openxlsx::read.xlsx(
    file_path,
    startRow = well_row,
    sheet = 1,
    colNames = TRUE
  )
  names(plate_data) <- gsub("\\.", " ", names(plate_data))

  # Remove completely empty rows
  plate_data <- plate_data[!apply(
    plate_data, 1,
    function(x) all(is.na(x) | trimws(x) == "" | trimws(x) == "NA")
  ), ]

  # Add source file tracking
  plate_data$source_file <- file_name
  header_info$source_file <- file_name

  list(
    header = header_info,
    plate = plate_data,
    file_name = file_name
  )
}

#' Extract and assign plate numbers to header list
#' @param header_list List of header data frames
#' @return Modified header list with plate numbers assigned
assign_plate_numbers <- function(header_list) {
  used_plate_nums <- c()

  for (i in seq_along(header_list)) {
    file_name <- names(header_list)[i]
    plate_num <- NA_character_
    extraction_source <- ""

    # PRIORITY 1: plateid from header metadata
    if ("plateid" %in% names(header_list[[i]]) &&
        !is.na(header_list[[i]]$plateid[1]) &&
        header_list[[i]]$plateid[1] != "") {
      plate_num <- extract_plate_number(header_list[[i]]$plateid[1])
      if (!is.na(plate_num)) {
        extraction_source <- paste0("plateid ('", header_list[[i]]$plateid[1], "')")
      }
    }

    # PRIORITY 2: file_name from header metadata
    if (is.na(plate_num) && "file_name" %in% names(header_list[[i]])) {
      plate_num <- extract_plate_number(header_list[[i]]$file_name[1])
      if (!is.na(plate_num)) {
        extraction_source <- paste0("file_name ('", header_list[[i]]$file_name[1], "')")
      }
    }

    # PRIORITY 3: uploaded filename (source_file)
    if (is.na(plate_num)) {
      plate_num <- extract_plate_number(file_name)
      if (!is.na(plate_num)) {
        extraction_source <- paste0("source_file ('", file_name, "')")
      }
    }

    # FALLBACK: sequential number
    if (is.na(plate_num)) {
      fallback_num <- 1
      while (paste0("plate_", fallback_num) %in% used_plate_nums) {
        fallback_num <- fallback_num + 1
      }
      plate_num <- paste0("plate_", fallback_num)
      extraction_source <- "fallback (sequential)"
      cat("  ⚠️  ", file_name, "→ Could not extract plate number, using fallback:", plate_num, "\n")
    }

    # Handle duplicates
    if (plate_num %in% used_plate_nums) {
      original_plate_num <- plate_num
      base_num <- as.numeric(gsub("plate_", "", plate_num))
      while (paste0("plate_", base_num) %in% used_plate_nums) {
        base_num <- base_num + 1
      }
      plate_num <- paste0("plate_", base_num)
      cat("  ⚠️  Duplicate plate number '", original_plate_num,
          "' reassigned to: ", plate_num, "\n", sep = "")
    }

    used_plate_nums <- c(used_plate_nums, plate_num)
    header_list[[i]]$plate <- plate_num

    if (is.na(header_list[[i]]$plateid[1]) || header_list[[i]]$plateid[1] == "") {
      header_list[[i]]$plateid <- plate_num
    }

    cat("  ✓ ", file_name, " → ", plate_num, " (from ", extraction_source, ")\n", sep = "")
  }

  header_list
}

#' Check description field status
#' @param plate_data Combined plate data
#' @param delimiter Delimiter for parsing description
#' @param required_elements Minimum required elements
#' @return List with status information
check_and_report_description <- function(plate_data, delimiter = "_", required_elements = 3) {
  desc_check <- check_description_elements(
    plate_data = plate_data,
    delimiter = delimiter,
    required_elements = required_elements
  )

  list(
    has_content = desc_check$has_content,
    has_sufficient_elements = desc_check$has_sufficient_elements,
    min_elements_found = desc_check$min_elements_found,
    required_elements = desc_check$required_elements,
    checked = TRUE,
    message = desc_check$message
  )
}

#' Process all uploaded experiment files (single pass)
#' @param upload_df Data frame from fileInput with datapath and name columns
#' @return List with combined_plates, header_list, and plate_list
process_experiment_files <- function(upload_df) {
  cat("Processing", nrow(upload_df), "files...\n")

  # Single pass: read all files
  results <- lapply(seq_len(nrow(upload_df)), function(i) {
    file_path <- upload_df$datapath[i]
    file_name <- upload_df$name[i]
    cat("  Processing:", file_name, "\n")

    result <- process_single_plate_file(file_path, file_name)
    if (!is.null(result)) {
      cat("    → Columns:", ncol(result$plate), ", Rows:", nrow(result$plate), "\n")
    }
    result
  })

  # Remove any NULL results (failed files)
  results <- Filter(Negate(is.null), results)

  if (length(results) == 0) {
    stop("No valid plate files could be processed")
  }

  # Extract headers and plates
  names(results) <- sapply(results, `[[`, "file_name")
  header_list <- lapply(results, `[[`, "header")
  plate_list <- lapply(results, `[[`, "plate")

  # Assign plate numbers
  cat("\nExtracting plate numbers...\n")
  header_list <- assign_plate_numbers(header_list)

  # Combine all plates into single data frame
  cat("\nCombining plate data...\n")
  combined_plates <- dplyr::bind_rows(plate_list)

  # Add plateid to combined data if needed
  if ("source_file" %in% names(combined_plates)) {
    combined_plates$plateid <- clean_plate_id(combined_plates$source_file)
  }

  cat("  → Combined dimensions:", nrow(combined_plates), "rows x", ncol(combined_plates), "cols\n")
  cat("  → Source files:", paste(unique(combined_plates$source_file), collapse = ", "), "\n")

  list(
    combined_plates = combined_plates,
    header_list = header_list,
    plate_list = plate_list
  )
}

generate_well_list <- function(plate_size) {
  # Matches logic from plate_plot()
  dims <- switch(
    as.character(plate_size),
    "6" = c(2, 3),
    "12" = c(3, 4),
    "24" = c(4, 6),
    "48" = c(6, 8),
    "96" = c(8, 12),
    "384" = c(16, 24),
    "1536" = c(32, 48)
  )

  n_rows <- dims[1]
  n_cols <- dims[2]

  # Row letters support (A–Z, AA–AF to match function
  MORELETTERS <- c(LETTERS, "AA", "AB", "AC", "AD", "AE", "AF")

  rows <- MORELETTERS[seq_len(n_rows)]
  cols <- seq_len(n_cols)

  # Create well labels e.g., A1, B1...
  expand.grid(Row = rows, Col = cols) |>
    dplyr::mutate(
      well = paste0(Row, Col)
    ) |>
    dplyr::pull(well)
}

add_source_column <- function(df_list) {
  # Check if input is a list
  if (!is.list(df_list)) {
    stop("Input must be a list of data frames")
  }

  # Check if list has names
  if (is.null(names(df_list))) {
    stop("List must have names")
  }

  # Get the names of list elements
  list_names <- names(df_list)

  # Add source_file column to each data frame
  result <- mapply(function(df, name) {
    if (!is.data.frame(df)) {
      warning(paste("Element", name, "is not a data frame. Skipping."))
      return(df)
    }
    df$source_file <- name
    return(df)
  }, df_list, list_names, SIMPLIFY = FALSE)

  return(result)
}

get_element_position <- function(element,
                                 order_vector,
                                 ignore_case = FALSE,
                                 default = NA_integer_) {

  # Validate inputs
  if (missing(order_vector)) {
    stop("'order_vector' is required")
  }

  if (!is.character(element)) {
    stop("'element' must be a character vector")
  }

  if (!is.character(order_vector)) {
    stop("'order_vector' must be a character vector")
  }

  if (length(order_vector) == 0) {
    stop("'order_vector' cannot be empty")
  }

  # Find positions
  if (ignore_case) {
    position <- match(tolower(element), tolower(order_vector))
  } else {
    position <- match(element, order_vector)
  }

  # Replace NA with default value
  position[is.na(position)] <- default

  # Name the results if multiple elements
  if (length(element) > 1) {
    names(position) <- element
  }

  return(position)
}

#' Clean Antigen Label String
#'
#' Takes a raw antigen_label_on_plate string that may be irregularly formatted
#' and returns a well-behaved string. Removes content in parentheses, replaces
#' slashes with underscores, replaces whitespace with nothing, and removes
#' punctuation except for '.' and '_'.
#'
#' @param antigen_label A character string (or vector) containing the raw antigen label
#' @return A cleaned character string (or vector) with only alphanumeric characters, '.', and '_'
#'
#' @examples
#' clean_antigen_label("  Washington_32 (18) ")
#' # Returns: "Washington_32"
#'
#' clean_antigen_label("A/Hong Kong_NA(123)")
#' # Returns: "A_Hong.Kong_NA"
#'
#' clean_antigen_label(c("Test/Label (1)", "Another_One(99)"))
#' # Returns: c("Test_Label", "Another_One")

clean_antigen_label <- function(antigen_label) {
  if (!is.character(antigen_label)) {
    stop("Input must be a character string or vector")
  }

  cleaned <- antigen_label

  # Step 1: Remove content within parentheses (including the parentheses)
  cleaned <- gsub("\\s*\\([^)]*\\)", "", cleaned)

  # Step 2: Trim leading and trailing whitespace
  cleaned <- trimws(cleaned)

  # Step 3: Replace forward slashes with underscores
  cleaned <- gsub("/", "_", cleaned)

  # Step 4: Replace internal whitespace with dots
  cleaned <- gsub("\\s+", ".", cleaned)

  # Step 5: Remove any remaining punctuation except '.' and '_'
  # This regex matches any punctuation that is NOT a dot or underscore
  cleaned <- gsub("[^[:alnum:]._]", "", cleaned)

  # Step 6: Clean up any multiple consecutive dots or underscores
  cleaned <- gsub("\\.{2,}", ".", cleaned)
  cleaned <- gsub("_{2,}", "_", cleaned)

  # Step 7: Remove leading/trailing dots or underscores
  cleaned <- gsub("^[._]+|[._]+$", "", cleaned)

  return(cleaned)
}




#' Check if Excel sheet names match expected names
#'
#' @param file_path Path to the Excel file
#' @param expected_sheets Vector of expected sheet names (optional, uses default if not provided)
#' @param exact_match If TRUE, requires exact match; if FALSE, checks if expected sheets exist
#'
#' @return A list with validation status and details
check_sheet_names <- function(file_path,
                              expected_sheets = c("plate_id", "subject_groups", "timepoint",
                                                  "antigen_list", "plates_map", "cell_valid", "assay_response_long"),
                              exact_match = TRUE) {

  # Get actual sheet names from file
  actual_sheets <- readxl::excel_sheets(file_path)

  # Check for missing sheets
  missing_sheets <- setdiff(expected_sheets, actual_sheets)

  # Check for extra sheets
  extra_sheets <- setdiff(actual_sheets, expected_sheets)

  # Determine if valid

  if (exact_match) {
    is_valid <- length(missing_sheets) == 0 && length(extra_sheets) == 0
  } else {
    is_valid <- length(missing_sheets) == 0
  }

  # Build result
  result <- list(
    valid = is_valid,
    actual_sheets = actual_sheets,
    expected_sheets = expected_sheets,
    missing_sheets = missing_sheets,
    extra_sheets = extra_sheets,
    message = NULL
  )

  # Create informative message
  if (is_valid) {
    result$message <- "All sheet names are valid."
  } else {
    msg_parts <- c()
    if (length(missing_sheets) > 0) {
      msg_parts <- c(msg_parts, paste("Missing sheets:", paste(missing_sheets, collapse = ", ")))
    }
    if (exact_match && length(extra_sheets) > 0) {
      msg_parts <- c(msg_parts, paste("Unexpected sheets:", paste(extra_sheets, collapse = ", ")))
    }
    result$message <- paste(msg_parts, collapse = ". ")
  }

  return(result)
}

#' Remove trailing rows that are all NA
#'
#' @param df A data frame
#'
#' @return Data frame with trailing all-NA rows removed
remove_trailing_na_rows <- function(df) {


  if (nrow(df) == 0) return(df)

  # Find rows where all values are NA
  all_na_rows <- apply(df, 1, function(row) all(is.na(row)))

  # Find the last non-all-NA row
  if (all(all_na_rows)) {
    # All rows are NA, return empty data frame with same structure
    return(df[0, , drop = FALSE])
  }

  # Find trailing NA rows
  last_valid_row <- max(which(!all_na_rows))

  # Return data frame up to last valid row
  return(df[1:last_valid_row, , drop = FALSE])
}


#' Identify rows to skip at the top of each sheet
#'
#' @param file_path Path to the Excel file
#' @param max_rows_to_search Maximum number of rows to search for header (default: 50)
#'
#' @return A named list with skip values for each sheet
identify_rows_to_skip <- function(file_path, max_rows_to_search = 50) {


  # Define sheet types and their header identifiers.
  # assay_response_long is marked optional: it may be absent in older
  # bead-array templates and is allowed to be silently skipped.
  sheet_config <- list(
    plate_id            = "study_name",
    subject_groups      = "study_name",
    timepoint           = "study_name",
    antigen_list        = "study_name",
    plates_map          = "study_name",
    cell_valid          = "l_asy_constraint_method",
    assay_response_long = "project_id"   # flowjo + xPONENT pathway
  )
  optional_sheets <- c("assay_response_long")

  # Get actual sheets in file
  actual_sheets <- readxl::excel_sheets(file_path)

  # Initialize results
  results <- list()

  for (sheet_name in names(sheet_config)) {

    # Check if sheet exists in file
    if (!sheet_name %in% actual_sheets) {
      results[[sheet_name]] <- list(
        skip = NA,
        found = FALSE,
        message = paste("Sheet", sheet_name, "not found in file")
      )
      next
    }

    # Get the header identifier for this sheet
    header_identifier <- sheet_config[[sheet_name]]

    # Read first column of the sheet (limited rows) to find header
    preview_data <- readxl::read_excel(
      file_path,
      sheet = sheet_name,
      n_max = max_rows_to_search,
      col_names = FALSE,
      col_types = "text"
    )

    # Search for header identifier in all columns
    header_row <- NA

    for (i in seq_len(nrow(preview_data))) {
      row_values <- as.character(preview_data[i, ])
      # Check if any cell in this row contains the header identifier
      if (any(grepl(header_identifier, row_values, ignore.case = TRUE))) {
        header_row <- i
        break
      }
    }

    # Calculate skip value (rows before header row)
    if (!is.na(header_row)) {
      skip_value <- header_row - 1
      results[[sheet_name]] <- list(
        skip = skip_value,
        header_row = header_row,
        found = TRUE,
        header_identifier = header_identifier,
        message = paste("Header found at row", header_row, "- skip", skip_value, "rows")
      )
    } else {
      results[[sheet_name]] <- list(
        skip = NA,
        header_row = NA,
        found = FALSE,
        header_identifier = header_identifier,
        message = paste("Header identifier '", header_identifier, "' not found in first",
                        max_rows_to_search, "rows")
      )
    }
  }

  return(results)
}


#' Get skip values as a simple named vector
#'
#' @param file_path Path to the Excel file
#' @param max_rows_to_search Maximum number of rows to search for header
#'
#' @return Named numeric vector of skip values
get_skip_values <- function(file_path, max_rows_to_search = 50) {


  full_results <- identify_rows_to_skip(file_path, max_rows_to_search)

  skip_values <- sapply(full_results, function(x) x$skip)

  return(skip_values)
}

#' Validate that all headers were found
#'
#' @param skip_results Results from identify_rows_to_skip()
#'
#' @return List with validation status and details
validate_skip_results <- function(skip_results,
                                   optional_sheets = c("assay_response_long")) {

  # Only required sheets must be found; optional ones can be missing without
  # causing a validation failure (backwards-compatible with older templates).
  required_results <- skip_results[!names(skip_results) %in% optional_sheets]
  all_found        <- all(sapply(required_results, function(x) x$found))
  failed_sheets    <- names(required_results)[!sapply(required_results, function(x) x$found)]
  messages         <- sapply(skip_results, function(x) x$message)

  list(
    valid = all_found,
    failed_sheets = failed_sheets,
    messages = messages,
    summary = if (all_found) {
      "All required header rows successfully identified."
    } else {
      paste("Failed to find headers in:", paste(failed_sheets, collapse = ", "))
    }
  )
}

#' Read a single sheet with skip value and remove trailing NA rows
#'
#' @param file_path Path to the Excel file
#' @param sheet_name Name of the sheet to read
#' @param skip Number of rows to skip
#'
#' @return Cleaned data frame
read_and_clean_sheet <- function(file_path, sheet_name, skip) {


  df <- readxl::read_excel(
    file_path,
    sheet = sheet_name,
    skip = skip
  )

  # Remove trailing NA rows
  df <- remove_trailing_na_rows(df)

  return(df)
}

#' Read all sheets with correct skip values and clean trailing NA rows
#'
#' @param file_path Path to the Excel file
#' @param max_rows_to_search Maximum rows to search for header
#'
#' @return Named list of cleaned data frames
read_all_sheets <- function(file_path, max_rows_to_search = 50) {


  skip_values <- get_skip_values(file_path, max_rows_to_search)

  # Sheets with NA skip values were not found in the file (e.g. assay_response_long
  # in older bead-array templates).  Hard-fail only for required missing sheets;
  # silently drop optional ones rather than stopping with an error.
  optional_sheets <- c("assay_response_long")
  required_na     <- names(skip_values)[is.na(skip_values) &
                                         !names(skip_values) %in% optional_sheets]
  if (length(required_na) > 0) {
    stop("Could not find header row for required sheets: ",
         paste(required_na, collapse = ", "))
  }

  # Keep only sheets where the header was successfully located
  found_skip_values <- skip_values[!is.na(skip_values)]

  # Read and clean each located sheet
  sheet_data <- lapply(names(found_skip_values), function(sheet_name) {
    read_and_clean_sheet(file_path, sheet_name, found_skip_values[[sheet_name]])
  })

  names(sheet_data) <- names(found_skip_values)

  return(sheet_data)
}

#' Full validation and import pipeline
#'
#' @param file_path Path to the Excel file
#' @param expected_sheets Vector of expected sheet names
#' @param max_rows_to_search Maximum rows to search for header
#'
#' @return List with status, messages, and data
import_layout_file <- function(file_path,
                               expected_sheets = c("plate_id", "subject_groups", "timepoint",
                                                   "antigen_list", "plates_map", "cell_valid"),
                               max_rows_to_search = 50) {


  result <- list(
    success = FALSE,
    messages = c(),
    data = NULL,
    row_counts = NULL
  )

  # Step 1: Check sheet names exist
  actual_sheets <- readxl::excel_sheets(file_path)
  missing_sheets <- setdiff(expected_sheets, actual_sheets)

  if (length(missing_sheets) > 0) {
    result$messages <- c(result$messages,
                         paste("Missing sheets:", paste(missing_sheets, collapse = ", ")))
    return(result)
  }

  # Step 2: Identify skip values
  skip_results <- identify_rows_to_skip(file_path, max_rows_to_search)
  validation <- validate_skip_results(skip_results)

  if (!validation$valid) {
    result$messages <- c(result$messages, validation$summary)
    return(result)
  }

  # Step 3: Read and clean all sheets
  tryCatch({
    sheet_data <- read_all_sheets(file_path, max_rows_to_search)

    result$success <- TRUE
    result$data <- sheet_data
    result$row_counts <- sapply(sheet_data, nrow)
    result$messages <- c(result$messages, "All sheets imported successfully.")

  }, error = function(e) {
    result$messages <- c(result$messages, paste("Error reading sheets:", e$message))
  })

  return(result)
}

#' Extract plate identification columns from plate metadata list
#'
#' @param plate_metadata_list A named list of tibbles/data frames, each containing
#'   plate metadata with columns: plate, plateid, plate_id, source_file
#'
#' @return A data frame with one row per plate and columns: plate, plateid, plate_id, source_file
#'
extract_plate_identifiers <- function(plate_metadata_list) {

  # Define columns to extract
  cols_to_extract <- c("plate", "plateid", "plate_id", "source_file", "reader_serial_number",
                       "file_name", "rp1_pmt_volts", "rp1_target", "acquisition_date")

  # Extract and combine
  result <- do.call(rbind, lapply(names(plate_metadata_list), function(nm) {
    df <- plate_metadata_list[[nm]]

    # Check which columns exist
    available_cols <- intersect(cols_to_extract, names(df))
    missing_cols <- setdiff(cols_to_extract, names(df))

    # Extract available columns (take first row if multiple)
    extracted <- df[1, available_cols, drop = FALSE]

    # Add NA for any missing columns
    for (col in missing_cols) {
      extracted[[col]] <- NA_character_
    }

    # Ensure column order is consistent
    extracted[, cols_to_extract, drop = FALSE]
  }))

  # Convert to plain data.frame and reset row names
  result <- as.data.frame(result, stringsAsFactors = FALSE)
  rownames(result) <- NULL

  return(result)
}

#' Transpose batch header data frame
#'
#' @param batch_header A data frame with a source_file column to use as column names
#' @param id_column The column to use for naming the transposed columns (default: "source_file")
#'
#' @return A transposed data frame where original rows become columns named by source_file values
#'
transpose_batch_header <- function(batch_header, id_column = "source_file") {

  # Get the column names to use for new columns
  new_col_names <- batch_header[[id_column]]

  # Remove the id column from the data before transposing
  data_to_transpose <- batch_header[, names(batch_header) != id_column, drop = FALSE]

  # Transpose the data
  transposed <- as.data.frame(t(data_to_transpose), stringsAsFactors = FALSE)

  # Set column names to the source_file values
  names(transposed) <- new_col_names

  # The row names are now the original column names - convert to a column
  transposed$variable <- rownames(transposed)
  rownames(transposed) <- NULL

  # Reorder so variable is first
  transposed <- transposed[, c("variable", new_col_names)]

  return(transposed)
}

# Join sample dilutions for the plate to the header

# In batch_layout_functions.R - Replace the construct_batch_upload_metadata function

construct_batch_upload_metadata <- function(plates_map, plate_metadata_list, currentuser, workspace_id, project_id = userWorkSpaceID()) {

  cat("\n=== CONSTRUCT BATCH UPLOAD METADATA ===\n")
  cat("plates_map columns:", paste(names(plates_map), collapse=", "), "\n")
  cat("Number of plate metadata items:", length(plate_metadata_list), "\n")
  cat("Plate metadata names:", paste(names(plate_metadata_list), collapse=", "), "\n")

  # Check if plate_metadata_list items have 'plate' column
  first_item <- plate_metadata_list[[1]]
  cat("First metadata item columns:", paste(names(first_item), collapse=", "), "\n")
  print(plate_metadata_list)
  cat("\n")

  # FIX: Get sample dilution factors - use the MOST COMMON value per plate
  # or the minimum if there are multiple dilutions (for mixed dilution plates)
  sample_dilutions_raw <- plates_map[plates_map$specimen_type == "X" &
                                       !is.na(plates_map$plate_number),
                                     c("plate_number", "specimen_dilution_factor")]

  # Remove NA dilution factors before aggregation
  sample_dilutions_raw <- sample_dilutions_raw[!is.na(sample_dilutions_raw$specimen_dilution_factor), ]

  # FIX: Use a custom aggregation that picks the most frequent dilution factor
  # If ties, use the minimum value
  sample_dilutions_by_plate <- do.call(rbind, lapply(
    split(sample_dilutions_raw, sample_dilutions_raw$plate_number),
    function(df) {
      if (nrow(df) == 0) return(NULL)

      # Get frequency table
      freq_table <- table(df$specimen_dilution_factor)

      # Get the most frequent value (or min if tied)
      max_freq <- max(freq_table)
      most_common <- as.numeric(names(freq_table)[freq_table == max_freq])
      representative_dilution <- min(most_common)  # Use min if there's a tie

      data.frame(
        plate_number = unique(df$plate_number),
        sample_dilution_factor = representative_dilution,
        stringsAsFactors = FALSE
      )
    }
  ))
  rownames(sample_dilutions_by_plate) <- NULL

  cat("Sample dilutions by plate:\n")
  print(sample_dilutions_by_plate)
  cat("\n")

  # FIX: Get experiment by plate - ensure TRUE uniqueness by plate_number
  # Remove 'feature' from distinct to prevent multiplication
  experiment_by_plate <- unique(
    plates_map[!is.na(plates_map$plate_number),
               c("study_name", "plate_number", "experiment_name"),
               drop = FALSE]
  )

  # CRITICAL FIX: Ensure only ONE row per plate_number
  experiment_by_plate <- experiment_by_plate[!duplicated(experiment_by_plate$plate_number), ]

  experiment_by_plate$workspace_id <- workspace_id
  experiment_by_plate$auth0_user <- currentuser

  cat("Experiment by plate (after dedup):\n")
  print(experiment_by_plate)
  cat("Number of rows:", nrow(experiment_by_plate), "\n")
  cat("\n")

  source_file_by_plate <- extract_plate_identifiers(plate_metadata_list)
  # Source_file_by_plate_v <<- source_file_by_plate
  source_file_by_plate <- add_clean_plate_id_to_identifiers(source_file_by_plate)
  source_file_by_plate$plate_number <- source_file_by_plate$plate

  cat("Source file by plate:\n")
  print(source_file_by_plate)
  cat("Number of rows:", nrow(source_file_by_plate), "\n")
  cat("\n")

  # First join
  plate_metadata_list_updated <- merge(
    experiment_by_plate,
    sample_dilutions_by_plate,
    by = "plate_number",
    all.x = TRUE
  )

  cat("After first join - rows:", nrow(plate_metadata_list_updated), "\n")

  # Check for unexpected row multiplication
  if (nrow(plate_metadata_list_updated) > nrow(experiment_by_plate)) {
    cat("⚠️  WARNING: Row multiplication detected after first join!\n")
    cat("   Expected:", nrow(experiment_by_plate), "Got:", nrow(plate_metadata_list_updated), "\n")
    # Deduplicate
    plate_metadata_list_updated <- plate_metadata_list_updated[
      !duplicated(plate_metadata_list_updated$plate_number), ]
    cat("   After dedup:", nrow(plate_metadata_list_updated), "\n")
  }

  # Second join
  plate_metadata_list_updated <- merge(
    plate_metadata_list_updated,
    source_file_by_plate,
    by = "plate_number",
    all.x = TRUE
  )

  cat("After second join - rows:", nrow(plate_metadata_list_updated), "\n")

  # Check for unexpected row multiplication again
  if (nrow(plate_metadata_list_updated) > length(plate_metadata_list)) {
    cat("⚠️  WARNING: Row multiplication detected after second join!\n")
    cat("   Expected:", length(plate_metadata_list), "Got:", nrow(plate_metadata_list_updated), "\n")
    # Deduplicate by plate_number (the true unique key)
    plate_metadata_list_updated <- plate_metadata_list_updated[
      !duplicated(plate_metadata_list_updated$plate_number), ]
    cat("   After dedup:", nrow(plate_metadata_list_updated), "\n")
  }

  cat("plate_metadata_list_updated:\n")
  print(plate_metadata_list_updated)
  cat("\n")

  # add project_id
  # plate_metadata_list_updated$project_id <- project_id
  # plates_map$project_id <- project_id

  # # string with all unique sample dilutions separated by a pipe
  # # Define the natural key
  # natural_key <- c("project_id", "study_name", "experiment_name", "plate_id")
  #
  # # Group, filter, and concatenate
  # result <- plates_map %>%
  #   dplyr::filter(specimen_type == "X") %>%
  #   dplyr::group_by(across(all_of(natural_key))) %>%
  #   dplyr::summarise(
  #     nominal_sample_dilution = paste(
  #       unique(specimen_dilution_factor),
  #       collapse = "|"
  #     ),
  #     .groups = "drop"
  #   ) %>%
  #   dplyr::mutate(
  #     # Ensure it's character type (varchar equivalent in R)
  #     nominal_sample_dilution = as.character(nominal_sample_dilution)
  #   )
  #
  # nsdx_result <<- result
  #
  # plate_metadata_list_updated <- plate_metadata_list_updated %>%
  #   left_join(result, by = natural_key)

  leadernames <- c("project_id","study_name","experiment_name","plate_number","nominal_sample_dilution")
  other_names <- names(plate_metadata_list_updated)[!(names(plate_metadata_list_updated) %in% leadernames)]

  plate_metadata_list_updated <- plate_metadata_list_updated[ , c(leadernames, other_names)]

  cat(paste("plate_metadata_list_updated:", names(plate_metadata_list_updated),"\n"))
  cat(paste("leader:", names(plate_metadata_list_updated),"\n"))
  cat(paste("other:", names(plate_metadata_list_updated),"\n"))

  # plate_well_map <- plate_well_map[ , c("study_name",	"plate_number",	"well",	"specimen_type",	"specimen_source",
  #                                       "specimen_dilution_factor",	"experiment_name",	"feature",	"subject_id",	"biosample_id_barcode",	"timepoint_tissue_abbreviation")]
  #
  #
  # plate_metadata_list_updated$nominal_sample_dilution <-paste(
  #   sort(unique(
  #     plates_map[
  #       plates_map$specimen_type == "X",
  #     ]$specimen_dilution_factor
  #   )),
  #   collapse = "|"
  # )
  #
  #
  # plate_metadata_list_updated_v <<- plate_metadata_list_updated

  cat("✓ Batch metadata construction complete\n")
  cat("=====================================\n\n")

  return(plate_metadata_list_updated)
}

plot_plate_layout <- function(plates_map, plate_id_data) {
  #plates_map_v <<- plates_map
  # Join in number_of_wells info
  plates_map_joined <- merge(
    plates_map,
    plate_id_data[, c("study_name", "experiment_name", "plate_number", "number_of_wells")],
    by = c("study_name", "experiment_name", "plate_number"),
    all.x = TRUE
  )
  plates_map_joined$specimen_type <- ifelse(is.na(plates_map_joined$specimen_type),"E",plates_map_joined$specimen_type)
  specimen_labels <- c(
    "S" = "Standard Curve",
    "C" = "Controls",
    "X" = "Samples",
    "B" = "Blanks",
    "E" = "Empty"
  )
  plates_map_joined$`Specimen Type` <- specimen_labels[plates_map_joined$specimen_type]

  # Initialize list for storing plots
  plate_plots <- list()
  specimen_palette <-  c("Blanks" = "#F3C300",
                         "Controls" = "#2B3D26",
                         "Standard Curve" = "#A1CAF1",
                         "Samples" = "#8DB600",
                         "Empty" = "white"
  )

  # Get unique combinations of study, experiment, and plate
  unique_combos <- unique(
    plates_map_joined[, c("study_name", "experiment_name", "plate_number")]
  )
  # remove trailing NA rows
  unique_combos <- unique_combos[rowSums(is.na(unique_combos)) != ncol(unique_combos), ]


  # Loop through each combination
  for (i in seq_len(nrow(unique_combos))) {
    study <- unique_combos$study_name[i]
    exp <- unique_combos$experiment_name[i]
    plate <- unique_combos$plate_number[i]

    plates_map_joined_filtered <- plates_map_joined[plates_map_joined$study_name == study & plates_map_joined$experiment_name == exp
                                                    & plates_map_joined$plate_number == plate,]
    plates_map_joined_filtered <- plates_map_joined_filtered[rowSums(is.na(plates_map_joined_filtered)) != ncol(plates_map_joined_filtered), ]

    n_wells <- unique(plates_map_joined_filtered$number_of_wells)

    plot_title <- paste0(
      "Study: ", study,
      " | Experiment: ", exp,
      " | Plate: ", plate,
      " (", n_wells, " wells)"
    )

    plate_layout <- plate_plot_plotly2(
      data = plates_map_joined_filtered,
      plate_size = n_wells,
      title = plot_title,
      colour = specimen_palette
    )

    list_name <- paste(study, exp, plate, sep = "_")
    plate_plots[[list_name]] <- plate_layout
  }

  #plate_plots <<- plate_plots

  return(plate_plots)
}

plate_plot_plotly2 <- function(data, plate_size = 96, title = NULL, colour = NULL) {
  #data_v <<- data
  cat("Plate")
  print(unique(data$plate_number))
  cat("Plate Size")
  print(plate_size)
  data <- data |>
    dplyr::mutate(
      row = stringr::str_extract(well, pattern = "[:upper:]+"),
      col = as.numeric(stringr::str_extract(well, pattern = "\\d+")),
      row_num = as.numeric(match(row, LETTERS))
    )

  # determine number of rows and columns
  dims <- switch(
    as.character(plate_size),
    "6" = c(2, 3),
    "12" = c(3, 4),
    "24" = c(4, 6),
    "48" = c(6, 8),
    "96" = c(8, 12),
    "384" = c(16, 24),
    "1536" = c(32, 48)
  )
  n_rows <- dims[1]
  n_cols <- dims[2]

  # make grid of all wells
  plate_layout_grid <- expand.grid(
    Row = LETTERS[1:n_rows],
    Column = 1:n_cols
  )
  plate_layout_grid$well <- paste(plate_layout_grid$Row, plate_layout_grid$Column, sep = "")

  # # find empty wells
  # empty_wells <- setdiff(plate_layout_grid$well, data$well)
  # empty_df <- plate_layout_grid |>
  #   dplyr::filter(well %in% empty_wells) |>
  #   dplyr::mutate(
  #     row_num = as.numeric(match(Row, LETTERS)),
  #     col = Column,
  #     `Specimen Type` = "Empty Well"
  #   )

  # flip y-axis to match lab plate layout
  data$row_num <- abs(data$row_num - (n_rows + 1))
  # empty_df$row_num <- abs(empty_df$row_num - (n_rows + 1))

  # Build plot
  p <- plot_ly() %>%
    add_trace(
      data = data,
      x = ~col,
      y = ~row_num,
      type = "scatter",
      mode = "markers",
      color = ~`Specimen Type`,
      colors = colour,
      text = ~paste(
        "Well:", well,
        "<br>Specimen Type:", `Specimen Type`,
        ifelse(`Specimen Type` == "Samples",
               paste0("<br>Patient ID: ", subject_id,
                      "<br>Timepoint: ", timepoint_tissue_abbreviation),
               ""),
        "<br>Biosample Barcode:", biosample_id_barcode,
        "<br>Specimen Dilution Factor:", specimen_dilution_factor,
        "<br>Feature:", feature
      ),
      hoverinfo = "text",
      marker = list(size = 20, line = list(width = 1, color = "black"), symbol = "circle")
    ) %>%
    # add_trace(
    #   data = empty_df,
    #   x = ~col,
    #   y = ~row_num,
    #   name = "Empty Well",
    #   type = "scatter",
    #   mode = "markers",
    #   marker = list(size = 20, color = "white", line = list(width = 1, color = "grey")),
    #   hoverinfo = "text",
    #   text = ~paste("Well:", well, "<br>Empty")
    # ) %>%
    layout(
      title = title,
      legend = list(title = list(text = "Specimen Type")),
      yaxis = list(
        title = "",
        tickvals = seq(1, n_rows),
        ticktext = rev(LETTERS[1:n_rows]),
        showgrid = FALSE
      ),
      xaxis = list(
        title = "",
        tickvals = seq(1, n_cols),
        showgrid = FALSE
      ),
      showlegend = TRUE,
      shapes = list(
        list(
          type = "rect",
          x0 = 0.5, x1 = n_cols + 0.5,
          y0 = 0.5, y1 = n_rows + 0.5,
          line = list(color = "black", width = 1),
          fillcolor = "rgba(0,0,0,0)"
        )
      )
    )

  return(p)
}

# Helper function to combine plate metadata from batch load
combine_plate_metadata <- function(head_list) {
  plate_metadata <- do.call(rbind, lapply(names(head_list), function(nm) {
    df <- head_list[[nm]]
    # Ensure sample_dilution_factor column exists
    if (!"sample_dilution_factor" %in% names(df)) {
      df$sample_dilution_factor <- 1
      cat("  ⚠ Added default sample_dilution_factor for:", nm, "\n")
    }
    df
  }))

  return(plate_metadata)
}

## this function merges layout info and raw file info for uploading samples
prepare_batch_bead_assay_samples <- function(sample_plate_map, combined_plate_data, batch_metadata, antigen_import_list, subject_map) {
  cat("combined_plate_data:", paste(names(combined_plate_data), collapse = ", "), "\n")
  cat("nrows in combined_plate_data:", nrow(combined_plate_data), "\n")
  cat("sample_plate_map columns:", paste(names(sample_plate_map), collapse = ", "), "\n")
  cat("nrows in sample_plate_map:", nrow(sample_plate_map), "\n")
  cat("subject_map:", paste(names(subject_map), collapse = ", "), "\n")
  cat("nrows in subject_map:", nrow(subject_map), "\n")
  cat("batch_metadata:", paste(names(batch_metadata), collapse = ", "), "\n")
  cat("nrows in batch_metadata:", nrow(batch_metadata), "\n")

  sample_joined <- merge(sample_plate_map, subject_map,
                         by = c("study_name", "subject_id"),
                         all.x = TRUE
  )
  sample_joined$agroup <-ifelse(is.na(sample_joined$groupb),
                                sample_joined$groupa,
                                paste(sample_joined$groupa, sample_joined$groupb, sep = "_"))
  # sample_j <- merge(
  #   sample_joined,
  #   combined_plate_data[, !(names(combined_plate_data) %in% c("Type", "Description"))],
  #   by.x = c("experiment_name", "plate_number", "well"),
  #   by.y = c("experiment_name", "plate", "Well")
  # )
  sample_j <- merge(
    sample_joined,
    combined_plate_data[, !(names(combined_plate_data) %in% c("Type", "Description"))],
    by.x = c("plate_number", "well"),
    by.y = c("plate", "Well")
  )

  sample_long <- sample_j %>%
    pivot_longer(
      cols = matches("\\([0-9]+\\)"), # all columns containing bead count
      names_to = "antigen",
      values_to = "mfi_bead"
    ) %>%
    mutate(
      # Remove the bead number from column name to get clean antigen name
      antigen = gsub("\\.", " ", antigen),#str_remove(antigen, "\\.\\([0-9]+\\)"),
      # Split MFI and bead count from the value column
      antibody_mfi = as.numeric(str_extract(mfi_bead, "^[0-9.]+")),
      antibody_n = as.numeric(str_extract(mfi_bead, "(?<=\\()[0-9]+(?=\\))"))
    ) %>%
    select(-mfi_bead) %>%
    relocate(antigen, antibody_mfi, antibody_n, .after = biosample_id_barcode)


  # join in the antigen abbreviation
  sample_long2 <- merge(sample_long, antigen_import_list[, c("study_name", "experiment_name", "antigen_label_on_plate", "antigen_abbreviation")],
                        by.x = c("study_name", "experiment_name", "antigen"),
                        by.y =  c("study_name", "experiment_name", "antigen_label_on_plate"),
                        all.x = T)

  # join in the plate_id from the metadata
  sample_long2 <- merge(sample_long2,
                        batch_metadata[, c("source_file", "plate_id", "plateid", "plate", "project_id", "nominal_sample_dilution")],
                        by = "source_file",
                        all.x = T)

  sample_long2 <- sample_long2[, !names(sample_long2) %in% c("antigen")]
  names(sample_long2)[names(sample_long2) == "antigen_abbreviation"] <- "antigen"
  names(sample_long2)[names(sample_long2) == "study_name"] <- "study_accession"
  names(sample_long2)[names(sample_long2) == "experiment_name"] <- "experiment_accession"
  names(sample_long2)[names(sample_long2) == "specimen_type"] <- "stype"
  names(sample_long2)[names(sample_long2) == "specimen_dilution_factor"] <- "dilution"
  names(sample_long2)[names(sample_long2) == "% Agg Beads"] <- "pctaggbeads"
  names(sample_long2)[names(sample_long2) == "Sampling Errors"] <- "samplingerrors"
  names(sample_long2)[names(sample_long2) == "timepoint_tissue_abbreviation"] <- "timeperiod"
  names(sample_long2)[names(sample_long2) == "subject_id"] <- "patientid"
  names(sample_long2)[names(sample_long2) == "biosample_id_barcode"] <- "sampleid"

  samples_to_upload <- sample_long2[, c("study_accession", "experiment_accession", "plate_id", "timeperiod", "patientid", "well",
                                        "stype", "sampleid", "agroup", "dilution", "pctaggbeads", "samplingerrors", "antigen",
                                        "antibody_mfi", "antibody_n", "feature", "nominal_sample_dilution", "project_id", "plateid", "plate")]
  # Before returning, add debug:
  cat("\n=== DEBUG: Inside prepare_batch_bead_assay_samples ===\n")
  cat("Result columns:", paste(names(samples_to_upload), collapse = ", "), "\n")

  if ("sampleid" %in% names(samples_to_upload)) {
    cat("sampleid present, null count:", sum(is.na(samples_to_upload$sampleid)), "\n")
  } else {
    cat("WARNING: sampleid column NOT FOUND\n")
    cat("Looking for similar columns:",
        paste(grep("sample", names(samples_to_upload), value = TRUE, ignore.case = TRUE), collapse = ", "), "\n")
  }

  # Check subject_map for the source of sampleid
  cat("\nsubject_map columns:", paste(names(subject_map), collapse = ", "), "\n")
  if ("sampleid" %in% names(subject_map)) {
    cat("sampleid in subject_map, null count:", sum(is.na(subject_map$sampleid)), "\n")
  }

  return(samples_to_upload)
}

prepare_batch_bead_assay_standards <- function(standard_plate_map, combined_plate_data, antigen_import_list, batch_metadata) {
  # standard_j <- merge(
  #   standard_plate_map,
  #   combined_plate_data[, !(names(combined_plate_data) %in% c("Type", "Description"))],
  #   by.x = c("experiment_name", "plate_number", "well"),
  #   by.y = c("experiment_name", "plate", "Well")
  # )
  standard_j <- merge(
    standard_plate_map,
    combined_plate_data[, !(names(combined_plate_data) %in% c("Type", "Description"))],
    by.x = c("plate_number", "well"),
    by.y = c("plate", "Well")
  )

  standard_long <- standard_j %>%
    pivot_longer(
      cols = matches("\\([0-9]+\\)"), # all columns containing bead count
      names_to = "antigen",
      values_to = "mfi_bead"
    ) %>%
    mutate(
      # Remove the bead number from column name to get clean antigen name
      antigen = gsub("\\.", " ", antigen),#str_remove(antigen, "\\.\\([0-9]+\\)"),
      # Split MFI and bead count from the value column
      antibody_mfi = as.numeric(str_extract(mfi_bead, "^[0-9.]+")),
      antibody_n = as.numeric(str_extract(mfi_bead, "(?<=\\()[0-9]+(?=\\))"))
    ) %>%
    select(-mfi_bead) %>%
    relocate(antigen, antibody_mfi, antibody_n, .after = biosample_id_barcode)

  standard_long2 <- merge(standard_long, antigen_import_list[, c("study_name", "experiment_name", "antigen_label_on_plate", "antigen_abbreviation")],
                          by.x = c("study_name", "experiment_name", "antigen"),
                          by.y =  c("study_name", "experiment_name", "antigen_label_on_plate"),
                          all.x = T)

  # join in the plate_id from the metadata
  standard_long2 <- merge(standard_long2,
                          batch_metadata[, c("source_file", "plate_id", "plateid", "plate", "project_id", "nominal_sample_dilution")],
                          by = "source_file",
                          all.x = T)

  standard_long2 <- standard_long2[, !names(standard_long2) %in% c("antigen")]
  names(standard_long2)[names(standard_long2) == "antigen_abbreviation"] <- "antigen"
  names(standard_long2)[names(standard_long2) == "study_name"] <- "study_accession"
  names(standard_long2)[names(standard_long2) == "experiment_name"] <- "experiment_accession"
  names(standard_long2)[names(standard_long2) == "specimen_type"] <- "stype"
  names(standard_long2)[names(standard_long2) == "specimen_source"] <- "source"
  names(standard_long2)[names(standard_long2) == "specimen_dilution_factor"] <- "dilution"
  names(standard_long2)[names(standard_long2) == "biosample_id_barcode"] <- "sampleid"
  names(standard_long2)[names(standard_long2) == "% Agg Beads"] <- "pctaggbeads"
  names(standard_long2)[names(standard_long2) == "Sampling Errors"] <- "samplingerrors"

  standards_to_upload <- standard_long2[, c("study_accession", "experiment_accession", "plate_id","well",
                                            "stype", "sampleid", "source", "dilution", "pctaggbeads", "samplingerrors", "antigen",
                                            "antibody_mfi", "antibody_n", "feature", "project_id", "plateid", "nominal_sample_dilution", "plate")]
  return(standards_to_upload)
}

prepare_batch_bead_assay_blanks <- function(blanks_plate_map, combined_plate_data, antigen_import_list, batch_metadata) {
  # blanks_j <- merge(blanks_plate_map, combined_plate_data[, !(names(combined_plate_data) %in% c("Type", "Description"))],
  #                   by.x = c("experiment_name", "plate_number", "well"),
  #                   by.y = c("experiment_name", "plate", "Well"))
  blanks_j <- merge(blanks_plate_map, combined_plate_data[, !(names(combined_plate_data) %in% c("Type", "Description"))],
                    by.x = c("plate_number", "well"),
                    by.y = c("plate", "Well"))

  blanks_long <- blanks_j %>%
    pivot_longer(
      cols = matches("\\([0-9]+\\)"), # all columns containing bead count
      names_to = "antigen",
      values_to = "mfi_bead"
    ) %>%
    mutate(
      # Remove the bead number from column name to get clean antigen name
      antigen = gsub("\\.", " ", antigen),#str_remove(antigen, "\\.\\([0-9]+\\)"),
      # Split MFI and bead count from the value column
      antibody_mfi = as.numeric(str_extract(mfi_bead, "^[0-9.]+")),
      antibody_n = as.numeric(str_extract(mfi_bead, "(?<=\\()[0-9]+(?=\\))"))
    ) %>%
    select(-mfi_bead) %>%
    relocate(antigen, antibody_mfi, antibody_n, .after = biosample_id_barcode)

  blanks_long2 <- merge(blanks_long, antigen_import_list[, c("study_name", "experiment_name", "antigen_label_on_plate", "antigen_abbreviation")],
                        by.x = c("study_name", "experiment_name", "antigen"),
                        by.y =  c("study_name", "experiment_name", "antigen_label_on_plate"),
                        all.x = T)

  blanks_long2 <- merge(blanks_long2,
                        batch_metadata[, c("source_file", "plate_id", "plateid", "plate", "project_id", "nominal_sample_dilution")],
                        by = "source_file",
                        all.x = T)

  blanks_long2 <- blanks_long2[, !names(blanks_long2) %in% c("antigen")]
  names(blanks_long2)[names(blanks_long2) == "antigen_abbreviation"] <- "antigen"
  names(blanks_long2)[names(blanks_long2) == "study_name"] <- "study_accession"
  names(blanks_long2)[names(blanks_long2) == "experiment_name"] <- "experiment_accession"
  names(blanks_long2)[names(blanks_long2) == "specimen_type"] <- "stype"
  names(blanks_long2)[names(blanks_long2) == "specimen_dilution_factor"] <- "dilution"
  names(blanks_long2)[names(blanks_long2) == "% Agg Beads"] <- "pctaggbeads"
  names(blanks_long2)[names(blanks_long2) == "Sampling Errors"] <- "samplingerrors"

  blanks_to_upload <- blanks_long2[, c("study_accession", "experiment_accession", "plate_id","well",
                                       "stype", "pctaggbeads", "samplingerrors", "antigen",
                                       "antibody_mfi", "antibody_n", "dilution", "feature", "project_id", "plateid", "nominal_sample_dilution", "plate")]

  return(blanks_to_upload)

}

prepare_batch_bead_assay_controls <- function(controls_plate_map, combined_plate_data, antigen_import_list, batch_metadata) {
  # controls_j <- merge(controls_plate_map, combined_plate_data[, !(names(combined_plate_data) %in% c("Type", "Description"))],
  #                     by.x = c("experiment_name", "plate_number", "well"),
  #                     by.y = c("experiment_name", "plate", "Well"))
  controls_j <- merge(controls_plate_map, combined_plate_data[, !(names(combined_plate_data) %in% c("Type", "Description"))],
                      by.x = c("plate_number", "well"),
                      by.y = c("plate", "Well"))

  controls_long <- controls_j %>%
    pivot_longer(
      cols = matches("\\([0-9]+\\)"), # all columns containing bead count
      names_to = "antigen",
      values_to = "mfi_bead"
    ) %>%
    mutate(
      # Remove the bead number from column name to get clean antigen name
      antigen = gsub("\\.", " ", antigen),#str_remove(antigen, "\\.\\([0-9]+\\)"),
      # Split MFI and bead count from the value column
      antibody_mfi = as.numeric(str_extract(mfi_bead, "^[0-9.]+")),
      antibody_n = as.numeric(str_extract(mfi_bead, "(?<=\\()[0-9]+(?=\\))"))
    ) %>%
    select(-mfi_bead) %>%
    relocate(antigen, antibody_mfi, antibody_n, .after = biosample_id_barcode)

  # merge in tehe antigen name
  controls_long2 <- merge(controls_long, antigen_import_list[, c("study_name", "experiment_name", "antigen_label_on_plate", "antigen_abbreviation")],
                          by.x = c("study_name", "experiment_name", "antigen"),
                          by.y =  c("study_name", "experiment_name", "antigen_label_on_plate"),
                          all.x = T)

  # merge in the plate_id
  controls_long2 <- merge(controls_long2,
                        batch_metadata[, c("source_file", "plate_id", "plateid", "plate", "project_id", "nominal_sample_dilution")],
                        by = "source_file",
                        all.x = T)


  controls_long2 <- controls_long2[, !names(controls_long2) %in% c("antigen")]
  names(controls_long2)[names(controls_long2) == "antigen_abbreviation"] <- "antigen"
  names(controls_long2)[names(controls_long2) == "study_name"] <- "study_accession"
  names(controls_long2)[names(controls_long2) == "experiment_name"] <- "experiment_accession"
  names(controls_long2)[names(controls_long2) == "specimen_type"] <- "stype"
  names(controls_long2)[names(controls_long2) == "biosample_id_barcode"] <- "sampleid"
  names(controls_long2)[names(controls_long2) == "specimen_source"] <- "source"

  names(controls_long2)[names(controls_long2) == "specimen_dilution_factor"] <- "dilution"
  names(controls_long2)[names(controls_long2) == "%.Agg.Beads"] <- "pctaggbeads"
  names(controls_long2)[names(controls_long2) == "Sampling.Errors"] <- "samplingerrors"

  controls_to_upload <- controls_long2[, c("study_accession", "experiment_accession", "plate_id","well",
                                           "stype", "sampleid", "source", "dilution", "pctaggbeads", "samplingerrors", "antigen",
                                           "antibody_mfi", "antibody_n", "feature", "project_id", "plateid", "nominal_sample_dilution", "plate")]
  return(controls_to_upload)
}

prepare_batch_antigen_family <- function(antigen_import_list, default_family = "All Antigens") {
  antigen_import_list_df <- antigen_import_list[, c("project_id", "study_name", "experiment_name", "antigen_abbreviation", "feature","antigen_family", 
                                                    "standard_curve_max_concentration", "antigen_name", "virus_bacterial_strain", "antigen_source",
                                      "catalog_number", "l_asy_min_constraint", "l_asy_max_constraint", "l_asy_constraint_method")]
  names(antigen_import_list_df)[names(antigen_import_list_df) == "study_name"] <- "study_accession"
  names(antigen_import_list_df)[names(antigen_import_list_df) == "experiment_name"] <- "experiment_accession"
  names(antigen_import_list_df)[names(antigen_import_list_df) == "standard_curve_max_concentration"] <- "standard_curve_concentration"
  names(antigen_import_list_df)[names(antigen_import_list_df) == "antigen_abbreviation"] <- "antigen"

  # --- Step 3: Backfill empty/NA antigen_family values in the returned data ---
  if (nrow(antigen_import_list_df) > 0 && "antigen_family" %in% names(antigen_import_list_df)) {
    na_or_empty <- is.na(antigen_import_list_df$antigen_family) |
      trimws(as.character(antigen_import_list_df$antigen_family)) == ""
    if (any(na_or_empty)) {
      antigen_import_list_df$antigen_family[na_or_empty] <- default_family
    }
  }
  
  # features <- data.frame(feature = unique(samples_to_upload$feature))
  # antigen_import_list_df <- merge(antigen_import_list_df, features, by = NULL)
  
  return(antigen_import_list_df)
}

prepare_planned_visits <- function(timepoint_map) {
  names(timepoint_map)[names(timepoint_map) == "study_name"] <- "study_accession"
  names(timepoint_map)[names(timepoint_map) == "timepoint_tissue_abbreviation"] <- "timepoint_name"
  names(timepoint_map)[names(timepoint_map) == "tissue_type"] <- "type"
  names(timepoint_map)[names(timepoint_map) == "tissue_subtype"] <- "subtype"
  names(timepoint_map)[names(timepoint_map) == "description"] <- "end_rule"
  names(timepoint_map)[names(timepoint_map) == "min_time_since_day_0"] <- "min_start_day"
  names(timepoint_map)[names(timepoint_map) == "max_time_since_day_0"] <- "max_start_day"



  return(timepoint_map)
}

prepare_batch_header <- function(metadata_batch) {

  # Ensure sample_dilution_factor exists and has no NA values
  if (!"sample_dilution_factor" %in% names(metadata_batch)) {
    metadata_batch$sample_dilution_factor <- 1
    cat("  ⚠ Added default sample_dilution_factor column\n")
  } else {
    # Replace NA values with default
    na_count <- sum(is.na(metadata_batch$sample_dilution_factor))
    if (na_count > 0) {
      metadata_batch$sample_dilution_factor[is.na(metadata_batch$sample_dilution_factor)] <- 1
      cat("  ⚠ Replaced", na_count, "NA sample_dilution_factor values with default 1\n")
    }
  }

  cat("Columns in header before cull:", paste(names(metadata_batch), collapse = ", "), "\n")

  metadata_batch <- metadata_batch[,c("study_accession", "experiment_accession", "plate_id", "file_name", "acquisition_date",
                                      "reader_serial_number", "rp1_pmt_volts", "rp1_target",
                                      "auth0_user", "workspace_id", "plateid", "plate",
                                      "assay_response_variable", "assay_independent_variable",
                                      "n_wells", "nominal_sample_dilution", "project_id")]


  nk_cols <- c("project_id", "study_accession", "experiment_accession", "plate_id", "nominal_sample_dilution")
  n_before <- nrow(metadata_batch)
  metadata_batch <- metadata_batch[!duplicated(metadata_batch[, nk_cols, drop = FALSE]), ]
  n_after <- nrow(metadata_batch)

  if (n_before != n_after) {
    cat("  ⚠ Removed", n_before - n_after, "duplicate header rows\n")
  }

  # ensure order for database insert
  metadata_batch <- metadata_batch[,c("study_accession", "experiment_accession", "plate_id", "file_name", "acquisition_date",
                                      "reader_serial_number", "rp1_pmt_volts", "rp1_target",
                                      "auth0_user", "workspace_id", "plateid", "plate",
                                      "n_wells", "assay_response_variable", "assay_independent_variable",
                                      "nominal_sample_dilution", "project_id")]

  # Debug output
  cat("\n=== prepare_batch_header DEBUG ===\n")
  cat("Columns in prepared header:", paste(names(metadata_batch), collapse = ", "), "\n")
  cat("Number of plates:", nrow(metadata_batch), "\n")
  cat("nominal_sample_dilution_factor value:", paste(metadata_batch$nominal_sample_dilution, collapse = ", "), "\n")
  cat("===================================\n")

  return(metadata_batch)
}

parse_metadata_df <- function(header_matrix) {
  # header_matrix is a matrix/data.frame from reading Excel header rows
  # It contains rows like "File Name: C:\\path\\file.xlsx"

  # Convert to data frame if needed
  if (!is.data.frame(header_matrix)) {
    header_matrix <- as.data.frame(header_matrix, stringsAsFactors = FALSE)
  }

  # Initialize result
  result <- data.frame(
    file_name = NA_character_,
    acquisition_date = NA_character_,
    rp1_pmt_volts = NA_character_,
    rp1_target = NA_character_,
    plateid = NA_character_,
    plate = NA_character_,
    stringsAsFactors = FALSE
  )

  # Parse each row
  for (i in 1:nrow(header_matrix)) {
    # Get the first column value
    cell_value <- as.character(header_matrix[i, 1])

    # Extract based on label
    if (grepl("File Name:", cell_value, ignore.case = TRUE)) {
      result$file_name <- trimws(gsub("^.*File Name:\\s*", "", cell_value))
    } else if (grepl("Acquisition Date:", cell_value, ignore.case = TRUE)) {
      result$acquisition_date <- trimws(gsub("^.*Acquisition Date:\\s*", "", cell_value))
    } else if (grepl("RP1 PMT.*Volts", cell_value, ignore.case = TRUE)) {
      result$rp1_pmt_volts <- trimws(gsub("^.*RP1 PMT.*Volts.*:\\s*", "", cell_value))
    } else if (grepl("RP1 Target:", cell_value, ignore.case = TRUE)) {
      result$rp1_target <- trimws(gsub("^.*RP1 Target:\\s*", "", cell_value))
    } else if (grepl("Plate ID:", cell_value, ignore.case = TRUE)) {
      extracted <- trimws(gsub("^.*Plate ID:\\s*", "", cell_value))
      result$plateid <- ifelse(extracted == "" || is.na(extracted), NA_character_, extracted)
    }
  }

  return(result)
}


# SECTION 1: COLUMN MAPPING UTILITIES (unchanged from v1)

#' Create the standard column mapping
create_column_mapping <- function() {
  list(
    # Study/Experiment identifiers
    study_name = "study_accession",
    experiment_name = "experiment_accession",

    # Plate identifiers
    plate_number = "plate",
    number_of_wells = "n_wells",

    # Specimen data
    specimen_type = "stype",
    specimen_dilution_factor = "dilution",
    biosample_id_barcode = "sampleid",
    specimen_source = "source",

    # Time/Subject
    timepoint_tissue_abbreviation = "timeperiod",
    subject_id = "patientid",

    # Quality metrics
    `% Agg Beads` = "pctaggbeads",
    `%.Agg.Beads` = "pctaggbeads",
    `Sampling Errors` = "samplingerrors",
    `Sampling.Errors` = "samplingerrors",

    # Antigen
    antigen_abbreviation = "antigen",

    # Assay response
    assay_response = "antibody_mfi",
    assay_bead_count = "antibody_n",

    # Metadata
    currentuser = "auth0_user"
  )
}


#' Apply column mapping to a data frame
apply_column_mapping <- function(df, mapping = NULL) {
  if (is.null(mapping)) {
    mapping <- create_column_mapping()
  }

  for (old_name in names(mapping)) {
    if (old_name %in% names(df)) {
      new_name <- mapping[[old_name]]
      names(df)[names(df) == old_name] <- new_name
    }
  }
  df
}

#' #' Extract plate number from text
#' #'
#' #' Handles multi-dilution plates by extracting the core plate number
#' #' and ignoring dilution-related suffixes.
#' #'
#' #' @param text Character string containing plate identifier
#' #' @return Plate number in format "plate_N" or NA if not found
#' extract_plate_number <- function(text) {
#'   if (is.na(text) || text == "") return(NA_character_)
#'
#'   # Convert to uppercase for matching
#'   text_upper <- toupper(text)
#'
#'   # Pattern 1: "PLATEN" where N is one or more digits
#'   # This catches: PLATE1, PLATE2, plate_1, PLATE.1, etc.
#'   # But we want to extract just the core plate number, ignoring:
#'   #   - Dilution suffixes like "_1000", "_10000"
#'   #   - Run identifiers like ".1.QIV1", "_QIV3"
#'
#'   # Look for PLATE followed by digits, possibly with separators
#'   match <- regmatches(text_upper, regexpr("PLATE[_\\s\\.-]*(\\d+)", text_upper, perl = TRUE))
#'
#'   if (length(match) > 0 && nchar(match) > 0) {
#'     # Extract just the number
#'     num <- gsub("[^0-9]", "", match)
#'     if (nchar(num) > 0) {
#'       return(paste0("plate_", num))
#'     }
#'   }
#'
#'   # Pattern 2: Try without PLATE prefix - look for plate-like patterns
#'   # e.g., "P1", "P_1" at the start
#'   match2 <- regmatches(text_upper, regexpr("^P[_\\s\\.-]*(\\d+)", text_upper, perl = TRUE))
#'   if (length(match2) > 0 && nchar(match2) > 0) {
#'     num <- gsub("[^0-9]", "", match2)
#'     if (nchar(num) > 0) {
#'       return(paste0("plate_", num))
#'     }
#'   }
#'
#'   return(NA_character_)
#' }


#' Compute all plate identifiers in a single pass
compute_plate_identifiers <- function(header_list) {

  used_plate_nums <- c()
  results <- list()

  for (i in seq_along(header_list)) {
    source_file <- names(header_list)[i]
    header_df <- header_list[[i]]

    # Extract plate number using priority order
    plate_number <- NA_character_
    extraction_source <- NA_character_

    # Priority 1: plateid from header
    if ("plateid" %in% names(header_df) &&
        !is.na(header_df$plateid[1]) &&
        header_df$plateid[1] != "") {
      plate_number <- extract_plate_number(header_df$plateid[1])
      if (!is.na(plate_number)) extraction_source <- "plateid"
    }

    # Priority 2: file_name from header
    if (is.na(plate_number) && "file_name" %in% names(header_df)) {
      plate_number <- extract_plate_number(header_df$file_name[1])
      if (!is.na(plate_number)) extraction_source <- "file_name"
    }

    # Priority 3: source_file
    if (is.na(plate_number)) {
      plate_number <- extract_plate_number(source_file)
      if (!is.na(plate_number)) extraction_source <- "source_file"
    }

    # Fallback: sequential
    if (is.na(plate_number)) {
      fallback_num <- 1
      while (paste0("plate_", fallback_num) %in% used_plate_nums) {
        fallback_num <- fallback_num + 1
      }
      plate_number <- paste0("plate_", fallback_num)
      extraction_source <- "fallback"
    }

    # Handle duplicates
    if (plate_number %in% used_plate_nums) {
      base_num <- as.numeric(gsub("plate_", "", plate_number))
      while (paste0("plate_", base_num) %in% used_plate_nums) {
        base_num <- base_num + 1
      }
      plate_number <- paste0("plate_", base_num)
    }

    used_plate_nums <- c(used_plate_nums, plate_number)

    # Get file_name for plate_id computation
    file_name_val <- if ("file_name" %in% names(header_df)) header_df$file_name[1] else source_file

    results[[i]] <- data.frame(
      source_file = source_file,
      plateid = clean_plate_id(source_file),
      plate_id = clean_plate_id(file_name_val),
      plate_number = plate_number,
      extraction_source = extraction_source,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, results)
}



# SECTION 3: NOMINAL SAMPLE DILUTION COMPUTATION
#' Compute nominal_sample_dilution for each plate
#'
#' Creates a pipe-delimited string of unique specimen_dilution_factor values
#' for sample wells (specimen_type = "X") on each plate.
#'
#' @param plates_map The plates_map data frame
#' @param plate_key_cols Columns that define a unique plate
#' @return Data frame with plate keys and nominal_sample_dilution
compute_nominal_sample_dilution <- function(plates_map,
                                            plate_key_cols = c("project_id", "study_name",
                                                               "experiment_name", "plate_id")) {

  cat("\n=== Computing nominal_sample_dilution ===\n")

  # Filter to sample rows only (specimen_type starts with "X")
  sample_rows <- plates_map[
    substr(plates_map$specimen_type, 1, 1) == "X" &
      !is.na(plates_map$specimen_dilution_factor),
  ]

  cat("  Sample rows found:", nrow(sample_rows), "\n")

  if (nrow(sample_rows) == 0) {
    # No samples - return plates with default dilution
    all_plates <- unique(plates_map[, plate_key_cols, drop = FALSE])
    all_plates$nominal_sample_dilution <- "1"
    return(all_plates)
  }

  # Aggregate dilution factors by plate
  # Using base R aggregate to avoid dplyr dependency issues
  agg_formula <- as.formula(
    paste("specimen_dilution_factor ~", paste(plate_key_cols, collapse = " + "))
  )

  result <- aggregate(
    agg_formula,
    data = sample_rows,
    FUN = function(x) paste(sort(unique(x)), collapse = "|")
  )

  names(result)[names(result) == "specimen_dilution_factor"] <- "nominal_sample_dilution"

  # Ensure all plates are represented (some might have only standards/blanks)
  all_plates <- unique(plates_map[, plate_key_cols, drop = FALSE])

  result <- merge(all_plates, result, by = plate_key_cols, all.x = TRUE)
  result$nominal_sample_dilution[is.na(result$nominal_sample_dilution)] <- "1"

  cat("  Plates processed:", nrow(result), "\n")
  cat("  Unique dilution patterns:", length(unique(result$nominal_sample_dilution)), "\n")
  cat("=====================================\n\n")

  return(result)
}


#' Join nominal_sample_dilution to plates_map and plate_id sheets
#'
#' @param plates_map The plates_map data frame
#' @param plate_id The plate_id data frame
#' @param plate_key_cols Columns that define a unique plate
#' @return List with enhanced plates_map and plate_id
add_nominal_sample_dilution_to_sheets <- function(plates_map, plate_id,
                                                  plate_key_cols = c("project_id", "study_name",
                                                                     "experiment_name", "plate_id")) {

  # Compute dilution once
  nsd_df <- compute_nominal_sample_dilution(plates_map, plate_key_cols)

  # Remove existing nominal_sample_dilution if present (to avoid duplicates)
  if ("nominal_sample_dilution" %in% names(plates_map)) {
    plates_map <- plates_map[, names(plates_map) != "nominal_sample_dilution"]
  }
  if ("nominal_sample_dilution" %in% names(plate_id)) {
    plate_id <- plate_id[, names(plate_id) != "nominal_sample_dilution"]
  }

  # Join to plates_map (propagates to all rows for each plate)
  plates_map_enhanced <- merge(
    plates_map,
    nsd_df,
    by = plate_key_cols,
    all.x = TRUE
  )

  # Join to plate_id (one row per plate)
  plate_id_enhanced <- merge(
    plate_id,
    nsd_df,
    by = plate_key_cols,
    all.x = TRUE
  )

  list(
    plates_map = plates_map_enhanced,
    plate_id = plate_id_enhanced
  )
}



# SECTION 4: ASSAY_RESPONSE_LONG SHEET CREATION
#' Create the assay_response_long sheet from raw plate data
#'
#' Pivots wide MFI data to long format with natural key columns.
#' This sheet becomes the single source of truth for assay response data
#' at upload time.
#'
#' @param combined_plates Combined raw plate data from all files
#' @param plate_identifiers Data frame from compute_plate_identifiers()
#' @param antigen_list The antigen_list sheet from layout
#' @param project_id Project identifier
#' @param study_name Study accession
#' @param experiment_name Experiment accession
#' @return Data frame in long format with natural key + assay response
#' Create assay_response_long sheet from combined plate data
#'
#' Pivots wide MFI data to long format with natural key columns.
#' This is the single source of truth for assay response data.
#'
#' @param all_plates Combined raw plate data (with source_file, Well, and MFI columns)
#' @param plate_id Data frame with plate identifiers (must have plateid column)
#' @param antigen_list Data frame with antigen mapping (antigen_label_on_plate -> antigen_abbreviation)
#' @param project_id Project identifier
#' @param study_name Study accession/name
#' @param experiment_name Experiment accession/name
#' @return Data frame with columns: project_id, study_name, experiment_name, plateid, well, antigen, assay_response, assay_bead_count
create_assay_response_long <- function(all_plates,
                                       plate_id,
                                       antigen_list,
                                       project_id,
                                       study_name,
                                       experiment_name) {

  cat("\n╔══════════════════════════════════════════════════════════╗\n")
  cat("║         CREATING ASSAY_RESPONSE_LONG SHEET               ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n")
  # STEP 1: Identify MFI columns
  all_cols <- names(all_plates)
  mfi_cols <- grep("\\([0-9]+\\)", all_cols, value = TRUE)

  cat("  → MFI columns found:", length(mfi_cols), "\n")

  if (length(mfi_cols) == 0) {
    cat("  ⚠ WARNING: No MFI columns detected!\n")
    return(NULL)
  }

  # =========================================================================
  # STEP 2: Ensure plateid exists in all_plates
  # =========================================================================
  if (!"plateid" %in% names(all_plates)) {
    if ("source_file" %in% names(all_plates)) {
      all_plates$plateid <- sapply(all_plates$source_file, clean_plate_id)
      cat("  → Created plateid from source_file\n")
    } else {
      cat("  ⚠ ERROR: Cannot determine plateid\n")
      return(NULL)
    }
  }

  # =========================================================================
  # STEP 3: Subset to relevant columns and pivot
  # =========================================================================
  # Keep only plateid, Well, and MFI columns for pivot
  meta_cols <- c("plateid", "Well")
  available_meta <- intersect(meta_cols, names(all_plates))

  pivot_data <- all_plates[, c(available_meta, mfi_cols), drop = FALSE]

  cat("  → Rows to pivot:", nrow(pivot_data), "\n")
  cat("  → Pivoting from wide to long...\n")

  # Pivot to long format
  assay_long <- tidyr::pivot_longer(
    pivot_data,
    cols = tidyselect::all_of(mfi_cols),
    names_to = "antigen_label_on_plate",
    values_to = "mfi_bead_combined"
  )

  cat("  → Rows after pivot:", nrow(assay_long), "\n")

  # =========================================================================
  # STEP 4: Extract MFI and bead count, clean antigen label
  # =========================================================================
  assay_long <- assay_long %>%
    dplyr::mutate(
      # Clean antigen label (dots to spaces)
      antigen_label_on_plate = gsub("\\.", " ", antigen_label_on_plate),

      # Extract MFI value
      assay_response = as.numeric(stringr::str_extract(
        as.character(mfi_bead_combined), "^[0-9.]+"
      )),

      # Extract bead count
      assay_bead_count = as.integer(stringr::str_extract(
        as.character(mfi_bead_combined), "(?<=\\()[0-9]+(?=\\))"
      ))
    ) %>%
    dplyr::select(-mfi_bead_combined)

  # Rename Well to well
  if ("Well" %in% names(assay_long)) {
    names(assay_long)[names(assay_long) == "Well"] <- "well"
  }

  # =========================================================================
  # STEP 5: Join antigen abbreviation
  # =========================================================================
  cat("  → Joining antigen abbreviations...\n")

  # Handle different possible column names for antigen abbreviation
  antigen_abbrev_col <- if ("antigen_abbreviation" %in% names(antigen_list)) {
    "antigen_abbreviation"
  } else if ("antigen" %in% names(antigen_list)) {
    "antigen"
  } else {
    NULL
  }

  if (!is.null(antigen_abbrev_col) && "antigen_label_on_plate" %in% names(antigen_list)) {
    antigen_lookup <- unique(antigen_list[, c("antigen_label_on_plate", antigen_abbrev_col), drop = FALSE])

    assay_long <- merge(
      assay_long,
      antigen_lookup,
      by = "antigen_label_on_plate",
      all.x = TRUE
    )

    # Rename to "antigen" if needed
    if (antigen_abbrev_col != "antigen" && antigen_abbrev_col %in% names(assay_long)) {
      names(assay_long)[names(assay_long) == antigen_abbrev_col] <- "antigen"
    }
  } else {
    cat("  ⚠ WARNING: Cannot find antigen mapping columns\n")
    assay_long$antigen <- NA_character_
  }

  # Remove antigen_label_on_plate - not needed in final output
  assay_long$antigen_label_on_plate <- NULL

  # =========================================================================
  # STEP 6: Add context columns
  # =========================================================================
  assay_long$project_id <- project_id
  assay_long$study_name <- study_name
  assay_long$experiment_name <- experiment_name

  # =========================================================================
  # STEP 7: Select and order final columns
  # =========================================================================
  final_cols <- c(
    "project_id",
    "study_name",
    "experiment_name",
    "plateid",
    "well",
    "antigen",
    "assay_response",
    "assay_bead_count"
  )

  available_final <- intersect(final_cols, names(assay_long))
  assay_long <- assay_long[, available_final, drop = FALSE]

  # =========================================================================
  # STEP 8: Report results
  # =========================================================================
  missing_antigen <- sum(is.na(assay_long$antigen))
  if (missing_antigen > 0) {
    cat("  ⚠ WARNING:", missing_antigen, "rows have no antigen match\n")
  }

  cat("  → Final columns:", paste(names(assay_long), collapse = ", "), "\n")
  cat("  ✓ assay_response_long created with", nrow(assay_long), "rows\n")
  cat("╚══════════════════════════════════════════════════════════╝\n\n")

  return(assay_long)
}




# SECTION 5: ENHANCED PLATE_ID SHEET
#' Enhance the plate_id sheet with file mapping columns
#'
#' Adds source_file, plateid, plate_id from plate_identifiers,
#' plus nominal_sample_dilution and project_id.
#'
#' @param plate_id_sheet Original plate_id sheet
#' @param plate_identifiers Data frame from compute_plate_identifiers()
#' @param nominal_sample_dilution_df Data frame with plate-level dilutions
#' @param project_id Project identifier
#' @return Enhanced plate_id data frame
enhance_plate_id_sheet <- function(plate_id_sheet,
                                   plate_identifiers,
                                   nominal_sample_dilution_df = NULL,
                                   project_id) {

  cat("\n=== Enhancing plate_id sheet ===\n")

  # Start with original sheet
  enhanced <- plate_id_sheet

  # Add project_id if not present
  if (!"project_id" %in% names(enhanced)) {
    enhanced$project_id <- project_id
  }

  # Join plate identifiers
  # Match on plate_filename (original file name) to source_file
  if ("plate_filename" %in% names(enhanced)) {
    # Create a lookup from plate_identifiers
    id_lookup <- plate_identifiers[, c("source_file", "plateid", "plate_id", "plate_number")]

    # First try matching plate_filename to source_file
    enhanced <- merge(
      enhanced,
      id_lookup,
      by.x = "plate_filename",
      by.y = "source_file",
      all.x = TRUE,
      suffixes = c("", "_from_id")
    )

    # If plate_id was already present, use it; otherwise use the joined one
    if ("plate_id_from_id" %in% names(enhanced)) {
      if ("plate_id" %in% names(enhanced)) {
        # Keep original, remove duplicate
        enhanced <- enhanced[, names(enhanced) != "plate_id_from_id"]
      } else {
        names(enhanced)[names(enhanced) == "plate_id_from_id"] <- "plate_id"
      }
    }

    # Add source_file column (same as plate_filename for mapping purposes)
    enhanced$source_file <- enhanced$plate_filename
  }

  # Join nominal_sample_dilution if provided
  if (!is.null(nominal_sample_dilution_df)) {
    join_cols <- intersect(
      names(nominal_sample_dilution_df),
      names(enhanced)
    )
    join_cols <- setdiff(join_cols, "nominal_sample_dilution")

    if (length(join_cols) > 0) {
      # Remove existing nominal_sample_dilution if present
      enhanced <- enhanced[, names(enhanced) != "nominal_sample_dilution", drop = FALSE]

      enhanced <- merge(
        enhanced,
        nominal_sample_dilution_df,
        by = join_cols,
        all.x = TRUE
      )
    }
  }

  cat("  → Columns in enhanced plate_id:", paste(names(enhanced), collapse = ", "), "\n")
  cat("  → Rows:", nrow(enhanced), "\n")
  cat("================================\n\n")

  return(enhanced)
}



# SECTION 6: UPLOAD PREPARATION USING ASSAY_RESPONSE_LONG
#' Prepare specimen data for upload using assay_response_long
#'
#' Joins plates_map (specimen metadata) with assay_response_long (MFI data)
#' using the natural key, then applies column mapping for database insert.
#'
#' @param plates_map The plates_map sheet (with nominal_sample_dilution)
#' @param assay_response_long The pre-pivoted MFI data
#' @param subject_map The subject_groups sheet (for samples only)
#' @param specimen_type One of "X", "S", "B", "C"
#' @param col_mapping Column mapping for database
#' @return Data frame ready for database insert
prepare_specimen_for_upload <- function(plates_map,
                                        assay_response_long,
                                        subject_map = NULL,
                                        specimen_type,
                                        col_mapping = NULL) {

  if (is.null(col_mapping)) {
    col_mapping <- create_column_mapping()
  }

  cat("\n  Preparing", specimen_type, "data for upload...\n")

  # Filter plates_map to this specimen type
  specimen_map <- plates_map[substr(plates_map$specimen_type, 1, 1) == specimen_type, ]

  if (nrow(specimen_map) == 0) {
    cat("    → No", specimen_type, "specimens found\n")
    return(NULL)
  }

  cat("    → Specimen rows:", nrow(specimen_map), "\n")

  # For samples, join subject_groups first
  if (specimen_type == "X" && !is.null(subject_map)) {
    specimen_map <- merge(
      specimen_map,
      subject_map,
      by = c("study_name", "subject_id"),
      all.x = TRUE
    )
    # Create combined group
    specimen_map$agroup <- ifelse(
      is.na(specimen_map$groupb),
      specimen_map$groupa,
      paste(specimen_map$groupa, specimen_map$groupb, sep = "_")
    )
  }

  # Define natural key for joining to assay_response
  natural_key <- c("project_id", "study_name", "experiment_name", "plateid", "well")

  # Join assay response data
  specimen_data <- merge(
    specimen_map,
    assay_response_long,
    by = natural_key,
    all.x = TRUE,
    suffixes = c("", "_assay")
  )

  cat("    → After join with assay_response:", nrow(specimen_data), "\n")

  # Handle duplicate columns from merge
  # Prefer the plate_id from plates_map
  if ("plate_id_assay" %in% names(specimen_data)) {
    specimen_data <- specimen_data[, names(specimen_data) != "plate_id_assay"]
  }

  # Apply column mapping
  specimen_data <- apply_column_mapping(specimen_data, col_mapping)

  # Select appropriate columns based on specimen type
  if (specimen_type == "X") {
    final_cols <- c(
      "study_accession", "experiment_accession", "plate_id", "timeperiod",
      "patientid", "well", "stype", "sampleid", "agroup", "dilution",
      "pctaggbeads", "samplingerrors", "antigen", "antibody_mfi", "antibody_n",
      "feature", "nominal_sample_dilution", "project_id", "plateid", "plate"
    )
  } else if (specimen_type == "S") {
    final_cols <- c(
      "study_accession", "experiment_accession", "plate_id", "well",
      "stype", "source", "dilution", "pctaggbeads", "samplingerrors",
      "antigen", "antibody_mfi", "antibody_n", "feature", "project_id",
      "plateid", "nominal_sample_dilution", "plate"
    )
  } else if (specimen_type == "B") {
    final_cols <- c(
      "study_accession", "experiment_accession", "plate_id", "well",
      "stype", "pctaggbeads", "samplingerrors", "antigen", "antibody_mfi",
      "antibody_n", "dilution", "feature", "project_id", "plateid",
      "nominal_sample_dilution", "plate"
    )
  } else if (specimen_type == "C") {
    final_cols <- c(
      "study_accession", "experiment_accession", "plate_id", "well",
      "stype", "sampleid", "source", "dilution", "pctaggbeads",
      "samplingerrors", "antigen", "antibody_mfi", "antibody_n", "feature",
      "project_id", "plateid", "nominal_sample_dilution", "plate"
    )
  }

  available_cols <- intersect(final_cols, names(specimen_data))
  specimen_data <- specimen_data[, available_cols, drop = FALSE]

  cat("    → Final columns:", length(available_cols), "\n")

  return(specimen_data)
}



# SECTION 7: LAYOUT TEMPLATE GENERATION ADDITIONS
#' Add assay_response_long sheet to workbook
#'
#' @param wb An openxlsx workbook object
#' @param assay_response_long Data frame from create_assay_response_long()
#' @return Modified workbook
add_assay_response_long_sheet <- function(wb, assay_response_long) {

  if (is.null(assay_response_long) || nrow(assay_response_long) == 0) {
    cat("  ⚠ No assay_response_long data to write\n")
    return(wb)
  }

  addWorksheet(wb, "assay_response_long")
  writeData(wb, "assay_response_long", assay_response_long, startRow = 1, startCol = 1)

  # Style header
  bold_style <- createStyle(textDecoration = "bold")
  addStyle(wb, "assay_response_long", style = bold_style, rows = 1, cols = 1:ncol(assay_response_long))

  # Set column widths
  setColWidths(wb, "assay_response_long", cols = 1:ncol(assay_response_long), widths = "auto")

  cat("  ✓ Added assay_response_long sheet with", nrow(assay_response_long), "rows\n")

  return(wb)
}


#' Add upload_config sheet to workbook
add_upload_config_sheet <- function(wb, config = list()) {

  default_config <- list(
    upload_version = "2.0",
    description_delimiter = "_",
    description_element_order = "PatientID,TimePeriod,DilutionFactor",
    bcs_element_order = "Source,DilutionFactor",
    default_dilution_factor = "1",
    default_subject_id = "1",
    default_timepoint = "T0",
    generated_timestamp = as.character(Sys.time())
  )

  final_config <- modifyList(default_config, config)

  config_df <- data.frame(
    field_name = names(final_config),
    field_value = unlist(final_config),
    description = c(
      "Schema version for compatibility checking",
      "Delimiter used in Description field parsing",
      "Order of elements in sample Description",
      "Order of elements for Blanks/Controls/Standards",
      "Default value when dilution factor cannot be parsed",
      "Default value when subject ID cannot be parsed",
      "Default value when timepoint cannot be parsed",
      "Timestamp when this template was generated"
    ),
    stringsAsFactors = FALSE
  )
  rownames(config_df) <- NULL

  addWorksheet(wb, "upload_config")
  writeData(wb, "upload_config", config_df, startRow = 1, startCol = 1)

  bold_style <- createStyle(textDecoration = "bold")
  addStyle(wb, "upload_config", style = bold_style, rows = 1, cols = 1:3)
  setColWidths(wb, "upload_config", cols = 1:3, widths = c(30, 50, 50))

  return(wb)
}



# SECTION 8: COMPLETE UPLOAD WORKFLOW
#' Prepare complete upload package from layout sheets
#'
#' This is the main entry point for preparing upload data.
#' Should be called after layout validation succeeds.
#'
#' @param layout_sheets List of layout template sheets
#' @param assay_response_long Pre-created assay response data
#' @param project_id Project identifier
#' @param workspace_id Workspace identifier
#' @param auth0_user Current user
#' @return List of data frames ready for database insert
prepare_upload_package <- function(layout_sheets,
                                   assay_response_long,
                                   project_id,
                                   workspace_id,
                                   auth0_user) {

  cat("\n╔══════════════════════════════════════════════════════════╗\n")
  cat("║         PREPARING UPLOAD PACKAGE                         ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n")

  # Get layout components
  plate_id <- layout_sheets[["plate_id"]]
  plates_map <- layout_sheets[["plates_map"]]
  antigen_list <- layout_sheets[["antigen_list"]]
  subject_map <- layout_sheets[["subject_groups"]]
  timepoint_map <- layout_sheets[["timepoint"]]

  col_mapping <- create_column_mapping()

  # === HEADER ===
  cat("  → Preparing header...\n")
  header_data <- plate_id
  header_data$workspace_id <- workspace_id
  header_data$auth0_user <- auth0_user
  header_data$assay_response_variable <- "mfi"
  header_data$assay_independent_variable <- "concentration"
  header_data <- apply_column_mapping(header_data, col_mapping)

  # Rename for database
  if ("plate_filename" %in% names(header_data)) {
    names(header_data)[names(header_data) == "plate_filename"] <- "file_name"
  }

  # === SPECIMENS ===
  cat("  → Preparing specimens...\n")
  samples_data <- prepare_specimen_for_upload(
    plates_map, assay_response_long, subject_map, "X", col_mapping
  )

  standards_data <- prepare_specimen_for_upload(
    plates_map, assay_response_long, NULL, "S", col_mapping
  )

  blanks_data <- prepare_specimen_for_upload(
    plates_map, assay_response_long, NULL, "B", col_mapping
  )

  controls_data <- prepare_specimen_for_upload(
    plates_map, assay_response_long, NULL, "C", col_mapping
  )

  # === ANTIGENS ===
  cat("  → Preparing antigens...\n")
  antigens_data <- antigen_list[, c(
    "study_name", "experiment_name", "antigen_abbreviation", "antigen_family",
    "standard_curve_max_concentration", "antigen_name", "virus_bacterial_strain",
    "antigen_source", "catalog_number", "l_asy_min_constraint",
    "l_asy_max_constraint", "l_asy_constraint_method"
  ), drop = FALSE]
  names(antigens_data)[names(antigens_data) == "standard_curve_max_concentration"] <- "standard_curve_concentration"
  antigens_data <- apply_column_mapping(antigens_data, col_mapping)

  # === VISITS ===
  cat("  → Preparing planned visits...\n")
  visits_data <- timepoint_map
  names(visits_data)[names(visits_data) == "timepoint_tissue_abbreviation"] <- "timepoint_name"
  names(visits_data)[names(visits_data) == "tissue_type"] <- "type"
  names(visits_data)[names(visits_data) == "tissue_subtype"] <- "subtype"
  names(visits_data)[names(visits_data) == "description"] <- "end_rule"
  names(visits_data)[names(visits_data) == "min_time_since_day_0"] <- "min_start_day"
  names(visits_data)[names(visits_data) == "max_time_since_day_0"] <- "max_start_day"
  visits_data <- apply_column_mapping(visits_data, col_mapping)

  # === METADATA ===
  study_accession <- unique(plates_map$study_name)[1]
  experiment_accession <- unique(plates_map$experiment_name)[1]
  plate_ids <- unique(plate_id$plate_id)

  cat("  ✓ Upload package prepared\n")
  cat("╚══════════════════════════════════════════════════════════╝\n\n")

  list(
    header = header_data,
    samples = samples_data,
    standards = standards_data,
    blanks = blanks_data,
    controls = controls_data,
    antigens = antigens_data,
    visits = visits_data,
    metadata = list(
      study_accession = study_accession,
      experiment_accession = experiment_accession,
      project_id = project_id,
      plate_ids = plate_ids
    )
  )
}

#' Write assay_response_long sheet to workbook
#'
#' @param wb An openxlsx workbook object
#' @param assay_response_long Data frame from build_assay_response_long()
#' @param bold_style Style for header row
#' @return Modified workbook
write_assay_response_long_sheet <- function(wb, assay_response_long, bold_style) {

  if (is.null(assay_response_long) || nrow(assay_response_long) == 0) {
    cat("  ⚠ No assay_response_long data to write\n")
    return(wb)
  }

  addWorksheet(wb, "assay_response_long")
  writeData(wb, "assay_response_long", assay_response_long, startRow = 1, startCol = 1)

  # Style header
  addStyle(wb, "assay_response_long", style = bold_style, rows = 1, cols = 1:ncol(assay_response_long))

  # Set column widths
  setColWidths(wb, "assay_response_long", cols = 1:ncol(assay_response_long), widths = "auto")

  cat("  ✓ Added assay_response_long sheet with", nrow(assay_response_long), "rows\n")

  return(wb)
}


write_workbook_sheets_v2 <- function(wb, workbook, bold_style, italic_style) {

  # Write standard sheets
  for (sheet_name in names(workbook)) {
    if (sheet_name == "assay_response_long") {
      # Handle separately due to potentially large size
      next
    }

    sheet_data <- workbook[[sheet_name]]

    if (is.null(sheet_data) || nrow(sheet_data) == 0) {
      cat("  → Skipping empty sheet:", sheet_name, "\n")
      next
    }

    addWorksheet(wb, sheet_name)
    writeData(wb, sheet_name, sheet_data, startRow = 1, startCol = 1)

    # Style header
    addStyle(wb, sheet_name, style = bold_style, rows = 1, cols = 1:ncol(sheet_data))

    # Set column widths
    setColWidths(wb, sheet_name, cols = 1:ncol(sheet_data), widths = "auto")

    cat("  ✓ Wrote", sheet_name, "sheet (", nrow(sheet_data), "rows)\n")
  }

  # Write assay_response_long separately (may be large)
  if ("assay_response_long" %in% names(workbook)) {
    wb <- write_assay_response_long_sheet(
      wb,
      workbook[["assay_response_long"]],
      bold_style
    )
  }

  return(wb)
}

process_uploaded_files <- function(upload_df) {
  # Define temp directory for this batch
  tmpdir <- file.path(tempdir(), "uploaded_batch")
  
  # CRITICAL FIX: Clear the directory if it exists to remove old files
  if (dir.exists(tmpdir)) {
    cat("  → Clearing old temp directory:", tmpdir, "\n")
    old_files <- list.files(tmpdir, full.names = TRUE)
    if (length(old_files) > 0) {
      cat("  → Removing", length(old_files), "old files\n")
      file.remove(old_files)
    }
  } else {
    cat("  → Creating temp directory:", tmpdir, "\n")
    dir.create(tmpdir)
  }
  
  # Copy new files to temp directory
  cat("  → Copying", nrow(upload_df), "new files to temp directory\n")
  for (i in 1:nrow(upload_df)) {
    file.copy(upload_df$datapath[i],
              file.path(tmpdir, upload_df$name[i]),
              overwrite = TRUE)
  }
  
  # Verify what files are in the directory
  files_in_dir <- list.files(tmpdir, pattern = "plate", full.names = FALSE)
  cat("  → Files in temp directory:", paste(files_in_dir, collapse = ", "), "\n")
  
  # Read and combine all Excel files
  result <- read_bind_xlsx(folder = tmpdir, x = "plate")
  
  cat("  → Combined data dimensions:", nrow(result), "rows x", ncol(result), "cols\n")
  
  return(result)
}

