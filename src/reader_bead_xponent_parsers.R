# =============================================================================
# reader_bead_xponent_parsers.R  —  11.10 Assay Import Refactor, Phase 4
# -----------------------------------------------------------------------------
# Pure xPONENT parser functions LIFTED verbatim from the retired xPonentReader.R
# (its observers/UI are gone — the generic module replaces them). Sourced by
# reader_bead.R (bead/xponent format). Depends on moach::read_Xponent_csv and
# the kept helpers (clean_plate_id, assign_plate_numbers) from
# batch_layout_functions.R.
# =============================================================================

#' Convert a single xPONENT file to wide-format plate data
#'
#' Takes the output of moach::read_Xponent_csv() and pivots it from long format
#' (one row per well x analyte) to wide format (one row per well, one column per
#' analyte MFI value), matching the structure produced by raw .xlsx plate files.
#'
#' @param lumcsv Output of moach::read_Xponent_csv()
#' @param file_name Original file name for the source_file column
#' @return Data frame in wide format with columns: source_file, Well, Type,
#'         Description, and one column per analyte (Net MFI values)
#'
xponent_to_wide_plate_data <- function(lumcsv, file_name) {

  # Extract the expression data and well information
  raw_exprs <- lumcsv$AssayData$Exprs
  raw_wells <- lumcsv$AssayData$Wells

  # Join expression data with well metadata
  combined <- raw_exprs[, c("Location", "Sample", "Analyte", "Count", "Net_MFI")] %>%
    dplyr::full_join(
      raw_wells[, c("Location", "Sample")],
      by = c("Location", "Sample")
    ) %>%
    janitor::clean_names()

  # Pivot to wide format: one column per analyte with Net_MFI values
  wide_data <- combined %>%
    dplyr::select(location, sample, analyte, net_mfi) %>%
    tidyr::pivot_wider(
      id_cols = c(location, sample),
      names_from = analyte,
      values_from = net_mfi,
      values_fn = list(net_mfi = dplyr::first)
    )

  # Map xPONENT columns to the standard raw file format
  # The raw format expects: Well, Type, Description, then antigen columns
  plate_data <- wide_data %>%
    dplyr::rename(
      Well = location,
      Description = sample
    )

  # Infer Type from the sample name (xPONENT uses naming conventions)
  # Standards often start with "S", Controls with "C", Blanks with "B"
  # Samples are everything else (Type = "X")
  plate_data$Type <- vapply(plate_data$Description, function(desc) {
    if (is.na(desc) || desc == "") return("X")
    desc_upper <- toupper(trimws(desc))

    # Check for standard patterns
    if (grepl("^S[0-9]", desc_upper) || grepl("^STD", desc_upper, ignore.case = TRUE)) {
      return(paste0("S", gsub("[^0-9]", "", substr(desc_upper, 1, 5))))
    }
    if (grepl("^C[0-9]", desc_upper) || grepl("^CTRL", desc_upper, ignore.case = TRUE) ||
        grepl("^CONTROL", desc_upper, ignore.case = TRUE)) {
      return(paste0("C", gsub("[^0-9]", "", substr(desc_upper, 1, 5))))
    }
    if (grepl("^B[0-9]?$", desc_upper) || grepl("^BLANK", desc_upper, ignore.case = TRUE) ||
        grepl("^BACKGROUND", desc_upper, ignore.case = TRUE)) {
      return("B")
    }
    return("X")
  }, character(1))

  # Add source file column
  plate_data$source_file <- file_name

  # Reorder: source_file, Well, Type, Description, then antigens
  antigen_cols <- setdiff(names(plate_data), c("source_file", "Well", "Type", "Description"))
  plate_data <- plate_data[, c("source_file", "Well", "Type", "Description", antigen_cols)]

  return(plate_data)
}

#' Extract header metadata from an xPONENT file
#'
#' Creates a header data frame matching the structure expected by
#' generate_layout_template() and the batch upload workflow.
#'
#' @param lumcsv Output of moach::read_Xponent_csv()
#' @param file_name Original file name
#' @return Data frame with one row of header metadata
#'
extract_xponent_header <- function(lumcsv, file_name) {

  batch_info <- lumcsv$BatchHeader$BatchInfo

  # Build plate_id from batch name (consistent with existing xPONENT logic)
  plate_id_raw <- file.path("E:", "batch",
                            paste0(gsub("[[:punct:][:blank:]]+", ".", batch_info$Batch), ".csv"))

  # Extract acquisition date and normalize to DB format
  acq_date <- tryCatch({
    raw_date <- as.character(batch_info$Date)
    normalize_acquisition_date(raw_date)
  }, error = function(e) NA_character_)

  # Extract serial number
  serial_number <- tryCatch({
    batch_info$SN
  }, error = function(e) NA_character_)

  data.frame(
    source_file = file_name,
    file_name = file_name,
    plateid = clean_plate_id(plate_id_raw),
    acquisition_date = as.character(acq_date),
    reader_serial_number = as.character(serial_number),
    rp1_pmt_volts = NA_character_,
    rp1_target = NA_character_,
    stringsAsFactors = FALSE
  )
}

#' Build assay_response_long from a single xPONENT CSV
#'
#' Extracts the long-format expression data (Net_MFI + bead count per well per
#' analyte) directly from the parsed xPONENT object, BEFORE any wide pivot.
#' This preserves bead counts that are lost in the wide plate data.
#'
#' @param lumcsv Parsed xPONENT object from moach::read_Xponent_csv()
#' @param file_name Original file name for the source_file column
#' @return Data frame with columns: source_file, well, antigen, feature,
#'         assay_response, assay_bead_count
#'
extract_xponent_assay_response_long <- function(lumcsv, file_name) {

  raw_exprs <- lumcsv$AssayData$Exprs

  # Build long format directly from expression data
  assay_long <- raw_exprs[, c("Location", "Analyte", "Net_MFI", "Count")] %>%
    dplyr::rename(
      well             = Location,
      antigen          = Analyte,
      assay_response   = Net_MFI,
      assay_bead_count = Count
    ) %>%
    dplyr::mutate(
      source_file      = file_name,
      feature          = "Net_MFI",
      assay_response   = as.numeric(assay_response),
      assay_bead_count = as.integer(assay_bead_count)
    )

  return(assay_long)
}

#' Process multiple xPONENT files into the standard batch format
#'
#' This is the xPONENT equivalent of process_experiment_files(). It reads
#' multiple xPONENT CSV files, converts each to wide format, and combines
#' them. The output format matches what process_experiment_files() produces,
#' so all downstream processing (layout generation, validation, upload) works
#' identically.
#'
#' @param upload_df Data frame from Shiny fileInput (columns: name, datapath, etc.)
#' @return List with combined_plates, header_list, plate_list, assay_response_long
#'
process_xponent_files <- function(upload_df) {
  cat("Processing", nrow(upload_df), "xPONENT files...\n")

  results <- lapply(seq_len(nrow(upload_df)), function(i) {
    file_path <- upload_df$datapath[i]
    file_name <- upload_df$name[i]
    cat("  Processing xPONENT:", file_name, "\n")

    tryCatch({
      # Read xPONENT CSV using moach
      lumcsv <- moach::read_Xponent_csv(file_path)

      # Convert to wide plate data
      plate_data <- xponent_to_wide_plate_data(lumcsv, file_name)
      cat("    -> Columns:", ncol(plate_data), ", Rows:", nrow(plate_data), "\n")

      # Extract header metadata
      header_data <- extract_xponent_header(lumcsv, file_name)

      # Extract long-format assay response (preserves bead counts)
      assay_long <- extract_xponent_assay_response_long(lumcsv, file_name)

      list(
        plate = plate_data,
        header = header_data,
        assay_long = assay_long,
        file_name = file_name
      )
    }, error = function(e) {
      cat("    x ERROR:", conditionMessage(e), "\n")
      showNotification(
        paste("Failed to read xPONENT file:", file_name, "-", conditionMessage(e)),
        type = "error", duration = 10
      )
      return(NULL)
    })
  })

  # Remove failed files
  results <- Filter(Negate(is.null), results)

  if (length(results) == 0) {
    stop("No valid xPONENT files could be processed")
  }

  # Extract headers and plates
  names(results) <- sapply(results, `[[`, "file_name")
  header_list <- lapply(results, `[[`, "header")
  plate_list <- lapply(results, `[[`, "plate")
  assay_long_list <- lapply(results, `[[`, "assay_long")

  # Assign plate numbers (reuse existing function)
  cat("\nExtracting plate numbers...\n")
  header_list <- assign_plate_numbers(header_list)

  # Combine all plates
  cat("\nCombining xPONENT plate data...\n")
  combined_plates <- dplyr::bind_rows(plate_list)

  # Add plateid from source_file
  if ("source_file" %in% names(combined_plates)) {
    combined_plates$plateid <- clean_plate_id(combined_plates$source_file)
  }

  # Combine assay_response_long across files and add plateid
  cat("Combining xPONENT assay_response_long...\n")
  assay_response_long <- dplyr::bind_rows(assay_long_list)
  if ("source_file" %in% names(assay_response_long)) {
    assay_response_long$plateid <- clean_plate_id(assay_response_long$source_file)
  }
  cat("  -> assay_response_long:", nrow(assay_response_long), "rows x",
      ncol(assay_response_long), "cols\n")

  cat("  -> Combined dimensions:", nrow(combined_plates), "rows x", ncol(combined_plates), "cols\n")
  cat("  -> Source files:", paste(unique(combined_plates$source_file), collapse = ", "), "\n")

  list(
    combined_plates = combined_plates,
    header_list = header_list,
    plate_list = plate_list,
    assay_response_long = assay_response_long
  )
}
