# =============================================================================
# reader_bead_xponent_parsers.R  —  11.10 Assay Import Refactor, Phase 4
# -----------------------------------------------------------------------------
# Pure xPONENT parser functions LIFTED verbatim from the retired xPonentReader.R
# (its observers/UI are gone — the generic module replaces them). Sourced by
# reader_bead.R (bead/xponent format). Depends on moach::read_Xponent_csv and
# the kept helpers (clean_plate_id, assign_plate_numbers) from
# batch_layout_functions.R.
# =============================================================================

# ---- MFI source selection ---------------------------------------------------
# An xPONENT export carries several response blocks, each introduced by a
# "DataType:,<name>" line: Median, Net MFI, Count, Avg Net MFI, ... Which ones
# are exported, and in what ORDER, depends on the lab's protocol/export setup, so
# nothing here may assume a block's position. moach finds each block by its
# label; these helpers find it by label too and let the user pick which one
# becomes assay_response.
#
#   key          what it is                                   where moach puts it
#   net_mfi      Median less the protocol's background        AssayData$Exprs$Net_MFI
#   median       raw median fluorescence                      AssayData$Exprs$Median
#   avg_net_mfi  mean Net MFI over wells sharing a sample     AssayData$Average$Avg_Net_MFI
#                name (keyed by Sample only, NOT by well)
#
# Net MFI stays the default: it is what this reader always used.
XPONENT_MFI_SOURCES <- list(
  net_mfi     = list(label = "Net MFI",         section = "Net MFI",     table = "Exprs",   column = "Net_MFI"),
  median      = list(label = "Median",          section = "Median",      table = "Exprs",   column = "Median"),
  avg_net_mfi = list(label = "Average Net MFI", section = "Avg Net MFI", table = "Average", column = "Avg_Net_MFI")
)
XPONENT_DEFAULT_MFI_SOURCE <- "net_mfi"

# Named choices for a selectInput: label -> key.
xponent_mfi_choices <- function(keys = names(XPONENT_MFI_SOURCES)) {
  stats::setNames(keys, vapply(XPONENT_MFI_SOURCES[keys], `[[`, "", "label"))
}

# Normalise a possibly NULL/blank/unknown UI value to a valid source key.
.xponent_mfi_key <- function(x) {
  x <- if (is.null(x) || !length(x)) "" else trimws(as.character(x)[1])
  if (!nzchar(x) || is.na(x)) return(XPONENT_DEFAULT_MFI_SOURCE)
  if (!x %in% names(XPONENT_MFI_SOURCES))
    stop(sprintf("Unknown MFI source '%s' (expected one of: %s)",
                 x, paste(names(XPONENT_MFI_SOURCES), collapse = ", ")), call. = FALSE)
  x
}

#' DataType sections present in one xPONENT file, in FILE order
#'
#' A cheap line scan (no moach parse), so the UI can report what a file offers
#' as soon as it is selected. Tolerates quoted fields and ';' delimiters, like
#' moach does.
xponent_file_sections <- function(path) {
  lines <- readLines(path, warn = FALSE)
  hit <- grep("^\"?DataType:\"?[,;]", lines, value = TRUE)
  vapply(strsplit(hit, "[,;]"), function(p) gsub("^\"|\"$", "", trimws(p[2])), "")
}

#' Which selectable MFI sources a set of sections offers
#'
#' @param file_order FALSE (default): canonical order, for stable menus. TRUE:
#'   the order the blocks appear in the file, to tell the user where they are.
xponent_mfi_sources_in <- function(sections, file_order = FALSE) {
  sec <- vapply(XPONENT_MFI_SOURCES, `[[`, "", "section")
  if (file_order) return(names(sec)[match(unique(sections[sections %in% sec]), sec)])
  names(sec)[sec %in% sections]
}

#' Scan uploaded xPONENT files for the MFI sources they offer
#'
#' @param upload_df Shiny fileInput data frame (name, datapath)
#' @return list(sections = per-file section names in file order,
#'         sources = per-file source keys, common = keys offered by EVERY file,
#'         lacking = for each key, the file names that do not offer it)
xponent_scan_upload <- function(upload_df) {
  paths <- upload_df$datapath; nms <- upload_df$name
  sections <- stats::setNames(lapply(paths, xponent_file_sections), nms)
  sources  <- lapply(sections, xponent_mfi_sources_in)
  common   <- Reduce(intersect, sources, names(XPONENT_MFI_SOURCES))
  lacking  <- stats::setNames(lapply(names(XPONENT_MFI_SOURCES), function(k)
    nms[!vapply(sources, function(s) k %in% s, logical(1))]), names(XPONENT_MFI_SOURCES))
  list(sections = sections, sources = sources, common = common, lacking = lacking)
}

#' Long (well x analyte) response table for the chosen MFI source
#'
#' @return data frame: Location, Sample, Analyte, Count, mfi. Values are NOT
#'   coerced here (callers coerce where they always did) so the default source
#'   reproduces the previous Net_MFI output exactly.
xponent_mfi_long <- function(lumcsv, mfi_source = XPONENT_DEFAULT_MFI_SOURCE) {
  key <- .xponent_mfi_key(mfi_source)
  src <- XPONENT_MFI_SOURCES[[key]]
  exprs <- lumcsv$AssayData$Exprs

  offered <- c(
    names(Filter(function(s) s$table == "Exprs" && s$column %in% names(exprs), XPONENT_MFI_SOURCES)),
    if (is.data.frame(lumcsv$AssayData$Average))
      names(Filter(function(s) s$table == "Average" && s$column %in% names(lumcsv$AssayData$Average),
                   XPONENT_MFI_SOURCES)))
  if (!key %in% offered)
    stop(sprintf("This file has no '%s' section, so '%s' cannot be used. MFI sources it does offer: %s.",
                 src$section, src$label,
                 if (length(offered)) paste(vapply(XPONENT_MFI_SOURCES[offered], `[[`, "", "label"),
                                            collapse = ", ") else "none"),
         call. = FALSE)

  base <- exprs[, intersect(c("Location", "Sample", "Analyte", "Count"), names(exprs)), drop = FALSE]
  if (!"Count" %in% names(base)) base$Count <- NA_integer_

  if (identical(src$table, "Exprs")) {
    base$mfi <- exprs[[src$column]]
  } else {
    # Average blocks are keyed by Sample + Analyte only: every replicate well of
    # a sample name receives that sample's one average. A keyed lookup (not a
    # join) so well order and row count are untouched, and the keys are compared
    # as text because moach type-converts each column on its own.
    avg <- lumcsv$AssayData$Average
    key <- function(d) paste(as.character(d$Sample), as.character(d$Analyte), sep = "\r")
    base$mfi <- avg[[src$column]][match(key(base), key(avg))]
  }
  base
}

#' Convert a single xPONENT file to wide-format plate data
#'
#' Takes the output of moach::read_Xponent_csv() and pivots it from long format
#' (one row per well x analyte) to wide format (one row per well, one column per
#' analyte MFI value), matching the structure produced by raw .xlsx plate files.
#'
#' @param lumcsv Output of moach::read_Xponent_csv()
#' @param file_name Original file name for the source_file column
#' @param mfi_source Which response block fills the analyte columns: "net_mfi"
#'   (default), "median" or "avg_net_mfi" (see XPONENT_MFI_SOURCES)
#' @return Data frame in wide format with columns: source_file, Well, Type,
#'         Description, and one column per analyte (the chosen MFI source)
#'
xponent_to_wide_plate_data <- function(lumcsv, file_name,
                                       mfi_source = XPONENT_DEFAULT_MFI_SOURCE) {

  # Extract the response table for the chosen source and the well information
  raw_exprs <- xponent_mfi_long(lumcsv, mfi_source)
  raw_wells <- lumcsv$AssayData$Wells

  # Join response data with well metadata
  combined <- raw_exprs[, c("Location", "Sample", "Analyte", "Count", "mfi")] %>%
    dplyr::full_join(
      raw_wells[, c("Location", "Sample")],
      by = c("Location", "Sample")
    ) %>%
    janitor::clean_names()

  # Pivot to wide format: one column per analyte with the chosen MFI values
  wide_data <- combined %>%
    dplyr::select(location, sample, analyte, mfi) %>%
    tidyr::pivot_wider(
      id_cols = c(location, sample),
      names_from = analyte,
      values_from = mfi,
      values_fn = list(mfi = dplyr::first)
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
#' Extracts the long-format response data (chosen MFI source + bead count per
#' well per analyte) directly from the parsed xPONENT object, BEFORE any wide
#' pivot. This preserves bead counts that are lost in the wide plate data.
#'
#' @param lumcsv Parsed xPONENT object from moach::read_Xponent_csv()
#' @param file_name Original file name for the source_file column
#' @param mfi_source Which response block becomes assay_response (see
#'   XPONENT_MFI_SOURCES); default "net_mfi"
#' @return Data frame with columns: source_file, well, antigen, feature,
#'         assay_response, assay_bead_count
#'
extract_xponent_assay_response_long <- function(lumcsv, file_name,
                                                mfi_source = XPONENT_DEFAULT_MFI_SOURCE) {

  raw_exprs <- xponent_mfi_long(lumcsv, mfi_source)
  src_column <- XPONENT_MFI_SOURCES[[.xponent_mfi_key(mfi_source)]]$column

  # Build long format directly from the response table
  assay_long <- raw_exprs[, c("Location", "Analyte", "mfi", "Count")] %>%
    dplyr::rename(
      well             = Location,
      antigen          = Analyte,
      assay_response   = mfi,
      assay_bead_count = Count
    ) %>%
    dplyr::mutate(
      source_file      = file_name,
      feature          = src_column,
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
#' @param mfi_source Which response block becomes assay_response for EVERY file
#'   in the batch: "net_mfi" (default), "median" or "avg_net_mfi". A batch is
#'   refused up front if any file lacks the chosen block -- mixing sources
#'   inside one batch (or silently dropping a file) would corrupt the data.
#' @return List with combined_plates, header_list, plate_list,
#'         assay_response_long, mfi_source
#'
process_xponent_files <- function(upload_df, mfi_source = XPONENT_DEFAULT_MFI_SOURCE) {
  mfi_source <- .xponent_mfi_key(mfi_source)
  src_label  <- XPONENT_MFI_SOURCES[[mfi_source]]$label
  cat("Processing", nrow(upload_df), "xPONENT files (MFI source:", src_label, ")...\n")

  lacking <- xponent_scan_upload(upload_df)$lacking[[mfi_source]]
  if (length(lacking))
    stop(sprintf("'%s' is not in: %s. Choose an MFI source that every selected file contains.",
                 src_label, paste(lacking, collapse = ", ")), call. = FALSE)

  results <- lapply(seq_len(nrow(upload_df)), function(i) {
    file_path <- upload_df$datapath[i]
    file_name <- upload_df$name[i]
    cat("  Processing xPONENT:", file_name, "\n")

    tryCatch({
      # Read xPONENT CSV using moach
      lumcsv <- moach::read_Xponent_csv(file_path)

      # Convert to wide plate data
      plate_data <- xponent_to_wide_plate_data(lumcsv, file_name, mfi_source)
      cat("    -> Columns:", ncol(plate_data), ", Rows:", nrow(plate_data), "\n")

      # Extract header metadata
      header_data <- extract_xponent_header(lumcsv, file_name)

      # Extract long-format assay response (preserves bead counts)
      assay_long <- extract_xponent_assay_response_long(lumcsv, file_name, mfi_source)

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
    assay_response_long = assay_response_long,
    mfi_source = mfi_source
  )
}
