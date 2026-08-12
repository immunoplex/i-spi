# =============================================================================
# reader_bead_rbx.R  —  11.10 Assay Import Refactor, Phase 5
# -----------------------------------------------------------------------------
# Registers the bead-array Bio-Plex binary format (.rbx / .srbx) as a third bead
# format. One .rbx = one plate carrying the full analyte panel. Parsing is done
# by the provided pure-R binary parser (rbx_binary_parser.R): parse_rbx() +
# long_dataframe().
#
# Design: the .rbx reader ONLY supplies a new parse_raw. It converts parse_rbx
# output into the SAME seed shape process_xponent_files produces — combined_plates
# (wide) + assay_response_long_override (long, preserves bead counts) + header_list
# — so it flows through the identical, already-tested bead pipeline:
#   make_template  = .bead_make_template  (xPONENT/override pathway of
#                    generate_layout_template)
#   parse_layout   = .bead_parse_layout
#   validate_sheets= .bead_validate_sheets
#   assemble       = .bead_assemble
# Nothing in the contract, backend, or module changes — the payoff of the
# pluggable-reader design.
#
# Depends on: assay_import_contract.R, reader_bead.R (shared .bead_* fns),
#   rbx_binary_parser.R (parse_rbx, long_dataframe), and the kept helpers
#   assign_plate_numbers()/clean_plate_id()/check_and_report_description()
#   [batch_layout_functions.R]. Source AFTER all of those.
# =============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a


# ---- .rbx description builder (order-aware) ---------------------------------
# .rbx_dil_from() / .rbx_strip_ratio() are defined once in reader_bead.R (sourced
# before this file) — no duplicate here. This builder places the parsed dilution
# and the (ratio-stripped, <=15) name into the correct slots of the user's
# element order, so build_plates_map extracts them regardless of how the
# description elements are ordered. For X wells the name goes to the PatientID
# slot; for B/S/C wells to the Source slot; the dilution to the DilutionFactor
# slot; all other slots (TimePeriod, SampleGroupA/B, ...) are left empty.
.rbx_build_description <- function(type_first, name, dil_chr, x_order, bcs_order, delim) {
  n <- length(name)
  out <- character(n)
  for (i in seq_len(n)) {
    ord <- if (!is.na(type_first[i]) && type_first[i] == "X") x_order else bcs_order
    parts <- vapply(ord, function(el) {
      if (el %in% c("PatientID", "Source")) name[i]
      else if (el == "DilutionFactor")      dil_chr[i]
      else ""
    }, character(1))
    while (length(parts) > 1L && !nzchar(parts[length(parts)]))
      parts <- parts[-length(parts)]
    out[i] <- paste(parts, collapse = delim)
  }
  out
}


# ---- Type code from the .rbx sample label / category ------------------------
# Empty wells (no sample assigned) -> NA specimen_type so the assembler drops
# them. Standards/Controls keep their number (S1, C1, ...); Blank -> B; else X.
.rbx_type_from <- function(label, category) {
  vapply(seq_along(label), function(i) {
    lab <- toupper(trimws(as.character(label[i])))
    if (is.na(label[i]) || !nzchar(lab)) return(NA_character_)
    num   <- gsub("[^0-9]", "", lab)
    cat_i <- as.character(category[i])
    if (identical(cat_i, "Standard") || grepl("^S", lab)) return(paste0("S", num))
    if (identical(cat_i, "Control")  || grepl("^C", lab)) return(paste0("C", num))
    if (identical(cat_i, "Blank")    || grepl("^B", lab)) return("B")
    "X"
  }, character(1))
}


# ---- Per-batch reader (mirrors process_xponent_files) -----------------------
process_rbx_files <- function(upload_df, delimiter = "_",
                              x_order = c("PatientID", "DilutionFactor", "TimePeriod"),
                              bcs_order = c("Source", "DilutionFactor")) {
  cat("Processing", nrow(upload_df), ".rbx file(s)...\n")

  results <- lapply(seq_len(nrow(upload_df)), function(i) {
    fp <- upload_df$datapath[i]; fn <- upload_df$name[i]
    cat("  Processing .rbx:", fn, "\n")
    tryCatch({
      doc  <- parse_rbx(fp)
      long <- long_dataframe(doc)
      long <- long[!is.na(long$well) & nzchar(trimws(as.character(long$well))), , drop = FALSE]
      if (!nrow(long)) stop("no wells parsed from .rbx")

      # Canonicalise analyte names the SAME way build_antigen_df does
      # (clean_antigen_label: '/' -> '_', spaces -> '.', other punctuation
      # removed). This strips slashes from every field and makes the response
      # antigen match antigen_list$antigen_abbreviation (no "Fim 2/3" vs
      # "Fim.2_3" mismatch). Applied once here so the wide columns AND the long
      # response both use the clean name.
      long$analyte <- clean_antigen_label(as.character(long$analyte))

      # per-well sample info (one row per well)
      wi <- unique(long[, c("well", "sample_label", "sample_description",
                            "sample_category", "dilution"), drop = FALSE])
      wi$Type <- .rbx_type_from(wi$sample_label, wi$sample_category)

      # Build the Description so the layout template fills specimen_dilution_factor
      # at GENERATION (not just at commit). The .rbx embeds the real dilution as a
      # "1:N" ratio in the description (standards: "Inhouse Ref 1:2952450";
      # diluted samples: "QC1 1:2500"). We split it into a delimiter-joined string
      # with the ratio-stripped, <=15 name in the PatientID/Source slot and N in
      # the DilutionFactor slot, per the user's element order (order-aware).
      # build_plates_map then parses name -> subject_id/source and N ->
      # specimen_dilution_factor, so the downloaded template AND the commit carry
      # the correct standard/sample dilutions with no manual editing, and
      # subject_id never exceeds 15.
      desc <- as.character(wi$sample_description)
      desc <- ifelse(!is.na(desc) & nzchar(trimws(desc)), desc, as.character(wi$sample_label))
      dil  <- .rbx_dil_from(desc)                          # numeric N, or NA
      name <- .rbx_strip_ratio(desc)                       # label without the ratio
      name <- substr(ifelse(is.na(name), "", name), 1, 15) # patientid limit
      dil_chr <- ifelse(is.na(dil), "", format(dil, scientific = FALSE, trim = TRUE))
      wi$Description <- .rbx_build_description(
        substr(as.character(wi$Type), 1, 1), name, dil_chr, x_order, bcs_order, delimiter)

      # wide plate: one column per analyte with median (FI)
      wide <- tidyr::pivot_wider(
        long[, c("well", "analyte", "median"), drop = FALSE],
        id_cols = "well", names_from = "analyte", values_from = "median",
        values_fn = list(median = dplyr::first))

      plate <- merge(wi[, c("well", "Type", "Description")], wide, by = "well", all = TRUE)
      names(plate)[names(plate) == "well"] <- "Well"
      plate$source_file <- fn
      antigen_cols <- setdiff(names(plate), c("source_file", "Well", "Type", "Description"))
      plate <- plate[, c("source_file", "Well", "Type", "Description", antigen_cols), drop = FALSE]

      # header (one row) from metadata; plateid keyed on source_file so it joins
      # consistently with combined_plates below.
      meta <- doc$metadata
      # file_name: the validator requires a full path with separators. The .rbx
      # binary carries the real source path; use it, else synthesise one.
      src_path <- if (!is.null(meta$source_path) &&
                      grepl("[\\\\/]", meta$source_path)) meta$source_path
                  else file.path("rbx", fn)
      # acquisition_date: not decoded from the binary. Fall back to the file's
      # timestamp in an accepted format so validation passes; the user can
      # correct it in the plate_id sheet during template review.
      acq <- tryCatch(format(file.info(fp)$mtime, "%d-%b-%Y %H:%M"),
                      error = function(e) NA_character_)
      header <- data.frame(
        source_file          = fn,
        file_name            = src_path,
        plateid              = clean_plate_id(fn),
        acquisition_date     = acq,
        reader_serial_number = if (!is.null(meta$reader_serial)) meta$reader_serial else NA_character_,
        rp1_pmt_volts        = NA_character_,
        rp1_target           = NA_character_,
        stringsAsFactors     = FALSE)

      # long assay response (preserves bead counts); feature overridden later
      assay_long <- data.frame(
        well             = as.character(long$well),
        antigen          = as.character(long$analyte),
        assay_response   = as.numeric(long$median),
        assay_bead_count = suppressWarnings(as.integer(long$bead_count)),
        source_file      = fn,
        feature          = "Net_MFI",
        stringsAsFactors  = FALSE)

      cat("    -> wells:", nrow(wi), " analytes:", length(antigen_cols),
          " long rows:", nrow(assay_long), "\n")

      # per-well dilution (authoritative from the .rbx; standards carry the
      # curve dilution, samples their dilution). Keyed to join into plates_map.
      dil <- data.frame(
        plateid  = clean_plate_id(fn),
        well     = as.character(wi$well),
        dilution = suppressWarnings(as.numeric(wi$dilution)),
        stringsAsFactors = FALSE)

      list(plate = plate, header = header, assay_long = assay_long,
           dil = dil, file_name = fn)
    }, error = function(e) {
      cat("    x ERROR:", conditionMessage(e), "\n")
      NULL
    })
  })

  results <- Filter(Negate(is.null), results)
  if (length(results) == 0) stop("No valid .rbx files could be processed")

  names(results)      <- sapply(results, `[[`, "file_name")
  header_list         <- lapply(results, `[[`, "header")
  plate_list          <- lapply(results, `[[`, "plate")
  assay_long_list     <- lapply(results, `[[`, "assay_long")
  dil_list            <- lapply(results, `[[`, "dil")

  header_list <- assign_plate_numbers(header_list)

  combined_plates <- dplyr::bind_rows(plate_list)
  if ("source_file" %in% names(combined_plates))
    combined_plates$plateid <- clean_plate_id(combined_plates$source_file)

  assay_response_long <- dplyr::bind_rows(assay_long_list)
  if ("source_file" %in% names(assay_response_long))
    assay_response_long$plateid <- clean_plate_id(assay_response_long$source_file)

  dilution_map <- dplyr::bind_rows(dil_list)
  dilution_map <- dilution_map[!is.na(dilution_map$dilution), , drop = FALSE]

  cat("  -> combined:", nrow(combined_plates), "rows x", ncol(combined_plates),
      "cols; assay_long:", nrow(assay_response_long), "rows; dilutions:",
      nrow(dilution_map), "\n")

  list(combined_plates = combined_plates, header_list = header_list,
       plate_list = plate_list, assay_response_long = assay_response_long,
       dilution_map = dilution_map)
}


# ---- Stage 1: raw -> preview + template seed (xPONENT/override shape) --------
.rbx_parse_raw <- function(files, opts) {
  p <- process_rbx_files(
    files,
    delimiter = if (is.null(opts$delimiter)) "_" else opts$delimiter,
    x_order   = if (is.null(opts$element_order))
                  c("PatientID", "DilutionFactor", "TimePeriod") else opts$element_order,
    bcs_order = if (is.null(opts$bcs_element_order))
                  c("Source", "DilutionFactor") else opts$bcs_element_order)
  desc <- tryCatch(
    check_and_report_description(p$combined_plates, opts$delimiter %||% "_",
                                 required_elements = 3),
    error = function(e) NULL)
  list(
    preview        = p$combined_plates,
    plate_metadata = p$header_list,
    template_seed  = list(
      all_plates                   = p$combined_plates,
      header_list                  = p$header_list,
      assay_response_long_override = p$assay_response_long,
      dilution_map                 = p$dilution_map,
      description_status           = desc
    )
  )
}


# ---- Registration (reuses bead's shared make_template/parse_layout/etc.) -----
local({
  rbx <- new_assay_reader(
    assay = "bead", format_id = "rbx", label = "Bio-Plex (.rbx)",
    accept = c(".rbx", ".srbx"),
    parse_raw       = .rbx_parse_raw,
    make_template   = .bead_make_template,
    parse_layout    = .bead_parse_layout,
    validate_sheets = .bead_validate_sheets,
    assemble        = .bead_assemble
  )
  register_assay_format(rbx)
})
