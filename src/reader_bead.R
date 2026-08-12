# =============================================================================
# reader_bead.R  —  11.10 Assay Import Refactor, Phase 2 (Option A)
# -----------------------------------------------------------------------------
# Registers the two bead-array formats — Raw File (.xlsx) and xPONENT (.csv) —
# onto the assay import contract. Both share ONE parse_layout / assemble /
# validate; they differ only in the raw parser and whether a long-format
# response override is threaded into the template.
#
# Option A (wrap-then-lift): this reader DELEGATES parsing to existing functions
# rather than reimplementing them. Delegated-in-place (lift into this file in
# Phase 4, then retire the source):
#   process_xponent_files()      [xPonentReader.R          — RETIRE set]
#   generate_layout_template()   [generate_layout_template_ref.R — RETIRE set]
# Delegated but staying (kept shared libraries):
#   process_experiment_files(), import_layout_file(), check_sheet_names(),
#   check_and_report_description(), clean_plate_id()  [batch_layout_functions.R]
#   validate_batch_plate_metadata()                   [plate_validator_functions.R]
#
# Depends on: assay_import_contract.R (new_assay_reader, register_assay_format,
#   validate_layout_sheets, ai_validate_assay_response, ai_bridge_result,
#   assemble_upload_frames, AI_MIN_PROJECT_ID). Source this AFTER the contract
# and the kept libraries.
#
# opts (supplied by the module in Phase 3; sensible fallbacks here):
#   project_id, study, experiment, user      — scope
#   n_wells, delimiter, element_order,
#   bcs_element_order, feature_value          — template controls
#   raw_preview                               — combined_plates, for the
#                                               assay_response_long fallback
# =============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

# Parse the dilution factor N from a "1:N" (or "1/N") ratio embedded in a text
# field (the .rbx puts standard dilutions in specimen_source, sample dilutions
# in subject_id). Returns numeric N, or NA where no ratio is present.
.rbx_dil_from <- function(x) {
  x <- as.character(x)
  vapply(seq_along(x), function(i) {
    if (is.na(x[i])) return(NA_real_)
    m <- regmatches(x[i], regexec("1[[:space:]]*[:/][[:space:]]*([0-9][0-9,]*)", x[i]))[[1]]
    if (length(m) >= 2) suppressWarnings(as.numeric(gsub(",", "", m[2]))) else NA_real_
  }, numeric(1))
}

# Strip a "1:N" ratio token out of a text field (used to clean specimen_source
# and subject_id once the dilution has been extracted).
.rbx_strip_ratio <- function(v) {
  trimws(gsub("[[:space:]]*1[[:space:]]*[:/][[:space:]]*[0-9][0-9,]*", "", as.character(v)))
}


# ---- Stage 1: raw -> preview + template seed --------------------------------

# format_id is baked into each registered reader's parse_raw closure below.
.bead_parse_raw <- function(files, opts, format_id) {
  if (identical(format_id, "xponent")) {
    p <- process_xponent_files(files)
    arl_override <- p$assay_response_long
  } else {
    p <- process_experiment_files(files)
    arl_override <- NULL
  }

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
      assay_response_long_override = arl_override,
      description_status           = desc
    )
  )
}


# ---- Stage 1: seed -> downloadable .xlsx template ---------------------------

.bead_make_template <- function(seed, opts) {
  out <- if (is.null(opts$output_file)) tempfile(fileext = ".xlsx") else opts$output_file
  desc <- seed$description_status
  el  <- if (is.null(opts$element_order))
           c("PatientID", "TimePeriod", "DilutionFactor") else opts$element_order
  bcs <- if (is.null(opts$bcs_element_order))
           c("Source", "DilutionFactor") else opts$bcs_element_order
  generate_layout_template(
    all_plates                   = seed$all_plates,
    study_accession              = opts$study,
    experiment_accession         = opts$experiment,
    n_wells                      = opts$n_wells %||% 96,
    header_list                  = seed$header_list,
    output_file                  = out,
    project_id                   = opts$project_id,
    description_status           = desc,
    delimiter                    = opts$delimiter %||% "_",
    element_order                = el,
    bcs_element_order            = bcs,
    assay_response_long_override = seed$assay_response_long_override,
    feature_value                = opts$feature_value
  )
  out
}


# ---- Stage 2: completed template -> normalized sheets -----------------------

.bead_parse_layout <- function(file, opts) {
  path <- if (is.list(file)) file$datapath else file
  project_id           <- opts$project_id
  study_accession      <- opts$study
  experiment_accession <- opts$experiment

  chk <- tryCatch(check_sheet_names(path, exact_match = FALSE),
                  error = function(e) list(valid = FALSE, message = conditionMessage(e)))
  if (isFALSE(chk$valid))
    return(structure(list(), parse_error = chk$message %||% "invalid sheet names"))

  all <- import_layout_file(path)
  if (!isTRUE(all$success))
    return(structure(list(), parse_error = paste(all$messages, collapse = "; ")))

  sheets <- all$data
  plates_map     <- sheets[["plates_map"]]
  plate_id_sheet <- sheets[["plate_id"]]
  antigen_list   <- sheets[["antigen_list"]]

  # inject project_id where absent
  if (!is.null(plates_map) && !"project_id" %in% names(plates_map))
    plates_map$project_id <- project_id
  if (!is.null(plate_id_sheet) && !"project_id" %in% names(plate_id_sheet))
    plate_id_sheet$project_id <- project_id

  # .rbx dilution injection: the Bio-Plex binary carries an authoritative
  # per-well dilution (standard-curve dilutions for S, sample dilutions for X)
  # that the Description parser can't recover. If opts$dilution_map is supplied
  # (rbx reader only), write it into specimen_dilution_factor by (plateid, well)
  # and force nominal_sample_dilution to be recomputed from it below.
  # .rbx dilution + subject_id normalisation (opts$dilution_map flags the .rbx
  # reader). The Bio-Plex binary embeds the real dilution as a "1:N" ratio in
  # specimen_source (standards, e.g. "Inhouse Ref 1:2952450") and in subject_id
  # (diluted samples, e.g. "QC1 1:2500"); its numeric dilution field is 1 for
  # standards, so the ratio is the authoritative source. We: (1) extract N into
  # specimen_dilution_factor, (2) strip the ratio out of specimen_source and
  # subject_id, (3) truncate subject_id to the patientid limit (15). Then
  # nominal_sample_dilution is force-recomputed below from the X wells only.
  injected_dilution <- FALSE
  if (!is.null(opts$dilution_map) && !is.null(plates_map) && nrow(plates_map)) {
    tryCatch({
      n   <- nrow(plates_map)
      src <- if ("specimen_source" %in% names(plates_map)) as.character(plates_map$specimen_source) else rep(NA_character_, n)
      sid <- if ("subject_id" %in% names(plates_map))      as.character(plates_map$subject_id)      else rep(NA_character_, n)

      src_dil <- .rbx_dil_from(src)
      sid_dil <- .rbx_dil_from(sid)
      dil     <- ifelse(!is.na(src_dil), src_dil, sid_dil)   # prefer source (standards)
      have    <- !is.na(dil)
      if (any(have)) {
        if (!"specimen_dilution_factor" %in% names(plates_map))
          plates_map$specimen_dilution_factor <- NA_character_
        plates_map$specimen_dilution_factor <- as.character(plates_map$specimen_dilution_factor)
        plates_map$specimen_dilution_factor[have] <- as.character(dil[have])
        injected_dilution <- TRUE
        cat(sprintf("[reader_bead] parsed '1:N' dilution into %d well(s) (standards + diluted samples)\n",
                    sum(have)))
      }

      # clean the ratio out of the text fields, then truncate subject_id to 15
      if ("specimen_source" %in% names(plates_map))
        plates_map$specimen_source <- .rbx_strip_ratio(src)
      if ("subject_id" %in% names(plates_map)) {
        sid2 <- .rbx_strip_ratio(sid)
        too_long <- !is.na(sid2) & nchar(sid2) > 15
        if (any(too_long)) {
          sid2[too_long] <- substr(sid2[too_long], 1, 15)
          cat(sprintf("[reader_bead] truncated %d subject_id(s) to 15 chars\n", sum(too_long)))
        }
        plates_map$subject_id <- sid2
      }
    }, error = function(e)
      cat("[reader_bead] .rbx dilution/subject_id normalisation skipped:", conditionMessage(e), "\n"))
  }

  # compute nominal_sample_dilution if absent (ported from the legacy observer),
  # or force a recompute after a dilution injection.
  need_nsd <- injected_dilution ||
              !("nominal_sample_dilution" %in% names(plates_map)) ||
              !("nominal_sample_dilution" %in% names(plate_id_sheet))
  if (need_nsd && !is.null(plates_map)) {
    tryCatch({
    st_nsd <- substr(as.character(plates_map$specimen_type), 1, 1)
    sample_rows <- plates_map[
      !is.na(st_nsd) & st_nsd == "X" &
        !is.na(plates_map$specimen_dilution_factor), , drop = FALSE]
    grp <- intersect(c("project_id", "study_name", "experiment_name", "plate_number"),
                     names(plates_map))
    if (nrow(sample_rows) > 0 && length(grp)) {
      nsd_df <- aggregate(
        stats::reformulate(grp, response = "specimen_dilution_factor"),
        data = sample_rows,
        FUN  = function(x) paste(sort(unique(x)), collapse = "|"))
      names(nsd_df)[names(nsd_df) == "specimen_dilution_factor"] <- "nominal_sample_dilution"
    } else {
      nsd_df <- unique(plates_map[, grp, drop = FALSE])
      nsd_df$nominal_sample_dilution <- "1"
    }
    plates_map     <- .bead_attach_nsd(plates_map, nsd_df, grp)
    plate_id_sheet <- .bead_attach_nsd(plate_id_sheet, nsd_df,
                                       intersect(grp, names(plate_id_sheet)))
    }, error = function(e)
      stop(sprintf("[nsd recompute] %s", conditionMessage(e)), call. = FALSE))
  }

  # assay_response_long: prefer the sheet; else fall back to the raw preview
  arl <- sheets[["assay_response_long"]]
  if (is.null(arl) && !is.null(opts$raw_preview)) {
    arl <- .bead_build_arl_from_preview(opts$raw_preview, plate_id_sheet,
                                        antigen_list, project_id,
                                        study_accession, experiment_accession)
  }

  # Bead feature (isotype) is a per-batch scalar the user types in the module;
  # the template's assay_response_long does not carry it. Inject it here so the
  # sheet has an (antigen, feature) pair for the contract (feature-explicit
  # antigens) and the assembler. Applied to arl (which has antigen) and to
  # plates_map so the assembler sources feature from one consistent side.
  tryCatch({
    fv <- opts$feature_value
    has_fv <- !is.null(fv) && nzchar(trimws(as.character(fv)[1]))
    fv1 <- if (has_fv) trimws(as.character(fv)[1]) else NA_character_
    inject_feature <- function(df) {
      if (is.null(df) || !nrow(df) || !has_fv) return(df)
      if (!"feature" %in% names(df)) { df$feature <- fv1; return(df) }
      empty <- is.na(df$feature) | !nzchar(trimws(as.character(df$feature)))
      df$feature[empty] <- fv1
      df
    }
    arl        <- inject_feature(arl)
    plates_map <- inject_feature(plates_map)
  }, error = function(e)
    stop(sprintf("[feature inject] %s", conditionMessage(e)), call. = FALSE))

  sheets[["plates_map"]]          <- plates_map
  sheets[["plate_id"]]            <- plate_id_sheet
  sheets[["assay_response_long"]] <- arl
  sheets
}

# join a computed nominal_sample_dilution onto a sheet by the available group cols
.bead_attach_nsd <- function(df, nsd_df, grp) {
  if (is.null(df) || !length(grp)) return(df)
  df <- df[, names(df) != "nominal_sample_dilution", drop = FALSE]
  df <- merge(df, nsd_df[, c(grp, "nominal_sample_dilution"), drop = FALSE],
              by = grp, all.x = TRUE)
  df$nominal_sample_dilution[is.na(df$nominal_sample_dilution)] <- "1"
  df
}

# fallback: pivot the wide raw preview to assay_response_long (ported, minimal)
.bead_build_arl_from_preview <- function(combined_plates, plate_id_sheet,
                                         antigen_list, project_id,
                                         study_accession, experiment_accession) {
  all_cols <- names(combined_plates)
  mfi_cols <- grep("\\([0-9]+\\)", all_cols, value = TRUE)
  if (!length(mfi_cols)) return(NULL)
  meta_cols <- intersect(c("source_file", "Well"), all_cols)

  arl <- tidyr::pivot_longer(
    combined_plates[, c(meta_cols, mfi_cols), drop = FALSE],
    cols = tidyselect::all_of(mfi_cols),
    names_to = "antigen_label_on_plate", values_to = "mfi_bead_combined")
  arl$antigen_label_on_plate <- gsub("\\.", " ", arl$antigen_label_on_plate)
  arl$assay_response   <- as.numeric(stringr::str_extract(arl$mfi_bead_combined, "^[0-9.]+"))
  arl$assay_bead_count <- as.numeric(
    stringr::str_extract(arl$mfi_bead_combined, "(?<=\\()[0-9]+(?=\\))"))
  arl$mfi_bead_combined <- NULL
  if ("Well" %in% names(arl)) names(arl)[names(arl) == "Well"] <- "well"

  if ("source_file" %in% names(arl) && !is.null(plate_id_sheet)) {
    arl$plateid <- clean_plate_id(arl$source_file)
    pid <- unique(plate_id_sheet[, intersect(c("plateid", "plate_id", "plate_number"),
                                             names(plate_id_sheet)), drop = FALSE])
    if ("plateid" %in% names(pid)) arl <- merge(arl, pid, by = "plateid", all.x = TRUE)
  }
  if (!is.null(antigen_list) &&
      all(c("antigen_label_on_plate", "antigen_abbreviation") %in% names(antigen_list))) {
    lk <- unique(antigen_list[, c("antigen_label_on_plate", "antigen_abbreviation")])
    arl <- merge(arl, lk, by = "antigen_label_on_plate", all.x = TRUE)
    names(arl)[names(arl) == "antigen_abbreviation"] <- "antigen"
  }
  arl$project_id      <- project_id
  arl$study_name      <- study_accession
  arl$experiment_name <- experiment_accession
  arl
}


# ---- Stage 2: validation ----------------------------------------------------

.bead_validate_sheets <- function(sheets, opts = list()) {
  if (!length(sheets)) {
    pe <- attr(sheets, "parse_error")
    return(data.frame(sheet = "layout_file", severity = "error",
                      column = NA_character_,
                      message = pe %||% "layout file could not be read",
                      stringsAsFactors = FALSE))
  }

  # Run the shared structural validator resiliently: a throw becomes a visible
  # error row (not an opaque "no layout validated") so its message is readable.
  base <- tryCatch(
    validate_layout_sheets(sheets, opts),
    error = function(e) data.frame(
      sheet = "layout_file", severity = "error", column = NA_character_,
      message = paste("validate_layout_sheets failed:", conditionMessage(e)),
      stringsAsFactors = FALSE))

  # bridge the legacy bead validators into the issues frame
  extra <- list(base)
  pid <- sheets[["plate_id"]]
  if (!is.null(pid) && nrow(pid)) {
    # validate_batch_plate_metadata expects DB-name 'file_name' in plate_metadata
    # and compares it to plate_id_data$plate_filename. The template plate_id sheet
    # carries plate_filename (file_name is only applied at commit), so derive a
    # file_name column for the check.
    meta_in <- pid
    if (!"file_name" %in% names(meta_in) && "plate_filename" %in% names(meta_in))
      meta_in$file_name <- meta_in$plate_filename
    if (!is.null(opts$user))    meta_in$currentuser  <- opts$user
    if (!is.null(opts$project_id)) meta_in$workspace_id <- opts$project_id
    mres <- tryCatch(
      validate_batch_plate_metadata(plate_metadata = meta_in, plate_id_data = pid),
      error = function(e) list(is_valid = FALSE, messages = conditionMessage(e)))
    # validate_batch_plate_metadata may use $messages or $message
    if (is.null(mres$messages) && !is.null(mres$message)) mres$messages <- mres$message
    if (is.null(mres$warnings)) mres$warnings <- character()
    mres$messages <- if (isTRUE(mres$is_valid)) character()
                     else if (is.null(mres$messages)) "metadata invalid" else mres$messages
    extra[[length(extra) + 1L]] <- ai_bridge_result(mres, "plate_id")
  }

  arl <- sheets[["assay_response_long"]]
  ares <- tryCatch(
    ai_validate_assay_response(arl, sheets[["antigen_list"]], sheets[["plates_map"]]),
    error = function(e) list(is_valid = FALSE,
                             messages = paste("assay_response check failed:", conditionMessage(e)),
                             warnings = character()))
  extra[[length(extra) + 1L]] <- ai_bridge_result(ares, "assay_response_long")

  do.call(rbind, extra)
}


# ---- Registration -----------------------------------------------------------

.bead_assemble <- function(sheets, scope, opts = list()) {
  # bead does the subject_groups -> agroup merge on samples
  assemble_upload_frames(sheets, scope,
                         modifyList(opts, list(subject_merge = TRUE)))
}

local({
  raw <- new_assay_reader(
    assay = "bead", format_id = "raw", label = "Raw File (.xlsx)",
    accept = c(".xlsx", ".xls"),
    parse_raw     = function(files, opts) .bead_parse_raw(files, opts, "raw"),
    make_template = .bead_make_template,
    parse_layout  = .bead_parse_layout,
    validate_sheets = .bead_validate_sheets,
    assemble      = .bead_assemble
  )
  xponent <- new_assay_reader(
    assay = "bead", format_id = "xponent", label = "xPONENT (.csv)",
    accept = c(".csv"),
    parse_raw     = function(files, opts) .bead_parse_raw(files, opts, "xponent"),
    make_template = .bead_make_template,
    parse_layout  = .bead_parse_layout,
    validate_sheets = .bead_validate_sheets,
    assemble      = .bead_assemble
  )
  register_assay_format(raw)
  register_assay_format(xponent)
})
