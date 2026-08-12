# =============================================================================
# assay_import_contract.R  —  11.10 Assay Import Refactor, Phase 0
# -----------------------------------------------------------------------------
# The single interface every assay format reader implements, plus the canonical
# vocabulary the whole import subsystem shares. No behaviour change on its own:
# this file only DEFINES the contract, a format registry, and one shared
# assembler. The backend (assay_import_backend.R) and the generic module
# (assay_import_module.R) build on top of it.
#
# Conventions (see REFACTOR_settings_cascade.md §4): pool-first, project-second,
# named args at call sites. This file is pure R (no DB handle captured); the
# DB-touching seams live in the backend.
#
# ---- The two-stage reader (confirmed design) --------------------------------
# A format reader turns raw instrument files into the shared LAYOUT TEMPLATE and
# back. The layout template (an .xlsx workbook with a fixed sheet set) is the
# human-editable source of truth and the cross-batch consistency check: parse ->
# template -> review/edit -> re-upload -> re-validate -> (if passing) commit.
#
#   Stage 1  parse_raw(files, opts)   -> list(preview, plate_metadata,
#                                             template_seed)
#            make_template(seed, opts)-> path to a downloadable .xlsx template
#   Stage 2  parse_layout(file, opts) -> a SHEETS list (the AI_SHEET_NAMES set,
#                                        INCLUDING assay_response_long)
#            validate_sheets(sheets)  -> a data.frame of issues (0 rows = clean)
#            assemble(sheets, scope)  -> per-table DB frames (see AI_FRAME_NAMES)
#
# The backend commits the assembled frames; it never sees raw instrument files.
# =============================================================================


# ---- Canonical natural keys & column vocabulary -----------------------------

# curve_lookup natural key (from the DDL). feature is PART of the key, so a
# multi-feature experiment yields one curve row per (feature, antigen) — this is
# what makes bead multi-feature "explicit" without any special-casing.
CURVE_LOOKUP_NK <- c(
  "project_id", "study_accession", "experiment_accession",
  "plateid", "plate", "nominal_sample_dilution",
  "source", "wavelength", "antigen", "feature"
)

# The well-level merge key used to join a plates_map row to its measured
# response in assay_response_long. Template/import column names (study_name /
# experiment_name), not DB names — the assembler renames to DB names last.
AI_NATURAL_KEY <- c("study_name", "experiment_name", "plateid", "well")

# The fixed layout-template sheet set. Every assay produces exactly these.
AI_SHEET_NAMES <- c(
  "plate_id", "plates_map", "antigen_list",
  "subject_groups", "timepoint", "assay_response_long"
)

# The per-table DB frames the assembler emits and the backend commits, in
# commit order. header/samples/standards/blanks/controls land in xmap_*;
# antigen_list -> xmap_antigen_family (+ cascade); timepoint_map -> planned visits.
AI_FRAME_NAMES <- c(
  "header", "samples", "standards", "blanks", "controls",
  "antigen_list", "timepoint_map"
)

# Allowlist of real xmap_header columns (union of the bead + ELISA header sets
# used by the legacy commits). insert_to_table() does NOT trim to table columns,
# so the assembler trims header to this set before insert (an unknown column
# would fail dbAppendTable). ELISA-only cols (absorbance_id/wavelengths/
# autoloading_range) are included; they simply don't appear for bead/flow.
AI_HEADER_COLS <- c(
  "study_accession", "experiment_accession", "plate_id", "file_name",
  "acquisition_date", "reader_serial_number", "rp1_pmt_volts", "rp1_target",
  "absorbance_id", "wavelengths", "autoloading_range",
  "auth0_user", "workspace_id", "plateid", "plate", "n_wells",
  "assay_response_variable", "assay_independent_variable",
  "nominal_sample_dilution", "project_id"
)

# antigen_list columns REQUIRED for upload_antigen_family() ->
# prepare_batch_antigen_family(). NOTE feature is required (11.10): bead/flow
# used to omit it and insert inline; the contract forbids that now.
AI_ANTIGEN_LIST_COLS <- c(
  "project_id", "study_name", "experiment_name", "feature",
  "antigen_abbreviation", "antigen_family", "standard_curve_max_concentration",
  "antigen_name", "virus_bacterial_strain", "antigen_source", "catalog_number",
  "l_asy_min_constraint", "l_asy_max_constraint", "l_asy_constraint_method"
)

# The subset of AI_ANTIGEN_LIST_COLS the TEMPLATE sheet must carry. project_id /
# study_name / experiment_name are stamped by the backend and feature is derived
# from the measurements, so those four are excluded. prepare_batch_antigen_family
# errors if any of the rest is absent, so the validator treats a miss as an error.
AI_ANTIGEN_TEMPLATE_COLS <- setdiff(
  AI_ANTIGEN_LIST_COLS,
  c("project_id", "study_name", "experiment_name", "feature")
)

# varchar limits from the curve_lookup / xmap_* DDL, keyed by the DB column name.
# The stage-2 validator flags any value exceeding these so over-length keys are
# corrected in the template rather than truncated silently at insert (a frequent
# cross-batch inconsistency). Keyed by DB name; the validator maps template
# names (study_name/experiment_name) onto these.
AI_COL_LIMITS <- list(
  study_accession         = 15L,
  experiment_accession    = 15L,
  feature                 = 15L,
  wavelength              = 15L,
  antigen                 = 64L,
  source                  = 25L,
  patientid               = 15L,
  plateid                 = 100L,
  plate                   = 40L,
  nominal_sample_dilution = 128L
)

# Text/project sentinels (see cascade doc §2). Real projects are >= 16.
AI_TEXT_WILDCARD <- "__none__"
AI_MIN_PROJECT_ID <- 16L


# ---- Reader specification ---------------------------------------------------

#' Construct and validate one format reader.
#'
#' @param assay       "bead" | "elisa" | "flow"
#' @param format_id   unique within the assay, e.g. "raw", "xponent", "rbx"
#' @param label       human label for the format selector
#' @param accept      file extensions accepted by the upload control
#' @param parse_raw   function(files, opts) -> list(preview, plate_metadata,
#'                     template_seed)
#' @param make_template function(seed, opts) -> path to an .xlsx template
#' @param parse_layout function(file, opts) -> sheets list (AI_SHEET_NAMES)
#' @param validate_sheets function(sheets, opts) -> issues data.frame; defaults
#'                     to the shared validator (validate_layout_sheets).
#' @param assemble    function(sheets, scope, opts) -> frames list
#'                     (AI_FRAME_NAMES); defaults to the shared
#'                     assemble_upload_frames().
#' @param split_experiments function(frames) -> list of per-experiment "units"
#'                     (see backend). Default: one unit, features carried as a
#'                     column. Flow overrides this to split by feature.
#' @return a validated reader (a plain list) suitable for register_assay_format().
new_assay_reader <- function(assay,
                             format_id,
                             label,
                             accept,
                             parse_raw,
                             make_template,
                             parse_layout,
                             validate_sheets   = NULL,
                             assemble          = NULL,
                             split_experiments = NULL,
                             assay_controls    = NULL) {

  stopifnot(
    is.character(assay), length(assay) == 1L,
    is.character(format_id), length(format_id) == 1L,
    is.character(label), length(label) == 1L,
    is.character(accept), length(accept) >= 1L
  )
  for (nm in c("parse_raw", "make_template", "parse_layout")) {
    f <- get(nm)
    if (!is.function(f))
      stop(sprintf("new_assay_reader(%s/%s): '%s' must be a function",
                   assay, format_id, nm), call. = FALSE)
  }

  reader <- list(
    assay             = assay,
    format_id         = format_id,
    label             = label,
    accept            = accept,
    parse_raw         = parse_raw,
    make_template     = make_template,
    parse_layout      = parse_layout,
    validate_sheets   = if (is.null(validate_sheets))   validate_layout_sheets     else validate_sheets,
    assemble          = if (is.null(assemble))          assemble_upload_frames     else assemble,
    split_experiments = if (is.null(split_experiments)) default_split_experiments else split_experiments,
    assay_controls    = assay_controls
  )
  class(reader) <- "assay_reader"
  reader
}

# tiny null-coalescing helper (avoids a hard dependency on rlang's %||%)
`%||%` <- function(a, b) if (is.null(a)) b else a


# ---- Format registry --------------------------------------------------------
# One process-wide registry keyed "assay/format_id". Descriptors read it to
# populate their format selector; the module dispatches on it. Idempotent:
# re-registering the same key overwrites (safe across app reloads / re-sourcing).

.assay_format_registry <- new.env(parent = emptyenv())

register_assay_format <- function(reader) {
  if (!inherits(reader, "assay_reader"))
    stop("register_assay_format(): expected an object from new_assay_reader()",
         call. = FALSE)
  key <- paste(reader$assay, reader$format_id, sep = "/")
  assign(key, reader, envir = .assay_format_registry)
  invisible(reader)
}

get_assay_reader <- function(assay, format_id) {
  key <- paste(assay, format_id, sep = "/")
  if (!exists(key, envir = .assay_format_registry, inherits = FALSE))
    stop(sprintf("get_assay_reader(): no reader registered for '%s'", key),
         call. = FALSE)
  get(key, envir = .assay_format_registry, inherits = FALSE)
}

#' List registered formats, optionally filtered to one assay.
#' @return a data.frame(assay, format_id, label) ordered by assay then label.
list_assay_formats <- function(assay = NULL) {
  keys <- ls(envir = .assay_format_registry)
  if (!length(keys))
    return(data.frame(assay = character(), format_id = character(),
                      label = character(), stringsAsFactors = FALSE))
  rows <- lapply(keys, function(k) {
    r <- get(k, envir = .assay_format_registry, inherits = FALSE)
    data.frame(assay = r$assay, format_id = r$format_id, label = r$label,
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (!is.null(assay)) out <- out[out$assay == assay, , drop = FALSE]
  out <- out[order(out$assay, out$label), , drop = FALSE]
  rownames(out) <- NULL
  out
}

clear_assay_formats <- function() {
  rm(list = ls(envir = .assay_format_registry), envir = .assay_format_registry)
  invisible(NULL)
}


# ---- Shared stage-2 validator -----------------------------------------------
# Structural + length checks on the completed layout sheets. Returns a tidy
# issues frame the module renders (and highlights). 0 rows == clean == uploadable.
# Assay-specific readers can pass their own validate_sheets for extra rules; the
# convention is to rbind onto this base so the shared checks always run.

validate_layout_sheets <- function(sheets, opts = list()) {
  issues <- list()
  add <- function(sheet, severity, column, message) {
    issues[[length(issues) + 1L]] <<- data.frame(
      sheet = sheet, severity = severity, column = column %||% NA_character_,
      message = message, stringsAsFactors = FALSE
    )
  }

  # 1) required sheets present and non-empty
  for (nm in AI_SHEET_NAMES) {
    if (is.null(sheets[[nm]])) {
      add(nm, "error", NA, sprintf("required sheet '%s' is missing", nm))
    } else if (!nrow(sheets[[nm]])) {
      sev <- if (nm %in% c("subject_groups", "timepoint")) "warning" else "error"
      add(nm, sev, NA, sprintf("sheet '%s' has no rows", nm))
    }
  }

  # 2) antigen_list carries the metadata columns prepare_batch_antigen_family()
  #    selects (missing any is fatal there). feature may be absent from the sheet
  #    if it is derivable from the measurements (checked in step 5).
  al <- sheets[["antigen_list"]]
  if (!is.null(al) && nrow(al)) {
    for (m in setdiff(AI_ANTIGEN_TEMPLATE_COLS, names(al)))
      add("antigen_list", "error", m,
          sprintf("antigen_list is missing required column '%s'", m))
  }

  # 3) varchar length limits (map template names -> DB names for lookup)
  name_map <- c(study_name = "study_accession",
                experiment_name = "experiment_accession",
                subject_id = "patientid",
                specimen_source = "source")
  check_lengths <- function(sheet_name) {
    df <- sheets[[sheet_name]]
    if (is.null(df) || !nrow(df)) return(invisible())
    for (col in names(df)) {
      db_col <- if (col %in% names(name_map)) name_map[[col]] else col
      lim <- AI_COL_LIMITS[[db_col]]
      if (is.null(lim)) next
      vals <- as.character(df[[col]])
      over <- which(!is.na(vals) & nchar(vals) > lim)
      if (length(over))
        add(sheet_name, "error", col,
            sprintf("%d value(s) exceed %d chars (e.g. '%s')",
                    length(over), lim, vals[over[1]]))
    }
  }
  for (s in AI_SHEET_NAMES) check_lengths(s)

  # 4) project id sanity (real projects are >= 16)
  al2 <- sheets[["antigen_list"]]
  if (!is.null(al2) && "project_id" %in% names(al2) && nrow(al2)) {
    pid <- suppressWarnings(as.integer(al2$project_id))
    if (any(is.na(pid)) || any(pid < AI_MIN_PROJECT_ID, na.rm = TRUE))
      add("antigen_list", "error", "project_id",
          sprintf("project_id must be an integer >= %d", AI_MIN_PROJECT_ID))
  }

  # 5) antigen <-> measurement reconciliation (feature explicitness, 11.10)
  #    Every antigen family row lands per (antigen, feature); check the two
  #    sides agree so nothing is silently dropped or invented at commit.
  pairs <- antigen_feature_pairs(sheets)
  if (!is.null(al2) && nrow(al2)) {
    key <- if ("antigen_abbreviation" %in% names(al2)) "antigen_abbreviation" else
           if ("antigen" %in% names(al2)) "antigen" else NA_character_
    has_feature_col <- "feature" %in% names(al2) &&
      all(!is.na(al2$feature) & nzchar(trimws(as.character(al2$feature))))
    if (!has_feature_col && !nrow(pairs))
      add("antigen_list", "error", "feature",
          "cannot determine feature(s): antigen_list has no feature column and assay_response_long has no (antigen, feature) rows")
    if (!is.na(key) && nrow(pairs)) {
      listed   <- unique(.ai_clean_antigen(as.character(al2[[key]])))
      measured <- unique(.ai_clean_antigen(pairs$antigen))
      unmeasured <- setdiff(listed, measured)
      unlisted   <- setdiff(measured, listed)
      if (length(unmeasured))
        add("antigen_list", "warning", key,
            sprintf("%d listed antigen(s) have no measured data and will not be imported (e.g. '%s')",
                    length(unmeasured), unmeasured[1]))
      if (length(unlisted))
        add("assay_response_long", "warning", "antigen",
            sprintf("%d measured antigen(s) are not in antigen_list and will import with default metadata (e.g. '%s')",
                    length(unlisted), unlisted[1]))
    }
  }

  if (!length(issues))
    return(data.frame(sheet = character(), severity = character(),
                      column = character(), message = character(),
                      stringsAsFactors = FALSE))
  do.call(rbind, issues)
}

#' Convenience predicate: does an issues frame block upload?
layout_sheets_ok <- function(issues) {
  is.null(issues) || !nrow(issues) || !any(issues$severity == "error")
}

# Shared assay_response_long validator, ported from the legacy
# validate_assay_response_data() (was in import_lumifile.R, retire set) so new
# readers don't depend on a retired file. Assay-agnostic: accepts 'feature' or
# 'antigen' as the analyte column. Returns the legacy list(is_valid, messages,
# warnings); readers bridge it into the issues frame via ai_bridge_result().
ai_validate_assay_response <- function(assay_response_long, antigen_import_list,
                                       plates_map) {
  result <- list(is_valid = TRUE, messages = character(), warnings = character())

  if (is.null(assay_response_long) || nrow(assay_response_long) == 0) {
    result$is_valid <- FALSE
    result$messages <- c(result$messages, "assay_response_long is empty or NULL")
    return(result)
  }

  analyte_col <- if ("feature" %in% names(assay_response_long)) "feature" else "antigen"
  required_cols <- c("plateid", "well", analyte_col, "assay_response")
  missing_cols  <- setdiff(required_cols, names(assay_response_long))
  if (length(missing_cols) > 0) {
    result$is_valid <- FALSE
    result$messages <- c(result$messages,
      paste("assay_response_long missing columns:", paste(missing_cols, collapse = ", ")))
  }

  # Antigen reconciliation: match the response antigens to antigen_list, keyed on
  # the abbreviation (the vocabulary assay_response_long$antigen uses), not the
  # raw plate label — otherwise every bead antigen looks "missing".
  if (!is.null(antigen_import_list) && "antigen" %in% names(assay_response_long)) {
    al_col <- if ("antigen_abbreviation" %in% names(antigen_import_list)) "antigen_abbreviation"
              else if ("antigen" %in% names(antigen_import_list)) "antigen"
              else if ("antigen_label_on_plate" %in% names(antigen_import_list)) "antigen_label_on_plate"
              else NA_character_
    if (!is.na(al_col)) {
      missing_antigens <- setdiff(unique(.ai_clean_antigen(as.character(assay_response_long$antigen))),
                                  unique(.ai_clean_antigen(as.character(antigen_import_list[[al_col]]))))
      if (length(missing_antigens) > 0)
        result$warnings <- c(result$warnings,
          paste(length(missing_antigens), "antigens in response data not in antigen_list"))
    }
  }

  if (!is.null(plates_map) && all(c("plateid", "well") %in% names(plates_map))) {
    resp_w <- unique(paste(assay_response_long$plateid, assay_response_long$well, sep = "|"))
    pm_w   <- unique(paste(plates_map$plateid, plates_map$well, sep = "|"))
    missing_wells <- setdiff(resp_w, pm_w)
    if (length(missing_wells) > 0)
      result$warnings <- c(result$warnings,
        paste(length(missing_wells), "response well positions not in plates_map"))
  }

  na_resp <- sum(is.na(assay_response_long$assay_response))
  if (na_resp > 0)
    result$warnings <- c(result$warnings, paste(na_resp, "rows have NA assay_response"))

  result
}

#' Bridge a legacy validator's list(is_valid, messages, warnings) into rows for
#' the issues frame (messages -> error, warnings -> warning) under one sheet name.
ai_bridge_result <- function(res, sheet, column = NA_character_) {
  rows <- list()
  emit <- function(sev, msgs) for (m in msgs)
    rows[[length(rows) + 1L]] <<- data.frame(
      sheet = sheet, severity = sev, column = column, message = m,
      stringsAsFactors = FALSE)
  if (length(res$messages)) emit("error",   res$messages)
  if (length(res$warnings)) emit("warning", res$warnings)
  if (!length(rows))
    return(data.frame(sheet = character(), severity = character(),
                      column = character(), message = character(),
                      stringsAsFactors = FALSE))
  do.call(rbind, rows)
}


# ---- Shared assembler: sheets -> per-table DB frames ------------------------
# Factors the (previously duplicated) bead + ELISA assembly into one routine:
# for each specimen type, join the plates_map slice to assay_response_long on the
# natural key, apply the column mapping, select the DB columns. feature rides
# through the join, so multi-feature is automatic. Flow reuses this per feature.
#
# Runtime dependencies (in scope once the app sources the helper libraries):
#   create_column_mapping(), apply_column_mapping(df, mapping)   [db_functions.R]
# opts:
#   extra_cols     : character() of extra DB columns to keep (ELISA: "wavelength")
#   subject_merge  : logical, join subject_groups into samples for agroup (bead)
#   response_variable / independent_variable : header assay_* values
#   col_mapping    : optional pre-built mapping (else create_column_mapping())

# Canonicalise antigen names (clean_antigen_label: '/' -> '_', spaces -> '.',
# other punctuation removed). Idempotent; NA passes through. Used by the
# assembler and the validators so the antigen vocabulary is slash-free and
# consistent across every assay.
.ai_clean_antigen <- function(x) {
  if (is.null(x)) return(x)
  tryCatch(clean_antigen_label(as.character(x)), error = function(e) as.character(x))
}

assemble_upload_frames <- function(sheets, scope, opts = list()) {

  # NB: never use %||% on a non-scalar — the app's global %||% is comparison-based
  # (x == "") and throws "'length = N' in coercion to logical(1)" on vectors.
  extra_cols   <- if (is.null(opts$extra_cols)) character() else opts$extra_cols
  do_subject   <- isTRUE(opts$subject_merge)
  resp_var     <- if (is.null(opts$response_variable)) "mfi" else opts$response_variable
  indep_var    <- if (is.null(opts$independent_variable)) "concentration" else opts$independent_variable
  col_mapping  <- if (is.null(opts$col_mapping)) create_column_mapping() else opts$col_mapping
  nk           <- if (is.null(opts$natural_key)) AI_NATURAL_KEY else opts$natural_key

  project_id           <- scope$project_id
  study_accession      <- scope$study
  experiment_accession <- scope$experiment
  user                 <- opts$user %||% scope$user

  assay_response <- sheets[["assay_response_long"]]
  plates_map     <- sheets[["plates_map"]]
  plate_id_sheet <- sheets[["plate_id"]]
  antigen_list   <- sheets[["antigen_list"]]
  subject_map    <- sheets[["subject_groups"]]
  timepoint_map  <- sheets[["timepoint"]]

  if (is.null(assay_response) || is.null(plates_map))
    stop("assemble_upload_frames(): assay_response_long and plates_map are required",
         call. = FALSE)

  # No slashes / consistent antigen vocabulary across ALL assays: canonicalise
  # antigen names the same way build_antigen_df does (clean_antigen_label:
  # '/' -> '_', spaces -> '.', other punctuation removed). Idempotent for names
  # that are already clean, so it does not disturb the working assays; it just
  # guarantees assay_response_long$antigen matches antigen_list$antigen_abbreviation
  # and that no field carries a slash. NA antigens (e.g. flow before threading)
  # pass through unchanged.
  if ("antigen" %in% names(assay_response))
    assay_response$antigen <- .ai_clean_antigen(assay_response$antigen)
  if ("antigen" %in% names(plates_map))
    plates_map$antigen <- .ai_clean_antigen(plates_map$antigen)
  if (!is.null(antigen_list) && "antigen_abbreviation" %in% names(antigen_list))
    antigen_list$antigen_abbreviation <- .ai_clean_antigen(antigen_list$antigen_abbreviation)

  # feature comes from EXACTLY one side of the join to avoid a .x/.y collision:
  # prefer plates_map (bead/ELISA); otherwise carry it from assay_response.
  feature_from_response <- !("feature" %in% names(plates_map))
  resp_extra <- c("antigen", "assay_response", "assay_bead_count", extra_cols)
  if (feature_from_response) resp_extra <- c(resp_extra, "feature")
  # quality metrics live on the response side for flow (pct_agg->pctaggbeads) and
  # on the plates_map side for bead; add them to the response intersect too. No
  # assay carries them on BOTH sides, so this cannot create a .x/.y collision.
  resp_extra <- c(resp_extra, "pctaggbeads", "samplingerrors")
  resp_cols  <- intersect(c(nk, resp_extra), names(assay_response))

  # header (one row per plate) from plate_id sheet
  header <- plate_id_sheet
  if (!is.null(header)) {
    header$workspace_id               <- project_id
    header$project_id                 <- project_id
    header$assay_response_variable    <- resp_var
    header$assay_independent_variable <- indep_var
    if (!is.null(user) && !"auth0_user" %in% names(header)) header$auth0_user <- user
    if ("study_name" %in% names(header))
      names(header)[names(header) == "study_name"] <- "study_accession"
    if ("experiment_name" %in% names(header))
      names(header)[names(header) == "experiment_name"] <- "experiment_accession"
    header <- apply_column_mapping(header, col_mapping)
    if ("plate_filename" %in% names(header))
      names(header)[names(header) == "plate_filename"] <- "file_name"
    if (!"n_wells" %in% names(header) && "number_of_wells" %in% names(header))
      header$n_wells <- header$number_of_wells
    header <- header[, intersect(AI_HEADER_COLS, names(header)), drop = FALSE]
  }

  # build one specimen frame for a given specimen_type prefix ("X","S","B","C")
  build_specimen <- function(prefix) {
    if (is.null(plates_map) || !nrow(plates_map)) return(NULL)
    # NA specimen_type (empty wells) must be excluded explicitly: indexing a
    # data.frame with a logical vector containing NA injects an all-NA row per
    # NA, which then fails the NOT NULL plate_id/well check on insert.
    st   <- substr(as.character(plates_map$specimen_type), 1, 1)
    keep <- !is.na(st) & st == prefix
    smap <- plates_map[keep, , drop = FALSE]
    if (!nrow(smap)) return(NULL)

    if (prefix == "X" && do_subject && !is.null(subject_map) &&
        all(c("study_name", "subject_id") %in% names(smap)) &&
        all(c("study_name", "subject_id") %in% names(subject_map))) {
      smap <- merge(smap, subject_map,
                    by = c("study_name", "subject_id"), all.x = TRUE)
      if (all(c("groupa", "groupb") %in% names(smap)))
        smap$agroup <- ifelse(is.na(smap$groupb), smap$groupa,
                              paste(smap$groupa, smap$groupb, sep = "_"))
    }

    df <- merge(smap, assay_response[, resp_cols, drop = FALSE],
                by = nk, all.x = TRUE)

    if (!"plate_id" %in% names(df) && !is.null(plate_id_sheet) &&
        all(c("plateid", "plate_id") %in% names(plate_id_sheet))) {
      pid <- unique(plate_id_sheet[, c("plateid", "plate_id"), drop = FALSE])
      df  <- merge(df, pid, by = "plateid", all.x = TRUE)
    }

    df$study_accession      <- study_accession
    df$experiment_accession <- experiment_accession
    df$project_id           <- project_id
    df <- apply_column_mapping(df, col_mapping)

    # sampleid must be non-null; fall back to well
    if (!"sampleid" %in% names(df)) df$sampleid <- NA_character_
    empty_sid <- is.na(df$sampleid) | trimws(as.character(df$sampleid)) == ""
    if (any(empty_sid) && "well" %in% names(df))
      df$sampleid[empty_sid] <- df$well[empty_sid]

    keep <- c(
      "project_id", "study_accession", "experiment_accession", "timeperiod",
      "patientid", "plate_id", "well", "stype", "sampleid", "agroup", "source",
      "dilution", "pctaggbeads", "samplingerrors", "antigen", "antibody_mfi",
      "antibody_n", "feature", "plate", "plateid", "nominal_sample_dilution",
      extra_cols
    )
    df <- df[, intersect(keep, names(df)), drop = FALSE]
    if ("antibody_mfi" %in% names(df))
      df$antibody_mfi <- round(as.double(df$antibody_mfi), 4)
    df
  }

  list(
    header       = header,
    samples      = build_specimen("X"),
    standards    = build_specimen("S"),
    blanks       = build_specimen("B"),
    controls     = build_specimen("C"),
    # 11.10 Phase 1: emit a FEATURE-EXPLICIT antigen list — one row per
    # (antigen, feature) actually measured — so bead/flow converge onto the same
    # antigen-family landing as ELISA instead of inserting inline without feature.
    antigen_list = attach_feature_to_antigen_list(
                     antigen_list, antigen_feature_pairs(sheets)),
    timepoint_map = timepoint_map
  )
}


# ---- Phase 1: feature-explicit antigen list ---------------------------------
# The "explicit in features and antigens" transform. Bead/flow historically set
# one scalar feature for the whole batch and inserted antigen-family rows per
# antigen (feature omitted). Multi-feature (isotype) imports need one family row
# per (antigen, feature). The measured reality — the distinct (antigen, feature)
# pairs in assay_response_long — is the source of truth; antigen_list supplies
# the per-antigen metadata.

#' Distinct (antigen, feature) pairs actually present in the measurements.
#' Falls back to standards/plates_map if assay_response_long lacks a column.
antigen_feature_pairs <- function(sheets) {
  src <- sheets[["assay_response_long"]]
  if (is.null(src) || !all(c("antigen", "feature") %in% names(src))) {
    alt <- sheets[["plates_map"]]
    if (!is.null(alt) && all(c("antigen", "feature") %in% names(alt))) {
      src <- alt
    } else {
      return(data.frame(antigen = character(), feature = character(),
                        stringsAsFactors = FALSE))
    }
  }
  p <- unique(data.frame(
    antigen = as.character(src$antigen),
    feature = as.character(src$feature),
    stringsAsFactors = FALSE
  ))
  p <- p[!is.na(p$antigen) & nzchar(trimws(p$antigen)) &
         !is.na(p$feature) & nzchar(trimws(p$feature)), , drop = FALSE]
  rownames(p) <- NULL
  p
}

#' Fan an antigen_list (keyed antigen_abbreviation) out to one row per measured
#' (antigen, feature), carrying the antigen metadata. If antigen_list already
#' has a fully-populated feature column it is trusted and returned unchanged.
attach_feature_to_antigen_list <- function(antigen_list, pairs) {
  if (is.null(antigen_list) || !nrow(antigen_list)) return(antigen_list)

  # already explicit? (every row has a non-empty feature)
  if ("feature" %in% names(antigen_list)) {
    f <- as.character(antigen_list$feature)
    if (all(!is.na(f) & nzchar(trimws(f)))) return(antigen_list)
    antigen_list$feature <- NULL   # partial -> rebuild from pairs
  }

  if (is.null(pairs) || !nrow(pairs)) {
    # no measured features to attach; leave feature absent so the validator /
    # backend surfaces it rather than silently inventing one.
    return(antigen_list)
  }

  key <- if ("antigen_abbreviation" %in% names(antigen_list))
    "antigen_abbreviation" else "antigen"
  merged <- merge(pairs, antigen_list,
                  by.x = "antigen", by.y = key, all.x = TRUE)
  # merge keeps the by.x name ("antigen"); restore the family pipeline's name
  names(merged)[names(merged) == "antigen"] <- "antigen_abbreviation"
  rownames(merged) <- NULL
  merged
}


# ---- Default experiment split -----------------------------------------------
# One unit; features (if several) ride as a column within the single experiment.
# Flow overrides split_experiments to fan out one unit per feature-experiment.

default_split_experiments <- function(frames) {
  list(list(
    experiment   = NULL,   # backend uses scope$experiment
    header       = frames$header,
    samples      = frames$samples,
    standards    = frames$standards,
    blanks       = frames$blanks,
    controls     = frames$controls,
    antigen_list = frames$antigen_list
  ))
}
