# =============================================================================
# reader_elisa.R  —  11.10 Assay Import Refactor, Phase 2 (Option A)
# -----------------------------------------------------------------------------
# Registers the ELISA format (.xlsx) onto the assay import contract. ELISA uses
# the SHARED assembler (assemble_upload_frames) with ELISA options: it carries a
# `wavelength` column, joins response to plates_map on (plateid, well), does no
# subject/agroup merge, and its response variable is "absorbance".
#
# Option A (wrap-then-lift): delegates parsing to existing functions. All ELISA
# parsers/validators live in elisa_reader.R (RETIRE set) — lift into this file in
# Phase 4, then retire the source:
#   process_elisa_files(), generate_elisa_layout_template(),
#   validate_elisa_plate_metadata(), validate_elisa_layout_data()
#
# Depends on: assay_import_contract.R. Source AFTER the contract and the kept
# libraries; during transition, also after elisa_reader.R (for the delegated
# parsers), until Phase 4 lifts them here.
#
# NOTE vs legacy: the old ELISA layout observer cross-joined antigen_list with
# unique(plates_map$feature). We DROP that here — the shared assembler derives a
# feature-explicit antigen list from the MEASURED (antigen, feature) pairs
# (Phase 1), which is stricter and consistent across all three assays.
# =============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a


# ---- Stage 1: raw -> preview + template seed --------------------------------

.elisa_parse_raw <- function(files, opts) {
  p <- process_elisa_files(files)
  desc <- opts$description_status
  list(
    preview        = p$combined_data,
    plate_metadata = p$header_list,
    template_seed  = list(
      combined_data      = p$combined_data,
      plate_map          = p$plate_map,
      header_list        = p$header_list,
      description_status = desc
    )
  )
}


# ---- Stage 1: seed -> downloadable .xlsx template ---------------------------

.elisa_make_template <- function(seed, opts) {
  out <- if (is.null(opts$output_file)) tempfile(fileext = ".xlsx") else opts$output_file
  el  <- if (is.null(opts$element_order))
           c("PatientID", "TimePeriod", "DilutionFactor") else opts$element_order
  bcs <- if (is.null(opts$bcs_element_order))
           c("Source", "DilutionFactor") else opts$bcs_element_order
  generate_elisa_layout_template(
    combined_data        = seed$combined_data,
    plate_map            = seed$plate_map,
    header_list          = seed$header_list,
    study_accession      = opts$study,
    experiment_accession = opts$experiment,
    n_wells              = opts$n_wells %||% 96,
    output_file          = out,
    project_id           = opts$project_id,
    description_status   = seed$description_status,
    delimiter            = opts$delimiter %||% "_",
    element_order        = el,
    bcs_element_order    = bcs
  )
  out
}


# ---- Stage 2: completed template -> normalized sheets -----------------------

.elisa_parse_layout <- function(file, opts) {
  path <- if (is.list(file)) file$datapath else file
  project_id <- opts$project_id
  user       <- opts$user

  present <- tryCatch(readxl::excel_sheets(path),
                      error = function(e) character())
  required <- c("plates_map", "plate_id", "antigen_list", "assay_response_long")
  if (!all(required %in% present))
    return(structure(list(), parse_error = paste(
      "missing required sheets:",
      paste(setdiff(required, present), collapse = ", "))))

  sheets <- list()
  for (s in present)
    sheets[[s]] <- as.data.frame(
      readxl::read_excel(path, sheet = s), stringsAsFactors = FALSE)

  plate_id_sheet <- sheets[["plate_id"]]
  plates_map     <- sheets[["plates_map"]]
  arl            <- sheets[["assay_response_long"]]

  # force assay_response to double (readxl can guess integer)
  if (!is.null(arl) && "assay_response" %in% names(arl))
    arl$assay_response <- round(as.double(arl$assay_response), 4)

  # plate_id metadata
  if (!is.null(plate_id_sheet)) {
    plate_id_sheet$workspace_id <- project_id
    if (!is.null(user)) plate_id_sheet$auth0_user <- user
    if ("plate_filename" %in% names(plate_id_sheet))
      names(plate_id_sheet)[names(plate_id_sheet) == "plate_filename"] <- "file_name"
    if (!"project_id" %in% names(plate_id_sheet)) plate_id_sheet$project_id <- project_id
  }
  if (!is.null(plates_map) && !"project_id" %in% names(plates_map))
    plates_map$project_id <- project_id

  sheets[["plate_id"]]            <- plate_id_sheet
  sheets[["plates_map"]]          <- plates_map
  sheets[["assay_response_long"]] <- arl
  sheets
}


# ---- Stage 2: validation ----------------------------------------------------

.elisa_validate_sheets <- function(sheets, opts = list()) {
  if (!length(sheets)) {
    pe <- attr(sheets, "parse_error")
    return(data.frame(sheet = "layout_file", severity = "error",
                      column = NA_character_,
                      message = pe %||% "layout file could not be read",
                      stringsAsFactors = FALSE))
  }

  base <- validate_layout_sheets(sheets, opts)
  extra <- list(base)

  arl          <- sheets[["assay_response_long"]]
  antigen_list <- sheets[["antigen_list"]]
  plates_map   <- sheets[["plates_map"]]
  elisa_meta   <- sheets[["elisa_metadata"]]

  mres <- tryCatch(
    validate_elisa_plate_metadata(plate_id_data = sheets[["plate_id"]],
                                  assay_response_long = arl),
    error = function(e) list(is_valid = FALSE, messages = conditionMessage(e)))
  mres <- .elisa_norm_result(mres)
  extra[[length(extra) + 1L]] <- ai_bridge_result(mres, "plate_id")

  dres <- tryCatch(
    validate_elisa_layout_data(assay_response_long = arl,
                               antigen_list = antigen_list,
                               plates_map = plates_map,
                               elisa_metadata = elisa_meta),
    error = function(e) list(is_valid = FALSE, messages = conditionMessage(e)))
  dres <- .elisa_norm_result(dres)
  extra[[length(extra) + 1L]] <- ai_bridge_result(dres, "assay_response_long")

  do.call(rbind, extra)
}

# normalise a legacy validator result to list(is_valid, messages, warnings)
.elisa_norm_result <- function(res) {
  if (is.null(res$messages) && !is.null(res$message)) res$messages <- res$message
  if (is.null(res$warnings)) res$warnings <- character()
  res$messages <- if (isTRUE(res$is_valid)) character()
                  else if (is.null(res$messages)) "invalid" else res$messages
  res
}


# ---- Assemble (shared, ELISA options) + registration ------------------------

.elisa_assemble <- function(sheets, scope, opts = list()) {
  assemble_upload_frames(sheets, scope, modifyList(opts, list(
    extra_cols           = "wavelength",
    subject_merge        = FALSE,
    natural_key          = c("plateid", "well"),
    response_variable    = "absorbance",
    independent_variable = "concentration"
  )))
}

local({
  elisa <- new_assay_reader(
    assay = "elisa", format_id = "xlsx", label = "ELISA (.xlsx)",
    accept = c(".xlsx", ".xls"),
    parse_raw       = .elisa_parse_raw,
    make_template   = .elisa_make_template,
    parse_layout    = .elisa_parse_layout,
    validate_sheets = .elisa_validate_sheets,
    assemble        = .elisa_assemble
  )
  register_assay_format(elisa)
})
