# =============================================================================
# reader_flow.R  —  11.10 Assay Import Refactor, Phase 2 (Option A)
# -----------------------------------------------------------------------------
# Registers the post-gating flow-cytometry format (FlowJo .xlsx) onto the assay
# import contract. Flow differs from bead/ELISA in two ways that the contract
# was designed to absorb:
#
#   1. The measured channel is the ISOTYPE, carried as `feature` in
#      assay_response_long; there is no per-well `antigen` column. But flow
#      binding is always to a real target (a whole virus/bacterium, e.g. "PT"),
#      so the antigen IS specified — in antigen_list (antigen_abbreviation),
#      one row per isotype sharing the single antigen. This reader threads that
#      antigen onto the standards/samples so curve_lookup records the real
#      antigen, and REFUSES a missing/blank/'__none__' antigen at validation.
#      `feature` comes from the response side (the shared assembler handles
#      this) and quality metric pct_agg is renamed to pctaggbeads.
#   2. One experiment per feature: exp_accession -> exp_accession_<feature>.
#      Flow overrides split_experiments() to fan the assembled frames out into
#      one commit unit per isotype, tagging that unit's antigen_list with the
#      feature so the (feature-required) antigen-family landing is satisfied.
#      This makes the feature EXPLICIT in the antigen_list column as well as the
#      experiment name (legacy flow encoded it only in the experiment name).
#
# Option A (wrap-then-lift): delegates parsing to existing functions (lift in
# Phase 4, then retire the source):
#   load_flowjo_file(), pivot_flowjo_long()   [flowjo_read_functions.R — RETIRE]
#   generate_flowjo_layout_template()         [generate_flowjo_layout_template.R — RETIRE]
# Delegated but staying (kept libraries):
#   import_layout_file(), check_sheet_names() [batch_layout_functions.R]
#   validate_batch_plate_metadata()           [plate_validator_functions.R]
#
# Depends on: assay_import_contract.R. Source AFTER the contract and kept
# libraries; during transition, after the flow parser files too.
#
# opts: project_id, study, experiment, user; feature_value, n_wells;
#       dilutions_ref (from parse_raw, for pipe-dilution resolution);
#       source_filepath (for the plate_id file_name).
# =============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a


# ---- Stage 1: raw -> preview + template seed --------------------------------

.flow_parse_raw <- function(files, opts) {
  # flow uploads a single workbook; accept the first file if a frame is passed
  path <- if (is.data.frame(files)) files$datapath[1] else
          if (is.list(files)) files$datapath else files
  src_name <- if (is.data.frame(files)) files$name[1] else basename(path)

  # FlowJo workbooks must carry a 'Sheet1' (data) and 'dilutions' tab. Detect a
  # non-FlowJo file (e.g. a Luminex/xPONENT/.rbx bead export whose data sheet is
  # analyte-named) and fail with an actionable message instead of a cryptic
  # "Sheet 'Sheet1' not found".
  present <- tryCatch(readxl::excel_sheets(path), error = function(e) character())
  if (!("Sheet1" %in% present) || !("dilutions" %in% present)) {
    stop(sprintf(
      paste0("This does not look like a FlowJo export. Found sheet(s): %s. ",
             "A FlowJo workbook needs a 'Sheet1' (data) and a 'dilutions' tab. ",
             "If this is a Luminex/bead file (xPONENT/.rbx), use the Bead Array tab."),
      paste(sprintf("'%s'", present), collapse = ", ")), call. = FALSE)
  }

  result    <- load_flowjo_file(path)
  flowjo_df <- result$flowjo_df
  dilutions <- result$dilutions

  flowjo_df$source_file     <- src_name
  flowjo_df$project_id      <- opts$project_id
  flowjo_df$study_name      <- opts$study
  flowjo_df$experiment_name <- opts$experiment %||% NA_character_

  flowjo_long <- pivot_flowjo_long(flowjo_df, dilutions)

  list(
    preview        = flowjo_long,
    plate_metadata = NULL,
    template_seed  = list(
      flowjo_long     = flowjo_long,
      dilutions       = dilutions,
      source_filepath = path
    )
  )
}


# ---- Stage 1: seed -> downloadable .xlsx template ---------------------------

.flow_make_template <- function(seed, opts) {
  out  <- opts$output_file %||% tempfile(fileext = ".xlsx")
  feat <- substr(trimws(opts$feature_value %||% "MFI"), 1, 15)
  if (!nzchar(feat)) feat <- "MFI"

  fl <- seed$flowjo_long
  if (!is.null(opts$experiment)) fl$experiment_name <- opts$experiment

  generate_flowjo_layout_template(
    flowjo_long     = fl,
    dilutions       = seed$dilutions,
    project_id      = opts$project_id,
    study_name      = opts$study,
    experiment_name = opts$experiment,
    output_file     = out,
    source_filepath = seed$source_filepath,
    feature         = feat,
    n_wells         = opts$n_wells %||% 96
  )
  out
}


# ---- Stage 2: completed template -> sheets ----------------------------------

.flow_parse_layout <- function(file, opts) {
  path <- if (is.list(file)) file$datapath else file

  chk <- tryCatch(check_sheet_names(path, exact_match = FALSE),
                  error = function(e) list(valid = FALSE, message = conditionMessage(e)))
  if (isFALSE(chk$valid))
    return(structure(list(), parse_error = chk$message %||% "invalid sheet names"))

  all <- import_layout_file(path)
  if (!isTRUE(all$success))
    return(structure(list(), parse_error = paste(all$messages, collapse = "; ")))

  sheets   <- all$data
  required <- c("plates_map", "plate_id", "antigen_list", "assay_response_long")
  if (!all(required %in% names(sheets)))
    return(structure(list(), parse_error = paste(
      "missing required sheets:",
      paste(setdiff(required, names(sheets)), collapse = ", "))))

  if (!is.null(sheets[["plates_map"]]) && !"project_id" %in% names(sheets[["plates_map"]]))
    sheets[["plates_map"]]$project_id <- opts$project_id
  if (!is.null(sheets[["plate_id"]]) && !"project_id" %in% names(sheets[["plate_id"]]))
    sheets[["plate_id"]]$project_id <- opts$project_id
  sheets
}


# ---- Stage 2: validation ----------------------------------------------------

.flow_validate_sheets <- function(sheets, opts = list()) {
  if (!length(sheets)) {
    pe <- attr(sheets, "parse_error")
    return(data.frame(sheet = "layout_file", severity = "error",
                      column = NA_character_,
                      message = pe %||% "layout file could not be read",
                      stringsAsFactors = FALSE))
  }

  base  <- validate_layout_sheets(sheets, opts)
  extra <- list(base)

  pid <- sheets[["plate_id"]]
  if (!is.null(pid) && nrow(pid)) {
    mres <- tryCatch(
      validate_batch_plate_metadata(plate_metadata = pid, plate_id_data = pid),
      error = function(e) list(is_valid = FALSE, messages = conditionMessage(e)))
    if (is.null(mres$messages) && !is.null(mres$message)) mres$messages <- mres$message
    if (is.null(mres$warnings)) mres$warnings <- character()
    mres$messages <- if (isTRUE(mres$is_valid)) character()
                     else if (is.null(mres$messages)) "metadata invalid" else mres$messages
    extra[[length(extra) + 1L]] <- ai_bridge_result(mres, "plate_id")
  }

  ares <- ai_validate_assay_response(sheets[["assay_response_long"]],
                                     sheets[["antigen_list"]], sheets[["plates_map"]])
  extra[[length(extra) + 1L]] <- ai_bridge_result(ares, "assay_response_long")

  # Flow-specific: binding is always to a real target, so an antigen MUST be
  # specified — refuse blank / '__none__'. (curve_lookup would otherwise default
  # antigen to '__none__', which we do not allow for flow.)
  al <- sheets[["antigen_list"]]
  ag_col <- if (!is.null(al) && "antigen_abbreviation" %in% names(al)) "antigen_abbreviation"
            else if (!is.null(al) && "antigen" %in% names(al)) "antigen" else NA_character_
  vals <- if (!is.na(ag_col)) as.character(al[[ag_col]]) else character()
  bad  <- is.na(vals) | !nzchar(trimws(vals)) | tolower(trimws(vals)) == tolower(AI_TEXT_WILDCARD)
  if (!length(vals) || any(bad))
    extra[[length(extra) + 1L]] <- data.frame(
      sheet = "antigen_list", severity = "error",
      column = if (is.na(ag_col)) "antigen_abbreviation" else ag_col,
      message = "flow antigen must be specified (whole-virus/bacterium target); blank or '__none__' is not allowed",
      stringsAsFactors = FALSE)

  do.call(rbind, extra)
}


# ---- Assemble (shared + flow steps) -----------------------------------------

.flow_assemble <- function(sheets, scope, opts = list()) {
  arl <- sheets[["assay_response_long"]]
  if (!is.null(arl) && "pct_agg" %in% names(arl))
    names(arl)[names(arl) == "pct_agg"] <- "pctaggbeads"
  sheets[["assay_response_long"]] <- arl

  frames <- assemble_upload_frames(
    sheets, scope, modifyList(opts, list(subject_merge = TRUE)))

  # flow pipe-dilution resolution on samples + controls
  frames$samples  <- .flow_resolve_dilution(frames$samples,  opts$dilutions_ref)
  frames$controls <- .flow_resolve_dilution(frames$controls, opts$dilutions_ref)

  # stash the base experiment so split_experiments() can build exp_<feature>
  attr(frames, "base_experiment") <- scope$experiment
  frames
}

# resolve a pipe-separated dilution (e.g. "500|1000") to one numeric per row,
# preferring a (feature, stype) lookup in the dilutions reference; else the first
# value. Ported from flowjo_reader.R resolve_feature_dilution().
.flow_resolve_dilution <- function(df, dil_ref) {
  if (is.null(df) || !nrow(df) || !"dilution" %in% names(df)) return(df)
  if (!any(grepl("|", as.character(df$dilution), fixed = TRUE), na.rm = TRUE)) return(df)
  if (!is.null(dil_ref) && all(c("feature", "stype") %in% names(df)) &&
      all(c("antibody", "stype", "dilution_factor") %in% names(dil_ref))) {
    lkp <- unique(dil_ref[, c("antibody", "stype", "dilution_factor"), drop = FALSE])
    names(lkp) <- c("feature", "stype", ".dil_resolved")
    df <- merge(df, lkp, by = c("feature", "stype"), all.x = TRUE)
    df$dilution <- ifelse(!is.na(df$.dil_resolved), as.numeric(df$.dil_resolved),
                          suppressWarnings(as.numeric(gsub("\\|.*", "", as.character(df$dilution)))))
    df$.dil_resolved <- NULL
  } else {
    df$dilution <- suppressWarnings(as.numeric(gsub("\\|.*", "", as.character(df$dilution))))
  }
  df
}


# ---- Split into one commit unit per feature (isotype) -----------------------

.flow_feature_set <- function(frames) {
  feats <- character()
  for (nm in c("samples", "standards", "controls", "blanks")) {
    df <- frames[[nm]]
    if (!is.null(df) && "feature" %in% names(df))
      feats <- c(feats, as.character(df$feature))
  }
  sort(unique(feats[!is.na(feats) & nzchar(trimws(feats))]))
}

.flow_split_experiments <- function(frames) {
  feats    <- .flow_feature_set(frames)
  base_exp <- attr(frames, "base_experiment")
  al_all   <- frames$antigen_list
  if (!length(feats)) return(default_split_experiments(frames))

  slice_feat <- function(df, feat) {
    if (is.null(df) || !nrow(df) || !"feature" %in% names(df)) return(df)
    out <- df[df$feature == feat, , drop = FALSE]
    rownames(out) <- NULL
    out
  }

  lapply(feats, function(feat) {
    exp_feat <- if (!is.null(base_exp)) paste0(base_exp, "_", feat) else feat

    # antigen_list for this isotype: slice if it already carries feature,
    # otherwise tag it with this feature.
    al <- al_all
    if (!is.null(al) && nrow(al)) {
      if ("feature" %in% names(al)) {
        al <- al[al$feature == feat, , drop = FALSE]
      } else {
        al$feature <- feat
      }
      rownames(al) <- NULL
    }

    # the real antigen for this unit (flow has one target per experiment)
    ag_vals <- if (!is.null(al) && "antigen_abbreviation" %in% names(al))
      unique(as.character(al$antigen_abbreviation)) else character()
    ag_vals <- ag_vals[!is.na(ag_vals) & nzchar(trimws(ag_vals)) &
                       tolower(trimws(ag_vals)) != tolower(AI_TEXT_WILDCARD)]
    antigen_val <- if (length(ag_vals) == 1L) ag_vals else NA_character_

    # thread the antigen onto specimen frames so curve_lookup records it
    # (flow response has no per-well antigen). Only fills when absent/blank/none.
    stamp_ag <- function(df) {
      if (is.null(df) || !nrow(df) || is.na(antigen_val)) return(df)
      if (!"antigen" %in% names(df)) {
        df$antigen <- antigen_val
      } else {
        empty <- is.na(df$antigen) | !nzchar(trimws(as.character(df$antigen))) |
                 tolower(trimws(as.character(df$antigen))) == tolower(AI_TEXT_WILDCARD)
        df$antigen[empty] <- antigen_val
      }
      df
    }

    list(
      experiment   = exp_feat,
      header       = frames$header,          # all plates; backend stamps experiment
      samples      = stamp_ag(slice_feat(frames$samples,   feat)),
      standards    = stamp_ag(slice_feat(frames$standards, feat)),
      blanks       = stamp_ag(slice_feat(frames$blanks,    feat)),
      controls     = stamp_ag(slice_feat(frames$controls,  feat)),
      antigen_list = al
    )
  })
}


# ---- Registration -----------------------------------------------------------

local({
  flow <- new_assay_reader(
    assay = "flow", format_id = "flowjo", label = "Post-gating Flow (FlowJo .xlsx)",
    accept = c(".xlsx", ".xls"),
    parse_raw         = .flow_parse_raw,
    make_template     = .flow_make_template,
    parse_layout      = .flow_parse_layout,
    validate_sheets   = .flow_validate_sheets,
    assemble          = .flow_assemble,
    split_experiments = .flow_split_experiments
  )
  register_assay_format(flow)
})
