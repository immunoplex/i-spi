# =============================================================================
# assay_well_inventory.R  --  the normalized, editable well inventory
# -----------------------------------------------------------------------------
# WHY THIS FILE EXISTS
# Every format reader's parse_raw() returns a DIFFERENT preview shape:
#   bead/raw      combined_plates : Well, Type, Description, source_file, plateid
#                                   + wide antigen cols named "Ag (24)"
#   bead/xponent  combined_plates : same keys, numeric antigen cols
#   bead/rbx      combined_plates : same keys, numeric antigen cols
#   elisa         combined_data   : well, stype, description, plateid, plate,
#                                   wavelength, absorbance  (LONG: n rows/well)
#   flow          flowjo_long     : well, Sample_ID, stype, plate, antibody, MFI
#
# Nothing downstream can offer a plate-by-plate editor, a delimiter chooser, or
# a shape classifier against five different shapes. This file collapses them to
# ONE table -- the well inventory -- with exactly one row per (plate, well):
#
#   plate_key      stable within-batch plate identifier (the reader's plateid)
#   plate_label    human label for the plate switcher
#   plate_index    1..n_plates, for prev/next navigation and stable ordering
#   well           normalized well id, UNPADDED ("A1", not "A01")
#   well_raw       the well id exactly as the instrument wrote it
#   row_letter     "A".."P" (or "AA".."AF" for 1536)
#   col_number     integer
#   type_code      specimen type AS WRITTEN ("X", "S3", "C1", "B") -- editable
#   specimen_type  first character of type_code: X | S | B | C | NA (empty well)
#   description    the description field -- editable
#   raw_type_code  what the instrument said, never mutated (for diff/audit)
#   raw_description  ditto
#   source_file    originating file
#   response_hint  mean response across analytes for this well, or NA. Used ONLY
#                  to PROPOSE blank/standard identity; never written to the DB.
#   type_origin    "file" | "proposed" | "user"
#   desc_origin    "file" | "user"
#
# The inventory is the pre-processor's single piece of mutable state. Stage 1
# (plate grid) edits type_code/description; stages 2-4 read it and never change
# it. The layout template is generated from the RESOLVED inventory, so what the
# user confirmed on screen is what lands in the workbook.
#
# Pure base R. No DB handle, no Shiny. Source AFTER the readers (it only needs
# their output shapes, not their functions) and BEFORE assay_plate_grid.R.
# =============================================================================

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

AI_WELL_INVENTORY_COLS <- c(
  "plate_key", "plate_label", "plate_index",
  "well", "well_raw", "row_letter", "col_number",
  "type_code", "specimen_type", "description",
  "raw_type_code", "raw_description",
  "source_file", "response_hint", "type_origin", "desc_origin"
)

AI_SPECIMEN_TYPES <- c("X", "S", "B", "C")

AI_SPECIMEN_LABELS <- c(X = "Samples", S = "Standards", B = "Blanks",
                        C = "Controls", E = "Empty")

# Row letters, matching generate_well_list() in batch_layout_functions.R.
AI_ROW_LETTERS <- c(LETTERS, "AA", "AB", "AC", "AD", "AE", "AF")


# ---- Well id normalization --------------------------------------------------

#' Normalize a well id to the unpadded canonical form used throughout the app.
#'
#' Instruments disagree: xPONENT writes "A1", some ELISA plate maps write "A01",
#' a few write "1(A1)". generate_well_list() produces UNPADDED ids, and
#' build_plates_map joins on them, so a padded id silently fails the join and
#' the well arrives as an all-NA row. Normalize once, here.
ai_normalize_well <- function(x) {
  s <- toupper(trimws(as.character(x)))
  s[is.na(x)] <- NA_character_
  # "1(A1)" / "A1 " / "A-1" -> extract the letter+digit core
  m <- regmatches(s, regexec("([A-Z]{1,2})[[:space:]_-]*0*([0-9]{1,2})", s))
  vapply(seq_along(s), function(i) {
    if (is.na(s[i])) return(NA_character_)
    p <- m[[i]]
    if (length(p) == 3L) paste0(p[2], as.integer(p[3])) else s[i]
  }, character(1))
}

#' Split a normalized well id into its row letter and column number.
ai_well_parts <- function(well) {
  w <- as.character(well)
  row <- sub("^([A-Z]{1,2}).*$", "\\1", w)
  col <- suppressWarnings(as.integer(sub("^[A-Z]{1,2}([0-9]+)$", "\\1", w)))
  bad <- is.na(w) | !grepl("^[A-Z]{1,2}[0-9]+$", w)
  row[bad] <- NA_character_
  col[bad] <- NA_integer_
  data.frame(row_letter = row, col_number = col, stringsAsFactors = FALSE)
}

#' Plate geometry for a well count. Mirrors generate_well_list()'s switch.
ai_plate_dims <- function(n_wells) {
  switch(as.character(n_wells),
    "6" = c(2, 3), "12" = c(3, 4), "24" = c(4, 6), "48" = c(6, 8),
    "96" = c(8, 12), "384" = c(16, 24), "1536" = c(32, 48),
    c(8, 12))
}

# Standard plate sizes, smallest first, for geometry inference.
AI_PLATE_SIZES <- c(6L, 12L, 24L, 48L, 96L, 384L, 1536L)

#' The smallest standard plate size that actually contains every observed well.
#'
#' The declared size (a UI field, or a reader's default) can be too small for
#' the file in hand. That used to fail silently: the inventory is built from the
#' full well list of the declared size, so a 384-well plate read as 96 lost 288
#' wells with no message anywhere. Infer the real size from the wells present
#' and let the caller report the disagreement.
#'
#' @return list(n_wells, rows, cols, upgraded = <lgl>, declared)
ai_infer_plate_size <- function(wells, declared = 96L) {
  dec <- suppressWarnings(as.integer(declared)[1])
  if (is.na(dec)) dec <- 96L
  w <- as.character(wells)
  w <- w[!is.na(w) & grepl("^[A-Z]{1,2}[0-9]+$", w)]
  if (!length(w)) {
    d <- ai_plate_dims(dec)
    return(list(n_wells = dec, rows = d[1], cols = d[2],
                upgraded = FALSE, declared = dec))
  }
  parts <- ai_well_parts(w)
  need_row <- max(match(parts$row_letter, AI_ROW_LETTERS), na.rm = TRUE)
  need_col <- max(parts$col_number, na.rm = TRUE)

  fits <- function(sz) {
    d <- ai_plate_dims(sz)
    d[1] >= need_row && d[2] >= need_col
  }
  cand <- AI_PLATE_SIZES[AI_PLATE_SIZES >= dec]
  hit  <- cand[vapply(cand, fits, logical(1))]
  size <- if (length(hit)) hit[1] else dec
  d <- ai_plate_dims(size)
  list(n_wells = size, rows = d[1], cols = d[2],
       upgraded = !identical(as.integer(size), dec), declared = dec)
}


# ---- Response hint ----------------------------------------------------------

#' Mean numeric response per row across whatever analyte columns are present.
#'
#' Deliberately forgiving: raw bead files store "1234.5 (45)" strings (value +
#' bead count), xPONENT/.rbx store plain numerics. Take the leading number of
#' every candidate column, keep the columns where most rows parse, and average.
#' The result is a magnitude hint for blank detection -- NOT data. It never
#' reaches the template or the database.
.ai_response_hint <- function(df, exclude_cols) {
  cand <- setdiff(names(df), exclude_cols)
  if (!length(cand)) return(rep(NA_real_, nrow(df)))
  keep <- character(0)
  vals <- list()
  for (cl in cand) {
    v <- suppressWarnings(as.numeric(
      sub("^[^0-9.-]*(-?[0-9]*\\.?[0-9]+).*$", "\\1", as.character(df[[cl]]))))
    if (sum(!is.na(v)) >= 0.5 * nrow(df)) { keep <- c(keep, cl); vals[[cl]] <- v }
  }
  if (!length(keep)) return(rep(NA_real_, nrow(df)))
  m <- do.call(cbind, vals)
  rowMeans(m, na.rm = TRUE)
}


# ---- Adapter registry -------------------------------------------------------
# One adapter per assay (not per format): the formats within an assay already
# converge on a common preview shape before this point. An adapter takes the
# reader's parse_raw() result and returns a data.frame with AT LEAST
# plate_key, well, type_code, description, source_file, response_hint.
# Registering the same assay twice overwrites, so re-sourcing is safe.

.ai_inventory_adapters <- new.env(parent = emptyenv())

register_well_inventory_adapter <- function(assay, fn) {
  stopifnot(is.character(assay), length(assay) == 1L, is.function(fn))
  assign(assay, fn, envir = .ai_inventory_adapters)
  invisible(fn)
}

get_well_inventory_adapter <- function(assay) {
  if (!exists(assay, envir = .ai_inventory_adapters, inherits = FALSE))
    stop(sprintf("no well-inventory adapter registered for assay '%s'", assay),
         call. = FALSE)
  get(assay, envir = .ai_inventory_adapters, inherits = FALSE)
}

# first non-NULL, non-empty column from a set of candidate names
.ai_pick_col <- function(df, candidates, default = NA) {
  hit <- intersect(candidates, names(df))
  if (!length(hit)) return(rep(default, nrow(df)))
  as.character(df[[hit[1]]])
}


# ---- Adapter: bead (raw / xponent / rbx) ------------------------------------

.ai_inventory_bead <- function(raw, opts = list()) {
  df <- raw$preview
  if (is.null(df) || !nrow(df)) stop("bead preview is empty", call. = FALSE)

  meta <- c("source_file", "Well", "well", "Type", "type", "Description",
            "description", "plateid", "plate", "plate_number", "Location",
            "Sample", "Outlier", "Analysis", "Notes")

  data.frame(
    plate_key      = .ai_pick_col(df, c("plateid", "plate_number", "plate", "source_file")),
    well_raw       = .ai_pick_col(df, c("Well", "well", "Location")),
    type_code      = .ai_pick_col(df, c("Type", "type"), NA_character_),
    description    = .ai_pick_col(df, c("Description", "description"), NA_character_),
    source_file    = .ai_pick_col(df, "source_file", NA_character_),
    response_hint  = .ai_response_hint(df, meta),
    stringsAsFactors = FALSE)
}


# ---- Adapter: ELISA ---------------------------------------------------------
# combined_data is LONG: one row per (plate, well, wavelength). Collapse to one
# row per well, averaging absorbance across wavelengths for the hint.

.ai_inventory_elisa <- function(raw, opts = list()) {
  df <- raw$preview
  if (is.null(df) || !nrow(df)) stop("ELISA preview is empty", call. = FALSE)

  flat <- data.frame(
    plate_key   = .ai_pick_col(df, c("plateid", "plate_number", "plate")),
    well_raw    = .ai_pick_col(df, c("well", "Well")),
    type_code   = .ai_pick_col(df, c("stype", "SType", "Type"), NA_character_),
    description = .ai_pick_col(df, c("description", "Description", "sample_label"),
                               NA_character_),
    source_file = .ai_pick_col(df, "source_file", NA_character_),
    resp        = suppressWarnings(as.numeric(
                    .ai_pick_col(df, c("absorbance", "assay_response"), NA))),
    stringsAsFactors = FALSE)

  key <- paste(flat$plate_key, flat$well_raw, sep = "\r")
  hint <- tapply(flat$resp, key, function(v) mean(v, na.rm = TRUE))
  first <- !duplicated(key)
  out <- flat[first, setdiff(names(flat), "resp"), drop = FALSE]
  out$response_hint <- as.numeric(hint[key[first]])
  rownames(out) <- NULL
  out
}


# ---- Adapter: flow ----------------------------------------------------------
# flowjo_long is LONG (one row per antibody). Sample_ID IS the description --
# a delimited identity string, exactly what the pre-processor is for. Wiring
# flow through here retires the hardcoded parse_sample_id() delimiter list
# c("-","_") and its fixed 3-part patientid/timepoint/plate assumption.

.ai_inventory_flow <- function(raw, opts = list()) {
  df <- raw$preview
  if (is.null(df) || !nrow(df)) stop("flow preview is empty", call. = FALSE)

  flat <- data.frame(
    plate_key   = .ai_pick_col(df, c("plateid", "plate", "plate_number")),
    well_raw    = .ai_pick_col(df, c("well", "Well")),
    type_code   = .ai_pick_col(df, c("stype", "Type"), NA_character_),
    description = .ai_pick_col(df, c("Sample_ID", "sample_id", "description"),
                               NA_character_),
    source_file = .ai_pick_col(df, "source_file", NA_character_),
    resp        = suppressWarnings(as.numeric(
                    .ai_pick_col(df, c("MFI", "assay_response"), NA))),
    stringsAsFactors = FALSE)

  key <- paste(flat$plate_key, flat$well_raw, sep = "\r")
  hint <- tapply(flat$resp, key, function(v) mean(v, na.rm = TRUE))
  first <- !duplicated(key)
  out <- flat[first, setdiff(names(flat), "resp"), drop = FALSE]
  out$response_hint <- as.numeric(hint[key[first]])
  rownames(out) <- NULL
  out
}

register_well_inventory_adapter("bead",  .ai_inventory_bead)
register_well_inventory_adapter("elisa", .ai_inventory_elisa)
register_well_inventory_adapter("flow",  .ai_inventory_flow)


# ---- Build the inventory ----------------------------------------------------

#' Normalize a reader's parse_raw() result into the well inventory.
#'
#' @param raw      the parse_raw() result (needs $preview).
#' @param assay    "bead" | "elisa" | "flow".
#' @param n_wells  plate size; wells absent from the file are added as EMPTY
#'                 rows so the grid shows the whole plate, not just used wells.
#' @param opts     passed to the adapter.
#' @return a data.frame with AI_WELL_INVENTORY_COLS, ordered plate then well.
ai_well_inventory <- function(raw, assay, n_wells = 96, opts = list()) {
  flat <- get_well_inventory_adapter(assay)(raw, opts)

  flat$well <- ai_normalize_well(flat$well_raw)

  # A reader that knows its own plate size wins over the argument: the .rbx
  # binary reports geometry$n_wells, and trusting a UI default over the file is
  # how 288 wells go missing.
  if (!is.null(raw$n_wells) && !is.na(raw$n_wells)) n_wells <- raw$n_wells
  geo <- ai_infer_plate_size(flat$well, n_wells)
  n_wells <- geo$n_wells
  flat <- flat[!is.na(flat$plate_key) & nzchar(trimws(flat$plate_key)), , drop = FALSE]
  if (!nrow(flat)) stop("no identifiable plates in the uploaded file(s)", call. = FALSE)

  # A duplicate (plate, well) means two source rows claim the same well. Keep
  # the first and report it -- silently collapsing would hide a real layout bug.
  dupe_key <- paste(flat$plate_key, flat$well, sep = "\r")
  n_dupes  <- sum(duplicated(dupe_key))
  flat <- flat[!duplicated(dupe_key), , drop = FALSE]

  # Stable plate order: by first appearance in the file(s).
  plates <- unique(flat$plate_key)
  full_well_list <- .ai_full_well_list(n_wells)

  rows <- lapply(seq_along(plates), function(i) {
    pk  <- plates[i]
    src <- flat[flat$plate_key == pk, , drop = FALSE]
    # every well of the plate, whether the file mentioned it or not
    idx <- match(full_well_list, src$well)
    data.frame(
      plate_key       = pk,
      plate_label     = .ai_plate_label(pk, src$source_file[1], i),
      plate_index     = i,
      well            = full_well_list,
      well_raw        = ifelse(is.na(idx), full_well_list, src$well_raw[idx]),
      type_code       = src$type_code[idx],
      description     = src$description[idx],
      raw_type_code   = src$type_code[idx],
      raw_description = src$description[idx],
      source_file     = src$source_file[1],
      response_hint   = src$response_hint[idx],
      stringsAsFactors = FALSE)
  })

  inv <- do.call(rbind, rows)
  parts <- ai_well_parts(inv$well)
  inv$row_letter <- parts$row_letter
  inv$col_number <- parts$col_number

  inv$type_code   <- .ai_blank_to_na(inv$type_code)
  inv$description <- .ai_blank_to_na(inv$description)
  inv$raw_type_code   <- inv$type_code
  inv$raw_description <- inv$description

  inv$specimen_type <- ai_type_letter(inv$type_code)
  inv$type_origin <- ifelse(is.na(inv$type_code), NA_character_, "file")
  inv$desc_origin <- ifelse(is.na(inv$description), NA_character_, "file")

  inv <- inv[order(inv$plate_index, inv$col_number, inv$row_letter), AI_WELL_INVENTORY_COLS,
             drop = FALSE]
  rownames(inv) <- NULL
  attr(inv, "n_wells")         <- n_wells
  attr(inv, "plate_rows")      <- geo$rows
  attr(inv, "plate_cols")      <- geo$cols
  attr(inv, "plate_upgraded")  <- isTRUE(geo$upgraded)
  attr(inv, "plate_declared")  <- geo$declared
  attr(inv, "assay")           <- assay
  attr(inv, "duplicate_wells") <- n_dupes
  inv
}

.ai_blank_to_na <- function(x) {
  x <- as.character(x)
  x[!is.na(x) & (!nzchar(trimws(x)) | trimws(x) %in% c("NA", "na", "-"))] <- NA_character_
  x
}

.ai_full_well_list <- function(n_wells) {
  d <- ai_plate_dims(n_wells)
  rows <- AI_ROW_LETTERS[seq_len(d[1])]
  cols <- seq_len(d[2])
  g <- expand.grid(row = rows, col = cols, stringsAsFactors = FALSE)
  paste0(g$row, g$col)
}

.ai_plate_label <- function(plate_key, source_file, i) {
  lbl <- plate_key
  if (!is.na(source_file) && nzchar(source_file) && !identical(source_file, plate_key))
    lbl <- sprintf("%s  (%s)", plate_key, source_file)
  sprintf("%d. %s", i, lbl)
}

#' First character of a type code: "S3" -> "S". NA / unknown letters -> NA.
ai_type_letter <- function(type_code) {
  tc <- toupper(substr(trimws(as.character(type_code)), 1L, 1L))
  tc[is.na(type_code) | !(tc %in% AI_SPECIMEN_TYPES)] <- NA_character_
  tc
}

#' The numeric suffix of a type code: "S3" -> "3", "B" -> "". Used for the
#' standard-point / control index the existing parser reads out of Type.
ai_type_suffix <- function(type_code) {
  s <- trimws(as.character(type_code))
  out <- substring(s, 2L)
  out[is.na(s)] <- NA_character_
  out
}


# ---- Editing ----------------------------------------------------------------

#' Apply a type/description edit to specific wells of the inventory.
#'
#' @param inv        the inventory.
#' @param plate_key  which plate (NULL = every plate -- used by "apply to all
#'                   plates" when a batch repeats one layout).
#' @param wells      character() of well ids.
#' @param type_code  new type code, or NULL to leave alone. "" clears the well
#'                   to empty (specimen_type NA).
#' @param description new description, or NULL to leave alone.
#' @return the modified inventory; touched rows get origin "user".
ai_inventory_set <- function(inv, plate_key, wells,
                             type_code = NULL, description = NULL) {
  if (!length(wells)) return(inv)
  sel <- inv$well %in% wells
  if (!is.null(plate_key)) sel <- sel & inv$plate_key %in% plate_key
  if (!any(sel)) return(inv)

  if (!is.null(type_code)) {
    tc <- trimws(as.character(type_code)[1])
    inv$type_code[sel]     <- if (nzchar(tc)) tc else NA_character_
    inv$specimen_type[sel] <- ai_type_letter(inv$type_code[sel])
    inv$type_origin[sel]   <- "user"
  }
  if (!is.null(description)) {
    dsc <- as.character(description)[1]
    inv$description[sel] <- if (!is.na(dsc) && nzchar(trimws(dsc))) dsc else NA_character_
    inv$desc_origin[sel] <- "user"
  }
  inv
}

#' Wells (across the whole batch, or one plate) whose description matches a
#' given string exactly. Powers "apply this fix to every well that says 'blank'".
ai_inventory_matching <- function(inv, description, plate_key = NULL) {
  sel <- !is.na(inv$description) & inv$description == description
  if (!is.null(plate_key)) sel <- sel & inv$plate_key %in% plate_key
  inv[sel, c("plate_key", "well", "type_code", "description"), drop = FALSE]
}


# ---- Summaries and the sample-type contract ---------------------------------

#' Per-plate specimen-type counts, plus batch totals. Drives the grid header
#' and the "can we proceed" gate.
ai_inventory_type_summary <- function(inv) {
  st <- ifelse(is.na(inv$specimen_type), "E", inv$specimen_type)
  tab <- table(factor(inv$plate_key, levels = unique(inv$plate_key)),
               factor(st, levels = c(AI_SPECIMEN_TYPES, "E")))
  out <- as.data.frame.matrix(tab, stringsAsFactors = FALSE)
  out$plate_key <- rownames(out)
  rownames(out) <- NULL
  out[, c("plate_key", AI_SPECIMEN_TYPES, "E"), drop = FALSE]
}

#' Enforce the specimen-type contract PER PLATE.
#'
#' The requirement you stated: every plate must carry standard-curve points (S),
#' test samples (X) and blanks (B). Controls (C) are optional. A plate that is
#' entirely type X has not been resolved yet and is an error, not a warning --
#' proceeding would land unusable standards.
#'
#' @return the standard issues frame: data.frame(sheet, severity, column, message).
ai_inventory_requirements <- function(inv, require_blanks = TRUE) {
  empty <- data.frame(sheet = character(), severity = character(),
                      column = character(), message = character(),
                      stringsAsFactors = FALSE)
  if (is.null(inv) || !nrow(inv)) return(empty)

  s <- ai_inventory_type_summary(inv)
  rows <- list()
  add <- function(sev, col, msg)
    rows[[length(rows) + 1L]] <<- data.frame(
      sheet = "well_inventory", severity = sev, column = col, message = msg,
      stringsAsFactors = FALSE)

  for (i in seq_len(nrow(s))) {
    pk <- s$plate_key[i]
    if (s$X[i] == 0)
      add("error", "specimen_type",
          sprintf("Plate '%s' has no test samples (type X).", pk))
    if (s$S[i] == 0)
      add("error", "specimen_type",
          sprintf("Plate '%s' has no standard-curve points (type S). Assign the standards before continuing.", pk))
    if (require_blanks && s$B[i] == 0)
      add("error", "specimen_type",
          sprintf("Plate '%s' has no blanks (type B).", pk))
    if (s$S[i] > 0 && s$S[i] < 4)
      add("warning", "specimen_type",
          sprintf("Plate '%s' has only %d standard point(s) -- a curve fit needs more.",
                  pk, s$S[i]))
  }

  if (isTRUE(attr(inv, "plate_upgraded")))
    add("warning", "well",
        sprintf("The file(s) contain wells beyond a %d-well plate; the layout is being read as %d wells (%dx%d).",
                attr(inv, "plate_declared") %||% 96L,
                attr(inv, "n_wells") %||% 96L,
                attr(inv, "plate_rows") %||% 8L,
                attr(inv, "plate_cols") %||% 12L))

  nd <- attr(inv, "duplicate_wells") %||% 0L
  if (nd > 0)
    add("warning", "well",
        sprintf("%d duplicate (plate, well) row(s) in the raw file(s); the first was kept.", nd))

  if (!length(rows)) return(empty)
  do.call(rbind, rows)
}

#' TRUE when no plate is "all X" -- i.e. somebody has distinguished the
#' standards and blanks, either in the file or in the grid.
ai_inventory_types_resolved <- function(inv) {
  if (is.null(inv) || !nrow(inv)) return(FALSE)
  !any(ai_inventory_requirements(inv)$severity == "error")
}


# =============================================================================
# SPECIMEN-TYPE INFERENCE  --  the "everything is X" case
# -----------------------------------------------------------------------------
# When a file types every well X, we have to work out which X wells are really
# blanks and which are standards. Three independent signals, each producing
# PROPOSALS with a reason the user can read and accept or reject in the grid.
# Nothing here mutates the inventory: proposals are advisory, always.
#
#   keyword   the description says so ("blank", "PBS", "STD 3", "QC1")
#   response  the well's mean response sits in the bottom cluster (blanks)
#   ladder    a set of wells share an identity stem and differ only by a
#             dilution token, with responses that fall monotonically
#             -- that is a standard curve
#
# Precedence: keyword beats ladder beats response. An explicit label is better
# evidence than a magnitude, and a magnitude alone can't tell a blank from a
# genuinely negative sample -- which is why response-only proposals come out at
# low confidence and are never auto-applied.
# =============================================================================

# Matched against the description, case-insensitively. These are PREFIX-tolerant
# on purpose. Fully anchored patterns ("^qc[0-9]+$") looked tidy and failed on
# every real string, because a real description carries a qualifier and a
# dilution after the label: "QC1 (Low) 1:2500". A trailing \\b also fails there --
# "qc1" has no word boundary between "qc" and "1" -- so the label patterns
# anchor at the START and let the rest of the string follow.
AI_BLANK_PATTERNS <- c(
  "^b(lan)?k?[0-9]*$", "\\bblank", "\\bpbs", "buffer", "diluent",
  "^bg[0-9]*$", "background", "no[ _-]?(serum|sample|ab|antibody)",
  "^neg(ative)?[0-9]*\\b"
)
AI_STANDARD_PATTERNS <- c(
  "^s(td)?[ _-]?[0-9]+\\b", "\\bstd", "standard", "\\bcal(ib)?",
  "\\bref", "curve", "\\bnibsc", "in[ _-]?house"
)
AI_CONTROL_PATTERNS <- c(
  "^q?c[ _-]?[0-9]+\\b", "\\bqc", "\\bctrl", "control", "spike",
  "^pool",
  # parenthesised concentration qualifiers are near-universal on control labels
  # and are strong evidence on their own: "QC2 (High)", "Pool (Mid)"
  "\\((low|high|mid|med|medium)\\)"
)

.ai_match_any <- function(x, patterns) {
  x <- tolower(trimws(as.character(x)))
  out <- rep(FALSE, length(x))
  for (p in patterns)
    out <- out | (!is.na(x) & grepl(p, x, perl = TRUE))
  out
}

#' Propose specimen types for wells whose type is missing or uniformly X.
#'
#' @param inv the inventory.
#' @param blank_quantile wells below this quantile of response, per plate, are
#'   blank candidates on response evidence alone (default bottom 8%).
#' @param min_ladder how many distinct dilution steps make a standard ladder.
#' @return data.frame(plate_key, well, current_type, proposed_type, confidence,
#'   evidence, reason). Empty frame when there is nothing to propose.
ai_propose_specimen_types <- function(inv, blank_quantile = 0.08,
                                      min_ladder = 4L) {
  empty <- data.frame(plate_key = character(), well = character(),
                      current_type = character(), proposed_type = character(),
                      confidence = character(), evidence = character(),
                      reason = character(), stringsAsFactors = FALSE)
  if (is.null(inv) || !nrow(inv)) return(empty)

  # Only consider wells that are occupied and currently undifferentiated:
  # typed X, or typed at all but with no letter we recognise.
  cand <- !is.na(inv$type_code) &
          (is.na(inv$specimen_type) | inv$specimen_type == "X")
  if (!any(cand)) return(empty)

  out <- list()
  push <- function(rows) if (nrow(rows)) out[[length(out) + 1L]] <<- rows

  # ---- 1. keyword evidence ----
  d <- inv$description
  for (spec in list(list("B", AI_BLANK_PATTERNS,    "blank keyword"),
                    list("S", AI_STANDARD_PATTERNS, "standard keyword"),
                    list("C", AI_CONTROL_PATTERNS,  "control keyword"))) {
    hit <- cand & .ai_match_any(d, spec[[2]])
    if (any(hit))
      push(data.frame(
        plate_key = inv$plate_key[hit], well = inv$well[hit],
        current_type = inv$type_code[hit], proposed_type = spec[[1]],
        confidence = "high", evidence = "keyword",
        reason = sprintf("description '%s' matches a %s",
                         inv$description[hit], spec[[3]]),
        stringsAsFactors = FALSE))
  }

  # ---- 2. dilution-ladder evidence (standards) ----
  push(.ai_ladder_proposals(inv, cand, min_ladder))

  # ---- 3. response-magnitude evidence (blanks), per plate ----
  push(.ai_low_response_proposals(inv, cand, blank_quantile))

  if (!length(out)) return(empty)
  prop <- do.call(rbind, out)

  # One proposal per well: keyword > ladder > response.
  rank <- c(keyword = 1L, ladder = 2L, response = 3L)
  prop <- prop[order(rank[prop$evidence]), , drop = FALSE]
  prop <- prop[!duplicated(paste(prop$plate_key, prop$well, sep = "\r")), , drop = FALSE]
  prop <- prop[order(prop$plate_key, prop$well), , drop = FALSE]
  rownames(prop) <- NULL
  prop
}

# =============================================================================
# Standard-curve (ladder) detection
# -----------------------------------------------------------------------------
# The first version stripped every digit run to build a "stem" and then looked
# for a falling response across the varying number. On real data that fired on
# the SAMPLES: descriptions like "70 V1" / "051 V2" all collapse to the stem
# "# v#", the varying number is the VISIT, and antibody level genuinely falls
# with visit -- so it proposed 70 sample wells as standards with
#   "70 wells share the pattern '# v#' over 4 steps with falling response"
# which is both wrong and, at 70 wells, obviously wrong.
#
# Five things distinguish a dilution series from that:
#
#  1. TOKENS, NOT DIGIT RUNS. Classify each token and only ever treat a `ratio`
#     (1:N) or a bare `integer` as a candidate dilution. A `timepoint` token
#     ("V1", "T0", "D7") can never be one, which alone kills the visit case.
#  2. NEVER THE FIRST TOKEN. Position 1 is the subject id by convention, and a
#     numeric subject id ("70 V1") is exactly what got eaten before.
#  3. SIZE. A standard curve is a curve, not a plate. Capped in absolute terms
#     and as a share of the plate's candidate wells.
#  4. SPAN. Serial dilutions span decades: 150 -> 2952450 is 19,683-fold. Visit
#     or replicate numbers 1,2,3,4 span 4-fold and step by exactly 1. Require a
#     real span and reject consecutive runs.
#  5. EVIDENCE. A bare-integer ladder needs a falling response to be proposed at
#     all. Only an explicit 1:N ratio is strong enough on its own -- previously
#     a missing response hint PASSED the trend test, so the weakest case
#     produced the most proposals.
# =============================================================================

AI_LADDER_MAX_WELLS    <- 32L   # absolute cap on one curve
AI_LADDER_MAX_SHARE    <- 0.40  # ... and as a share of the plate's candidates
AI_LADDER_MIN_SPAN     <- 8     # fold-change from lowest to highest step
AI_LADDER_MAX_TREND    <- -0.6  # Spearman rho of response vs dilution

# minimal token split: delimiters WITHOUT ":" or "/", so a 1:N ratio survives
.ai_ladder_tokens <- function(x) {
  if (is.na(x)) return(character(0))
  t <- trimws(strsplit(as.character(x), "[ _,;|\\t-]+", perl = TRUE)[[1]])
  t[nzchar(t)]
}

# ai_token_class() lives in assay_shape_rules.R, which is sourced after this
# file; resolve at call time and fall back to the three classes needed here.
.ai_ladder_class <- function(tok) {
  if (exists("ai_token_class", mode = "function")) return(ai_token_class(tok))
  s <- trimws(as.character(tok)); out <- rep("mixed", length(s))
  out[grepl("^[0-9][0-9,]*[[:space:]]*[:/][[:space:]]*[0-9][0-9,]*$", s)] <- "ratio"
  out[out == "mixed" & grepl("^[0-9][0-9,]*$", s)] <- "integer"
  out[out == "mixed" & grepl("^(v|d|t|m|w)[ _-]?[0-9]+$", tolower(s))] <- "timepoint"
  out
}

# numeric value of a candidate token: the denominator of a ratio, or the integer
.ai_ladder_value <- function(tok, cls) {
  if (identical(cls, "ratio")) {
    p <- strsplit(gsub("[[:space:],]", "", tok), "[:/]")[[1]]
    return(suppressWarnings(as.numeric(p[length(p)])))
  }
  if (identical(cls, "integer")) return(suppressWarnings(as.numeric(gsub(",", "", tok))))
  NA_real_
}

.ai_ladder_proposals <- function(inv, cand, min_ladder) {
  empty <- data.frame(plate_key = character(), well = character(),
                      current_type = character(), proposed_type = character(),
                      confidence = character(), evidence = character(),
                      reason = character(), stringsAsFactors = FALSE)
  idx <- which(cand & !is.na(inv$description))
  if (length(idx) < min_ladder) return(empty)

  # ---- candidate dilution tokens, per well ----
  rows <- list()
  for (k in idx) {
    toks <- .ai_ladder_tokens(inv$description[k])
    if (length(toks) < 2L) next                      # nothing to vary against
    cls <- .ai_ladder_class(toks)
    for (pos in seq_along(toks)) {
      if (pos == 1L) next                            # rule 2: not the subject slot
      if (!cls[pos] %in% c("ratio", "integer")) next # rule 1
      val <- .ai_ladder_value(toks[pos], cls[pos])
      if (is.na(val) || val <= 0) next
      stem <- toks; stem[pos] <- "#"
      rows[[length(rows) + 1L]] <- data.frame(
        row = k, plate_key = inv$plate_key[k], pos = pos,
        stem = paste(stem, collapse = " "), value = val, form = cls[pos],
        stringsAsFactors = FALSE)
    }
  }
  if (!length(rows)) return(empty)
  cnd <- do.call(rbind, rows)

  out <- list()
  for (pk in unique(cnd$plate_key)) {
    on_plate  <- cnd[cnd$plate_key == pk, , drop = FALSE]
    n_cand_wells <- length(unique(on_plate$row))
    grp_key <- paste(on_plate$pos, on_plate$stem, sep = "\r")

    for (g in unique(grp_key)) {
      sel  <- on_plate[grp_key == g, , drop = FALSE]
      sel  <- sel[!duplicated(sel$row), , drop = FALSE]
      wells_n <- nrow(sel)
      vals <- sort(unique(sel$value))

      # rule 3: a curve, not a plate
      if (wells_n > AI_LADDER_MAX_WELLS) next
      if (n_cand_wells > 0 && wells_n / n_cand_wells > AI_LADDER_MAX_SHARE) next
      if (length(vals) < min_ladder) next

      # rule 4: real span, and not a consecutive run (visits / replicates)
      span <- max(vals) / min(vals)
      consecutive <- length(vals) > 1L && all(diff(vals) == 1)
      is_ratio <- any(sel$form == "ratio")
      if (consecutive) next
      if (!is_ratio && span < AI_LADDER_MIN_SPAN) next

      # rule 5: evidence
      h <- inv$response_hint[sel$row]
      trend <- NA_real_
      if (sum(!is.na(h)) >= min_ladder) {
        agg <- tapply(h, sel$value, function(v) mean(v, na.rm = TRUE))
        xs  <- as.numeric(names(agg))
        if (sum(!is.na(agg)) >= min_ladder)
          trend <- suppressWarnings(stats::cor(xs, as.numeric(agg),
                                               method = "spearman",
                                               use = "complete.obs"))
      }
      has_trend <- !is.na(trend) && trend <= AI_LADDER_MAX_TREND
      if (!is_ratio && !has_trend) next        # bare integers need the response

      conf <- if (is_ratio && has_trend) "high"
              else if (is_ratio) "medium"
              else "medium"
      rsn <- sprintf(
        "%d wells differ only at element %d of \"%s\", over %d %s steps spanning %s-fold%s",
        wells_n, sel$pos[1], sub("^.*\r", "", g), length(vals),
        if (is_ratio) "dilution (1:N)" else "numeric",
        format(round(span), big.mark = ",", trim = TRUE),
        if (has_trend) sprintf(" with falling response (rho %.2f)", trend) else "")

      out[[length(out) + 1L]] <- data.frame(
        plate_key = inv$plate_key[sel$row], well = inv$well[sel$row],
        current_type = inv$type_code[sel$row], proposed_type = "S",
        confidence = conf, evidence = "ladder", reason = rsn,
        stringsAsFactors = FALSE)
    }
  }
  if (!length(out)) return(empty)
  do.call(rbind, out)
}

# Blanks sit at the floor. Compare each well to its OWN plate's distribution,
# never the batch's -- plates differ in PMT/gain and a batch-wide threshold
# mislabels whole plates. Low confidence by construction: a very negative
# sample looks identical from here, which is why this never auto-applies.
.ai_low_response_proposals <- function(inv, cand, blank_quantile) {
  empty <- data.frame(plate_key = character(), well = character(),
                      current_type = character(), proposed_type = character(),
                      confidence = character(), evidence = character(),
                      reason = character(), stringsAsFactors = FALSE)
  if (all(is.na(inv$response_hint))) return(empty)

  res <- list()
  for (pk in unique(inv$plate_key)) {
    sel <- cand & inv$plate_key == pk & !is.na(inv$response_hint)
    if (sum(sel) < 12L) next
    h   <- inv$response_hint[sel]
    cut <- suppressWarnings(stats::quantile(h, probs = blank_quantile,
                                            na.rm = TRUE, names = FALSE))
    med <- stats::median(h, na.rm = TRUE)
    # require a real gap: floor wells must be well under the plate median,
    # otherwise "the bottom 8%" is just noise on a flat plate.
    if (!is.finite(cut) || !is.finite(med) || med <= 0 || cut > 0.25 * med) next
    hit <- which(sel)[h <= cut]
    if (!length(hit)) next
    res[[length(res) + 1L]] <- data.frame(
      plate_key = inv$plate_key[hit], well = inv$well[hit],
      current_type = inv$type_code[hit], proposed_type = "B",
      confidence = "low", evidence = "response",
      reason = sprintf("response %.1f is at the floor of plate '%s' (median %.1f)",
                       inv$response_hint[hit], pk, med),
      stringsAsFactors = FALSE)
  }
  if (!length(res)) return(empty)
  do.call(rbind, res)
}

#' Apply chosen proposals to the inventory. Wells updated this way are marked
#' type_origin = "proposed" so the grid can outline them as unconfirmed and the
#' gate can insist a human looked at them.
ai_apply_proposals <- function(inv, proposals, which_rows = NULL) {
  if (is.null(proposals) || !nrow(proposals)) return(inv)
  p <- if (is.null(which_rows)) proposals else proposals[which_rows, , drop = FALSE]
  for (i in seq_len(nrow(p))) {
    sel <- inv$plate_key == p$plate_key[i] & inv$well == p$well[i]
    if (!any(sel)) next
    inv$type_code[sel]     <- p$proposed_type[i]
    inv$specimen_type[sel] <- ai_type_letter(p$proposed_type[i])
    inv$type_origin[sel]   <- "proposed"
  }
  inv
}

#' Confirm every proposed type as user-reviewed (the grid's "accept all" path).
ai_confirm_proposals <- function(inv) {
  sel <- !is.na(inv$type_origin) & inv$type_origin == "proposed"
  inv$type_origin[sel] <- "user"
  inv
}
