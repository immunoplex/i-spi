# =============================================================================
# assay_std_reference_rules.R  --  Standards reference table (pure logic)
# -----------------------------------------------------------------------------
# RBX_DILUTION_AUTHORITATIVE_SOURCE_PLAN.md's "Stage 1.5": for a specimen type
# whose dilution can't be recovered from the instrument file (confirmed, for
# Standards, that the Bio-Plex binary carries no usable value at all -- see
# that plan's Phase 0 Findings) nor from the Description text (no "1:N"
# ratio), the user supplies an explicit label->dilution mapping once per
# experiment. This file is the pure logic: parse/serialize the saved JSON,
# decide which distinct descriptions still need an entry, and merge newly
# entered values into the saved set. The Shiny module
# (assay_std_reference_ui.R) is a thin wrapper around these.
#
# Deliberately keyed on the raw Description TEXT, not a type-code suffix or
# position: .rbx_type_from() (reader_bead_rbx.R) derives e.g. "S1".."S11" by
# stripping non-digits from the instrument's internal sample_label, which
# collapses every Standard on a plate to the bare code "S" if that label has
# no digits at all. An ordinal index isn't a safe key; the text a human can
# actually see is.
#
# Pure base R + jsonlite (already a dependency; assay_shape_rules.R uses it
# the same way for profile save/load). No DB handle, no Shiny.
# Source AFTER assay_well_inventory.R (reuses ai_desc_split/ai_token_class
# where available, with a local fallback so this file cannot fail cryptically
# if sourced alone) and BEFORE assay_std_reference_ui.R.
# =============================================================================

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

AI_STD_REFERENCE_EMPTY <- data.frame(
  description = character(), dilution = numeric(), stringsAsFactors = FALSE)

# ---- Dependency wrappers (mirrors assay_shape_rules.R's .ai_sr_* pattern) ---

.ai_std_split <- function(x, delimiters = c("_", " ", ",", ";")) {
  if (exists("ai_desc_split", mode = "function")) return(ai_desc_split(x, delimiters))
  toks <- strsplit(as.character(x), "[_ ,;]+")[[1]]
  toks <- trimws(toks)
  toks[nzchar(toks)]
}

.ai_std_token_class <- function(toks) {
  if (exists("ai_token_class", mode = "function")) return(ai_token_class(toks))
  # minimal local fallback: only "ratio" matters to this file
  ifelse(grepl("^[0-9][0-9,]*[[:space:]]*[:/][[:space:]]*[0-9][0-9,]*$", toks),
        "ratio", "other")
}


# ---- Parse / serialize -------------------------------------------------------

#' Parse a saved reference value (JSON array of {description, dilution}) back
#' into a data.frame. NULL/NA/empty/malformed input all return zero rows --
#' never an error, since this reads whatever is currently in the DB and a
#' malformed value should surface as "needs re-entry," not a crash.
ai_std_reference_parse <- function(json_text) {
  if (is.null(json_text) || length(json_text) == 0 || is.na(json_text) ||
      !nzchar(trimws(as.character(json_text))))
    return(AI_STD_REFERENCE_EMPTY)
  df <- tryCatch(jsonlite::fromJSON(as.character(json_text), simplifyDataFrame = TRUE),
                error = function(e) NULL)
  if (is.null(df) || !is.data.frame(df) || !nrow(df) ||
      !all(c("description", "dilution") %in% names(df)))
    return(AI_STD_REFERENCE_EMPTY)
  data.frame(
    description = trimws(as.character(df$description)),
    dilution    = suppressWarnings(as.numeric(df$dilution)),
    stringsAsFactors = FALSE)
}

#' Serialize a data.frame(description, dilution) to the JSON array stored in
#' param_character_value. Always a character scalar (never a jsonlite `json`
#' S4 wrapper) so it binds cleanly as a plain DBI parameter.
ai_std_reference_serialize <- function(df) {
  if (is.null(df) || !nrow(df)) return("[]")
  df <- df[, c("description", "dilution"), drop = FALSE]
  as.character(jsonlite::toJSON(df, dataframe = "rows", auto_unbox = TRUE))
}


# ---- Candidate detection (the pre-scan) -------------------------------------

#' Distinct descriptions of `specimen_type` that still need a manually-entered
#' dilution, i.e. NOT already covered by:
#'   1. a type-eligible instrument value (X/C only -- same gate as
#'      ai_resolve_one()'s instrument_dilution; never true for Standards, see
#'      the file header),
#'   2. a parseable "1:N"/"N/M"/bare-ratio token in the text, or
#'   3. an existing saved entry for this experiment.
#'
#' Ratio detection uses the DEFAULT delimiter set regardless of whatever the
#' user eventually configures in Stage 2/3 -- deliberately, because this scan
#' runs BEFORE that configuration exists, and ratio recognition doesn't depend
#' on delimiter choice anyway (":" and "/" are always reserved, never split,
#' by ai_desc_delim_regex() no matter which characters the user later picks).
#'
#' @param inv the well inventory (needs specimen_type, description,
#'   instrument_dilution).
#' @param saved existing reference data.frame(description, dilution) for this
#'   experiment, or NULL/empty if none yet.
#' @return data.frame(description, n_wells), one row per distinct description
#'   still needing an entry. Zero rows means nothing is blocking.
ai_std_reference_candidates <- function(inv, specimen_type, saved = NULL,
                                        delimiters = c("_", " ", ",", ";")) {
  empty <- data.frame(description = character(), n_wells = integer(),
                      stringsAsFactors = FALSE)
  if (is.null(inv) || !nrow(inv)) return(empty)
  sel <- !is.na(inv$specimen_type) & inv$specimen_type == specimen_type
  d <- inv[sel, , drop = FALSE]
  if (!nrow(d)) return(empty)

  desc <- as.character(d$description)
  ok_text <- !is.na(desc) & nzchar(trimws(desc))
  if (!any(ok_text)) return(empty)
  desc <- trimws(desc)

  saved_desc <- if (is.null(saved) || !nrow(saved)) character(0)
                else trimws(as.character(saved$description))

  inst <- if ("instrument_dilution" %in% names(d))
            suppressWarnings(as.numeric(d$instrument_dilution)) else rep(NA_real_, nrow(d))

  distinct_desc <- unique(desc[ok_text])
  needs <- character(0); counts <- integer(0)
  for (s in distinct_desc) {
    in_group <- ok_text & desc == s

    inst_ok <- specimen_type %in% c("X", "C") &&
               all(is.finite(inst[in_group]) & inst[in_group] > 0)
    if (inst_ok) next

    toks <- .ai_std_split(s, delimiters)
    if ("ratio" %in% .ai_std_token_class(toks)) next

    if (s %in% saved_desc) next

    needs  <- c(needs, s)
    counts <- c(counts, sum(in_group))
  }
  if (!length(needs)) return(empty)
  data.frame(description = needs, n_wells = counts, stringsAsFactors = FALSE)
}


# ---- Merge --------------------------------------------------------------

#' Merge newly-entered entries into the saved set, last-write-wins by
#' description. Returns the full merged table, ready to re-serialize and
#' write back whole -- set_setting() replaces param_character_value entirely;
#' there is no partial JSON-field update at the SQL layer, so the merge has to
#' happen here, in R, before every save.
ai_std_reference_merge <- function(saved, new) {
  saved <- if (is.null(saved)) AI_STD_REFERENCE_EMPTY else saved
  new   <- if (is.null(new))   AI_STD_REFERENCE_EMPTY else new
  if (!nrow(new)) return(saved)
  keep <- !(trimws(saved$description) %in% trimws(new$description))
  out <- rbind(saved[keep, , drop = FALSE], new)
  out[order(out$description), , drop = FALSE]
}


# ---- Paste-from-spreadsheet (both UI entry points) --------------------------

#' Parse a two-column table pasted from a spreadsheet (Excel, etc.) into
#' data.frame(description, dilution) -- the same shape ai_std_reference_merge()
#' takes as `new`. One row per line; Excel's clipboard format is tab-delimited,
#' so that is tried first, falling back to runs of 2+ spaces, then a comma --
#' covers a paste that lands as plain text too.
#'
#' A header row ("Standard point" / "Dilution factor" or similar) is detected
#' and dropped automatically when it is the FIRST line and its second field
#' isn't a positive number -- no need to strip it before pasting. A header
#' appearing anywhere else, or any other line that doesn't yield a
#' nonempty label plus a positive number, is skipped (never an error) and
#' listed in the "skipped" attribute of the result so the caller can tell the
#' user what didn't parse.
#'
#' @param text the raw pasted text.
#' @return data.frame(description, dilution), possibly zero rows, with a
#'   "skipped" attribute (character() of the raw lines that didn't parse).
ai_std_reference_parse_pasted <- function(text) {
  empty <- structure(AI_STD_REFERENCE_EMPTY, skipped = character(0))
  if (is.null(text) || !length(text) || is.na(text[1]) ||
      !nzchar(trimws(as.character(text)[1])))
    return(empty)

  lines <- strsplit(as.character(text)[1], "\r\n|\r|\n")[[1]]
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  if (!length(lines)) return(empty)

  split_line <- function(ln) {
    parts <- strsplit(ln, "\t")[[1]]
    if (length(parts) < 2) parts <- strsplit(ln, "[ ]{2,}")[[1]]
    if (length(parts) < 2) parts <- strsplit(ln, ",")[[1]]
    trimws(parts)
  }
  parsed <- lapply(lines, split_line)

  desc <- vapply(parsed, function(r)
    if (length(r) >= 1 && nzchar(r[1])) r[1] else NA_character_, character(1))
  dil_txt <- vapply(parsed, function(r)
    if (length(r) >= 2) r[2] else NA_character_, character(1))
  dil <- suppressWarnings(as.numeric(gsub(",", "", dil_txt)))

  valid <- !is.na(desc) & is.finite(dil) & dil > 0

  is_header_row1 <- length(lines) > 1 && !valid[1] && !is.na(desc[1]) && is.na(dil[1])
  skipped <- lines[!valid]
  if (is_header_row1) skipped <- skipped[skipped != lines[1]]

  out <- data.frame(description = desc[valid], dilution = dil[valid],
                    stringsAsFactors = FALSE)
  structure(out, skipped = skipped)
}

#' Match a pasted (description, dilution) table against the CURRENT candidate
#' descriptions -- the exact Description text read from the well inventory.
#' Exact match first (trimmed, case-insensitive); whatever that leaves
#' unmatched falls back to matching by TRAILING DIGITS (pasted "STD_1"
#' matches candidate description "S1" -- both end in "1"), since the
#' instrument's own label and a lab's spreadsheet convention for the same
#' standard point are rarely written identically, and the join has to happen
#' on something more forgiving than exact text to be useful in practice.
#'
#' @param candidate_desc character() of candidate descriptions, e.g.
#'   ai_std_reference_candidates()'s $description column.
#' @param pasted data.frame(description, dilution) from
#'   ai_std_reference_parse_pasted().
#' @return data.frame(description, dilution, matched_from) aligned 1:1 with
#'   candidate_desc -- dilution is NA_real_ and matched_from is NA_character_
#'   for a candidate the paste didn't cover.
ai_std_reference_match_pasted <- function(candidate_desc, pasted) {
  out <- data.frame(description = candidate_desc, dilution = NA_real_,
                    matched_from = NA_character_, stringsAsFactors = FALSE)
  if (is.null(pasted) || !nrow(pasted) || !length(candidate_desc)) return(out)

  norm <- function(x) tolower(trimws(as.character(x)))
  trailing_num <- function(x) {
    m <- regmatches(x, regexpr("[0-9]+$", x))
    ifelse(nzchar(m), m, NA_character_)
  }

  cand_n <- norm(candidate_desc)
  past_n <- norm(pasted$description)

  # pass 1: exact (trimmed, case-insensitive) match
  idx <- match(cand_n, past_n)
  hit <- !is.na(idx)
  out$dilution[hit]     <- pasted$dilution[idx[hit]]
  out$matched_from[hit] <- pasted$description[idx[hit]]

  # pass 2: trailing-digits match, for whatever pass 1 left uncovered
  remain <- !hit
  if (any(remain)) {
    cand_num <- trailing_num(cand_n[remain])
    past_num <- trailing_num(past_n)
    idx2 <- match(cand_num, past_num)
    hit2 <- !is.na(idx2)
    ri <- which(remain)[hit2]
    out$dilution[ri]     <- pasted$dilution[idx2[hit2]]
    out$matched_from[ri] <- pasted$description[idx2[hit2]]
  }
  out
}
