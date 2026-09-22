# =============================================================================
# assay_description_parse.R  —  unified description-string parser + validator
# -----------------------------------------------------------------------------
# ONE source of truth for turning the `Description` field into the canonical
# plates_map columns, replacing the three positional copies that drifted apart:
#
#     parse_all_descriptions()          [generate_layout_template_ref.R]
#     parse_description_with_defaults() [batch_layout_functions.R]
#     parse_elisa_descriptions()        [reader_elisa_parsers.R]
#
# and the token-COUNT check (check_description_elements) with a field-AWARE
# validator that enforces the per-type contract:
#
#     X            : PatientID, TimePeriod, DilutionFactor  (+ optional groups)
#     S | B | C    : Source, DilutionFactor
#
# Design (see the three requirements this file exists to satisfy):
#   * flexible / mixed delimiters   -> split on a delimiter CHARACTER CLASS
#                                      (default "_ , ;"; never ":" or "/", which
#                                      belong to a "1:N" / "1/N" dilution ratio)
#   * 1-2 word sources, 1-3 word    -> exactly ONE field per type is "greedy"
#     timeperiods                      (TimePeriod for X, Source for B/S/C) and
#                                      absorbs the surplus tokens; every other
#                                      field is atomic (one token).
#   * dilution = integer denominator -> bare N, or 1:N / 1/N / M:N (take the
#     OR "1:denominator"                denominator); thousands separators ok;
#                                       decimals / non-numeric rejected.
#
# The dilution is used as a CONTENT ANCHOR: it is located by shape, removed,
# then the remaining tokens are laid onto the remaining fields in the configured
# order. dilution_mode = "position" restores strict positional behaviour for
# clean single-token data (backward compatible).
#
# Output contract (matches what flow already emits, so downstream is uniform):
#   subject_id, timepoint_tissue_abbreviation, specimen_dilution_factor,
#   specimen_source, groupa, groupb
#
# Pure base R. Because the three replacements below deliberately shadow the
# legacy functions by name, source this file AFTER the files that define the
# originals (batch_layout_functions.R, generate_layout_template_ref.R,
# reader_elisa_parsers.R) so the drop-ins win — i.e. LAST among the parser
# files, but before the app handles any upload.
# =============================================================================

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

# Which description elements are "greedy" (may span several tokens). Everything
# else is atomic. Keyed by the element labels used in the orderInput widgets.
.AI_DESC_GREEDY <- c("TimePeriod", "Source")


# ---- Delimiter handling -----------------------------------------------------

#' Build a PCRE character-class regex ("[...]+") from a set of delimiters.
#'
#' Accepts either a single string whose characters are each a delimiter
#' (e.g. "_" or "_ ,;") or a character vector of single characters. ":" and "/"
#' are stripped if present — they are reserved for the "1:N"/"1/N" dilution form
#' and must never split a token. Runs of delimiters collapse (the "+").
ai_desc_delim_regex <- function(delimiters = c("_", " ", ",", ";")) {
  chars <- unique(unlist(strsplit(paste0(as.character(delimiters), collapse = ""), "")))
  chars <- chars[nzchar(chars)]
  chars <- setdiff(chars, c(":", "/"))          # reserved for dilution ratios
  if (!length(chars)) chars <- "_"
  esc <- vapply(chars, function(c)
    if (grepl("[[:alnum:]]", c)) c else paste0("\\", c), character(1))
  paste0("[", paste0(esc, collapse = ""), "]+")
}

#' Split one description into trimmed, non-empty tokens on the delimiter class.
ai_desc_split <- function(x, delimiters = c("_", " ", ",", ";")) {
  if (length(x) != 1L || is.na(x)) return(character(0))
  rx <- if (length(delimiters) == 1L && grepl("^\\[", delimiters))
    delimiters else ai_desc_delim_regex(delimiters)
  toks <- strsplit(as.character(x), rx, perl = TRUE)[[1]]
  toks <- trimws(toks)
  toks[nzchar(toks)]
}


# ---- Dilution: integer denominator ------------------------------------------

#' Parse a dilution token to its integer denominator.
#'
#' "100" -> 100 (bare); "1:100" / "1/100" -> 100 (ratio); "2:100" -> 100
#' (denominator of an M:N ratio); commas tolerated ("1:2,952,450" -> 2952450).
#' Decimals or anything non-numeric -> ok = FALSE.
#'
#' @return list(value = <integer|NA>, ok = <logical>, form = "ratio"|"bare"|NA)
ai_parse_dilution <- function(x) {
  if (length(x) != 1L || is.na(x)) return(list(value = NA_integer_, ok = FALSE, form = NA_character_))
  s <- trimws(as.character(x))
  if (!nzchar(s)) return(list(value = NA_integer_, ok = FALSE, form = NA_character_))

  m <- regmatches(s, regexec("^([0-9][0-9,]*)[[:space:]]*[:/][[:space:]]*([0-9][0-9,]*)$", s))[[1]]
  if (length(m) == 3L) {
    den <- suppressWarnings(as.integer(gsub(",", "", m[3])))
    return(list(value = den, ok = !is.na(den), form = "ratio"))
  }
  if (grepl("^[0-9][0-9,]*$", s)) {
    v <- suppressWarnings(as.integer(gsub(",", "", s)))
    return(list(value = v, ok = !is.na(v), form = "bare"))
  }
  list(value = NA_integer_, ok = FALSE, form = NA_character_)
}

.ai_is_dil <- function(tok) isTRUE(ai_parse_dilution(tok)$ok)


# ---- Token -> field allocation ("one greedy field") -------------------------

#' Lay `tokens` onto `fields` (in order). The single greedy field (TimePeriod or
#' Source) absorbs any surplus; all other fields take one token. Atomic fields
#' left of the greedy one fill from the start, atomic fields to the right fill
#' from the end, so the greedy field is whatever remains in the middle.
#' @return named list(field = value_string); missing values are "".
.ai_allocate <- function(tokens, fields) {
  k <- length(fields); n <- length(tokens)
  vals <- setNames(as.list(rep("", k)), fields)
  if (k == 0L || n == 0L) return(vals)

  g <- which(fields %in% .AI_DESC_GREEDY)
  if (!length(g)) {                                  # no greedy field: positional
    for (i in seq_len(min(k, n))) vals[[i]] <- tokens[i]
    if (n > k) vals[[k]] <- paste(tokens[k:n], collapse = " ")
    return(vals)
  }
  g <- g[1]
  left  <- seq_len(g - 1L)
  right <- if (g < k) (g + 1L):k else integer(0)
  nl <- length(left); nr <- length(right)

  li <- 0L
  for (idx in left) { li <- li + 1L; if (li <= n) vals[[idx]] <- tokens[li] }
  ri <- 0L
  for (idx in rev(right)) { pos <- n - ri; ri <- ri + 1L; if (pos > nl) vals[[idx]] <- tokens[pos] }
  lo <- nl + 1L; hi <- n - nr
  vals[[g]] <- if (hi >= lo) paste(tokens[lo:hi], collapse = " ") else ""
  vals
}

#' Assign a token vector to an ordered element list, using the dilution as a
#' content anchor when possible.
#'
#' @param dilution_mode "auto" (content anchor, positional fallback), "content"
#'   (anchor only), or "position" (strict positional).
#' @return list(values = named-by-element, dilution = list(value, ok, form),
#'              dilution_src = "content"|"position"|"none", ambiguous = logical)
ai_desc_assign <- function(tokens, order, dilution_mode = c("position", "auto", "content")) {
  dilution_mode <- match.arg(dilution_mode)
  has_dil <- "DilutionFactor" %in% order
  ambiguous <- FALSE
  anchor <- NA_integer_

  if (has_dil && dilution_mode %in% c("auto", "content") && length(tokens)) {
    cand <- which(vapply(tokens, .ai_is_dil, logical(1)))
    if (length(cand)) {
      forms <- vapply(tokens[cand], function(t) ai_parse_dilution(t)$form, character(1))
      ratio <- cand[forms == "ratio"]
      pick_from <- if (length(ratio)) ratio else cand   # prefer 1:N over bare
      if (length(pick_from) > 1L) ambiguous <- TRUE
      anchor <- pick_from[length(pick_from)]            # last occurrence
    }
  }

  if (!is.na(anchor)) {
    dil_tok <- tokens[anchor]
    vals <- .ai_allocate(tokens[-anchor], setdiff(order, "DilutionFactor"))
    vals[["DilutionFactor"]] <- dil_tok
    src <- "content"
  } else {
    vals <- .ai_allocate(tokens, order)
    src  <- if (has_dil) "position" else "none"
    dil_tok <- if (has_dil) vals[["DilutionFactor"]] else NA_character_
  }

  # normalise: every element in `order` present as a scalar string
  for (el in order) if (is.null(vals[[el]])) vals[[el]] <- ""
  dil <- ai_parse_dilution(if (length(dil_tok)) dil_tok else NA_character_)
  list(values = vals, dilution = dil, dilution_src = src, ambiguous = ambiguous)
}


# ---- One row -> canonical fields + issues -----------------------------------

.ai_issue_df <- function(field, severity, message, kind) {
  if (!length(field)) return(data.frame(field = character(), severity = character(),
                                         message = character(), kind = character(),
                                         stringsAsFactors = FALSE))
  data.frame(field = field, severity = severity, message = message, kind = kind,
             stringsAsFactors = FALSE)
}

#' Parse a single (Type, Description) into the canonical fields and per-row issues.
#'
#' @param type e.g. "X", "X12", "B", "S3", "C1"
#' @param use_defaults fill missing atomic fields (subject/time) and default the
#'   dilution to 1; issues are still reported so the caller can surface them.
#' @return list(subject_id, groupa, groupb, timeperiod, dilution_factor,
#'              source, type_char, dilution_ok, issues)
ai_parse_description_row <- function(type, description,
                                     element_order     = c("PatientID", "TimePeriod", "DilutionFactor"),
                                     bcs_element_order = c("Source", "DilutionFactor"),
                                     delimiters        = c("_", " ", ",", ";"),
                                     dilution_mode     = c("position", "auto", "content"),
                                     use_defaults      = TRUE) {
  dilution_mode <- match.arg(dilution_mode)

  tc <- if (is.na(type) || !nzchar(trimws(as.character(type)))) NA_character_
        else substr(trimws(as.character(type)), 1L, 1L)

  out <- list(subject_id = "", groupa = "", groupb = "", timeperiod = "",
              dilution_factor = NA_integer_, source = "", type_char = tc,
              dilution_ok = FALSE,
              issues = .ai_issue_df(character(), character(), character(), character()))

  blank <- is.na(description) || !nzchar(trimws(as.character(description))) ||
           identical(trimws(as.character(description)), "NA")

  # ---- empty well / unknown type ----
  if (is.na(tc)) { out$dilution_factor <- 1L; out$dilution_ok <- TRUE; return(out) }
  if (!tc %in% c("X", "S", "B", "C")) {
    out$subject_id <- "0"; out$dilution_factor <- 1L; out$dilution_ok <- TRUE
    return(out)
  }

  iss <- list()
  add <- function(field, severity, message, kind)
    iss[[length(iss) + 1L]] <<- .ai_issue_df(field, severity, message, kind)

  if (blank)
    add(NA_character_, "warning",
        sprintf("Type %s well has a blank Description.", tc), "blank_description")

  if (tc == "X") {
    order <- element_order
    a <- if (blank) NULL else ai_desc_assign(ai_desc_split(description, delimiters), order, dilution_mode)
    v <- if (is.null(a)) NULL else a$values

    out$subject_id <- (v[["PatientID"]]    %||% "")
    out$timeperiod <- (v[["TimePeriod"]]   %||% "")
    out$groupa     <- (v[["SampleGroupA"]] %||% "")
    out$groupb     <- (v[["SampleGroupB"]] %||% "")
    out$source     <- "sample"

    if (!is.null(a)) {
      out$dilution_factor <- a$dilution$value
      out$dilution_ok     <- isTRUE(a$dilution$ok)
    }

    # element-set-driven contract: only the identity elements the user placed in
    # the order are required to be non-empty. DilutionFactor is NOT hard-checked
    # here — a missing/unparseable dilution is resolved by ai_dilution_plan()
    # (parsed / constant / per-description), never a blocking error.
    if (!blank) {
      field_of <- c(PatientID = "subject_id", TimePeriod = "timeperiod",
                    SampleGroupA = "groupa", SampleGroupB = "groupb")
      for (el in setdiff(order, "DilutionFactor")) {
        fld <- field_of[[el]]
        if (!is.null(fld) && !nzchar(out[[fld]] %||% ""))
          add(el, "error", sprintf("Type X: %s is empty.", el), "missing_required")
      }
    }

  } else {                                   # S | B | C
    order <- bcs_element_order
    a <- if (blank) NULL else ai_desc_assign(ai_desc_split(description, delimiters), order, dilution_mode)
    v <- if (is.null(a)) NULL else a$values

    out$source     <- (v[["Source"]] %||% "")
    out$timeperiod <- ""
    # subject_id comes from the Type token (S3 -> "3", C1 -> "1"), B -> "1"
    sfx <- substr(trimws(as.character(type)), 2L, nchar(trimws(as.character(type))))
    out$subject_id <- if (tc == "B") "1" else if (nzchar(sfx)) sfx else "1"

    if (!is.null(a)) {
      out$dilution_factor <- a$dilution$value
      out$dilution_ok     <- isTRUE(a$dilution$ok)
    }

    if (!blank) {
      field_of <- c(Source = "source")
      for (el in setdiff(order, "DilutionFactor")) {
        fld <- field_of[[el]]
        if (!is.null(fld) && !nzchar(out[[fld]] %||% ""))
          add(el, "error", sprintf("Type %s: %s is empty.", tc, el), "missing_required")
      }
    }
  }

  # ---- defaults (values only; issues above stand) ----
  if (use_defaults) {
    if (!nzchar(out$subject_id)) out$subject_id <- "1"
    if (tc == "X" && !nzchar(out$timeperiod)) out$timeperiod <- "T0"
    if (!isTRUE(out$dilution_ok) || is.na(out$dilution_factor)) out$dilution_factor <- 1L
    if (tc == "X") {
      if (!nzchar(out$groupa)) out$groupa <- "Unknown"
      if (!nzchar(out$groupb)) out$groupb <- "Unknown"
    }
  }

  if (length(iss)) out$issues <- do.call(rbind, iss)
  out
}


# ---- Vectorised, drop-in replacements ---------------------------------------

# Resolve the delimiter argument: prefer an explicit `delimiters` set; otherwise
# treat the legacy single `delimiter` string as a set of delimiter characters.
.ai_resolve_delims <- function(delimiter = "_", delimiters = NULL) {
  if (!is.null(delimiters)) return(delimiters)
  if (is.null(delimiter) || !nzchar(delimiter)) return(c("_", " ", ",", ";"))
  delimiter
}

#' Drop-in replacement for parse_all_descriptions() (bead / shared).
#'
#' A thin legacy wrapper: it builds a single-rule-per-type ruleset from the old
#' flat arguments (one delimiter set, element_order for X, bcs_element_order for
#' S/B/C) and hands off to the per-type driver ai_parse_ruleset(). Returns the
#' SAME six columns as before with the issues frame on attr(result, "issues").
parse_all_descriptions <- function(plate_data,
                                   delimiter          = "_",
                                   element_order      = c("PatientID", "TimePeriod", "DilutionFactor"),
                                   bcs_element_order  = c("Source", "DilutionFactor"),
                                   use_defaults       = TRUE,
                                   delimiters         = NULL,
                                   dilution_mode      = c("position", "auto", "content")) {
  dilution_mode <- match.arg(dilution_mode)
  delims <- .ai_resolve_delims(delimiter, delimiters)
  rules  <- ai_ruleset_from_legacy(delims, element_order, bcs_element_order, dilution_mode)
  ai_parse_ruleset(plate_data, rules, use_defaults = use_defaults)
}

#' Drop-in replacement for parse_elisa_descriptions() (ELISA).
#'
#' Same return list (subject_id, specimen_dilution_factor,
#' timepoint_tissue_abbreviation, specimen_source); issues attached as an attr.
parse_elisa_descriptions <- function(descriptions, stypes,
                                     delimiter          = "_",
                                     element_order      = c("PatientID", "TimePeriod", "DilutionFactor"),
                                     bcs_element_order  = c("Source", "DilutionFactor"),
                                     use_defaults       = FALSE,
                                     delimiters         = NULL,
                                     dilution_mode      = c("position", "auto", "content")) {
  dilution_mode <- match.arg(dilution_mode)
  delims <- .ai_resolve_delims(delimiter, delimiters)
  n <- length(descriptions)

  result <- list(
    subject_id                    = rep(NA_character_, n),
    specimen_dilution_factor      = rep(1, n),
    timepoint_tissue_abbreviation = rep(NA_character_, n),
    specimen_source               = rep(NA_character_, n))
  issues <- list()

  for (i in seq_len(n)) {
    r <- ai_parse_description_row(
      type = stypes[i], description = descriptions[i],
      element_order = element_order, bcs_element_order = bcs_element_order,
      delimiters = delims, dilution_mode = dilution_mode, use_defaults = use_defaults)

    # preserve ELISA's NA-when-not-defaulted convention for the text fields
    result$subject_id[i]                    <- if (nzchar(r$subject_id)) r$subject_id else if (use_defaults) "1" else NA_character_
    result$timepoint_tissue_abbreviation[i] <- if (nzchar(r$timeperiod))  r$timeperiod else if (use_defaults && r$type_char %in% c("X", NA)) "T0" else NA_character_
    result$specimen_source[i]               <- r$source
    result$specimen_dilution_factor[i]      <- if (is.na(r$dilution_factor)) 1 else as.numeric(r$dilution_factor)

    if (nrow(r$issues))
      issues[[length(issues) + 1L]] <- cbind(
        row = i, type = as.character(stypes[i]), r$issues, stringsAsFactors = FALSE)
  }

  attr(result, "issues") <- if (length(issues)) do.call(rbind, issues) else NULL
  result
}

#' Backward-compatible shim for parse_description_with_defaults() (X semantics).
parse_description_with_defaults <- function(description,
                                            delimiter         = "_",
                                            element_order     = c("PatientID", "SampleGroupA", "SampleGroupB", "TimePeriod", "DilutionFactor"),
                                            optional_elements = c("SampleGroupA", "SampleGroupB")) {
  order <- element_order[element_order %in%
    c("PatientID", "TimePeriod", "DilutionFactor", optional_elements)]
  r <- ai_parse_description_row(
    type = "X", description = description, element_order = order,
    delimiters = .ai_resolve_delims(delimiter, NULL),
    dilution_mode = "position", use_defaults = TRUE)
  list(subject_id = r$subject_id, groupa = r$groupa, groupb = r$groupb,
       timeperiod = r$timeperiod, dilution_factor = r$dilution_factor)
}


# ---- Field-aware validator (per-type contract) ------------------------------

#' Validate the Description contract for every specimen row.
#'
#' X needs PatientID + TimePeriod + integer-denominator dilution; S/B/C need
#' Source + integer-denominator dilution. Returns the standard issues frame the
#' import validators already rbind: data.frame(sheet, severity, column, message).
#'
#' @param on_blank severity for a wholly blank Description ("warning" keeps the
#'   defaults-and-proceed behaviour; "error" blocks; "ignore" drops it).
#' @param severity_missing severity for a missing required field / bad dilution.
validate_plate_descriptions <- function(plate_data,
                                         delimiter          = "_",
                                         element_order      = c("PatientID", "TimePeriod", "DilutionFactor"),
                                         bcs_element_order  = c("Source", "DilutionFactor"),
                                         delimiters         = NULL,
                                         dilution_mode      = c("position", "auto", "content"),
                                         sheet              = "plates_map",
                                         severity_missing   = "error",
                                         on_blank           = "warning") {
  dilution_mode <- match.arg(dilution_mode)
  delims <- .ai_resolve_delims(delimiter, delimiters)
  rules  <- ai_ruleset_from_legacy(delims, element_order, bcs_element_order, dilution_mode)
  .ai_validate_rows(plate_data, rules, sheet, severity_missing, on_blank)
}

#' Per-type-rule form of the validator (the preview/commit path). Same standard
#' issues frame; enforces each type's own rule.
validate_plate_descriptions_ruleset <- function(plate_data, rules,
                                                 sheet            = "plates_map",
                                                 severity_missing = "error",
                                                 on_blank         = "warning") {
  .ai_validate_rows(plate_data, rules, sheet, severity_missing, on_blank)
}


# =============================================================================
# PER-TYPE RULES  —  one rule per specimen type (X / S / B / C)
# -----------------------------------------------------------------------------
# A "rule" is exactly the four knobs the engine already executes — nothing the
# UI can express that the parser cannot run verbatim:
#     list(type, delimiters, order, dilution_mode)
# delimiters is a set of delimiter CHARACTERS (a string like "_" or "_ ,;", or a
# character vector). order is that type's element order (X: PatientID/TimePeriod/
# DilutionFactor[+groups]; S/B/C: Source/DilutionFactor). A ruleset is a named
# list keyed by the single-letter type: list(X=, S=, B=, C=).
# =============================================================================

AI_DESC_TYPES <- c("X", "S", "B", "C")

#' Default element order for a specimen type.
ai_default_order <- function(type) {
  if (identical(type, "X")) {
    c("PatientID", "TimePeriod", "DilutionFactor")
  } else {
    c("Source", "DilutionFactor")
  }
}

#' Construct one per-type rule (validates dilution_mode; defaults order by type).
#' delimiters defaults to "_" — the starting delimiter before the user picks any.
ai_parse_rule <- function(type, delimiters = "_", order = NULL,
                          dilution_mode = "position") {
  dilution_mode <- dilution_mode[1]
  if (!dilution_mode %in% c("auto", "position", "content"))
    stop("ai_parse_rule(): dilution_mode must be auto | position | content", call. = FALSE)
  if (is.null(order) || !length(order)) order <- ai_default_order(type)
  list(type = type, delimiters = delimiters, order = order,
       dilution_mode = dilution_mode)
}

#' Default ruleset: every type starts at delimiter "_", default order, auto.
ai_default_ruleset <- function(types = AI_DESC_TYPES, delimiters = "_",
                               dilution_mode = "position")
  setNames(lapply(types, function(t)
    ai_parse_rule(t, delimiters, NULL, dilution_mode)), types)

#' Build a ruleset from the legacy flat arguments (one delimiter set; X uses
#' element_order, S/B/C share bcs_element_order). Used by the drop-in wrappers.
ai_ruleset_from_legacy <- function(delimiters, element_order, bcs_element_order,
                                   dilution_mode = "position")
  list(
    X = ai_parse_rule("X", delimiters, element_order,     dilution_mode),
    S = ai_parse_rule("S", delimiters, bcs_element_order, dilution_mode),
    B = ai_parse_rule("B", delimiters, bcs_element_order, dilution_mode),
    C = ai_parse_rule("C", delimiters, bcs_element_order, dilution_mode))

#' Seed the delimiter set of every not-yet-customised type from one set.
#'
#' Implements the "first delimiter of the first configured type becomes the
#' common default" behaviour: the module tracks which types the user has edited
#' (`except`) and, on the first edit, calls this to propagate that set to the
#' rest. Types in `except` keep their own (independent) delimiters.
ai_propagate_delimiters <- function(rules, delimiters, except = character()) {
  for (t in names(rules))
    if (!t %in% except) rules[[t]]$delimiters <- delimiters
  rules
}

# route a per-type rule into the (type-aware) row engine and parse one row
.ai_row_with_rule <- function(type, description, rules, use_defaults) {
  tc <- if (is.na(type) || !nzchar(trimws(as.character(type)))) NA_character_
        else substr(trimws(as.character(type)), 1L, 1L)
  rule <- if (!is.na(tc) && !is.null(rules[[tc]])) rules[[tc]]
          else ai_parse_rule(if (is.na(tc)) "X" else tc)
  is_x <- identical(tc, "X")
  ai_parse_description_row(
    type = type, description = description,
    element_order     = if (is_x) rule$order else ai_default_order("X"),
    bcs_element_order = if (is_x) c("Source", "DilutionFactor") else rule$order,
    delimiters = rule$delimiters, dilution_mode = rule$dilution_mode,
    use_defaults = use_defaults)
}

#' Parse every row under its type's own rule. Same six-column output and issues
#' attribute as parse_all_descriptions(); the applied ruleset rides on
#' attr(result, "ruleset") for stamping/audit.
ai_parse_ruleset <- function(plate_data, rules, use_defaults = TRUE) {
  n <- nrow(plate_data)
  result <- data.frame(
    subject_id                    = character(n),
    specimen_dilution_factor      = numeric(n),
    timepoint_tissue_abbreviation = character(n),
    specimen_source               = character(n),
    groupa                        = character(n),
    groupb                        = character(n),
    stringsAsFactors = FALSE)

  well <- if ("well" %in% names(plate_data)) plate_data$well
          else if ("Well" %in% names(plate_data)) plate_data$Well else rep(NA, n)
  issues <- list()

  for (i in seq_len(n)) {
    r <- .ai_row_with_rule(plate_data$Type[i], plate_data$Description[i],
                           rules, use_defaults)
    result$subject_id[i]                    <- r$subject_id
    result$specimen_dilution_factor[i]      <- if (is.na(r$dilution_factor)) NA_real_ else as.numeric(r$dilution_factor)
    result$timepoint_tissue_abbreviation[i] <- r$timeperiod
    result$specimen_source[i]               <- r$source
    result$groupa[i]                        <- r$groupa
    result$groupb[i]                        <- r$groupb
    if (nrow(r$issues))
      issues[[length(issues) + 1L]] <- cbind(
        row = i, well = well[i], type = as.character(plate_data$Type[i]),
        r$issues, stringsAsFactors = FALSE)
  }

  attr(result, "issues") <- if (length(issues)) do.call(rbind, issues) else
    data.frame(row = integer(), well = character(), type = character(),
               field = character(), severity = character(), message = character(),
               kind = character(), stringsAsFactors = FALSE)
  attr(result, "ruleset") <- rules
  result
}

# shared row-level validator behind both validate_plate_descriptions* functions
.ai_validate_rows <- function(plate_data, rules, sheet, severity_missing, on_blank) {
  empty <- data.frame(sheet = character(), severity = character(),
                      column = character(), message = character(),
                      stringsAsFactors = FALSE)
  if (is.null(plate_data) || !nrow(plate_data) ||
      !all(c("Type", "Description") %in% names(plate_data))) return(empty)

  well <- if ("well" %in% names(plate_data)) plate_data$well
          else if ("Well" %in% names(plate_data)) plate_data$Well else rep(NA, nrow(plate_data))
  rows <- list()
  for (i in seq_len(nrow(plate_data))) {
    r <- .ai_row_with_rule(plate_data$Type[i], plate_data$Description[i], rules, FALSE)
    if (!nrow(r$issues)) next
    for (j in seq_len(nrow(r$issues))) {
      sev <- switch(r$issues$kind[j],
        missing_required   = severity_missing,
        bad_dilution       = severity_missing,
        blank_description  = on_blank,
        ambiguous_dilution = "warning",
        r$issues$severity[j])
      if (identical(sev, "ignore")) next
      loc <- if (!is.na(well[i])) sprintf(" (well %s)", well[i]) else sprintf(" (row %d)", i)
      rows[[length(rows) + 1L]] <- data.frame(
        sheet = sheet, severity = sev,
        column = r$issues$field[j] %||% NA_character_,
        message = paste0(r$issues$message[j], loc,
                         sprintf(" [Type %s, Description: '%s']",
                                 as.character(plate_data$Type[i]),
                                 as.character(plate_data$Description[i]))),
        stringsAsFactors = FALSE)
    }
  }
  if (!length(rows)) return(empty)
  do.call(rbind, rows)
}


# =============================================================================
# REPRESENTATIVE SAMPLING  —  pick the structurally distinct strings to show
# -----------------------------------------------------------------------------
# Two descriptions are "the same case" for a rule if they'd exercise it
# identically. The signature captures exactly what the engine keys on:
#   (token count under this rule's delimiters, dilution shape ratio/bare/none,
#    which delimiter chars actually appear, contract valid/invalid).
# One exemplar per distinct signature; order so the informative shapes lead:
#   INVALID first, then token-count EXTREMES, then RAREST, then the rest.
# The verdict (all_valid, n_distinct_shapes) is computed over ALL signatures —
# not just the shown ones — so the visual check can't give false confidence.
# =============================================================================

.ai_delims_present <- function(description, delimiters) {
  chars <- setdiff(unique(unlist(strsplit(paste0(delimiters, collapse = ""), ""))),
                   c(":", "/"))
  chars <- chars[nzchar(chars)]
  used <- chars[vapply(chars, function(d) grepl(d, description, fixed = TRUE), logical(1))]
  paste(sort(used), collapse = "")
}

.ai_signature <- function(description, type, rule) {
  toks  <- ai_desc_split(description, rule$delimiters)
  forms <- vapply(toks, function(t) { f <- ai_parse_dilution(t)$form
                                      if (is.na(f)) "none" else f }, character(1))
  dform <- if ("ratio" %in% forms) "ratio" else if ("bare" %in% forms) "bare" else "none"
  used  <- .ai_delims_present(description, rule$delimiters)
  r     <- .ai_row_with_rule(type, description, setNames(list(rule), type), FALSE)
  valid <- !any(r$issues$severity == "error")
  list(n_tokens = length(toks), dilution_form = dform, delims = used, valid = valid,
       signature = paste(length(toks), dform, used, valid, sep = "|"))
}

#' Choose representative example strings for ONE specimen type under a rule.
#'
#' @param descriptions the Description values for this type (any length).
#' @param type single-letter specimen type ("X"/"S"/"B"/"C").
#' @param rule  the per-type rule to evaluate against.
#' @param n     how many exemplars to surface (panel budget; default 4).
#' @return list(
#'   examples          : character() exemplar strings, ordered most-informative first,
#'   signatures        : data.frame(description, n_tokens, dilution_form, delims,
#'                                   valid, signature, is_exemplar, shown),
#'   n_distinct_shapes : integer, total distinct signatures,
#'   n_shown           : integer, length(examples),
#'   all_valid         : logical, rule satisfies the contract for EVERY shape,
#'   n_failing_shapes  : integer, distinct signatures that fail,
#'   failing_examples  : character() one exemplar per failing shape)
ai_representative_descriptions <- function(descriptions, type, rule, n = 4L) {
  d <- unique(as.character(descriptions))
  d <- d[!is.na(d) & nzchar(trimws(d)) & trimws(d) != "NA"]
  empty_sig <- data.frame(description = character(), n_tokens = integer(),
                          dilution_form = character(), delims = character(),
                          valid = logical(), signature = character(),
                          is_exemplar = logical(), shown = logical(),
                          stringsAsFactors = FALSE)
  if (!length(d))
    return(list(examples = character(), signatures = empty_sig,
                n_distinct_shapes = 0L, n_shown = 0L, all_valid = TRUE,
                n_failing_shapes = 0L, failing_examples = character()))

  sig <- lapply(d, .ai_signature, type = type, rule = rule)
  tab <- data.frame(
    description   = d,
    n_tokens      = vapply(sig, `[[`, integer(1),   "n_tokens"),
    dilution_form = vapply(sig, `[[`, character(1), "dilution_form"),
    delims        = vapply(sig, `[[`, character(1), "delims"),
    valid         = vapply(sig, `[[`, logical(1),   "valid"),
    signature     = vapply(sig, `[[`, character(1), "signature"),
    stringsAsFactors = FALSE)

  groups <- split(seq_len(nrow(tab)), tab$signature)
  # exemplar = shortest (easiest to read) string in the group
  reps <- vapply(groups, function(idx) idx[which.min(nchar(tab$description[idx]))], integer(1))

  g_valid <- tab$valid[reps]
  g_ntok  <- tab$n_tokens[reps]
  g_size  <- vapply(groups, length, integer(1))
  rng     <- range(g_ntok)
  key_valid   <- ifelse(g_valid, 1L, 0L)                 # invalid shapes first
  key_extreme <- ifelse(g_ntok %in% rng, 0L, 1L)         # token-count extremes next
  ord <- order(key_valid, key_extreme, g_size, g_ntok)   # then rarest, then by size

  reps_ord  <- reps[ord]
  shown_idx <- reps_ord[seq_len(min(as.integer(n), length(reps_ord)))]

  tab$is_exemplar <- seq_len(nrow(tab)) %in% reps
  tab$shown       <- seq_len(nrow(tab)) %in% shown_idx

  failing_reps <- reps[!g_valid]
  list(
    examples          = tab$description[shown_idx],
    signatures        = tab,
    n_distinct_shapes = length(groups),
    n_shown           = length(shown_idx),
    all_valid         = all(tab$valid),
    n_failing_shapes  = sum(!g_valid),
    failing_examples  = tab$description[failing_reps])
}


# =============================================================================
# PREVIEW PAYLOAD  —  what the interactive panel renders for one string
# =============================================================================

#' Parse one string under a rule and return a render-ready breakdown: the tokens,
#' the field chips in rule order (value + whether required + whether satisfied),
#' the resolved integer dilution, the contract issues, and an overall verdict.
ai_preview_description <- function(type, description, rule) {
  tc      <- substr(trimws(as.character(type)), 1L, 1L)
  toks    <- ai_desc_split(description, rule$delimiters)
  a       <- ai_desc_assign(toks, rule$order, rule$dilution_mode)
  r       <- .ai_row_with_rule(type, description, setNames(list(rule), tc), FALSE)
  required <- if (identical(tc, "X")) c("PatientID", "TimePeriod", "DilutionFactor")
              else c("Source", "DilutionFactor")

  vals <- vapply(rule$order, function(f) {
    v <- if (identical(f, "DilutionFactor") && isTRUE(a$dilution$ok))
           as.character(a$dilution$value) else a$values[[f]]
    if (is.null(v)) "" else as.character(v)
  }, character(1))
  ok <- vapply(rule$order, function(f)
    if (identical(f, "DilutionFactor")) isTRUE(a$dilution$ok)
    else nzchar(a$values[[f]] %||% ""), logical(1))

  fields <- data.frame(field = rule$order, value = vals,
                       required = rule$order %in% required, ok = ok,
                       stringsAsFactors = FALSE)

  list(
    type        = tc,
    description = description,
    tokens      = toks,
    fields      = fields,
    dilution    = list(raw = a$values[["DilutionFactor"]] %||% NA_character_,
                       value = a$dilution$value, ok = a$dilution$ok,
                       form = a$dilution$form),
    issues      = r$issues,
    valid       = !any(r$issues$severity == "error"))
}


# =============================================================================
# parse_rule SHEET  —  stamp the approved ruleset into the template (audit)
# =============================================================================

#' Flatten a ruleset to a data.frame for the workbook's `parse_rule` sheet.
ai_ruleset_to_sheet <- function(rules, stamped_at = as.character(Sys.time())) {
  do.call(rbind, lapply(names(rules), function(t) {
    r <- rules[[t]]
    data.frame(
      specimen_type = t,
      delimiters    = paste0(r$delimiters, collapse = ""),
      element_order = paste(r$order, collapse = ","),
      greedy_field  = paste(intersect(r$order, .AI_DESC_GREEDY), collapse = ","),
      dilution_mode = r$dilution_mode,
      stamped_at    = stamped_at,
      stringsAsFactors = FALSE)
  }))
}

#' Rebuild a ruleset from a previously-stamped `parse_rule` sheet (for audit /
#' re-validation of an uploaded template).
ai_sheet_to_ruleset <- function(df) {
  setNames(lapply(seq_len(nrow(df)), function(i)
    ai_parse_rule(
      type          = as.character(df$specimen_type[i]),
      delimiters    = as.character(df$delimiters[i]),
      order         = trimws(strsplit(as.character(df$element_order[i]), ",")[[1]]),
      dilution_mode = as.character(df$dilution_mode[i]))),
    as.character(df$specimen_type))
}


# =============================================================================
# Small helpers used by the template generators / rule panel
# =============================================================================

#' The delimiter CHARACTERS of a rule's delimiter set (":"/"/" excluded, since
#' they are reserved for the 1:N / 1/N dilution form). Accepts a string of
#' characters ("_", "_ ,;") or a character vector.
ai_delim_chars <- function(delimiters) {
  chars <- unique(unlist(strsplit(paste0(as.character(delimiters), collapse = ""), "")))
  chars <- chars[nzchar(chars)]
  setdiff(chars, c(":", "/"))
}

#' Finalize a parsed timepoint for the plates_map.
#'
#' When space is NOT one of the type's delimiters, a space in the value is stray
#' whitespace and is stripped (the legacy behaviour). When space IS a delimiter,
#' the greedy TimePeriod field was intentionally assembled from several tokens
#' (e.g. "Day 7 Post"), so internal single spaces are meaningful and preserved;
#' only control characters are removed. NA passes through.
ai_finalize_timepoint <- function(x, space_is_delim) {
  x <- as.character(x)
  if (isTRUE(space_is_delim)) return(gsub("[[:cntrl:]]", "", x))
  gsub("[[:space:][:cntrl:]]", "", x)
}


# =============================================================================
# DILUTION RESOLUTION  —  three-state, auto-detected per type
# -----------------------------------------------------------------------------
# After the description is parsed positionally, each well's dilution comes from
# one of three sources, chosen automatically:
#
#   parsed          DilutionFactor is in the order and yields a valid integer
#                   denominator (any number of distinct values across wells).
#   constant        X or B only, when DilutionFactor is NOT in the order at all
#                   -> one value for every well of that type (the neat/.rbx case).
#   per_description X/B partial (DilutionFactor in order but some wells fail) OR
#                   any S/C that can't be parsed -> one manual value per UNIQUE
#                   failing Description string (hybrid: parsed wells keep their
#                   value; only the failing uniques need entry).
#
# ai_dilution_plan() reports, per type, which state applies and exactly which
# unique strings still need a value; ai_apply_dilution() produces the final
# per-row dilution vector once the manual values are supplied. Both are pure.
# =============================================================================

# parsed dilution for one description under a rule (NA if DF absent/unparseable)
.ai_parsed_dilution <- function(description, rule) {
  if (!("DilutionFactor" %in% rule$order)) return(NA_integer_)
  if (is.na(description) || !nzchar(trimws(as.character(description)))) return(NA_integer_)
  a <- ai_desc_assign(ai_desc_split(description, rule$delimiters), rule$order, rule$dilution_mode)
  if (isTRUE(a$dilution$ok)) as.integer(a$dilution$value) else NA_integer_
}

#' Plan how a type's dilutions resolve under a rule.
#'
#' @param descriptions the Description values for this type.
#' @param type "X" | "S" | "B" | "C".
#' @param rule the per-type rule.
#' @return list(
#'   type, mode = "parsed" | "constant" | "per_description",
#'   has_dilution_element = <lgl>,                 # DilutionFactor in the order
#'   parsed = named integer over unique descriptions (NA where unparseable),
#'   needs  = character() unique descriptions requiring a manual value
#'            (empty for "parsed"; empty for "constant" — one value covers all),
#'   n_wells_parsed, n_wells_manual)
ai_dilution_plan <- function(descriptions, type, rule) {
  tc <- substr(trimws(as.character(type)), 1L, 1L)
  d  <- as.character(descriptions)
  nonblank <- d[!is.na(d) & nzchar(trimws(d)) & trimws(d) != "NA"]
  uniq <- unique(nonblank)
  has_df <- "DilutionFactor" %in% rule$order

  parsed <- setNames(vapply(uniq, .ai_parsed_dilution, integer(1), rule = rule), uniq)
  fail   <- uniq[is.na(parsed)]

  if (!has_df && tc %in% c("X", "B")) {
    mode  <- "constant"; needs <- character()
  } else if (length(fail) == 0L && has_df) {
    mode  <- "parsed";   needs <- character()
  } else {
    # per-description: S/C with no usable dilution -> all uniques; X/B or S/C
    # partial parse -> just the failing uniques (hybrid).
    mode  <- "per_description"
    needs <- if (!has_df) uniq else fail
  }

  # well counts (over the full, non-unique vector)
  pv <- if (length(parsed)) parsed[match(nonblank, names(parsed))] else integer(0)
  n_parsed <- sum(!is.na(pv))
  list(type = tc, mode = mode, has_dilution_element = has_df,
       parsed = parsed, needs = needs,
       n_wells_parsed = n_parsed, n_wells_manual = length(nonblank) - n_parsed)
}

#' Resolve the final per-row dilution for one type, given the manual values.
#'
#' @param descriptions per-row Description values for this type.
#' @param type,rule as in ai_dilution_plan.
#' @param constant single numeric for a "constant"-mode type (X/B, DF absent).
#' @param per_description named numeric map (Description -> value) for manual
#'   entries; supplied for "per_description" mode.
#' @return integer vector aligned to `descriptions` (NA only if still unresolved).
ai_apply_dilution <- function(descriptions, type, rule,
                              constant = NA_integer_, per_description = NULL) {
  d <- as.character(descriptions)
  out <- vapply(d, .ai_parsed_dilution, integer(1), rule = rule)  # parsed where possible
  fill <- is.na(out)
  if (any(fill)) {
    if (!is.null(per_description) && length(per_description)) {
      m <- suppressWarnings(as.integer(per_description[d[fill]]))
      out[fill] <- m
    }
    still <- is.na(out)
    if (any(still) && !is.na(constant))
      out[still] <- as.integer(constant)
  }
  unname(out)
}

#' Whether a plan's manual requirements are fully satisfied (for approval gating).
#' @param plan an ai_dilution_plan() result.
#' @param constant the constant input for the type (NA if not entered).
#' @param per_description named numeric map of entered per-description values.
ai_dilution_satisfied <- function(plan, constant = NA_integer_, per_description = NULL) {
  if (identical(plan$mode, "parsed")) return(TRUE)
  if (identical(plan$mode, "constant"))
    return(!is.na(suppressWarnings(as.integer(constant))))
  # per_description: every needed unique must have a positive integer value
  if (!length(plan$needs)) return(TRUE)
  vals <- suppressWarnings(as.integer(per_description[plan$needs]))
  length(vals) == length(plan$needs) && all(!is.na(vals) & vals > 0)
}
