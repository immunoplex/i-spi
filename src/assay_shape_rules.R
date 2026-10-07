# =============================================================================
# assay_shape_rules.R  --  Stages 2 & 3 of the description pre-processor
# -----------------------------------------------------------------------------
# Turns the confirmed well inventory into a declarative, per-specimen-type,
# per-SHAPE description ruleset, and resolves it to the identity columns the
# layout template expects.
#
# WHY SHAPE, NOT TYPE
# The drafted model (assay_description_parse.R) holds ONE positional element
# order per specimen type. That cannot describe a batch whose controls arrive as
# "QC 1", "SPIKE A" and "LtyUp" -- one, two and one token, all type C -- and it
# cannot express a blank whose description says only "blank" but which needs
# source = PBS and dilution = 1. So the unit a user binds is the description
# SHAPE, and each identity component gets a binding KIND:
#
#   slot       token at index i (or joined tokens i..j -- explicit, not greedy)
#   pattern    first token whose format class matches (survives a shifting slot)
#   constant   one value for the whole shape, typed once
#   from_type  the numeric suffix of the type code:  S3 -> 3
#   ignore     token contributes nothing (batch ids, operator initials)
#
# WHAT IT REUSES
# ai_desc_split(), ai_desc_delim_regex() and ai_parse_dilution() from
# assay_description_parse.R are kept as-is: splitting on a delimiter character
# class and normalising 1:N / 1/N / M:N / bare N to an integer denominator are
# already correct. Thin wrappers below fall back to a local implementation if
# that file has not been sourced, so this file cannot fail cryptically.
#
# WHAT IT REPLACES
# .ai_allocate()'s greedy-field trick (surplus tokens absorbed into TimePeriod
# or Source) and ai_representative_descriptions()'s fixed four-exemplar panel.
# Both are illustrative; a shape key is addressable -- you can hang a binding
# off it.
#
# Pure base R (+ yaml/jsonlite for profile files only). Source AFTER
# assay_description_parse.R and assay_well_inventory.R.
# =============================================================================

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

AI_PROFILE_VERSION <- 1L

AI_COMPONENTS <- c("PatientID", "TimePeriod", "DilutionFactor", "Source",
                   "SampleGroupA", "SampleGroupB", "Replicate")

AI_BINDING_KINDS <- c("slot", "pattern", "constant", "from_type", "ignore")

# The per-type contract. Required components must resolve non-empty or the shape
# is an error. X's dilution is deliberately NOT required here: a missing sample
# dilution is legitimate and is settled by ai_dilution_plan() in stage 4.
AI_TYPE_REQUIRED <- list(
  X = c("PatientID", "TimePeriod"),
  S = c("Source", "DilutionFactor"),
  B = c("Source", "DilutionFactor"),
  C = c("DilutionFactor"))

AI_TYPE_OPTIONAL <- list(
  X = c("DilutionFactor", "Source", "SampleGroupA", "SampleGroupB", "Replicate"),
  S = c("Replicate", "SampleGroupA"),
  B = c("Replicate"),
  C = c("Source", "Replicate", "SampleGroupA"))

#' Components offered for a type, required first.
ai_type_components <- function(type) {
  t <- if (type %in% names(AI_TYPE_REQUIRED)) type else "X"
  unique(c(AI_TYPE_REQUIRED[[t]], AI_TYPE_OPTIONAL[[t]]))
}


# ---- Dependency wrappers ----------------------------------------------------

.ai_sr_split <- function(x, delimiters) {
  if (exists("ai_desc_split", mode = "function"))
    return(ai_desc_split(x, delimiters))
  if (length(x) != 1L || is.na(x)) return(character(0))
  chars <- setdiff(unique(strsplit(paste0(as.character(delimiters), collapse = ""), "")[[1]]),
                   c(":", "/", ""))
  if (!length(chars)) chars <- "_"
  esc <- vapply(chars, function(c)
    if (grepl("[[:alnum:]]", c)) c else paste0("\\", c), character(1))
  rx <- paste0("[", paste0(esc, collapse = ""), "]+")
  t <- trimws(strsplit(as.character(x), rx, perl = TRUE)[[1]])
  t[nzchar(t)]
}

.ai_sr_dilution <- function(x) {
  if (exists("ai_parse_dilution", mode = "function")) return(ai_parse_dilution(x))
  if (length(x) != 1L || is.na(x))
    return(list(value = NA_integer_, ok = FALSE, form = NA_character_))
  s <- trimws(as.character(x))
  m <- regmatches(s, regexec("^([0-9][0-9,]*)[[:space:]]*[:/][[:space:]]*([0-9][0-9,]*)$", s))[[1]]
  if (length(m) == 3L) {
    d <- suppressWarnings(as.integer(gsub(",", "", m[3])))
    return(list(value = d, ok = !is.na(d), form = "ratio"))
  }
  if (grepl("^[0-9][0-9,]*$", s)) {
    v <- suppressWarnings(as.integer(gsub(",", "", s)))
    return(list(value = v, ok = !is.na(v), form = "bare"))
  }
  list(value = NA_integer_, ok = FALSE, form = NA_character_)
}


# =============================================================================
# TOKEN FORMAT CLASSES
# -----------------------------------------------------------------------------
# These group strings; they do not assign meaning. "D123" classifies as
# `timepoint` on shape alone even when it is a subject id -- which is fine,
# because the class only decides which strings share a shape, and the user
# states the meaning explicitly in stage 3.
# =============================================================================

AI_TOKEN_CLASSES <- c("empty", "ratio", "integer", "decimal", "timepoint",
                      "alpha", "alnum", "code", "mixed")

.AI_RX_TIMEPOINT <- paste0(
  "^(?:(?:v|d|t|m|w|visit|day|wk|week|mo|month|yr|year)[ _-]?[0-9]+",
  "|[0-9]+[ _-]?(?:d|w|wk|m|mo|y|yr|days?|weeks?|months?|years?)",
  "|pre|post|baseline|cord|birth|term|delivery|screening|eos|eot)$")

#' Format class of each token. Vectorised.
ai_token_class <- function(tok) {
  s <- trimws(as.character(tok))
  out <- rep("mixed", length(s))
  out[is.na(s) | !nzchar(s)]                                  <- "empty"
  todo <- out == "mixed"
  hit <- function(rx, cls, perl = TRUE) {
    m <- todo & grepl(rx, s, perl = perl)
    out[m] <<- cls; todo <<- todo & !m
  }
  hit("^[0-9][0-9,]*[[:space:]]*[:/][[:space:]]*[0-9][0-9,]*$", "ratio")
  hit("^[0-9][0-9,]*$",                                        "integer")
  hit("^[0-9]*\\.[0-9]+$",                                     "decimal")
  hit(.AI_RX_TIMEPOINT,                                        "timepoint")
  # case-insensitive second pass for timepoints written upper-case
  m <- todo & grepl(.AI_RX_TIMEPOINT, tolower(s), perl = TRUE)
  out[m] <- "timepoint"; todo <- todo & !m
  hit("^[A-Za-z]+$",                                           "alpha")
  hit("^(?=.*[A-Za-z])(?=.*[0-9])[A-Za-z0-9]+$",               "alnum")
  hit("^[-A-Za-z0-9(\\[][-A-Za-z0-9.+()\\[\\]#&'_]*$",          "code")
  out
}


# =============================================================================
# SHAPE SIGNATURES
# -----------------------------------------------------------------------------
# Three granularities, as you described -- number of elements, element format,
# element content:
#
#   count    "3"                            arity only
#   format   "3|alnum-timepoint-integer"    default; separates PID_V1_100
#                                           from PID_100_V1
#   content  "2|alpha-integer|qc"           adds the literal value at positions
#                                           whose vocabulary is small, so
#                                           "QC 1" and "SPIKE 1" become
#                                           different shapes with different
#                                           meanings
#
# `content` needs corpus knowledge (which positions are low-cardinality), so
# that set is computed once by ai_shape_table() and STORED IN THE RULE as
# content_positions. Resolution then reproduces identical keys from the rule
# alone -- no corpus needed, which is what makes a saved profile portable.
# =============================================================================

AI_SHAPE_BY <- c("count", "format", "content")

# Wells with no description at all get their OWN shape rather than being
# dropped. Without this, retyping a descriptionless well to (say) Control gave
# that type zero description groups: nothing to select, nothing to bind, nothing
# to approve, and a gate that could never open. An empty description is a real
# case -- it is the "blank well labelled nothing" case -- and what it needs is
# somewhere to hang constants (Source = PBS, DilutionFactor = 1).
AI_EMPTY_SHAPE_KEY <- "0|(no description)"

.ai_base_key <- function(n, classes, shape_by) {
  if (identical(shape_by, "count")) return(as.character(n))
  paste0(n, "|", paste(classes, collapse = "-"))
}

#' Shape key for one description under a rule's keying settings.
#'
#' @param rule a rule (uses $delimiters, $shape_by, $content_positions).
ai_shape_key_one <- function(description, rule) {
  if (is.na(description) || !nzchar(trimws(as.character(description))) ||
      identical(trimws(as.character(description)), "NA"))
    return(AI_EMPTY_SHAPE_KEY)
  toks <- .ai_sr_split(description, rule$delimiters)
  cls  <- ai_token_class(toks)
  base <- .ai_base_key(length(toks), cls, rule$shape_by %||% "format")
  if (!identical(rule$shape_by, "content")) return(base)
  pos <- rule$content_positions[[base]]
  if (is.null(pos) || !length(pos)) return(base)
  pos <- pos[pos >= 1L & pos <= length(toks)]
  if (!length(pos)) return(base)
  paste0(base, "|", paste(tolower(toks[pos]), collapse = "-"))
}

#' Distinct description strings for a corpus, ordered longest first.
#'
#' Longest-first is the useful default: the longest string exercises the most
#' slots and exposes a wrong delimiter fastest.
#'
#' @param order_by "length" (default) or "frequency".
ai_desc_corpus <- function(descriptions, plates = NULL, order_by = "length") {
  d <- as.character(descriptions)
  keep <- !is.na(d) & nzchar(trimws(d)) & trimws(d) != "NA"
  d <- d[keep]
  if (!length(d))
    return(data.frame(description = character(), n_wells = integer(),
                      n_chars = integer(), plates = character(),
                      stringsAsFactors = FALSE))
  p <- if (is.null(plates)) rep(NA_character_, length(descriptions))[keep]
       else as.character(plates)[keep]
  u <- unique(d)
  out <- data.frame(
    description = u,
    n_wells = as.integer(table(factor(d, levels = u))[u]),
    n_chars = nchar(u),
    plates  = vapply(u, function(s) {
      pl <- unique(stats::na.omit(p[d == s]))
      if (!length(pl)) "" else paste(pl, collapse = ", ")
    }, character(1)),
    stringsAsFactors = FALSE)
  out <- if (identical(order_by, "frequency"))
    out[order(-out$n_wells, out$description), , drop = FALSE]
  else
    out[order(-out$n_chars, out$description), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Group a type's descriptions into shapes.
#'
#' @param descriptions the Description values for one specimen type (per well).
#' @param delimiters   delimiter character set.
#' @param shape_by     "count" | "format" | "content".
#' @param content_max_levels a position is treated as literal (content keying)
#'   when its distinct vocabulary within the base group is at most this size.
#' @param plates optional per-well plate keys, for the plates column.
#' @return list(
#'   shapes = data.frame(shape_key, base_key, n_tokens, classes, n_strings,
#'                       n_wells, example, longest, plates),
#'   content_positions = named list(base_key -> integer positions),
#'   keys = character() per-well shape key aligned to `descriptions`)
ai_shape_table <- function(descriptions, delimiters, shape_by = "format",
                           content_max_levels = 6L, plates = NULL) {
  d <- as.character(descriptions)
  ok <- !is.na(d) & nzchar(trimws(d)) & trimws(d) != "NA"
  empty_shapes <- data.frame(
    shape_key = character(), base_key = character(), n_tokens = integer(),
    classes = character(), n_strings = integer(), n_wells = integer(),
    example = character(), longest = character(), plates = character(),
    stringsAsFactors = FALSE)
  n_empty <- sum(!ok)
  empty_row <- function(n_wells) data.frame(
    shape_key = AI_EMPTY_SHAPE_KEY, base_key = AI_EMPTY_SHAPE_KEY,
    n_tokens = 0L, classes = "(none)", n_strings = 1L, n_wells = n_wells,
    example = "", longest = "",
    plates = paste(unique(stats::na.omit(
      if (is.null(plates)) character(0) else as.character(plates)[!ok])),
      collapse = ", "),
    stringsAsFactors = FALSE)

  if (!any(ok)) {
    keys <- rep(NA_character_, length(d))
    if (n_empty) keys[!ok] <- AI_EMPTY_SHAPE_KEY
    return(list(
      shapes = if (n_empty) empty_row(n_empty) else empty_shapes,
      content_positions = list(), keys = keys))
  }

  toks <- lapply(d[ok], .ai_sr_split, delimiters = delimiters)
  cls  <- lapply(toks, ai_token_class)
  n    <- vapply(toks, length, integer(1))
  base <- vapply(seq_along(toks), function(i)
    .ai_base_key(n[i], cls[[i]], shape_by), character(1))

  # content keying: find the low-cardinality positions of each base group
  content_positions <- list()
  if (identical(shape_by, "content")) {
    for (bk in unique(base)) {
      g <- which(base == bk)
      if (!length(g)) next
      width <- n[g][1]
      pos <- integer(0)
      for (j in seq_len(width)) {
        vals <- tolower(vapply(toks[g], function(t)
          if (length(t) >= j) t[j] else "", character(1)))
        nu <- length(unique(vals))
        # a position is "literal" only if it varies a little but does vary --
        # a constant position adds nothing, and a free-form id adds noise
        if (nu > 1L && nu <= content_max_levels) pos <- c(pos, j)
      }
      content_positions[[bk]] <- pos
    }
  }

  key_ok <- vapply(seq_along(toks), function(i) {
    if (!identical(shape_by, "content")) return(base[i])
    pos <- content_positions[[base[i]]]
    if (is.null(pos) || !length(pos)) return(base[i])
    pos <- pos[pos >= 1L & pos <= length(toks[[i]])]
    if (!length(pos)) return(base[i])
    paste0(base[i], "|", paste(tolower(toks[[i]][pos]), collapse = "-"))
  }, character(1))

  keys <- rep(NA_character_, length(d))
  keys[ok] <- key_ok
  if (n_empty) keys[!ok] <- AI_EMPTY_SHAPE_KEY

  pl <- if (is.null(plates)) rep(NA_character_, length(d)) else as.character(plates)
  dd <- d[ok]; pp <- pl[ok]

  uk <- unique(key_ok)
  shapes <- do.call(rbind, lapply(uk, function(k) {
    g  <- which(key_ok == k)
    ds <- dd[g]
    ud <- unique(ds)
    data.frame(
      shape_key = k,
      base_key  = base[g][1],
      n_tokens  = n[g][1],
      classes   = paste(cls[[g[1]]], collapse = "-"),
      n_strings = length(ud),
      n_wells   = length(g),
      example   = ud[which.min(nchar(ud))],
      longest   = ud[which.max(nchar(ud))],
      plates    = paste(unique(stats::na.omit(pp[g])), collapse = ", "),
      stringsAsFactors = FALSE)
  }))
  # the no-description group, if any, is a shape like the others
  if (n_empty) shapes <- rbind(shapes, empty_row(n_empty))

  # widest shapes first: they carry the most structure and are what the user
  # should bind before the degenerate one-token cases
  shapes <- shapes[order(-shapes$n_tokens, -shapes$n_wells, shapes$shape_key), ,
                   drop = FALSE]
  rownames(shapes) <- NULL

  list(shapes = shapes, content_positions = content_positions, keys = keys)
}


# =============================================================================
# DELIMITER SUGGESTION
# -----------------------------------------------------------------------------
# A lab using "_" consistently yields ONE distinct token count across its
# corpus; a wrong guess yields five. So score candidates by how consistently
# they cut, and offer the winner before the user types anything.
# =============================================================================

AI_DELIM_CANDIDATES <- c("_", "-", " ", ",", ";", "|", ".", "+")

#' Score delimiter candidates against a corpus.
#'
#' @return data.frame(delimiter, coverage, mean_tokens, n_token_counts, score)
#'   ordered best first. `coverage` is the fraction of strings containing it;
#'   `n_token_counts` is how many distinct arities it produces (lower is better).
ai_suggest_delimiters <- function(descriptions,
                                  candidates = AI_DELIM_CANDIDATES) {
  d <- unique(as.character(descriptions))
  d <- d[!is.na(d) & nzchar(trimws(d)) & trimws(d) != "NA"]
  if (!length(d))
    return(data.frame(delimiter = character(), coverage = numeric(),
                      mean_tokens = numeric(), n_token_counts = integer(),
                      score = numeric(), stringsAsFactors = FALSE))

  rows <- lapply(setdiff(candidates, c(":", "/")), function(ch) {
    cover <- mean(vapply(d, function(s) grepl(ch, s, fixed = TRUE), logical(1)))
    n <- vapply(d, function(s) length(.ai_sr_split(s, ch)), integer(1))
    data.frame(delimiter = ch, coverage = cover,
               mean_tokens = mean(n), n_token_counts = length(unique(n)),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  # reward coverage and splitting at all; penalise inconsistent arity
  out$score <- ifelse(out$mean_tokens <= 1, 0,
                      out$coverage * (1 / out$n_token_counts))
  out <- out[order(-out$score, -out$coverage), , drop = FALSE]
  rownames(out) <- NULL
  out
}


# =============================================================================
# BINDINGS AND RULES
# =============================================================================

#' One component binding.
#'
#' @param how   "slot" | "pattern" | "constant" | "from_type" | "ignore".
#' @param slot  integer index, or a vector for joined tokens (how = "slot").
#' @param class token format class(es) to look for (how = "pattern").
#' @param value literal value (how = "constant").
#' @param join  separator used when `slot` spans several tokens.
ai_binding <- function(how, slot = NULL, class = NULL, value = NULL,
                       join = " ") {
  how <- match.arg(how, AI_BINDING_KINDS)
  if (identical(how, "slot") && (is.null(slot) || !length(slot)))
    stop("ai_binding(): how = 'slot' needs a slot index", call. = FALSE)
  if (identical(how, "pattern") && (is.null(class) || !length(class)))
    stop("ai_binding(): how = 'pattern' needs a class", call. = FALSE)
  out <- list(how = how, join = join)
  if (!is.null(slot))  out$slot  <- as.integer(slot)
  if (!is.null(class)) out$class <- as.character(class)
  if (!is.null(value)) out$value <- as.character(value)
  out
}

#' One per-type rule.
#'
#' @param shapes named list: shape_key -> named list of component bindings.
#' @param fallback bindings for an unmatched shape, or NULL (unmatched = error).
ai_shape_rule <- function(type, delimiters = "_", shape_by = "format",
                          shapes = list(), fallback = NULL,
                          content_positions = list()) {
  shape_by <- match.arg(shape_by, AI_SHAPE_BY)
  list(type = type, delimiters = delimiters, shape_by = shape_by,
       shapes = shapes, fallback = fallback,
       content_positions = content_positions)
}


# ---- Proposed bindings ------------------------------------------------------

#' Guess a shape's bindings from its token classes.
#'
#' The starting point every batch gets, so a well-behaved lab needs no clicks.
#' Two rules here are pinned by real .rbx descriptions (see
#' test-assay-shape-rules.R) and are NOT arbitrary:
#'
#' 1. FOR TYPE X, A BARE INTEGER IS NEVER PROPOSED AS THE DILUTION.
#'    Real sample descriptions look like "051 V1" and "80 V1" -- the patient id
#'    IS an integer. Proposing the only integer as a dilution ate the subject id
#'    and left PatientID unbound. X's dilution is optional by contract (stage 4
#'    resolves it), so leaving it unbound costs nothing, whereas guessing wrong
#'    corrupts identity. Only an explicit ratio (1:N) binds X's dilution here.
#'    For S/B/C a bare integer IS proposed: a standard point's description
#'    usually carries its dilution and nothing else numeric.
#'
#' 2. SOURCE IS GREEDY OVER THE LEADING UNCLAIMED RUN.
#'    Real standards read "Inhouse Ref 1:2952450" and controls "QC1 (Low) 1:2500"
#'    -- the source is two tokens. Binding only the first token gives "Inhouse"
#'    and "QC1", which then land in curve_lookup as distinct sources from the
#'    same reference material. A contiguous run is bound as slot = c(1, 2) and
#'    joined, which states the span explicitly rather than relying on a greedy
#'    field the user cannot see or override.
#'
#' A required component with no candidate is proposed as an EMPTY constant, so
#' the shape reads as incomplete rather than silently wrong -- that is the
#' "description says only 'blank'" case surfacing as a value the user must type.
ai_propose_bindings <- function(type, example, delimiters,
                                shape_by = "format") {
  toks <- .ai_sr_split(example, delimiters)
  cls  <- ai_token_class(toks)
  used <- rep(FALSE, length(toks))
  b    <- list()

  take <- function(classes, prefer_last = FALSE) {
    cand <- which(!used & cls %in% classes)
    if (!length(cand)) return(NA_integer_)
    i <- if (prefer_last) cand[length(cand)] else cand[1]
    used[i] <<- TRUE
    i
  }
  # the leading contiguous run of still-unclaimed tokens
  take_run <- function() {
    rem <- which(!used)
    if (!length(rem)) return(integer(0))
    run <- rem[1]; k <- 1L
    while (k < length(rem) && rem[k + 1L] == rem[k] + 1L) {
      run <- c(run, rem[k + 1L]); k <- k + 1L
    }
    used[run] <<- TRUE
    run
  }
  any_class <- setdiff(AI_TOKEN_CLASSES, "empty")

  # A ratio is unambiguous for every type: nothing else looks like 1:N.
  i <- take("ratio")
  if (!is.na(i)) b$DilutionFactor <- ai_binding("slot", slot = i)

  if (type == "X") {
    i <- take("timepoint")
    if (!is.na(i)) b$TimePeriod <- ai_binding("slot", slot = i)
    i <- take(any_class)                       # first unclaimed token = subject
    if (!is.na(i)) b$PatientID <- ai_binding("slot", slot = i)

    # fill any still-unbound REQUIRED component from the remaining tokens in
    # order -- this is what turns "IFO 100" into PatientID=IFO, TimePeriod=100
    for (comp in AI_TYPE_REQUIRED$X) {
      if (!is.null(b[[comp]])) next
      i <- take(any_class)
      if (!is.na(i)) b[[comp]] <- ai_binding("slot", slot = i)
    }
    if (is.null(b$Source)) b$Source <- ai_binding("constant", value = "sample")

  } else {
    # S/B/C: a bare integer is the dilution when no ratio is present. Note this
    # can pick up a standard POINT number ("STD 3" -> 3); the shape preview
    # shows the resolved value, so it is visible and correctable.
    if (is.null(b$DilutionFactor)) {
      i <- take("integer", prefer_last = TRUE)
      if (!is.na(i)) b$DilutionFactor <- ai_binding("slot", slot = i)
    }
    run <- take_run()                          # greedy source (see note 2)
    if (length(run)) b$Source <- ai_binding("slot", slot = run)
  }

  # Blanks almost always mean neat diluent at 1x. Propose the dilution, but
  # force the buffer name to be typed: "blank" is not a source, and writing it
  # through would land a meaningless source on every blank in the study.
  if (type == "B") {
    if (is.null(b$DilutionFactor))
      b$DilutionFactor <- ai_binding("constant", value = "1")
    lbl <- tolower(paste(toks, collapse = " "))
    if (!nzchar(lbl) ||
        grepl("^b$|^blk$|blank|background|^bg$|^empty$", lbl))
      b$Source <- ai_binding("constant", value = "")
  }

  # any still-missing required component becomes an empty constant to fill in
  req_t <- if (type %in% names(AI_TYPE_REQUIRED)) type else "X"
  for (comp in AI_TYPE_REQUIRED[[req_t]])
    if (is.null(b[[comp]])) b[[comp]] <- ai_binding("constant", value = "")

  b
}

#' Build a starting ruleset from the confirmed inventory.
#'
#' One rule per specimen type PRESENT in the batch, delimiters suggested from
#' that type's own corpus, and proposed bindings for every shape found.
ai_ruleset_init <- function(inv, delimiters = NULL, shape_by = "format") {
  types <- intersect(AI_SPECIMEN_TYPES, unique(stats::na.omit(inv$specimen_type)))
  rules <- list()
  for (t in types) {
    d <- inv$description[!is.na(inv$specimen_type) & inv$specimen_type == t]
    d <- d[!is.na(d)]
    dl <- delimiters
    if (is.null(dl)) {
      s <- ai_suggest_delimiters(d)
      dl <- if (nrow(s) && s$score[1] > 0) s$delimiter[1] else "_"
    }
    st <- ai_shape_table(d, dl, shape_by)
    shapes <- list()
    for (i in seq_len(nrow(st$shapes)))
      shapes[[st$shapes$shape_key[i]]] <-
        ai_propose_bindings(t, st$shapes$example[i], dl, shape_by)
    rules[[t]] <- ai_shape_rule(t, dl, shape_by, shapes, NULL,
                                st$content_positions)
  }
  rules
}

#' Build a rule for a single specimen type.
#'
#' @param inherit optional rule to take delimiters and keying from. A new type
#'   appearing mid-session belongs to the SAME submitter as the types already
#'   configured, so its separator is almost certainly the same one. Inheriting
#'   it means a user who retypes two wells to Control does not have to rediscover
#'   the delimiter for a two-well corpus, where the suggestion scorer has almost
#'   nothing to work from.
ai_rule_new_for_type <- function(inv, type, inherit = NULL) {
  sel  <- !is.na(inv$specimen_type) & inv$specimen_type == type
  desc <- inv$description[sel]

  if (!is.null(inherit)) {
    dl <- inherit$delimiters
    sb <- inherit$shape_by
  } else {
    d2 <- desc[!is.na(desc)]
    sg <- if (length(d2)) ai_suggest_delimiters(d2) else NULL
    dl <- if (!is.null(sg) && nrow(sg) && sg$score[1] > 0) sg$delimiter[1] else "_"
    sb <- "format"
  }

  st <- ai_shape_table(desc, dl, sb)
  shapes <- list()
  for (i in seq_len(nrow(st$shapes)))
    shapes[[st$shapes$shape_key[i]]] <-
      ai_propose_bindings(type, st$shapes$example[i], dl, sb)
  ai_shape_rule(type, dl, sb, shapes, NULL, st$content_positions)
}

#' Keep a ruleset in step with the inventory's CURRENT specimen types.
#'
#' ai_ruleset_init() runs once, so it only ever covers the types present when
#' the files were first parsed. Editing wells on the plate grid changes that
#' set: retyping two wells to Control introduced a type with no rule at all,
#' which surfaced as a Controls tab with no description groups -- nothing to
#' apply, nothing to approve, and a gate that could never open.
#'
#' Called on every inventory change. For each type now present:
#'   * no rule yet          -> build one, inheriting delimiters from a
#'                             configured type
#'   * rule exists, corpus changed -> refresh its shapes, KEEPING the bindings
#'                             of every shape key that survives
#'   * rule exists, corpus unchanged -> left completely alone
#' Types that are no longer present keep their rules (dormant), so retyping a
#' well back does not throw away work.
#'
#' @param fingerprints named list of the sorted unique descriptions last seen
#'   per type; pass the value returned in $fingerprints to skip untouched types.
#' @return list(rules, fingerprints, added, refreshed, dormant)
ai_ruleset_sync <- function(inv, rules = list(), fingerprints = list()) {
  out <- list(rules = rules, fingerprints = fingerprints,
              added = character(), refreshed = character(),
              dormant = character())
  if (is.null(inv) || !nrow(inv)) return(out)

  present <- intersect(AI_SPECIMEN_TYPES,
                       unique(stats::na.omit(inv$specimen_type)))
  if (!length(present)) return(out)

  # a configured type to inherit conventions from: prefer one with bindings
  donor <- NULL
  for (t in present) {
    r <- rules[[t]]
    if (!is.null(r) && length(r$shapes)) { donor <- r; break }
  }
  if (is.null(donor))
    for (t in names(rules)) if (!is.null(rules[[t]])) { donor <- rules[[t]]; break }

  for (t in present) {
    sel <- !is.na(inv$specimen_type) & inv$specimen_type == t
    fp  <- sort(unique(as.character(inv$description[sel])), na.last = TRUE)

    if (is.null(rules[[t]])) {
      out$rules[[t]] <- ai_rule_new_for_type(inv, t, inherit = donor)
      out$added <- c(out$added, t)
    } else if (!identical(fp, fingerprints[[t]])) {
      # the corpus moved: new strings need proposed bindings, vanished shapes
      # must go, and surviving keys keep whatever the user bound
      out$rules[[t]] <- tryCatch(
        ai_rule_refresh_shapes(rules[[t]], inv$description[sel]),
        error = function(e) rules[[t]])
      out$refreshed <- c(out$refreshed, t)
    }
    out$fingerprints[[t]] <- fp
  }

  out$dormant <- setdiff(names(out$rules), present)
  out
}

#' Shape keys a ruleset no longer contains, so stale approvals can be pruned.
#'
#' An approval is per (type, shape_key). When a corpus change drops or re-keys a
#' shape, its approval must go too, or a type can read as fully approved on the
#' strength of groups that no longer exist.
ai_stale_approvals <- function(rules, approved) {
  if (!length(approved)) return(character())
  live <- unlist(lapply(names(rules), function(t)
    paste(t, names(rules[[t]]$shapes), sep = "\r")), use.names = FALSE)
  setdiff(names(approved), live %||% character())
}

#' Present specimen types that cannot currently be approved, and why.
#'
#' The diagnostic for exactly the reported fault: a type in the inventory with
#' no rule, or a rule with no description groups, can never be approved, and
#' before this it showed as an empty tab with no explanation.
ai_ruleset_coverage <- function(inv, rules) {
  empty <- data.frame(specimen_type = character(), n_wells = integer(),
                      status = character(), message = character(),
                      stringsAsFactors = FALSE)
  if (is.null(inv) || !nrow(inv)) return(empty)
  present <- intersect(AI_SPECIMEN_TYPES,
                       unique(stats::na.omit(inv$specimen_type)))
  if (!length(present)) return(empty)

  rows <- list()
  for (t in present) {
    sel <- !is.na(inv$specimen_type) & inv$specimen_type == t
    nw  <- sum(sel)
    r   <- rules[[t]]
    if (is.null(r)) {
      rows[[length(rows) + 1L]] <- data.frame(
        specimen_type = t, n_wells = nw, status = "no_rule",
        message = sprintf("%s (%s) has %d well(s) but no description rule yet.",
                          AI_SPECIMEN_LABELS[[t]], t, nw),
        stringsAsFactors = FALSE)
      next
    }
    st <- tryCatch(ai_shape_table(inv$description[sel], r$delimiters, r$shape_by),
                   error = function(e) NULL)
    if (is.null(st) || !nrow(st$shapes))
      rows[[length(rows) + 1L]] <- data.frame(
        specimen_type = t, n_wells = nw, status = "no_groups",
        message = sprintf("%s (%s) has %d well(s) but no description groups.",
                          AI_SPECIMEN_LABELS[[t]], t, nw),
        stringsAsFactors = FALSE)
  }
  if (!length(rows)) return(empty)
  do.call(rbind, rows)
}

#' Recompute a type's shapes after its delimiters or keying changed.
#'
#' Bindings for shape keys that still exist are KEPT (the user's work survives a
#' delimiter tweak); brand-new keys get proposed bindings; vanished keys are
#' dropped.
ai_rule_refresh_shapes <- function(rule, descriptions) {
  st <- ai_shape_table(descriptions, rule$delimiters, rule$shape_by)
  keep <- rule$shapes[intersect(names(rule$shapes), st$shapes$shape_key)]
  for (i in seq_len(nrow(st$shapes))) {
    k <- st$shapes$shape_key[i]
    if (is.null(keep[[k]]))
      keep[[k]] <- ai_propose_bindings(rule$type, st$shapes$example[i],
                                       rule$delimiters, rule$shape_by)
  }
  rule$shapes <- keep
  rule$content_positions <- st$content_positions
  rule
}


# =============================================================================
# RESOLUTION
# =============================================================================

.ai_eval_binding <- function(b, toks, cls, type_code) {
  if (is.null(b) || identical(b$how, "ignore")) return("")
  switch(b$how,
    slot = {
      idx <- b$slot[b$slot >= 1L & b$slot <= length(toks)]
      if (!length(idx)) "" else paste(toks[idx], collapse = b$join %||% " ")
    },
    pattern = {
      hit <- which(cls %in% b$class)
      if (!length(hit)) "" else toks[hit[1]]
    },
    constant  = as.character(b$value %||% ""),
    from_type = {
      s <- if (exists("ai_type_suffix", mode = "function"))
        ai_type_suffix(type_code) else substring(trimws(as.character(type_code)), 2L)
      if (is.na(s)) "" else s
    },
    "")
}

#' Resolve one description under a rule.
#'
#' @param instrument_dilution the reader's own authoritative per-well dilution
#'   (e.g. .rbx/.srbx's binary dilution field), or NA if the format/well has
#'   none. When finite and > 0, and the well is a Sample (X) or Control (C),
#'   this WINS over whatever the description text would otherwise resolve to,
#'   and the well is never flagged for a missing/bad DilutionFactor. Never
#'   applied to Standards (S) or Blanks (B): the Bio-Plex binary's numeric
#'   dilution field is a constant placeholder for Standards (confirmed empty
#'   of information against two real files -- see
#'   RBX_DILUTION_AUTHORITATIVE_SOURCE_PLAN.md), and Blanks already resolve
#'   correctly without it.
#' @param reference_dilution a value from the experiment-scoped Standards
#'   reference table (assay_std_reference_rules.R), or NA if none matches this
#'   description. A second, LOWER-priority fallback: only applied when
#'   neither the instrument override nor the text resolves the dilution. Not
#'   type-gated like `instrument_dilution` -- any type whose DilutionFactor
#'   survives both checks unresolved is eligible (the generalizable pattern
#'   from RBX_DILUTION_AUTHORITATIVE_SOURCE_PLAN.md's design; Standards are
#'   simply the first, and so far only, real consumer).
#' @return list(shape_key, matched, values (named chr over AI_COMPONENTS),
#'   dilution_value, dilution_ok,
#'   dilution_src = "text"|"instrument"|"reference"|NA (which source actually
#'   won, for UI provenance display -- see assay_shape_ui.R's preview table),
#'   issues data.frame(component, severity, message, kind))
ai_resolve_one <- function(description, type_code, rule, instrument_dilution = NA_real_,
                           reference_dilution = NA_real_, instrument_source = NA_character_) {
  tc <- if (exists("ai_type_letter", mode = "function"))
    ai_type_letter(type_code) else toupper(substr(trimws(as.character(type_code)), 1L, 1L))

  vals <- stats::setNames(rep("", length(AI_COMPONENTS)), AI_COMPONENTS)
  iss  <- list()
  add  <- function(comp, sev, msg, kind)
    iss[[length(iss) + 1L]] <<- data.frame(
      component = comp, severity = sev, message = msg, kind = kind,
      stringsAsFactors = FALSE)

  out <- list(shape_key = NA_character_, matched = FALSE, values = vals,
              dilution_value = NA_integer_, dilution_ok = FALSE,
              dilution_src = NA_character_, issues = NULL)

  blank <- is.na(description) || !nzchar(trimws(as.character(description))) ||
           identical(trimws(as.character(description)), "NA")
  toks  <- if (blank) character(0) else .ai_sr_split(description, rule$delimiters)
  cls   <- ai_token_class(toks)
  key   <- ai_shape_key_one(description, rule)   # blank -> AI_EMPTY_SHAPE_KEY
  out$shape_key <- key

  bindings <- if (!is.na(key)) rule$shapes[[key]] else NULL
  if (is.null(bindings)) bindings <- rule$fallback

  if (is.null(bindings)) {
    add(NA_character_, "error",
        if (blank) sprintf("Type %s well has no description; set constant values for the \"%s\" group.",
                           tc, AI_EMPTY_SHAPE_KEY)
        else sprintf("No rule for description shape '%s'.", key),
        if (blank) "blank_description" else "unmatched_shape")
    out$issues <- do.call(rbind, iss)
    return(out)
  }
  out$matched <- !is.na(key) && !is.null(rule$shapes[[key]])

  for (comp in names(bindings))
    if (comp %in% AI_COMPONENTS)
      vals[[comp]] <- .ai_eval_binding(bindings[[comp]], toks, cls, type_code)

  # Instrument override for Source: authoritative when supplied, wins over
  # whatever the text binding produced -- same philosophy as DilutionFactor's
  # instrument override below, but not type-gated (there's no placeholder
  # quirk for source the way the Bio-Plex binary's dilution field has).
  if (!is.na(instrument_source) && nzchar(trimws(instrument_source)))
    vals[["Source"]] <- trimws(instrument_source)

  # dilution normalises to the integer denominator: 1:100 / 1/100 / 100 -> 100
  dil <- .ai_sr_dilution(if (nzchar(vals[["DilutionFactor"]])) vals[["DilutionFactor"]] else NA_character_)
  out$dilution_value <- dil$value
  out$dilution_ok    <- isTRUE(dil$ok)
  if (isTRUE(out$dilution_ok)) out$dilution_src <- "text"

  # Instrument override: authoritative for Samples/Controls, wins over text,
  # and the well is never faulted for its DilutionFactor below. See the
  # @param note above for why this is X/C-only.
  inst_ok <- is.finite(instrument_dilution) && instrument_dilution > 0 &&
             tc %in% c("X", "C")
  if (inst_ok) {
    out$dilution_value <- as.integer(instrument_dilution)
    out$dilution_ok    <- TRUE
    out$dilution_src   <- "instrument"
  }

  # Experiment-scoped reference table: a second, lower-priority fallback --
  # only applied when neither the instrument override nor the text already
  # resolved it. See @param reference_dilution above for why this one isn't
  # type-gated.
  ref_ok <- !inst_ok && !isTRUE(dil$ok) &&
            is.finite(reference_dilution) && reference_dilution > 0
  if (ref_ok) {
    out$dilution_value <- as.integer(reference_dilution)
    out$dilution_ok    <- TRUE
    out$dilution_src   <- "reference"
  }

  if (!inst_ok && !ref_ok && nzchar(vals[["DilutionFactor"]]) && !isTRUE(dil$ok)) {
    add("DilutionFactor", "error",
        sprintf("Dilution '%s' is not an integer or a 1:N ratio.",
                vals[["DilutionFactor"]]), "bad_dilution")
  }
  if (isTRUE(out$dilution_ok)) vals[["DilutionFactor"]] <- as.character(out$dilution_value)

  req <- AI_TYPE_REQUIRED[[if (tc %in% names(AI_TYPE_REQUIRED)) tc else "X"]]
  dilution_satisfied <- inst_ok || ref_ok
  for (comp in req)
    if (!(comp == "DilutionFactor" && dilution_satisfied) && !nzchar(vals[[comp]]))
      add(comp, "error", sprintf("Type %s: %s is empty.", tc, comp),
          "missing_required")

  out$values <- vals
  out$issues <- if (length(iss)) do.call(rbind, iss) else NULL
  out
}

#' Verdict for one shape: does it satisfy its type's contract for every string?
#'
#' @param descriptions the strings belonging to this shape (one per well).
#' @param instrument_dilution optional, same length/order as `descriptions`:
#'   each well's instrument-sourced dilution (NA where none). A distinct
#'   string counts as instrument-covered only if EVERY well carrying it has a
#'   valid value -- a conservative choice; a well whose text would otherwise
#'   fail still fails here if even one of its sibling wells lacks coverage,
#'   even though ai_resolve_inventory() always applies the real per-well
#'   value regardless of what this gate decides.
#' @param reference_dilution optional, same shape as `instrument_dilution`,
#'   sourced from the experiment-scoped Standards reference table instead of
#'   the instrument file. Same "every well must be covered" conservatism.
#' @param instrument_source optional, same length/order as `descriptions`:
#'   each well's instrument-sourced Source label (NA where none). A distinct
#'   string counts as covered only if every well carrying it agrees on the
#'   SAME value -- same conservatism as the dilution overrides.
#' @return list(ok, n_strings, n_failing, failing_examples, missing_components)
ai_shape_verdict <- function(type, shape_key, descriptions, rule,
                             instrument_dilution = NULL, reference_dilution = NULL,
                             instrument_source = NULL) {
  d_all <- as.character(descriptions)
  keep  <- !is.na(d_all)
  d_all <- d_all[keep]
  inst  <- if (is.null(instrument_dilution)) rep(NA_real_, length(d_all))
           else as.numeric(instrument_dilution)[keep]
  ref   <- if (is.null(reference_dilution)) rep(NA_real_, length(d_all))
           else as.numeric(reference_dilution)[keep]
  src   <- if (is.null(instrument_source)) rep(NA_character_, length(d_all))
           else as.character(instrument_source)[keep]
  if (!length(d_all))
    return(list(ok = TRUE, n_strings = 0L, n_failing = 0L,
                failing_examples = character(), missing_components = character()))
  d <- unique(d_all)
  miss <- character(); fail <- character()
  for (s in d) {
    in_s <- d_all == s
    # NA if ANY well sharing this string lacks coverage from that source.
    cover_inst <- suppressWarnings(min(inst[in_s], na.rm = FALSE))
    cover_ref  <- suppressWarnings(min(ref[in_s],  na.rm = FALSE))
    # character analogue: covered only if every well sharing this string
    # agrees on one non-NA value.
    src_u <- unique(src[in_s])
    cover_src <- if (length(src_u) == 1L) src_u else NA_character_
    r <- ai_resolve_one(s, type, rule, instrument_dilution = cover_inst,
                        reference_dilution = cover_ref, instrument_source = cover_src)
    if (!is.null(r$issues) && any(r$issues$severity == "error")) {
      fail <- c(fail, s)
      miss <- c(miss, stats::na.omit(r$issues$component))
    }
  }
  list(ok = !length(fail), n_strings = length(d), n_failing = length(fail),
       failing_examples = utils::head(unique(fail), 3L),
       missing_components = unique(miss))
}

#' Resolve the whole inventory to the identity columns the template expects.
#'
#' Output column names match what build_plates_map() already writes, so nothing
#' downstream is renamed:
#'   subject_id, timepoint_tissue_abbreviation, specimen_dilution_factor,
#'   specimen_source, groupa, groupb, biosample_id_barcode
#' Keyed on (plateid, well) -- plateid is the inventory's plate_key, which is the
#' reader's plateid, so this merges straight onto plate_well_map.
#'
#' @param reference optional data.frame(specimen_type, description, dilution)
#'   from the experiment-scoped Standards reference table
#'   (assay_std_reference_rules.R / assay_std_reference_ui.R). Matched by
#'   exact, trimmed (specimen_type, description) -- the same key the
#'   reference-entry UI groups candidates by.
#' @return list(resolved = data.frame, issues = data.frame(sheet, severity,
#'   column, message))
ai_resolve_inventory <- function(inv, ruleset, reference = NULL) {
  occupied <- !is.na(inv$type_code) & !is.na(inv$specimen_type)
  d <- inv[occupied, , drop = FALSE]
  empty_iss <- data.frame(sheet = character(), severity = character(),
                          column = character(), message = character(),
                          stringsAsFactors = FALSE)
  if (!nrow(d))
    return(list(resolved = data.frame(), issues = empty_iss))

  n <- nrow(d)
  ref_dilution <- rep(NA_real_, n)
  if (!is.null(reference) && nrow(reference)) {
    key_d   <- paste(d$specimen_type, trimws(as.character(d$description)))
    key_ref <- paste(reference$specimen_type, trimws(as.character(reference$description)))
    ref_dilution <- suppressWarnings(as.numeric(reference$dilution[match(key_d, key_ref)]))
  }
  res <- data.frame(
    plateid                       = d$plate_key,
    well                          = d$well,
    specimen_type                 = d$specimen_type,
    type_code                     = d$type_code,
    description                   = d$description,
    shape_key                     = rep(NA_character_, n),
    subject_id                    = rep("", n),
    timepoint_tissue_abbreviation = rep("", n),
    specimen_dilution_factor      = rep(NA_integer_, n),
    specimen_source               = rep("", n),
    groupa                        = rep("", n),
    groupb                        = rep("", n),
    biosample_id_barcode          = rep("", n),
    stringsAsFactors = FALSE)

  rows <- list()
  for (i in seq_len(n)) {
    t <- d$specimen_type[i]
    rule <- ruleset[[t]]
    if (is.null(rule)) {
      rows[[length(rows) + 1L]] <- data.frame(
        sheet = "description_rules", severity = "error", column = "specimen_type",
        message = sprintf("No rule configured for specimen type %s (well %s, plate %s).",
                          t, d$well[i], d$plate_key[i]),
        stringsAsFactors = FALSE)
      next
    }
    r <- ai_resolve_one(d$description[i], d$type_code[i], rule,
                        instrument_dilution = d$instrument_dilution[i],
                        reference_dilution = ref_dilution[i],
                        instrument_source = if ("instrument_source" %in% names(d))
                          d$instrument_source[i] else NA_character_)
    res$shape_key[i]                     <- r$shape_key
    res$subject_id[i]                    <- r$values[["PatientID"]]
    res$timepoint_tissue_abbreviation[i] <- r$values[["TimePeriod"]]
    res$specimen_source[i]               <- r$values[["Source"]]
    res$groupa[i]                        <- r$values[["SampleGroupA"]]
    res$groupb[i]                        <- r$values[["SampleGroupB"]]
    res$specimen_dilution_factor[i]      <- r$dilution_value

    # S3 -> standard point 3, C1 -> control 1: the index rides in the type code,
    # exactly as the existing parser reads it
    sfx <- if (exists("ai_type_suffix", mode = "function"))
      ai_type_suffix(d$type_code[i]) else substring(trimws(d$type_code[i]), 2L)
    res$biosample_id_barcode[i] <- if (is.na(sfx)) "" else sfx
    if (t != "X" && !nzchar(res$subject_id[i]))
      res$subject_id[i] <- if (nzchar(sfx %||% "")) sfx else "1"

    if (!is.null(r$issues))
      for (j in seq_len(nrow(r$issues)))
        rows[[length(rows) + 1L]] <- data.frame(
          sheet = "description_rules", severity = r$issues$severity[j],
          column = r$issues$component[j] %||% NA_character_,
          message = sprintf("%s (well %s, plate %s, Type %s, Description '%s')",
                            r$issues$message[j], d$well[i], d$plate_key[i],
                            d$type_code[i],
                            if (is.na(d$description[i])) "" else d$description[i]),
          stringsAsFactors = FALSE)
  }

  list(resolved = res,
       issues = if (length(rows)) do.call(rbind, rows) else empty_iss)
}

#' Per-type roll-up: is every shape of every present type satisfied?
#'
#' @param reference optional data.frame(specimen_type, description, dilution)
#'   -- see ai_resolve_inventory(). Matched the same way, per type, so a shape
#'   satisfied only via the Standards reference table doesn't block approval.
ai_ruleset_ready <- function(inv, ruleset, approved = NULL, reference = NULL) {
  types <- intersect(AI_SPECIMEN_TYPES, unique(stats::na.omit(inv$specimen_type)))
  if (!length(types)) return(FALSE)
  for (t in types) {
    rule <- ruleset[[t]]
    if (is.null(rule)) return(FALSE)
    sel <- !is.na(inv$specimen_type) & inv$specimen_type == t
    d    <- inv$description[sel]
    dils <- inv$instrument_dilution[sel]
    refs <- rep(NA_real_, length(d))
    if (!is.null(reference) && nrow(reference)) {
      key_d   <- paste(t, trimws(as.character(d)))
      key_ref <- paste(reference$specimen_type, trimws(as.character(reference$description)))
      refs <- suppressWarnings(as.numeric(reference$dilution[match(key_d, key_ref)]))
    }
    st <- ai_shape_table(d, rule$delimiters, rule$shape_by)
    if (!nrow(st$shapes)) return(FALSE)
    for (i in seq_len(nrow(st$shapes))) {
      k <- st$shapes$shape_key[i]
      if (is.null(rule$shapes[[k]]) && is.null(rule$fallback)) return(FALSE)
      in_shape <- !is.na(st$keys) & st$keys == k
      strs <- d[in_shape]
      if (!ai_shape_verdict(t, k, strs, rule, instrument_dilution = dils[in_shape],
                           reference_dilution = refs[in_shape])$ok)
        return(FALSE)
      if (!is.null(approved) && !isTRUE(approved[[paste(t, k, sep = "\r")]]))
        return(FALSE)
    }
  }
  TRUE
}


# =============================================================================
# PORTABLE PROFILES
# -----------------------------------------------------------------------------
# A profile is a named ruleset in a file the user downloads and re-uploads for
# the next batch from the same submitter. No database, no schema migration: the
# file travels with the data. YAML by default because a lab will hand-edit it;
# JSON accepted too (extension decides).
# =============================================================================

#' Wrap a ruleset as a profile object.
ai_ruleset_to_profile <- function(ruleset, name, notes = "", assay = NA_character_) {
  list(profile_version = AI_PROFILE_VERSION,
       name = as.character(name)[1],
       notes = as.character(notes)[1],
       assay = as.character(assay)[1],
       created = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
       rules = ruleset)
}

#' Write a profile to .yaml / .yml / .json. Extension chooses the format.
ai_write_profile <- function(path, ruleset, name, notes = "",
                             assay = NA_character_) {
  prof <- ai_ruleset_to_profile(ruleset, name, notes, assay)
  ext <- tolower(sub("^.*\\.", "", path))
  if (ext == "json") {
    if (!requireNamespace("jsonlite", quietly = TRUE))
      stop("jsonlite is needed to write a .json profile", call. = FALSE)
    writeLines(jsonlite::toJSON(prof, auto_unbox = TRUE, pretty = TRUE,
                                null = "null"), path)
  } else {
    if (!requireNamespace("yaml", quietly = TRUE))
      stop("yaml is needed to write a .yaml profile", call. = FALSE)
    writeLines(yaml::as.yaml(prof), path)
  }
  invisible(path)
}

#' Read a profile file and validate its structure.
#'
#' @return list(name, notes, assay, created, rules) -- rules is a ruleset.
ai_read_profile <- function(path) {
  ext <- tolower(sub("^.*\\.", "", path))
  prof <- if (ext == "json") {
    if (!requireNamespace("jsonlite", quietly = TRUE))
      stop("jsonlite is needed to read a .json profile", call. = FALSE)
    jsonlite::fromJSON(path, simplifyVector = TRUE, simplifyDataFrame = FALSE)
  } else {
    if (!requireNamespace("yaml", quietly = TRUE))
      stop("yaml is needed to read a .yaml profile", call. = FALSE)
    yaml::yaml.load_file(path)
  }
  if (is.null(prof$rules) || !length(prof$rules))
    stop("that file has no `rules` section -- it is not a parse profile",
         call. = FALSE)
  v <- suppressWarnings(as.integer(prof$profile_version %||% NA))
  if (!is.na(v) && v > AI_PROFILE_VERSION)
    stop(sprintf("profile was written by a newer version (%d > %d)",
                 v, AI_PROFILE_VERSION), call. = FALSE)

  # normalise: JSON/YAML round-trips turn scalars into lists and integers into
  # doubles, so rebuild every rule and binding through the constructors. This is
  # also the validation -- a malformed binding stops here, not mid-import.
  prof$rules <- stats::setNames(lapply(names(prof$rules), function(t) {
    r <- prof$rules[[t]]
    shapes <- stats::setNames(lapply(names(r$shapes %||% list()), function(k) {
      b <- r$shapes[[k]]
      stats::setNames(lapply(names(b), function(cm) {
        bb <- b[[cm]]
        ai_binding(how = as.character(bb$how)[1],
                   slot = if (!is.null(bb$slot)) as.integer(unlist(bb$slot)) else NULL,
                   class = if (!is.null(bb$class)) as.character(unlist(bb$class)) else NULL,
                   value = if (!is.null(bb$value)) as.character(bb$value)[1] else NULL,
                   join = as.character(bb$join %||% " ")[1])
      }), names(b))
    }), names(r$shapes %||% list()))
    cp <- stats::setNames(lapply(r$content_positions %||% list(),
                                 function(p) as.integer(unlist(p))),
                          names(r$content_positions %||% list()))
    ai_shape_rule(type = as.character(r$type %||% t)[1],
                  delimiters = paste0(as.character(unlist(r$delimiters %||% "_")),
                                      collapse = ""),
                  shape_by = as.character(r$shape_by %||% "format")[1],
                  shapes = shapes, fallback = r$fallback,
                  content_positions = cp)
  }), names(prof$rules))
  prof
}

#' Apply a profile's rules to this batch and report the fit.
#'
#' A profile from a previous batch will not cover a shape that batch did not
#' contain, and this batch may not contain shapes the profile knows. Say so
#' rather than quietly falling through: an unmatched shape is the exact place a
#' wrong import starts.
#'
#' @return list(rules, report = data.frame(specimen_type, shape_key, status,
#'   n_wells, example)) where status is "from_profile" | "new_shape" |
#'   "unused_in_batch".
ai_profile_apply <- function(profile_rules, inv) {
  types <- intersect(AI_SPECIMEN_TYPES, unique(stats::na.omit(inv$specimen_type)))
  rules <- list(); rows <- list()

  for (t in types) {
    d <- inv$description[!is.na(inv$specimen_type) & inv$specimen_type == t]
    pr <- profile_rules[[t]]
    if (is.null(pr)) {                       # profile says nothing about this type
      rules[[t]] <- ai_ruleset_init(inv[!is.na(inv$specimen_type) &
                                        inv$specimen_type == t, , drop = FALSE])[[t]]
      st <- ai_shape_table(d, rules[[t]]$delimiters, rules[[t]]$shape_by)
      for (i in seq_len(nrow(st$shapes)))
        rows[[length(rows) + 1L]] <- data.frame(
          specimen_type = t, shape_key = st$shapes$shape_key[i],
          status = "new_shape", n_wells = st$shapes$n_wells[i],
          example = st$shapes$example[i], stringsAsFactors = FALSE)
      next
    }

    st <- ai_shape_table(d, pr$delimiters, pr$shape_by)
    kept <- list()
    for (i in seq_len(nrow(st$shapes))) {
      k <- st$shapes$shape_key[i]
      if (!is.null(pr$shapes[[k]])) {
        kept[[k]] <- pr$shapes[[k]]; status <- "from_profile"
      } else {
        kept[[k]] <- ai_propose_bindings(t, st$shapes$example[i],
                                         pr$delimiters, pr$shape_by)
        status <- "new_shape"
      }
      rows[[length(rows) + 1L]] <- data.frame(
        specimen_type = t, shape_key = k, status = status,
        n_wells = st$shapes$n_wells[i], example = st$shapes$example[i],
        stringsAsFactors = FALSE)
    }
    for (k in setdiff(names(pr$shapes), names(kept)))
      rows[[length(rows) + 1L]] <- data.frame(
        specimen_type = t, shape_key = k, status = "unused_in_batch",
        n_wells = 0L, example = "", stringsAsFactors = FALSE)

    pr$shapes <- kept
    pr$content_positions <- st$content_positions
    rules[[t]] <- pr
  }

  list(rules = rules,
       report = if (length(rows)) do.call(rbind, rows) else
         data.frame(specimen_type = character(), shape_key = character(),
                    status = character(), n_wells = integer(),
                    example = character(), stringsAsFactors = FALSE))
}

#' Flatten a ruleset for the workbook's `parse_rule` audit sheet.
ai_shape_ruleset_to_sheet <- function(ruleset,
                                      stamped_at = format(Sys.time())) {
  rows <- list()
  for (t in names(ruleset)) {
    r <- ruleset[[t]]
    for (k in names(r$shapes)) {
      b <- r$shapes[[k]]
      for (cm in names(b)) {
        bb <- b[[cm]]
        rows[[length(rows) + 1L]] <- data.frame(
          specimen_type = t, shape_key = k, component = cm, how = bb$how,
          slot  = if (is.null(bb$slot))  "" else paste(bb$slot, collapse = "+"),
          class = if (is.null(bb$class)) "" else paste(bb$class, collapse = "|"),
          value = if (is.null(bb$value)) "" else bb$value,
          delimiters = paste0(r$delimiters, collapse = ""),
          shape_by = r$shape_by, stamped_at = stamped_at,
          stringsAsFactors = FALSE)
      }
    }
  }
  if (!length(rows))
    return(data.frame(specimen_type = character(), shape_key = character(),
                      component = character(), how = character(),
                      slot = character(), class = character(),
                      value = character(), delimiters = character(),
                      shape_by = character(), stamped_at = character(),
                      stringsAsFactors = FALSE))
  do.call(rbind, rows)
}


# =============================================================================
# APPLYING THE RESOLUTION TO A TEMPLATE SHEET
# -----------------------------------------------------------------------------
# The seam that makes the pre-processor authoritative. Template generators used
# to re-derive identity from the raw Description (build_plates_map ->
# parse_all_descriptions), which meant two code paths could disagree and the
# workbook's won. They now merge the RESOLVED table instead.
#
# Wells absent from `resolved` are empty wells: specimen_type is blanked and the
# identity columns are left empty rather than defaulted, because inventing a
# subject for an unused well is how phantom rows reach the database.
# =============================================================================

AI_RESOLVED_COLS <- c("specimen_type", "subject_id",
                      "timepoint_tissue_abbreviation",
                      "specimen_dilution_factor", "specimen_source",
                      "groupa", "groupb", "biosample_id_barcode")

#' Overwrite a sheet's identity columns from the resolved inventory.
#'
#' @param df       a plates_map-like frame carrying plate and well columns.
#' @param resolved ai_resolve_inventory()$resolved.
#' @param plate_col name of df's plate key column (default "plateid").
#' @param well_col  name of df's well column (default "well").
#' @param keep_description overwrite df$Description from the (possibly edited)
#'   inventory description. TRUE for bead, whose Description column rides along
#'   from the raw file and would otherwise contradict the grid's edits.
#' @return df with AI_RESOLVED_COLS set, plus `shape_key` for audit.
ai_merge_resolved <- function(df, resolved, plate_col = "plateid",
                              well_col = "well", keep_description = TRUE) {
  if (is.null(df) || !nrow(df)) return(df)
  if (is.null(resolved) || !nrow(resolved)) return(df)
  if (!all(c(plate_col, well_col) %in% names(df)))
    stop(sprintf("ai_merge_resolved(): '%s' and '%s' are required in the sheet",
                 plate_col, well_col), call. = FALSE)

  key_df  <- paste(as.character(df[[plate_col]]),
                   ai_normalize_well(df[[well_col]]), sep = "\r")
  key_res <- paste(as.character(resolved$plateid),
                   ai_normalize_well(resolved$well), sep = "\r")
  i <- match(key_df, key_res)

  df$specimen_type <- ifelse(is.na(i), "", resolved$specimen_type[i])
  for (cl in c("subject_id", "timepoint_tissue_abbreviation",
               "specimen_source", "groupa", "groupb", "biosample_id_barcode")) {
    v <- ifelse(is.na(i), "", as.character(resolved[[cl]][i]))
    v[is.na(v)] <- ""
    df[[cl]] <- v
  }
  df$specimen_dilution_factor <- suppressWarnings(
    as.numeric(resolved$specimen_dilution_factor[i]))
  df$shape_key <- ifelse(is.na(i), NA_character_, resolved$shape_key[i])

  if (isTRUE(keep_description)) {
    dsc <- ifelse(is.na(i), NA_character_, resolved$description[i])
    if ("Description" %in% names(df)) df$Description <- dsc else df$Description <- dsc
    if ("Type" %in% names(df))
      df$Type <- ifelse(is.na(i), NA_character_, resolved$type_code[i])
  }
  df
}

#' Unresolved dilutions, grouped for the stage-4 planner.
#'
#' Bridges the resolution to the existing three-state dilution machinery in
#' assay_description_parse.R: ai_dilution_plan() decides parsed / constant /
#' per_description, and only the descriptions listed here need a typed value.
ai_dilution_gaps <- function(resolved) {
  if (is.null(resolved) || !nrow(resolved))
    return(data.frame(specimen_type = character(), description = character(),
                      n_wells = integer(), stringsAsFactors = FALSE))
  bad <- is.na(resolved$specimen_dilution_factor) |
         resolved$specimen_dilution_factor <= 0
  if (!any(bad))
    return(data.frame(specimen_type = character(), description = character(),
                      n_wells = integer(), stringsAsFactors = FALSE))
  d <- resolved[bad, , drop = FALSE]
  key <- paste(d$specimen_type, d$description, sep = "\r")
  u <- !duplicated(key)
  out <- data.frame(specimen_type = d$specimen_type[u],
                    description = d$description[u],
                    n_wells = as.integer(table(factor(key, levels = key[u]))[key[u]]),
                    stringsAsFactors = FALSE)
  out[order(-out$n_wells, out$specimen_type), , drop = FALSE]
}
