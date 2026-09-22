# =============================================================================
# test-assay-shape-rules.R
# -----------------------------------------------------------------------------
# Tests for the shape-scoped description engine, pinned to the SAME real .rbx
# descriptions as test-assay-description-parse.R. That corpus is the reason
# these tests exist: three of the assertions below correspond to bugs the real
# strings exposed in the first draft of ai_propose_bindings() and
# ai_token_class().
#
#   "051 V1"                 patient id IS an integer -> must not be proposed
#                            as the dilution
#   "Inhouse Ref 1:2952450"  source is TWO tokens -> must bind as a span
#   "QC1 (Low) 1:2500"       parenthesised qualifier -> must classify as `code`
#                            and stay with the source
#
# Run: testthat::test_file("test-assay-shape-rules.R")
#
# NOTE: written without an R interpreter available. Treat a failure here as a
# question about which side is wrong, not as a settled verdict on the code.
# =============================================================================

if (!exists("ai_shape_rule")) {
  cand <- c("assay_shape_rules.R", "src/assay_shape_rules.R",
            "../../assay_shape_rules.R", "../../../src/assay_shape_rules.R")
  hit <- cand[file.exists(cand)][1]
  if (is.na(hit)) stop("cannot find assay_shape_rules.R; edit the path in this test")
  # the inventory module supplies ai_normalize_well / ai_type_letter, which the
  # resolver and ai_merge_resolved() both use
  inv <- sub("assay_shape_rules\\.R$", "assay_well_inventory.R", hit)
  if (file.exists(inv)) source(inv)
  par <- sub("assay_shape_rules\\.R$", "assay_description_parse.R", hit)
  if (file.exists(par)) source(par)
  source(hit)
}

library(testthat)

# ---- the real corpus --------------------------------------------------------
RBX_X <- c("051 V1", "80 V1", "051 V2", "IFO 100")
RBX_S <- c("Inhouse Ref 1:2952450", "Inhouse Ref 1:984150",
           "Inhouse Ref 1:328050", "Inhouse Ref 1:150")
RBX_C <- c("QC1 (Low) 1:2500", "QC2 (High) 1:2500")
RBX_B <- c("blank", "Blank1")
XPONENT_X <- c("PT01_T0_100", "PT02_T0_100", "PT03_T0")


# ---- token classification ---------------------------------------------------

test_that("token classes cover the real vocabulary", {
  expect_equal(ai_token_class("051"),       "integer")
  expect_equal(ai_token_class("V1"),        "timepoint")
  expect_equal(ai_token_class("V2"),        "timepoint")
  expect_equal(ai_token_class("T0"),        "timepoint")
  expect_equal(ai_token_class("IFO"),       "alpha")
  expect_equal(ai_token_class("PT01"),      "alnum")
  expect_equal(ai_token_class("1:2952450"), "ratio")
  expect_equal(ai_token_class("1/100"),     "ratio")
  expect_equal(ai_token_class("100.5"),     "decimal")
  # BUG FIX: a parenthesised qualifier must not fall through to `mixed`, or
  # every control label gets a shape of its own
  expect_equal(ai_token_class("(Low)"),     "code")
  expect_equal(ai_token_class("(High)"),    "code")
  expect_equal(ai_token_class("QC1"),       "alnum")
})

test_that("classification is vectorised and order-stable", {
  expect_equal(ai_token_class(c("051", "V1")), c("integer", "timepoint"))
  expect_equal(ai_token_class(c("", NA)),      c("empty", "empty"))
})


# ---- delimiter suggestion ---------------------------------------------------

test_that("the .rbx corpus suggests space; the xPONENT corpus suggests underscore", {
  s <- ai_suggest_delimiters(RBX_S)
  expect_equal(s$delimiter[1], " ")

  x <- ai_suggest_delimiters(XPONENT_X)
  expect_equal(x$delimiter[1], "_")
})

test_that("a delimiter that never appears scores zero", {
  s <- ai_suggest_delimiters(c("abc", "def"))
  expect_true(all(s$score == 0))
})


# ---- shape grouping ---------------------------------------------------------

test_that("format keying separates the two X shapes in the .rbx corpus", {
  st <- ai_shape_table(RBX_X, " ", "format")
  # "051 V1"/"80 V1"/"051 V2" are integer-timepoint; "IFO 100" is alpha-integer
  expect_equal(nrow(st$shapes), 2L)
  expect_setequal(st$shapes$classes, c("integer-timepoint", "alpha-integer"))
  expect_equal(sum(st$shapes$n_wells), length(RBX_X))
})

test_that("count keying collapses them, because arity is the same", {
  st <- ai_shape_table(RBX_X, " ", "count")
  expect_equal(nrow(st$shapes), 1L)
  expect_equal(st$shapes$shape_key, "2")
})

test_that("standards form one shape; controls form one shape", {
  expect_equal(nrow(ai_shape_table(RBX_S, " ", "format")$shapes), 1L)
  cs <- ai_shape_table(RBX_C, " ", "format")$shapes
  expect_equal(nrow(cs), 1L)
  expect_equal(cs$classes, "alnum-code-ratio")
})

test_that("content keying splits controls that format keying merges", {
  st <- ai_shape_table(RBX_C, " ", "content")
  # QC1 (Low) vs QC2 (High): both positions vary over a small vocabulary, so
  # content keying makes them separate cases the user can bind differently
  expect_equal(nrow(st$shapes), 2L)
})

test_that("shape keys are reproducible from the rule alone (profile portability)", {
  st   <- ai_shape_table(RBX_C, " ", "content")
  rule <- ai_shape_rule("C", " ", "content", list(), NULL, st$content_positions)
  # keying one string with only the rule must agree with the corpus pass
  expect_equal(ai_shape_key_one(RBX_C[1], rule),
               st$keys[1])
})


# ---- proposed bindings: the three pinned bugs -------------------------------

test_that("X: a numeric patient id is NOT proposed as the dilution", {
  b <- ai_propose_bindings("X", "051 V1", " ")
  expect_equal(b$PatientID$how,  "slot")
  expect_equal(b$PatientID$slot, 1L)
  expect_equal(b$TimePeriod$how,  "slot")
  expect_equal(b$TimePeriod$slot, 2L)
  # no ratio present -> X's dilution stays unbound (optional by contract)
  expect_true(is.null(b$DilutionFactor) ||
              !identical(b$DilutionFactor$how, "slot"))
})

test_that("X: required components fill positionally when class gives no hint", {
  b <- ai_propose_bindings("X", "IFO 100", " ")
  expect_equal(b$PatientID$slot,  1L)
  expect_equal(b$TimePeriod$slot, 2L)
})

test_that("S: a two-token source binds as a span, not just its first token", {
  b <- ai_propose_bindings("S", "Inhouse Ref 1:2952450", " ")
  expect_equal(b$Source$how,  "slot")
  expect_equal(b$Source$slot, c(1L, 2L))
  expect_equal(b$DilutionFactor$slot, 3L)
})

test_that("C: a parenthesised qualifier stays with the source", {
  b <- ai_propose_bindings("C", "QC1 (Low) 1:2500", " ")
  expect_equal(b$Source$slot, c(1L, 2L))
  expect_equal(b$DilutionFactor$slot, 3L)
})

test_that("B: 'blank' is not accepted as a source; dilution defaults to 1", {
  b <- ai_propose_bindings("B", "blank", " ")
  expect_equal(b$Source$how,   "constant")
  expect_equal(b$Source$value, "")            # must be typed (PBS, etc.)
  expect_equal(b$DilutionFactor$how,   "constant")
  expect_equal(b$DilutionFactor$value, "1")
})


# ---- resolution -------------------------------------------------------------

mk_rule <- function(type, desc, delim = " ", shape_by = "format") {
  st <- ai_shape_table(desc, delim, shape_by)
  shapes <- list()
  for (i in seq_len(nrow(st$shapes)))
    shapes[[st$shapes$shape_key[i]]] <-
      ai_propose_bindings(type, st$shapes$example[i], delim, shape_by)
  ai_shape_rule(type, delim, shape_by, shapes, NULL, st$content_positions)
}

test_that("X resolves to subject + timepoint with no dilution error", {
  rule <- mk_rule("X", RBX_X)
  r <- ai_resolve_one("051 V1", "X", rule)
  expect_equal(r$values[["PatientID"]],  "051")
  expect_equal(r$values[["TimePeriod"]], "V1")
  expect_true(is.na(r$dilution_value))
  # X does not require a dilution, so this must NOT be an error
  expect_false(!is.null(r$issues) && any(r$issues$severity == "error"))
})

test_that("S resolves source and the integer denominator of the ratio", {
  rule <- mk_rule("S", RBX_S)
  r <- ai_resolve_one("Inhouse Ref 1:2952450", "S1", rule)
  expect_equal(r$values[["Source"]], "Inhouse Ref")
  expect_equal(r$dilution_value, 2952450L)
  expect_true(r$dilution_ok)
  r2 <- ai_resolve_one("Inhouse Ref 1:150", "S10", rule)
  expect_equal(r2$values[["Source"]], "Inhouse Ref")
  expect_equal(r2$dilution_value, 150L)
})

test_that("C resolves a parenthesised source", {
  rule <- mk_rule("C", RBX_C)
  r <- ai_resolve_one("QC1 (Low) 1:2500", "C1", rule)
  expect_equal(r$values[["Source"]], "QC1 (Low)")
  expect_equal(r$dilution_value, 2500L)
})

test_that("an unmatched shape is an error, never a silent fallthrough", {
  rule <- mk_rule("S", RBX_S)
  r <- ai_resolve_one("Something Else Entirely Here Now", "S1", rule)
  expect_true(any(r$issues$kind == "unmatched_shape"))
  expect_false(r$matched)
})

test_that("shape verdicts report which component is missing", {
  rule <- mk_rule("B", RBX_B)
  v <- ai_shape_verdict("B", names(rule$shapes)[1], "blank", rule)
  expect_false(v$ok)
  expect_true("Source" %in% v$missing_components)
})

test_that("a supplied constant completes the blank shape", {
  rule <- mk_rule("B", "blank")
  k <- names(rule$shapes)[1]
  rule$shapes[[k]]$Source <- ai_binding("constant", value = "PBS")
  v <- ai_shape_verdict("B", k, "blank", rule)
  expect_true(v$ok)
  r <- ai_resolve_one("blank", "B", rule)
  expect_equal(r$values[["Source"]], "PBS")
  expect_equal(r$dilution_value, 1L)
})

test_that("from_type recovers the standard point index", {
  rule <- mk_rule("S", RBX_S)
  k <- names(rule$shapes)[1]
  rule$shapes[[k]]$Replicate <- ai_binding("from_type")
  r <- ai_resolve_one("Inhouse Ref 1:150", "S10", rule)
  expect_equal(r$values[["Replicate"]], "10")
})

test_that("pattern binding survives a shifted slot", {
  rule <- ai_shape_rule("S", " ", "count", list(
    "3" = list(Source = ai_binding("slot", slot = c(1L, 2L)),
               DilutionFactor = ai_binding("pattern", class = c("ratio", "integer")))))
  expect_equal(ai_resolve_one("Inhouse Ref 1:150", "S1", rule)$dilution_value, 150L)
  rule2 <- ai_shape_rule("S", " ", "count", list(
    "3" = list(Source = ai_binding("slot", slot = c(2L, 3L)),
               DilutionFactor = ai_binding("pattern", class = c("ratio", "integer")))))
  expect_equal(ai_resolve_one("1:150 Inhouse Ref", "S1", rule2)$dilution_value, 150L)
})


# ---- backward compatibility with the xPONENT corpus ------------------------

test_that("underscore-delimited xPONENT descriptions resolve as before", {
  rule <- mk_rule("X", XPONENT_X, delim = "_")
  r <- ai_resolve_one("PT01_T0_100", "X", rule)
  expect_equal(r$values[["PatientID"]],  "PT01")
  expect_equal(r$values[["TimePeriod"]], "T0")
  expect_equal(r$dilution_value, 100L)
})

test_that("a missing trailing dilution is a separate shape, not a parse failure", {
  st <- ai_shape_table(XPONENT_X, "_", "format")
  expect_equal(nrow(st$shapes), 2L)          # 3-token and 2-token forms
  rule <- mk_rule("X", XPONENT_X, delim = "_")
  r <- ai_resolve_one("PT03_T0", "X", rule)
  expect_equal(r$values[["PatientID"]],  "PT03")
  expect_equal(r$values[["TimePeriod"]], "T0")
  expect_false(!is.null(r$issues) && any(r$issues$severity == "error"))
})


# ---- profile round trip -----------------------------------------------------

test_that("a ruleset survives a YAML round trip with binding types intact", {
  skip_if_not_installed("yaml")
  rs <- list(S = mk_rule("S", RBX_S), C = mk_rule("C", RBX_C))
  f  <- tempfile(fileext = ".yaml")
  ai_write_profile(f, rs, name = "rbx test", notes = "pinned corpus")
  prof <- ai_read_profile(f)

  expect_equal(prof$name, "rbx test")
  expect_setequal(names(prof$rules), c("S", "C"))
  b <- prof$rules$S$shapes[[names(rs$S$shapes)[1]]]
  expect_equal(b$Source$how,  "slot")
  expect_equal(b$Source$slot, c(1L, 2L))      # integer, not double, after YAML
  expect_true(is.integer(b$Source$slot))
  unlink(f)
})

test_that("a profile file with no rules section is rejected", {
  skip_if_not_installed("yaml")
  f <- tempfile(fileext = ".yaml")
  writeLines("name: not a profile", f)
  expect_error(ai_read_profile(f), "rules")
  unlink(f)
})

test_that("ai_profile_apply flags shapes the profile has never seen", {
  skip_if_not_installed("yaml")
  inv <- data.frame(
    plate_key = "plate_1", well = c("A1", "A2", "A3"),
    specimen_type = c("S", "S", "S"),
    type_code = c("S1", "S2", "S3"),
    description = c(RBX_S[1], RBX_S[2], "Totally Different 1:10 Extra"),
    stringsAsFactors = FALSE)
  rs <- list(S = mk_rule("S", RBX_S[1:2]))
  ap <- ai_profile_apply(rs, inv)
  expect_true(any(ap$report$status == "from_profile"))
  expect_true(any(ap$report$status == "new_shape"))
})


# ---- the template seam ------------------------------------------------------

test_that("ai_merge_resolved overwrites matched wells and blanks the rest", {
  resolved <- data.frame(
    plateid = "plate_1", well = c("A1", "A2"),
    specimen_type = c("X", "S"), type_code = c("X", "S1"),
    description = c("051 V1", "Inhouse Ref 1:150"),
    shape_key = c("k1", "k2"),
    subject_id = c("051", "1"),
    timepoint_tissue_abbreviation = c("V1", ""),
    specimen_dilution_factor = c(NA, 150L),
    specimen_source = c("sample", "Inhouse Ref"),
    groupa = "", groupb = "", biosample_id_barcode = c("", "1"),
    stringsAsFactors = FALSE)

  pm <- data.frame(plateid = "plate_1", well = c("A1", "A2", "A3"),
                   specimen_type = c("X", "X", "X"),
                   stringsAsFactors = FALSE)
  out <- ai_merge_resolved(pm, resolved)

  expect_equal(out$specimen_type, c("X", "S", ""))   # A3 unused -> blanked
  expect_equal(out$subject_id,    c("051", "1", ""))
  expect_equal(out$specimen_source, c("sample", "Inhouse Ref", ""))
  expect_true(is.na(out$specimen_dilution_factor[3]))
})

test_that("ai_merge_resolved matches padded well ids", {
  resolved <- data.frame(
    plateid = "plate_1", well = "A1", specimen_type = "B", type_code = "B",
    description = "blank", shape_key = "k", subject_id = "1",
    timepoint_tissue_abbreviation = "", specimen_dilution_factor = 1L,
    specimen_source = "PBS", groupa = "", groupb = "",
    biosample_id_barcode = "", stringsAsFactors = FALSE)
  # the sheet writes A01, the inventory normalised to A1 -- this join used to
  # miss silently and produce an all-NA row
  pm <- data.frame(plateid = "plate_1", well = "A01", stringsAsFactors = FALSE)
  out <- ai_merge_resolved(pm, resolved)
  expect_equal(out$specimen_source, "PBS")
})

test_that("ai_dilution_gaps lists only the unresolved descriptions", {
  resolved <- data.frame(
    plateid = "plate_1", well = c("A1", "A2", "A3"),
    specimen_type = c("S", "S", "C"),
    description = c("Inhouse Ref 1:150", "Inhouse Ref", "QC1"),
    specimen_dilution_factor = c(150L, NA, NA),
    stringsAsFactors = FALSE)
  g <- ai_dilution_gaps(resolved)
  expect_equal(nrow(g), 2L)
  expect_setequal(g$description, c("Inhouse Ref", "QC1"))
})


# ---- the audit sheet --------------------------------------------------------

test_that("a ruleset flattens to one row per binding", {
  rs <- list(S = mk_rule("S", RBX_S))
  sh <- ai_shape_ruleset_to_sheet(rs)
  expect_true(all(c("specimen_type", "shape_key", "component", "how",
                    "slot", "class", "value") %in% names(sh)))
  expect_true(any(sh$component == "Source"))
  expect_equal(unique(sh$specimen_type), "S")
  expect_equal(sh$slot[sh$component == "Source"][1], "1+2")
})


# =============================================================================
# .rbx integration — pinned to what rbx_binary_parser.R actually emits
# -----------------------------------------------------------------------------
# long_dataframe() supplies sample_label, sample_description, sample_category.
# parse_samples() constrains labels to ^[A-Za-z]{1,4}[0-9]{0,4}$ and
# sample_category() classifies on the FIRST LETTER only (B/S/C/X, else
# "Unknown"), so "QC1" -> "Q" -> "Unknown" -> .rbx_type_from() returns "X".
# Every QC control therefore arrives typed as a test sample, and the keyword
# proposal is the only thing standing between that and a bad import.
# =============================================================================

test_that("control labels the .rbx types as X are proposed as controls", {
  skip_if_not(exists("ai_propose_specimen_types"))
  inv <- data.frame(
    plate_key = "plate_1", plate_label = "1", plate_index = 1L,
    well = c("A1", "A2", "A3", "A4"),
    well_raw = c("A1", "A2", "A3", "A4"),
    row_letter = "A", col_number = 1:4,
    type_code = c("X", "X", "X", "X"),          # what .rbx_type_from produced
    specimen_type = c("X", "X", "X", "X"),
    description = c("QC1 (Low) 1:2500", "QC2 (High) 1:2500",
                    "80 V1", "051 V1"),
    raw_type_code = "X", raw_description = NA_character_,
    source_file = "p1.rbx", response_hint = NA_real_,
    type_origin = "file", desc_origin = "file",
    stringsAsFactors = FALSE)

  p <- ai_propose_specimen_types(inv)
  qc <- p[p$well %in% c("A1", "A2"), ]
  expect_equal(nrow(qc), 2L)
  expect_true(all(qc$proposed_type == "C"))
  expect_true(all(qc$evidence == "keyword"))
  expect_true(all(qc$confidence == "high"))
  # and the genuine samples are left alone
  expect_false(any(p$well %in% c("A3", "A4")))
})

test_that("real .rbx standards and blanks are proposed correctly", {
  skip_if_not(exists("ai_propose_specimen_types"))
  mk <- function(desc) data.frame(
    plate_key = "p", plate_label = "1", plate_index = 1L,
    well = paste0("A", seq_along(desc)), well_raw = paste0("A", seq_along(desc)),
    row_letter = "A", col_number = seq_along(desc),
    type_code = "X", specimen_type = "X", description = desc,
    raw_type_code = "X", raw_description = NA_character_,
    source_file = "p.rbx", response_hint = NA_real_,
    type_origin = "file", desc_origin = "file", stringsAsFactors = FALSE)

  p <- ai_propose_specimen_types(mk(c("Inhouse Ref 1:2952450", "NIBSC06 1:100",
                                      "blank", "PBS", "pool (mid)")))
  ty <- setNames(p$proposed_type, p$description)
  expect_equal(ty[["Inhouse Ref 1:2952450"]], "S")
  expect_equal(ty[["NIBSC06 1:100"]],         "S")
  expect_equal(ty[["blank"]],                 "B")
  expect_equal(ty[["PBS"]],                   "B")
  expect_equal(ty[["pool (mid)"]],            "C")
})

test_that("the raw .rbx description binds without a synthetic rebuild", {
  # what the reader now emits verbatim, instead of rebuilding "Inhouse Ref_2952450"
  rule <- mk_rule("S", "Inhouse Ref 1:2952450")
  r <- ai_resolve_one("Inhouse Ref 1:2952450", "S1", rule)
  expect_equal(r$values[["Source"]], "Inhouse Ref")
  expect_equal(r$dilution_value, 2952450L)

  # and the space-delimited sample form survives, which the synthetic rebuild
  # broke: joining under "_" left "80 V1" as a single token
  rx <- mk_rule("X", c("80 V1", "051 V1"))
  rr <- ai_resolve_one("80 V1", "X", rx)
  expect_equal(rr$values[["PatientID"]],  "80")
  expect_equal(rr$values[["TimePeriod"]], "V1")
})


# =============================================================================
# Plate geometry — 96 and 384 must both label correctly
# -----------------------------------------------------------------------------
# The parser indexes wells row-major over the plate's own column count. A fixed
# 12 was right for 96 (8x12) and wrong for 384 (16x24): index 96 came out "I1"
# where the real position is "E1", and the same constant bounded the
# sample-record validity test, so records addressing higher wells were dropped.
# =============================================================================

test_that("rbx_well_label places wells correctly on a 96-well plate", {
  skip_if_not(exists("rbx_well_label"))
  expect_equal(rbx_well_label(0L,  12L), "A1")
  expect_equal(rbx_well_label(11L, 12L), "A12")
  expect_equal(rbx_well_label(12L, 12L), "B1")
  expect_equal(rbx_well_label(95L, 12L), "H12")   # last well of a 96 plate
})

test_that("rbx_well_label places wells correctly on a 384-well plate", {
  skip_if_not(exists("rbx_well_label"))
  expect_equal(rbx_well_label(0L,   24L), "A1")
  expect_equal(rbx_well_label(23L,  24L), "A24")
  expect_equal(rbx_well_label(24L,  24L), "B1")
  # the regression: index 96 is E1 on a 24-column plate, not I1
  expect_equal(rbx_well_label(96L,  24L), "E1")
  expect_equal(rbx_well_label(383L, 24L), "P24")  # last well of a 384 plate
})

test_that("rbx_well_label is vectorised and NA-safe", {
  skip_if_not(exists("rbx_well_label"))
  expect_equal(rbx_well_label(c(0L, 12L, 95L), 12L), c("A1", "B1", "H12"))
  expect_true(is.na(rbx_well_label(NA_integer_, 12L)))
})

test_that("plate size is decided by the wells present, not assumed", {
  skip_if_not(exists("rbx_plate_size"))
  expect_equal(rbx_plate_size(96L,  95L),  96L)
  expect_equal(rbx_plate_size(384L, 383L), 384L)
  # a 384 plate with only part of it acquired: the high well index is what
  # gives it away, and it used to bound samples out of existence
  expect_equal(rbx_plate_size(40L, 300L), 384L)
  expect_equal(rbx_plate_size(40L, -1L),   96L)   # nothing to suggest otherwise
})

test_that("rbx_geometry returns the right rows and columns", {
  skip_if_not(exists("rbx_geometry"))
  expect_equal(unname(rbx_geometry(96L)),  c(8L, 12L))
  expect_equal(unname(rbx_geometry(384L)), c(16L, 24L))
  expect_error(rbx_geometry(1536L), "neither")
})

test_that("the inventory infers a larger plate rather than dropping wells", {
  skip_if_not(exists("ai_infer_plate_size"))
  # declared 96, but the file holds P24 -> must be read as 384
  g <- ai_infer_plate_size(c("A1", "H12", "P24"), declared = 96L)
  expect_equal(g$n_wells, 384L)
  expect_equal(g$rows, 16L)
  expect_equal(g$cols, 24L)
  expect_true(g$upgraded)
  expect_equal(g$declared, 96L)

  # a genuine 96 plate is left alone
  g2 <- ai_infer_plate_size(c("A1", "D6", "H12"), declared = 96L)
  expect_equal(g2$n_wells, 96L)
  expect_false(g2$upgraded)
})

test_that("a column beyond 12 alone forces 384, even in row A", {
  skip_if_not(exists("ai_infer_plate_size"))
  g <- ai_infer_plate_size(c("A1", "A24"), declared = 96L)
  expect_equal(g$n_wells, 384L)
})

test_that("inferring geometry never shrinks below the declared size", {
  skip_if_not(exists("ai_infer_plate_size"))
  # only a few wells present, but the user said 384 -- keep 384
  g <- ai_infer_plate_size(c("A1", "B2"), declared = 384L)
  expect_equal(g$n_wells, 384L)
  expect_false(g$upgraded)
})


# =============================================================================
# Standard-curve detection must not fire on sample visit series
# -----------------------------------------------------------------------------
# Reported from a live batch:
#   Now: X   Suggested: S
#   "70 wells share the pattern '# v#' over 4 steps with falling response
#    (rho -0.80)"
# Descriptions like "70 V1" / "051 V2" all collapsed to the digit-stripped stem
# "# v#", the varying number was the VISIT, and antibody level genuinely falls
# with visit -- so 70 sample wells were proposed as standards.
# =============================================================================

.mk_inv <- function(desc, type = "X", plate = "plate_1", hint = NA_real_) {
  n <- length(desc)
  data.frame(
    plate_key = plate, plate_label = "1", plate_index = 1L,
    well = paste0("A", seq_len(n)), well_raw = paste0("A", seq_len(n)),
    row_letter = "A", col_number = seq_len(n),
    type_code = type, specimen_type = type, description = desc,
    raw_type_code = type, raw_description = NA_character_,
    source_file = "f", response_hint = hint,
    type_origin = "file", desc_origin = "file", stringsAsFactors = FALSE)
}

test_that("a visit series across many sample wells is NOT proposed as standards", {
  skip_if_not(exists("ai_propose_specimen_types"))
  # 70 wells, 4 visits, response falling with visit -- the reported case
  subj  <- rep(sprintf("%03d", 1:18), each = 4)[1:70]
  visit <- rep(paste0("V", 1:4), length.out = 70)
  hint  <- rep(c(9000, 6000, 3000, 900), length.out = 70)
  inv <- .mk_inv(paste(subj, visit), hint = hint)

  p <- ai_propose_specimen_types(inv)
  expect_false(any(p$proposed_type == "S"))
})

test_that("a numeric subject id at element 1 is never read as a dilution step", {
  skip_if_not(exists("ai_propose_specimen_types"))
  inv <- .mk_inv(c("70 V1", "80 V1", "90 V1", "100 V1", "110 V1", "120 V1"),
                 hint = c(9000, 7000, 5000, 3000, 2000, 900))
  p <- ai_propose_specimen_types(inv)
  expect_false(any(p$proposed_type == "S"))
})

test_that("a real 1:N dilution ladder IS still proposed as standards", {
  skip_if_not(exists("ai_propose_specimen_types"))
  dil  <- c(150, 450, 1350, 4050, 12150, 36450, 109350, 328050)
  inv  <- .mk_inv(paste0("Unknown Ref 1:", dil),
                  hint = c(30000, 22000, 15000, 9000, 5000, 2500, 1100, 400))
  p <- ai_propose_specimen_types(inv)
  expect_true(all(p$proposed_type == "S"))
  expect_true(any(grepl("dilution \\(1:N\\)", p$reason)))
})

test_that("a consecutive integer run is rejected even with a falling response", {
  skip_if_not(exists("ai_propose_specimen_types"))
  # "Pool 1".."Pool 6" -- replicate indices, not a dilution series
  inv <- .mk_inv(paste("Pool", 1:6), type = "X",
                 hint = c(9000, 8000, 7000, 6000, 5000, 4000))
  p <- ai_propose_specimen_types(inv)
  expect_false(any(p$evidence == "ladder"))
})

test_that("a bare-integer ladder needs both a wide span and a falling response", {
  skip_if_not(exists("ai_propose_specimen_types"))
  vals <- c(100, 300, 900, 2700, 8100)          # 81-fold span, not consecutive
  # no response hint at all -> must NOT be proposed on span alone
  p_no <- ai_propose_specimen_types(.mk_inv(paste("Ref", vals), hint = NA_real_))
  expect_false(any(p_no$evidence == "ladder"))
  # with a falling response -> proposed
  p_yes <- ai_propose_specimen_types(
    .mk_inv(paste("Ref", vals), hint = c(20000, 12000, 6000, 2000, 500)))
  expect_true(any(p_yes$evidence == "ladder"))
})

test_that("an implausibly large ladder group is rejected on size", {
  skip_if_not(exists("ai_propose_specimen_types"))
  # 60 wells sharing a stem with a wide, non-consecutive numeric span and a
  # falling response: still not a curve, because a curve is not 60 wells
  vals <- rep(c(10, 100, 1000, 10000), each = 15)
  inv  <- .mk_inv(paste("Ref", vals),
                  hint = rep(c(20000, 8000, 2000, 300), each = 15))
  p <- ai_propose_specimen_types(inv)
  expect_false(any(p$evidence == "ladder"))
})

test_that("keyword evidence still wins over everything else", {
  skip_if_not(exists("ai_propose_specimen_types"))
  inv <- .mk_inv(c("QC1 (Low) 1:2500", "blank", "051 V1"))
  p <- ai_propose_specimen_types(inv)
  ty <- stats::setNames(p$proposed_type, p$description)
  expect_equal(ty[["QC1 (Low) 1:2500"]], "C")
  expect_equal(ty[["blank"]], "B")
  expect_false("051 V1" %in% p$description)
})


# =============================================================================
# Well-label width scales with the plate
# =============================================================================

test_that("label geometry widens for 96-well and narrows for 384", {
  skip_if_not(exists("ai_grid_label_geometry"))
  g96  <- ai_grid_label_geometry(12L)
  g384 <- ai_grid_label_geometry(24L)
  g1536 <- ai_grid_label_geometry(48L)
  expect_equal(as.integer(g96[["chars"]]),   8L)
  expect_equal(as.integer(g384[["chars"]]),  4L)
  expect_equal(as.integer(g1536[["chars"]]), 2L)
  # cells shrink as the plate gets denser
  expect_true(g96[["cell"]] > g384[["cell"]])
  expect_true(g384[["cell"]] > g1536[["cell"]])
})


# =============================================================================
# Specimen-type scope changes after plate-grid edits
# -----------------------------------------------------------------------------
# Reported: a plate parses as X + S + B, the user retypes two wells to Control
# on the grid, and the Controls tab then shows NO description groups -- so the
# parsing cannot be applied and the group cannot be approved, leaving the gate
# permanently shut. ai_ruleset_init() ran once at parse time, so a type that
# appeared later never got a rule.
# =============================================================================

.mk_typed <- function(type, desc, plate = "plate_1") {
  n <- length(desc)
  data.frame(
    plate_key = plate, plate_label = "1", plate_index = 1L,
    well = paste0("A", seq_len(n)), well_raw = paste0("A", seq_len(n)),
    row_letter = "A", col_number = seq_len(n),
    type_code = type, specimen_type = type, description = desc,
    raw_type_code = type, raw_description = NA_character_,
    source_file = "f", response_hint = NA_real_,
    type_origin = "file", desc_origin = "file", stringsAsFactors = FALSE)
}

.batch <- function(...) {
  parts <- list(...)
  out <- do.call(rbind, parts)
  out$well <- paste0("A", seq_len(nrow(out)))
  out$col_number <- seq_len(nrow(out))
  out
}

test_that("a specimen type introduced by a grid edit gets a rule", {
  skip_if_not(exists("ai_ruleset_sync"))
  before <- .batch(.mk_typed("X", c("PT01_T0_100", "PT02_T0_100")),
                   .mk_typed("S", c("Ref_1:150", "Ref_1:450")),
                   .mk_typed("B", "blank"))
  s1 <- ai_ruleset_sync(before)
  expect_setequal(names(s1$rules), c("X", "S", "B"))
  expect_setequal(s1$added, c("X", "S", "B"))
  expect_false("C" %in% names(s1$rules))

  # the user retypes two wells to Control
  after <- .batch(.mk_typed("X", "PT02_T0_100"),
                  .mk_typed("S", c("Ref_1:150", "Ref_1:450")),
                  .mk_typed("C", c("QC1_1:2500", "QC2_1:2500")))
  s2 <- ai_ruleset_sync(after, s1$rules, s1$fingerprints)

  expect_true("C" %in% names(s2$rules))       # the fix
  expect_true("C" %in% s2$added)
  expect_true(length(s2$rules$C$shapes) > 0)  # and it has groups to bind
})

test_that("a new type inherits the delimiter already configured for the batch", {
  skip_if_not(exists("ai_ruleset_sync"))
  before <- .batch(.mk_typed("X", c("PT01_T0_100", "PT02_T0_100")))
  s1 <- ai_ruleset_sync(before)
  expect_equal(s1$rules$X$delimiters, "_")

  after <- .batch(.mk_typed("X", "PT02_T0_100"),
                  .mk_typed("C", "QC1_1:2500"))
  s2 <- ai_ruleset_sync(after, s1$rules, s1$fingerprints)
  # a two-well corpus gives the suggestion scorer almost nothing, so the
  # batch's existing convention is the better default
  expect_equal(s2$rules$C$delimiters, "_")
})

test_that("an untouched type is not refreshed, so its approvals survive", {
  skip_if_not(exists("ai_ruleset_sync"))
  before <- .batch(.mk_typed("X", c("PT01_T0_100", "PT02_T0_100")),
                   .mk_typed("S", c("Ref_1:150", "Ref_1:450")))
  s1 <- ai_ruleset_sync(before)
  # only X's corpus changes
  after <- .batch(.mk_typed("X", "PT02_T0_100"),
                  .mk_typed("S", c("Ref_1:150", "Ref_1:450")),
                  .mk_typed("C", "QC1_1:2500"))
  s2 <- ai_ruleset_sync(after, s1$rules, s1$fingerprints)
  expect_true("X" %in% s2$refreshed)
  expect_false("S" %in% s2$refreshed)
})

test_that("a type with no wells left goes dormant, keeping its bindings", {
  skip_if_not(exists("ai_ruleset_sync"))
  before <- .batch(.mk_typed("X", "PT01_T0_100"), .mk_typed("B", "blank"))
  s1 <- ai_ruleset_sync(before)
  after <- .batch(.mk_typed("X", "PT01_T0_100"), .mk_typed("C", "QC1_1:2500"))
  s2 <- ai_ruleset_sync(after, s1$rules, s1$fingerprints)
  expect_true("B" %in% s2$dormant)
  expect_true("B" %in% names(s2$rules))   # retyping back restores the work
})

test_that("stale approvals are identified when a shape disappears", {
  skip_if_not(exists("ai_stale_approvals"))
  rules <- list(X = ai_shape_rule("X", "_", "format",
                                  list("3|alnum-timepoint-integer" = list())))
  approved <- list()
  approved[[paste("X", "3|alnum-timepoint-integer", sep = "\r")]] <- TRUE
  approved[[paste("X", "2|alnum-timepoint", sep = "\r")]] <- TRUE   # gone
  st <- ai_stale_approvals(rules, approved)
  expect_length(st, 1L)
  expect_true(grepl("2\\|alnum-timepoint", st))
})

test_that("coverage names a present type that cannot be approved", {
  skip_if_not(exists("ai_ruleset_coverage"))
  inv <- .batch(.mk_typed("X", "PT01_T0_100"), .mk_typed("C", "QC1_1:2500"))
  rules <- list(X = ai_rule_new_for_type(inv, "X"))     # C deliberately absent
  cov <- ai_ruleset_coverage(inv, rules)
  expect_equal(nrow(cov), 1L)
  expect_equal(cov$specimen_type, "C")
  expect_equal(cov$status, "no_rule")
  expect_true(grepl("no description rule", cov$message))

  # once synced, coverage is clean
  s <- ai_ruleset_sync(inv, rules)
  expect_equal(nrow(ai_ruleset_coverage(inv, s$rules)), 0L)
})


# =============================================================================
# Wells with no description at all
# -----------------------------------------------------------------------------
# The second version of the same trap: a retyped well whose description is
# empty was dropped by ai_shape_table(), so its type had zero groups and could
# never be approved even once it had a rule.
# =============================================================================

test_that("descriptionless wells form their own bindable group", {
  skip_if_not(exists("AI_EMPTY_SHAPE_KEY"))
  st <- ai_shape_table(c("QC1 1:2500", NA, ""), " ", "format")
  expect_true(AI_EMPTY_SHAPE_KEY %in% st$shapes$shape_key)
  row <- st$shapes[st$shapes$shape_key == AI_EMPTY_SHAPE_KEY, ]
  expect_equal(row$n_tokens, 0L)
  expect_equal(row$n_wells, 2L)
  # and the per-well keys point at it
  expect_equal(st$keys[2], AI_EMPTY_SHAPE_KEY)
  expect_equal(st$keys[3], AI_EMPTY_SHAPE_KEY)
})

test_that("a corpus of only empty descriptions still yields one group", {
  skip_if_not(exists("AI_EMPTY_SHAPE_KEY"))
  st <- ai_shape_table(c(NA, "", NA), "_", "format")
  expect_equal(nrow(st$shapes), 1L)
  expect_equal(st$shapes$shape_key, AI_EMPTY_SHAPE_KEY)
})

test_that("constants bound to the empty group resolve a descriptionless well", {
  skip_if_not(exists("AI_EMPTY_SHAPE_KEY"))
  rule <- ai_shape_rule("C", "_", "format", list())
  rule$shapes[[AI_EMPTY_SHAPE_KEY]] <- list(
    DilutionFactor = ai_binding("constant", value = "100"),
    Source         = ai_binding("constant", value = "QC pool"))
  r <- ai_resolve_one(NA_character_, "C1", rule)
  expect_equal(r$shape_key, AI_EMPTY_SHAPE_KEY)
  expect_equal(r$dilution_value, 100L)
  expect_equal(r$values[["Source"]], "QC pool")
  expect_false(!is.null(r$issues) && any(r$issues$severity == "error"))
})

test_that("an unbound empty group reports what to do, not just a failure", {
  skip_if_not(exists("AI_EMPTY_SHAPE_KEY"))
  rule <- ai_shape_rule("C", "_", "format", list())
  r <- ai_resolve_one("", "C1", rule)
  expect_true(any(r$issues$kind == "blank_description"))
  expect_true(any(grepl("constant", r$issues$message)))
})


# =============================================================================
# Plate file name validation
# -----------------------------------------------------------------------------
# Reported from an xPONENT batch:
#   plate_id | error | INVALID FILE PATHS: ... incorrectly formatted:
#     - 20221003_AGA_1_2000_IgG1_Plate_1.csv
#     - 20221003_AGA_1_2000_IgG1_Plate_2.csv
#     File paths must include directory separators (/ or \)
#
# looks_like_file_path() required a directory separator and was applied to
# plate_metadata$file_name, which comes from Shiny's fileInput as
# upload_df$name -- the base name only, because browsers do not send the
# client's directory. The demand was impossible to satisfy through the UI.
# =============================================================================

test_that("an uploaded base name is a valid plate file name", {
  skip_if_not(exists("looks_like_plate_filename"))
  expect_true(looks_like_plate_filename("20221003_AGA_1_2000_IgG1_Plate_1.csv"))
  expect_true(looks_like_plate_filename("20221003_AGA_1_2000_IgG1_Plate_2.csv"))
  expect_true(looks_like_plate_filename("plate_1.CSV"))
  expect_true(looks_like_plate_filename("batch_2.rbx"))
  expect_true(looks_like_plate_filename("plate map.xlsx"))
})

test_that("a full path is still accepted, not required", {
  skip_if_not(exists("looks_like_plate_filename"))
  expect_true(looks_like_plate_filename("C:\\\\data\\\\plate_1.csv"))
  expect_true(looks_like_plate_filename("/home/u/plates/plate_1.csv"))
})

test_that("genuinely unusable names are still rejected", {
  skip_if_not(exists("looks_like_plate_filename"))
  expect_false(looks_like_plate_filename("no_extension"))
  expect_false(looks_like_plate_filename(""))
  expect_false(looks_like_plate_filename("C:/data/"))       # a directory
  expect_false(looks_like_plate_filename(NA_character_))
})

test_that("plate_file_basename strips both separator styles", {
  skip_if_not(exists("plate_file_basename"))
  expect_equal(plate_file_basename("C:\\\\data\\\\plate_1.csv"), "plate_1.csv")
  expect_equal(plate_file_basename("/home/u/plate_1.csv"),     "plate_1.csv")
  expect_equal(plate_file_basename("plate_1.csv"),             "plate_1.csv")
  expect_true(is.na(plate_file_basename(NA_character_)))
})

test_that("the layout join matches a pasted path against an uploaded name", {
  skip_if_not(exists("plate_file_basename"))
  uploaded <- "plate_1.csv"
  in_sheet <- "C:\\\\data\\\\2022\\\\plate_1.csv"
  expect_equal(plate_file_basename(uploaded), plate_file_basename(in_sheet))
})

test_that("looks_like_file_path still tests for an actual path", {
  skip_if_not(exists("looks_like_file_path"))
  # kept for that purpose, just no longer used to validate an upload
  expect_true(looks_like_file_path("C:/data/plate_1.csv"))
  expect_false(looks_like_file_path("plate_1.csv"))
})
