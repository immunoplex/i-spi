# =============================================================================
# test-assay-std-reference-rules.R
# -----------------------------------------------------------------------------
# Tests for assay_std_reference_rules.R's pure logic: parse/serialize, the
# candidate pre-scan, and the save-time merge. Synthetic inventories only --
# the real-file end-to-end behavior (both GBSIGG-WP4-417_test.srbx and the PIH
# reference file) was verified separately against the actual parser before
# this file was written; see RBX_DILUTION_AUTHORITATIVE_SOURCE_PLAN.md.
#
# Run: testthat::test_file("test-assay-std-reference-rules.R")
# =============================================================================

if (!exists("ai_std_reference_candidates")) {
  cand <- c("assay_std_reference_rules.R", "src/assay_std_reference_rules.R",
            "../../assay_std_reference_rules.R", "../../../src/assay_std_reference_rules.R")
  hit <- cand[file.exists(cand)][1]
  if (is.na(hit)) stop("cannot find assay_std_reference_rules.R; edit the path in this test")
  wi <- sub("assay_std_reference_rules\\.R$", "assay_well_inventory.R", hit)
  if (file.exists(wi)) source(wi)
  sr <- sub("assay_std_reference_rules\\.R$", "assay_shape_rules.R", hit)
  if (file.exists(sr)) source(sr)
  source(hit)
}

library(testthat)

mk_inv <- function(specimen_type, description, instrument_dilution = NA_real_) {
  n <- length(description)
  data.frame(specimen_type = rep(specimen_type, n), description = description,
            instrument_dilution = rep_len(instrument_dilution, n),
            stringsAsFactors = FALSE)
}

# ---- parse / serialize -------------------------------------------------------

test_that("parse handles NULL/NA/empty/malformed input as zero rows, never an error", {
  for (bad in list(NULL, NA_character_, "", "   ", "not json", "{\"oops\":1}",
                   "[{\"description\":\"S1\"}]")) {   # missing dilution field
    out <- ai_std_reference_parse(bad)
    expect_equal(nrow(out), 0L)
    expect_identical(names(out), c("description", "dilution"))
  }
})

test_that("serialize -> parse is a round trip", {
  df <- data.frame(description = c("S1", "S2", "QC1"), dilution = c(500, 1500, 450),
                   stringsAsFactors = FALSE)
  js <- ai_std_reference_serialize(df)
  expect_type(js, "character")
  back <- ai_std_reference_parse(js)
  expect_equal(back$description, df$description)
  expect_equal(back$dilution, df$dilution)
})

test_that("serialize of zero rows is a valid empty JSON array", {
  expect_equal(ai_std_reference_serialize(NULL), "[]")
  expect_equal(ai_std_reference_serialize(AI_STD_REFERENCE_EMPTY), "[]")
  expect_equal(nrow(ai_std_reference_parse(ai_std_reference_serialize(NULL))), 0L)
})


# ---- candidate detection ------------------------------------------------------

test_that("a description with a parseable ratio needs no entry", {
  inv <- mk_inv("S", c("Inhouse Ref 1:150", "Inhouse Ref 1:150"))
  cd <- ai_std_reference_candidates(inv, "S")
  expect_equal(nrow(cd), 0L)
})

test_that("a bare label with no ratio and no instrument coverage needs an entry", {
  inv <- mk_inv("S", c("S1", "S1", "S2"))
  cd <- ai_std_reference_candidates(inv, "S")
  expect_equal(sort(cd$description), c("S1", "S2"))
  expect_equal(cd$n_wells[cd$description == "S1"], 2L)
})

test_that("instrument coverage is only honored for X/C, never for Standards", {
  # same bare-label shape, but instrument_dilution happens to be populated --
  # must NOT short-circuit a Standard the way it would a Sample/Control.
  inv_s <- mk_inv("S", c("S1", "S1"), instrument_dilution = c(1, 1))
  expect_equal(nrow(ai_std_reference_candidates(inv_s, "S")), 1L)

  inv_x <- mk_inv("X", c("1", "1"), instrument_dilution = c(500, 500))
  expect_equal(nrow(ai_std_reference_candidates(inv_x, "X")), 0L)

  inv_c <- mk_inv("C", c("QC1", "QC1"), instrument_dilution = c(450, 450))
  expect_equal(nrow(ai_std_reference_candidates(inv_c, "C")), 0L)
})

test_that("instrument coverage must apply to EVERY well sharing the description", {
  inv_x <- mk_inv("X", c("1", "1"), instrument_dilution = c(500, NA))
  expect_equal(nrow(ai_std_reference_candidates(inv_x, "X")), 1L)
})

test_that("an already-saved description is excluded from candidates", {
  inv <- mk_inv("S", c("S1", "S2"))
  saved <- data.frame(description = "S1", dilution = 500, stringsAsFactors = FALSE)
  cd <- ai_std_reference_candidates(inv, "S", saved = saved)
  expect_equal(cd$description, "S2")
})

test_that("blank/NA descriptions never appear as candidates", {
  inv <- mk_inv("S", c(NA_character_, "", "  ", "S1"))
  cd <- ai_std_reference_candidates(inv, "S")
  expect_equal(cd$description, "S1")
})

test_that("an inventory with none of the requested type returns zero candidates", {
  inv <- mk_inv("X", c("1", "2"))
  expect_equal(nrow(ai_std_reference_candidates(inv, "S")), 0L)
})


# ---- merge --------------------------------------------------------------------

test_that("merge of a fresh save with no prior entries is just the new entries", {
  new <- data.frame(description = c("S1", "S2"), dilution = c(500, 1500),
                    stringsAsFactors = FALSE)
  out <- ai_std_reference_merge(NULL, new)
  expect_equal(sort(out$description), c("S1", "S2"))
})

test_that("merge is last-write-wins on description, keeps everything else", {
  saved <- data.frame(description = c("S1", "S2"), dilution = c(999, 1500),
                      stringsAsFactors = FALSE)
  new   <- data.frame(description = "S1", dilution = 500, stringsAsFactors = FALSE)
  out   <- ai_std_reference_merge(saved, new)
  expect_equal(nrow(out), 2L)
  expect_equal(out$dilution[out$description == "S1"], 500)
  expect_equal(out$dilution[out$description == "S2"], 1500)
})

test_that("merging in nothing new returns the saved set unchanged", {
  saved <- data.frame(description = "S1", dilution = 500, stringsAsFactors = FALSE)
  out <- ai_std_reference_merge(saved, NULL)
  expect_equal(out, saved)
})


# ---- paste-from-spreadsheet --------------------------------------------------

test_that("ai_std_reference_parse_pasted() parses the user's real 11-point example, header included, tab-delimited", {
  txt <- paste(
    "Standard point\tDilution factor",
    "STD_1\t50", "STD_2\t125", "STD_3\t313", "STD_4\t781", "STD_5\t1953",
    "STD_6\t4883", "STD_7\t12207", "STD_8\t30518", "STD_9\t76294",
    "STD_10\t190735", "STD_11\t476837",
    sep = "\n")
  out <- ai_std_reference_parse_pasted(txt)
  expect_equal(nrow(out), 11L)
  expect_equal(out$description, paste0("STD_", 1:11))
  expect_equal(out$dilution,
              c(50, 125, 313, 781, 1953, 4883, 12207, 30518, 76294, 190735, 476837))
  expect_equal(attr(out, "skipped"), character(0))   # header dropped silently, not "skipped"
})

test_that("ai_std_reference_parse_pasted() also handles space-delimited paste and no header", {
  txt <- "STD_1    50\nSTD_2    125\nSTD_3    313"
  out <- ai_std_reference_parse_pasted(txt)
  expect_equal(nrow(out), 3L)
  expect_equal(out$dilution, c(50, 125, 313))
})

test_that("ai_std_reference_parse_pasted() skips malformed lines and reports them, never errors", {
  txt <- "STD_1\t50\nnonsense line\nSTD_2\t-5\n\tnotanumber\nSTD_3\t313"
  out <- ai_std_reference_parse_pasted(txt)
  expect_equal(out$description, c("STD_1", "STD_3"))
  expect_true(length(attr(out, "skipped")) >= 1L)
})

test_that("ai_std_reference_parse_pasted() returns zero rows, never an error, for empty/NULL input", {
  for (bad in list(NULL, "", "   ", NA_character_)) {
    out <- ai_std_reference_parse_pasted(bad)
    expect_equal(nrow(out), 0L)
  }
})

test_that("ai_std_reference_match_pasted() matches exact descriptions first", {
  pasted <- data.frame(description = c("S1", "S2"), dilution = c(50, 125),
                       stringsAsFactors = FALSE)
  out <- ai_std_reference_match_pasted(c("S1", "S2"), pasted)
  expect_equal(out$dilution, c(50, 125))
  expect_equal(out$matched_from, c("S1", "S2"))
})

test_that("ai_std_reference_match_pasted() falls back to trailing-digit match when exact text differs", {
  pasted <- data.frame(description = paste0("STD_", 1:3), dilution = c(50, 125, 313),
                       stringsAsFactors = FALSE)
  out <- ai_std_reference_match_pasted(c("S1", "S2", "S3"), pasted)
  expect_equal(out$dilution, c(50, 125, 313))
  expect_equal(out$matched_from, c("STD_1", "STD_2", "STD_3"))
})

test_that("ai_std_reference_match_pasted() leaves an uncovered candidate as NA rather than guessing", {
  pasted <- data.frame(description = "STD_1", dilution = 50, stringsAsFactors = FALSE)
  out <- ai_std_reference_match_pasted(c("S1", "S2"), pasted)
  expect_equal(out$dilution, c(50, NA_real_))
  expect_true(is.na(out$matched_from[2]))
})
