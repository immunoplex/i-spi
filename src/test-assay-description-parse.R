# tests for assay_description_parse.R — pinned to the real .rbx descriptions.
# Run: testthat::test_file("test-assay-description-parse.R")
# (adjust the source path below to wherever assay_description_parse.R lives)

if (!exists("ai_parse_rule")) {
  cand <- c("assay_description_parse.R",
            "src/assay_description_parse.R",
            "../../assay_description_parse.R",
            "../../../src/assay_description_parse.R")
  hit <- cand[file.exists(cand)][1]
  if (is.na(hit)) stop("cannot find assay_description_parse.R; edit the path in this test")
  source(hit)
}

library(testthat)

# ---- real per-type rules (from the .rbx files: space-delimited) -------------
rule_X  <- ai_parse_rule("X", delimiters = " ", order = c("PatientID", "TimePeriod"))          # no DilutionFactor
rule_S  <- ai_parse_rule("S", delimiters = " ", order = c("Source", "DilutionFactor"))
rule_C  <- ai_parse_rule("C", delimiters = " ", order = c("Source", "DilutionFactor"))

row <- function(type, desc, rule)
  ai_parse_description_row(
    type = type, description = desc,
    element_order     = if (identical(substr(type,1,1),"X")) rule$order else c("PatientID","TimePeriod","DilutionFactor"),
    bcs_element_order = if (identical(substr(type,1,1),"X")) c("Source","DilutionFactor") else rule$order,
    delimiters = rule$delimiters, dilution_mode = rule$dilution_mode, use_defaults = FALSE)


# ---- X: numeric patient IDs must NOT be misread as a dilution ---------------
test_that("X samples parse positionally; numeric IDs are not eaten as dilution", {
  r <- row("X", "051 V1", rule_X)
  expect_equal(r$subject_id, "051")
  expect_equal(r$timeperiod, "V1")
  expect_false(r$dilution_ok)                 # no DilutionFactor element -> no parse
  expect_true(is.na(r$dilution_factor))
  # and crucially: no error about a missing dilution
  expect_false(any(r$issues$kind == "bad_dilution"))
  expect_equal(nrow(r$issues), 0L)

  r2 <- row("X", "80 V1", rule_X)             # 2-digit id
  expect_equal(r2$subject_id, "80"); expect_equal(r2$timeperiod, "V1")

  r3 <- row("X", "IFO 100", rule_X)           # numeric timeperiod, alpha id
  expect_equal(r3$subject_id, "IFO"); expect_equal(r3$timeperiod, "100")
  expect_true(is.na(r3$dilution_factor))      # "100" is NOT taken as dilution
})

# ---- S: multi-word source + 1:N ratio ---------------------------------------
test_that("standards: 'Inhouse Ref 1:2952450' -> source + integer denominator", {
  r <- row("S1", "Inhouse Ref 1:2952450", rule_S)
  expect_equal(r$source, "Inhouse Ref")
  expect_equal(r$dilution_factor, 2952450L)
  expect_true(r$dilution_ok)
  r2 <- row("S10", "Inhouse Ref 1:150", rule_S)
  expect_equal(r2$source, "Inhouse Ref"); expect_equal(r2$dilution_factor, 150L)
})

# ---- C: source with parentheses + ratio -------------------------------------
test_that("controls: 'QC1 (Low) 1:2500' -> source keeps parens, dilution 2500", {
  r <- row("C1", "QC1 (Low) 1:2500", rule_C)
  expect_equal(r$source, "QC1 (Low)")
  expect_equal(r$dilution_factor, 2500L)
  r2 <- row("C2", "QC2 (High) 1:2500", rule_C)
  expect_equal(r2$source, "QC2 (High)"); expect_equal(r2$dilution_factor, 2500L)
})

# ---- element-set-driven contract --------------------------------------------
test_that("only the elements placed in the order are required", {
  # X rule without DilutionFactor: a missing dilution is NOT an error
  r <- row("X", "051 V1", rule_X)
  expect_equal(nrow(r$issues), 0L)
  # empty required identity element IS an error
  bad <- row("X", "051", rule_X)              # only one token -> TimePeriod empty
  expect_true(any(bad$issues$kind == "missing_required" & bad$issues$field == "TimePeriod"))
})

# ---- integer-denominator dilution parser ------------------------------------
test_that("ai_parse_dilution handles bare / ratio / thousands / rejects decimals", {
  expect_equal(ai_parse_dilution("100")$value, 100L)
  expect_equal(ai_parse_dilution("1:100")$value, 100L)
  expect_equal(ai_parse_dilution("1/100")$value, 100L)
  expect_equal(ai_parse_dilution("1:2,952,450")$value, 2952450L)
  expect_false(ai_parse_dilution("100.5")$ok)
  expect_false(ai_parse_dilution("abc")$ok)
})

# ---- three-state dilution plan ----------------------------------------------
test_that("plan: X/.rbx with no DilutionFactor -> constant", {
  x_desc <- c("051 V1","052 V1","051 V2","IFO 100")
  p <- ai_dilution_plan(x_desc, "X", rule_X)
  expect_equal(p$mode, "constant")
  expect_length(p$needs, 0L)
})

test_that("plan: standards with 1:N -> parsed (many distinct values, no input)", {
  s_desc <- paste("Inhouse Ref 1:", c(2952450,984150,328050,150), sep = "")
  s_desc <- sub("1: ", "1:", s_desc)          # tidy
  s_desc <- c("Inhouse Ref 1:2952450","Inhouse Ref 1:984150","Inhouse Ref 1:150")
  p <- ai_dilution_plan(s_desc, "S", rule_S)
  expect_equal(p$mode, "parsed")
  expect_length(p$needs, 0L)
  expect_equal(unname(p$parsed["Inhouse Ref 1:150"]), 150L)
})

test_that("plan: standards with NO DilutionFactor element -> per unique (all rows)", {
  rule_S_noDF <- ai_parse_rule("S", delimiters = " ", order = c("Source"))
  s_desc <- c("Inhouse Ref 1:2952450","Inhouse Ref 1:984150","Inhouse Ref 1:150")
  p <- ai_dilution_plan(s_desc, "S", rule_S_noDF)
  expect_equal(p$mode, "per_description")
  expect_setequal(p$needs, unique(s_desc))    # every unique curve point needs a value
})

test_that("plan: X partial parse -> hybrid, only the failing unique needs entry", {
  rule_Xd <- ai_parse_rule("X", delimiters = "_", order = c("PatientID","TimePeriod","DilutionFactor"))
  x_desc  <- c("PT01_T0_100","PT02_T0_100","PT03_T0")   # last has no dilution token
  p <- ai_dilution_plan(x_desc, "X", rule_Xd)
  expect_equal(p$mode, "per_description")
  expect_equal(p$needs, "PT03_T0")                        # parsed wells are NOT re-prompted
  expect_equal(unname(p$parsed["PT01_T0_100"]), 100L)
})

# ---- apply + gating ----------------------------------------------------------
test_that("apply: parsed kept, constant fills X, per-description fills failing", {
  # constant
  x_desc <- c("051 V1","052 V1")
  expect_equal(ai_apply_dilution(x_desc, "X", rule_X, constant = 1L), c(1L,1L))
  # parsed
  s_desc <- c("Inhouse Ref 1:2952450","Inhouse Ref 1:150")
  expect_equal(ai_apply_dilution(s_desc, "S", rule_S), c(2952450L,150L))
  # hybrid: keep parsed, fill the failing one
  rule_Xd <- ai_parse_rule("X", delimiters = "_", order = c("PatientID","TimePeriod","DilutionFactor"))
  x2 <- c("PT01_T0_100","PT03_T0")
  got <- ai_apply_dilution(x2, "X", rule_Xd, per_description = c("PT03_T0" = 50L))
  expect_equal(got, c(100L,50L))
})

test_that("dilution gating reflects manual completeness", {
  x_desc <- c("051 V1","052 V1")
  p_const <- ai_dilution_plan(x_desc, "X", rule_X)
  expect_false(ai_dilution_satisfied(p_const, constant = NA))
  expect_true(ai_dilution_satisfied(p_const, constant = 1L))

  rule_S_noDF <- ai_parse_rule("S", delimiters = " ", order = c("Source"))
  s_desc <- c("Inhouse Ref 1:2952450","Inhouse Ref 1:150")
  p_pd <- ai_dilution_plan(s_desc, "S", rule_S_noDF)
  expect_false(ai_dilution_satisfied(p_pd, per_description = c("Inhouse Ref 1:2952450" = 1L)))  # one still missing
  expect_true(ai_dilution_satisfied(
    p_pd, per_description = c("Inhouse Ref 1:2952450" = 1L, "Inhouse Ref 1:150" = 20000L)))
})

# ---- timepoint finalize (space-delimited multi-word preserved) --------------
test_that("timepoint keeps internal spaces only when space is a delimiter", {
  expect_equal(ai_finalize_timepoint("Day 7 Post", TRUE),  "Day 7 Post")
  expect_equal(ai_finalize_timepoint("Day 7 Post", FALSE), "Day7Post")
})

# ---- backward compatibility: single-token positional under "_" --------------
test_that("legacy xPONENT-style still parses positionally", {
  rule_Xd <- ai_parse_rule("X", delimiters = "_", order = c("PatientID","TimePeriod","DilutionFactor"))
  r <- row("X", "PT01_T0_100", rule_Xd)
  expect_equal(r$subject_id, "PT01")
  expect_equal(r$timeperiod, "T0")
  expect_equal(r$dilution_factor, 100L)
})
