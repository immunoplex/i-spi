# =============================================================================
# test-weights-data-check.R
# The "Check the data" step of the Compute-weights panel (weights_data_check.R).
# Run from src/:
#   Rscript -e "testthat::test_file('test-weights-data-check.R')"
# =============================================================================
library(testthat)
if (!exists("weights_data_check")) source("weights_data_check.R")

# n_people people x n_tp time points in one curve group; se log-normal
mk <- function(n_people = 30, n_tp = 3, reads = 1, se_sd = 0.7, censor = NULL,
               patientid = TRUE, seed = 1) {
  set.seed(seed)
  d <- expand.grid(p = seq_len(n_people), t = seq_len(n_tp), r = seq_len(reads))
  data.frame(
    antigen = "PT", feature = "IgG1",
    sampleid = paste0("S", d$p %% 7),                 # NOT a physical-sample id
    patientid = if (patientid) paste0("P", d$p) else "__none__",
    timeperiod = paste0("T", d$t), dilution = as.character(100 * d$r),
    se_concentration = exp(rnorm(nrow(d), log(0.1), se_sd)),
    censor_type = if (is.null(censor)) NA_character_ else rep_len(censor, nrow(d)),
    stringsAsFactors = FALSE)
}

test_that("GAPS-like data: repeated people, no censoring recorded, one reading per sample", {
  ck <- weights_data_check(mk())
  f <- setNames(ck$facts$status, ck$facts$item)
  expect_equal(ck$n_samples, 90L); expect_equal(ck$n_groups, 1L)
  expect_equal(unname(f["censoring"]), "warn")
  expect_match(ck$facts$value[ck$facts$item == "censoring"], "not recorded")
  expect_false(ck$available$use_censoring)
  expect_match(ck$why_unavailable$use_censoring, "not recorded")
  expect_true(ck$available$patient_effect); expect_true(ck$defaults$patient_effect)
  expect_equal(ck$facts$status[ck$facts$item == "dilutions"], "ok")
  expect_false(ck$defaults$fix_kappa)                   # wide SE spread
  expect_match(ck$recommendation, "^Additive model recommended")
  expect_match(ck$recommendation, "several time points")
})

test_that("repeated readings are found per person x time point, never by sampleid", {
  # sampleid repeats across people (as in MADI_P3_GAPS); one reading per person x time
  d <- mk()
  expect_true(anyDuplicated(d$sampleid) > 0)
  expect_equal(weights_data_check(d)$readings_per_sample, 1L)
  # two dilutions of every person x time point: reported, with the limitation
  ck <- weights_data_check(mk(reads = 2))
  expect_equal(ck$readings_per_sample, 2L)
  expect_match(ck$facts$value[ck$facts$item == "dilutions"], "up to 2")
  expect_equal(ck$facts$status[ck$facts$item == "dilutions"], "warn")
  expect_match(ck$facts$note[ck$facts$item == "dilutions"], "separate sample")
  expect_null(ck$available$sample_effect)
})

test_that("censoring recorded: counts, percentages, and the option is on", {
  ck <- weights_data_check(mk(censor = c("none", "none", "left", "right", "interval")))
  v <- ck$facts$value[ck$facts$item == "censoring"]
  expect_match(v, "^60% \\(")
  expect_match(v, "18 below, 18 above, 18 interval")
  expect_true(ck$available$use_censoring); expect_true(ck$defaults$use_censoring)
  expect_match(ck$recommendation, "60% of samples are out of range")
  # recorded but nothing out of range: nothing to censor
  ck0 <- weights_data_check(mk(censor = "none"))
  expect_equal(ck0$facts$value[ck0$facts$item == "censoring"], "none")
  expect_false(ck0$available$use_censoring)
  expect_match(ck0$why_unavailable$use_censoring, "no sample is out of range")
})

test_that("narrow SE spread proposes fixing kappa", {
  ck <- weights_data_check(mk(se_sd = 0.05))
  expect_lt(ck$se_spread, WEIGHTS_SE_SPREAD_NARROW)
  expect_true(ck$defaults$fix_kappa)
  expect_equal(ck$facts$status[ck$facts$item == "se_spread"], "warn")
  expect_match(ck$facts$note[ck$facts$item == "se_spread"], "fixing kappa at 1")
})

test_that("no patient IDs: person and sample terms unavailable, with reasons", {
  ck <- weights_data_check(mk(patientid = FALSE))
  expect_false(ck$available$patient_effect)
  expect_match(ck$why_unavailable$patient_effect, "no patient ID")
  expect_match(ck$facts$value[ck$facts$item == "people"], "no patient IDs")
})

test_that("one sample per person: person term available but off by default", {
  ck <- weights_data_check(mk(n_tp = 1))
  expect_true(ck$available$patient_effect); expect_false(ck$defaults$patient_effect)
})

test_that("stored weights are reported and change the recommendation", {
  st <- data.frame(model = "multiplicative", n = 21252L,
                   last_computed = as.POSIXct("2026-11-20 10:00:00", tz = "UTC"))
  ck <- weights_data_check(mk(), stored = st)
  expect_match(ck$facts$value[ck$facts$item == "stored"], "multiplicative \\(21,252 rows, 2026-11-20\\)")
  expect_equal(ck$facts$status[ck$facts$item == "stored"], "warn")
  expect_match(ck$recommendation, "keep the multiplicative model")
  expect_equal(weights_data_check(mk())$facts$value[6], "none")
})

test_that("SE spread is judged per curve group, on quantified samples", {
  a <- mk(se_sd = 0.7, seed = 2)                       # wide
  b <- mk(se_sd = 0.05, seed = 3); b$antigen <- "FHA"  # narrow
  ck <- weights_data_check(rbind(a, b))
  expect_equal(ck$n_groups, 2L)
  expect_equal(ck$n_groups_narrow, 1L)
  v <- ck$facts$value[ck$facts$item == "se_spread"]
  expect_match(v, "^median .*, lowest 1\\.[0-9]; 1 of 2 groups below 2; fewest quantified samples in a group: 90$")
  expect_equal(ck$facts$status[ck$facts$item == "se_spread"], "warn")
  expect_match(ck$facts$note[ck$facts$item == "se_spread"], "Some groups have a narrow spread")
  expect_match(ck$facts$note[ck$facts$item == "se_spread"], "rough guide")
  # censored samples do not count toward the spread or the quantified count
  cz <- mk(censor = c("none", "left", "left")); ckz <- weights_data_check(cz)
  expect_equal(ckz$min_quantified, 30L)
})

test_that("empty selection does not error", {
  ck <- weights_data_check(mk()[0, ])
  expect_equal(ck$n_samples, 0L)
  expect_match(ck$recommendation, "No calibrated samples")
  expect_equal(weights_data_check(NULL)$n_samples, 0L)
})

test_that("review line describes the job in words", {
  r <- weights_review_text("additive", "bayesian",
                           list(use_censoring = TRUE, patient_effect = TRUE, fix_kappa = FALSE),
                           n_groups = 22, stored_models = "multiplicative")
  expect_match(r, "^Additive model \\(out-of-range samples censored, repeated samples per person accounted for, kappa estimated\\)")
  expect_match(r, "Bayesian calibration, 22 curve groups, about 55 min")   # censoring: ~150 s per group
  r2 <- weights_review_text("additive", "bayesian", list(use_censoring = FALSE), n_groups = 22)
  expect_match(r2, "about 16 min")
  expect_match(r, "Replaces: multiplicative weights for this method\\.$")
  expect_match(weights_review_text("multiplicative", "frequentist", n_groups = 1),
               "^Multiplicative model, frequentist calibration, 1 curve group, about 1 min\\. Replaces: nothing\\.$")
})

test_that("job params: defaults send nothing; changes send only what differs", {
  avail <- list(use_censoring = TRUE, patient_effect = TRUE, fix_kappa = TRUE)
  def <- list(use_censoring = TRUE, patient_effect = TRUE, fix_kappa = FALSE)
  expect_identical(weights_additive_params(def, avail), list())
  p <- weights_additive_params(list(use_censoring = FALSE, patient_effect = FALSE,
                                    fix_kappa = TRUE), avail)
  expect_identical(p, list(use_censoring = "false", patient_effect = "false",
                           fix_kappa = "true"))
  # an unavailable option is never sent, whatever its checkbox says
  none <- list(use_censoring = FALSE, patient_effect = FALSE, fix_kappa = TRUE)
  expect_identical(weights_additive_params(list(use_censoring = FALSE, patient_effect = FALSE), none),
                   list())
})
