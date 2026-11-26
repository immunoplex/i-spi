# =============================================================================
# test-bead-count-gc.R
# bead_count_gc() must not fail when an experiment lacks one of the optional
# display columns (preprod log 2026-11-24: "[.data.frame: undefined columns
# selected" in the low-bead-count table).
# Run from src/: testthat::test_file("test-bead-count-gc.R")
# =============================================================================
library(testthat)
source("bead_count_functions.R", local = TRUE)

wells <- data.frame(study_accession = "S1", experiment_accession = "E1", plateid = "P1",
                    well = c("A1", "A2", "A3"), well_number = c(3, 1, 2),
                    antigen = "Ag", n = c(80, 20, NA), stringsAsFactors = FALSE)

test_that("missing optional columns are skipped, not an error", {
  out <- bead_count_gc(wells, lower_threshold = 35, upper_threshold = 50,
                       failed_well_criteria = "Below_Upper_Threshold")
  expect_equal(names(out), c("study_accession", "experiment_accession", "plateid", "well",
                             "antigen", "n", "bead_count_gc", "is_low_bead_count"))
  expect_equal(out$well, c("A2", "A3", "A1"))                # ordered by well_number
  expect_equal(out$is_low_bead_count, c(TRUE, NA, FALSE))
})

test_that("lower threshold and no-threshold paths still work", {
  out <- bead_count_gc(wells, 35, NA, "Below_Lower_Threshold")
  expect_equal(as.character(out$bead_count_gc), c("Low Bead Count", NA, "Sufficient Bead Count"))
  expect_equal(nrow(bead_count_gc(wells, NA, NA, "Below_Upper_Threshold")), 0L)
})
