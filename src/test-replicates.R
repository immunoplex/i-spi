# =============================================================================
# test-replicates.R  --  Data > Raw inputs > Replicates (replicates_module.R)
# Pure-R parts only (grouping, report, aggregation, naming, assembly); the DB
# write mirrors the wavelength-subtraction pattern and is exercised in the app.
# Run: testthat::test_file("src/test-replicates.R")
# =============================================================================
library(testthat)

if (!exists("rep_aggregate")) {
  root <- c(".", "src", "..", "../src")
  hit  <- root[file.exists(file.path(root, "replicates_module.R"))][1]
  if (is.na(hit)) stop("cannot find src/ -- run from the repo root")
  source(file.path(hit, "replicates_module.R"))
}

mk_sample <- function() {
  # sample 1001 V1 on plate P1: wells A1,A2,A3 (triplicate) for antigen PT;
  #   B1,B2 duplicate for FHA; 1002 single. 1001 also on plate P2 (NOT a replicate).
  data.frame(
    xmap_sample_id = 1:9, study_accession = "S1", experiment_accession = "E1",
    plate_id = "P1", plateid = c(rep("P1", 8), "P2"), plate = "plate_1",
    nominal_sample_dilution = "100", stype = "X",
    sampleid = c("1001","1001","1001","1001","1001","1002","1001","1001","1001"),
    patientid = "p", timeperiod = "V1", dilution = 100, wavelength = "__none__",
    antigen = c("PT","PT","PT","FHA","FHA","PT","PT","PT","PT"), feature = "MFI", source = "Serum",
    well = c("A1","A2","A3","B1","B2","C1","D1","D2","A1"),
    antibody_mfi = c(10, 20, 60, 5, 7, 100, 1000, 3000, 42), antibody_n = 30L,
    gate_class = "ok", masked = c(rep(FALSE, 6), TRUE, TRUE, FALSE), mask_reason = NA_character_,
    project_id = 1L, stringsAsFactors = FALSE)
}

test_that("replicate groups: same plate + identity; masked rows and other plates excluded", {
  d <- rep_detail(mk_sample(), "X")
  expect_equal(nrow(d), 2L)                                   # PT triplicate, FHA duplicate
  expect_equal(sort(d$Replicates), c(2L, 3L))
  expect_equal(d$Wells[d$antigen == "PT"], "A1, A2, A3")
  expect_equal(d$Mean[d$antigen == "PT"], 30)
  expect_equal(d$Median[d$antigen == "PT"], 20)
  expect_equal(d$CV_pct[d$antigen == "FHA"], round(100 * sd(c(5, 7)) / 6, 1))
})

test_that("summary counts duplicates, triplicates, masked and rows after aggregation", {
  s <- rep_summary(list(X = mk_sample(), S = NULL, C = NULL, B = NULL))
  x <- s[s$type == "X", ]
  expect_equal(c(x$rows, x$masked, x$rep_groups, x$duplicates, x$triplicates), c(9, 2, 2, 1, 1))
  expect_equal(x$rows_after, 4L)                              # PT-1001 (3->1), FHA-1001 (2->1), 1002, P2 single
})

test_that("mean and median aggregation; counts sum; QC columns blanked; masked dropped", {
  m <- rep_aggregate(mk_sample(), "mean")
  expect_equal(nrow(m), 4L)
  pt <- m[m$antigen == "PT" & m$sampleid == "1001" & m$plateid == "P1", ]
  expect_equal(pt$antibody_mfi, 30); expect_equal(pt$well, "A1"); expect_equal(pt$antibody_n, 90L)
  expect_true(is.na(pt$gate_class)); expect_false(pt$masked)
  expect_false(any(grepl("^xmap_.*_id$", names(m))))
  med <- rep_aggregate(mk_sample(), "median")
  expect_equal(med$antibody_mfi[med$antigen == "PT" & med$plateid == "P1" & med$sampleid == "1001"], 20)
  expect_equal(m$antibody_mfi[m$plateid == "P2"], 42)         # the other plate is untouched
})

test_that("a table without timeperiod/patientid (controls) groups on what it has", {
  ctl <- data.frame(study_accession = "S1", experiment_accession = "E1", plate_id = "P1",
    plateid = "P1", plate = "plate_1", nominal_sample_dilution = "100", stype = "C1",
    sampleid = "QC1", source = "ref", dilution = 50, wavelength = "delta", antigen = "PT",
    feature = "Absorbance", well = c("E1", "E2"), assay_response = c(1, 3), project_id = 1L,
    masked = FALSE, stringsAsFactors = FALSE)
  a <- rep_aggregate(ctl, "mean")
  expect_equal(nrow(a), 1L); expect_equal(a$assay_response, 2)    # response column auto-detected
})

test_that("new experiment is complete: aggregated types aggregated, others carried (or dropped)", {
  std <- mk_sample(); std$stype <- "S1"
  fr <- list(X = mk_sample(), S = std, C = NULL, B = NULL,
             header = data.frame(xmap_header_id = 1:2, study_accession = "S1",
                                 experiment_accession = "E1", plate_id = c("P1", "P2")))
  b <- rep_build_new_experiment(fr, "E1ag", types = "X", fun = "mean", carry_others = TRUE)
  expect_equal(nrow(b$tables$xmap_sample), 4L)
  expect_equal(nrow(b$tables$xmap_standard), 9L)               # unchanged copy
  expect_true(all(b$tables$xmap_sample$experiment_accession == "E1ag"))
  expect_true(all(b$header$experiment_accession == "E1ag")); expect_false("xmap_header_id" %in% names(b$header))
  expect_equal(b$stats$X$after, 4L); expect_true(b$stats$X$aggregated); expect_false(b$stats$S$aggregated)
  b2 <- rep_build_new_experiment(fr, "E1ag", "X", "mean", carry_others = FALSE)
  expect_null(b2$tables$xmap_standard)
})

test_that("default name: source + 'ag', within 15 characters, '|' removed", {
  expect_equal(rep_default_name("EXP1"), "EXP1ag")
  expect_equal(rep_default_name("ELISA1|D"), "ELISA1Dag")
  expect_equal(rep_default_name("ABCDEFGHIJKLMNO"), "ABCDEFGHIJKLMag")   # 15 chars: trim the source, keep the suffix
  expect_lte(nchar(rep_default_name("ABCDEFGHIJKLMNO")), 15L)
})

test_that("name validation", {
  ex <- c("E1", "E1ag")
  expect_null(rep_validate_name("E1ag2", "E1", ex))
  expect_match(rep_validate_name("", "E1", ex), "Enter a name")
  expect_match(rep_validate_name("ABCDEFGHIJKLMNOP", "E1", ex), "at most 15")
  expect_match(rep_validate_name("E1 ag", "E1", ex), "letters")
  expect_match(rep_validate_name("E1ag", "E1", ex), "already exists")
  expect_match(rep_validate_name("E1", "E1", character(0)), "differ")
})
