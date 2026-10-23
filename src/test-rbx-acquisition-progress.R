# =============================================================================
# test-rbx-acquisition-progress.R
# -----------------------------------------------------------------------------
# 1. A plate's acquisition_date comes from the run stamps inside the .rbx/.srbx
#    (when the plate went through the reader), not from the uploaded file's
#    modification time.
# 2. process_rbx_files() reports plate-level progress through an optional
#    callback, so a long batch can show "Plate i of n".
#
# Run from the repo root: testthat::test_file("src/test-rbx-acquisition-progress.R")
# =============================================================================
library(testthat)

root <- c(".", "src", "..", "../src")
hit  <- root[file.exists(file.path(root, "rbx_binary_parser.R"))][1]
if (is.na(hit)) stop("cannot find src/ -- run from the repo root")
if (!exists("parse_rbx")) source(file.path(hit, "rbx_binary_parser.R"))
dev_dir <- c("dev", file.path(hit, "..", "dev"))[file.exists(c("dev", file.path(hit, "..", "dev")))][1]
sample_file <- function(pat) {
  f <- list.files(dev_dir, pattern = pat, full.names = TRUE)
  if (!length(f)) skip(paste("sample file not available:", pat))
  f[1]
}

test_that("run time is read from the binary (original run, even for 'Reanalysed' files)", {
  f1 <- sample_file("^PIH 08JAN18 Plate 8[.]rbx$")
  m1 <- parse_rbx(f1)$metadata
  expect_s3_class(m1$acquired_first, "POSIXct")
  expect_equal(format(m1$acquired_first, "%Y-%m-%d %H:%M", tz = "UTC"), "2018-01-08 05:37")
  expect_gt(m1$acquired_last, m1$acquired_first)

  f2 <- sample_file("^PIH 08Mar18 Plate 17 Reanalysed")
  m2 <- parse_rbx(f2)$metadata
  expect_equal(format(m2$acquired_first, "%Y-%m-%d", tz = "UTC"), "2018-03-08")  # not 2019 reanalysis
})

test_that(".srbx files carry the stamps too", {
  f <- sample_file("^GBSIGG-WP4-417[.]srbx$")
  m <- parse_rbx(f)$metadata
  expect_equal(format(m$acquired_first, "%Y-%m-%d %H:%M", tz = "UTC"), "2025-11-07 15:08")
})

test_that("no stamp -> empty result, never an error", {
  expect_length(parse_acquisition_times(as.raw(1:50)), 0L)
  expect_length(parse_acquisition_times(raw(0)), 0L)
})

test_that("process_rbx_files reports per-plate progress and uses the run date", {
  skip_if(!exists("clean_plate_id") || !exists("assign_plate_numbers"),
          "batch helpers not loaded (run inside the app environment)")
  fs <- c(sample_file("^PIH 08JAN18 Plate 8[.]rbx$"), sample_file("^PIH 09JAN18 Plate 10"))
  up <- data.frame(name = basename(fs), datapath = fs, stringsAsFactors = FALSE)
  calls <- list()
  p <- process_rbx_files(up, progress = function(i, n, fn, stage)
    calls[[length(calls) + 1]] <<- list(i = i, n = n, fn = fn, stage = stage))
  expect_equal(vapply(calls, `[[`, "", "stage"), c("reading", "done", "reading", "done"))
  expect_equal(vapply(calls, `[[`, 0, "n"), rep(2, 4))
  acq <- vapply(p$header_list, function(h) h$acquisition_date, "")
  expect_match(acq[[1]], "^08-Jan-2018 05:37$")
  expect_match(acq[[2]], "^09-Jan-2018 ")
})
