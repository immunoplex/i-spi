# Tests for fda2018_classify_group(): accuracy from the recovery_pct the compute job
# stores in calib_standards (I-SPI 1.1.0). No database: .calib_q and
# curve_group_members are mocked.
library(testthat)
for (p in c("src/calib_data_access.R", "calib_data_access.R")) if (file.exists(p)) { suppressMessages(source(p)); break }

# Two plates (curves 1, 2), three levels, duplicate wells.
mk_sp <- function(rec = 100) do.call(rbind, lapply(1:2, function(cid) do.call(rbind, lapply(c(3, 2, 1), function(lv)
  data.frame(curve_id = cid, log10_concentration = lv, concentration = 10^lv,
             response_model = lv, assay_response_raw = 100 * lv * c(1, 1.05), included = TRUE,
             recovery_pct = rec, pcov = 5, predicted_log10_concentration = lv, censor_class = "observed")))))

with_mock_db <- function(sp, code) {
  old_q <- .calib_q; old_m <- curve_group_members
  assign(".calib_q", function(pool, sql, params = list())
    if (grepl("calib_fit", sql)) data.frame(curve_id = 1:2, model_name = "logistic4") else sp, envir = globalenv())
  assign("curve_group_members", function(pool, curve_id) 1:2, envir = globalenv())
  on.exit({ assign(".calib_q", old_q, envir = globalenv()); assign("curve_group_members", old_m, envir = globalenv()) })
  force(code)
}

test_that("recovery is the median stored per-well recovery; nothing is inverted", {
  sp <- mk_sp(); sp$recovery_pct <- rep(c(98, 104), length.out = nrow(sp))
  r <- with_mock_db(sp, fda2018_classify_group(NULL, 1L))
  expect_equal(r$summary$status, "OK")
  expect_equal(r$levels$recovery, rep(101, 3))
  expect_equal(r$levels$n_invertible, rep(2L, 3))
  expect_true(all(r$levels$flag == "PASS"))
  expect_false(exists(".fda_backcalc_conc"))
})

test_that("a level without any stored recovery fails accuracy (off-curve), not NA", {
  sp <- mk_sp(); sp$recovery_pct[sp$log10_concentration == 3] <- NA
  r <- with_mock_db(sp, fda2018_classify_group(NULL, 1L))
  top <- r$levels[r$levels$log10c == 3, ]
  expect_equal(top$n_invertible, 0L); expect_false(top$acc_pass); expect_match(top$flag, "^FAIL")
})

test_that("recovery outside the limits fails; standards without stored estimates say so", {
  r <- with_mock_db(mk_sp(rec = 130), fda2018_classify_group(NULL, 1L))
  expect_true(all(!r$levels$acc_pass)); expect_equal(r$summary$status, "NO_PASSING_LEVELS")
  sp <- mk_sp(); sp[, c("recovery_pct", "pcov", "predicted_log10_concentration", "censor_class")] <- NA
  expect_equal(with_mock_db(sp, fda2018_classify_group(NULL, 1L))$summary$status, "NO_STORED_ESTIMATES")
})
