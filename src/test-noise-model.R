# Tests for the noise model setting and the response offset in the app:
# the Compute-fits choice -> job parameter, the curve view's Parameters tab
# caption and term meanings, and the offset-aware natural-unit axis labels.
# Origin: ELISA project 118 (absorbance with responses <= 0).
library(testthat)
.nm_src <- function(f) for (p in c(file.path("src", f), f)) if (file.exists(p)) return(invisible(source(p)))
suppressMessages(library(plotly))
if (!exists(".sc_response_caption")) .nm_src("std_curve_view_module.R")
if (!exists("noise_model_param")) .nm_src("std_curve_calc_module.R")

test_that("Compute fits sends noise_model only for an explicit choice", {
  expect_null(noise_model_param("settings"))          # cascade decides
  expect_null(noise_model_param(NULL))
  expect_null(noise_model_param("power"))
  expect_identical(noise_model_param("add_prop"), "add_prop")
  expect_identical(noise_model_param("constant"), "constant")
  expect_identical(unname(NOISE_MODEL_CHOICES[1]), "settings")
})

test_that("Parameters tab caption names the noise model and the offset", {
  expect_match(.sc_response_caption(NULL), "Noise model: constant")
  expect_match(.sc_response_caption(NULL), "no response offset")
  r <- data.frame(noise_model = "add_prop", response_offset = 0.006, blank_value = 0)
  cap <- .sc_response_caption(r)
  expect_match(cap, "additive \\+ proportional")
  expect_match(cap, "c = 0.006")
  expect_false(grepl("blank value", cap))
  r2 <- data.frame(noise_model = "constant", response_offset = 0, blank_value = 0.012)
  expect_match(.sc_response_caption(r2), "blank value subtracted: 0.012")
  expect_match(.sc_response_caption(r[0, ]), "Noise model: constant")   # no fit row
})

test_that("noise terms get a plain-language meaning; curve terms are left blank", {
  p <- data.frame(term = c("a", "sigma_add", "cv"), estimate = c(0.05, 0.004, 0.06))
  v <- .sc_params_view(p)
  expect_identical(v$meaning[1], "")
  expect_match(v$meaning[2], "Additive noise floor")
  expect_match(v$meaning[3], "fraction of the response")
})

test_that("axis labels undo the offset and add back the blank value", {
  expect_equal(.sc_y_natural(log10(0.106), offset = 0.006), 0.1)
  expect_equal(.sc_y_natural(log10(0.5)), 0.5)
  expect_equal(.sc_y_natural(log10(0.2), offset = 0, blank_value = 0.05), 0.25)
  expect_equal(.sc_y_natural(1, offset = NA), 10)
})
