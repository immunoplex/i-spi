# =============================================================================
# test-weights-model-ui.R
# Pure-logic checks for the model-aware parts of the Precision Weights tab
# (helpers at the top of precision_weight_panel.R). Run from src/:
#   Rscript -e "testthat::test_file('test-weights-model-ui.R')"
#
# The contract these pin: with the feature flag off, and for data written
# before the additive model existed, nothing the user sees changes.
# =============================================================================
library(testthat)
if (!exists("weights_fit_table")) source("precision_weight_panel.R")

old_fit_cols <- c("multiplate_group_id", "method", "antigen", "feature",
                  "design_cols", "scale_predictor", "phi", "beta1",
                  "interpretation", "n_fit", "n_eff", "weight_ratio")
old_sample_cols <- c("antigen", "feature", "curve_id", "method", "sampleid",
                     "patientid", "timeperiod", "agroup", "se", "pcov",
                     "sigma", "w", "w_norm")

mult_fit <- function(model_col = FALSE) {
  d <- data.frame(multiplate_group_id = "g1", method = "bayesian", antigen = "PT",
                  feature = "IgG1", design_cols = "timeperiod,agroup",
                  scale_predictor = "se", phi = 1.23456, beta1 = 0.98765,
                  interpretation = "calibrated", n_fit = 100L, n_eff = 80.5,
                  weight_ratio = 4.2, source = "plate", stringsAsFactors = FALSE)
  if (model_col) {
    d$model <- "multiplicative"
    # columns that exist after the 0.6.0 migration but are empty for old rows
    d$tau <- NA_real_; d$kappa <- NA_real_; d$reliability <- NA_real_
    d$n_censored <- NA_integer_; d$fix_beta1 <- NA; d$identifiability <- NA_character_
  }
  d
}
add_fit <- function() {
  d <- mult_fit(TRUE)
  d$model <- "additive"; d$phi <- NA_real_
  d$tau <- 0.4123; d$kappa <- 1; d$reliability <- 0.8123
  d$n_censored <- 28L; d$fix_beta1 <- TRUE; d$identifiability <- "kappa identified"
  d
}

test_that("feature flag defaults off and is read from the option", {
  op <- options(ispi.enable_additive_weights = NULL); on.exit(options(op))
  expect_false(additive_weights_enabled())
  options(ispi.enable_additive_weights = TRUE)
  expect_true(additive_weights_enabled())
  options(ispi.enable_additive_weights = "yes")   # only a real TRUE counts
  expect_false(additive_weights_enabled())
})

test_that("flag off: no model selector, no links, predictor always visible", {
  op <- options(ispi.enable_additive_weights = NULL); on.exit(options(op))
  ns <- shiny::NS("w")
  expect_null(weights_model_selector_ui(ns))
  expect_null(weights_docs_links())
  expect_identical(weights_js_is_multiplicative(ns), "true")
})

test_that("flag on: selector present, links point at the published articles", {
  op <- options(ispi.enable_additive_weights = TRUE); on.exit(options(op))
  ns <- shiny::NS("w")
  html <- as.character(weights_model_selector_ui(ns))
  expect_match(html, "w-weights_model")
  expect_match(html, "additive")
  expect_match(html, "replaces the stored weights")
  expect_match(html, "curveRweights/articles/measurement-error.html", fixed = TRUE)
  expect_match(html, "curveRweights/articles/downstream-analysis.html", fixed = TRUE)
  expect_match(html, "curveRfreq", fixed = TRUE)
  expect_match(html, "curveRbayes", fixed = TRUE)
  expect_match(weights_js_is_multiplicative(ns), "!= 'additive'", fixed = TRUE)
})

test_that("models present: missing column or NA means multiplicative", {
  expect_identical(weights_models_present(NULL), character(0))
  expect_identical(weights_models_present(mult_fit()), "multiplicative")
  d <- rbind(mult_fit(TRUE), add_fit())
  expect_identical(weights_models_present(d), c("additive", "multiplicative"))
  d$model[1] <- NA
  expect_identical(weights_models_present(d), c("additive", "multiplicative"))
})

test_that("fit table for multiplicative data is unchanged, with or without new columns", {
  expect_identical(names(weights_fit_table(mult_fit())), old_fit_cols)
  expect_identical(names(weights_fit_table(mult_fit(model_col = TRUE))), old_fit_cols)
  expect_equal(weights_fit_table(mult_fit())$phi, 1.235)
})

test_that("fit table shows additive columns once an additive fit exists", {
  out <- weights_fit_table(rbind(mult_fit(TRUE), add_fit()))
  expect_true(all(c("model", "phi", "beta1", "tau", "kappa", "reliability",
                    "n_censored", "fix_beta1", "identifiability") %in% names(out)))
  add_only <- weights_fit_table(add_fit())
  expect_false("phi" %in% names(add_only))
  expect_equal(add_only$tau, 0.412); expect_equal(add_only$reliability, 0.81)
})

test_that("per-sample table for multiplicative data keeps the old columns", {
  s <- data.frame(antigen = "PT", feature = "IgG1", curve_id = 1L, method = "bayesian",
                  sampleid = "s", patientid = "p", timeperiod = "t", agroup = "a",
                  se = .1, pcov = 5, sigma = .123456, w = 65.4321, w_norm = 1.23456,
                  model = "multiplicative", v_meas = NA_real_, tau2 = NA_real_,
                  reliability = NA_real_, reliability_lo = NA_real_,
                  reliability_hi = NA_real_, censor_type = NA_character_,
                  bound = NA_real_, stringsAsFactors = FALSE)
  expect_identical(names(weights_sample_table(s)), old_sample_cols)
  s2 <- s; s2$model <- "additive"; s2$v_meas <- .01; s2$tau2 <- .16
  s2$reliability <- .9123; s2$censor_type <- "none"
  out <- weights_sample_table(s2)
  expect_true(all(c("model", "v_meas", "tau2", "reliability", "censor_type") %in% names(out)))
  expect_equal(out$reliability, 0.912)
})

test_that("annotation lines: multiplicative text is exactly the legacy text", {
  r <- mult_fit()
  expect_identical(weights_fit_annotation(r),
                   "plate: phi=1.23 beta1=0.988 n_eff=80.5\ncalibrated")
  expect_identical(weights_fit_annotation(mult_fit(TRUE)),
                   "plate: phi=1.23 beta1=0.988 n_eff=80.5\ncalibrated")
  r$phi <- NA
  expect_match(weights_fit_annotation(r), "phi=NA", fixed = TRUE)
})

test_that("annotation lines for the additive model name tau, kappa and reliability", {
  a <- weights_fit_annotation(add_fit())
  expect_match(a, "tau=0.412", fixed = TRUE)
  expect_match(a, "kappa=1", fixed = TRUE)
  expect_match(a, "reliability=0.81", fixed = TRUE)
  expect_false(grepl("phi=", a))
})

test_that("caption describes the model(s) actually drawn", {
  expect_match(weights_model_caption("multiplicative"), "sigma_i = phi * se_i^beta1", fixed = TRUE)
  expect_match(weights_model_caption(character(0)), "phi")            # legacy default
  expect_match(weights_model_caption("additive"), "tau^2", fixed = TRUE)
  expect_match(weights_model_caption("additive"), "1/tau^2", fixed = TRUE)
  mixed <- weights_model_caption(c("additive", "multiplicative"))
  expect_match(mixed, "Mixed models"); expect_match(mixed, "phi"); expect_match(mixed, "tau")
})

test_that("caption wrapping fits the drawn width and keeps paragraphs", {
  cap <- paste0(weights_model_caption("additive"), "\n\nSecond paragraph with some words in it.")
  for (w in c(4, 6, 9, 12)) {
    lines <- strsplit(wrap_weights_caption(cap, w), "\n", fixed = TRUE)[[1]]
    expect_lte(max(nchar(lines)), floor((w - 0.4) * 13) + 0L, label = paste("width", w))
  }
  expect_true(grepl("\n\n", wrap_weights_caption(cap, 6), fixed = TRUE))   # paragraph break kept
  expect_identical(wrap_weights_caption("short", 9), "short")
  expect_identical(wrap_weights_caption(cap, NULL), wrap_weights_caption(cap, 9))  # default 9 in
  expect_identical(wrap_weights_caption(cap, NA_real_), wrap_weights_caption(cap, 9))
  # a tiny window still wraps to a usable minimum rather than one word per line
  expect_gte(max(nchar(strsplit(wrap_weights_caption(cap, 0.5), "\n")[[1]])), 20)
})

# ---- weights metadata in the download bundles --------------------------------
if (!exists("weights_bundle_meta")) source("data_dictionary.R")

test_that("bundle metadata: no weights gives one explanatory row", {
  m <- weights_bundle_meta(NULL, NULL)
  expect_equal(nrow(m), 1L); expect_equal(m$n_samples, 0L)
  expect_match(m$description, "No precision weights")
  expect_equal(nrow(weights_bundle_meta(data.frame(), NULL)), 1L)
})

test_that("bundle metadata for pre-0.6.0 (multiplicative, no model column) rows", {
  w <- data.frame(method = "bayesian", sampleid = 1:3)
  m <- weights_bundle_meta(w, data.frame(multiplate_group_id = c("a", "b"), method = "bayesian"))
  expect_equal(m$model, "multiplicative"); expect_equal(m$n_samples, 3L)
  expect_equal(m$n_curve_groups, 2L); expect_equal(m$n_censored_samples, 0L)
  expect_match(m$description, "phi * se_i^beta1", fixed = TRUE)
  expect_match(m$guide, "measurement-error.html", fixed = TRUE)
})

test_that("bundle metadata separates methods and models, counts censoring and versions", {
  w <- data.frame(method = c("bayesian", "bayesian", "bayesian", "frequentist"),
                  model = c("additive", "additive", "additive", "multiplicative"),
                  weights_pkg_version = c("0.3.0", "0.3.0", "0.3.0", "0.2.2"),
                  censor_type = c("none", "left", "right", NA),
                  created_at = c("2026-11-12 10:00:00", "2026-11-12 10:00:00",
                                 "2026-11-12 11:00:00", "2026-11-01 09:00:00"),
                  stringsAsFactors = FALSE)
  f <- data.frame(multiplate_group_id = c("a", "a"), method = c("bayesian", "frequentist"),
                  model = c("additive", "multiplicative"), stringsAsFactors = FALSE)
  m <- weights_bundle_meta(w, f)
  expect_equal(nrow(m), 2L)
  a <- m[m$model == "additive", ]
  expect_equal(a$n_samples, 3L); expect_equal(a$n_censored_samples, 2L)
  expect_equal(a$weights_pkg_version, "0.3.0"); expect_equal(a$n_curve_groups, 1L)
  expect_equal(a$first_computed, "2026-11-12 10:00:00"); expect_equal(a$last_computed, "2026-11-12 11:00:00")
  expect_match(a$description, "tau^2", fixed = TRUE)
  expect_equal(m$weights_pkg_version[m$model == "multiplicative"], "0.2.2")
})
