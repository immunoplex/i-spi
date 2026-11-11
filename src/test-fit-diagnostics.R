# =============================================================================
# test-fit-diagnostics.R  --  fit-diagnostics report + assessment (pure R).
# Run: testthat::test_file("src/test-fit-diagnostics.R")
# =============================================================================
library(testthat)

if (!exists("fit_diag_build")) {
  root <- c(".", "src", "..", "../src")
  hit  <- root[file.exists(file.path(root, "fit_diagnostics_report.R"))][1]
  if (is.na(hit)) stop("cannot find src/ -- run from the repo root")
  source(file.path(hit, "fit_diagnostics_report.R"))
}

# Unit 1: 10 plates pooled in one job, P7 discordant and failing a gate, noisy sampler.
# Unit 2: 5 healthy plates from an earlier job.
mk_data <- function(bad_sampler = TRUE) {
  ids1 <- 1:10; ids2 <- 11:15
  ids  <- c(ids1, ids2)
  grp  <- c(rep("G", 10), rep("G", 5))             # same multiplate group, two jobs
  job  <- c(rep("J3", 10), rep("J1", 5))
  fit <- data.frame(curve_id = ids, method = "bayesian", model_name = "logistic4",
    is_best = TRUE, is_fallback = FALSE, eligible = ids != 7L, job_id = job,
    plateid = paste0("P", ids), antigen = "Ag", feature = "Abs", source = "Serum",
    wavelength = "delta", stringsAsFactors = FALSE)
  d_est <- ifelse(ids == 7L, 1.2, 3.0 + 0.05 * (ids %% 3))
  param <- do.call(rbind, lapply(seq_along(ids), function(i) data.frame(
    curve_id = ids[i], method = "bayesian", model_name = "logistic4",
    term = c("a", "b", "c", "d"),
    estimate = c(0.05, 1.0, 0.1 * (i %% 4), d_est[i]),
    std_error = c(0.01, 0.05, 0.05, if (ids[i] == 7L) 0.9 else 0.1), stringsAsFactors = FALSE)))
  diag <- data.frame(curve_id = ids, method = "bayesian", model_name = "logistic4",
    fit_seconds = ifelse(ids %in% ids1, 100, 40), fit_seed = ifelse(ids %in% ids1, 1, 2),
    converged = TRUE, rhat_max = ifelse(ids %in% ids1 & bad_sampler, 1.03, 1.001),
    ess_bulk_min = ifelse(ids %in% ids1 & bad_sampler, 250, 2000), ess_tail_min = 1500,
    n_divergent = ifelse(ids %in% ids1 & bad_sampler, 25L, 0L),
    pct_divergent = ifelse(ids %in% ids1 & bad_sampler, 0.6, 0),
    max_treedepth_hit = ifelse(ids %in% ids1 & bad_sampler, 12L, 0L), ebfmi_min = 0.9,
    stringsAsFactors = FALSE)
  hyper <- do.call(rbind, lapply(ids, function(cid) data.frame(
    curve_id = cid, method = "bayesian", model_name = "logistic4",
    term = c("mu_d", "sigma_d", "nu", "sigma_obs"),
    estimate = if (cid %in% ids1) c(2.9, 1.8, 3, 0.1) else c(3.0, 0.1, 30, 0.1),
    std_error = 0.1, q_lo = 0, q_hi = 1, stringsAsFactors = FALSE)))
  gate <- data.frame(curve_id = 7L, method = "bayesian", model_name = "logistic4",
    gate = "rel_se", passed = FALSE, detail = "SE/|est| 0.9 > 0.5", stringsAsFactors = FALSE)
  list(fit = fit, diag = diag, hyper = hyper, param = param, gate = gate,
       groups = data.frame(curve_id = ids, multiplate_group_id = grp), loo = NULL, run = NULL, lookup = NULL)
}

test_that("fit units are (group, job): the 1-5 / 1-10 overwrite case gives two units", {
  rep <- fit_diag_build(mk_data(), list(project_id = 92, study = "S", experiment = "ELISA"))
  expect_length(rep$units, 2L)
  expect_setequal(vapply(rep$units, `[[`, 0L, "n_plates"), c(10L, 5L))
})

test_that("pooled vs per-plate fit mode is inferred from the shared seed/runtime", {
  rep <- fit_diag_build(mk_data())
  big <- Filter(function(i) i$n_plates == 10L, rep$units)[[1]]
  expect_match(big$fit_mode, "pooled")
  d <- mk_data(); d$diag$fit_seed <- seq_len(nrow(d$diag))
  big2 <- Filter(function(i) i$n_plates == 10L, fit_diag_build(d)$units)[[1]]
  expect_match(big2$fit_mode, "per-plate")
})

test_that("the discordant plate is found three ways: outlier, ineligible, weak", {
  rep <- fit_diag_build(mk_data())
  big <- Filter(function(i) i$n_plates == 10L, rep$units)[[1]]
  expect_true("P7" %in% big$outlier_plates)
  expect_equal(big$ineligible_plates, "P7")
  expect_true("P7" %in% big$weak_plates)
  ids <- big$findings$finding
  expect_true(all(c("heterogeneity-outlier", "eligibility", "weak-plate",
                    "heterogeneity-signal-level", "population-wide") %in% ids))
  expect_match(big$findings$evidence[big$findings$finding == "eligibility"], "P7: rel_se")
})

test_that("sampler problems are reported with their numbers", {
  rep <- fit_diag_build(mk_data())
  f <- rep$findings
  expect_true(all(c("sampler-rhat", "sampler-ess", "sampler-divergences", "sampler-treedepth") %in% f$finding))
  expect_equal(f$severity[f$finding == "sampler-divergences"], "warn")   # 0.6% < 1%
  d <- mk_data(); d$diag$pct_divergent[d$diag$curve_id <= 10] <- 2
  expect_equal(fit_diag_build(d)$findings$severity[fit_diag_build(d)$findings$finding == "sampler-divergences"], "error")
})

test_that("the healthy unit raises nothing; pooled-size note only rides on real problems", {
  rep <- fit_diag_build(mk_data())
  healthy <- Filter(function(i) i$n_plates == 5L, rep$units)[[1]]
  expect_null(healthy$findings); expect_null(healthy$strategy)
  big <- Filter(function(i) i$n_plates == 10L, rep$units)[[1]]
  expect_true("pooled-size" %in% big$findings$finding)
})

test_that("strategy: review the plate first, then batch/per-plate, then sampler knobs", {
  rep <- fit_diag_build(mk_data())
  s <- rep$strategy; s <- s[order(s$priority), ]
  expect_match(s$action[1], "P7"); expect_equal(s$priority[1], 1)
  expect_true(any(grepl("bayes_single_plate", s$action)))
  expect_true(any(grepl("adapt_delta", s$action)))
})

test_that("notice is NULL for a clean fit and names the cause for a bad one", {
  d <- mk_data(); d$fit <- d$fit[d$fit$curve_id %in% 11:15, ]
  expect_null(fit_diag_notice(fit_diag_build(d)))
  n <- fit_diag_notice(fit_diag_build(mk_data()))
  expect_equal(n$severity, "warn"); expect_match(paste(n$text, collapse = " "), "10 plate")
})

test_that("a finished job is only blamed for its own fits", {
  rep <- fit_diag_build(mk_data())
  expect_equal(fit_diag_notice(rep, "J3")$severity, "warn")      # the noisy 10-plate job
  expect_null(fit_diag_notice(rep, "J1"))                        # the healthy 5-plate job
  expect_null(fit_diag_notice(rep, "no-such-job"))
})

test_that("text report is self-contained and the JSON parses", {
  rep <- fit_diag_build(mk_data(), list(project_id = 92, study = "Bionet_new", experiment = "ELISA"))
  txt <- fit_diag_report_text(rep)
  for (h in c("# I-SPI compute-fit diagnostics report", "## How to read this", "## Automated assessment",
              "## Recommended strategy", "## Sampler / optimizer diagnostics", "Per-plate curve parameters"))
    expect_true(grepl(h, txt, fixed = TRUE), info = h)
  expect_true(grepl("Project: 92", txt, fixed = TRUE))
  j <- jsonlite::fromJSON(as.character(fit_diag_report_json(rep)))
  expect_equal(j$severity, "warn"); expect_true(nrow(j$findings) > 5)
})

test_that("the tab: build on click, verdict + preview + downloads, stale on scope change", {
  library(shiny)
  if (!exists("fitDiagnosticsServer")) source(file.path(hit, "fit_diagnostics_module.R"))
  sc <- reactiveVal(list(project_id = 92, study = "Bionet_new", experiment = "ELISA"))
  testServer(fitDiagnosticsServer,
             args = list(scope = reactive(sc()), pool = NULL, fetch = function(...) mk_data()), {
    expect_null(output$verdict)                       # nothing before the click
    session$setInputs(build = 1)
    expect_false(is.null(rpt()))
    expect_match(as.character(output$verdict$html), "Check these fits")
    expect_match(as.character(output$verdict$html), "10 plate")
    expect_match(as.character(output$preview_ui$html), "Recommended strategy")
    f <- output$dl_md
    expect_match(basename(f), "^fit_diagnostics_Bionet_new_ELISA_\\d{8}\\.md$")
    expect_true(grepl("Automated assessment", paste(readLines(f), collapse = "\n"), fixed = TRUE))
    j <- jsonlite::fromJSON(paste(readLines(output$dl_json), collapse = "\n"))
    expect_equal(j$scope$experiment, "ELISA")
    sc(list(project_id = 92, study = "Bionet_new", experiment = "Other"))
    session$flushReact()
    expect_null(rpt())                             # report belongs to the old experiment
  })
  testServer(fitDiagnosticsServer,
             args = list(scope = reactive(sc()), pool = NULL, fetch = function(...) stop("db down")), {
    session$setInputs(build = 1)
    expect_null(rpt())                             # error is surfaced, not thrown
  })
})

scale_frames <- function(d, raw_ids, log_ids, is_log_setting = "false", nu = 2.1) {
  ids <- d$fit$curve_id
  d$std <- data.frame(curve_id = ids, method = "bayesian", n_std = 8L, n_included = 8L,
    raw_min = 0.0075, raw_max = 3.65,
    dev_raw   = ifelse(ids %in% raw_ids, 0, 0.9),
    dev_log10 = ifelse(ids %in% log_ids, 0, 1.1), dev_ln = 2.5)
  d$settings <- data.frame(feature = "Abs", antigen = "Ag", param = "is_log_response",
                           value = is_log_setting, stringsAsFactors = FALSE)
  d$hyper$estimate[d$hyper$term == "nu"] <- nu
  d$run <- data.frame(job_id = c("J3", "J1"), params = c('{"use_heteroscedastic_noise": false}', "{}"))
  d
}

test_that("scale: raw-scale fit with nu pinned at 2 points to the log scale, not the plate count", {
  d <- scale_frames(mk_data(), raw_ids = 1:15, log_ids = integer(0))
  rep <- fit_diag_build(d)
  big <- Filter(function(i) i$n_plates == 10L, rep$units)[[1]]
  expect_equal(big$scale, "raw")
  f <- big$findings[big$findings$finding == "noise-heavy-tails", ]
  expect_equal(f$severity, "warn"); expect_match(f$action, "is_log_response = true")
  s <- big$strategy[order(big$strategy$priority), ]
  expect_equal(s$priority[1], 0); expect_match(s$action[1], "log scale")
  expect_match(s$why[1], "Plate count is not the cause")
  expect_match(fit_diag_report_text(rep), "fitted on the **raw** scale", fixed = TRUE)
  expect_match(fit_diag_report_text(rep), "use_heteroscedastic_noise", fixed = TRUE)
})

test_that("scale: a stored fit that disagrees with the current setting is flagged stale", {
  d <- scale_frames(mk_data(), raw_ids = integer(0), log_ids = 1:15, is_log_setting = "false", nu = 25)
  rep <- fit_diag_build(d)
  expect_true("settings-stale" %in% rep$findings$finding)
  d2 <- scale_frames(mk_data(), raw_ids = integer(0), log_ids = 1:15, is_log_setting = "true", nu = 25)
  expect_false("settings-stale" %in% fit_diag_build(d2)$findings$finding)
})

test_that("scale: units fitted on different scales are flagged at experiment level", {
  d <- scale_frames(mk_data(), raw_ids = 1:10, log_ids = 11:15, nu = 25)
  rep <- fit_diag_build(d)
  expect_true("scale-mixed" %in% rep$findings$finding)
  expect_equal(rep$strategy$priority[rep$strategy$unit == "(experiment)"], 0)
  expect_match(rep$findings$evidence[rep$findings$finding == "scale-mixed"], "raw")
})

test_that("integer64 curve ids are keyed correctly", {
  skip_if_not_installed("bit64")
  d <- mk_data(); for (n in names(d)) if (is.data.frame(d[[n]]) && "curve_id" %in% names(d[[n]]))
    d[[n]]$curve_id <- bit64::as.integer64(d[[n]]$curve_id)
  expect_length(fit_diag_build(d)$units, 2L)
})

test_that("missing pieces degrade instead of failing", {
  d <- mk_data(); d$diag <- NULL; d$hyper <- NULL; d$param <- NULL; d$gate <- NULL
  rep <- fit_diag_build(d)
  expect_true("no-sampler-diagnostics" %in% rep$findings$finding)
  expect_type(fit_diag_report_text(rep), "character")
  empty <- fit_diag_build(list(fit = NULL))
  expect_match(fit_diag_report_text(empty), "No fitted curves")
  expect_null(fit_diag_notice(empty))
})
