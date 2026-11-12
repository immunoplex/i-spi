# Tests for linearity_backcalc.R: controls and standards in QC > Replicate Linearity.
# Synthetic curves with known parameters, so the back-calculation has a right answer.
# No database (lin_fetch_extra is exercised through a mocked .calib_q).
library(testthat)
.bc_src <- function(f) for (p in c(file.path("src", f), f)) if (file.exists(p)) return(invisible(source(p)))
if (!exists(".fda_backcalc_conc")) suppressMessages(.bc_src("calib_data_access.R"))
if (!exists("lin_prepare")) .bc_src("linearity_report.R")
if (!exists("lin_backcalc")) .bc_src("linearity_backcalc.R")

# Forward models: the inverses in calib_data_access.R undo these.
.fwd <- function(model, x, p) {
  with(p, switch(model,
    logistic4    = { e <- exp((x - c) / b); (a + d * e) / (1 + e) },
    logistic5    = a + (d - a) / (1 + exp(-(x - c) / b))^g,
    loglogistic4 = a + (d - a) / (1 + (c / 10^x)^b),
    loglogistic5 = a + (d - a) * (1 + g * exp(-b * (x - c)))^(-1 / g),
    gompertz4    = a + (d - a) * exp(-exp(-b * (x - c)))))
}
.P <- list(a = 1, b = 0.8, c = 2, d = 4, g = 1.5)

# Two plates x two methods, logistic4 on the log10 response scale. pcov is a straight
# line in log10 concentration, so reading it back by interpolation is exact.
mk_curves <- function(model = "logistic4", p = .P) {
  cur <- data.frame(curve_id = c(101L, 101L, 102L, 102L), method = rep(c("frequentist", "bayesian"), 2),
                    plate = rep(c("P1", "P2"), each = 2), stringsAsFactors = FALSE)
  best <- data.frame(curve_id = cur$curve_id, method = cur$method, model_name = model, stringsAsFactors = FALSE)
  terms <- names(p)[vapply(p, is.numeric, TRUE)]
  if (model %in% c("logistic4", "loglogistic4", "gompertz4")) terms <- setdiff(terms, "g")
  params <- do.call(rbind, lapply(seq_len(nrow(cur)), function(i)
    data.frame(curve_id = cur$curve_id[i], method = cur$method[i], model_name = model, term = terms,
               estimate = unlist(p[terms]), q_med = unlist(p[terms]), stringsAsFactors = FALSE)))
  xs <- seq(-2, 6, by = 0.04)
  grid <- do.call(rbind, lapply(seq_len(nrow(cur)), function(i)
    data.frame(curve_id = cur$curve_id[i], method = cur$method[i], log10_concentration = xs,
               predicted_response = .fwd(model, xs, p), pcov = 20 + 5 * xs, stringsAsFactors = FALSE)))
  diag <- data.frame(curve_id = cur$curve_id, method = cur$method, lloq_log10 = 0, uloq_log10 = 4,
                     inflect_x = 2, pcov_threshold = 20, stringsAsFactors = FALSE)
  list(cur = cur, best = best, params = params, grid = grid, diag = diag)
}

# ---- pieces ---------------------------------------------------------------------------------
test_that("the response is put on the fitted scale, with the worker's floor", {
  expect_equal(lin_model_response(c(100, 0, NA), "log10"), c(2, -6, NA))
  expect_equal(lin_model_response(10, "ln"), log(10))
  expect_equal(lin_model_response(c(5, 7), "raw"), c(5, 7))
  expect_true(all(is.na(lin_model_response(5, "something"))))
})

test_that("the response scale is read from the stored deviations, with an experiment-wide fallback", {
  s <- data.frame(curve_id = 1:4, method = "frequentist",
                  dev_raw = c(2, 2, 2, NA), dev_log10 = c(0, 0, 0.7, NA), dev_ln = c(0.9, 0.9, 0.9, NA))
  r <- lin_scales(s)
  expect_equal(r$scale, c("log10", "log10", "log10", "log10"))
  expect_equal(r$inferred, c(FALSE, FALSE, TRUE, TRUE))            # 0.7 off both: not a plain transform
  r2 <- lin_scales(data.frame(curve_id = 1:2, method = "bayesian", dev_raw = c(0, 3), dev_log10 = c(5, 0), dev_ln = c(5, 5)))
  expect_equal(r2$scale, c("raw", "log10"))
  expect_equal(nrow(lin_scales(NULL)), 0L)
})

test_that("gate class follows LLOQ/ULOQ, else the pcov threshold and the inflection", {
  g <- lin_gate_class(c(-1, 1, 5, NA), c(30, 10, 30, 10), 20, lloq = 0, uloq = 4)
  expect_equal(g$pcov_gate_class, c("belowLLOQ", "inDynamicRange", "aboveULOQ", NA))
  expect_equal(g$pcov_pass, c(FALSE, TRUE, FALSE, NA))
  f <- lin_gate_class(c(-1, 1, 5), c(30, 10, 30), 20, lloq = NA, uloq = NA, inflect = 2)   # fallback
  expect_equal(f$pcov_gate_class, c("belowLLOQ", "inDynamicRange", "aboveULOQ"))
  nf <- lin_gate_class(c(-1, 5), c(30, 30), 20)                                            # no inflection: failing side unknown
  expect_true(all(is.na(nf$pcov_gate_class))); expect_equal(nf$pcov_pass, c(FALSE, FALSE))
})

# ---- the back-calculation ---------------------------------------------------------------------
test_that("every model form is inverted back to the concentration that made the response", {
  for (m in c("logistic4", "logistic5", "loglogistic4", "loglogistic5", "gompertz4")) {
    cv <- mk_curves(m)
    xt <- c(0.5, 1.5, 2.5, 3.2)
    resp <- data.frame(curve_id = 101L, method = "frequentist", y = .fwd(m, xt, .P), tag = letters[1:4])
    r <- lin_backcalc(resp, cv$best, cv$params, cv$grid, cv$diag)
    expect_equal(r$conc, 10^xt, tolerance = 1e-6, info = m)
    expect_equal(r$x_log10, xt, tolerance = 1e-6, info = m)
    expect_equal(r$pcov, 20 + 5 * xt, tolerance = 1e-9, info = m)       # the grid read at the estimate
    expect_equal(r$tag, letters[1:4])                                   # pass-through columns survive
    expect_true(all(r$model_name == m))
  }
})

test_that("a response off the curve has no concentration, the capped pcov and no gate class", {
  cv <- mk_curves()
  resp <- data.frame(curve_id = 101L, method = "frequentist", y = c(.P$d + 1, .P$a - 1, 2.5))
  r <- lin_backcalc(resp, cv$best, cv$params, cv$grid, cv$diag)
  expect_true(all(is.na(r$conc[1:2]))); expect_equal(r$pcov[1:2], c(150, 150))
  expect_true(all(is.na(r$pcov_gate_class[1:2]))); expect_true(all(is.na(r$pcov_pass[1:2])))
  expect_true(is.finite(r$conc[3]))
  # downstream these become "unclassified", not dropped
  raw <- data.frame(curve_id = r$curve_id, method = r$method, sampleid = "c", dilution = "1", final_concentration = r$conc,
                    pcov = r$pcov, pcov_pass = r$pcov_pass, pcov_gate_class = r$pcov_gate_class, plateid = "P1", plate = "P1",
                    antigen = "A", feature = "F", source = "S", wavelength = "__none__", well = "A1", stringsAsFactors = FALSE)
  expect_equal(lin_prepare(raw, "control")$gate_class[1:2], c("unclassified", "unclassified"))
})

test_that("pcov beyond the stored grid is the cap", {
  cv <- mk_curves(); cv$grid <- cv$grid[cv$grid$log10_concentration <= 3, ]        # grid stops at x = 3
  resp <- data.frame(curve_id = 101L, method = "frequentist", y = .fwd("logistic4", c(2, 3.5), .P))
  r <- lin_backcalc(resp, cv$best, cv$params, cv$grid, cv$diag)
  expect_equal(r$pcov, c(30, 150))
})

test_that("Bayesian curves use the posterior median; a missing method expands to every method", {
  cv <- mk_curves()
  cv$params$q_med[cv$params$method == "bayesian" & cv$params$term == "c"] <- 2.2          # the median differs
  resp <- data.frame(curve_id = 101L, y = .fwd("logistic4", 2, .P))                         # no method column
  r <- lin_backcalc(resp, cv$best, cv$params, cv$grid, cv$diag)
  expect_equal(sort(r$method), c("bayesian", "frequentist"))
  expect_equal(r$x_log10[r$method == "frequentist"], 2, tolerance = 1e-9)
  expect_equal(r$x_log10[r$method == "bayesian"], 2.2, tolerance = 1e-9)                   # x shifts with c
  # an unknown curve is dropped rather than guessed
  expect_equal(nrow(lin_backcalc(data.frame(curve_id = 999L, y = 2), cv$best, cv$params, cv$grid, cv$diag)), 0L)
  expect_equal(nrow(lin_backcalc(resp[0, ], cv$best, cv$params, cv$grid, cv$diag)), 0L)
})

test_that("integer64 curve ids work", {
  skip_if_not_installed("bit64")
  cv <- mk_curves()
  cv$best$curve_id <- bit64::as.integer64(cv$best$curve_id); cv$params$curve_id <- bit64::as.integer64(cv$params$curve_id)
  cv$grid$curve_id <- bit64::as.integer64(cv$grid$curve_id); cv$diag$curve_id <- bit64::as.integer64(cv$diag$curve_id)
  resp <- data.frame(curve_id = bit64::as.integer64(101), method = "frequentist", y = .fwd("logistic4", 1, .P))
  expect_equal(lin_backcalc(resp, cv$best, cv$params, cv$grid, cv$diag)$x_log10, 1, tolerance = 1e-9)
})

# ---- controls and standards, end to end through the point table ----------------------------
mk_ctl <- function() {
  rows <- list()
  for (pl in c("P1", "P2")) for (w in c("A1", "A2")) for (nm in c("Pos Ctrl", "Low Ctrl"))
    rows[[length(rows) + 1L]] <- data.frame(
      curve_id = if (pl == "P1") 101L else 102L, well = if (nm == "Pos Ctrl") w else paste0("B", substr(w, 2, 2)),
      sampleid = nm, dilution = 100, mfi = 10^.fwd("logistic4", if (nm == "Pos Ctrl") 3 else 1, .P) * if (w == "A2") 1.02 else 1,
      plateid = pl, plate = pl, antigen = "AG1", feature = "IgG", source = "S1", wavelength = NA_character_, stringsAsFactors = FALSE)
  do.call(rbind, rows)
}

test_that("controls: log response, dilution applied, one set per control name", {
  cv <- mk_curves()
  sc <- lin_scales(data.frame(curve_id = c(101L, 101L, 102L, 102L), method = rep(c("frequentist", "bayesian"), 2),
                              dev_raw = 3, dev_log10 = 0, dev_ln = 1))
  raw <- lin_backcalc_controls(mk_ctl(), cv$best, cv$params, cv$grid, cv$diag, sc)
  expect_equal(nrow(raw), 16L)                                       # 8 wells x 2 methods
  pos <- raw[raw$sampleid == "Pos Ctrl" & raw$plate == "P1" & raw$well == "A1" & raw$method == "frequentist", ]
  expect_equal(pos$final_concentration, 10^3 * 100, tolerance = 1e-6)                # estimate x dilution of 100
  expect_equal(pos$pcov, 20 + 5 * 3, tolerance = 1e-9)
  expect_equal(pos$pcov_gate_class, "inDynamicRange")
  expect_true(all(raw$patientid == "__none__") && all(raw$timeperiod == "__none__"))
  p <- lin_prepare(raw, "control")
  expect_true(all(p$kind == "control")); expect_true(all(is.na(p$patientid)))
  expect_true(all(grepl("^AG1 \\| IgG \\| control:spec:(Pos|Low) Ctrl \\| -$", p$set_id)))
  s <- lin_sets(p)
  expect_equal(nrow(s), 2L)                                                          # Pos Ctrl and Low Ctrl
  expect_true(all(s$kind == "control")); expect_true(all(s$plate)); expect_true(all(s$method))
  expect_true(all(s$duplicate))                                                      # two wells per control on each plate, same dilution
  # the ordinary presets work on it
  k <- lin_preset_sel(p, "plate")
  pr <- lin_pairs(p, k$x, k$y, k$match, k$differ)
  expect_true(nrow(pr) > 0); expect_true(all(pr$pair_type == "plate"))
  expect_null(lin_backcalc_controls(NULL, cv$best, cv$params, cv$grid))
})

test_that("a control that cannot be matched to a curve with a best model is dropped", {
  cv <- mk_curves(); ctl <- mk_ctl(); ctl$curve_id <- 555L
  expect_null(lin_backcalc_controls(ctl, cv$best, cv$params, cv$grid, cv$diag))
})

test_that("standards: the level is the set, the estimate is not multiplied by a dilution", {
  cv <- mk_curves()
  std <- do.call(rbind, lapply(c(101L, 102L), function(id) do.call(rbind, lapply(c(1, 3, 9), function(dl) data.frame(
    curve_id = id, method = "frequentist", well = paste0("A", dl), dilution = dl, nominal = 1000 / dl,
    y_model = .fwd("logistic4", log10(1000 / dl), .P), plateid = if (id == 101L) "P1" else "P2", plate = if (id == 101L) "P1" else "P2",
    antigen = "AG1", feature = "IgG", source = "S1", wavelength = NA_character_, stringsAsFactors = FALSE)))))
  raw <- lin_backcalc_standards(std, cv$best, cv$params, cv$grid, cv$diag)
  expect_equal(nrow(raw), 6L)
  r9 <- raw[raw$dilution == "9" & raw$plate == "P1", ]
  expect_equal(r9$final_concentration, 1000 / 9, tolerance = 1e-6)                 # back-calculated = nominal here, no dilution factor
  expect_equal(r9$sampleid, "std 9")
  p <- lin_prepare(raw, "standard")
  expect_true(all(grepl("^AG1 \\| IgG \\| standard:spec:std (1|3|9) \\| -$", p$set_id)))
  s <- lin_sets(p)
  expect_equal(nrow(s), 3L); expect_true(all(s$plate)); expect_true(all(s$kind == "standard"))
  expect_null(lin_backcalc_standards(std[0, ], cv$best, cv$params, cv$grid, cv$diag))
})

test_that("sets of different types never share a set id", {
  cv <- mk_curves()
  smp <- data.frame(curve_id = "101", method = "frequentist", sampleid = "Pos Ctrl", patientid = "__none__", timeperiod = "__none__",
                    dilution = "100", final_concentration = 5, pcov = 10, pcov_pass = TRUE, pcov_gate_class = "inDynamicRange",
                    plateid = "P1", plate = "P1", antigen = "AG1", feature = "IgG", source = "S1", wavelength = "__none__", well = "A1")
  ctl <- lin_backcalc_controls(mk_ctl(), cv$best, cv$params, cv$grid, cv$diag, NULL)
  all <- rbind(lin_prepare(smp, "sample"), lin_prepare(ctl, "control"))
  s <- lin_sets(all)
  expect_equal(sort(unique(s$kind)), c("control", "sample"))
  expect_false(any(duplicated(s$set_id)))
  d <- lin_set_summary_by_antigen(s)
  expect_equal(names(d)[1:3], c("antigen", "kind", "sets"))                           # one row per antigen and type
  expect_equal(sort(d$kind), c("Controls", "Test samples"))
  expect_false("kind" %in% names(lin_set_summary_by_antigen(lin_sets(lin_prepare(smp, "sample")))))
})

# ---- the check against the worker ------------------------------------------------------------
test_that("the check is ~0 when the inverse is the worker's, and shows the Bayesian median gap", {
  cv <- mk_curves()
  smp <- data.frame(curve_id = 101L, method = "frequentist", dilution = "100",
                    final_concentration = 10^c(1, 2, 3) * 100, pcov = 20 + 5 * c(1, 2, 3))
  ck <- lin_backcalc_check(cv$best, cv$params, cv$grid, smp)
  f <- ck[ck$method == "frequentist", ]
  expect_lt(f$inverse_med_dlog10, 1e-6); expect_gt(f$n_grid, 100)
  expect_lt(f$pcov_med_diff, 1e-6); expect_equal(f$n_samples, 3L)
  cv$params$q_med[cv$params$method == "bayesian" & cv$params$term == "c"] <- 2.05
  b <- lin_backcalc_check(cv$best, cv$params, cv$grid, NULL); b <- b[b$method == "bayesian", ]
  expect_equal(b$inverse_med_dlog10, 0.05, tolerance = 1e-6)
  expect_equal(b$n_samples, 0L)
  txt <- lin_backcalc_check_text(ck)
  expect_match(txt, "frequentist: inverse vs stored grid"); expect_match(txt, "pcov vs stored test samples")
  expect_equal(lin_backcalc_check_text(NULL), "")
  expect_null(lin_backcalc_check(cv$best[0, ], cv$params, cv$grid))
  # a stored pcov that the grid does not reproduce shows up
  smp$pcov <- smp$pcov + 4
  expect_equal(lin_backcalc_check(cv$best, cv$params, cv$grid, smp)$pcov_med_diff[1], 4, tolerance = 1e-9)
})

# ---- the fetch, through a mocked query helper ----------------------------------------------------
test_that("a control stored under its well is labelled by type code and dilution", {
  # the name lives in source; the dilution (2500 on one plate, 5000 on another) is not part of it
  expect_equal(lin_control_label(c("A4", "G3", "B4", "x1", "A1"), c("A4", "G3", "B4", "A1", "A1"),
                                 c("QC1 (Low)", "Inhouse Ref", NA, "QC2", NA), c("C", "C", "C1", "C", "C"), c(2500, 1, 500, 5000, NA)),
               c("QC1 (Low)", "Inhouse Ref", "C1 1:500", "x1", "A1"))
  expect_equal(lin_control_label("A4", "A4"), "A4")                                  # nothing better is stored
  cv <- mk_curves(); ctl <- mk_ctl(); ctl$sampleid <- ctl$well; ctl$control_source <- ifelse(ctl$sampleid == "Pos Ctrl", "x", "QC1 (Low)")
  r <- lin_backcalc_controls(ctl, cv$best, cv$params, cv$grid, cv$diag, NULL)
  expect_equal(sort(unique(r$sampleid)), "QC1 (Low)")
  # the same name at two dilutions is one set
  ctl$dilution <- rep(c(2500, 5000), 4)
  s <- lin_sets(lin_prepare(lin_backcalc_controls(ctl, cv$best, cv$params, cv$grid, cv$diag, NULL), "control"))
  expect_equal(nrow(s), 1L); expect_true(s$dilution[1] || s$plate[1] || s$duplicate[1])
})

test_that("lin_fetch_extra asks for controls through the key without source, skips masked wells, and shapes the result", {
  cv <- mk_curves(); ctl <- mk_ctl()
  sqls <- character()
  summ <- data.frame(curve_id = c(101L, 101L, 102L, 102L), method = cv$cur$method, dev_raw = 3, dev_log10 = 0, dev_ln = 1)
  std <- data.frame(curve_id = 101L, method = "frequentist", well = "A1", dilution = 3, nominal = 333,
                    y_model = .fwd("logistic4", log10(333), .P), plateid = "P1", plate = "P1", antigen = "AG1", feature = "IgG",
                    source = "S1", wavelength = NA_character_, stringsAsFactors = FALSE)
  mock <- function(pool, sql, params = list()) {
    sql <- gsub("\\s+", " ", sql); sqls <<- c(sqls, sql)
    if (grepl("calib_param", sql)) cv$params
    else if (grepl("xmap_control", sql)) ctl
    else if (grepl("dev_raw", sql)) summ
    else if (grepl("y_model", sql)) std
    else if (grepl("calib_grid", sql)) cv$grid
    else if (grepl("calib_diagnostics", sql)) cv$diag
    else cv$best
  }
  old <- get(".calib_q", envir = globalenv()); assign(".calib_q", mock, envir = globalenv())
  on.exit(assign(".calib_q", old, envir = globalenv()), add = TRUE)
  ex <- lin_fetch_extra(NULL, 92, "Luminex", "IgG1_500", kinds = c("control", "standard"))
  expect_equal(nrow(ex$control), 16L); expect_equal(nrow(ex$standard), 1L)
  expect_true(is.data.frame(ex$check)); expect_equal(ex$scales_inferred, 0L)
  expect_equal(ex$control_stages$joined, 8L); expect_equal(ex$control_stages$with_conc, 16L)
  ctl$masked <- c(TRUE, rep(FALSE, 7))                                               # a masked well is dropped in R and counted
  ex2 <- lin_fetch_extra(NULL, 92, "Luminex", "IgG1_500", kinds = "control")
  expect_equal(ex2$control_stages$joined, 8L); expect_equal(ex2$control_stages$kept, 7L)
  expect_match(lin_control_stages_text(ex2$control_stages), "8 matched to a curve.*7 not masked")
  expect_equal(lin_control_stages_text(NULL), "")
  ctl$masked <- NULL
  cq <- sqls[grepl("xmap_control", sqls)]
  cq <- unique(cq[grepl("JOIN", cq)])                                                       # the join query, not the count
  expect_false(grepl("NOT c.masked", cq, fixed = TRUE))                               # masked wells are dropped in R, so they can be counted
  expect_false(grepl("cl.source", gsub(".*ON ", "", sub(" WHERE.*", "", cq))), info = "a control's source is not part of the curve key")
  expect_match(cq, "plateid", fixed = TRUE)
  expect_match(sqls[grepl("y_model", sqls)], "AND cs.included", fixed = TRUE)
  sqls <- character()
  lin_fetch_extra(NULL, 92, "Luminex", "IgG1_500", kinds = "control", include_masked = TRUE)
  expect_false(any(grepl("y_model", sqls)))                                          # standards were not asked for
  # nothing asked for, or no fits stored
  expect_null(lin_fetch_extra(NULL, 92, "L", "E", kinds = "sample")$control)
  mock2 <- function(pool, sql, params = list()) data.frame()
  assign(".calib_q", mock2, envir = globalenv())
  expect_null(lin_fetch_extra(NULL, 92, "L", "E", kinds = "control")$control)
})

# ---- the module ----------------------------------------------------------------------------------
if (!exists("linearityServer")) .bc_src("linearity_module.R")

test_that("module: load controls and standards with the samples, switch type, report the check", {
  cv <- mk_curves()
  ctl <- lin_backcalc_controls(mk_ctl(), cv$best, cv$params, cv$grid, cv$diag, NULL)
  smp <- data.frame(curve_id = "101", method = "frequentist", sampleid = "S1", patientid = "P01", timeperiod = "D0",
                    dilution = "100", final_concentration = c(100, 110), pcov = 10, pcov_pass = TRUE, pcov_gate_class = "inDynamicRange",
                    plateid = "P1", plate = "P1", antigen = "AG1", feature = "IgG", source = "S1", wavelength = "__none__", well = c("A1", "A2"))
  calls <- list()
  fx <- function(pool, project, study, experiment, kinds, samples) {
    calls[[length(calls) + 1L]] <<- list(kinds = kinds, samples = samples)
    list(control = if ("control" %in% kinds) ctl, standard = NULL, scales_inferred = 2L,
         check = lin_backcalc_check(cv$best, cv$params, cv$grid, samples))
  }
  scope <- shiny::reactive(list(project_id = 92, study = "Luminex", experiment = "IgG1_500"))
  shiny::testServer(linearityServer, args = list(pool = NULL, scope = scope, fetch = function(...) smp, fetch_extra = fx), {
    session$setInputs(kinds = c("sample", "control", "standard"), load = 1)
    expect_equal(sort(unique(pts_all()$kind)), c("control", "sample"))
    expect_equal(calls[[1]]$kinds, c("control", "standard"))
    expect_equal(nrow(calls[[1]]$samples), 2L)                                   # the samples are handed over for the pcov check
    expect_true(all(pts()$kind == "sample"))                                      # the first type until one is chosen
    session$setInputs(kind = "control")
    expect_true(all(pts()$kind == "control")); expect_equal(cur_kind(), "control")
    expect_true(all(grepl("control:", sets()$set_id)))
    expect_match(chk_txt(), "inverse vs stored grid")
    expect_equal(nrow(sets_all()), nrow(lin_sets(pts_all())))
    session$setInputs(kind = "bogus"); expect_equal(cur_kind(), "sample")         # a stale value falls back
    # a reload with samples only needs no extra fetch and clears the check
    n <- length(calls)
    session$setInputs(kinds = "sample", load = 2)
    expect_equal(length(calls), n); expect_equal(chk_txt(), "")
    expect_true(all(pts()$kind == "sample"))
  })
})

test_that("module: controls only, none found, says why", {
  fx <- function(...) list(control = NULL, standard = NULL, check = NULL, scales_inferred = 0L)
  scope <- shiny::reactive(list(project_id = 92, study = "L", experiment = "E"))
  shiny::testServer(linearityServer, args = list(pool = NULL, scope = scope, fetch = function(...) stop("not asked for"), fetch_extra = fx), {
    session$setInputs(kinds = "control", load = 1)
    expect_equal(nrow(pts_all()), 0L)
    html <- as.character(output$body$html)
    expect_match(html, "controls and standards need a stored best model")
  })
})

test_that("the report names the sample type and, for controls, how it was computed and checked", {
  cv <- mk_curves()
  ctl <- lin_prepare(lin_backcalc_controls(mk_ctl(), cv$best, cv$params, cv$grid, cv$diag, NULL), "control")
  k <- lin_preset_sel(ctl, "plate"); pr <- lin_pairs(ctl, k$x, k$y, k$match, k$differ)
  rg <- lin_regress(pr, scale = "log10", error = "y", weighting = "full", B = 0)
  rep <- lin_report_build(list(scope = list(project_id = 92, study = "L", experiment = "E"), preset = "Plate vs plate",
                               x = k$x, y = k$y, match = k$match, differ = k$differ, kind = "control", backcalc = "frequentist: inverse vs stored grid, median 0",
                               filters = list(antigen = "AG1"), sets = lin_sets(ctl), pairs = pr, reg = rg, sens = NULL, consensus = NULL, n_gate_inferred = 0L))
  txt <- lin_report_text(rep)
  expect_match(txt, "sample type: Controls", fixed = TRUE)
  expect_match(txt, "computed in the app", fixed = TRUE)
  expect_match(txt, "check against the worker: frequentist: inverse vs stored grid", fixed = TRUE)
  js <- as.character(lin_report_json(rep))
  expect_match(js, '"sample_type": "control"', fixed = TRUE)
  # test samples carry no such lines
  rep$kind <- "sample"; t2 <- lin_report_text(rep)
  expect_match(t2, "sample type: Test samples", fixed = TRUE); expect_false(grepl("computed in the app", t2, fixed = TRUE))
})

test_that("the help note for the new control exists and is linked from the module", {
  base <- c("src", ".")[file.exists(file.path(c("src", "."), "help", "qc", "linearity.kinds.md"))][1]
  expect_false(is.na(base))
  src <- paste(readLines(file.path(base, "linearity_module.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(src, 'help_icon("qc.linearity.kinds"', fixed = TRUE)
  md <- paste(readLines(file.path(base, "help", "qc", "linearity.kinds.md"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(md, "id: qc.linearity.kinds", fixed = TRUE)
})
