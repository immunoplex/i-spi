# Tests for linearity_stored.R: controls and standards in QC > Replicate Linearity, read
# from the estimates the compute job stores (calib_controls, calib_standards).
# No database (lin_fetch_extra is exercised through a mocked .calib_q).
library(testthat)
.bc_src <- function(f) for (p in c(file.path("src", f), f)) if (file.exists(p)) return(invisible(source(p)))
if (!exists(".tbl")) suppressMessages(.bc_src("calib_data_access.R"))
if (!exists("lin_prepare")) .bc_src("linearity_report.R")
if (!exists("lin_fetch_extra")) .bc_src("linearity_stored.R")

# Stored control rows as lin_fetch_extra reads them: two plates (one curve each), two
# methods, two controls in duplicate wells; one masked well.
mk_ctl <- function() {
  rows <- list()
  for (pl in c("P1", "P2")) for (me in c("frequentist", "bayesian")) for (w in c("A1", "A2")) for (nm in c("Pos Ctrl", "Low Ctrl"))
    rows[[length(rows) + 1L]] <- data.frame(
      curve_id = if (pl == "P1") 101L else 102L, method = me,
      well = if (nm == "Pos Ctrl") w else paste0("B", substr(w, 2, 2)), sampleid = nm, control_source = NA_character_,
      dilution = 100, included = TRUE,
      final_concentration = (if (nm == "Pos Ctrl") 1e5 else 1e3) * (if (w == "A2") 1.02 else 1) * (if (pl == "P2") 0.97 else 1),
      pcov = 12, pcov_pass = TRUE, pcov_gate_class = "inDynamicRange", censor_class = "observed",
      plateid = pl, plate = pl, antigen = "AG1", feature = "IgG", source = "S1", wavelength = NA_character_,
      stringsAsFactors = FALSE)
  do.call(rbind, rows)
}
mk_std <- function() do.call(rbind, lapply(c(101L, 102L), function(id) do.call(rbind, lapply(c(1, 3, 9), function(dl) data.frame(
  curve_id = id, method = "frequentist", well = paste0("A", dl), dilution = dl, nominal = 1000 / dl,
  predicted_log10_concentration = log10(1000 / dl) + 0.01, pcov = 8, pcov_pass = TRUE, pcov_gate_class = "inDynamicRange",
  censor_class = "observed", recovery_pct = 100 * 10^0.01,
  plateid = if (id == 101L) "P1" else "P2", plate = if (id == 101L) "P1" else "P2",
  antigen = "AG1", feature = "IgG", source = "S1", wavelength = NA_character_, stringsAsFactors = FALSE)))))

test_that("a control stored under its well is labelled by its name, type code and dilution", {
  expect_equal(lin_control_label(c("A4", "G3", "B4", "x1", "A1"), c("A4", "G3", "B4", "A1", "A1"),
                                 c("QC1 (Low)", "Inhouse Ref", NA, "QC2", NA), c("C", "C", "C1", "C", "C"), c(2500, 1, 500, 5000, NA)),
               c("QC1 (Low)", "Inhouse Ref", "C1 1:500", "x1", "A1"))
  expect_equal(lin_control_label("A4", "A4"), "A4")
})

test_that("controls: stored estimates pass through unchanged; one set per control name", {
  raw <- lin_stored_controls(mk_ctl())
  expect_equal(nrow(raw), 16L)
  expect_equal(range(raw$final_concentration), c(1e3 * 0.97, 1e5 * 1.02))       # as stored, already x dilution
  expect_true(all(raw$patientid == "__none__") && all(raw$dilution == "100"))
  p <- lin_prepare(raw, "control")
  expect_true(all(p$kind == "control")); expect_true(all(p$censor_class == "observed"))
  expect_true(all(grepl("^AG1 \\| IgG \\| control:spec:(Pos|Low) Ctrl \\| -$", p$set_id)))
  s <- lin_sets(p)
  expect_equal(nrow(s), 2L); expect_true(all(s$plate)); expect_true(all(s$method)); expect_true(all(s$duplicate))
  k <- lin_preset_sel(p, "plate"); pr <- lin_pairs(p, k$x, k$y, k$match, k$differ)
  expect_true(nrow(pr) > 0); expect_true(all(pr$pair_type == "plate"))
  expect_null(lin_stored_controls(NULL)); expect_null(lin_stored_controls(mk_ctl()[0, ]))
  # named by the stored control name when sampleid is only the well; two dilutions, one set
  ctl <- mk_ctl(); ctl$sampleid <- ctl$well; ctl$control_source <- "QC1 (Low)"; ctl$dilution <- rep(c(2500, 5000), 8)
  s2 <- lin_sets(lin_prepare(lin_stored_controls(ctl), "control"))
  expect_equal(nrow(s2), 1L)
})

test_that("standards: the estimate at the level (not x dilution), with nominal and recovery", {
  raw <- lin_stored_standards(mk_std())
  expect_equal(nrow(raw), 6L)
  r9 <- raw[raw$dilution == "9" & raw$plate == "P1", ]
  expect_equal(r9$final_concentration, 1000 / 9 * 10^0.01, tolerance = 1e-9)
  expect_equal(r9$nominal, 1000 / 9); expect_equal(r9$recovery_pct, 100 * 10^0.01)
  expect_equal(r9$sampleid, "std 9")
  p <- lin_prepare(raw, "standard")
  expect_true(all(grepl("^AG1 \\| IgG \\| standard:spec:std (1|3|9) \\| -$", p$set_id)))
  s <- lin_sets(p); expect_equal(nrow(s), 3L); expect_true(all(s$plate))
  # a point beyond the asymptote has no concentration and becomes "unclassified", not dropped
  st <- mk_std()[1, ]; st$predicted_log10_concentration <- NA; st$pcov <- NA; st$pcov_pass <- NA; st$pcov_gate_class <- NA
  q <- lin_prepare(lin_stored_standards(st), "standard")
  expect_true(is.na(q$conc)); expect_equal(q$gate_class, "unclassified")
})

test_that("sets of different types never share a set id", {
  smp <- data.frame(curve_id = "101", method = "frequentist", sampleid = "Pos Ctrl", patientid = "__none__", timeperiod = "__none__",
                    dilution = "100", final_concentration = 5, pcov = 10, pcov_pass = TRUE, pcov_gate_class = "inDynamicRange",
                    plateid = "P1", plate = "P1", antigen = "AG1", feature = "IgG", source = "S1", wavelength = "__none__", well = "A1")
  all <- rbind(lin_prepare(smp, "sample"), lin_prepare(lin_stored_controls(mk_ctl()), "control"))
  s <- lin_sets(all)
  expect_equal(sort(unique(s$kind)), c("control", "sample")); expect_false(any(duplicated(s$set_id)))
})

test_that("lin_fetch_extra reads stored rows, drops masked controls in R, counts coverage", {
  ctl <- mk_ctl(); std <- mk_std()
  std_unest <- std[1:3, ]; std_unest$curve_id <- 103L
  std_unest[, c("predicted_log10_concentration", "pcov", "censor_class")] <- NA            # fitted before 1.1, not backfilled
  sqls <- character()
  mock <- function(pool, sql, params = list()) {
    sql <- gsub("\\s+", " ", sql); sqls <<- c(sqls, sql)
    if (grepl("to_regclass", sql)) data.frame(ok = TRUE)
    else if (grepl("count\\(\\*\\)::int AS need", sql)) data.frame(need = 5L, have = 4L)
    else if (grepl("FROM madi_results.xmap_control", sql)) data.frame(n = 9L, n_masked = 1L, names = "Pos Ctrl; Low Ctrl")
    else if (grepl("calib_controls cc JOIN", sql)) ctl
    else if (grepl("calib_standards cs", sql)) rbind(std, std_unest)
    else data.frame()
  }
  old <- get(".calib_q", envir = globalenv()); assign(".calib_q", mock, envir = globalenv())
  on.exit(assign(".calib_q", old, envir = globalenv()), add = TRUE)
  ex <- lin_fetch_extra(NULL, 92, "Luminex", "IgG1_500", kinds = c("control", "standard"))
  expect_true(ex$stored)
  expect_equal(nrow(ex$control), 16L); expect_equal(nrow(ex$standard), 6L)        # unestimated curve left out
  expect_equal(ex$coverage$control, list(need = 5L, have = 4L))
  expect_equal(ex$coverage$standard, list(need = 3L, have = 2L))
  expect_match(lin_coverage_text(ex$coverage), "Controls: stored estimates for 4 of 5 fitted curve")
  expect_match(lin_coverage_text(ex$coverage), "Standards: stored estimates for 2 of 3")
  expect_match(lin_coverage_text(ex$coverage), "backfill")
  expect_equal(ex$control_stages$estimated, 16L); expect_equal(ex$control_stages$with_conc, 16L)
  ctl$included[1] <- FALSE                                                          # a masked well: stored, left out
  ex2 <- lin_fetch_extra(NULL, 92, "Luminex", "IgG1_500", kinds = "control")
  expect_equal(ex2$control_stages$estimated, 16L); expect_equal(ex2$control_stages$kept, 15L)
  expect_match(lin_control_stages_text(ex2$control_stages), "9 stored \\(1 masked\\) > 16 estimates stored.*15 not masked")
  expect_equal(nrow(lin_fetch_extra(NULL, 92, "L", "E", kinds = "control", include_masked = TRUE)$control), 16L)
  expect_match(sqls[grepl("calib_standards cs", sqls)][1], "AND cs.included", fixed = TRUE)
  expect_false(any(grepl("calib_param|calib_grid", sqls)))                          # nothing is recomputed
  # nothing asked for; a database without migration 011
  expect_null(lin_fetch_extra(NULL, 92, "L", "E", kinds = "sample")$control)
  assign(".calib_q", function(pool, sql, params = list()) if (grepl("to_regclass", sql)) data.frame(ok = FALSE) else stop("not asked"),
         envir = globalenv())
  ex3 <- lin_fetch_extra(NULL, 92, "L", "E", kinds = c("control", "standard"))
  expect_false(ex3$stored); expect_null(ex3$control); expect_null(ex3$standard)
  expect_equal(lin_coverage_text(NULL), ""); expect_equal(lin_control_stages_text(NULL), "")
})

# ---- the module ----------------------------------------------------------------------------------
if (!exists("linearityServer")) .bc_src("linearity_module.R")

test_that("module: load controls with the samples, switch type, report coverage", {
  ctl <- lin_stored_controls(mk_ctl())
  smp <- data.frame(curve_id = "101", method = "frequentist", sampleid = "S1", patientid = "P01", timeperiod = "D0",
                    dilution = "100", final_concentration = c(100, 110), pcov = 10, pcov_pass = TRUE, pcov_gate_class = "inDynamicRange",
                    plateid = "P1", plate = "P1", antigen = "AG1", feature = "IgG", source = "S1", wavelength = "__none__", well = c("A1", "A2"))
  calls <- list()
  fx <- function(pool, project, study, experiment, kinds, samples) {
    calls[[length(calls) + 1L]] <<- list(kinds = kinds)
    list(control = if ("control" %in% kinds) ctl, standard = NULL, stored = TRUE,
         coverage = list(control = list(need = 2L, have = 2L)), control_stages = NULL)
  }
  scope <- shiny::reactive(list(project_id = 92, study = "Luminex", experiment = "IgG1_500"))
  shiny::testServer(linearityServer, args = list(pool = NULL, scope = scope, fetch = function(...) smp, fetch_extra = fx), {
    session$setInputs(kinds = c("sample", "control", "standard"), load = 1)
    expect_equal(sort(unique(pts_all()$kind)), c("control", "sample"))
    expect_equal(calls[[1]]$kinds, c("control", "standard"))
    session$setInputs(kind = "control")
    expect_true(all(pts()$kind == "control")); expect_equal(cur_kind(), "control")
    expect_match(chk_txt(), "Controls: stored estimates for 2 of 2")
    n <- length(calls)
    session$setInputs(kinds = "sample", load = 2)
    expect_equal(length(calls), n); expect_equal(chk_txt(), "")
  })
})

test_that("module: controls only, none found, says why", {
  fx <- function(...) list(control = NULL, standard = NULL, stored = TRUE, coverage = list(), control_stages = NULL)
  scope <- shiny::reactive(list(project_id = 92, study = "L", experiment = "E"))
  shiny::testServer(linearityServer, args = list(pool = NULL, scope = scope, fetch = function(...) stop("not asked for"), fetch_extra = fx), {
    session$setInputs(kinds = "control", load = 1)
    expect_equal(nrow(pts_all()), 0L)
    expect_match(as.character(output$body$html), "estimates stored by i-spi-compute 1.1")
  })
})

test_that("the report says controls are stored by the compute job, with the coverage", {
  ctl <- lin_prepare(lin_stored_controls(mk_ctl()), "control")
  k <- lin_preset_sel(ctl, "plate"); pr <- lin_pairs(ctl, k$x, k$y, k$match, k$differ)
  rg <- lin_regress(pr, scale = "log10", error = "y", weighting = "full", B = 0)
  rep <- lin_report_build(list(scope = list(project_id = 92, study = "L", experiment = "E"), preset = "Plate vs plate",
                               x = k$x, y = k$y, match = k$match, differ = k$differ, kind = "control",
                               backcalc = "Controls: stored estimates for 2 of 2 fitted curve(s)",
                               filters = list(antigen = "AG1"), sets = lin_sets(ctl), pairs = pr, reg = rg, sens = NULL, consensus = NULL, n_gate_inferred = 0L))
  txt <- lin_report_text(rep)
  expect_match(txt, "sample type: Controls", fixed = TRUE)
  expect_match(txt, "stored by the compute job", fixed = TRUE)
  expect_match(txt, "stored estimates: Controls: stored estimates for 2 of 2", fixed = TRUE)
  expect_false(grepl("computed in the app", txt, fixed = TRUE))
  expect_match(as.character(lin_report_json(rep)), '"stored_estimates"', fixed = TRUE)
  rep$kind <- "sample"; expect_false(grepl("stored by the compute job", lin_report_text(rep), fixed = TRUE))
})

test_that("the help note for the sample types exists and is linked from the module", {
  base <- c("src", ".")[file.exists(file.path(c("src", "."), "help", "qc", "linearity.kinds.md"))][1]
  expect_false(is.na(base))
  src <- paste(readLines(file.path(base, "linearity_module.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(src, 'help_icon("qc.linearity.kinds"', fixed = TRUE)
  md <- paste(readLines(file.path(base, "help", "qc", "linearity.kinds.md"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(md, "Stored by the compute job", fixed = TRUE)
  expect_false(grepl("Computed here", md, fixed = TRUE))
})
