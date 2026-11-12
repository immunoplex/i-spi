# Tests for linearity_report.R (QC > Replicate Linearity). Pure functions over
# synthetic data; no database.
library(testthat)
.lin_src <- function(f) for (p in c(file.path("src", f), f)) if (file.exists(p)) return(invisible(source(p)))
if (!exists("lin_prepare")) .lin_src("linearity_report.R")

# One antigen, two plates, two dilutions, two methods. Each patient-timeperiod
# is run: duplicated at 100 on P1 (wells A1, A2), at 400 on P1 (B1), and at 100
# on P2 (A1). True concentration t; the 400 estimate is biased as t^0.9 * 1.2.
mk_raw <- function(n = 30, seed = 1) {
  set.seed(seed)
  rows <- list()
  for (i in seq_len(n)) {
    t <- 10^runif(1, 1, 4)
    pid <- sprintf("P%02d", i)
    spec <- list(
      list(plate = "P1", well = "A1", dil = "100", c = t * exp(rnorm(1, 0, .05))),
      list(plate = "P1", well = "A2", dil = "100", c = t * exp(rnorm(1, 0, .05))),
      list(plate = "P1", well = "B1", dil = "400", c = 1.2 * t^0.9 * exp(rnorm(1, 0, .05))),
      list(plate = "P2", well = "A1", dil = "100", c = t * exp(rnorm(1, 0, .05))))
    for (s in spec) for (m in c("bayesian", "frequentist"))
      rows[[length(rows) + 1L]] <- data.frame(
        curve_id = paste0(s$plate, "-", m), method = m, sampleid = paste0("S", i),
        patientid = pid, timeperiod = "D0", dilution = s$dil,
        final_concentration = s$c * if (m == "frequentist") 1.05 else 1,
        pcov = runif(1, 5, 40), pcov_pass = TRUE,
        pcov_gate_class = sample(c("inDynamicRange", "belowLLOQ", "aboveULOQ"), 1, prob = c(.8, .1, .1)),
        plateid = s$plate, plate = s$plate, antigen = "AG1", feature = "IgG",
        source = "V2", wavelength = "__none__", well = s$well, stringsAsFactors = FALSE)
  }
  do.call(rbind, rows)
}

test_that("lin_prepare normalises ids, decodes sentinels and infers a missing gate class", {
  raw <- mk_raw(3)
  raw$patientid[1] <- "__none__"
  raw$pcov_gate_class[2] <- NA; raw$pcov_pass[2] <- TRUE
  raw$pcov_gate_class[3] <- NA; raw$pcov_pass[3] <- FALSE
  p <- lin_prepare(raw)
  expect_true(is.na(p$patientid[1]))
  expect_true(is.na(p$wavelength[1]))
  expect_equal(p$gate_class[2], "inDynamicRange"); expect_true(p$gate_inferred[2])
  expect_equal(p$gate_class[3], "unclassified")
  expect_true(startsWith(p$set_id[1], "AG1 | IgG | spec:S1"))   # no patient -> specimen id
  expect_equal(nrow(lin_prepare(NULL)), 0L)
})

test_that("integer64 curve ids are kept as text", {
  skip_if_not_installed("bit64")
  raw <- mk_raw(2); raw$curve_id <- bit64::as.integer64(seq_len(nrow(raw)) + 1e12)
  p <- lin_prepare(raw)
  expect_true(all(grepl("^[0-9]+$", p$curve_id)))
})

test_that("lin_sets flags each kind of replicate set", {
  pts <- lin_prepare(mk_raw(5))
  s <- lin_sets(pts)
  expect_equal(nrow(s), 5L)
  expect_true(all(s$duplicate)); expect_true(all(s$dilution)); expect_true(all(s$plate))
  expect_true(all(s$platedil)); expect_true(all(s$method)); expect_false(any(s$source))
  # a sample run once is in none of them
  one <- pts[pts$patientid == "P01" & pts$plate == "P1" & pts$well == "A1" & pts$method == "bayesian", ]
  s1 <- lin_sets(one)
  expect_false(any(unlist(s1[, c("duplicate", "dilution", "plate", "platedil", "method", "source")])))
  sm <- lin_set_summary(s); expect_equal(sm$sets[sm$type == "duplicate"], 5L)
  expect_equal(lin_set_summary_by_antigen(s)$sets, 5L)
})

test_that("duplicate preset pairs wells on the same plate and dilution once each", {
  pts <- lin_prepare(mk_raw(5))
  pr <- lin_pairs(pts, x = list(method = "bayesian"), y = list(method = "bayesian"),
                  match = LIN_PRESETS$duplicate$match)
  expect_equal(nrow(pr), 5L)                      # one A1/A2 pair per sample
  expect_true(all(pr$pair_type == "duplicate"))
  expect_true(all(pr$x_plate == pr$y_plate & pr$x_dilution == pr$y_dilution))
  expect_true(all(pr$x_well != pr$y_well))
})

test_that("dilution, plate and method comparisons select the right pairs", {
  pts <- lin_prepare(mk_raw(5))
  d <- lin_pairs(pts, x = list(dilution = "100", method = "bayesian"), y = list(dilution = "400", method = "bayesian"),
                 match = c("plate", "method"))
  expect_equal(nrow(d), 10L)                      # 2 wells at 100 x 1 well at 400, per sample
  expect_true(all(d$pair_type == "dilution"))
  p <- lin_pairs(pts, x = list(plate = "P1", dilution = "100", method = "bayesian"),
                 y = list(plate = "P2", dilution = "100", method = "bayesian"), match = c("dilution", "method"))
  expect_equal(nrow(p), 10L); expect_true(all(p$pair_type == "plate"))
  pd <- lin_pairs(pts, x = list(plate = "P1", dilution = "400", method = "bayesian"),
                  y = list(plate = "P2", dilution = "100", method = "bayesian"), match = "method")
  expect_equal(nrow(pd), 5L); expect_true(all(pd$pair_type == "plate+dilution"))
  m <- lin_pairs(pts, x = list(method = "frequentist"), y = list(method = "bayesian"), match = c("well", "source"))
  expect_equal(nrow(m), 20L)                      # 4 wells x 5 samples
  expect_true(all(m$pair_type == "method"))
  expect_equal(nrow(lin_pairs(pts, x = list(dilution = "999"), y = list())), 0L)
})

test_that("matching treats missing values as equal and an empty selection as any", {
  pts <- lin_prepare(mk_raw(3))
  pr <- lin_pairs(pts, x = list(dilution = "100"), y = list(dilution = "400"), match = "wavelength")
  expect_true(nrow(pr) > 0)                       # wavelength is NA on both sides
})

test_that("lin_regress recovers the slope and intercept on the log scale", {
  pts <- lin_prepare(mk_raw(60, seed = 3))
  pr <- lin_pairs(pts, x = list(dilution = "100", plate = "P1", method = "bayesian"),
                  y = list(dilution = "400", method = "bayesian"), match = c("plate", "method"))
  reg <- lin_regress(pr, scale = "log10")
  expect_true(reg$ok)
  expect_equal(reg$fit$b, 0.9, tolerance = 0.05)
  expect_equal(reg$fit$a, log10(1.2), tolerance = 0.1)
  expect_true(reg$fit$b_lo < reg$fit$b && reg$fit$b < reg$fit$b_hi)
  expect_true(reg$fit$p_slope1 < 0.05)
  expect_equal(nrow(reg$band), 80L)
  expect_true(all(reg$band$lwr <= reg$band$fit & reg$band$fit <= reg$band$upr))
  expect_match(lin_equation(reg$fit, "log10"), "^log10\\(y\\) = ")
})

test_that("the 1:1 transform makes the refit slope 1 and intercept 0", {
  pts <- lin_prepare(mk_raw(60, seed = 4))
  pr <- lin_pairs(pts, x = list(dilution = "100", plate = "P1", method = "bayesian"),
                  y = list(dilution = "400", method = "bayesian"), match = c("plate", "method"))
  for (sc in c("log10", "natural")) {
    reg <- lin_regress(pr, scale = sc, transform = TRUE)
    expect_equal(reg$refit$b, 1, tolerance = 1e-8, info = sc)
    expect_equal(reg$refit$a, 0, tolerance = 1e-6, info = sc)
    expect_equal(reg$refit$r2, reg$fit$r2, tolerance = 1e-8)
    expect_identical(reg$fit_plot, reg$refit)
    expect_length(reg$y_plot, nrow(pr))
  }
  expect_equal(lin_transform_y(c(10, 100), 0, 1, "log10"), c(10, 100))
  expect_equal(lin_transform_y(c(1, -1), 0, 1, "log10")[2], NA_real_)
})

test_that("the pcov gate limits which pairs contribute, per axis", {
  pts <- lin_prepare(mk_raw(60, seed = 5))
  pr <- lin_pairs(pts, x = list(dilution = "100", plate = "P1", method = "bayesian"),
                  y = list(dilution = "400", method = "bayesian"), match = c("plate", "method"))
  all_ <- lin_regress(pr)
  gx <- lin_regress(pr, x_pcov_max = 15)
  gxy <- lin_regress(pr, x_pcov_max = 15, y_pcov_max = 15)
  expect_true(gx$n < all_$n); expect_true(gxy$n <= gx$n)
  expect_equal(gx$n, sum(pr$x_pcov <= 15 & is.finite(pr$x_conc) & is.finite(pr$y_conc)))
  expect_equal(gx$n_gated, all_$n - gx$n)
  pr$x_pcov[1] <- NA
  expect_false(lin_regress(pr, x_pcov_max = 1000)$inc[1])   # unknown pcov fails an active gate
  tiny <- lin_regress(pr, x_pcov_max = 0.01)
  expect_false(tiny$ok); expect_match(tiny$reason, "Only 0")
})

test_that("non-positive concentrations are dropped on the log scale only", {
  pts <- lin_prepare(mk_raw(10, seed = 6))
  pr <- lin_pairs(pts, x = list(dilution = "100", plate = "P1", method = "bayesian"),
                  y = list(dilution = "400", method = "bayesian"), match = c("plate", "method"))
  pr$y_conc[1] <- 0
  expect_equal(lin_regress(pr, scale = "log10")$n_invalid, 1L)
  expect_equal(lin_regress(pr, scale = "natural")$n_invalid, 0L)
  expect_false(lin_regress(pr[0, ])$ok)
})

test_that("assessment reports proportional bias and clean agreement correctly", {
  pts <- lin_prepare(mk_raw(60, seed = 7))
  pr <- lin_pairs(pts, x = list(dilution = "100", plate = "P1", method = "bayesian"),
                  y = list(dilution = "400", method = "bayesian"), match = c("plate", "method"))
  f <- lin_assess(lin_regress(pr))
  codes <- vapply(f, function(z) z$code, "")
  expect_true("slope-not-1" %in% codes)
  expect_equal(lin_severity(f), "warn")
  dup <- lin_pairs(pts, x = list(method = "bayesian"), y = list(method = "bayesian"), match = LIN_PRESETS$duplicate$match)
  fd <- lin_assess(lin_regress(dup))
  expect_false("slope-not-1" %in% vapply(fd, function(z) z$code, ""))
  expect_equal(lin_assess(list(ok = FALSE, reason = "x"))[[1]]$code, "no-fit")
})

test_that("report text and JSON carry scope, set definition, regression and transform", {
  pts <- lin_prepare(mk_raw(40, seed = 8)); sets <- lin_sets(pts)
  xs <- list(dilution = "100", plate = "P1", method = "bayesian"); ys <- list(dilution = "400", method = "bayesian")
  pr <- lin_pairs(pts, x = xs, y = ys, match = c("plate", "method"))
  reg <- lin_regress(pr, transform = TRUE, x_pcov_max = 30, y_pcov_max = 30)
  rep <- lin_report_build(list(scope = list(project_id = 92, study = "Luminex", experiment = "IgG1_500"),
                               preset = "Dilution vs dilution", x = xs, y = ys, match = c("plate", "method"),
                               filters = list(antigen = "AG1"), sets = sets, pairs = pr, reg = reg))
  txt <- lin_report_text(rep)
  for (pat in c("project_id: 92", "study: Luminex", "experiment: IgG1_500", "dilution = 100", "must match on: plate, method",
                "equation: log10\\(y\\) =", "pcov gate: x <= 30", "## Transformation", "applied: map y onto x",
                "y' = 10\\^\\(\\(log10\\(y\\) - a\\) / b\\)", "parameters from the primary fit: a =", "check: refit", "Findings",
                "Lin's concordance", "Bland-Altman bias", "limits of agreement"))
    expect_match(txt, pat)
  js <- jsonlite::fromJSON(as.character(lin_report_json(rep)))
  expect_equal(js$scope$project_id, 92)
  expect_true(js$transform$applied); expect_equal(js$regression$scale, "log10"); expect_equal(js$regression$error_model, "y")
  expect_true(is.numeric(js$regression$fit$b))
  # no-fit and non-transform reports still render
  rep2 <- lin_report_build(list(scope = list(), x = list(), y = list(), match = character(), filters = list(),
                                sets = sets, pairs = pr[0, ], reg = lin_regress(pr[0, ])))
  expect_match(lin_report_text(rep2), "no fit")
  expect_match(lin_report_text(lin_report_build(list(scope = list(), x = xs, y = ys, match = "plate", filters = list(),
                                sets = sets, pairs = pr, reg = lin_regress(pr)))), "applied: no")
})

test_that("a large experiment's sets and pairs are computed quickly", {
  big <- lin_prepare(mk_raw(1500, seed = 9))
  t0 <- Sys.time()
  s <- lin_sets(big)
  pr <- lin_pairs(big, x = list(dilution = "100", method = "bayesian"), y = list(dilution = "400", method = "bayesian"), match = c("plate", "method"))
  expect_true(as.numeric(difftime(Sys.time(), t0, units = "secs")) < 15)
  expect_equal(nrow(s), 1500L); expect_equal(nrow(pr), 3000L)
})

# ---- presets, differ, plot, module ---------------------------------------------
if (!exists("linearityServer")) .lin_src("linearity_module.R")

test_that("lin_preset_sel picks the varied dimension's two most common values", {
  pts <- lin_prepare(mk_raw(6))
  s <- lin_preset_sel(pts, "dilution")
  expect_equal(s$x$dilution, "100"); expect_equal(s$y$dilution, "400")   # lower dilution on x
  expect_equal(s$differ, "dilution")
  m <- lin_preset_sel(pts, "method")
  expect_equal(m$x$method, "frequentist"); expect_equal(m$y$method, "bayesian")
  expect_equal(lin_preset_sel(pts, "duplicate")$x, list())
  expect_equal(lin_preset_sel(pts, "custom")$match, character())
})

test_that("every preset yields pairs of its own type", {
  pts <- lin_prepare(mk_raw(6))
  want <- c(duplicate = "duplicate", dilution = "dilution", plate = "plate", platedil = "plate+dilution", method = "method")
  for (k in names(want)) {
    s <- lin_preset_sel(pts, k)
    pr <- lin_pairs(pts, s$x, s$y, s$match, s$differ)
    expect_true(nrow(pr) > 0, info = k)
    expect_true(all(pr$pair_type == want[[k]]), info = k)
  }
})

test_that("'differ' removes same-plate pairs that 'match' alone would keep", {
  pts <- lin_prepare(mk_raw(4))
  base <- lin_pairs(pts, list(dilution = "100", method = "bayesian"), list(dilution = "100", method = "bayesian"), match = "method")
  diff <- lin_pairs(pts, list(dilution = "100", method = "bayesian"), list(dilution = "100", method = "bayesian"),
                    match = "method", differ = "plate")
  expect_true(nrow(diff) < nrow(base)); expect_true(all(diff$x_plate != diff$y_plate))
})

test_that("point size falls as pcov rises and missing pcov is the smallest", {
  s <- lin_point_size(c(0, 20, 100, NA), c(0, 20, 100, 5))
  expect_equal(s[1], 16); expect_true(all(diff(s[1:3]) < 0)); expect_equal(s[3], 5); expect_equal(s[4], 5)
})

test_that("the plot has one trace per gate combination, a 1:1 line, and hover with every field", {
  skip_if_not_installed("plotly")
  pts <- lin_prepare(mk_raw(40, seed = 2))
  pr <- lin_pairs(pts, list(dilution = "100", plate = "P1", method = "bayesian"), list(dilution = "400", method = "bayesian"), match = c("plate", "method"))
  reg <- lin_regress(pr, transform = TRUE)
  p <- plotly::plotly_build(lin_make_plot(pr, reg))
  nm <- vapply(p$x$data, function(t) .lin_or(t$name, ""), "")
  expect_true("1:1" %in% nm); expect_true("regression" %in% nm)
  ncomb <- nrow(unique(pr[, c("x_gate_class", "y_gate_class")]))
  expect_equal(sum(grepl("^x .* / y ", nm)), ncomb)
  hv <- p$x$data[[which(grepl("^x .* / y ", nm))[1]]]$text[1]
  for (f in c("plate ", "well ", "patientid ", "timeperiod ", "specimen_id ", "sample_dilution ", "wavelength ",
              "method ", "source ", "pcov ", "After transform"))
    expect_match(hv, f, fixed = TRUE)
  expect_false(any(grepl("^x .* / y ", vapply(plotly::plotly_build(lin_make_plot(pr, reg, color_by_gate = FALSE))$x$data, function(t) .lin_or(t$name, ""), ""))))
  expect_no_error(plotly::plotly_build(lin_make_plot(pr[0, ], lin_regress(pr[0, ]))))
})

test_that("module: load, choose a preset, regress, report", {
  scope <- shiny::reactive(list(project_id = 92, study = "Luminex", experiment = "IgG1_500"))
  shiny::testServer(linearityServer, args = list(pool = NULL, scope = scope, fetch = function(...) mk_raw(30, seed = 11)), {
    expect_null(pts())
    session$setInputs(load = 1)
    expect_equal(nrow(pts()), 30L * 8L)
    session$setInputs(antigen = "AG1", feature = "(all)", timeperiod = NULL, patient = "",
                      x_method = "bayesian", y_method = "bayesian", x_dilution = "100", y_dilution = "400",
                      x_plate = character(), y_plate = character(), x_source = character(), y_source = character(),
                      x_wavelength = character(), y_wavelength = character(),
                      match = c("plate", "method"), differ = "dilution",
                      scale = "log10", error = "y", weighting = "gate", transform = "y", boot = 0, x_pcov_max = 25, y_pcov_max = 25,
                      level = 0.95, preset = "dilution")
    expect_equal(nrow(pairs()), 60L)
    rg <- reg()
    expect_true(rg$ok); expect_true(rg$n < 60L); expect_equal(rg$refit$b, 1, tolerance = 1e-8)
    txt <- text()
    expect_match(txt, "project_id: 92"); expect_match(txt, "experiment: IgG1_500")
    expect_match(txt, "x axis points: method = bayesian; dilution = 100", fixed = TRUE); expect_match(txt, "applied: map y onto x")
    # a new scope clears the loaded points
  })
})

test_that("module: a failed fetch notifies and leaves nothing loaded; an empty experiment is handled", {
  scope <- shiny::reactive(list(project_id = 1, study = "s", experiment = "e"))
  shiny::testServer(linearityServer, args = list(pool = NULL, scope = scope, fetch = function(...) stop("db down")), {
    session$setInputs(load = 1); expect_null(pts())
  })
  shiny::testServer(linearityServer, args = list(pool = NULL, scope = scope, fetch = function(...) mk_raw(0)[0, ]), {
    session$setInputs(load = 1); expect_equal(nrow(pts()), 0L)
  })
})

# ---- error-in-variables fitting, agreement, harmonization, consensus ----------------
# Pairs built directly: latent log10 concentration z, x = z + ex, y = a0 + b0 z + ey.
mk_pairs <- function(n = 300, a0 = 0, b0 = 1, sdx = .5, sdy = .5, seed = 1, het = FALSE) {
  set.seed(seed)
  z <- runif(n, 0.5, 4)
  sx <- if (het) runif(n, .05, .5) else rep(sdx, n); sy <- if (het) runif(n, .05, .5) else rep(sdy, n)
  x <- z + rnorm(n, 0, sx); y <- a0 + b0 * z + rnorm(n, 0, sy)
  data.frame(set_id = paste0("s", seq_len(n)), antigen = "AG", feature = "IgG", patientid = paste0("p", seq_len(n)),
             timeperiod = "D0", x_pt_id = paste0("x", seq_len(n)), y_pt_id = paste0("y", seq_len(n)),
             x_conc = 10^x, y_conc = 10^y, x_pcov = sx * 100 * log(10), y_pcov = sy * 100 * log(10),
             x_gate_class = "inDynamicRange", y_gate_class = "inDynamicRange", pair_type = "dilution",
             stringsAsFactors = FALSE)
}
swap_pairs <- function(p) {
  q <- p; for (nm in c("pt_id", "conc", "pcov", "gate_class")) { q[[paste0("x_", nm)]] <- p[[paste0("y_", nm)]]; q[[paste0("y_", nm)]] <- p[[paste0("x_", nm)]] }
  q
}

test_that("lin_sd converts pcov to an SD on each scale", {
  expect_equal(lin_sd(100, 100 * log(10) * 0.1, "log10"), 0.1)
  expect_equal(lin_sd(200, 10, "natural"), 20)
  expect_equal(lin_sd(1, 0, "log10"), LIN_THRESH$pcov_floor / 100 / log(10))   # floor
})

test_that("Deming removes the attenuation that ordinary regression shows", {
  p <- mk_pairs(600, a0 = 0.2, b0 = 1)
  ols <- lin_regress(p, error = "y", B = 0)
  dem <- lin_regress(p, error = "both", B = 150)
  expect_true(ols$fit$b < 0.9)                         # attenuated towards 0
  expect_equal(dem$fit$b, 1, tolerance = 0.06)
  expect_equal(dem$delta, 1, tolerance = 0.01)         # equal pcov -> orthogonal regression
  expect_match(dem$delta_source, "pcov")
  expect_true(dem$fit$b_lo < dem$fit$b && dem$fit$b < dem$fit$b_hi)
  expect_match(dem$fit$ci_method, "bootstrap")
  expect_true(dem$fit$p_slope1 > 0.05)
  expect_equal(ols$fit$b, ols$stats$pearson, tolerance = 0.05)   # the OLS slope is about r under noise alone
})

test_that("a user-set delta is used, and delta follows the pcov ratio", {
  p <- mk_pairs(300, sdx = .3, sdy = .6)
  expect_equal(lin_regress(p, error = "both", B = 0)$delta, 4, tolerance = 0.02)
  expect_equal(lin_regress(p, error = "both", delta = 2, B = 0)$delta, 2)
  expect_match(lin_regress(p, error = "both", delta = 2, B = 0)$delta_source, "user")
})

test_that("swapping the axes inverts the Deming line exactly", {
  p <- mk_pairs(400, a0 = 0.3, b0 = 0.85, seed = 4)
  f1 <- lin_regress(p, error = "both", B = 0)$fit
  f2 <- lin_regress(swap_pairs(p), error = "both", B = 0)$fit
  expect_equal(f2$b, 1 / f1$b, tolerance = 1e-9)
  expect_equal(f2$a, -f1$a / f1$b, tolerance = 1e-9)
})

test_that("York weights by each point's own SD and recovers the line", {
  p <- mk_pairs(800, a0 = 0.1, b0 = 0.9, seed = 5, het = TRUE)
  w <- lin_regress(p, error = "both", weighting = "weights", B = 100)
  expect_equal(w$fit$b, 0.9, tolerance = 0.05)
  expect_equal(w$fit$a, 0.1, tolerance = 0.1)
  expect_true(is.na(w$delta))                          # delta is not used by York
  expect_true(lin_regress(p, error = "y", weighting = "weights", B = 0)$ok)
  p$x_pcov[1:10] <- NA
  expect_equal(lin_regress(p, error = "both", weighting = "weights", B = 0)$n_nopcov, 10L)
})

test_that("CCC and Bland-Altman follow their definitions", {
  u <- c(1, 2, 3, 4, 5); expect_equal(.lin_ccc(u, u), 1)
  v <- u + 1; man <- 2 * mean((u - mean(u)) * (v - mean(v))) / (mean((u - mean(u))^2) + mean((v - mean(v))^2) + 1)
  expect_equal(.lin_ccc(u, v), man); expect_true(.lin_ccc(u, v) < 1)
  ag <- .lin_agree(u, v)
  expect_equal(ag$bias, 1); expect_equal(ag$sd_diff, 0); expect_equal(ag$pearson, 1)
  p <- mk_pairs(500, a0 = 0.15, b0 = 1, sdx = .2, sdy = .2, seed = 6)
  r <- lin_regress(p, error = "both", B = 150)
  expect_equal(r$stats$bias, 0.15, tolerance = 0.1)
  expect_equal(r$stats$loa_hi, r$stats$bias + 1.96 * r$stats$sd_diff)
  expect_true(r$stats$ccc_lo < r$stats$ccc && r$stats$ccc < r$stats$ccc_hi)
  expect_true(r$stats$ccc < r$stats$pearson)           # a shift lowers CCC, not r
  expect_equal(lin_ccc_label(0.999), "almost perfect"); expect_equal(lin_ccc_label(0.8), "poor")
})

test_that("map-y transform makes the refit 1:1 for every error model", {
  p <- mk_pairs(300, a0 = 0.2, b0 = 0.8, seed = 7, het = TRUE)
  for (cfg in list(list("y", "full"), list("both", "full"), list("both", "weights"), list("y", "weights"))) {
    r <- lin_regress(p, error = cfg[[1]], weighting = cfg[[2]], transform = "y", B = 0)
    expect_equal(r$refit$b, 1, tolerance = 1e-6, info = paste(cfg, collapse = "/"))
    expect_equal(r$refit$a, 0, tolerance = 1e-6, info = paste(cfg, collapse = "/"))
  }
})

test_that("harmonization puts both sets on the latent scale and is symmetric", {
  p <- mk_pairs(300, a0 = 0.3, b0 = 0.8, seed = 8)
  r <- lin_regress(p, error = "both", transform = "harmonize", B = 0)
  expect_equal(r$refit$b, 1, tolerance = 1e-6); expect_equal(r$refit$a, 0, tolerance = 1e-6)
  expect_equal(r$tf$beta1 * r$tf$beta2, 1); expect_equal(r$tf$alpha1 + r$tf$alpha2, 0)
  expect_equal(r$tf$beta2^2, r$fit$b)
  # a point exactly on the fitted line gets the same latent value from either axis
  h <- .lin_harm(data.frame(x_conc = 10^2, y_conc = 10^(r$fit$a + r$fit$b * 2), x_pcov = 10, y_pcov = 10), "log10", r$fit$a, r$fit$b)
  expect_equal(h$zx, h$zy)
  # swapping the axes gives the same consensus per sample
  c1 <- lin_consensus(p, lin_regress(p, error = "both", B = 0))
  c2 <- lin_consensus(swap_pairs(p), lin_regress(swap_pairs(p), error = "both", B = 0))
  expect_equal(c1$consensus[order(c1$set_id)], c2$consensus[order(c2$set_id)], tolerance = 1e-8)
  # only valid for errors-in-variables fits
  ols <- lin_regress(p, error = "y", transform = "harmonize", B = 0)
  expect_equal(ols$transform, "none"); expect_match(ols$transform_note, "both")
})

test_that("consensus is the inverse-variance mean, one per sample, with a heterogeneity measure", {
  p <- mk_pairs(200, a0 = 0.2, b0 = 0.9, seed = 9, het = TRUE)
  r <- lin_regress(p, error = "both", B = 0)
  cs <- lin_consensus(p, r)
  expect_equal(nrow(cs), 200L); expect_true(all(cs$n_points == 2L))
  h <- .lin_harm(p, "log10", r$fit$a, r$fit$b)
  i <- 7; w <- c(1 / h$vx[i], 1 / h$vy[i]); zz <- c(h$zx[i], h$zy[i])
  expect_equal(cs$z[cs$set_id == p$set_id[i]], sum(w * zz) / sum(w), tolerance = 1e-10)
  expect_equal(cs$se_z[cs$set_id == p$set_id[i]], 1 / sqrt(sum(w)), tolerance = 1e-10)
  expect_equal(cs$consensus[cs$set_id == p$set_id[i]], 10^cs$z[cs$set_id == p$set_id[i]])
  expect_true(all(cs$lo < cs$consensus & cs$consensus < cs$hi))
  expect_true(all(cs$se_z < pmin(sqrt(h$vx), sqrt(h$vy))[match(cs$set_id, p$set_id)] + 1e-12))  # more precise than either
  expect_equal(median(cs$q_ratio), qchisq(.5, 1), tolerance = 0.3)   # pcov is the whole error here
  expect_equal(r$harm$std_diff_sd, 1, tolerance = 0.2)
  expect_null(lin_consensus(p, lin_regress(p, error = "y", B = 0)))
  # a point seen in two pairs counts once: three wells of one sample
  q <- p[1:3, ]; q$set_id <- "s1"; q$y_pt_id <- c("y1", "y2", "y2"); q$x_pt_id <- c("x1", "x1", "x1")
  expect_equal(lin_consensus(q, r)$n_points, 3L)
})

test_that("sensitivity table runs the three weightings and flags the primary", {
  p <- mk_pairs(300, a0 = 0.1, b0 = 0.9, seed = 10, het = TRUE)
  r <- lin_regress(p, error = "both", weighting = "weights", B = 0)
  s <- lin_sensitivity(p, r, x_pcov_max = 40, y_pcov_max = 40, B = 0)
  expect_equal(s$weighting, c("full", "gate", "weights")); expect_equal(s$primary, c(FALSE, FALSE, TRUE))
  expect_true(s$n[2] < s$n[1]); expect_true(all(is.finite(s$b))); expect_true(all(is.finite(s$ccc)))
  expect_equal(s$b[3], r$fit$b)
})

test_that("replicate precision reports within-pair CV and ICC for duplicates", {
  set.seed(3); n <- 100; z <- runif(n, 1, 4)
  d <- data.frame(set_id = paste0("s", 1:n), x_pt_id = paste0("a", 1:n), y_pt_id = paste0("b", 1:n),
                  x_conc = 10^(z + rnorm(n, 0, .05)), y_conc = 10^(z + rnorm(n, 0, .05)), pair_type = "duplicate", stringsAsFactors = FALSE)
  rp <- lin_replicate_precision(d)
  expect_equal(rp$sd_within, .05, tolerance = 0.2); expect_true(rp$icc > 0.98); expect_true(rp$cv > 0.1 && rp$cv < 0.14)
  expect_null(lin_replicate_precision(d[0, ])); d$pair_type <- "plate"; expect_null(lin_replicate_precision(d))
})

test_that("assessment uses CCC, Bland-Altman trend and notes harmonizing dilutions", {
  p <- mk_pairs(300, a0 = 0, b0 = 0.7, sdx = .15, sdy = .15, seed = 11)
  r <- lin_regress(p, error = "both", transform = "harmonize", B = 100)
  codes <- vapply(lin_assess(r, list(pair_types = "dilution")), function(z) z$code, "")
  expect_true(all(c("slope-not-1", "ba-trend", "harmonize-dilutions", "transform-applied") %in% codes))
  q <- mk_pairs(300, a0 = 0.4, b0 = 1, sdx = .1, sdy = .1, seed = 12)
  rq <- lin_regress(q, error = "both", B = 100)
  expect_true("constant-fold" %in% vapply(lin_assess(rq), function(z) z$code, ""))
  noisy <- mk_pairs(300, b0 = 1, sdx = .6, sdy = .6, seed = 13)
  cc <- vapply(lin_assess(lin_regress(noisy, error = "both", B = 50)), function(z) z$code, "")
  expect_true("ccc-low" %in% cc)
})

test_that("natural-scale Deming works and the bootstrap leaves the caller's RNG alone", {
  p <- mk_pairs(200, seed = 14); p$x_pcov <- 20; p$y_pcov <- 20
  set.seed(99); r <- lin_regress(p, scale = "natural", error = "both", B = 50)
  after <- runif(1); set.seed(99)
  expect_true(r$ok); expect_true(is.finite(r$fit$b))
  expect_equal(after, runif(1))
  rt <- lin_regress(p, scale = "natural", error = "both", transform = "harmonize", B = 0)
  expect_equal(rt$refit$b, 1, tolerance = 1e-6)
})

test_that("the full report carries sensitivity, harmonization and consensus", {
  pts <- lin_prepare(mk_raw(60, seed = 21)); sets <- lin_sets(pts)
  xs <- list(dilution = "100", plate = "P1", method = "bayesian"); ys <- list(dilution = "400", method = "bayesian")
  pr <- lin_pairs(pts, xs, ys, match = c("plate", "method"), differ = "dilution")
  reg <- lin_regress(pr, error = "both", weighting = "full", transform = "harmonize", B = 60)
  rep <- lin_report_build(list(scope = list(project_id = 1, study = "S", experiment = "E"), preset = "Dilution vs dilution",
                               x = xs, y = ys, match = c("plate", "method"), differ = "dilution", filters = list(antigen = "AG1"),
                               sets = sets, pairs = pr, reg = reg, sens = lin_sensitivity(pr, reg, B = 0), consensus = lin_consensus(pr, reg)))
  txt <- lin_report_text(rep)
  for (pat in c("## Agreement \\(primary metrics\\)", "Lin's concordance correlation coefficient", "Bland-Altman limits of agreement",
                "Deming error-variance ratio delta", "## Sensitivity to weighting", "All pairs, weighted by precision",
                "harmonize both sets to the latent concentration", "alpha1 =", "beta2 =", "## Consensus per sample", "inverse-variance",
                "standardized differences"))
    expect_match(txt, pat)
  js <- jsonlite::fromJSON(as.character(lin_report_json(rep)))
  expect_equal(js$regression$error_model, "both"); expect_equal(nrow(js$sensitivity), 3L)
  expect_equal(js$transform$kind, "harmonize"); expect_true(js$consensus$n_samples > 0)
  expect_true(is.numeric(js$regression$stats$ccc))
})

test_that("module: errors-in-variables, harmonize, sensitivity and consensus", {
  scope <- shiny::reactive(list(project_id = 92, study = "Luminex", experiment = "IgG1_500"))
  shiny::testServer(linearityServer, args = list(pool = NULL, scope = scope, fetch = function(...) mk_raw(40, seed = 12)), {
    session$setInputs(load = 1)
    session$setInputs(antigen = "AG1", feature = "(all)", timeperiod = NULL, patient = "",
                      x_method = "bayesian", y_method = "bayesian", x_dilution = "100", y_dilution = "400",
                      x_plate = "P1", y_plate = character(), x_source = character(), y_source = character(),
                      x_wavelength = character(), y_wavelength = character(),
                      match = c("plate", "method"), differ = "dilution",
                      scale = "log10", error = "both", weighting = "full", delta_mode = "auto", transform = "harmonize",
                      boot = 40, x_pcov_max = 25, y_pcov_max = 25, level = 0.95, preset = "dilution")
    rg <- reg()
    expect_true(rg$ok); expect_equal(rg$error, "both"); expect_equal(rg$transform, "harmonize")
    expect_equal(rg$refit$b, 1, tolerance = 1e-6)
    expect_equal(sens()$weighting, c("full", "gate", "weights"))
    expect_equal(nrow(consensus()), 40L)
    txt <- text()
    for (pat in c("Deming error-variance ratio", "## Sensitivity to weighting", "## Consensus per sample", "harmonize both sets"))
      expect_match(txt, pat)
    # user delta, y-only error removes the consensus, weights mode uses York
    session$setInputs(delta_mode = "custom", delta = 2)
    expect_equal(reg()$delta, 2)
    session$setInputs(weighting = "weights"); expect_true(is.na(reg()$delta)); expect_true(reg()$ok)
    session$setInputs(error = "y"); expect_null(consensus()); expect_equal(reg()$transform, "none")
  })
})

test_that("every info icon in the module points at a note that exists, and the notes render", {
  skip_if_not(file.exists("help_utils.R") || file.exists(file.path("src", "help_utils.R")))
  base <- if (file.exists("help_utils.R")) "." else "src"
  if (!exists("load_help_merged")) source(file.path(base, "help_utils.R"))
  dirs <- list.dirs(file.path(base, "help"), recursive = FALSE)
  reg <- suppressWarnings(load_help_merged(dirs))
  src <- paste(readLines(file.path(base, "linearity_module.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  ids <- unique(regmatches(src, gregexpr('(?<=hi\\(")qc\\.linearity\\.[a-z_]+(?="\\))|(?<=help_icon\\(")qc\\.linearity\\.[a-z_]+', src, perl = TRUE))[[1]])
  expect_true(length(ids) >= 9)
  expect_true(all(ids %in% names(reg)), info = paste(setdiff(ids, names(reg)), collapse = ", "))
  # all the linearity notes, the three glossary concepts, and every [[link]] resolve
  mine <- grep("^qc\\.linearity\\.|^glossary\\.(errors_in_both_axes|agreement_ccc_ba|latent_consensus)$", names(reg), value = TRUE)
  expect_equal(length(mine), 14L)
  for (id in mine) {
    e <- reg[[id]]
    txt <- paste(e$body, e$more)
    links <- unique(regmatches(txt, gregexpr("(?<=\\[\\[)[A-Za-z0-9_.]+", txt, perl = TRUE))[[1]])
    expect_true(all(links %in% names(reg)), info = paste(id, paste(setdiff(links, names(reg)), collapse = ",")))
    expect_true(all(e$see_also %in% names(reg)), info = paste(id, "see_also"))
    expect_no_error(help_modal_body(id, function(x) x, reg))
  }
  expect_null(reg[["glossary.replicate_linearity"]]); expect_null(reg[["qc.replicate_linearity"]])
  g <- reg[["glossary.errors_in_both_axes"]]; expect_equal(g$topic, "Agreement and linearity")
  expect_true("Agreement and linearity" %in% GLOSSARY_TOPICS)
})
