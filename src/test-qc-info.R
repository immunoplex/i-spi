# Tests for the Quality Control information changes: Aggregate Beads, the
# Sources-tab sample hover, and the Standard Curve sample popup / table.
library(testthat)
.qi_src <- function(f) for (p in c(file.path("src", f), f)) if (file.exists(p)) return(invisible(source(p)))
suppressMessages(library(plotly))
if (!exists("pct_agg_prep")) .qi_src("bead_count_functions.R")
if (!exists(".sc_sample_hover")) .qi_src("std_curve_view_module.R")
if (!exists(".lin_spec")) .qi_src("linearity_report.R")
.qi_txt <- function(f) paste(readLines(if (file.exists(f)) f else file.path("src", f), warn = FALSE), collapse = "\n")

# ---- aggregate beads ------------------------------------------------------------
test_that("well position runs down each column, for 96 and 384 well plates", {
  expect_equal(pct_agg_well_position(c("A1", "B1", "H1", "A2", "H12"))$position, c(1, 2, 8, 9, 96))
  expect_equal(pct_agg_well_position(c("A1", "B1", "P1", "A2", "P24"))$position, c(1, 2, 16, 17, 384))
  expect_equal(pct_agg_well_position(c("a3", "A1"))$position, c(17, 1))          # lower case, 96-well
  bad <- pct_agg_well_position(c("A1", "??", "A0", ""))
  expect_equal(bad$position[1], 1); expect_true(all(is.na(bad$position[2:4])))
})

mk_tabs <- function() {
  s <- data.frame(well = rep(c("A1", "B1"), each = 2), pctaggbeads = c(4, 9, 12, 3), plate = "P1", plateid = "long.P1",
                  nominal_sample_dilution = "400", antigen = rep(c("a", "b"), 2), sampleid = rep(c("S1", "S2"), each = 2),
                  patientid = rep(c("p1", "p2"), each = 2), timeperiod = "D0", dilution = "400", masked = FALSE, stringsAsFactors = FALSE)
  st <- data.frame(well = c("C1", "D1"), pctaggbeads = c(2, 35), plate = "P1", plateid = "long.P1", nominal_sample_dilution = "400",
                   antigen = "a", sampleid = "STD", dilution = c("100", "200"), stringsAsFactors = FALSE)
  ct <- data.frame(well = "E1", pctaggbeads = 5, plate = "P1", plateid = "long.P1", nominal_sample_dilution = "400", antigen = "a", sampleid = "CTL", stringsAsFactors = FALSE)
  bl <- data.frame(well = "F1", pctaggbeads = 1, plate = "P1", plateid = "long.P1", nominal_sample_dilution = "400", antigen = "a", stringsAsFactors = FALSE)
  list(sample = s, standard = st, control = ct, blank = bl)
}

test_that("pct_agg_prep gives one row per well, with the highest value across antigens", {
  d <- pct_agg_prep(mk_tabs())
  expect_equal(nrow(d), 6L)                                   # 2 sample wells (not 4), 2 standards, 1 control, 1 blank
  expect_setequal(unique(d$type), c("Samples", "Standards", "Controls", "Blanks"))
  expect_equal(d$pctagg[d$type == "Samples" & d$well == "A1"], 9)       # max of 4 and 9
  expect_equal(unique(d$plate_nom), "P1-400")
  expect_false(is.unsorted(d$well_position))
  t2 <- mk_tabs(); t2$sample <- NULL; t2$blank$pctaggbeads <- NULL
  expect_setequal(unique(pct_agg_prep(t2)$type), c("Standards", "Controls"))     # missing / column-less tables are skipped
  expect_null(pct_agg_prep(list(sample = NULL, standard = data.frame())))
  t3 <- mk_tabs(); t3$sample$plate <- NA                                          # the long plateid stands in
  expect_true("long.P1-400" %in% pct_agg_prep(t3)$plate_nom)
})

test_that("pct_agg_plot draws a trace per well type, the threshold, and rings wells above it", {
  d <- pct_agg_prep(mk_tabs())
  p <- plotly_build(pct_agg_plot(d, threshold = 30))
  nm <- vapply(p$x$data, function(t) if (is.null(t$name)) "" else t$name, "")
  expect_true(all(c("Samples", "Standards", "Controls", "Blanks", "% Agg threshold") %in% nm))
  std <- p$x$data[[which(nm == "Standards")]]
  expect_equal(as.numeric(unlist(std$y)), c(2, 35)); expect_equal(as.numeric(unlist(std$x)), d$well_position[d$type == "Standards"])
  expect_true("#be0032" %in% unlist(std$marker$line$color))             # the 35% well is ringed
  expect_match(std$text[2], "Well: D1", fixed = TRUE); expect_match(std$text[2], "35", fixed = TRUE)
  smp <- p$x$data[[which(nm == "Samples")]]
  expect_match(smp$text[1], "Patient: p1", fixed = TRUE); expect_match(smp$text[1], "Timeperiod: D0", fixed = TRUE)
  ttl <- p$x$layout$title; if (is.list(ttl)) ttl <- ttl$text
  expect_match(ttl, "1 above the threshold of 30")
  tt <- function(a) if (is.list(a$title)) a$title$text else a$title
  expect_equal(tt(p$x$layout$yaxis), "% aggregate beads")
  expect_match(tt(p$x$layout$xaxis), "Well position")
  expect_true("D1" %in% unlist(p$x$layout$xaxis$ticktext))
  nm2 <- vapply(plotly_build(pct_agg_plot(d, NA))$x$data, function(t) if (is.null(t$name)) "" else t$name, "")
  expect_false("% Agg threshold" %in% nm2)                                # no threshold -> no line
  expect_no_error(plotly_build(pct_agg_plot(d[0, ], 30)))                 # no data -> a message, no error
})

test_that("the list under the plot shows wells above the threshold, or the ten highest", {
  d <- pct_agg_prep(mk_tabs())
  expect_equal(pct_agg_table(d, 10)$well, c("D1", "B1"))
  expect_equal(nrow(pct_agg_table(d, 30)), 1L)
  expect_equal(nrow(pct_agg_table(d, NA)), 6L)
  expect_equal(nrow(pct_agg_table(d[0, ], 5)), 0L)
})

# ---- Standard Curve sample popup ----------------------------------------------------
test_that("the sample popup names the patient and timeperiod", {
  smp <- data.frame(sampleid = c("S1", "S2"), patientid = c("pt-7", "__none__"), timeperiod = c("D28", NA),
                    final_concentration = c(120, 30), pcov = c(8, 40), censor_class = c("quantified", "below_lloq"), stringsAsFactors = FALSE)
  h <- .sc_sample_hover(smp)
  expect_match(h[1], "patientid pt-7", fixed = TRUE); expect_match(h[1], "timeperiod D28", fixed = TRUE)
  expect_match(h[1], "<b>S1</b>", fixed = TRUE)
  expect_match(h[2], "patientid – · timeperiod –", fixed = TRUE)         # not recorded
  expect_no_error(.sc_sample_hover(smp[, c("sampleid", "final_concentration", "pcov")]))   # older rows without them
})

# ---- the QC component choices ---------------------------------------------------------------
test_that("the QC tab is named Quality Control and offers Beads, not Bead Count", {
  txt <- .qi_txt("ui_handler.R")
  expect_match(txt, 'title = "Quality Control",', fixed = TRUE)
  expect_false(grepl('title = "Quality Control - Basic"', txt, fixed = TRUE))
  expect_match(txt, 'choices = c("Beads", "Plate Dilution Series", "Standard Curve",', fixed = TRUE)
  expect_match(txt, "input.qc_component == 'Beads' && output.exp_assay_type_js == 'bead_assay'", fixed = TRUE)
  expect_match(txt, 'if (input$qc_component == "Beads" &&', fixed = TRUE)
  expect_match(txt, 'is_bead <- !identical(info$assay_type, "ELISA")', fixed = TRUE)
  expect_false(grepl("qc_component == 'Bead Count'", txt, fixed = TRUE))
  bc <- .qi_txt("bead_count_analysis_ui.R")
  expect_match(bc, 'choices = c("Bead Count", "Aggregate Beads")', fixed = TRUE)
})

# ---- module-level checks with mocked data ----------------------------------------------------
.qi_mock <- function(env, ...) {
  m <- list(...); old <- lapply(names(m), function(n) get0(n, envir = globalenv(), inherits = FALSE))
  names(old) <- names(m)
  for (n in names(m)) assign(n, m[[n]], envir = globalenv())
  withr_free <- function() for (n in names(m)) if (is.null(old[[n]])) rm(list = n, envir = globalenv()) else assign(n, old[[n]], envir = globalenv())
  do.call(on.exit, list(quote(withr_free()), add = TRUE), envir = env)
  assign("withr_free", withr_free, envir = env)
}

test_that("Plate Dilution Series > Sources: test-sample points show well, patientid and timeperiod; standards are unchanged", {
  skip_if_not(exists("plateDilutionSeriesServer") || { .qi_src("mask_ui_helpers.R"); .qi_src("plate_dilution_series_helpers.R"); .qi_src("plate_dilution_series_module.R"); TRUE })
  if (!exists("pds_feat_label")) .qi_src("plate_dilution_series_helpers.R")
  if (!exists("lin_control_label")) .qi_src("linearity_stored.R")
  std <- data.frame(plate = rep(c("P1", "P2"), each = 4), plateid = "x", well = rep(c("A1", "B1", "C1", "D1"), 2),
                    dilution = rep(c(10, 20, 40, 80), 2), antibody_mfi = rep(c(900, 600, 300, 100), 2),
                    antigen = "AG", feature = "IgG", source = "V2", stype = "S", masked = FALSE, stringsAsFactors = FALSE)
  smp <- data.frame(plate = "P1", plateid = "x", well = c("E1", "E2", "F1", "F2"), dilution = c(100, 200, 100, 200),
                    antibody_mfi = c(500, 300, 450, 250), antigen = "AG", feature = "IgG", source = "V2", stype = "X",
                    patientid = c("pt-1", "pt-1", "pt-2", "pt-2"), timeperiod = c("D0", "D0", "D28", "D28"), sampleid = "S", stringsAsFactors = FALSE)
  ctl <- data.frame(plate = "P2", plateid = "x", well = c("G1", "G2"), dilution = c(50, 500), antibody_mfi = c(700, 200),
                    antigen = "AG", feature = "IgG", source = "supplierA", stype = "C", sampleid = "QC1Orange", stringsAsFactors = FALSE)
  .qi_mock(environment(), fetch_raw_standard = function(...) std, fetch_raw_sample = function(...) smp,
           fetch_raw_control = function(...) ctl)
  scope <- shiny::reactive(list(project_id = 1, study = "S", experiment = "E"))
  shiny::testServer(plateDilutionSeriesServer, args = list(pool = NULL, scope = scope), {
    session$setInputs(sources_antigen = "AG", sources_feature = "IgG", sources_show = c("samples", "controls"))
    p <- jsonlite::fromJSON(output$sources_plot, simplifyVector = FALSE)$x
    txt <- unlist(lapply(p$data, function(t) t$text))
    hit <- grep("^Test sample", txt, value = TRUE)
    expect_equal(length(hit), 4L)
    expect_true(all(grepl("well: [EF][12]", hit))); expect_true(all(grepl("patientid: pt-[12]", hit)))
    expect_true(all(grepl("timeperiod: D(0|28)", hit)))
    expect_match(hit[1], "plate: P1.*dilution: 100.*MFI: 500")            # what was there is kept
    std_hov <- grep("^Curve source:", txt, value = TRUE)
    expect_true(length(std_hov) > 0); expect_false(any(grepl("patientid", std_hov)))
    ctl_hov <- grep("^Control:", txt, value = TRUE)
    expect_equal(length(ctl_hov), 2L)
    expect_true(all(grepl("Control: QC1Orange<br>control source: supplierA<br>plate: P2", ctl_hov)))
    expect_true(any(vapply(p$data, function(t) identical(t$name, "QC1Orange | supplierA") && identical(t$line$dash, "dot"), logical(1))))
    session$setInputs(sources_show = "controls")
    txt2 <- unlist(lapply(jsonlite::fromJSON(output$sources_plot, simplifyVector = FALSE)$x$data, function(t) t$text))
    expect_false(any(grepl("^Test sample", txt2))); expect_true(any(grepl("^Control:", txt2)))
  })
})

test_that("Standard Curve > Back-calculated samples: experiment and plate (short name) are added, with downloads", {
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE))
  lk <- data.frame(curve_id = c("101", "102"), project_id = 1, study_accession = "S", experiment_accession = "EXP_1",
                   plateid = c("LONG.NAME.PLATE_1.400", "LONG.NAME.PLATE_2.400"), plate = c("plate_1", "plate_2"),
                   nominal_sample_dilution = "400", source = "V2", wavelength = "__none__", antigen = "AG1", feature = "IgG", stringsAsFactors = FALSE)
  smp <- data.frame(sampleid = c("A1", "S2"), well = c("A1", "B2"), patientid = c("pt-1", "pt-2"), timeperiod = c("D0", "D28"), dilution = "400",
                    predicted_concentration = c(2.1, 3.0), final_concentration = c(125, 1000), se_concentration = .1, pcov = c(12, 8), pcov_pass = TRUE, stringsAsFactors = FALSE)
  frames <- list(fit_best = data.frame(model_name = "logistic5"), fits = data.frame(model_name = "logistic5", is_best = TRUE),
                 params = data.frame(param = c("a", "d"), est = c(1, 2)), gates = data.frame(gate = "g", passed = TRUE),
                 grid = data.frame(log10_concentration = 1:3, y = 1:3), samples = smp, diagnostics = data.frame(lloq = 1), loo = data.frame())
  .qi_mock(environment(), fetch_curve_lookup = function(...) lk, fetch_raw_header = function(...) data.frame(assay_response_variable = "mfi"),
           fetch_calib_methods = function(pool, cid) c("bayesian", "frequentist"), fetch_calib_bundle = function(pool, cid, meth) frames,
           fetch_calib_standards = function(...) data.frame(well = "A1", concentration = 1), fetch_calib_blanks = function(...) data.frame(well = "H1", response_model = 1))
  scope <- shiny::reactive(list(project_id = 1, study = "S", experiment = "EXP_1"))
  shiny::testServer(stdCurveViewServer, args = list(pool = NULL, scope = scope), {
    session$setInputs(antigen = "AG1", curve = "102", method = "bayesian")
    v <- samples_view()
    expect_equal(names(v)[1:8], c("experiment", "plate", "well", "antigen", "feature", "method", "specimen_id", "patientid"))
    expect_false("sampleid" %in% names(v))                                    # replaced by specimen_id
    expect_equal(v$well, c("A1", "B2")); expect_equal(v$specimen_id, c("(same as well)", "S2"))
    expect_equal(unique(v$experiment), "EXP_1"); expect_equal(unique(v$plate), "plate_2")     # not the long plateid
    expect_false(any(grepl("LONG", unlist(v)))); expect_true(all(c("patientid", "timeperiod") %in% names(v)))
    csv <- utils::read.csv(output$dl_samples_csv); expect_equal(nrow(csv), 2L); expect_equal(csv$plate, c("plate_2", "plate_2"))
    xl <- output$dl_curve_xlsx
    expect_match(basename(xl), "curve_EXP_1_plate_2_AG1_bayesian.xlsx", fixed = TRUE)
    sh <- openxlsx::getSheetNames(xl)
    expect_true(all(c("curve", "samples", "standards", "blanks", "model_selection", "parameters", "gates", "limits_loq_lod", "fitted_grid") %in% sh))
    info <- openxlsx::read.xlsx(xl, "curve"); expect_equal(info$value[info$item == "plate"], "plate_2")
    expect_equal(nrow(openxlsx::read.xlsx(xl, "samples")), 2L)
  })
})

test_that("Beads > Aggregate Beads: reads the four tables, one plate at a time, with the study threshold", {
  skip_if_not(requireNamespace("shinyWidgets", quietly = TRUE))
  .qi_src("help_utils.R")
  mkw <- function(wells, pct, extra = list()) do.call(data.frame, c(list(well = wells, pctaggbeads = pct, plate = "plate_1", plateid = "LONG1",
    nominal_sample_dilution = "400", antigen = "AG", stringsAsFactors = FALSE), extra))
  smp <- mkw(c("A1", "B1", "C1"), c(5, 40, 8), list(sampleid = c("S1", "S2", "S3"), patientid = c("p1", "p2", "p3"), timeperiod = "D0", dilution = "400"))
  tabs <- list(sample = smp, standard = mkw(c("D1", "E1"), c(3, 6), list(sampleid = "STD", dilution = c("100", "200"))),
               control = mkw("F1", 4), blank = mkw("G1", 1))
  stubs <- list(makeModuleUIDestroyable = function(f) f, makeModuleServerDestroyable = function(f) f, help_icon = function(...) NULL)
  for (n in names(stubs)) if (!exists(n)) assign(n, stubs[[n]], envir = globalenv())
  suppressMessages({ library(shinyWidgets); library(dplyr) })
  if (!exists("beadCountServer")) { .qi_src("bead_count_analysis_ui.R") }
  .qi_mock(environment(), fetch_raw_sample = function(...) tabs$sample, fetch_raw_standard = function(...) tabs$standard,
           fetch_raw_control = function(...) tabs$control, fetch_raw_blank = function(...) tabs$blank,
           fetch_db_samples_bc = function(...) { s <- smp; s$n <- 50L; s$study_accession <- "S"; s$experiment_accession <- "E"; s$stype <- "X"
             s$agroup <- "g"; s$antibody_mfi <- 100; s$timeperiod <- "D0"; s$plate_nom <- paste(s$plate, s$nominal_sample_dilution, sep = "-"); s },
           fetch_study_configuration = function(...) data.frame(param_name = c("failed_well_criteria", "upper_bc_threshold", "lower_bc_threshold", "pct_agg_threshold"),
             param_character_value = c("Below_Lower_Threshold", NA, NA, NA), param_integer_value = c(NA, 100L, 35L, 30L), stringsAsFactors = FALSE),
           userWorkSpaceID = function() 1, db_pool = NULL)
  suppressMessages(shiny::testServer(beadCountServer, args = list(selected_study = shiny::reactive("S"), selected_experiment = shiny::reactive("E"), currentuser = NULL), {
    session$setInputs(bead_mode = "Aggregate Beads", agg_plate = "plate_1-400")
    d <- agg_data()
    expect_equal(nrow(d), 7L); expect_setequal(unique(d$type), c("Samples", "Standards", "Controls", "Blanks"))
    expect_equal(agg_threshold(), 30)
    expect_match(gsub("<[^>]+>", "", as.character(output$agg_status$html)), "7 wells on this plate; threshold 30%")
    p <- jsonlite::fromJSON(output$aggPlot, simplifyVector = FALSE)$x
    nm <- vapply(p$data, function(t) if (is.null(t$name)) "-" else t$name, "")
    expect_true(all(c("Samples", "Standards", "Controls", "Blanks", "% Agg threshold") %in% nm))
    tb <- pct_agg_table(agg_plate_data(), agg_threshold()); expect_equal(tb$well, "B1")
    expect_equal(nrow(utils::read.csv(output$download_pct_agg)), 7L)
  }))
})

test_that("specimen id is shown as (same as well) when the importer stored the well as the sample id", {
  expect_equal(.sc_specimen(c("B2", "b2 ", "S9", NA, ""), c("B2", "B2", "B2", "B2", "B2")), c("(same as well)", "(same as well)", "S9", NA, ""))
  expect_equal(.sc_specimen(c("S1", "A1"), c(NA, "A1")), c("S1", "(same as well)"))
  smp <- data.frame(sampleid = c("A1", "S2"), well = c("A1", "B2"), patientid = c("pt-1", "pt-2"), timeperiod = c("D0", "D28"),
                    final_concentration = c(125, 1000), pcov = c(12, 8), censor_class = "quantified", stringsAsFactors = FALSE)
  h <- .sc_sample_hover(smp)
  expect_match(h[1], "<b>A1</b>", fixed = TRUE); expect_false(grepl("specimen", h[1]))          # same as the well: not repeated
  expect_match(h[2], "<b>B2 (specimen S2)</b>", fixed = TRUE)
  v <- .sc_samples_view(smp, data.frame(experiment = "E", plate = "plate_1", antigen = "AG", feature = NA, method = "bayesian"))
  expect_equal(v$specimen_id, c("(same as well)", "S2")); expect_equal(v$well, c("A1", "B2")); expect_false("sampleid" %in% names(v))
  expect_equal(unname(unlist(v[1, 1:3])), c("E", "plate_1", "A1"))
  # a table without a well (older rows) still works
  expect_no_error(.sc_samples_view(smp[, setdiff(names(smp), "well")], data.frame(experiment = "E", plate = "p", antigen = "AG", feature = NA, method = "m")))
  expect_equal(.lin_spec("A1", "A1"), "(same as well)"); expect_equal(.lin_spec("S1", "A1"), "S1")
})

# ---- Explore fits: the Controls trace ---------------------------------------------
if (!exists("lin_control_label")) .qi_src("linearity_stored.R")
mk_cc <- function() data.frame(
  well = c("A9", "B9", "C9", "D9", "E9", "H1", "G3"),
  sampleid = c("QC1Orange", "QC1Orange", "QC1Orange", "QC1Orange", "QC1Orange", "QC2Green", "G3"),
  source = c(rep("jackson_high", 6), "QC1 (Low)"),
  dilution = c(50, 50, 500, 500, 500, 50, 2500), included = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
  predicted_log10_concentration = c(2.9, 2.91, 1.79, 1.80, 1.81, 2.5, 1.2),
  response_model = c(4.48, 4.48, 4.25, 4.24, 4.26, 4.1, 3.0),
  assay_response_raw = c(30000, 30200, 18000, 17500, 18200, 12000, 1000),
  final_concentration = c(4e4, 4.1e4, 3.1e4, 3.0e4, 3.2e4, 1.6e4, 4e4),
  pcov = 10, censor_class = "quantified", stringsAsFactors = FALSE)

test_that("control points: replicate count and CV% per name, control source and dilution", {
  d <- .sc_control_points(mk_cc())
  expect_equal(nrow(d), 7L)
  expect_equal(d$rep_n, c(2L, 2L, 3L, 3L, 3L, 1L, 1L))
  expect_equal(d$cv_resp_pct[1], 100 * sd(c(30000, 30200)) / mean(c(30000, 30200)))
  expect_equal(d$cv_conc_pct[3], 100 * sd(c(3.1e4, 3.0e4, 3.2e4)) / 3.1e4)
  expect_true(is.na(d$cv_resp_pct[6]))
  expect_match(d$hover[1], "Control: QC1Orange.*control source: jackson_high.*dilution: 1:50 [|] well A9")
  expect_match(d$hover[3], "replicates on this plate: 3.*CV% response: [0-9.]+%.*CV% concentration: [0-9.]+%")
  expect_false(grepl("replicates", d$hover[6]))
  # old-style control: name from Source, no control source
  expect_equal(d$label[7], "QC1 (Low)"); expect_true(is.na(d$control_source[7]))
  expect_match(d$hover[7], "control source: –")
})

test_that("control points: masked or off-axis wells are left out", {
  cc <- mk_cc(); cc$included[2] <- FALSE; cc$predicted_log10_concentration[6] <- NA
  d <- .sc_control_points(cc)
  expect_equal(nrow(d), 5L)
  expect_equal(d$rep_n[d$well == "A9"], 1L)          # its replicate was masked
  expect_null(.sc_control_points(NULL)); expect_null(.sc_control_points(cc[0, ]))
  cc$predicted_log10_concentration <- NA
  expect_null(.sc_control_points(cc))
})
