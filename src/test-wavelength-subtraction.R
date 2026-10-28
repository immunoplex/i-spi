# =============================================================================
# test-wavelength-subtraction.R  --  Data > Raw inputs > Subtract Wavelengths
# Pure-R parts (report, eligibility, pairing/subtraction, assembly) plus a
# testServer pass over the tab. The DB write is shared with the Replicates tool.
# Run: testthat::test_file("src/test-wavelength-subtraction.R")
# =============================================================================
library(testthat)
library(shiny)

if (!exists("wsub_build")) {
  root <- c(".", "src", "..", "../src")
  hit  <- root[file.exists(file.path(root, "wavelength_subtraction_module.R"))][1]
  if (is.na(hit)) stop("cannot find src/ -- run from the repo root")
  source(file.path(hit, "replicates_module.R"))
  source(file.path(hit, "wavelength_subtraction_module.R"))
}

# Two plates. P1 has 450 + 620 (+ 570 on samples); P2 has only 450.
mk <- function(wl, plateid, wells, resp, stype = "X", id = "S", masked = FALSE) {
  data.frame(study_accession = "S1", experiment_accession = "E1", plate_id = plateid,
    plateid = plateid, plate = "plate_1", nominal_sample_dilution = "100", stype = stype,
    sampleid = id, patientid = "p", timeperiod = "V1", dilution = 100, wavelength = wl,
    antigen = "Ag", feature = "Absorbance", source = "Serum", well = wells,
    antibody_mfi = resp, antibody_n = NA_integer_, gate_class = "old",
    masked = masked, mask_reason = NA_character_, project_id = 1L, stringsAsFactors = FALSE)
}
fr <- function() list(
  X = rbind(mk("450", "P1", c("A1", "A2", "A3"), c(1.0, 2.0, 3.0), id = c("a", "b", "c")),
            mk("620", "P1", c("A1", "A2"),       c(0.1, 0.2),      id = c("a", "b")),   # A3: no reference
            mk("570", "P1", c("A1", "A2"),       c(0.5, 0.6),      id = c("a", "b")),
            mk("450", "P2", "A1", 9, id = "z")),
  S = NULL, C = NULL, B = NULL,
  header = data.frame(xmap_header_id = 1:2, study_accession = "S1", experiment_accession = "E1",
    plate_id = c("P1", "P2"), plateid = c("P1", "P2"), plate = "plate_1",
    nominal_sample_dilution = "100", wavelengths = c("450|570|620", "450"), stringsAsFactors = FALSE))
k1 <- .wsub_plate_key(fr()$header)[1]; k2 <- .wsub_plate_key(fr()$header)[2]

test_that("wavelengths are detected, ordered numerically; delta and __none__ ignored", {
  expect_equal(wsub_wavelengths(fr()), c("450", "570", "620"))
  f <- fr(); f$X$wavelength[1] <- "delta"
  expect_false("delta" %in% wsub_wavelengths(f))
  b <- list(X = data.frame(wavelength = "__none__"), S = NULL, C = NULL, B = NULL)
  expect_length(wsub_wavelengths(b), 0L)
})

test_that("report: rows by sample type x wavelength, and one line per plate", {
  r <- wsub_report(fr())
  expect_equal(r$by_type$Rows[r$by_type$Wavelength == "450"], 4L)
  expect_equal(r$by_type$Plates[r$by_type$Wavelength == "450"], 2L)
  expect_equal(nrow(r$by_plate), 2L)
  expect_equal(r$by_plate$Wavelengths[grepl("^P1", r$by_plate$Plate)], "450 | 570 | 620")
})

test_that("only plates holding the reference AND a signal are eligible", {
  e <- wsub_eligible_plates(fr(), "450", "620")
  expect_equal(e$key, k1)                              # P2 has no 620
  expect_equal(nrow(wsub_eligible_plates(fr(), "620", "450")), 1L)   # roles are the user's choice
})

test_that("signal - reference per well; unpaired signal wells dropped and counted", {
  r <- wsub_apply(fr()$X, "450", "620", c(k1, k2), "xmap_sample")
  expect_equal(nrow(r$out), 2L); expect_equal(r$unmatched, 2L)   # A3 (P1) and A1 (P2)
  expect_equal(r$out$antibody_mfi[order(r$out$well)], c(0.9, 1.8))
  expect_true(all(r$out$wavelength == "delta"))
  expect_true(all(is.na(r$out$gate_class)))                      # computed columns blanked
  expect_false(any(grepl("^xmap_.*_id$", names(r$out))))
})

test_that("plate selection limits what is written", {
  r <- wsub_apply(fr()$X, "450", "620", k2, "xmap_sample")        # only P2 chosen
  expect_null(r$out)
})

test_that("several signal wavelengths against one reference get distinct labels", {
  r <- wsub_apply(fr()$X, c("450", "570"), "620", k1, "xmap_sample")
  expect_setequal(unique(r$out$wavelength), c("delta_450", "delta_570"))
  expect_equal(r$out$antibody_mfi[r$out$wavelength == "delta_570" & r$out$well == "A1"], 0.4)
})

test_that("a pair is masked when either well is masked", {
  f <- fr()$X; f$masked[f$wavelength == "620" & f$well == "A1"] <- TRUE
  r <- wsub_apply(f, "450", "620", k1, "xmap_sample")
  expect_equal(r$out$masked[order(r$out$well)], c(TRUE, FALSE))
})

test_that("wsub_build: renamed experiment, chosen plates only, header says delta", {
  b <- wsub_build(fr(), "E1D", "450", "620", k1)
  expect_true(all(b$tables$xmap_sample$experiment_accession == "E1D"))
  expect_equal(nrow(b$header), 1L)
  expect_equal(b$header$plateid, "P1"); expect_equal(b$header$wavelengths, "delta")
  expect_equal(b$header$experiment_accession, "E1D")
  expect_false("xmap_header_id" %in% names(b$header))
  expect_equal(b$stats$X$unmatched, 1L)
  b2 <- wsub_build(fr(), "E1D", c("450", "570"), "620", k1)
  expect_equal(b2$header$wavelengths, "delta_450,delta_570")
})

test_that("the tab: multi-wavelength detection, choices default sensibly, name validated", {
  .calib_q <<- function(...) data.frame(experiment_accession = c("E1", "E1D"))
  .tbl <<- function(x) x
  tabs <- list(xmap_sample = fr()$X, xmap_standard = NULL, xmap_control = NULL,
               xmap_buffer = NULL, xmap_header = fr()$header)
  sc <- reactive(list(study = "S1", experiment = "E1", project_id = 1L))
  testServer(wavelengthSubtractServer,
             args = list(scope = sc, get_table = function(t) tabs[[t]], pool = NULL,
                         reload_trigger = reactiveVal(0)), {
    expect_true(multi())
    expect_equal(wls(), c("450", "570", "620"))
    session$setInputs(ref = "620", signals = c("450", "570"), new_name = "E1D")
    expect_match(name_problem(), "already exists")          # E1D taken
    session$setInputs(new_name = "E1sub")
    expect_null(name_problem())
    expect_equal(eligible()$key, k1)
    html <- as.character(output$plates_ui$html)
    expect_match(html, "P1", fixed = TRUE); expect_false(grepl("P2", html, fixed = TRUE))
    expect_match(as.character(output$equation$html), "450 − 620", fixed = TRUE)
  })
  one <- list(xmap_sample = mk("450", "P1", "A1", 1), xmap_standard = NULL, xmap_control = NULL,
              xmap_buffer = NULL, xmap_header = fr()$header)
  testServer(wavelengthSubtractServer,
             args = list(scope = sc, get_table = function(t) one[[t]], pool = NULL,
                         reload_trigger = reactiveVal(0)), {
    expect_false(multi())                                   # single wavelength: no tab
  })
})
