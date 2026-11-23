# Tests for plate_dilution_series_helpers.R: the test-sample and control series
# overlaid on QC > Plate Dilution Series > Sources. No database, no Shiny.
library(testthat)
.bc_src <- function(f) for (p in c(file.path("src", f), f)) if (file.exists(p)) return(invisible(source(p)))
if (!exists("lin_control_label")) .bc_src("linearity_stored.R")
if (!exists("pds_control_series")) .bc_src("plate_dilution_series_helpers.R")

# Raw xmap_control rows: QC1Orange / QC2Green from two control sources on their own
# plates at 50 and 500 (duplicate wells), one QC at a single dilution, and an
# old-style control whose name sits in Source (sampleid = well).
mk_raw_ctl <- function() {
  rows <- list()
  for (cs in c("jackson_high", "southern_high")) for (nm in c("QC1Orange", "QC2Green")) for (dl in c(50, 500)) for (w in 1:2)
    rows[[length(rows) + 1L]] <- data.frame(
      plate = if (cs == "jackson_high") "plate_1" else "plate_2", plateid = "x",
      well = paste0(if (nm == "QC1Orange") "A" else "B", w + if (dl == 500) 2 else 0),
      stype = "C", sampleid = nm, source = cs, dilution = dl, antigen = "PT", feature = "IgG",
      antibody_mfi = 1000 / (dl / 50) * w, stringsAsFactors = FALSE)
  rows[[length(rows) + 1L]] <- data.frame(plate = "plate_1", plateid = "x", well = "H1", stype = "C",
    sampleid = "Single", source = "jackson_high", dilution = 50, antigen = "PT", feature = "IgG",
    antibody_mfi = 500, stringsAsFactors = FALSE)
  rows[[length(rows) + 1L]] <- data.frame(plate = "plate_3", plateid = "x", well = "G3", stype = "C",
    sampleid = "G3", source = "QC1 (Low)", dilution = 2500, antigen = "PT", feature = "IgG",
    antibody_mfi = 300, stringsAsFactors = FALSE)
  rows[[length(rows) + 1L]] <- data.frame(plate = "plate_1", plateid = "x", well = "H2", stype = "C",
    sampleid = "QC1Orange", source = "jackson_high", dilution = 50, antigen = "DTd", feature = "IgG",
    antibody_mfi = 800, stringsAsFactors = FALSE)
  do.call(rbind, rows)
}

test_that("controls are keyed '<name> | <control source>', one analyte, every dilution kept", {
  ctl <- pds_control_series(mk_raw_ctl(), "PT", "IgG")
  expect_equal(nrow(ctl), 18L)                       # 16 + single + old-style; DTd row left out
  expect_setequal(unique(ctl$series),
    c("QC1Orange | jackson_high", "QC2Green | jackson_high", "QC1Orange | southern_high",
      "QC2Green | southern_high", "Single | jackson_high", "QC1 (Low)"))
  expect_equal(ctl$dil_num[ctl$control_name == "Single"], 50)   # one dilution: kept as a point
  old <- ctl[ctl$well == "G3", ]
  expect_equal(old$control_name, "QC1 (Low)"); expect_true(is.na(old$control_source))
  expect_null(pds_control_series(mk_raw_ctl(), "RSV", "IgG"))
  expect_null(pds_control_series(NULL, "PT", "IgG"))
})

test_that("the control-source filter and the selector list", {
  raw <- mk_raw_ctl()
  expect_equal(pds_control_sources(raw, "PT", "IgG"), c("(no source)", "jackson_high", "southern_high"))
  j <- pds_control_series(raw, "PT", "IgG", sources = "jackson_high")
  expect_true(all(j$control_source == "jackson_high"))
  none <- pds_control_series(raw, "PT", "IgG", sources = PDS_NONE_SRC)
  expect_equal(none$series, "QC1 (Low)")
  expect_equal(nrow(pds_control_series(raw, "PT", "IgG", sources = character())), 18L)  # empty = all
  expect_null(pds_control_series(raw, "PT", "IgG", sources = "nowhere"))
})

test_that("dilutions are plotted as stored (factors) and non-positive values dropped", {
  raw <- mk_raw_ctl(); raw$antibody_mfi[1] <- 0; raw$dilution[2] <- NA
  ctl <- pds_control_series(raw, "PT", "IgG")
  expect_equal(nrow(ctl), 16L)
  expect_true(all(ctl$dil_num %in% c(50, 500, 2500)))
})

test_that("test samples keep the >1-dilution rule", {
  smp <- data.frame(plate = "p1", well = c("A1", "A2", "A3"), patientid = c("P1", "P1", "P2"),
                    timeperiod = "T0", dilution = c(100, 400, 100), antigen = "PT", feature = "IgG",
                    antibody_mfi = c(900, 300, 500), stringsAsFactors = FALSE)
  s <- pds_sample_series(smp, "PT", "IgG")
  expect_equal(unique(s$sample_lbl), "P1 | T0"); expect_equal(nrow(s), 2L)
  expect_null(pds_sample_series(smp[3, ], "PT", "IgG"))
  expect_equal(pds_feat_label(c(NA, "__none__", "IgG")), c("(no feature)", "(no feature)", "IgG"))
})
