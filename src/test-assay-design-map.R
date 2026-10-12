# =============================================================================
# test-assay-design-map.R
# -----------------------------------------------------------------------------
# The study-design map: a downloadable template, read back from the user's
# edited workbook, validated with the description parser's contract, applied to
# the well inventory and resolved to the identity columns the layout template
# is built from.
#
# Run from src/:  Rscript -e "testthat::test_file('test-assay-design-map.R')"
# =============================================================================

library(testthat)
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(janitor) })
`%||%` <- function(a, b) if (is.null(a)) b else a
for (f in c("assay_description_parse.R", "assay_well_inventory.R", "assay_shape_rules.R",
            "assay_design_map.R"))
  if (!file.exists(f)) stop("run from src/") else suppressWarnings(source(f))
skip_if_no_openxlsx <- function() skip_if_not_installed("openxlsx")

# A small hand-built inventory: two 96-well-style plates, a few wells each, typed
# the way a file typed all-X would be, so the design has something to change.
make_inv <- function() {
  one <- function(pk, idx) {
    w <- c("A1", "B1", "C1", "A2", "B2", "A3", "B3", "A12")
    data.frame(plate_key = pk, plate_label = paste0("label ", pk), plate_index = idx,
               well = w, well_raw = w, row_letter = substr(w, 1, 1),
               col_number = as.integer(substring(w, 2)),
               type_code = "X", specimen_type = "X",
               description = paste0("desc_", w), raw_type_code = "X",
               raw_description = paste0("desc_", w), source_file = paste0(pk, ".csv"),
               response_hint = NA_real_, type_origin = "file", desc_origin = "file",
               instrument_dilution = NA_real_, instrument_source = NA_character_,
               stringsAsFactors = FALSE)
  }
  rbind(one("PlateA", 1L), one("PlateB", 2L))
}

# write a design workbook (header at row 6, as in the template)
write_design <- function(df, sheet = AI_DESIGN_SHEET, path = tempfile(fileext = ".xlsx")) {
  wb <- openxlsx::createWorkbook(); openxlsx::addWorksheet(wb, sheet)
  openxlsx::writeData(wb, sheet, paste("instruction", 1:5), startRow = 1, colNames = FALSE)
  openxlsx::writeData(wb, sheet, df, startRow = 6)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE); path
}
design_df <- function(...) {
  base <- data.frame(plate = "PlateA", well = c("A1", "B1", "C1", "A2"),
                     sample_type = c("S1", "S2", "B", "X"),
                     subject_id = c(NA, NA, NA, "305"), group1 = c(NA, NA, NA, "NP"),
                     group2 = NA_character_, timepoint = c(NA, NA, NA, "S1"),
                     source = c("NIBSC", "NIBSC", "PBS", NA),
                     dilution = c("100", "1:200", "1", "10000"), stringsAsFactors = FALSE)
  mod <- list(...); for (n in names(mod)) base[[n]] <- mod[[n]]; base
}
check <- function(df, inv = make_inv()) ai_design_check(
  ai_design_read(write_design(df)), inv)
errs <- function(chk) chk$issues$message[chk$issues$severity == "error"]

test_that("template: two tabs, prefilled with the loaded wells, header findable", {
  skip_if_no_openxlsx()
  inv <- make_inv(); inv$instrument_dilution[1] <- 2500; inv$instrument_source[1] <- "Ref"
  p <- tempfile(fileext = ".xlsx"); ai_design_write_template(inv, p)
  expect_equal(openxlsx::getSheetNames(p), c(AI_DESIGN_EXAMPLE, AI_DESIGN_SHEET))
  d <- ai_design_read(p)
  expect_equal(nrow(d), nrow(inv))
  expect_setequal(names(d), c(AI_DESIGN_COLS, ".row"))
  expect_equal(d$plate[1:3], rep("PlateA", 3)); expect_equal(d$well[1:3], c("A1", "B1", "C1"))
  expect_equal(d$sample_type[1], "X")
  expect_equal(d$source[1], "Ref"); expect_equal(d$dilution[1], "2500")
  expect_true(all(is.na(d$subject_id)))
  # the prefilled map validates structurally (it just lacks the required values)
  chk <- ai_design_check(d, inv)
  expect_true(all(chk$issues$column[chk$issues$severity == "error"] %in% c("subject_id", "timepoint")))
})

test_that("example tab is illustrative and complete", {
  skip_if_no_openxlsx()
  p <- tempfile(fileext = ".xlsx"); ai_design_write_template(make_inv(), p)
  ex <- openxlsx::read.xlsx(p, AI_DESIGN_EXAMPLE, startRow = 6)
  expect_equal(names(ex), AI_DESIGN_COLS); expect_equal(nrow(ex), 96)
  expect_true(all(c("X", "S1", "B", "C") %in% ex$sample_type))
})

test_that("a valid design: S/B/X rows pass, dilution ratios normalise", {
  skip_if_no_openxlsx()
  chk <- check(design_df())
  expect_false(ai_design_has_errors(chk))
  expect_equal(chk$design$dilution, c(100L, 200L, 1L, 10000L))
  expect_equal(chk$design$sample_type, c("S", "S", "B", "X"))
  expect_equal(chk$design$type_suffix, c("1", "2", "", ""))
})

test_that("numbers typed as numbers in Excel read back as clean text", {
  skip_if_no_openxlsx()
  df <- design_df(); df$dilution <- c(100, 200, 1, 100000); df$subject_id <- c(NA, NA, NA, 305)
  chk <- check(df)
  expect_equal(chk$design$dilution[4], 100000L); expect_equal(chk$design$subject_id[4], "305")
})

test_that("required values follow the description parser's contract", {
  skip_if_no_openxlsx()
  # X without subject/timepoint, S without source, B without dilution
  df <- design_df(subject_id = NA_character_, timepoint = NA_character_,
                  source = c(NA, "NIBSC", "PBS", NA), dilution = c("100", "100", NA, "10000"))
  m <- errs(check(df))
  expect_true(any(grepl("type X needs a subject_id", m)))
  expect_true(any(grepl("type X needs a timepoint", m)))
  expect_true(any(grepl("type S needs a source", m)))
  expect_true(any(grepl("type B needs a dilution", m)))
  # X without dilution is only a warning (settled in the dilution step)
  w <- check(design_df(dilution = c("100", "100", "1", NA)))
  expect_false(ai_design_has_errors(w))
  expect_true(any(grepl("no sample dilution", w$issues$message[w$issues$severity == "warning"])))
})

test_that("bad plate, well, type, dilution and duplicates are errors naming the row", {
  skip_if_no_openxlsx()
  m <- errs(check(design_df(plate = c("PlateA", "Nope", "PlateA", "PlateA"))))
  expect_true(any(grepl("'Nope' is not one of the loaded plates", m)))
  m <- errs(check(design_df(well = c("A1", "B1", "Z99", "A2"))))
  expect_true(any(grepl("Well Z99 is not on plate", m)))
  m <- errs(check(design_df(sample_type = c("S1", "Q", "B", "X"))))
  expect_true(any(grepl("'Q' is not X, S, C or B", m)))
  m <- errs(check(design_df(dilution = c("100", "abc", "1", "10000"))))
  expect_true(any(grepl("'abc' is not a positive integer", m)))
  m <- errs(check(design_df(well = c("A1", "A1", "C1", "A2"))))
  expect_true(any(grepl("repeat a plate/well", m)))
  # the spreadsheet row is reported (header is row 6, so first data row is 7)
  chk <- check(design_df(dilution = c("100", "abc", "1", "10000")))
  expect_equal(chk$issues$row[chk$issues$severity == "error"][1], 8L)
})

test_that("plate may be given by its label; wells may be padded; case is ignored", {
  skip_if_no_openxlsx()
  chk <- check(design_df(plate = "label PlateA", well = c("a01", "B1", "c1", "A2"),
                         sample_type = c("s1", "s2", "b", "x")))
  expect_false(ai_design_has_errors(chk))
  expect_equal(chk$design$plate_key, rep("PlateA", 4)); expect_equal(chk$design$well[1], "A1")
})

test_that("plate names match loosely: file name, no xPONENT prefix, any case; ambiguity is refused", {
  skip_if_no_openxlsx()
  inv <- make_inv()
  inv$plate_key <- sub("Plate", "XPONENT_20221003_AGA_PLATE_", inv$plate_key)
  inv$source_file <- paste0("xPONENT_20221003_AGA_Plate_", c("A", "B")[inv$plate_index], ".csv")
  for (nm in c("20221003_AGA_Plate_A", "xPONENT_20221003_AGA_Plate_A.csv", "xponent_20221003_aga_plate_a"))
    expect_equal(check(design_df(plate = nm), inv)$design$plate_key[1], "XPONENT_20221003_AGA_PLATE_A")
  # one file feeding two plates: the file name alone cannot choose
  inv$source_file <- "same.csv"
  expect_true(any(grepl("not one of the loaded plates", errs(check(design_df(plate = "same.csv"), inv)))))
})

test_that("non-breaking spaces from copy-paste (as in the reference template) are ignored", {
  skip_if_no_openxlsx()
  chk <- check(design_df(plate = " PlateA ", sample_type = c("S1 ", "S2", "B", "X")))
  expect_false(ai_design_has_errors(chk))
  expect_equal(chk$design$plate_key[1], "PlateA")
})

test_that("blank sample_type keeps the plate's type; wells without readings are skipped", {
  skip_if_no_openxlsx()
  inv <- make_inv(); inv$type_code[inv$well == "B1"] <- "S4"; inv$specimen_type[inv$well == "B1"] <- "S"
  inv$type_code[inv$well == "C1"] <- NA; inv$specimen_type[inv$well == "C1"] <- NA
  df <- design_df(sample_type = c("S1", NA, "B", "X"))
  chk <- check(df, inv)
  expect_false(ai_design_has_errors(chk))
  expect_equal(chk$design$sample_type[chk$design$well == "B1"], "S")   # kept
  expect_false("C1" %in% chk$design$well)                              # no readings
  expect_true(any(grepl("no readings", chk$issues$message[chk$issues$severity == "warning"])))
})

test_that("a long subject_id is flagged and truncated in the resolved table", {
  skip_if_no_openxlsx()
  long <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  chk <- check(design_df(subject_id = c(NA, NA, NA, long)))
  expect_true(any(grepl("longer than 15", chk$issues$message)))
  r <- ai_design_resolved(ai_design_apply_inventory(make_inv(), chk$design), chk$design)
  expect_equal(r$subject_id[r$well == "A2" & r$plateid == "PlateA"], substr(long, 1, 15))
})

test_that("apply: types written, index kept or set, origin marked, others untouched", {
  skip_if_no_openxlsx()
  inv <- make_inv()
  chk <- check(design_df())
  inv2 <- ai_design_apply_inventory(inv, chk$design)
  g <- function(w, col) inv2[[col]][inv2$plate_key == "PlateA" & inv2$well == w]
  expect_equal(g("A1", "type_code"), "S1"); expect_equal(g("B1", "type_code"), "S2")
  expect_equal(g("C1", "type_code"), "B");  expect_equal(g("A2", "type_code"), "X")
  expect_equal(g("A1", "specimen_type"), "S")
  expect_equal(g("A1", "type_origin"), "user"); expect_equal(g("A2", "type_origin"), "file")  # X stayed X
  expect_true(all(inv2$type_code[inv2$plate_key == "PlateB"] == "X"))                          # other plate untouched
  # S without an index keeps an existing one
  inv$type_code[inv$plate_key == "PlateA" & inv$well == "A1"] <- "S7"
  inv$specimen_type[inv$plate_key == "PlateA" & inv$well == "A1"] <- "S"
  c3 <- check(design_df(sample_type = c("S", "S2", "B", "X")), inv)
  expect_equal(ai_design_apply_inventory(inv, c3$design)$type_code[1], "S7")
})

test_that("resolved identity has ai_resolve_inventory()'s shape and the right values", {
  skip_if_no_openxlsx()
  chk <- check(design_df(group2 = c(NA, NA, NA, "G2")))
  inv <- ai_design_apply_inventory(make_inv(), chk$design)
  r <- ai_design_resolved(inv, chk$design)
  expect_equal(nrow(r), 4)
  ref_cols <- c("plateid", "well", "specimen_type", "type_code", "description", "shape_key",
                "subject_id", "timepoint_tissue_abbreviation", "specimen_dilution_factor",
                "specimen_source", "groupa", "groupb", "biosample_id_barcode")
  expect_equal(names(r), ref_cols)
  x <- r[r$well == "A2", ]
  expect_equal(c(x$subject_id, x$timepoint_tissue_abbreviation, x$groupa, x$groupb,
                 x$specimen_dilution_factor), c("305", "S1", "NP", "G2", "10000"))
  s <- r[r$well == "B1", ]
  expect_equal(c(s$specimen_source, s$specimen_dilution_factor, s$biosample_id_barcode), c("NIBSC", "200", "2"))
  expect_equal(s$subject_id, "2")      # non-sample wells take their index, as the resolver does
  expect_type(r$specimen_dilution_factor, "integer")
  # and it overlays a rules-resolved table, design winning per well
  rules <- r[0, ]; rules[1:2, ] <- NA
  rules$plateid <- c("PlateA", "PlateB"); rules$well <- c("A1", "A1"); rules$subject_id <- c("old", "keep")
  o <- ai_design_overlay_resolved(rules, r)
  expect_equal(sum(o$plateid == "PlateA" & o$well == "A1"), 1)
  expect_equal(o$subject_id[o$plateid == "PlateB" & o$well == "A1"], "keep")
})

test_that("a well retyped on the grid after applying is released from the design", {
  skip_if_no_openxlsx()
  chk <- check(design_df())
  inv <- ai_design_apply_inventory(make_inv(), chk$design)
  expect_equal(sum(ai_design_coverage(inv, chk$design)), 4)
  inv <- ai_inventory_set(inv, "PlateA", "A2", type_code = "C")
  expect_equal(sum(ai_design_coverage(inv, chk$design)), 3)
  expect_false("A2" %in% ai_design_resolved(inv, chk$design)$well)
})

test_that("reading rejects a workbook without the tab or the header", {
  skip_if_no_openxlsx()
  expect_error(ai_design_read(write_design(design_df(), sheet = "other")), "no tab named 'current_plates_map'")
  p <- tempfile(fileext = ".xlsx")
  wb <- openxlsx::createWorkbook(); openxlsx::addWorksheet(wb, AI_DESIGN_SHEET)
  openxlsx::writeData(wb, AI_DESIGN_SHEET, data.frame(a = 1, b = 2)); openxlsx::saveWorkbook(wb, p)
  expect_error(ai_design_read(p), "header row")
  # header spelled the way the template's own text spells it
  df <- design_df(); names(df)[names(df) == "timepoint"] <- "timeperiod"
  expect_equal(check(df)$design$timepoint[4], "S1")
  # an empty table is reported, not crashed on
  e <- ai_design_check(ai_design_read(write_design(design_df()[0, ])), make_inv())
  expect_true(ai_design_has_errors(e))
})

test_that("with no plates loaded the check says to parse first", {
  skip_if_no_openxlsx()
  chk <- ai_design_check(ai_design_read(write_design(design_df())), NULL)
  expect_true(any(grepl("Parse the instrument", chk$issues$message)))
})

test_that("integrity: a clean design passes, and reports counts and what is left to the rules", {
  skip_if_no_openxlsx()
  chk <- check(design_df())
  inv <- ai_design_apply_inventory(make_inv(), chk$design)
  ig <- ai_design_integrity(inv, chk$design)
  expect_false(any(ig$issues$severity == "error"))
  expect_equal(ig$n_covered, 4L); expect_equal(ig$n_open, 12L)
  a <- ig$summary[ig$summary$plate_key == "PlateA", ]
  expect_equal(c(a$S, a$B, a$X, a$rules), c(2L, 1L, 1L, 4L))
  expect_equal(ig$summary$rules[ig$summary$plate_key == "PlateB"], 8L)
})

test_that("integrity: missing identity, instrument conflicts and curve order are caught", {
  skip_if_no_openxlsx()
  chk <- check(design_df())
  inv <- ai_design_apply_inventory(make_inv(), chk$design)
  # an instrument value that disagrees with the design
  i <- inv$plate_key == "PlateA" & inv$well == "A2"
  inv$instrument_dilution[i] <- 500
  inv$instrument_source <- ifelse(inv$plate_key == "PlateA" & inv$well == "B1", "SERUM", NA)
  ig <- ai_design_integrity(inv, chk$design)
  expect_true(any(grepl("differs from the one in the instrument file", ig$issues$message)))
  expect_true(any(grepl("source that differs", ig$issues$message)))
  expect_false(any(ig$issues$severity == "error"))
  # a design whose sample lost its subject
  dn <- chk$design; dn$subject_id[dn$well == "A2"] <- NA
  ig2 <- ai_design_integrity(inv, dn)
  expect_true(any(ig2$issues$severity == "error" & ig2$issues$column == "subject_id"))
  # standards that get more concentrated with the point number
  dn <- chk$design; dn$dilution[dn$well == "A1"] <- 9000L
  ig3 <- ai_design_integrity(inv, dn)
  expect_true(any(grepl("not increasing", ig3$issues$message)))
  # one subject in two groups
  dn <- chk$design; x <- dn[dn$well == "A2", ]; x$well <- "B2"; x$group1 <- "OTHER"
  dn <- rbind(dn, x)
  inv2 <- ai_design_apply_inventory(make_inv(), dn)
  ig4 <- ai_design_integrity(inv2, dn)
  expect_true(any(grepl("more than one group1", ig4$issues$message)))
})

test_that("integrity: no design, nothing to check", {
  ig <- ai_design_integrity(make_inv(), NULL)
  expect_equal(nrow(ig$issues), 0); expect_equal(ig$n_covered, 0L)
})
