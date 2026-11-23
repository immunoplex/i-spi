# =============================================================================
# test-assay-control-plates.R
# -----------------------------------------------------------------------------
# Plates that compare reagent batches: QC samples run as test samples (X) are
# moved to controls by description pattern, take their Source from the plate
# name, and the batch loads without test samples once the user confirms it has
# none. Also: sampleid from the well's description.
#
# The plate names and descriptions are those of the 2026-11-19/20 RPE supplier
# comparison (Jackson vs Southern; high / low reader voltage; half "ON JHM").
#
# Run from src/:  Rscript -e "testthat::test_file('test-assay-control-plates.R')"
# =============================================================================

library(testthat)
`%||%` <- function(a, b) if (is.null(a)) b else a
for (f in c("assay_description_parse.R", "assay_well_inventory.R", "assay_shape_rules.R",
            "assay_design_map.R"))
  if (!file.exists(f)) stop("run from src/") else suppressWarnings(source(f))

PLATE_FILES <- c(
  "2026_11_19_RPE_supplier_jackson_high.rbx", "2026_11_19_RPE_supplier_jackson_low.rbx",
  "2026_11_19_RPE_supplier_southern_high.rbx", "2026_11_19_RPE_supplier_southern_low.rbx",
  "2026_11_20_RPE_supplie_jackson ON JHM_high.rbx", "2026_11_20_RPE_supplie_jackson ON JHM_low.rbx",
  "2026_11_20_RPE_supplie_southern ON JHM_high.rbx", "2026_11_20_RPE_supplie_southern ON JHM_low.rbx")

# two plates laid out like the real ones: Jackson uses columns 6-10 (here 6-7),
# Southern columns 1-5 (here 1-2); every other well is empty
make_inv <- function() {
  one <- function(pk, file, cols) {
    w <- c(paste0(c("A", "B", "C", "D"), cols[1]), paste0(c("A", "B", "C", "D"), cols[2]),
           "A12", "B12")
    occupied <- seq_along(w) <= 8
    tc <- c("B", "S1", "S2", "S3", "X1", "X2", "X5", "X6", NA, NA)
    desc <- c("", "Inhouse Ref 1:2952450", "Inhouse Ref 1:984150", "Inhouse Ref 1:328050",
              "QC1 (Orange) 1:50", "QC1 (Orange) 1:500", "QC2 (Green) 1:50", "QC2 (Green) 1:500",
              NA, NA)
    data.frame(plate_key = pk, plate_label = pk, plate_index = 1L,
               well = w, well_raw = w, row_letter = substr(w, 1, 1),
               col_number = as.integer(substring(w, 2)),
               type_code = tc, specimen_type = substr(tc, 1, 1),
               description = desc, raw_type_code = tc, raw_description = desc,
               source_file = file, response_hint = NA_real_,
               type_origin = ifelse(occupied, "file", NA), desc_origin = "file",
               instrument_dilution = c(1, 1, 1, 1, 50, 500, 50, 500, NA, NA),
               instrument_source = NA_character_, stringsAsFactors = FALSE)
  }
  rbind(one("JACKSON_HIGH", PLATE_FILES[1], c(6, 7)),
        one("SOUTHERN_LOW.ON.JHM", PLATE_FILES[8], c(1, 2)))
}

test_that("sampleid comes from the description: ratio dropped, letters and digits, 15 chars", {
  f <- ai_sampleid_from_description
  expect_equal(f("QC1 (Orange) 1:50"), "QC1Orange")
  expect_equal(f("QC2 (Green) 1:5000"), "QC2Green")
  expect_equal(f("Inhouse Ref 1:2952450"), "InhouseRef")
  expect_equal(f("051 V1"), "051V1")                       # no ratio: nothing removed
  expect_equal(f("PT-0042_D28 1/100"), "PT0042D28")         # 1/100 is a ratio too
  expect_equal(f("BC123456789012345678"), "BC1234567890123")  # cut to 15
  expect_equal(f(c("", NA, "1:50"), c("3", "4", "")), c("3", "4", ""))   # fall back
  expect_equal(f("Sample 11:30"), "Sample1130")             # "11:30" is not a 1:N ratio
})

test_that("different descriptions that collapse to one sampleid are reported", {
  cl <- ai_sampleid_clashes(c("C", "C", "C", "C"),
                            c("QC1 (Orange) 1:50", "QC1 (Orange) 1:500", "QC1-Orange", "QC2 (Green) 1:50"),
                            c("QC1Orange", "QC1Orange", "QC1Orange", "QC2Green"))
  expect_equal(nrow(cl), 1L)                    # the dilutions of one control are not a clash
  expect_equal(cl$sampleid, "QC1Orange")
  expect_setequal(strsplit(cl$descriptions, " | ", fixed = TRUE)[[1]], c("QC1 (Orange)", "QC1-Orange"))
  expect_equal(nrow(ai_sampleid_clashes("X", "a", "a")), 0L)
})

test_that("description patterns select QC wells on every plate, at every dilution", {
  inv <- make_inv()
  m <- ai_inventory_match_pattern(inv, "QC1*, qc2*")
  expect_equal(nrow(m), 8L)                     # 4 QC wells on each of 2 plates
  expect_setequal(unique(m$plate_key), c("JACKSON_HIGH", "SOUTHERN_LOW.ON.JHM"))
  expect_equal(nrow(ai_inventory_match_pattern(inv, "QC1*")), 4L)
  expect_equal(nrow(ai_inventory_match_pattern(inv, "Orange")), 0L)   # whole description
  expect_equal(nrow(ai_inventory_match_pattern(inv, "*Orange*")), 4L)
  expect_equal(nrow(ai_inventory_match_pattern(inv, "  ")), 0L)
})

test_that("Source from the plate name: the parts that differ, without the typo or the date", {
  r <- ai_source_from_plate_names(PLATE_FILES)
  expect_equal(unname(r$source), c("jackson_high", "jackson_low", "southern_high", "southern_low",
                                   "jackson_ON_JHM_high", "jackson_ON_JHM_low",
                                   "southern_ON_JHM_high", "southern_ON_JHM_low"))
  p <- r$parts
  expect_false(any(p$proposed[p$part %in% c("supplier", "supplie", "19", "20", "2026", "RPE")]))
  # the user's own choice of parts wins
  r2 <- ai_source_from_plate_names(PLATE_FILES, keep = c("jackson", "southern", "high", "low"))
  expect_equal(unname(r2$source[5]), "jackson_high")
  # cut to the 25-character column
  long <- ai_source_from_plate_names(c("a_averyveryverylongsupplier_name_one", "a_b"))
  expect_true(all(nchar(long$source) <= AI_SOURCE_MAX))
  # a single plate has nothing that differs
  expect_equal(unname(ai_source_from_plate_names(PLATE_FILES[1])$source), "")
})

test_that("moving QC wells to controls with a Source, on every plate", {
  inv <- make_inv()
  m <- ai_inventory_match_pattern(inv, "QC1*, QC2*")
  src <- ai_source_from_plate_names(unique(inv$source_file))$source
  for (p in unique(m$plate_key)) {
    file <- inv$source_file[match(p, inv$plate_key)]
    inv <- ai_inventory_set(inv, p, m$well[m$plate_key == p], type_code = "C",
                            source = src[[file]])
    expect_equal(attr(inv, "n_set"), 4L)
  }
  qc <- inv[grepl("^QC", inv$description), ]
  expect_true(all(qc$specimen_type == "C"))
  expect_equal(unique(qc$instrument_source[qc$plate_key == "JACKSON_HIGH"]), "jackson_high")
  expect_equal(unique(qc$instrument_source[qc$plate_key == "SOUTHERN_LOW.ON.JHM"]),
               "southern_ON_JHM_low")
  # no test samples are left; the confirmation turns the error into a warning
  expect_true(ai_inventory_lacks_test_samples(inv))
  req <- ai_inventory_requirements(inv)
  expect_true(any(req$severity == "error" & grepl("no test samples", req$message)))
  ok <- ai_inventory_requirements(inv, allow_no_test_samples = TRUE)
  expect_false(any(ok$severity == "error"))
  expect_true(any(ok$severity == "warning" & grepl("accepted", ok$message)))
  expect_true(ai_inventory_types_resolved(inv, allow_no_test_samples = TRUE))
  expect_false(ai_inventory_types_resolved(inv))
})

test_that("same wells on every plate leaves the other plates' empty wells alone", {
  inv <- make_inv()
  # A6 is occupied on the Jackson plate and empty (absent) on the Southern one;
  # A1 is empty on Jackson and occupied (blank) on Southern
  inv <- ai_inventory_set(inv, NULL, c("A6", "A1", "A12"), type_code = "C",
                          skip_empty_except = "JACKSON_HIGH")
  j <- inv[inv$plate_key == "JACKSON_HIGH", ]; s <- inv[inv$plate_key != "JACKSON_HIGH", ]
  expect_equal(j$specimen_type[j$well == "A6"], "C")
  expect_equal(j$specimen_type[j$well == "A12"], "C")     # current plate: as selected
  expect_equal(s$specimen_type[s$well == "A1"], "C")      # occupied on the other plate
  expect_true(is.na(s$specimen_type[s$well == "A12"]))    # empty there: left alone
})

test_that("a user Source overrides the description rule; \"\" clears it", {
  inv <- make_inv()
  inv <- ai_inventory_set(inv, "JACKSON_HIGH", "A7", source = "  jackson_high ")
  expect_equal(inv$instrument_source[inv$plate_key == "JACKSON_HIGH" & inv$well == "A7"], "jackson_high")
  inv <- ai_inventory_set(inv, "JACKSON_HIGH", "A7", source = "")
  expect_true(is.na(inv$instrument_source[inv$plate_key == "JACKSON_HIGH" & inv$well == "A7"]))
  inv <- ai_inventory_set(inv, "JACKSON_HIGH", "A7", source = strrep("x", 40))
  expect_equal(nchar(inv$instrument_source[inv$plate_key == "JACKSON_HIGH" & inv$well == "A7"]), 25L)
  # and it wins over what the description rule binds to Source
  rule <- ai_shape_rule("C", " ", "format", list(), NULL, list())
  key <- ai_shape_key_one("QC1 (Orange) 1:50", rule)
  rule$shapes[[key]] <- list(Source = ai_binding("slot", slot = 1L),
                             DilutionFactor = ai_binding("slot", slot = 3L))
  r <- ai_resolve_one("QC1 (Orange) 1:50", "C1", rule, instrument_source = "jackson_high")
  expect_equal(r$values[["Source"]], "jackson_high")
})

test_that("the resolver stores the description-based sampleid, keeps the index as patientid", {
  inv <- make_inv()
  inv <- inv[!is.na(inv$specimen_type), ]
  m <- ai_inventory_match_pattern(inv, "QC*")
  inv$type_code[inv$well %in% m$well] <- "C1"; inv$specimen_type[inv$well %in% m$well] <- "C"
  rs <- ai_ruleset_init(inv)          # no bindings needed: sampleid does not depend on them
  res <- ai_resolve_inventory(inv, rs)$resolved
  s1 <- res[res$type_code == "S1", ][1, ]
  expect_equal(s1$biosample_id_barcode, "InhouseRef")
  qc <- res[res$specimen_type == "C", ]
  expect_setequal(unique(qc$biosample_id_barcode), c("QC1Orange", "QC2Green"))
  expect_true(all(qc$subject_id == "1"))          # non-sample patientid: the type index
  b <- res[res$specimen_type == "B", ][1, ]
  expect_equal(b$biosample_id_barcode, "")         # no description, no index -> well fallback later
})
