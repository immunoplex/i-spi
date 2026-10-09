# =============================================================================
# test-assay-well-inventory.R
# -----------------------------------------------------------------------------
# Regression test for a bug found 2026-10-09: uploading 2+ .rbx/.srbx files
# whose derived plate_key (clean_plate_id() of the filename) collided caused
# ai_well_inventory()'s (plate_key, well) dedup to treat the second plate's
# wells as duplicates of the first's and silently drop them -- the Standards
# dilution reference step then had nothing to scan for the dropped plate.
# Fixed at two layers: reader_bead_rbx.R disambiguates the per-file identity
# up front when two uploads share a literal filename, and ai_well_inventory()
# disambiguates plate_key whenever more than one distinct source_file maps to
# the same plate_key (covers the ELISA/flow adapters too, and any plateid
# collision that isn't a literal same-filename case).
#
# Run: testthat::test_file("test-assay-well-inventory.R")
# =============================================================================

if (!exists("ai_well_inventory")) {
  cand <- c("assay_well_inventory.R", "src/assay_well_inventory.R",
            "../../assay_well_inventory.R", "../../../src/assay_well_inventory.R")
  hit <- cand[file.exists(cand)][1]
  if (is.na(hit)) stop("cannot find assay_well_inventory.R; edit the path in this test")
  source(hit)
}

library(testthat)

# Two synthetic "plates" (as the bead adapter would see them after
# process_rbx_files()/dplyr::bind_rows()) that collide on plate_key (plateid)
# but come from two distinct source files -- the scenario this test guards.
mk_bead_preview <- function(plateid, source_file, wells, types, descs) {
  data.frame(plateid = plateid, Well = wells, Type = types, Description = descs,
            source_file = source_file, stringsAsFactors = FALSE)
}

test_that("ai_well_inventory() disambiguates a plate_key collision across two source files instead of dropping the second plate", {
  p1 <- mk_bead_preview("PLATE_A", "fileA.rbx",
                        c("A1", "A2"), c("S1", "X"), c("Std 1", "Sample 1"))
  p2 <- mk_bead_preview("PLATE_A", "fileB.rbx",   # same plateid, different file
                        c("A1", "A2"), c("S1", "X"), c("Std 1", "Sample 2"))
  raw <- list(preview = rbind(p1, p2), n_wells = 2L)

  expect_warning(
    inv <- ai_well_inventory(raw, "bead", n_wells = 6L),
    "disambiguated by filename"
  )

  expect_equal(length(unique(inv$plate_key)), 2L)       # disambiguated, not collapsed to 1
  expect_equal(sum(!is.na(inv$type_code)), 4L)           # both plates' 2 occupied wells each, not 2
  expect_setequal(inv$description[!is.na(inv$specimen_type) & inv$specimen_type == "X"],
                  c("Sample 1", "Sample 2"))             # both samples present, not one dropped
})

test_that("ai_well_inventory() still dedupes a genuine duplicate (plate, well) row within ONE file", {
  df <- mk_bead_preview("PLATE_A", "fileA.rbx",
                        c("A1", "A1", "A2"),             # A1 reported twice by the same file
                        c("S1", "S1", "X"),
                        c("Std 1", "Std 1", "Sample 1"))
  raw <- list(preview = df, n_wells = 6L)

  inv <- ai_well_inventory(raw, "bead", n_wells = 6L)

  expect_equal(length(unique(inv$plate_key)), 1L)        # one plate, not disambiguated
  expect_equal(sum(!is.na(inv$type_code)), 2L)            # one row per occupied well, not three
  expect_equal(attr(inv, "duplicate_wells"), 1L)          # the real duplicate is still reported
})
