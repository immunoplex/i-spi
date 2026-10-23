# =============================================================================
# test-assay-rollup-panels.R
# -----------------------------------------------------------------------------
# The optional parts of the import steps are rolled up: a visible heading and
# status line, with the controls inside a panel that starts CLOSED. These tests
# pin the structure so a refactor can't quietly re-expand them or bury a gate:
#   step 2  study design / edit selected wells / suggested types   (rolled up)
#           plate grid + Layout check                              (always open)
#   step 4  parse profile                                          (rolled up)
#           Description rules check                                (always open)
# (Step 3's panel is covered in test-assay-std-reference-optional.R.)
#
# Run: testthat::test_file("src/test-assay-rollup-panels.R")
# =============================================================================

library(testthat)
library(shiny)
suppressMessages(library(shinyBS))

if (!exists("ai_plate_grid_ui") || !exists("ai_shape_ui")) {
  root <- c(".", "src", "..", "../src", "../..", "../../src")
  hit  <- root[file.exists(file.path(root, "assay_plate_grid.R"))][1]
  if (is.na(hit)) stop("cannot find src/ -- run from the repo root")
  for (f in c("assay_description_parse.R", "assay_well_inventory.R",
              "assay_shape_rules.R", "assay_design_map.R",
              "assay_plate_grid.R", "assay_shape_ui.R"))
    source(file.path(hit, f))
}
# help_icon() lives in the app's help layer; the UI functions look it up in the
# global environment, so a stand-in has to be defined there.
if (!exists("help_icon", envir = globalenv()))
  assign("help_icon", function(...) NULL, envir = globalenv())

pos <- function(html, p) regexpr(p, html, fixed = TRUE)[1]
# bodies of bsCollapse panels; one carrying the `in` class would be open
collapse_bodies <- function(html)
  regmatches(html, gregexpr('class="panel-collapse collapse[^"]*"', html))[[1]]

test_that("step 2: three optional tools are rolled up and closed, each with a visible heading", {
  html <- as.character(ai_plate_grid_ui("grid"))
  for (id in c("design_collapse", "edit_collapse", "proposals_collapse"))
    expect_gt(pos(html, paste0("grid-", id)), 0)
  bodies <- collapse_bodies(html)
  expect_length(bodies, 3L)
  expect_false(any(grepl("collapse in", bodies, fixed = TRUE)))

  # each heading, and the status line it keeps, comes BEFORE its closed panel
  expect_lt(pos(html, "Assign the study design from a spreadsheet"), pos(html, "grid-design_collapse"))
  expect_lt(pos(html, "grid-design_status"),    pos(html, "grid-design_collapse"))
  expect_lt(pos(html, "Edit the selected wells"), pos(html, "grid-edit_collapse"))
  expect_lt(pos(html, "grid-selection_label"),  pos(html, "grid-edit_collapse"))
  expect_lt(pos(html, "Suggested specimen types"), pos(html, "grid-proposals_collapse"))
  expect_lt(pos(html, "grid-proposal_summary"), pos(html, "grid-proposals_collapse"))
  expect_gte(lengths(regmatches(html, gregexpr("(optional)", html, fixed = TRUE))), 3L)

  # the controls live INSIDE their panel
  expect_gt(pos(html, "grid-design_file"),     pos(html, "grid-design_collapse"))
  expect_gt(pos(html, "grid-apply_edit"),      pos(html, "grid-edit_collapse"))
  expect_gt(pos(html, "grid-apply_proposals"), pos(html, "grid-proposals_collapse"))
})

test_that("step 2: the plate grid and the Layout check are not rolled up", {
  html <- as.character(ai_plate_grid_ui("grid"))
  expect_lt(pos(html, "grid-grid_wrap"), pos(html, "grid-design_collapse"))   # grid first, open
  expect_gt(pos(html, "grid-gate"), pos(html, "grid-proposals_collapse"))     # gate after the roll-ups
  # nothing in the final 'Layout check' block is a collapse panel
  tail_html <- substr(html, pos(html, "Layout check"), nchar(html))
  expect_false(grepl("panel-collapse", tail_html, fixed = TRUE))
})

test_that("step 4: the parse profile is rolled up, its report stays visible, the check stays open", {
  html <- as.character(ai_shape_ui("shape"))
  expect_gt(pos(html, "shape-profile_collapse"), 0)
  expect_lt(pos(html, "shape-profile_report"), pos(html, "shape-profile_collapse"))
  expect_gt(pos(html, "shape-profile_dl"), pos(html, "shape-profile_collapse"))
  expect_gt(pos(html, "shape-profile_up"), pos(html, "shape-profile_collapse"))
  expect_false(any(grepl("collapse in", collapse_bodies(html), fixed = TRUE)))
  expect_gt(pos(html, "Description rules check"), pos(html, "shape-profile_collapse"))
  expect_gt(pos(html, "shape-overall"), pos(html, "Description rules check"))
})
