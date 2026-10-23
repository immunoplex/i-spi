# =============================================================================
# test-assay-shape-approve.R
# -----------------------------------------------------------------------------
# The "approve this group" control in the description-rules step (step 4).
#
# FAULT C: ticking approve while results were still being recomputed sent the
# section into an endless refresh. The control used to be a checkbox INPUT the
# server pushed values into (and later redrew); the browser reports a pushed or
# freshly drawn checkbox back as if it were a click, so one badly-timed click
# left browser and server correcting each other forever. Approval is now an
# EVENT ("group K was ticked/unticked") sent only by a user click, and the box
# is drawn only once the group fully resolves.
#
# The endless loop itself is a browser/server timing effect and was reproduced
# and fixed with a real browser (see the commit message); these tests pin down
# the parts that can run without one:
#   * the box is plain markup with NO Shiny input id (so it can never be
#     reported back by a redraw) and names the group it belongs to,
#   * it is withheld until the group resolves completely,
#   * approval events are idempotent, group-specific, and refused for a group
#     that doesn't resolve or doesn't exist.
#
# Run: testthat::test_file("src/test-assay-shape-approve.R")
# =============================================================================

library(testthat)
library(shiny)

if (!exists("ai_shape_server")) {
  root <- c(".", "src", "..", "../src", "../..", "../../src")
  hit  <- root[file.exists(file.path(root, "assay_shape_ui.R"))][1]
  if (is.na(hit)) stop("cannot find src/ -- run from the repo root")
  for (f in c("assay_description_parse.R", "assay_well_inventory.R",
              "assay_shape_rules.R", "assay_design_map.R", "assay_shape_ui.R"))
    source(file.path(hit, f))
}

make_inventory <- function(std_desc) {
  wells <- c(paste0("A", 1:4), paste0("B", 1:3), "C1")
  pv <- data.frame(
    Well = wells, Type = c(paste0("S", 1:4), rep("X", 3), "B"),
    Description = c(std_desc, "1001_V1_100", "1002_V1_100", "1003_V2_100", "PBS_1"),
    plateid = "plate1", source_file = "plate1.xlsx", stringsAsFactors = FALSE)
  ai_well_inventory(list(preview = pv), "bead", n_wells = 96)
}


std_ok  <- c("NIBSCO_100", "NIBSCO_200", "NIBSCO_400", "NIBSCO_800")  # resolves by itself
std_bad <- rep("NIBSCO", 4)                                           # no dilution anywhere

test_that("a resolved group gets a plain approve box: no Shiny id, tied to its group", {
  inv_rv <- reactiveVal(make_inventory(std_ok))
  testServer(ai_shape_server, args = list(inventory_rv = inv_rv, enabled = reactive(TRUE)), {
    session$flushReact()
    k <- state_of("S")$shapes$shape_key[1]
    html <- as.character(output$approve_ui_S$html)
    expect_match(html, 'type="checkbox"', fixed = TRUE)
    expect_match(html, "ai-approve-chk", fixed = TRUE)
    expect_false(grepl('id="', html, fixed = TRUE))               # not a Shiny input
    expect_match(html, paste0('data-key="', k, '"'), fixed = TRUE)
    expect_match(html, "approve_evt_S", fixed = TRUE)             # reports via the event
    expect_false(grepl("\\schecked(=|\\s|>)", html))           # starts unapproved
  })
})

test_that("a group that does not resolve gets a hint, not an approve box", {
  inv_rv <- reactiveVal(make_inventory(std_bad))
  testServer(ai_shape_server, args = list(inventory_rv = inv_rv, enabled = reactive(TRUE)), {
    session$flushReact()
    html <- as.character(output$approve_ui_S$html)
    expect_false(grepl('type="checkbox"', html, fixed = TRUE))
    expect_match(html, "once every row in the Result table says yes", fixed = TRUE)
  })
})

test_that("approval events are idempotent and apply to the group they name", {
  inv_rv <- reactiveVal(make_inventory(std_ok))
  testServer(ai_shape_server, args = list(inventory_rv = inv_rv, enabled = reactive(TRUE)), {
    session$flushReact()
    k <- state_of("S")$shapes$shape_key[1]
    expect_false(is_approved("S", k))

    session$setInputs(approve_evt_S = list(key = k, want = TRUE, n = 1))
    expect_true(is_approved("S", k))
    session$setInputs(approve_evt_S = list(key = k, want = TRUE, n = 2))   # repeat: no change
    expect_true(is_approved("S", k))
    # a redraw that carries the approved state in its markup, still no input id
    html <- as.character(output$approve_ui_S$html)
    expect_true(grepl("\\schecked(=|\\s|>)", html))

    session$setInputs(approve_evt_S = list(key = k, want = FALSE, n = 3))
    expect_false(is_approved("S", k))
  })
})

test_that("events for an unresolved or unknown group are refused", {
  inv_rv <- reactiveVal(make_inventory(std_bad))
  testServer(ai_shape_server, args = list(inventory_rv = inv_rv, enabled = reactive(TRUE)), {
    session$flushReact()
    k <- state_of("S")$shapes$shape_key[1]
    session$setInputs(approve_evt_S = list(key = k, want = TRUE, n = 1))   # does not resolve
    expect_false(is_approved("S", k))
    session$setInputs(approve_evt_S = list(key = "no-such-group", want = TRUE, n = 2))
    expect_false(is_approved("S", "no-such-group"))
  })
})

# ---- step 4 points back to step 3 -------------------------------------------
test_that("a Standards group missing its dilution says where to fix it", {
  inv_rv <- reactiveVal(make_inventory(std_bad))
  testServer(ai_shape_server, args = list(inventory_rv = inv_rv, enabled = reactive(TRUE)), {
    session$flushReact()
    expect_match(as.character(output$verdict_S$html), "step 3", fixed = TRUE)
  })
})

test_that("a resolved Standards group shows no such hint", {
  inv_rv <- reactiveVal(make_inventory(std_ok))
  testServer(ai_shape_server, args = list(inventory_rv = inv_rv, enabled = reactive(TRUE)), {
    session$flushReact()
    expect_false(grepl("step 3", as.character(output$verdict_S$html), fixed = TRUE))
  })
})
