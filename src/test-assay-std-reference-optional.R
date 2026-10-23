# =============================================================================
# test-assay-std-reference-optional.R
# -----------------------------------------------------------------------------
# Step 3 ("Standards dilution reference") is OPTIONAL. Its candidate scan only
# recognises a 1:N ratio, so a lab that writes the dilution into the Standard's
# description as a plain number ("NIBSCO_100") used to be stopped at step 3 even
# though step 4 reads that number fine. These tests pin down:
#   1. why the step used to block (the scan lists such descriptions),
#   2. that the step no longer blocks (ready() follows the grid, not the scan),
#   3. that step 4 is still the real gate: a Standard whose dilution is nowhere
#      (text, instrument, reference table) is still rejected, and entering it in
#      the reference table still fixes it.
#
# Run: testthat::test_file("src/test-assay-std-reference-optional.R")
# =============================================================================

library(testthat)
library(shiny)

if (!exists("ai_std_reference_server")) {
  root <- c(".", "src", "..", "../src", "../..", "../../src")
  hit  <- root[file.exists(file.path(root, "assay_std_reference_ui.R"))][1]
  if (is.na(hit)) stop("cannot find src/ -- run from the repo root")
  for (f in c("assay_description_parse.R", "assay_well_inventory.R",
              "assay_shape_rules.R", "assay_std_reference_rules.R",
              "assay_std_reference_ui.R"))
    source(file.path(hit, f))
}

# A raw-bead-shaped preview: one row per occupied well. `std_desc` is what the
# four Standards carry in their Description column.
make_inventory <- function(std_desc) {
  wells <- c(paste0("A", 1:4), paste0("B", 1:3), "C1")
  pv <- data.frame(
    Well        = wells,
    Type        = c(paste0("S", 1:4), rep("X", 3), "B"),
    Description = c(std_desc, "1001_V1_100", "1002_V1_100", "1003_V2_100", "PBS_1"),
    plateid     = "plate1", source_file = "plate1.xlsx",
    stringsAsFactors = FALSE)
  ai_well_inventory(list(preview = pv), "bead", n_wells = 96)
}

std_int   <- c("NIBSCO_100", "NIBSCO_200", "NIBSCO_400", "NIBSCO_800")  # dilution in text
std_none  <- rep("NIBSCO", 4)                                            # no dilution anywhere

test_that("the scan still lists plain-integer dilutions (why step 3 used to block)", {
  inv <- make_inventory(std_int)
  expect_setequal(ai_std_reference_candidates(inv, "S")$description, std_int)
})

test_that("step 3 never blocks: ready() follows enabled(), not the candidate scan", {
  inv <- make_inventory(std_int)
  inv_rv <- reactiveVal(inv)
  en     <- reactiveVal(TRUE)
  testServer(ai_std_reference_server,
             args = list(inventory_rv = inv_rv, pool = NULL,
                         scope = reactive(list()), enabled = en), {
    expect_gt(nrow(candidates()), 0L)   # the scan still has something to say...
    expect_true(ready())                # ...but the step is not a stop sign
    en(FALSE)                           # ready only once the layout is confirmed
    expect_false(ready())
  })
})

test_that("the entry table is rolled up by default; the status note is always visible", {
  skip_if_not_installed("shinyBS")
  suppressMessages(library(shinyBS))
  html <- as.character(ai_std_reference_ui("stdref"))
  at   <- function(p) regexpr(p, html, fixed = TRUE)[1]
  title <- "Enter standard dilutions manually (optional)"

  expect_true(at("stdref-status") > 0)                       # status is not inside the panel
  expect_lt(at("stdref-status"), at("stdref-manual_collapse"))
  expect_gt(at("stdref-table"), at(title))                   # table, save, paste all live inside it
  expect_gt(at("stdref-save"),  at(title))
  expect_gt(at("stdref-paste_collapse"), at("stdref-save"))
  # a bsCollapsePanel is open only when its body carries the `in` class
  bodies <- regmatches(html, gregexpr('class="panel-collapse collapse[^"]*"', html))[[1]]
  expect_length(bodies, 2L)
  expect_false(any(grepl("collapse in", bodies, fixed = TRUE)))
})

test_that("step 4 resolves plain-integer dilutions with no reference table at all", {
  inv   <- make_inventory(std_int)
  rules <- ai_ruleset_init(inv)
  res   <- ai_resolve_inventory(inv, rules)
  expect_equal(nrow(res$issues), 0L)
  s <- res$resolved[res$resolved$specimen_type == "S", ]
  expect_equal(sort(s$specimen_dilution_factor), c(100, 200, 400, 800))
  expect_true(all(s$specimen_source == "NIBSCO"))
})

test_that("step 4 is still the gate when a Standard's dilution is genuinely missing", {
  inv   <- make_inventory(std_none)
  rules <- ai_ruleset_init(inv)

  res <- ai_resolve_inventory(inv, rules)                      # nothing supplies it
  expect_gt(nrow(res$issues), 0L)
  expect_true(any(grepl("DilutionFactor", res$issues$column)))

  ref <- data.frame(specimen_type = "S", description = "NIBSCO", dilution = 50,
                    stringsAsFactors = FALSE)                  # optional step 3 fixes it
  res2 <- ai_resolve_inventory(inv, rules, reference = ref)
  expect_equal(nrow(res2$issues), 0L)
  expect_true(all(res2$resolved$specimen_dilution_factor[
    res2$resolved$specimen_type == "S"] == 50))
})

# ---- which candidates genuinely need an entry -------------------------------
# The scan can't see delimiters yet, so it separates "a plain number is written
# in the text (step 4 reads it)" from "no number anywhere (only step 3 can
# supply it)" -- the second group is what makes Step 3 actually needed.

test_that("has_number: a standalone whole number counts; letters-glued or decimal does not", {
  has <- .ai_std_has_number
  expect_true(all(has(c("NIBSCO_100", "Std 3 400", "Pool 2 (Mid)", "STD-12-2500", "7"))))
  expect_false(any(has(c("STD3", "QC1 (Low)", "Std 0.5", "Inhouse Ref", "Reference",
                         "S1", "1.5"))))
})

test_that("candidates are flagged per description, and saved ones stay excluded", {
  descs <- c("NIBSCO_100", "NIBSCO_200", "Inhouse Ref", "Reference")
  inv <- make_inventory(descs)
  cd  <- ai_std_reference_candidates(inv, "S")
  expect_true("has_number" %in% names(cd))
  expect_equal(cd$has_number[match(descs, cd$description)], c(TRUE, TRUE, FALSE, FALSE))
  saved <- data.frame(description = "Reference", dilution = 50, stringsAsFactors = FALSE)
  expect_false("Reference" %in% ai_std_reference_candidates(inv, "S", saved = saved)$description)
})

test_that("status separates 'needs your input' (no number) from 'optional' (number in text)", {
  inv_rv <- reactiveVal(make_inventory(c("NIBSCO_100", "NIBSCO_200", "Inhouse Ref", "Reference")))
  testServer(ai_std_reference_server,
             args = list(inventory_rv = inv_rv, pool = NULL,
                         scope = reactive(list()), enabled = reactive(TRUE)), {
    expect_equal(n_required(), 2L)
    html <- as.character(output$status$html)
    expect_match(html, "Needs your input: 2", fixed = TRUE)
    expect_match(html, "Optional: 2", fixed = TRUE)
    expect_true(ready())                       # still never a stop sign
    # rows that need a value are listed first in the entry table
    expect_setequal(staged_rv()$description[1:2], c("Inhouse Ref", "Reference"))
  })
})

test_that("the numbers-only case stays purely optional: nothing red, nothing required", {
  inv_rv <- reactiveVal(make_inventory(std_int))
  testServer(ai_std_reference_server,
             args = list(inventory_rv = inv_rv, pool = NULL,
                         scope = reactive(list()), enabled = reactive(TRUE)), {
    expect_equal(n_required(), 0L)
    html <- as.character(output$status$html)
    expect_false(grepl("Needs your input", html, fixed = TRUE))
    expect_match(html, "Optional: 4", fixed = TRUE)
  })
})
