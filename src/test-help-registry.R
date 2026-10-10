# =============================================================================
# test-help-registry.R
# -----------------------------------------------------------------------------
# Sync checks for the app-wide help engine (help_utils.R + help/<dir>/*.md),
# per docs/help-system/assessment/04-architecture-recommendation.md §3:
#   (a) every literal help_icon("<id>", ...) call found in src/*.R has a
#       matching entry in the merged registry -- catches a help icon left
#       pointing at a note that was renamed or never written. (NOT checked
#       for settings_help_icon(), whose param_name argument is a runtime
#       variable, not a literal id -- statically ungreppable by design.)
#   (b) every authored note's `category` matches 05-help-content-mapping.csv's
#       `category` column for that help_id, where a mapping-table row exists
#       for it -- catches a note drifting out of sync with how it was
#       classified. Skipped (not failed) for an id with no mapping-table row.
#
# Integration-style, not pure-logic: this reads the LIVE src/*.R tree and the
# real help/ directories, unlike the other test-*.R files' synthetic
# fixtures. Run from src/ (same as the rest of the suite):
#   Rscript -e "setwd('src'); testthat::test_file('test-help-registry.R')"
# =============================================================================

if (!exists("load_help_merged")) source("help_utils.R")

library(testthat)

HELP_DIRS <- HELP_CONTENT_DIRS   # single source of truth, see help_utils.R

test_that("every note's schema_table: resolves in the schema registry (help/schema/*.yaml)", {
  reg    <- load_help_merged(HELP_DIRS)
  schema <- load_schema_registry("help/schema")
  missing <- character(0)
  for (id in names(reg)) {
    st <- reg[[id]]$schema_table
    if (is.null(st) || is.na(st) || !nzchar(st)) next
    if (!(st %in% names(schema)))
      missing <- c(missing, sprintf("%s -> schema_table: %s", id, st))
  }
  expect_equal(missing, character(0),
              info = sprintf("note(s) with a schema_table not in help/schema/: %s",
                             paste(missing, collapse = "; ")))
})

test_that("every help/schema/*.yaml file has the expected columns/indexes/referenced_by shape", {
  schema <- load_schema_registry("help/schema")
  bad <- character(0)
  for (tbl in names(schema)) {
    t <- schema[[tbl]]
    if (is.null(t$columns) || !length(t$columns)) { bad <- c(bad, paste0(tbl, ": no columns")); next }
    for (col in t$columns)
      if (is.null(col$name) || !nzchar(col$name))
        bad <- c(bad, paste0(tbl, ": a column row is missing 'name'"))
  }
  expect_equal(bad, character(0), info = paste(bad, collapse = "; "))
})

test_that("every literal help_icon(\"id\", ...) call in src/*.R resolves in the merged registry", {
  reg <- load_help_merged(HELP_DIRS)
  r_files <- list.files(".", pattern = "\\.R$", full.names = FALSE)
  r_files <- r_files[!grepl("^test-", r_files)]   # don't scan the tests themselves

  ids_called <- character(0)
  for (f in r_files) {
    txt <- tryCatch(readLines(f, warn = FALSE, encoding = "UTF-8"), error = function(e) character(0))
    hits <- regmatches(txt, regexpr('help_icon\\(\\s*"([A-Za-z0-9_.]+)"', txt, perl = TRUE))
    hits <- hits[nzchar(hits)]
    if (length(hits)) {
      ids <- sub('help_icon\\(\\s*"([A-Za-z0-9_.]+)".*', "\\1", hits, perl = TRUE)
      ids_called <- c(ids_called, ids)
    }
  }
  ids_called <- unique(ids_called)
  missing <- ids_called[!ids_called %in% names(reg)]
  expect_equal(missing, character(0),
              info = sprintf("help_icon() referenced but not in the registry: %s",
                             paste(missing, collapse = ", ")))
})

test_that("help_entry_for_id() resolves a direct id AND falls back to param-name indirection", {
  # Regression test for a real bug: settings_cascade_ui.R's one help_show
  # observer receives EITHER a direct help_id (from this page's own
  # help_icon()) OR a calib_settings param_name (from a per-row
  # settings_help_icon()) -- it has to resolve both from the same lookup.
  # Caught 2026-10-09: the observer still called the old param-only
  # settings_help_content()/settings_help_title(), so a direct-id click
  # silently did nothing (returned NULL, no modal, no error).
  tmp <- tempfile("helptest_"); dir.create(tmp)
  writeLines(c("---", "id: test.direct_id", "title: Direct", "---", "", "Body."),
            file.path(tmp, "a.md"))
  writeLines(c("---", "id: test.via_param", "title: Via param", "params: [some_param]",
              "---", "", "Body."), file.path(tmp, "b.md"))
  reg <- load_help_merged(tmp)

  expect_false(is.null(help_entry_for_id("test.direct_id", reg)))     # direct hit
  expect_false(is.null(help_entry_for_id("some_param", reg)))         # falls back to param lookup
  expect_true(is.null(help_entry_for_id("nonexistent", reg)))         # neither -- NULL, not an error

  unlink(tmp, recursive = TRUE)
})

test_that("every module with a help_show observer uses the generalized help_modal_body()/help_modal_title(), not the settings-only functions", {
  # The settings_help_content()/settings_help_title() pair only does
  # param-name indirection -- correct ONLY for a module that exclusively
  # wires settings_help_icon() (param-keyed) and never a direct help_icon()
  # call. A module using BOTH (settings_cascade_ui.R does) must use the
  # generalized pair so one observer resolves either kind of click.
  r_files <- list.files(".", pattern = "\\.R$", full.names = FALSE)
  r_files <- r_files[!grepl("^test-", r_files)]

  bad <- character(0)
  for (f in r_files) {
    txt <- tryCatch(paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
                    error = function(e) "")
    if (!grepl("observeEvent\\(input\\$help_show", txt, fixed = FALSE)) next
    # pull just the observer body (from the match to the next blank-line-free
    # close) -- approximate by checking the 400 chars after the match, enough
    # to cover every observer in this codebase without pulling in unrelated code.
    pos  <- regexpr("observeEvent\\(input\\$help_show", txt)
    body <- substr(txt, pos, pos + 400)
    if (grepl("settings_help_content\\(|settings_help_title\\(", body))
      bad <- c(bad, f)
  }
  expect_equal(bad, character(0),
              info = sprintf("file(s) with a help_show observer still using the settings-only lookup: %s",
                             paste(bad, collapse = ", ")))
})

test_that("every authored note's category matches 05-help-content-mapping.csv where a mapping row exists", {
  reg <- load_help_merged(HELP_DIRS)
  csv_path <- "../docs/help-system/assessment/05-help-content-mapping.csv"
  skip_if_not(file.exists(csv_path), "mapping CSV not found (run from src/ inside the repo checkout)")
  map <- read.csv(csv_path, stringsAsFactors = FALSE)

  mismatches <- character(0)
  for (id in names(reg)) {
    row <- map[map$help_id == id, , drop = FALSE]
    if (!nrow(row)) next   # no mapping-table row for this id -- nothing to check
    expected <- row$category[1]
    actual   <- reg[[id]]$category
    if (is.na(actual) || !identical(actual, expected))
      mismatches <- c(mismatches, sprintf("%s: note says '%s', mapping table says '%s'",
                                          id, actual, expected))
  }
  expect_equal(mismatches, character(0), info = paste(mismatches, collapse = "\n"))
})
