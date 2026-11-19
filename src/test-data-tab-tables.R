# Every Data-tab table (CALIB_TABLE_ORDER, which also drives the bundles) has a
# dictionary entry, a help note and a schema page, and the server has a loader
# for it. No database.
library(testthat)
for (p in c("src/data_dictionary.R", "data_dictionary.R")) if (file.exists(p)) { source(p); break }
src_dir <- if (file.exists("src/data_tab_module.R")) "src" else "."

test_that("calib_standards and calib_blanks are Results tables", {
  expect_true(all(c("calib_standards", "calib_blanks", "calib_controls", "control_reference")
                  %in% CALIB_TABLE_ORDER$Results))
})

test_that("every Data-tab table has a doc entry, help id, schema page and loader", {
  mod <- readLines(file.path(src_dir, "data_tab_module.R"), warn = FALSE)
  for (tb in unlist(CALIB_TABLE_ORDER, use.names = FALSE)) {
    expect_false(identical(table_doc(tb)$group, "Other"), info = tb)
    expect_true(file.exists(file.path(src_dir, "help", "schema", paste0(tb, ".yaml"))), info = tb)
    expect_true(any(grepl(sprintf("^\\s*%s\\s*=\\s*function\\(\\)", tb), mod)), info = paste("loader:", tb))
    if (grepl("^calib_|^control_", tb))
      expect_true(any(grepl(sprintf("^\\s*%s\\s*=\\s*\"data\\.results\\.", tb), mod)), info = paste("help id:", tb))
  }
})
