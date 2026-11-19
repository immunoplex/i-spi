# Tests for about_page.R: build stamp parsing, compute releases from
# calib_run.manifest, and a database without migration 011. No database: .calib_q mocked.
library(testthat)
for (p in c("src/calib_data_access.R", "calib_data_access.R")) if (file.exists(p)) { suppressMessages(source(p)); break }
for (p in c("src/about_page.R", "about_page.R")) if (file.exists(p)) { source(p); break }

test_that("the build stamp gives version and commit; dev outside an image", {
  f <- tempfile(); writeLines("v1.1.0@0123456789abcdef", f)
  b <- about_build_stamp(f); expect_equal(b$version, "1.1.0"); expect_equal(b$commit, "0123456789abcdef")
  writeLines("main@abc", f); expect_equal(about_build_stamp(f)$version, "main")
  expect_equal(about_build_stamp(tempfile())$version, "dev")
})

with_q <- function(fun, code) {
  old <- .calib_q; assign(".calib_q", fun, envir = globalenv())
  on.exit(assign(".calib_q", old, envir = globalenv())); force(code)
}
rel <- data.frame(component = c("i-spi-compute-worker", NA), version = c("1.1.0-rc.1", NA), commit = c("9cd0d95", NA),
                  curveRcore = c("0.7.0", NA), curveRfreq = "0.5.2", curveRbayes = "0.5.2", curveRweights = "0.3.0",
                  cmdstan = "2.40.0", r_version = "4.5.2", calib_schema = "0.7.0", jobs = c(3L, 3486L),
                  last_used = Sys.time(), stringsAsFactors = FALSE)

test_that("compute releases: read from the manifest, scoped to a study when given; pre-1.1 jobs counted", {
  sqls <- character()
  r <- with_q(function(pool, sql, params = list()) {
    sqls <<- c(sqls, gsub("\\s+", " ", sql))
    if (grepl("information_schema", sql)) data.frame(n = 1L) else rel
  }, about_compute_releases(NULL, 92L, "S1"))
  expect_equal(nrow(r), 2L)
  expect_match(sqls[2], "manifest ->> 'version'", fixed = TRUE)
  expect_match(sqls[2], "cl.study_accession = $2", fixed = TRUE)
  expect_match(about_releases_note(r), "3,486 job\\(s\\) ran before i-spi-compute 1.1")
  expect_equal(about_releases_note(rel[1, ]), "")
})

test_that("a database without migration 011: no schema_version, no manifest column; the page still renders", {
  q <- function(pool, sql, params = list())
    if (grepl("to_regclass", sql)) data.frame(ok = FALSE) else if (grepl("information_schema", sql)) data.frame(n = 0L) else stop("not asked")
  expect_null(with_q(q, about_schema_versions(NULL)))
  expect_null(with_q(q, about_compute_releases(NULL)))
  html <- as.character(with_q(q, about_page_ui(NULL)))
  expect_match(html, "predates i-spi-compute migration 011")
  expect_match(html, "I-SPI version")
})

test_that("the page lists the releases with a manifest and the schema version", {
  q <- function(pool, sql, params = list())
    if (grepl("to_regclass", sql)) data.frame(ok = TRUE)
    else if (grepl("schema_version", sql)) data.frame(component = "calib_schema", version = "0.7.0", applied_at = Sys.time())
    else if (grepl("information_schema", sql)) data.frame(n = 1L) else rel
  html <- as.character(with_q(q, about_page_ui(NULL, 92L, "S1")))
  expect_match(html, "1.1.0-rc.1"); expect_match(html, "calib_schema")
  expect_match(html, "Compute releases behind study S1")
})
