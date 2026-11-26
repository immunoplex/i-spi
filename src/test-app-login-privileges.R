# =============================================================================
# test-app-login-privileges.R
# -----------------------------------------------------------------------------
# I-SPI connects as a restricted database login (i-spi-agent): table reads and
# writes, but NO CREATE on the schema. Mask / unmask saves and the stale-flag
# helpers must work under such a login. Regression: .ensure_recalc_flag_table()
# ran CREATE TABLE IF NOT EXISTS on every call, and PostgreSQL checks schema
# CREATE before it checks whether the table exists, so every mask save failed
# with "permission denied for schema madi_results" (preprod, 2026-11-26).
#
# Needs a local PostgreSQL (initdb + pg_ctl); skipped otherwise. Set PG_BIN to
# the directory holding them if they are not on PATH.
#
# Run from src/: testthat::test_file("test-app-login-privileges.R")
# =============================================================================
library(testthat)

.find_pg_bin <- function() {
  cand <- c(Sys.getenv("PG_BIN"), dirname(Sys.which("initdb")),
            Sys.glob("C:/Program Files/PostgreSQL/*/bin"),
            Sys.glob("/usr/lib/postgresql/*/bin"))
  cand <- cand[nzchar(cand)]
  ok <- cand[file.exists(file.path(cand, "initdb")) | file.exists(file.path(cand, "initdb.exe"))]
  if (length(ok)) ok[length(ok)] else NA_character_
}
pg_bin <- .find_pg_bin()
if (is.na(pg_bin) || !requireNamespace("RPostgres", quietly = TRUE)) {
  test_that("app login privileges (needs local PostgreSQL)", skip("no local PostgreSQL / RPostgres"))
} else {

.source_defs <- function(f, env = globalenv()) {
  for (e in parse(f, keep.source = FALSE)) {
    if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]])) {
      rhs <- e[[3]]
      is_def <- (is.call(rhs) && identical(rhs[[1]], as.name("function"))) ||
                grepl("^[.A-Z]", as.character(e[[2]]))
      if (is_def) eval(e, env)
    }
  }
}
.source_defs("calib_data_access.R")

port <- 54000L + sample.int(900L, 1L)
dir  <- tempfile("pgtest_")
exe  <- function(x) file.path(pg_bin, x)
system2(exe("initdb"), c("-D", shQuote(file.path(dir, "data")), "-U", "tester",
                         "--auth=trust", "-E", "UTF8"), stdout = FALSE, stderr = FALSE)
system2(exe("pg_ctl"), c("-D", shQuote(file.path(dir, "data")), "-o", shQuote(paste("-p", port)),
                         "-l", shQuote(file.path(dir, "pg.log")), "start"),
        stdout = FALSE, stderr = FALSE, wait = FALSE)
for (i in seq_len(60)) {
  if (system2(exe("pg_isready"), c("-h", "localhost", "-p", port), stdout = FALSE, stderr = FALSE) == 0L) break
  Sys.sleep(0.5)
}
withr_stop <- function() {
  system2(exe("pg_ctl"), c("-D", shQuote(file.path(dir, "data")), "-m", "fast", "stop"),
          stdout = FALSE, stderr = FALSE)
  unlink(dir, recursive = TRUE)
}
admin <- DBI::dbConnect(RPostgres::Postgres(), host = "localhost", port = port,
                        user = "tester", dbname = "postgres")
q <- function(sql) DBI::dbExecute(admin, sql)

# Schema as the curated DDL has it (calib_recalc_flag already exists), owned by
# the superuser; the app login gets data privileges only, like ispi_app.
q("CREATE SCHEMA madi_results")
q("CREATE TABLE madi_results.xmap_standard (xmap_standard_id int PRIMARY KEY, masked boolean, mask_reason text)")
q("CREATE TABLE madi_results.xmap_buffer   (xmap_buffer_id   int PRIMARY KEY, masked boolean, mask_reason text)")
for (t in CALIB_CURVE_TABLES)
  q(sprintf("CREATE TABLE madi_results.%s (curve_id bigint)", t))
q("CREATE TABLE madi_results.calib_recalc_flag (curve_id bigint PRIMARY KEY, reason text NOT NULL,
     flagged_at timestamptz NOT NULL DEFAULT now())")
q("CREATE ROLE app_login LOGIN NOSUPERUSER")
q("REVOKE CREATE ON SCHEMA public FROM PUBLIC")
q("GRANT USAGE ON SCHEMA madi_results TO app_login")
q("GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA madi_results TO app_login")

seed <- function() {
  q("TRUNCATE madi_results.xmap_standard, madi_results.xmap_buffer, madi_results.calib_recalc_flag")
  q("INSERT INTO madi_results.xmap_standard VALUES (1, false, NULL), (2, false, NULL)")
  q("INSERT INTO madi_results.xmap_buffer   VALUES (5, false, NULL)")
  for (t in CALIB_CURVE_TABLES) {
    q(sprintf("TRUNCATE madi_results.%s", t))
    q(sprintf("INSERT INTO madi_results.%s VALUES (10), (11), (99)", t))
  }
}
n <- function(sql) as.integer(DBI::dbGetQuery(admin, sql)[[1]])

app <- DBI::dbConnect(RPostgres::Postgres(), host = "localhost", port = port,
                      user = "app_login", dbname = "postgres")

test_that("the app login really cannot create in the schema (precondition)", {
  expect_error(DBI::dbExecute(app,
    "CREATE TABLE IF NOT EXISTS madi_results.calib_recalc_flag (curve_id bigint)"),
    "permission denied for schema madi_results")
})

test_that("apply_mask (immediate delete) works without schema CREATE", {
  seed()
  r <- apply_mask(app, std_ids = 1L, blk_ids = 5L, group_curve_ids = c(10L, 11L), reason = "outlier")
  expect_true(r$ok)
  expect_equal(n("SELECT count(*) FROM madi_results.xmap_standard WHERE masked"), 1L)
  expect_equal(n("SELECT count(*) FROM madi_results.xmap_buffer WHERE masked"), 1L)
  expect_equal(n("SELECT count(*) FROM madi_results.calib_fit"), 1L)   # only curve 99 left
})

test_that("apply_mask (deferred) flags stale; reads and unmask work without schema CREATE", {
  seed()
  r <- apply_mask(app, std_ids = 2L, blk_ids = integer(0), group_curve_ids = 10L,
                  reason = "outlier", delete_fits = FALSE)
  expect_true(r$ok)
  expect_equal(fetch_stale_curve_ids(app, c(10L, 11L)), 10L)
  expect_true(curve_recalc_flag(app, 10L)$stale)
  u <- apply_unmask(app, std_ids = 2L, blk_ids = integer(0), group_curve_ids = 10L)
  expect_true(u$ok)
  expect_equal(fetch_stale_curve_ids(app, 10L), integer(0))
})

test_that("a missing flag table is still created when the login may create", {
  q("DROP TABLE madi_results.calib_recalc_flag")
  .ensure_recalc_flag_table(admin)
  expect_equal(n("SELECT count(*) FROM pg_tables WHERE schemaname = 'madi_results'
                    AND tablename = 'calib_recalc_flag'"), 1L)
})

DBI::dbDisconnect(app)
DBI::dbDisconnect(admin)
withr_stop()
}
