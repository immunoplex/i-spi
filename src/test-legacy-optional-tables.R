# =============================================================================
# test-legacy-optional-tables.R
# -----------------------------------------------------------------------------
# Study clone / delete / their dry-run counts must work both on a database that
# still has the legacy tables (xmap_study_config, xmap_dilution_analysis,
# xmap_dilution_parameters) -- unchanged behaviour, their rows are cloned and
# deleted -- and on a fresh install whose curated DDL leaves them out.
#
# Needs a local PostgreSQL (initdb + pg_ctl); skipped otherwise. Set PG_BIN to
# the directory holding them if they are not on PATH. A throwaway cluster is
# created in a temp directory and removed afterwards.
#
# Run from src/: testthat::test_file("test-legacy-optional-tables.R")
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
  test_that("legacy optional tables (needs local PostgreSQL)", skip("no local PostgreSQL / RPostgres"))
} else {

# Load only the definitions (function and constant assignments) from files that
# also carry top-level Shiny server wiring.
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
for (f in c("calib_data_access.R", "clone_study_components.R", "delete_study_components.R"))
  .source_defs(f)

port <- 54000L + sample.int(900L, 1L)
dir  <- tempfile("pgtest_")
exe  <- function(x) file.path(pg_bin, x)
system2(exe("initdb"), c("-D", shQuote(file.path(dir, "data")), "-U", "tester",
                         "--auth=trust", "-E", "UTF8"), stdout = FALSE, stderr = FALSE)
# Start without waiting: on Windows the server inherits pg_ctl's handles and a
# blocking system2() never returns. Poll pg_isready instead (up to ~30 s).
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
con <- DBI::dbConnect(RPostgres::Postgres(), host = "localhost", port = port,
                      user = "tester", dbname = "postgres")

build_db <- function(con, with_legacy, with_011 = TRUE) {
  q <- function(sql) DBI::dbExecute(con, sql)
  q("DROP SCHEMA IF EXISTS madi_results CASCADE"); q("DROP SCHEMA IF EXISTS madi_lumi_users CASCADE")
  q("CREATE SCHEMA madi_results"); q("CREATE SCHEMA madi_lumi_users")
  q("CREATE TABLE madi_lumi_users.projects (project_id int PRIMARY KEY, project_name text)")
  q("INSERT INTO madi_lumi_users.projects VALUES (1, 'src'), (20, 'dst')")
  q("CREATE TABLE madi_results.curve_lookup (curve_id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
       project_id int, study_accession varchar(15), experiment_accession varchar(15),
       multiplate_group_id uuid)")
  curve_tabs <- if (with_011) CALIB_CURVE_TABLES else setdiff(CALIB_CURVE_TABLES, "calib_controls")
  for (t in curve_tabs)
    q(sprintf("CREATE TABLE madi_results.%s (curve_id bigint, job_id text)", t))
  if (with_011) {
    q("CREATE TABLE madi_results.control_reference (reference_id bigint GENERATED ALWAYS AS IDENTITY,
         project_id int, study_accession varchar(15) NOT NULL DEFAULT '__none__', v int)")
    # one study-specific value and one project-wide value (study '__none__')
    q("INSERT INTO madi_results.control_reference (project_id, study_accession, v)
         VALUES (1, 'S1', 1), (1, '__none__', 2)")
  }
  q("CREATE TABLE madi_results.calib_run (job_id text)")
  exp_tabs <- c(unname(CALIB_RAW_TABLES), "xmap_antigen_family", "annotation_analyte",
                "annotation_level", "annotation_order", "calib_settings",
                if (with_legacy) "xmap_dilution_analysis")
  for (t in exp_tabs)
    q(sprintf("CREATE TABLE madi_results.%s (project_id int, study_accession varchar(15),
                 experiment_accession varchar(15), v int)", t))
  if (with_legacy) for (t in c("xmap_dilution_parameters", "xmap_study_config"))
    q(sprintf("CREATE TABLE madi_results.%s (project_id int, study_accession varchar(15), v int)", t))
  q("CREATE TABLE madi_results.xmap_users (auth0_user text, project_name text, workspace_id bigint)")

  # one study (project 1, 'S1') with two experiments
  q("INSERT INTO madi_results.curve_lookup (project_id, study_accession, experiment_accession)
       VALUES (1,'S1','E1'), (1,'S1','E2')")
  for (t in curve_tabs)
    q(sprintf("INSERT INTO madi_results.%s SELECT curve_id, 'job1' FROM madi_results.curve_lookup", t))
  q("INSERT INTO madi_results.calib_run VALUES ('job1')")
  for (t in exp_tabs)
    q(sprintf("INSERT INTO madi_results.%s VALUES (1,'S1','E1',1), (1,'S1','E2',2)", t))
  if (with_legacy) for (t in c("xmap_dilution_parameters", "xmap_study_config"))
    q(sprintf("INSERT INTO madi_results.%s VALUES (1,'S1',1)", t))
}
n_rows <- function(t, proj, study)
  as.integer(DBI::dbGetQuery(con, sprintf(
    "SELECT count(*) FROM madi_results.%s WHERE project_id = $1 AND study_accession = $2", t),
    params = list(proj, study))[[1]])
legacy <- c("xmap_dilution_analysis", "xmap_dilution_parameters", "xmap_study_config")
m011 <- c("calib_controls", "control_reference")
n_curve_rows <- function(t)
  as.integer(DBI::dbGetQuery(con, sprintf("SELECT count(*) FROM madi_results.%s", t))[[1]])

test_that(".present_tables keeps legacy tables only where they exist", {
  build_db(con, with_legacy = TRUE)
  expect_equal(.present_tables(con, .CLONE_TABLES), .CLONE_TABLES)
  build_db(con, with_legacy = FALSE)
  expect_equal(.present_tables(con, .CLONE_TABLES), setdiff(.CLONE_TABLES, legacy))
  expect_equal(.present_tables(con, c("xmap_header", "curve_lookup")), c("xmap_header", "curve_lookup"))
  build_db(con, with_legacy = TRUE, with_011 = FALSE)
  expect_equal(.present_tables(con, .CLONE_TABLES), setdiff(.CLONE_TABLES, "control_reference"))
  expect_equal(.present_tables(con, CALIB_CURVE_TABLES), setdiff(CALIB_CURVE_TABLES, "calib_controls"))
})

test_that("migration-011 tables: calib_controls deleted with the curves, control_reference cloned and deleted by study", {
  build_db(con, with_legacy = TRUE)
  cnt <- count_study_components(con, 1L, "S1")
  expect_equal(unname(cnt["calib_controls"]), 2L)
  expect_equal(unname(cnt["control_reference"]), 1L)            # the study's row, not the project-wide one
  expect_equal(unname(count_clone_source(con, 1L, "S1")["control_reference"]), 1L)

  clone_study_components(con, 1L, "S1", 20L, "S2", auth0_user = "u")
  expect_equal(n_rows("control_reference", 20L, "S2"), 1L)     # study-specific value copied
  expect_equal(n_rows("control_reference", 20L, "__none__"), 0L) # project-wide value not copied
  expect_equal(n_curve_rows("calib_controls"), 2L)              # fitted results are not cloned

  delete_study_components(con, 1L, "S1", experiment = "E1")
  expect_equal(n_curve_rows("calib_controls"), 1L)              # experiment delete: that experiment's curves
  expect_equal(n_rows("control_reference", 1L, "S1"), 1L)      # study-level: kept by an experiment delete
  del <- delete_study_components(con, 1L, "S1")
  expect_equal(n_curve_rows("calib_controls"), 0L)
  expect_equal(n_rows("control_reference", 1L, "S1"), 0L)
  expect_equal(n_rows("control_reference", 1L, "__none__"), 1L) # project-wide value stays with the project
  expect_equal(n_rows("control_reference", 20L, "S2"), 1L)      # the clone is untouched
  expect_true(all(m011 %in% names(del)))

  # Recalculation deletes (masking) go through calib_group_rowcounts' table set too
  expect_true("calib_controls" %in% names(calib_group_rowcounts(con, 1:2)))
})

test_that("without the migration-011 tables: counts, clone and delete skip them", {
  build_db(con, with_legacy = TRUE, with_011 = FALSE)
  cnt <- count_study_components(con, 1L, "S1")
  expect_false(any(m011 %in% names(cnt)))
  expect_false("control_reference" %in% names(count_clone_source(con, 1L, "S1")))
  clone_study_components(con, 1L, "S1", 20L, "S2", auth0_user = "u")
  expect_equal(n_rows("xmap_header", 20L, "S2"), 2L)
  del <- delete_study_components(con, 1L, "S1")
  expect_false(any(m011 %in% names(del)))
  expect_equal(n_rows("curve_lookup", 1L, "S1"), 0L)
  expect_false("calib_controls" %in% names(calib_group_rowcounts(con, 1:2)))
})

test_that("with legacy tables: clone and delete still cover them (unchanged behaviour)", {
  build_db(con, with_legacy = TRUE)
  cnt <- count_study_components(con, 1L, "S1")
  expect_true(all(legacy %in% names(cnt)))
  expect_equal(unname(cnt["xmap_study_config"]), 1L)

  ins <- clone_study_components(con, 1L, "S1", 20L, "S2", auth0_user = "u")
  for (t in legacy) expect_equal(n_rows(t, 20L, "S2"), n_rows(t, 1L, "S1"), info = t)

  del <- delete_study_components(con, 1L, "S1")
  for (t in c(legacy, "xmap_header", "curve_lookup")) expect_equal(n_rows(t, 1L, "S1"), 0L, info = t)
  expect_true(all(legacy %in% names(del)))
})

test_that("without legacy tables: counts, clone and delete work and skip them", {
  build_db(con, with_legacy = FALSE)
  cnt <- count_study_components(con, 1L, "S1")
  expect_false(any(legacy %in% names(cnt)))
  expect_equal(unname(cnt["xmap_header"]), 2L)
  expect_false(any(legacy %in% names(count_clone_source(con, 1L, "S1"))))

  ins <- clone_study_components(con, 1L, "S1", 20L, "S2", auth0_user = "u")
  expect_equal(n_rows("xmap_header", 20L, "S2"), 2L)
  expect_equal(n_rows("curve_lookup", 20L, "S2"), 2L)

  # experiment-scoped delete, then whole-study delete
  delete_study_components(con, 1L, "S1", experiment = "E1")
  expect_equal(n_rows("xmap_header", 1L, "S1"), 1L)
  del <- delete_study_components(con, 1L, "S1")
  for (t in c("xmap_header", "curve_lookup", "annotation_order", "calib_settings"))
    expect_equal(n_rows(t, 1L, "S1"), 0L, info = t)
  expect_false(any(legacy %in% names(del)))
  # the clone is untouched
  expect_equal(n_rows("xmap_header", 20L, "S2"), 2L)
})

DBI::dbDisconnect(con)
withr_stop()
}
