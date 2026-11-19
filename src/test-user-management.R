# =============================================================================
# test-user-management.R
# -----------------------------------------------------------------------------
# Manage Project (user_management.R): create, join and load a project with input
# that used to break or subvert the SQL -- a project name with an apostrophe and
# an injection attempt, an email with an apostrophe, a non-numeric project
# number. The real handlers run in shiny::testServer against a throwaway
# PostgreSQL cluster with the production table definitions (from the deployment
# repo's db/10_ispi_core.sql, inlined here).
#
# Needs a local PostgreSQL (initdb + pg_ctl); skipped otherwise. PG_BIN may
# point at the directory holding them.
# Run from src/: testthat::test_file("test-user-management.R")
# =============================================================================
library(testthat)
library(DBI)    # the app attaches DBI in global.R; the handlers call it unqualified

.find_pg_bin <- function() {
  cand <- c(Sys.getenv("PG_BIN"), dirname(Sys.which("initdb")),
            Sys.glob("C:/Program Files/PostgreSQL/*/bin"),
            Sys.glob("/usr/lib/postgresql/*/bin"))
  cand <- cand[nzchar(cand)]
  ok <- cand[file.exists(file.path(cand, "initdb")) | file.exists(file.path(cand, "initdb.exe"))]
  if (length(ok)) ok[length(ok)] else NA_character_
}
pg_bin <- .find_pg_bin()
um_file <- if (file.exists("src/user_management.R")) "src/user_management.R" else "user_management.R"
if (is.na(pg_bin) || !requireNamespace("RPostgres", quietly = TRUE) ||
    !requireNamespace("pool", quietly = TRUE)) {
  test_that("user management (needs local PostgreSQL)", skip("no local PostgreSQL / RPostgres / pool"))
} else {

port <- 54000L + sample.int(900L, 1L)
dir  <- tempfile("pgtest_")
exe  <- function(x) file.path(pg_bin, x)
system2(exe("initdb"), c("-D", shQuote(file.path(dir, "data")), "-U", "tester",
                         "--auth=trust", "-E", "UTF8"), stdout = FALSE, stderr = FALSE)
system2(exe("pg_ctl"), c("-D", shQuote(file.path(dir, "data")), "-o", shQuote(paste("-p", port)),
                         "-l", shQuote(file.path(dir, "pg.log")), "start"),
        stdout = FALSE, stderr = FALSE, wait = FALSE)
# Wait until a real connection succeeds (pg_isready alone can report ready
# before the server accepts this client), up to ~60 s.
for (i in seq_len(120)) {
  ok <- tryCatch({ DBI::dbDisconnect(DBI::dbConnect(RPostgres::Postgres(), host = "127.0.0.1",
                     port = port, user = "tester", dbname = "postgres")); TRUE },
                 error = function(e) FALSE)
  if (ok) break
  Sys.sleep(0.5)
}
pg_stop <- function() {
  system2(exe("pg_ctl"), c("-D", shQuote(file.path(dir, "data")), "-m", "fast", "stop"),
          stdout = FALSE, stderr = FALSE)
  unlink(dir, recursive = TRUE)
}

db <- pool::dbPool(RPostgres::Postgres(), host = "127.0.0.1", port = port,
                   user = "tester", dbname = "postgres")
q <- function(sql) DBI::dbExecute(db, sql)
q("CREATE SCHEMA madi_lumi_users"); q("CREATE SCHEMA madi_results")
q("CREATE TABLE madi_lumi_users.projects (project_id serial PRIMARY KEY,
     project_name varchar(255) NOT NULL, creation_date timestamp DEFAULT CURRENT_TIMESTAMP NOT NULL)")
q("CREATE TABLE madi_lumi_users.project_users (project_id integer NOT NULL REFERENCES madi_lumi_users.projects,
     user_id varchar(255) NOT NULL, is_owner boolean DEFAULT false NOT NULL,
     joined_date timestamp DEFAULT CURRENT_TIMESTAMP NOT NULL, PRIMARY KEY (project_id, user_id))")
q("CREATE TABLE madi_lumi_users.project_access_keys (project_id integer UNIQUE REFERENCES madi_lumi_users.projects,
     access_key uuid DEFAULT gen_random_uuid() NOT NULL)")
q("CREATE FUNCTION public.add_project_access_key() RETURNS trigger LANGUAGE plpgsql AS $$
   BEGIN INSERT INTO madi_lumi_users.project_access_keys (project_id, access_key)
         VALUES (NEW.project_id, gen_random_uuid()); RETURN NEW; END $$")
q("CREATE TRIGGER after_project_insert AFTER INSERT ON madi_lumi_users.projects
     FOR EACH ROW EXECUTE FUNCTION public.add_project_access_key()")
q("CREATE TABLE madi_results.xmap_users (xmap_users_id serial PRIMARY KEY, auth0_user text,
     project_name text, workspace_id bigint)")
q("CREATE TABLE madi_results.canary (x int)")   # an injection would drop this

notes <- character()
run_as <- function(user, steps) {
  server <- function(input, output, session) {
    currentuser <- function() user
    usersession <- function() "test-session"
    refreshTabUI <- function(...) invisible()
    showNotification <- function(ui, ...) notes <<- c(notes, as.character(ui))
    updateTextInput <- function(...) invisible()
    db_pool <- db
    `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
    source(um_file, local = TRUE)
  }
  shiny::testServer(server, steps(session))
}
one <- function(sql, params = NULL)
  if (length(params)) DBI::dbGetQuery(db, sql, params = params) else DBI::dbGetQuery(db, sql)

test_that("a project name with an apostrophe or SQL in it is stored verbatim", {
  name <- "Smith's lab'); DROP TABLE madi_results.canary; --"
  run_as("o'brien@example.org", function(session) {
    session$setInputs(project_name = name, create_project = 1)
  })
  p <- one("SELECT project_id, project_name FROM madi_lumi_users.projects")
  expect_equal(p$project_name, name)
  expect_true(DBI::dbExistsTable(db, DBI::Id(schema = "madi_results", table = "canary")))
  m <- one("SELECT user_id, is_owner FROM madi_lumi_users.project_users WHERE project_id = $1",
           list(p$project_id))
  expect_equal(m$user_id, "o'brien@example.org"); expect_true(m$is_owner)
  expect_match(tail(notes, 1), "created successfully")
})

test_that("load: an email with an apostrophe works; a non-number is refused, not run", {
  pid <- one("SELECT project_id FROM madi_lumi_users.projects")$project_id[1]
  run_as("o'brien@example.org", function(session) {
    session$setInputs(load_project_id = as.character(pid), load_project = 1)
  })
  u <- one("SELECT auth0_user, workspace_id FROM madi_results.xmap_users")
  expect_equal(u$auth0_user, "o'brien@example.org"); expect_equal(u$workspace_id, pid)
  expect_match(tail(notes, 1), "loaded successfully")

  run_as("o'brien@example.org", function(session) {
    session$setInputs(load_project_id = "1 OR 1=1", load_project = 1)
  })
  expect_match(tail(notes, 1), "whole number")
  run_as("intruder@example.org", function(session) {
    session$setInputs(load_project_id = as.character(pid), load_project = 1)
  })
  expect_match(tail(notes, 1), "do not have access")
})

test_that("join with the access key; wrong key or bad input refused; joining twice is a warning", {
  k <- one("SELECT project_id, access_key::text AS key FROM madi_lumi_users.project_access_keys")
  join <- function(user, id, key) run_as(user, function(session)
    session$setInputs(project_id = id, access_id = key, add_project = 1))

  join("guest@example.org", as.character(k$project_id), "00000000-0000-4000-8000-000000000000")
  expect_match(tail(notes, 1), "Invalid project number or access ID")
  join("guest@example.org", "1; DELETE FROM madi_lumi_users.projects", k$key)
  expect_match(tail(notes, 1), "whole number")
  join("guest@example.org", as.character(k$project_id), "not-a-uuid")
  expect_match(tail(notes, 1), "Invalid UUID")

  join("guest@example.org", as.character(k$project_id), k$key)
  expect_match(tail(notes, 1), "added successfully")
  m <- one("SELECT is_owner FROM madi_lumi_users.project_users WHERE user_id = 'guest@example.org'")
  expect_false(m$is_owner)
  join("guest@example.org", as.character(k$project_id), k$key)
  expect_match(tail(notes, 1), "already a member")
  expect_equal(one("SELECT count(*)::int AS n FROM madi_lumi_users.projects")$n, 1L)
})

pool::poolClose(db)
pg_stop()
}
