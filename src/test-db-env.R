# =============================================================================
# test-db-env.R
# Database settings are read from DB_NAME, DB_HOST, DB_PORT, DB_USER and
# DB_PASSWORD; the old names (db, db_host, db_port, db_userid_x, db_pwd_x)
# are used only when the new ones are unset.
# Run from src/: testthat::test_file("test-db-env.R")
# =============================================================================
library(testthat)
for (e in parse("global.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name(".db_env")))
    eval(e)

test_that("new names win over old ones", {
  withr::local_envvar(DB_PASSWORD = "new", db_pwd_x = "old")
  expect_silent(expect_equal(.db_env("DB_PASSWORD", "db_pwd_x"), "new"))
})

test_that("old name is used, with a log line, when the new one is unset", {
  withr::local_envvar(DB_USER = NA, db_userid_x = "old-user")
  expect_message(v <- .db_env("DB_USER", "db_userid_x"), "DB_USER is unset; using the old variable db_userid_x")
  expect_equal(v, "old-user")
})

test_that("neither set gives an empty string and no message", {
  withr::local_envvar(DB_USER = NA, db_userid_x = NA)
  expect_silent(expect_equal(.db_env("DB_USER", "db_userid_x"), ""))
})

test_that("DB_PORT falls back to 5432 when neither name is set", {
  withr::local_envvar(DB_PORT = NA, db_port = NA)
  expect_equal(.db_env("DB_PORT", "db_port", default = "5432"), "5432")
  # (The fallback itself is tested above with names that differ beyond case:
  # on Windows DB_PORT and db_port are the same variable.)
})
