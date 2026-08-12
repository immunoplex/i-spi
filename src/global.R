# Description: This file contains the global variables and libraries that are used in the app.

# NOTE (calib refactor): the stanassay Bayesian-ensemble package load was removed.
# All Bayesian/frequentist standard-curve fitting now runs in the i-spi-compute
# worker (curveRbayes/curveRfreq); the app only reads results from madi_results.calib_*.

# Load necessary libraries
library(plotly);
library(shiny);
library(shinyjs);
library(shinyalert); library(shinydashboard);
library(shinyWidgets);
library(shinybusy); library(shinyBS); library(readxl); library(openxlsx);
library(RPostgres); library(glue); library(DBI); library(DT); library(pool);
library(data.table); library(stringi); library(stringr);
library(tidyverse); library(tidyr); library(plyr); library(modelr);
library(broom); library(rhandsontable);
library(gt); library(gtExtras); library(grid);
library(gridExtra); library(gtable); library(httr2); library(auth0);
library(moach); library(janitor);
library(bslib)
library(bsicons)   # info-circle icon for help drill-downs
library(yaml)      # parse concept-note YAML frontmatter

library(ggplot2)
#

## For study parameters
library(shinyFeedback)
library(later)
# For Study Overview
library(viridis)
library(htmltools)
library(ggrepel)
library(cowplot)

# For kernel density estimation
library(scales) # For color scaling
library(plotly) # For interactive plots

# For Standard Curve Fitting
# round_df function here
library(magrittr)
# for VIF

library(magrittr)
library(shinyWidgets)

## std-curver
library(patchwork)
library(rlang)

library(bit64)
library(shinycssloaders)

# For standard curve Summary

library(Polychrome)
library(shinyjqui)
library(future)
library(promises)
library(progressr)

# For Subgroup detection

## For Subgroup Detection Summary

## For Dilution analysis

library(tidyr)
library(digest)

## Dilution Linearity

library(purrr)

library(httr2)
library(jose)
library(openssl) # For rand_bytes
library(jsonlite)
library(urltools)

library(shiny.destroy)

# verbose diagnostic switch. Set the option TRUE to see >>> traces.
options(ispi.verbose = FALSE)   # flip to TRUE when debugging

vmsg <- function(...) {
  if (isTRUE(getOption("ispi.verbose"))) message(...)
}

# Enable progressr with shiny
handlers(global = TRUE)
handlers("shiny")

# Simple setup based on platform
if (Sys.info()["sysname"] == "Windows" || exists("RStudio.Version", envir = globalenv())) {
  plan(multisession)
  message("Using multisession plan (Windows/RStudio)")
} else {
  plan(multicore)
  message("Using multicore plan (Unix/Linux)")
}

# Set common options
options(future.globals.maxSize = 5000 * 1024^2)  # 5GB
options(future.rng.onMisuse = "ignore")

options(shiny.promise.backend = "future")
options(future.rng.onMisuse = "ignore")
options(shiny.maxRequestSize = 30*1024^2)
options(future.globals.maxSize = Inf)

# Print basic info
message("Cores available: ", parallel::detectCores())
message("Workers: ", future::nbrOfWorkers())

# Set options
options(shiny.maxRequestSize = 100 * 1024^2)
options(auth0_disable = FALSE)

# Define custom functions
rounddf <- function(x, digits = rep(2, ncol(x)), func = round, pad = FALSE) {
  DT <- FALSE
  if (class(x)[1] == "data.table") {
    x <- as.data.frame(x)
    if (requireNamespace("data.table", quietly = TRUE)) {
      DT <- TRUE
    }
  }

  if (length(digits) == 1) {
    digits <- rep(digits, ncol(x))
  } else if (length(digits) != ncol(x)) {
    digits <- c(digits, rep(digits[1], ncol(x) - length(digits)))
    warning("First value in digits repeated to match length.")
  }

  for (i in 1:ncol(x)) {
    if (class(x[, i, drop = TRUE])[1] == "numeric") {
      x[, i] <- func(x[, i], digits[i])
      if (pad && all(grepl("\\.", x[, i]))) {
        ff <- max(nchar(gsub(".+\\.", "", x[, i])))
        fmt <- paste0("%.0", ff, "f")
        x[, i] <- sprintf(fmt, x[, i])
      }
    }
  }

  if (DT) {
    x <- data.table::data.table(x)
  }

  return(x)
}

# Define color palettes
color_typ <- c("#DD4444", "orange", "#66BBBB", "#555599", "#C51B7D", "#91CF60")
focal_plate_color <- c("#66BBBB", "#555599")
color_groups <- c("#DD4444", "orange", "#66BBBB", "#555599")
color_times <- c("#DD4444", "orange", "#66BBBB", "#555599")
color_features <- c(
  "#E04C5C", "#7DAF4C", "#23AECE", "#FB894B", "#E7DA36", "#187A51",
  "#5EA4A2", "#3D3C4E", "#4D1836", "#C51B7D",
  "#E9A3C9", "#B35806", "#F1A340", "#FEE08B", "#D9EF8B",
  "#91CF60", "#C7EAE5", "#5AB4AC", "#01665E", "#E7D4E8",
  "#AF8DC3", "#762A83", "#FC0FC0", "#F9C7DE", "#f3a0c4"
)

names(color_features) <- c(
  "IgA2", "IgA1", "FcaR", "FcgR2A131", "FcgR3A158", "IgG",
  "ELISA_IgG", "IgG4", "IgG2", "IgA",
  "IgG3", "IgM", "FcgR1A", "MN", "FcgR2b",
  "IgG1", "HAI", "NAI", "FcgR3b", "ADCC",
  "ADCP", "ADCD"
)

# (legacy get_db_connection() removed -- app uses db_pool exclusively)
# App-level connection pool (shared across sessions). Preferred over holding a
# per-session connection open: it reuses connections (no per-query handshake),
# revalidates/reconnects stale ones (kills "server closed the connection" errors
# on long, bursty sessions), evicts idle ones (frees max_connections slots), and
# caps total usage. Passed as `conn` to the calib_* readers, which call
# dbGetQuery(conn, ...) -- DBI dispatches on a Pool exactly as on a connection.
# For multi-statement writes (the coming masking slice) use
# pool::poolWithTransaction(db_pool, function(c) { ... }) so they share one conn.
db_pool <- pool::dbPool(
  RPostgres::Postgres(),
  dbname   = Sys.getenv("db"),
  host     = Sys.getenv("db_host"),
  port     = Sys.getenv("db_port", "5432"),
  user     = Sys.getenv("db_userid_x"),
  password = Sys.getenv("db_pwd_x"),
  sslmode  = "require",
  sslcert = "/nonexistent/postgresql.crt",  # absent path -> stat() ENOENT -> libpq skips
  sslkey  = "/nonexistent/postgresql.key",  # (empty "" would NOT work: it defaults to ~/.postgresql)
  options  = "-c search_path=madi_results",
  minSize  = 1,
  maxSize  = 8,
  idleTimeout        = 60 * 1000,  # ms: drop idle connections after 60s
  validationInterval = 30          # s: re-check a connection at most every 30s
)
shiny::onStop(function() try(pool::poolClose(db_pool), silent = TRUE))

# (getProjectName removed from global.R -- live definition is in ui_handler.R;
# the two were identical and ui_handler.R is sourced last, so this copy was shadowed)

reloadReactive <- function(conn, userWorkSpaceID) {
  select_query <- "
    SELECT
      xmap_header.study_accession,
      xmap_header.experiment_accession,
      xmap_header.study_accession AS study_name,
      xmap_header.experiment_accession AS experiment_name,
      xmap_header.workspace_id,
      xmap_users.project_name
    FROM madi_results.xmap_header
    JOIN madi_results.xmap_users ON xmap_header.workspace_id = xmap_users.workspace_id
    WHERE xmap_header.workspace_id = $1;"

  query_result <- dbGetQuery(conn, select_query, params = list(userWorkSpaceID))
  query_result
}


# ---- Help/docs engine + settings concept registry (loaded ONCE per process) --
# help_utils.R defines the parser and the render helpers; HELP_SETTINGS is the
# concept registry the settings-cascade UI reads for Layer-3 drill-downs. Fails
# soft: a missing help/ dir warns and disables drill-downs rather than crashing.
# NOTE: source(local=FALSE) puts the engine functions in globalenv(); global.R
# itself is sourced local=TRUE (into the app env), so we must assign the registry
# into globalenv() explicitly or the engine's get0() lookup can't reach it.
source("help_utils.R")
assign("HELP_SETTINGS", load_help("help/settings"), envir = globalenv())

