# =============================================================================
# about_page.R  --  sidebar "About": which release of I-SPI is running, which
#                   calib schema the database holds, and which compute releases
#                   produced the stored results (I-SPI 1.1.0).
#
# I-SPI's own version is the image's BUILD_STAMP ("<tag or branch>@<commit>",
# written by the Dockerfile; "dev" outside an image). The compute side is read
# from calib_run.manifest: every job run by i-spi-compute 1.1 or later records its
# release manifest there (curveRcore::release_manifest(): worker version and
# commit, R, curveR package versions, CmdStan, calib schema). Jobs run earlier have
# no manifest and are counted as such. Both reads tolerate a database without
# migration 011 (no schema_version table, no manifest column).
#
# Pure helpers (about_build_stamp, about_*_text) plus two reads through .calib_q.
# =============================================================================

# BUILD_STAMP -> list(version, commit, raw). "v1.1.0@3f2a..." -> 1.1.0 / 3f2a...
about_build_stamp <- function(paths = c("BUILD_STAMP", "/srv/shiny-server/BUILD_STAMP")) {
  f <- paths[file.exists(paths)][1]
  raw <- if (is.na(f)) "dev" else trimws(paste(readLines(f, warn = FALSE), collapse = " "))
  if (!nzchar(raw)) raw <- "dev"
  ref <- sub("@.*$", "", raw); commit <- if (grepl("@", raw)) sub("^.*@", "", raw) else NA_character_
  list(version = sub("^v(?=[0-9])", "", ref, perl = TRUE), commit = commit, raw = raw)
}

# schema_version rows (component, version, applied_at), or NULL without migration 011.
about_schema_versions <- function(pool) {
  ok <- .calib_q(pool, "SELECT to_regclass($1) IS NOT NULL AS ok", params = list(.tbl("schema_version")))
  if (!is.data.frame(ok) || !nrow(ok) || !isTRUE(ok$ok[1])) return(NULL)
  .calib_q(pool, sprintf("SELECT component, version, applied_at FROM %s ORDER BY applied_at DESC",
                         .tbl("schema_version")))
}

# Compute releases behind the stored results: one row per distinct release (worker
# version, commit, package versions, CmdStan), with how many jobs used it and when
# last. Optionally limited to one study's jobs. NULL without the manifest column.
about_compute_releases <- function(pool, project = NULL, study = NULL, limit = 20L) {
  has <- .calib_q(pool, "SELECT count(*)::int AS n FROM information_schema.columns
                          WHERE table_schema = $1 AND table_name = 'calib_run' AND column_name = 'manifest'",
                  params = list(sub("\\..*$", "", .tbl("calib_run"))))
  if (!is.data.frame(has) || !nrow(has) || has$n[1] == 0L) return(NULL)
  scope <- ""; par <- list()
  if (!is.null(study) && length(study) && !is.na(study) && nzchar(study)) {
    scope <- sprintf(" WHERE r.job_id IN (SELECT f.job_id FROM %s f JOIN %s cl ON cl.curve_id = f.curve_id
                                           WHERE cl.project_id = $1 AND cl.study_accession = $2)",
                     .tbl("calib_fit"), .tbl("curve_lookup"))
    par <- list(project, study)
  }
  .calib_q(pool, sprintf(
    "SELECT r.manifest ->> 'component' AS component, r.manifest ->> 'version' AS version,
            left(r.manifest ->> 'commit', 12) AS commit,
            r.manifest -> 'packages' ->> 'curveRcore' AS curveRcore,
            r.manifest -> 'packages' ->> 'curveRfreq' AS curveRfreq,
            r.manifest -> 'packages' ->> 'curveRbayes' AS curveRbayes,
            r.manifest -> 'packages' ->> 'curveRweights' AS curveRweights,
            r.manifest ->> 'cmdstan' AS cmdstan, r.manifest ->> 'r_version' AS r_version,
            r.manifest ->> 'calib_schema' AS calib_schema,
            count(*)::int AS jobs, max(r.finished_at) AS last_used
       FROM %s r%s
      GROUP BY 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
      ORDER BY (r.manifest ->> 'component') IS NULL, max(r.finished_at) DESC NULLS LAST
      LIMIT %d", .tbl("calib_run"), scope, as.integer(limit)), params = par)
}

# One line for releases that recorded no manifest (component NA): fitted before 1.1.
about_releases_note <- function(rel) {
  if (is.null(rel) || !nrow(rel)) return("")
  pre <- rel[is.na(rel$component), , drop = FALSE]
  if (!nrow(pre)) return("")
  sprintf("%s job(s) ran before i-spi-compute 1.1 and recorded no release manifest.",
          format(sum(pre$jobs), big.mark = ","))
}

about_page_ui <- function(pool, project = NULL, study = NULL) {
  b <- about_build_stamp()
  sv <- tryCatch(about_schema_versions(pool), error = function(e) NULL)
  rel_all <- tryCatch(about_compute_releases(pool), error = function(e) NULL)
  # "Click here" is the sidebar's placeholder before a study is chosen: no study.
  has_study <- length(study) == 1 && !is.na(study) && nzchar(study) && study != "Click here"
  rel_study <- if (has_study)
    tryCatch(about_compute_releases(pool, project, study), error = function(e) NULL) else NULL
  tbl <- function(df) {
    if (is.null(df) || !nrow(df)) return(shiny::tags$p(shiny::tags$em("none recorded")))
    df <- df[!is.na(df$component) | !("component" %in% names(df)), , drop = FALSE]
    if (!nrow(df)) return(shiny::tags$p(shiny::tags$em("none recorded")))
    shiny::tags$table(class = "table table-condensed table-striped", style = "width:auto;",
      shiny::tags$thead(shiny::tags$tr(lapply(names(df), shiny::tags$th))),
      shiny::tags$tbody(lapply(seq_len(nrow(df)), function(i)
        shiny::tags$tr(lapply(df[i, ], function(v) shiny::tags$td(if (is.na(v)) "" else as.character(v)))))))
  }
  shiny::fluidPage(
    shiny::h3("About I-SPI"),
    shiny::tags$table(class = "table table-condensed", style = "width:auto;",
      shiny::tags$tr(shiny::tags$th("I-SPI version"), shiny::tags$td(b$version)),
      shiny::tags$tr(shiny::tags$th("Commit"), shiny::tags$td(if (is.na(b$commit)) "" else substr(b$commit, 1, 12))),
      shiny::tags$tr(shiny::tags$th("R"), shiny::tags$td(paste(R.version$major, R.version$minor, sep = ".")))),
    shiny::h4("Database calib schema"),
    if (is.null(sv)) shiny::tags$p("No schema_version table: the database predates i-spi-compute migration 011.")
    else tbl(sv),
    if (!is.null(rel_study)) shiny::tagList(
      shiny::h4(sprintf("Compute releases behind study %s", study)),
      tbl(rel_study), shiny::tags$p(shiny::tags$small(about_releases_note(rel_study)))),
    shiny::h4("Compute releases behind all stored results"),
    if (is.null(rel_all)) shiny::tags$p("calib_run has no manifest column: the database predates i-spi-compute migration 011.")
    else shiny::tagList(tbl(rel_all), shiny::tags$p(shiny::tags$small(about_releases_note(rel_all)))),
    shiny::tags$p(shiny::tags$small(
      "Each compute job records its release manifest in calib_run.manifest (Data tab > Results > Runs).")))
}
