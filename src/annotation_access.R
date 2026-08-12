# =============================================================================
# annotation_access.R  --  single read/write boundary for the annotation model
# -----------------------------------------------------------------------------
# Descriptive annotation, authored at the natural key
# (project_id, study_accession, experiment_accession). Separate from the
# configuration cascade (settings_cascade_access.R). Same conventions:
#   * every function takes `pool` first (a pool::Pool or a bare DBI connection)
#   * values bound as $1,$2,... ; project_id is required and integer
#   * schema from getOption("ispi.calib_schema", "madi_results")
# Tables: annotation_analyte, annotation_level, annotation_order (see
# sql/annotation_tables.sql).
# =============================================================================

stopifnot(requireNamespace("DBI", quietly = TRUE))

ANN_SCHEMA <- getOption("ispi.calib_schema", "madi_results")
.ann_tbl <- function(name) sprintf("%s.%s", ANN_SCHEMA, name)

`%||%` <- function(a, b) if (is.null(a)) b else a

# Debug breadcrumbs — silent unless options(ispi.annotation_debug = TRUE).
.ann_dbg <- function(...) if (isTRUE(getOption("ispi.annotation_debug", FALSE))) cat(...)

# ---- scope / query helpers --------------------------------------------------
.ann_nk <- function(project, study, experiment) {
  if (is.null(project) || length(project) == 0 || is.na(project))
    stop("annotation scope: project_id is required")
  proj <- suppressWarnings(as.integer(project))
  if (is.na(proj)) stop("annotation scope: project_id must be an integer, got: ", project)
  list(project = proj,
       study = as.character(study)[1],
       experiment = as.character(experiment)[1])
}
.ann_q <- function(pool, sql, params = list()) {
  out <- tryCatch(
    if (length(params)) DBI::dbGetQuery(pool, sql, params = params)
    else                DBI::dbGetQuery(pool, sql),
    error = function(e) {
      warning("annotation_access query failed: ", conditionMessage(e), call. = FALSE)
      NULL
    })
  if (is.null(out)) data.frame() else out
}
.ann_exec <- function(pool, sql, params = list()) {
  DBI::dbExecute(pool, sql, params = params)
}
.ann_chr <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) NA_character_ else as.character(x)[1]

# =============================================================================
# ANALYTE annotations (antigens + features)
# =============================================================================
get_analyte_annotations <- function(pool, project, study, experiment,
                                     analyte_type = NULL) {
  nk <- .ann_nk(project, study, experiment)
  sql <- sprintf(
    "SELECT analyte_type, analyte, source, virus_bacterial_strain, catalog_number,
            batch_lot, reagent_acq_date, family, updated_by, updated_at
       FROM %s
      WHERE project_id = ($1)::int AND study_accession = $2 AND experiment_accession = $3
      %s
      ORDER BY analyte_type, analyte",
    .ann_tbl("annotation_analyte"),
    if (!is.null(analyte_type)) "AND analyte_type = $4" else "")
  params <- list(nk$project, nk$study, nk$experiment)
  if (!is.null(analyte_type)) params <- c(params, list(as.character(analyte_type)))
  .ann_q(pool, sql, params)
}

set_analyte_annotation <- function(pool, project, study, experiment,
                                   analyte_type, analyte,
                                   source = NA, virus_bacterial_strain = NA,
                                   catalog_number = NA, batch_lot = NA,
                                   reagent_acq_date = NA, family = NA, user = NA) {
  nk <- .ann_nk(project, study, experiment)
  # reagent_acq_date: accept Date/character/NA; bind as text, cast in SQL
  acq <- if (is.null(reagent_acq_date) || length(reagent_acq_date) == 0 ||
             is.na(reagent_acq_date) || !nzchar(trimws(as.character(reagent_acq_date))))
           NA_character_ else as.character(reagent_acq_date)[1]
  sql <- sprintf(
    "INSERT INTO %s
       (project_id, study_accession, experiment_accession, analyte_type, analyte,
        source, virus_bacterial_strain, catalog_number, batch_lot,
        reagent_acq_date, family, updated_by, updated_at)
     VALUES (($1)::int,$2,$3,$4,$5,$6,$7,$8,$9,NULLIF($10,'')::date,$11,$12,now())
     ON CONFLICT ON CONSTRAINT annotation_analyte_pk DO UPDATE SET
        source=EXCLUDED.source, virus_bacterial_strain=EXCLUDED.virus_bacterial_strain,
        catalog_number=EXCLUDED.catalog_number, batch_lot=EXCLUDED.batch_lot,
        reagent_acq_date=EXCLUDED.reagent_acq_date, family=EXCLUDED.family,
        updated_by=EXCLUDED.updated_by, updated_at=now()",
    .ann_tbl("annotation_analyte"))
  .ann_exec(pool, sql, list(
    nk$project, nk$study, nk$experiment, as.character(analyte_type), as.character(analyte),
    .ann_chr(source), .ann_chr(virus_bacterial_strain), .ann_chr(catalog_number),
    .ann_chr(batch_lot), acq %||% NA_character_, .ann_chr(family), .ann_chr(user)))
  invisible(TRUE)
}

# =============================================================================
# LEVEL annotations (timeperiod + agroup levels)
# =============================================================================
get_level_annotations <- function(pool, project, study, experiment, variable = NULL) {
  nk <- .ann_nk(project, study, experiment)
  sql <- sprintf(
    "SELECT variable, level, description, is_referent, updated_by, updated_at
       FROM %s
      WHERE project_id = ($1)::int AND study_accession = $2 AND experiment_accession = $3
      %s
      ORDER BY variable, level",
    .ann_tbl("annotation_level"),
    if (!is.null(variable)) "AND variable = $4" else "")
  params <- list(nk$project, nk$study, nk$experiment)
  if (!is.null(variable)) params <- c(params, list(as.character(variable)))
  .ann_q(pool, sql, params)
}

set_level_annotation <- function(pool, project, study, experiment,
                                 variable, level, description = NA, user = NA) {
  nk <- .ann_nk(project, study, experiment)
  sql <- sprintf(
    "INSERT INTO %s
       (project_id, study_accession, experiment_accession, variable, level,
        description, updated_by, updated_at)
     VALUES (($1)::int,$2,$3,$4,$5,$6,$7,now())
     ON CONFLICT ON CONSTRAINT annotation_level_pk DO UPDATE SET
        description=EXCLUDED.description, updated_by=EXCLUDED.updated_by, updated_at=now()",
    .ann_tbl("annotation_level"))
  .ann_exec(pool, sql, list(
    nk$project, nk$study, nk$experiment, as.character(variable), as.character(level),
    .ann_chr(description), .ann_chr(user)))
  invisible(TRUE)
}

# Set (or clear) the single referent level for a (nk, variable). Passing
# level = NULL/NA clears the referent. Ensures the chosen level row exists, then
# one UPDATE sets is_referent = (level = chosen) so the partial unique index
# (at most one referent) is always satisfied.
set_referent <- function(pool, project, study, experiment, variable, level, user = NA) {
  nk <- .ann_nk(project, study, experiment)
  chosen <- .ann_chr(level)
  if (!is.na(chosen)) {
    # make sure the chosen level exists so the UPDATE can flip it on
    .ann_exec(pool, sprintf(
      "INSERT INTO %s (project_id, study_accession, experiment_accession, variable, level, updated_by, updated_at)
       VALUES (($1)::int,$2,$3,$4,$5,$6,now())
       ON CONFLICT ON CONSTRAINT annotation_level_pk DO NOTHING",
      .ann_tbl("annotation_level")),
      list(nk$project, nk$study, nk$experiment, as.character(variable), chosen, .ann_chr(user)))
  }
  .ann_exec(pool, sprintf(
    "UPDATE %s SET is_referent = (level IS NOT DISTINCT FROM $5), updated_by=$6, updated_at=now()
      WHERE project_id=($1)::int AND study_accession=$2 AND experiment_accession=$3 AND variable=$4",
    .ann_tbl("annotation_level")),
    list(nk$project, nk$study, nk$experiment, as.character(variable),
         if (is.na(chosen)) NA_character_ else chosen, .ann_chr(user)))
  invisible(TRUE)
}

# =============================================================================
# DISPLAY ORDER (one CSV per dimension)
# =============================================================================
get_order <- function(pool, project, study, experiment, dimension) {
  nk <- .ann_nk(project, study, experiment)
  df <- .ann_q(pool, sprintf(
    "SELECT ordered_csv FROM %s
      WHERE project_id=($1)::int AND study_accession=$2 AND experiment_accession=$3 AND dimension=$4",
    .ann_tbl("annotation_order")),
    list(nk$project, nk$study, nk$experiment, as.character(dimension)))
  if (!nrow(df) || is.na(df$ordered_csv[1]) || !nzchar(df$ordered_csv[1])) return(character(0))
  trimws(strsplit(df$ordered_csv[1], ",", fixed = TRUE)[[1]])
}

set_order <- function(pool, project, study, experiment, dimension, ordered_vector, user = NA) {
  nk <- .ann_nk(project, study, experiment)
  csv <- paste(trimws(as.character(ordered_vector)), collapse = ",")
  .ann_exec(pool, sprintf(
    "INSERT INTO %s (project_id, study_accession, experiment_accession, dimension, ordered_csv, updated_by, updated_at)
     VALUES (($1)::int,$2,$3,$4,$5,$6,now())
     ON CONFLICT ON CONSTRAINT annotation_order_pk DO UPDATE SET
        ordered_csv=EXCLUDED.ordered_csv, updated_by=EXCLUDED.updated_by, updated_at=now()",
    .ann_tbl("annotation_order")),
    list(nk$project, nk$study, nk$experiment, as.character(dimension), csv, .ann_chr(user)))
  invisible(TRUE)
}

# =============================================================================
# UNIVERSES — the distinct values available to annotate / order
# -----------------------------------------------------------------------------
# antigen/feature from curve_lookup; timeperiod/agroup from xmap_sample. All at
# the (project, study, experiment) natural key.
# =============================================================================
list_experiments <- function(pool, project, study) {
  nk <- .ann_nk(project, study, "__none__")
  # Authoritative experiment list = committed plates (xmap_header); fall back to
  # xmap_sample then curve_lookup (a study may have samples but no standards, so
  # curve_lookup alone can miss experiments).
  for (tbl in c("xmap_header", "xmap_sample", "curve_lookup")) {
    df <- .ann_q(pool, sprintf(
      "SELECT DISTINCT experiment_accession AS v FROM %s
        WHERE project_id = ($1)::int AND study_accession = $2
          AND experiment_accession IS NOT NULL
          AND experiment_accession <> '__none__'
        ORDER BY 1", .ann_tbl(tbl)), list(nk$project, nk$study))
    if (nrow(df)) {
      .ann_dbg(sprintf("[annotation] experiments project=%s study=%s: %d (from %s)\n",
                  nk$project, nk$study, nrow(df), tbl))
      return(as.character(df$v))
    }
  }
  .ann_dbg(sprintf("[annotation] experiments project=%s study=%s: 0 (none found in header/sample/curve_lookup)\n",
              nk$project, nk$study))
  character(0)
}

list_universe <- function(pool, project, study, experiment,
                          dimension = c("antigen", "feature", "timeperiod", "agroup")) {
  dimension <- match.arg(dimension)
  nk <- .ann_nk(project, study, experiment)
  if (dimension %in% c("antigen", "feature")) {
    col <- dimension
    df <- .ann_q(pool, sprintf(
      "SELECT DISTINCT %s AS v FROM %s
        WHERE project_id=($1)::int AND study_accession=$2 AND experiment_accession=$3
          AND %s IS NOT NULL AND %s <> '__none__' ORDER BY 1",
      col, .ann_tbl("curve_lookup"), col, col),
      list(nk$project, nk$study, nk$experiment))
  } else {
    col <- if (dimension == "timeperiod") "timeperiod" else "agroup"
    df <- .ann_q(pool, sprintf(
      "SELECT DISTINCT %s AS v FROM %s
        WHERE project_id=($1)::int AND study_accession=$2 AND experiment_accession=$3
          AND %s IS NOT NULL ORDER BY 1",
      col, .ann_tbl("xmap_sample"), col),
      list(nk$project, nk$study, nk$experiment))
  }
  if (!nrow(df)) character(0) else as.character(df$v)
}

# Saved order first, then any universe values not yet in it (appended, sorted).
# This is what the orderInput control is seeded with.
get_ordered_universe <- function(pool, project, study, experiment, dimension) {
  saved <- get_order(pool, project, study, experiment, dimension)
  univ  <- list_universe(pool, project, study, experiment, dimension)
  saved <- saved[saved %in% univ]                 # drop stale saved entries
  c(saved, sort(setdiff(univ, saved)))
}

# =============================================================================
# EXPORT / IMPORT -- a study's annotation rows (round-trippable)
# -----------------------------------------------------------------------------
# Mirrors export_settings_scoped / import_settings_scoped in
# settings_cascade_access.R so the Export/Import tab can bundle annotations
# alongside the cascade. export_* returns a named list of the three tables'
# rows for a study (all experiments); import_* re-applies them via the upsert
# helpers (reset_first clears the study's annotations first for a clean restore).
# =============================================================================
export_annotations_scoped <- function(pool, project, study) {
  nk <- .ann_nk(project, study, "__none__")
  q <- function(tbl) .ann_q(pool, sprintf(
    "SELECT * FROM %s WHERE project_id = ($1)::int AND study_accession = $2
      ORDER BY experiment_accession", .ann_tbl(tbl)),
    list(nk$project, nk$study))
  list(analyte = q("annotation_analyte"),
       level   = q("annotation_level"),
       order   = q("annotation_order"))
}

.ann_delete_study <- function(pool, project, study) {
  nk <- .ann_nk(project, study, "__none__")
  for (tbl in c("annotation_analyte", "annotation_level", "annotation_order"))
    .ann_exec(pool, sprintf(
      "DELETE FROM %s WHERE project_id = ($1)::int AND study_accession = $2",
      .ann_tbl(tbl)), list(nk$project, nk$study))
  invisible(TRUE)
}

import_annotations_scoped <- function(pool, bundle, project, study, user,
                                      reset_first = FALSE) {
  if (is.null(bundle)) return(invisible(0L))
  if (isTRUE(reset_first)) .ann_delete_study(pool, project, study)
  n <- 0L

  a <- bundle$analyte
  if (is.data.frame(a) && nrow(a)) for (i in seq_len(nrow(a))) {
    set_analyte_annotation(
      pool, project, study, a$experiment_accession[i],
      a$analyte_type[i], a$analyte[i],
      source = a$source[i], virus_bacterial_strain = a$virus_bacterial_strain[i],
      catalog_number = a$catalog_number[i], batch_lot = a$batch_lot[i],
      reagent_acq_date = a$reagent_acq_date[i], family = a$family[i], user = user)
    n <- n + 1L
  }

  l <- bundle$level
  if (is.data.frame(l) && nrow(l)) {
    for (i in seq_len(nrow(l)))
      set_level_annotation(pool, project, study, l$experiment_accession[i],
                           l$variable[i], l$level[i],
                           description = l$description[i], user = user)
    if ("is_referent" %in% names(l)) {
      refs <- l[!is.na(l$is_referent) & as.logical(l$is_referent), , drop = FALSE]
      if (nrow(refs)) for (i in seq_len(nrow(refs)))
        set_referent(pool, project, study, refs$experiment_accession[i],
                     refs$variable[i], refs$level[i], user = user)
    }
    n <- n + nrow(l)
  }

  o <- bundle$order
  if (is.data.frame(o) && nrow(o)) for (i in seq_len(nrow(o))) {
    csv <- o$ordered_csv[i]
    ord <- if (is.na(csv) || !nzchar(csv)) character(0)
           else trimws(strsplit(csv, ",", fixed = TRUE)[[1]])
    set_order(pool, project, study, o$experiment_accession[i], o$dimension[i],
              ord, user = user)
    n <- n + 1L
  }
  invisible(n)
}
