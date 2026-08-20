
# calib_data_access.R  --  the single read boundary between i-spi and calib_*
# -----------------------------------------------------------------------------
# This is the ONLY place the Shiny app is allowed to touch the calib_* tables.
# Every function here is pure: it takes a DB handle + identifiers and returns a
# tidy data.frame. No Shiny reactivity, no plotting, no side effects. The old
# split between bayes_* reads and best_*/xmap_standard_fits reads collapses into
# this one method-agnostic surface, so UI code never sees a raw legacy column
# or has to know which engine produced a fit.
#
# GRAIN (from the live schema / PK indexes -- do not re-derive elsewhere):
#   curve_lookup            PK curve_id; unique NK (10 cols, see CALIB_NK_COLS)
#   calib_fit               PK (curve_id, method, model_name); best = WHERE is_best
#   calib_diagnostics       PK (curve_id, method)              -> 1 row
#   calib_grid              PK (curve_id, method, point_index) -> ~200 rows
#   calib_samples           PK (curve_id, method, sampleid, patientid,
#                                timeperiod, dilution); missing id = '__none__'
#   calib_param             PK (curve_id, method, model_name, term)
#   calib_gate              PK (curve_id, method, model_name, gate)
#   calib_loo               PK (curve_id, method, model_name); BAYESIAN ONLY
#   calib_run               PK job_id
#
# CONVENTIONS
#   * `pool` is a DBI connection or a pool::Pool -- both work with dbGetQuery.
#   * `method` is 'bayesian' | 'frequentist'.
#   * All values are bound as query parameters ($1, $2, ...); only fixed
#     identifiers appear in the SQL text. (Scalar binds work in RPostgres; it is
#     only array binds via = ANY($1) that do not -- hence explicit IN lists.)
#   * curve_id is bigint -> comes back as integer64; pass it straight back in.


stopifnot(requireNamespace("DBI", quietly = TRUE))

# ---- Constants: schema, sentinels, natural key, family mapping --------------
CALIB_SCHEMA <- getOption("ispi.calib_schema", "madi_results")
CALIB_NONE   <- "__none__"   # sentinel used for missing sample-identity fields

# The 10-column natural key, in the exact order of the unique index
# curve_lookup_nk. Used to resolve a curve to its stable curve_id.
CALIB_NK_COLS <- c("project_id", "study_accession", "experiment_accession",
                   "plateid", "plate", "nominal_sample_dilution",
                   "source", "wavelength", "antigen", "feature")

# Model families. The authoritative set is curveRcore::available_models(), which
# BOTH curveRfreq and curveRbayes fit and put through model selection. Labels are
# taken from the curveRcore "Model Forms" vignette so the UI speaks the same
# language as the package docs. This table is display metadata ONLY -- it is not
# the gatekeeper of which models exist (see calib_available_models()).
# Notes from the vignette that matter downstream:
#   * loglogistic5 is the Richards / generalised logistic (NOT a literal
#     "5-param log-logistic").
#   * loglogistic4 is the Hill equation fit on the RAW concentration scale
#     (x > 0); the other four take x = log10(concentration).
# The module never needs the per-model x-scale for plotting, because calib_grid
# already carries BOTH log10_concentration and concentration from the worker.
CALIB_FAMILY <- data.frame(
  model_name = c("logistic4", "logistic5", "loglogistic4", "loglogistic5", "gompertz4"),
  label      = c("Four-Parameter Logistic (4PL)",
                 "Five-Parameter Logistic (5PL)",
                 "Four-Parameter Log-Logistic (LL4)",
                 "Generalised Logistic \u2014 Richards (LL5)",
                 "Four-Parameter Gompertz (G4)"),
  short      = c("4PL", "5PL", "LL4", "LL5", "G4"),
  n_params   = c(4L, 5L, 4L, 5L, 4L),
  stringsAsFactors = FALSE
)

#' Full human label for an engine model_name (logistic4 -> "Four-Parameter
#' Logistic (4PL)"), matching the curveRcore Model Forms vignette. Unknown/new
#' models fall back to the raw model_name so nothing ever renders blank.
family_label <- function(model_name) {
  i <- match(model_name, CALIB_FAMILY$model_name)
  ifelse(is.na(i), as.character(model_name), CALIB_FAMILY$label[i])
}
#' Compact family code for legends/tables (logistic4 -> "4PL").
family_short <- function(model_name) {
  i <- match(model_name, CALIB_FAMILY$model_name)
  ifelse(is.na(i), as.character(model_name), CALIB_FAMILY$short[i])
}

# ---- Internal query helper --------------------------------------------------
# Runs a parameterized SELECT and returns a data.frame (0-row frame on empty).
# Runs a SELECT and returns a data.frame (0-row frame on empty/error). RPostgres
# errors ("Query does not require parameters") if you pass params to a query with
# no $1 placeholders, so only pass params when there are some.
.calib_q <- function(pool, sql, params = list()) {
  out <- tryCatch(
    if (length(params)) DBI::dbGetQuery(pool, sql, params = params)
    else                DBI::dbGetQuery(pool, sql),
    error = function(e) {
      message("calib_data_access query FAILED",
              "\n  SQL: ",    gsub("\\s+", " ", substr(sql, 1, 240)),
              "\n  params: ", paste(unlist(params), collapse = " | "),
              "\n  error: ",  conditionMessage(e))
      NULL
    })
  if (is.null(out)) data.frame() else out
}

.tbl <- function(name) sprintf("%s.%s", CALIB_SCHEMA, name)

# Replace the '__none__' sentinel with NA on the way out, so the UI sees real
# missingness instead of a magic string.
.decode_none <- function(df, cols) {
  for (c in intersect(cols, names(df)))
    df[[c]][df[[c]] == CALIB_NONE] <- NA
  df
}

# ---------------------------------------------------------------------------
# curve_lookup <-> raw xmap join, SENTINEL-SAFE (shared by every function that
# matches a raw xmap_* row to its curve: fetch_standard_points, standards_support,
# the mask resolvers, and the blank fan-out).
#
# curve_lookup stores the '__none__' string sentinel (CALIB_NONE) for absent
# natural-key fields (see build_curve_lookup_candidates / .decode_none), whereas
# the raw xmap_* tables store real NULL (or '') for those same fields. A plain
# `cl.col IS NOT DISTINCT FROM s.col` therefore FAILS on any sentinel column --
# e.g. for a bead array wavelength = '__none__' in curve_lookup but NULL in
# xmap_standard, and '__none__' IS NOT DISTINCT FROM NULL is FALSE. That silently
# returned an EMPTY join, which surfaced downstream as "Nothing resolved to mask."
#
# Normalising the sentinel and '' to NULL on BOTH sides makes NULL, '' and the
# sentinel all compare equal, so the key matches whichever representation each
# table happens to use. `cols` is the natural-key subset to join on. project_id
# is numeric and is compared directly (NULLIF against a text literal is a type
# error).
.nk_join_on <- function(cols, cl = "cl", s = "s") {
  none_lit <- sprintf("'%s'", gsub("'", "''", CALIB_NONE))  # safe SQL literal for the sentinel
  side <- function(alias, col)
    if (identical(col, "project_id")) sprintf("%s.%s", alias, col)
    else sprintf("NULLIF(NULLIF(%s.%s, %s), '')", alias, col, none_lit)
  paste(vapply(cols, function(col)
    sprintf("%s IS NOT DISTINCT FROM %s", side(cl, col), side(s, col)),
    character(1)), collapse = "\n        AND ")
}

# The full standard-curve natural key (== CALIB_NK_COLS; order is irrelevant in
# an ANDed ON clause). Blanks join on the SAME key MINUS source, because a
# blank's source differs from the curve it feeds.
STD_NK_JOIN_COLS <- CALIB_NK_COLS
BLK_NK_JOIN_COLS <- setdiff(CALIB_NK_COLS, "source")


# 0b. Fitting configuration: what to fit (per antigen/feature settings)
# -----------------------------------------------------------------------------
# The list of models curveRfreq/curveRbayes fit and select among is USER-
# controlled, per antigen/feature. Those settings -- model_form_list, standard
# concentration, pcov threshold, lower-asymptote constraints, reporting unit --
# live in the purpose-named table antigen_feature_settings (created + seeded by
# create_antigen_feature_settings.sql from the misnamed xmap_antigen_family).
# The physical name is isolated in one constant so any further rename is trivial.
TBL_ANTIGEN_SETTINGS <- getOption("ispi.antigen_settings_table", "antigen_feature_settings")

# In antigen_feature_settings, model_form_list is stored in curveRcore model_name
# notation ("logistic4, gompertz4, ...") -- the exact strings curveRfreq/
# curveRbayes consume. This alias map is therefore only a FALLBACK: it maps any
# legacy Y-notation stragglers and passes curveRcore names through unchanged, so
# parse_model_form_list() is correct against either. (Yd -> loglogistic confirmed.)
MODEL_FORM_ALIASES <- c(
  Y4  = "logistic4",  Yd4 = "loglogistic4", Ygomp4 = "gompertz4",
  Y5  = "logistic5",  Yd5 = "loglogistic5",
  logistic4 = "logistic4", loglogistic4 = "loglogistic4", gompertz4 = "gompertz4",
  logistic5 = "logistic5", loglogistic5 = "loglogistic5")

#' Parse a model_form_list string ("logistic5, loglogistic5, logistic4, ...")
#' into an ordered vector of curveRcore model_name values. Post-migration these
#' are already curveRcore names (pass-through); pre-migration Y-notation is
#' still accepted. Unknown codes are dropped with a warning.
parse_model_form_list <- function(model_form_list) {
  if (is.null(model_form_list) || length(model_form_list) == 0) return(character(0))
  s <- model_form_list[1]
  if (is.na(s) || !nzchar(s)) return(character(0))
  raw <- trimws(strsplit(s, "[,;]")[[1]])
  raw <- raw[nzchar(raw)]
  mapped <- unname(MODEL_FORM_ALIASES[raw])
  if (anyNA(mapped)) {
    warning("parse_model_form_list: unknown code(s): ",
            paste(raw[is.na(mapped)], collapse = ", "), call. = FALSE)
    mapped <- mapped[!is.na(mapped)]
  }
  unique(mapped)
}

#' Serialize a vector of curveRcore model_name values into the comma string the
#' compute API expects in params$models (e.g. "logistic4,gompertz4").
model_list_to_param <- function(models) paste(models, collapse = ",")

# ## Deleted following refactored settings
#' #' Resolve the per-antigen/feature analysis settings for a curve. Rows range
#' #' from broad (study + antigen; experiment/feature NULL) to specific (study +
#' #' experiment + antigen + feature); this returns the MOST specific matching row.
#' #' Besides model_form_list it carries standard_curve_concentration,
#' #' pcov_threshold, l_asy_* constraints, and concentration_unit_reported -- all
#' #' inputs to a fit job.
#' #' NOTE: the specificity/precedence logic is an interpretation of how the table
#' #' layers defaults vs overrides; confirm it matches your data conventions.
#' fetch_antigen_feature_settings <- function(pool, project_id, study, antigen,
#'                                            experiment = NULL, feature = NULL) {
#'   .calib_q(pool, sprintf(
#'     "SELECT *,
#'             ( (experiment_accession IS NOT DISTINCT FROM $4)::int * 2
#'             + (feature             IS NOT DISTINCT FROM $5)::int ) AS specificity
#'        FROM %s
#'       WHERE project_id IS NOT DISTINCT FROM $1
#'         AND study_accession = $2
#'         AND antigen = $3
#'         AND (experiment_accession IS NOT DISTINCT FROM $4 OR experiment_accession IS NULL)
#'         AND (feature             IS NOT DISTINCT FROM $5 OR feature             IS NULL)
#'       ORDER BY specificity DESC
#'       LIMIT 1", .tbl(TBL_ANTIGEN_SETTINGS)),
#'     params = list(project_id, study, antigen, experiment, feature))
#' }


# 1. Identity: natural key  <->  curve_id


# The app resolves and lists curves through the unmasked view, so masked curves
# (rare, and masked as a whole -- curve + all its rows together) never appear in
# the selector and are never fit, matching the worker. Overridable for tests /
# admin views that need to see masked curves too.
TBL_CURVE_LOOKUP <- getOption("ispi.curve_lookup_table", "curve_lookup_unmasked")

#' Resolve one natural key to its curve_id.
#' @param nk named list/vector with the CALIB_NK_COLS elements.
#' @return single curve_id (integer64) or NA if the curve is unknown.
resolve_curve_id <- function(pool, nk) {
  missing <- setdiff(CALIB_NK_COLS, names(nk))
  if (length(missing))
    stop("resolve_curve_id: missing NK fields: ", paste(missing, collapse = ", "))
  where <- paste(sprintf("%s IS NOT DISTINCT FROM $%d", CALIB_NK_COLS,
                         seq_along(CALIB_NK_COLS)), collapse = " AND ")
  sql <- sprintf("SELECT curve_id FROM %s WHERE %s", .tbl(TBL_CURVE_LOOKUP), where)
  res <- .calib_q(pool, sql, params = as.list(unname(nk[CALIB_NK_COLS])))
  if (nrow(res) == 0) NA else res$curve_id[1]
}

#' Batch NK -> curve_id join. Give it a data.frame with the CALIB_NK_COLS and
#' get the same rows back with a curve_id column appended (NA where unmatched).
#' Prefer this over row-by-row resolve_curve_id() for tables of samples/plates.
resolve_curve_ids <- function(pool, nk_df) {
  lk <- fetch_curve_lookup(pool)
  merge(nk_df, lk[, c(CALIB_NK_COLS, "curve_id")],
        by = CALIB_NK_COLS, all.x = TRUE, sort = FALSE)
}

#' The curve registry (NK + curve_id) as the app sees it: unmasked curves only
#' (see TBL_CURVE_LOOKUP). Small enough (~27k rows) to pull once and join in R.
#' The unmasked view omits masked/mask_reason, so those are not returned here;
#' point-level masking counts for a "k of M masked" display come from elsewhere.
fetch_curve_lookup <- function(pool, project = NULL, study = NULL, experiment = NULL) {
  # Optional scope filters push the WHERE to the DB. No args -> whole registry
  # (preserves existing callers). Scoping this was the Standard Curve selector
  # slow link: it used to pull the ENTIRE curve_lookup table and filter in R.
  where <- character(0); params <- list()
  ok <- function(v) !is.null(v) && !is.na(v) && nzchar(as.character(v))
  if (ok(study))      { where <- c(where, sprintf("study_accession = $%d",      length(params) + 1L)); params <- c(params, list(study)) }
  if (ok(experiment)) { where <- c(where, sprintf("experiment_accession = $%d", length(params) + 1L)); params <- c(params, list(experiment)) }
  if (ok(project))    { where <- c(where, sprintf("project_id = $%d",           length(params) + 1L)); params <- c(params, list(project)) }
  wc <- if (length(where)) paste("WHERE", paste(where, collapse = " AND ")) else ""
  .calib_q(pool, sprintf("SELECT curve_id, %s FROM %s %s",
    paste(CALIB_NK_COLS, collapse = ", "), .tbl(TBL_CURVE_LOOKUP), wc), params = params)
}


# 2. Fits (candidate models + best selection)


#' All candidate model fits for a curve+method, with selection metadata.
#' Columns include is_best, is_fallback, converged, eligible, criterion,
#' score_type ('loo_elpd' bayes / 'aic' freq), selection_score, selection_weight.
fetch_calib_fit <- function(pool, curve_id, method) {
  df <- .calib_q(pool, sprintf(
    "SELECT * FROM %s WHERE curve_id = $1 AND method = $2
      ORDER BY is_best DESC, selection_score DESC", .tbl("calib_fit")),
    params = list(curve_id, method))
  if (nrow(df)) df$family_label <- family_label(df$model_name)
  df
}

#' The single winning model row for a curve+method (uses the is_best index).
#' Returns a 1-row frame, or a 0-row frame if the curve/method is absent.
fetch_calib_best_model <- function(pool, curve_id, method) {
  df <- .calib_q(pool, sprintf(
    "SELECT * FROM %s WHERE curve_id = $1 AND method = $2 AND is_best
      LIMIT 1", .tbl("calib_fit")),
    params = list(curve_id, method))
  if (nrow(df)) df$family_label <- family_label(df$model_name)
  df
}

#' The model families actually fit for a curve+method (or across the whole
#' table when curve_id is NULL) -- data-driven, straight from calib_fit, so a
#' model selector reflects exactly what curveRfreq/curveRbayes produced rather
#' than a hardcoded assumption. Best model sorts first when a curve is given.
calib_available_models <- function(pool, curve_id = NULL, method = NULL) {
  where <- character(0); params <- list(); i <- 0L
  if (!is.null(curve_id)) { i <- i + 1L; where <- c(where, sprintf("curve_id = $%d", i)); params <- c(params, list(curve_id)) }
  if (!is.null(method))   { i <- i + 1L; where <- c(where, sprintf("method = $%d",   i)); params <- c(params, list(method)) }
  wc  <- if (length(where)) paste("WHERE", paste(where, collapse = " AND ")) else ""
  ord <- if (!is.null(curve_id)) "ORDER BY bool_or(is_best) DESC, model_name" else "ORDER BY model_name"
  df  <- .calib_q(pool, sprintf(
    "SELECT model_name FROM %s %s GROUP BY model_name %s",
    .tbl("calib_fit"), wc, ord), params = params)
  df$model_name
}


# 3. Parameters, eligibility gates, LOO


#' Per-term parameters (estimate, std_error, q_lo/q_med/q_hi). Defaults to the
#' best model when model_name is NULL, so callers usually don't specify it.
fetch_calib_params <- function(pool, curve_id, method, model_name = NULL) {
  if (is.null(model_name)) {
    best <- fetch_calib_best_model(pool, curve_id, method)
    if (!nrow(best)) return(data.frame())
    model_name <- best$model_name[1]
  }
  .calib_q(pool, sprintf(
    "SELECT term, estimate, std_error, q_lo, q_med, q_hi, model_name
       FROM %s WHERE curve_id = $1 AND method = $2 AND model_name = $3
      ORDER BY term", .tbl("calib_param")),
    params = list(curve_id, method, model_name))
}

#' Eligibility gates (gate, passed, detail). NULL model_name -> best model.
fetch_calib_gates <- function(pool, curve_id, method, model_name = NULL) {
  if (is.null(model_name)) {
    best <- fetch_calib_best_model(pool, curve_id, method)
    if (!nrow(best)) return(data.frame())
    model_name <- best$model_name[1]
  }
  .calib_q(pool, sprintf(
    "SELECT gate, passed, detail, model_name
       FROM %s WHERE curve_id = $1 AND method = $2 AND model_name = $3
      ORDER BY gate", .tbl("calib_gate")),
    params = list(curve_id, method, model_name))
}

#' LOO comparison table. Bayesian only by design; returns a 0-row frame for
#' frequentist curves (calib_loo has no frequentist rows), which callers should
#' treat as "no LOO available", NOT as an error.
fetch_calib_loo <- function(pool, curve_id, method = "bayesian") {
  if (!identical(method, "bayesian")) return(data.frame())
  .calib_q(pool, sprintf(
    "SELECT * FROM %s WHERE curve_id = $1 AND method = $2
      ORDER BY elpd_loo DESC", .tbl("calib_loo")),
    params = list(curve_id, method))
}


# 4. Plotting grid  (the ONE curve visualization source)


#' The ~200-point fitted grid for a curve+method, ordered for plotting.
#' Carries both scales (log10_concentration + concentration), the response with
#' CI band (predicted_response, ci_lower, ci_upper), the inverse prediction
#' (predicted_concentration + se_concentration), and the pcov QC series.
fetch_calib_grid <- function(pool, curve_id, method) {
  .calib_q(pool, sprintf(
    "SELECT point_index, model_name, log10_concentration, concentration,
            predicted_response, ci_lower, ci_upper,
            predicted_concentration, se_concentration,
            pcov, pcov_rmse, pcov_pass, d2y_dx2, noise_mode
       FROM %s WHERE curve_id = $1 AND method = $2
      ORDER BY point_index", .tbl("calib_grid")),
    params = list(curve_id, method))
}

#' Observed STANDARD points for a curve+method, with transform + mask status.
#' Persisted by the worker (curveRcore >= 0.3.0) already on the fit's response
#' scale, so `log10_concentration` / `response_model` overlay directly on
#' calib_grid -- NO app-side concentration/response derivation. Split on
#' `included`: TRUE entered the fit; FALSE was excluded (`exclusion_reason` e.g.
#' 'masked', with `mask_reason` free text for the hover).
#' Resolve a fit scope to the curve batch to send to the worker. Returns
#' curve_id + multiplate_group_id (+ antigen/feature for display) from the
#' UNMASKED registry, so whole-masked curves are never submitted. `feature` and
#' `antigen` are optional narrowers for the "single feature/antigen" scope; when
#' both NULL the whole study/experiment is returned. Scope resolution lives HERE
#' (the app), not in the worker.
fetch_curve_batch <- function(pool, study, experiment, project_id,
                              feature = NULL, antigen = NULL) {
  where <- "study_accession = $1 AND experiment_accession = $2
            AND project_id IS NOT DISTINCT FROM $3"
  params <- list(study, experiment, project_id)
  if (!is.null(feature)) { params <- c(params, list(feature))
    where <- paste0(where, sprintf(" AND feature = $%d", length(params))) }
  if (!is.null(antigen)) { params <- c(params, list(antigen))
    where <- paste0(where, sprintf(" AND antigen = $%d", length(params))) }
  .calib_q(pool, sprintf(
    "SELECT curve_id, multiplate_group_id, antigen, feature
       FROM %s WHERE %s ORDER BY multiplate_group_id, curve_id",
    .tbl(TBL_CURVE_LOOKUP), where), params = params)
}

#' Distinct methods actually COMPUTED for a curve (present in calib_fit). Drives
#' the plot's method picker so only methods with results appear -- distinct from
#' the fit-engine selector, which always offers both engines to submit.
fetch_calib_methods <- function(pool, curve_id) {
  df <- .calib_q(pool, sprintf(
    "SELECT DISTINCT method FROM %s WHERE curve_id = $1 ORDER BY method",
    .tbl("calib_fit")), params = list(curve_id))
  if (is.null(df) || !nrow(df)) character(0) else as.character(df$method)
}

fetch_calib_standards <- function(pool, curve_id, method) {
  .calib_q(pool, sprintf(
    "SELECT well, dilution, concentration, log10_concentration,
            response_model, assay_response_raw, included, exclusion_reason, mask_reason
       FROM %s WHERE curve_id = $1 AND method = $2
      ORDER BY log10_concentration", .tbl("calib_standards")),
    params = list(curve_id, method))
}

#' Transformed BLANK points for a curve+method. Response only -- blanks have no
#' intrinsic concentration, so x-positioning is a plotting decision (see the
#' module's reference band). `response_model` is on the SAME scale as the
#' standards/grid. Split on `included` as with standards.
fetch_calib_blanks <- function(pool, curve_id, method) {
  .calib_q(pool, sprintf(
    "SELECT well, response_model, assay_response_raw, included, exclusion_reason, mask_reason
       FROM %s WHERE curve_id = $1 AND method = $2", .tbl("calib_blanks")),
    params = list(curve_id, method))
}


# 5. Back-calculated samples


#' Per-sample back-calculated concentrations for a curve+method.
#' predicted_concentration is on the curve; final_concentration is x dilution.
#' The '__none__' identity sentinels are decoded back to NA on the way out.
fetch_calib_samples <- function(pool, curve_id, method) {
  df <- .calib_q(pool, sprintf(
    "SELECT sampleid, patientid, timeperiod, dilution,
            predicted_concentration, final_concentration, se_concentration,
            pcov, pcov_rmse, pcov_pass
       FROM %s WHERE curve_id = $1 AND method = $2", .tbl("calib_samples")),
    params = list(curve_id, method))
  .decode_none(df, c("sampleid", "patientid", "timeperiod", "dilution"))
}


# 6. Diagnostics + LOQ/LOD/RDL bounds


#' The single diagnostics row for a curve+method (34 cols: LLOQ/ULOQ on both
#' scales, shape-based LOQ, inflection +/- CI, LOD, MDC, RDL, pcov threshold).
fetch_calib_diagnostics <- function(pool, curve_id, method) {
  .calib_q(pool, sprintf(
    "SELECT * FROM %s WHERE curve_id = $1 AND method = $2 LIMIT 1",
    .tbl("calib_diagnostics")),
    params = list(curve_id, method))
}

#' Pull LLOQ/ULOQ on the scale the caller needs. `scale = "conc"` gives a value
#' comparable to the old bayes_curves.lloq/uloq (raw concentration); "log10"
#' gives the values to place on a log10 plot axis. Returns list(lloq, uloq).
#' `diag` is a row from fetch_calib_diagnostics().
calib_loq <- function(diag, scale = c("conc", "log10"),
                      which = c("precision", "shape")) {
  scale <- match.arg(scale)
  which <- match.arg(which)
  if (is.null(diag) || !nrow(diag)) return(list(lloq = NA, uloq = NA))
  suffix <- if (scale == "conc") "_conc" else "_log10"
  prefix <- if (which == "shape") "shape_" else ""
  lcol <- paste0(prefix, "lloq", suffix)
  ucol <- paste0(prefix, "uloq", suffix)
  # missing shape_* cols on older diagnostics rows -> NA, not an error.
  list(lloq = if (lcol %in% names(diag)) diag[[lcol]][1] else NA_real_,
       uloq = if (ucol %in% names(diag)) diag[[ucol]][1] else NA_real_)
}


# 7. Run / job metadata


#' Run-level metadata for a job_id (method, package, version, best_model,
#' params jsonb, status, started_at/finished_at).
fetch_calib_run <- function(pool, job_id) {
  .calib_q(pool, sprintf(
    "SELECT * FROM %s WHERE job_id = $1", .tbl("calib_run")),
    params = list(job_id))
}


# 8. One-call bundle for the module


#' Everything the standard-curve view needs for one curve+method, in a single
#' list. This is the function the module server should call; it keeps the read
#' pattern in one place and one round of queries.
#' @return list(fit_best, fits, params, gates, grid, samples, diagnostics, loo)
fetch_calib_bundle <- function(pool, curve_id, method) {
  best <- fetch_calib_best_model(pool, curve_id, method)
  mdl  <- if (nrow(best)) best$model_name[1] else NULL
  list(
    curve_id    = curve_id,
    method      = method,
    fit_best    = best,
    fits        = fetch_calib_fit(pool, curve_id, method),
    params      = fetch_calib_params(pool, curve_id, method, mdl),
    gates       = fetch_calib_gates(pool, curve_id, method, mdl),
    grid        = fetch_calib_grid(pool, curve_id, method),
    samples     = fetch_calib_samples(pool, curve_id, method),
    diagnostics = fetch_calib_diagnostics(pool, curve_id, method),
    loo         = fetch_calib_loo(pool, curve_id, method)
  )
}


# 9. Raw input reads for the Data tab (BASE tables, masks VISIBLE)
# -----------------------------------------------------------------------------
# The Data tab is the audit/transparency surface, so it reads the BASE xmap_*
# tables -- which keep the masked + mask_reason columns and ALL rows -- NOT the
# *_unmasked views (those drop masks and hide rows; they are the WORKER's fitting
# lens). Same data, two lenses: worker fits on unmasked; Data tab shows/export
# everything with masks flagged. Always scoped by study/experiment/project so we
# never pull whole multi-million-row tables (xmap_sample is ~1.7M rows).
CALIB_RAW_TABLES <- c(header = "xmap_header", standard = "xmap_standard",
                      control = "xmap_control", blank = "xmap_buffer",
                      sample = "xmap_sample")

.fetch_raw_scoped <- function(pool, project, study, experiment, tbl) {
  .calib_q(pool, sprintf(
    "SELECT * FROM %s
      WHERE project_id = $1 AND study_accession = $2 AND experiment_accession = $3", .tbl(tbl)),
    params = list(project, study, experiment))   # $1 project, $2 study, $3 experiment
}
fetch_raw_header   <- function(pool, project, study, experiment) .fetch_raw_scoped(pool, project, study, experiment, "xmap_header")
fetch_raw_standard <- function(pool, project, study, experiment) .fetch_raw_scoped(pool, project, study, experiment, "xmap_standard")
fetch_raw_control  <- function(pool, project, study, experiment) .fetch_raw_scoped(pool, project, study, experiment, "xmap_control")
fetch_raw_blank    <- function(pool, project, study, experiment) .fetch_raw_scoped(pool, project, study, experiment, "xmap_buffer")
fetch_raw_sample   <- function(pool, project, study, experiment) .fetch_raw_scoped(pool, project, study, experiment, "xmap_sample")

# calib_* rows for a whole study/experiment, NK-denormalized (curve_lookup
# columns prepended so every row is self-describing). Joins BASE curve_lookup so
# nothing is hidden on the audit surface. calib_run is keyed on job_id, not
# curve_id -- fetch it separately with fetch_calib_run().
.fetch_calib_scoped <- function(pool, project, study, experiment, tbl) {
  .calib_q(pool, sprintf(
    "SELECT cl.project_id, cl.study_accession, cl.experiment_accession,
            cl.plateid, cl.plate, cl.nominal_sample_dilution, cl.feature,
            cl.antigen, cl.source, cl.wavelength, t.*
       FROM %s t
       JOIN %s cl ON cl.curve_id = t.curve_id
      WHERE cl.project_id = $1 AND cl.study_accession = $2 AND cl.experiment_accession = $3
      ORDER BY cl.antigen, cl.plateid, t.curve_id",
    .tbl(tbl), .tbl("curve_lookup")),
    params = list(project, study, experiment))   # $1 project, $2 study, $3 experiment
}
fetch_calib_fit_scoped         <- function(pool, project, study, experiment) .fetch_calib_scoped(pool, project, study, experiment, "calib_fit")
fetch_calib_param_scoped       <- function(pool, project, study, experiment) .fetch_calib_scoped(pool, project, study, experiment, "calib_param")
fetch_calib_gate_scoped        <- function(pool, project, study, experiment) .fetch_calib_scoped(pool, project, study, experiment, "calib_gate")
# calib_grid is by far the heaviest Data-tab load (132k rows). The server-side
# query is fast (~0.3s); the cost is RPostgres parsing ~13 arbitrary-precision
# `numeric` columns as text. Casting them to float8 is the fix -- RPostgres
# already returns numeric AS an R double, so this changes NOTHING downstream
# (same values, display, CSV) but swaps the slow text parse for the fast double
# path (verified ~40s -> ~1s). Column set + order match .fetch_calib_scoped.
fetch_calib_grid_scoped <- function(pool, project, study, experiment, display_limit = NULL) {
  lim <- if (!is.null(display_limit) && is.finite(display_limit))
           sprintf(" LIMIT %d", as.integer(display_limit)) else ""
  .calib_q(pool, sprintf(
    "SELECT cl.project_id, cl.study_accession, cl.experiment_accession,
            cl.plateid, cl.plate, cl.nominal_sample_dilution, cl.feature,
            cl.antigen, cl.source, cl.wavelength,
            t.curve_id, t.method, t.point_index, t.model_name,
            t.log10_concentration::float8     AS log10_concentration,
            t.concentration::float8           AS concentration,
            t.predicted_response::float8      AS predicted_response,
            t.ci_lower::float8                AS ci_lower,
            t.ci_upper::float8                AS ci_upper,
            t.predicted_concentration::float8 AS predicted_concentration,
            t.se_concentration::float8        AS se_concentration,
            t.pcov::float8                    AS pcov,
            t.pcov_rmse::float8               AS pcov_rmse,
            t.pcov_pass,
            t.d2y_dx2::float8                 AS d2y_dx2,
            t.noise_mode, t.job_id, t.created_at
       FROM %s t
       JOIN %s cl ON cl.curve_id = t.curve_id
      WHERE cl.project_id = $1 AND cl.study_accession = $2 AND cl.experiment_accession = $3
      ORDER BY cl.antigen, cl.plateid, t.curve_id%s",
    .tbl("calib_grid"), .tbl("curve_lookup"), lim),
    params = list(project, study, experiment))
}
fetch_calib_samples_scoped     <- function(pool, project, study, experiment) .fetch_calib_scoped(pool, project, study, experiment, "calib_samples")
fetch_calib_diagnostics_scoped <- function(pool, project, study, experiment) .fetch_calib_scoped(pool, project, study, experiment, "calib_diagnostics")
fetch_calib_loo_scoped         <- function(pool, project, study, experiment) .fetch_calib_scoped(pool, project, study, experiment, "calib_loo")

# curve_lookup registry rows for a study/experiment (unmasked view; masked
# curves excluded so they can't be offered as fit targets).
fetch_curve_lookup_scoped <- function(pool, project, study, experiment) {
  if (is.null(project) || is.na(project))
    stop("fetch_curve_lookup_scoped: project_id is required")
  .calib_q(pool, sprintf(
    "SELECT curve_id, %s FROM %s
      WHERE project_id = $1 AND study_accession = $2 AND experiment_accession = $3
      ORDER BY antigen, plateid",
    paste(CALIB_NK_COLS, collapse = ", "), .tbl(TBL_CURVE_LOOKUP)),
    params = list(project, study, experiment))
}

# calib_run rows for a study/experiment. calib_run has no study/experiment/
# curve_id columns (it's per job_id), so reach it through calib_fit -> curve_lookup.
fetch_calib_run_scoped <- function(pool, project, study, experiment) {
  .calib_q(pool, sprintf(
    "SELECT DISTINCT r.*
       FROM %s r
       JOIN %s f  ON f.job_id  = r.job_id
       JOIN %s cl ON cl.curve_id = f.curve_id
      WHERE cl.project_id = $1 AND cl.study_accession = $2 AND cl.experiment_accession = $3
      ORDER BY r.started_at DESC NULLS LAST",
    .tbl("calib_run"), .tbl("calib_fit"), .tbl("curve_lookup")),
    params = list(project, study, experiment))
}

# Calculation status for an experiment: one row per (registered curve x method)
# with the BEST fit's outcome, INCLUDING curves that have not been computed yet
# (method/model NULL). This is the assay-agnostic replacement for the old
# hierarchical "run freq/bayes + status" panel -- the worker runs; this reports.
# Uses the unmasked curve registry (masked curves aren't fit, so aren't shown).
fetch_calc_status_scoped <- function(pool, project, study, experiment) {
  .calib_q(pool, sprintf(
    "SELECT cl.curve_id, cl.antigen, cl.plateid, cl.plate, cl.feature,
            cl.source, cl.wavelength,
            f.method, f.model_name AS best_model, f.converged, f.eligible,
            f.score_type, f.selection_score, f.job_id,
            r.status AS job_status, r.finished_at
       FROM %s cl
       LEFT JOIN %s f ON f.curve_id = cl.curve_id AND f.is_best
       LEFT JOIN %s r ON r.job_id  = f.job_id
      WHERE cl.project_id = $1 AND cl.study_accession = $2 AND cl.experiment_accession = $3
      ORDER BY cl.antigen, cl.plateid, cl.feature, f.method",
    .tbl(TBL_CURVE_LOOKUP), .tbl("calib_fit"), .tbl("calib_run")),
    params = list(project, study, experiment))
}

# Observed standard-curve points for ONE curve, with mask status, so the plot
# can overlay them and show which were excluded from fitting. xmap_standard has
# no curve_id, so points are matched to the curve via the curve_lookup natural
# key (this also naturally returns the wavelength-subtracted "delta" points for
# ELISA curves, since those carry the curve's NK). The response is returned under
# the canonical name assay_response (see assay_response.R).
fetch_standard_points <- function(pool, curve_id) {
  .calib_q(pool, sprintf(
    "SELECT s.well, s.dilution, s.antibody_mfi AS assay_response,
            s.wavelength, s.masked, s.mask_reason
       FROM %s s JOIN %s cl ON %s
      WHERE cl.curve_id = $1
      ORDER BY s.dilution",
    .tbl("xmap_standard"), .tbl("curve_lookup"),
    .nk_join_on(STD_NK_JOIN_COLS, cl = "cl", s = "s")),
    params = list(curve_id))
}


# MASKING resolvers (read-only). Turn staged plot points into the exact
# xmap_standard / xmap_buffer rows, and compute the calib_* delete blast radius
# for the affected multiplate_group. NO writes here -- these back the dry-run.
# The curve_lookup <-> raw xmap join uses the shared, sentinel-safe .nk_join_on()
# / *_NK_JOIN_COLS defined up top.


# All curve_ids fit jointly with `curve_id` (its whole multiplate_group). A mask
# invalidates the JOINT fit, so the delete scope is the entire group.
curve_group_members <- function(pool, curve_id) {
  df <- .calib_q(pool, sprintf(
    "SELECT c2.curve_id
       FROM %s c1 JOIN %s c2 USING (multiplate_group_id)
      WHERE c1.curve_id = $1", .tbl("curve_lookup"), .tbl("curve_lookup")),
    params = list(curve_id))
  if (!nrow(df)) integer(0) else as.integer(df$curve_id)
}

# A masked BLANK feeds every curve that joins to it source-LESSLY (blank source
# != curve source), so masking it invalidates EVERY multiplate group those
# curves belong to -- across standard sources AND, since calib_* is per-method,
# across methods (the delete is method-agnostic: it removes all calib_* rows for
# the affected curve_ids). Given the masked xmap_buffer ids, return every
# curve_id in every group any of those blanks feeds.
curve_ids_for_blanks <- function(pool, buffer_ids) {
  ids <- as.integer(buffer_ids[!is.na(buffer_ids)])
  if (!length(ids)) return(integer(0))
  idlist <- paste(ids, collapse = ",")
  df <- .calib_q(pool, sprintf(
    "WITH fed AS (
       SELECT DISTINCT cl.multiplate_group_id
         FROM %s b
         JOIN %s cl ON %s
        WHERE b.xmap_buffer_id IN (%s))
     SELECT c.curve_id
       FROM %s c JOIN fed USING (multiplate_group_id)",
    .tbl("xmap_buffer"), .tbl("curve_lookup"),
    .nk_join_on(BLK_NK_JOIN_COLS, cl = "cl", s = "b"),
    idlist, .tbl("curve_lookup")))
  if (!nrow(df)) integer(0) else as.integer(df$curve_id)
}

# The calib_* tables keyed on curve_id (deleted as a set on mask). calib_run is
# job-keyed (may span groups) and is intentionally NOT included.
CALIB_CURVE_TABLES <- c("calib_fit", "calib_param", "calib_gate", "calib_grid",
                        "calib_samples", "calib_diagnostics", "calib_standards",
                        "calib_blanks", "calib_loo")

# Row counts that WOULD be deleted for a set of curve_ids, per table (dry-run).
calib_group_rowcounts <- function(pool, curve_ids) {
  if (!length(curve_ids)) return(stats::setNames(integer(0), character(0)))
  ids <- paste(as.integer(curve_ids), collapse = ",")
  out <- vapply(CALIB_CURVE_TABLES, function(tb) {
    df <- .calib_q(pool, sprintf("SELECT count(*) n FROM %s WHERE curve_id IN (%s)",
                                 .tbl(tb), ids))
    if (nrow(df)) as.integer(df$n[1]) else 0L
  }, integer(1))
  out
}

# Resolve staged STANDARD points (keys "std|well|dilution") to xmap_standard_id
# via the curve's NK (source-IN, dilution used). Mirrors fetch_standard_points.
resolve_std_mask_ids <- function(pool, curve_id, wells, dilutions = NULL) {
  if (!length(wells)) return(integer(0))
  df <- .calib_q(pool, sprintf(
    "SELECT s.xmap_standard_id, s.well, s.dilution, s.masked
       FROM %s s JOIN %s cl ON %s
      WHERE cl.curve_id = $1",
    .tbl("xmap_standard"), .tbl("curve_lookup"),
    .nk_join_on(STD_NK_JOIN_COLS, cl = "cl", s = "s")),
    params = list(curve_id))
  if (!nrow(df)) return(integer(0))
  # Match on WELL alone. The join already pins ONE curve (a single plate /
  # antigen / source / wavelength / nominal-dilution), and xmap_standard has one
  # row per standard well within that scope, so `well` uniquely identifies the
  # point -- exactly as resolve_blk_mask_ids() keys blanks on `well`.
  #
  # We deliberately do NOT also require the staged `dilution` to string-equal
  # xmap_standard.dilution. The staged value originates in calib_standards
  # (worker output) and can differ in representation from the raw xmap_standard
  # value -- numeric vs text, scientific notation (1e+05 vs 100000), or simply
  # absent, in which case the staged key's trailing "|<dil>" segment is empty and
  # strsplit() drops it. That extra equality made every standard fail to resolve,
  # surfacing as the misleading "Nothing resolved to mask." The `dilutions`
  # argument is retained for call-site compatibility but is no longer a filter.
  keep <- as.character(df$well) %in% as.character(wells)
  as.integer(df$xmap_standard_id[keep])
}

# Resolve staged BLANK points (keys "blk|well|") to xmap_buffer_id via the NK
# MINUS source (blank source != curve source), well only, NO dilution.
resolve_blk_mask_ids <- function(pool, curve_id, wells) {
  if (!length(wells)) return(integer(0))
  df <- .calib_q(pool, sprintf(
    "SELECT b.xmap_buffer_id, b.well, b.masked
       FROM %s b JOIN %s cl ON %s
      WHERE cl.curve_id = $1",
    .tbl("xmap_buffer"), .tbl("curve_lookup"),
    .nk_join_on(BLK_NK_JOIN_COLS, cl = "cl", s = "b")),
    params = list(curve_id))
  if (!nrow(df)) return(integer(0))
  as.integer(df$xmap_buffer_id[df$well %in% wells])
}


# Read-only DIAGNOSTIC for the masking UI. Explains, in one structured object,
# exactly what the standard/blank resolvers see for a given curve + staged wells,
# so a failed resolution can be understood from the modal instead of guessed at.
# It runs the SAME sentinel-safe NK join as the resolvers, but ALSO reports the
# raw row counts, the wells the join exposes, the wells calib_standards holds
# (i.e. what the plot/staging is built from), and the intersection the resolver
# would actually keep. This separates the two failure modes cleanly:
#   * join_rows == 0            -> the NK join itself finds nothing in-app
#                                  (stale build, param binding, or curve_id miss)
#   * join_rows > 0 but no match -> the staged `well` strings differ from the raw
#                                  xmap `well` strings (representation mismatch)
# NO writes. Types coerced to character so int64/int/text all compare cleanly.
diagnose_mask_resolution <- function(pool, curve_id, std_wells = character(0),
                                     blk_wells = character(0)) {
  chr <- function(x) if (length(x)) sort(unique(as.character(x))) else character(0)
  wells_of <- function(df) if (!is.null(df) && nrow(df) && "well" %in% names(df))
                             chr(df$well) else character(0)

  std_join <- .calib_q(pool, sprintf(
    "SELECT s.xmap_standard_id, s.well
       FROM %s s JOIN %s cl ON %s
      WHERE cl.curve_id = $1",
    .tbl("xmap_standard"), .tbl("curve_lookup"),
    .nk_join_on(STD_NK_JOIN_COLS, cl = "cl", s = "s")),
    params = list(curve_id))
  blk_join <- .calib_q(pool, sprintf(
    "SELECT b.xmap_buffer_id, b.well
       FROM %s b JOIN %s cl ON %s
      WHERE cl.curve_id = $1",
    .tbl("xmap_buffer"), .tbl("curve_lookup"),
    .nk_join_on(BLK_NK_JOIN_COLS, cl = "cl", s = "b")),
    params = list(curve_id))
  cl_row <- .calib_q(pool, sprintf(
    "SELECT curve_id FROM %s WHERE curve_id = $1 LIMIT 1", .tbl("curve_lookup")),
    params = list(curve_id))
  cs_wells <- .calib_q(pool, sprintf(
    "SELECT DISTINCT well FROM %s WHERE curve_id = $1", .tbl("calib_standards")),
    params = list(curve_id))

  std_wells <- chr(std_wells); blk_wells <- chr(blk_wells)
  list(
    curve_id         = as.character(curve_id),
    curve_in_lookup  = nrow(cl_row) > 0,
    std_join_rows    = nrow(std_join),
    std_join_wells   = wells_of(std_join),
    calib_std_wells  = wells_of(cs_wells),
    staged_std_wells = std_wells,
    std_matched      = intersect(std_wells, wells_of(std_join)),
    blk_join_rows    = nrow(blk_join),
    blk_join_wells   = wells_of(blk_join),
    staged_blk_wells = blk_wells,
    blk_matched      = intersect(blk_wells, wells_of(blk_join)))
}


# MASKING write (TRANSACTIONAL). The ONE destructive call: set masked/mask_reason
# on the resolved xmap rows, then delete ALL calib_* fits for the affected
# multiplate group (a mask invalidates the joint fit). All-or-nothing: any error
# rolls back so there is never a half-masked / half-deleted state.
#
# std_ids / blk_ids : integer xmap_standard_id / xmap_buffer_id (from the
#   resolvers). group_curve_ids : every curve_id in the group (from
#   curve_group_members). reason : required, written to every masked row.
# Returns list(ok, masked_std, masked_blk, deleted, group_n) or stops on error.

apply_mask <- function(pool, std_ids, blk_ids, group_curve_ids, reason,
                       set_masked = TRUE) {
  reason <- trimws(if (is.null(reason)) "" else as.character(reason)[1])
  if (!nzchar(reason)) stop("apply_mask: a non-empty reason is required.")
  std_ids <- as.integer(std_ids[!is.na(std_ids)])
  blk_ids <- as.integer(blk_ids[!is.na(blk_ids)])
  grp     <- as.integer(group_curve_ids[!is.na(group_curve_ids)])
  if (!length(std_ids) && !length(blk_ids))
    stop("apply_mask: no rows resolved to mask.")
  if (!length(grp))
    stop("apply_mask: empty multiplate group (nothing to invalidate).")

  # The transaction body, run against a SINGLE real DBI connection `co`.
  do_txn <- function(co) {
    DBI::dbBegin(co)
    tryCatch({
      n_std <- 0L; n_blk <- 0L
      if (length(std_ids)) {
        idlist <- paste(std_ids, collapse = ",")
        n_std <- DBI::dbExecute(co, sprintf(
          "UPDATE %s SET masked = $1, mask_reason = $2 WHERE xmap_standard_id IN (%s)",
          .tbl("xmap_standard"), idlist), params = list(set_masked, reason))
      }
      if (length(blk_ids)) {
        idlist <- paste(blk_ids, collapse = ",")
        n_blk <- DBI::dbExecute(co, sprintf(
          "UPDATE %s SET masked = $1, mask_reason = $2 WHERE xmap_buffer_id IN (%s)",
          .tbl("xmap_buffer"), idlist), params = list(set_masked, reason))
      }
      grplist <- paste(grp, collapse = ",")
      deleted <- stats::setNames(integer(length(CALIB_CURVE_TABLES)), CALIB_CURVE_TABLES)
      for (tb in CALIB_CURVE_TABLES) {
        deleted[[tb]] <- DBI::dbExecute(co, sprintf(
          "DELETE FROM %s WHERE curve_id IN (%s)", .tbl(tb), grplist))
      }
      DBI::dbCommit(co)
      list(ok = TRUE, masked_std = n_std, masked_blk = n_blk,
           deleted = deleted, group_n = length(grp))
    }, error = function(e) {
      DBI::dbRollback(co)
      stop(sprintf("apply_mask failed (rolled back): %s", conditionMessage(e)), call. = FALSE)
    })
  }

  # A pool cannot run a transaction directly (each call may get a different
  # physical connection). Check out ONE connection for the whole transaction and
  # return it after. Works whether `pool` is a pool or a bare DBI connection.
  if (inherits(pool, "Pool")) {
    co <- pool::poolCheckout(pool)
    on.exit(pool::poolReturn(co), add = TRUE)
    do_txn(co)
  } else {
    do_txn(pool)
  }
}


# UNMASKING write (TRANSACTIONAL). The inverse of apply_mask: clear masked and
# mask_reason on the resolved xmap rows, then delete ALL calib_* fits for the
# affected multiplate group. Unmasking changes the fit's input set, so the
# existing joint fit is stale and must be recomputed -- same invalidation as a
# mask (hence the same group delete). Differences vs apply_mask: no reason is
# required (unmasking needs no justification), and mask_reason is CLEARED rather
# than written. All-or-nothing: any error rolls back.
#
# std_ids / blk_ids : integer xmap_standard_id / xmap_buffer_id (from the SAME
#   resolvers used for masking -- they match on well regardless of mask state).
#   group_curve_ids : every curve_id in every affected group (curve_group_members
#   plus, for blanks, curve_ids_for_blanks).
# Returns list(ok, unmasked_std, unmasked_blk, deleted, group_n) or stops.
apply_unmask <- function(pool, std_ids, blk_ids, group_curve_ids) {
  std_ids <- as.integer(std_ids[!is.na(std_ids)])
  blk_ids <- as.integer(blk_ids[!is.na(blk_ids)])
  grp     <- as.integer(group_curve_ids[!is.na(group_curve_ids)])
  if (!length(std_ids) && !length(blk_ids))
    stop("apply_unmask: no rows resolved to unmask.")
  if (!length(grp))
    stop("apply_unmask: empty multiplate group (nothing to invalidate).")

  do_txn <- function(co) {
    DBI::dbBegin(co)
    tryCatch({
      n_std <- 0L; n_blk <- 0L
      if (length(std_ids)) {
        idlist <- paste(std_ids, collapse = ",")
        n_std <- DBI::dbExecute(co, sprintf(
          "UPDATE %s SET masked = FALSE, mask_reason = NULL WHERE xmap_standard_id IN (%s)",
          .tbl("xmap_standard"), idlist))
      }
      if (length(blk_ids)) {
        idlist <- paste(blk_ids, collapse = ",")
        n_blk <- DBI::dbExecute(co, sprintf(
          "UPDATE %s SET masked = FALSE, mask_reason = NULL WHERE xmap_buffer_id IN (%s)",
          .tbl("xmap_buffer"), idlist))
      }
      grplist <- paste(grp, collapse = ",")
      deleted <- stats::setNames(integer(length(CALIB_CURVE_TABLES)), CALIB_CURVE_TABLES)
      for (tb in CALIB_CURVE_TABLES) {
        deleted[[tb]] <- DBI::dbExecute(co, sprintf(
          "DELETE FROM %s WHERE curve_id IN (%s)", .tbl(tb), grplist))
      }
      DBI::dbCommit(co)
      list(ok = TRUE, unmasked_std = n_std, unmasked_blk = n_blk,
           deleted = deleted, group_n = length(grp))
    }, error = function(e) {
      DBI::dbRollback(co)
      stop(sprintf("apply_unmask failed (rolled back): %s", conditionMessage(e)), call. = FALSE)
    })
  }

  if (inherits(pool, "Pool")) {
    co <- pool::poolCheckout(pool)
    on.exit(pool::poolReturn(co), add = TRUE)
    do_txn(co)
  } else {
    do_txn(pool)
  }
}


# STANDARDS SUPPORT (read-only). Per curve: how many distinct standard levels
# (dilutions) and how much replication (wells per level). Drives the sparse-plate
# hint on the measurement-error toggle -- the measurement-error term is only
# trustworthy with several standards AND replication spanning the response range.
# Uses the SAME source-in NK join as the standards resolver.

# Per-curve counts for a scope: n_levels (distinct dilutions), n_std (rows),
# min_reps (fewest wells at any level). A curve is "well supported" when it has
# several levels and >1 rep per level; "thin" otherwise.
standards_support <- function(pool, project, study, experiment) {
  if (is.null(project) || is.na(project))
    stop("standards_support: project_id is required")
  .calib_q(pool, sprintf(
    "SELECT cl.curve_id,
            count(*)                         AS n_std,
            count(DISTINCT s.dilution)       AS n_levels,
            count(*)::numeric
              / NULLIF(count(DISTINCT s.dilution),0) AS avg_reps
       FROM %s cl JOIN %s s ON %s
      WHERE cl.project_id = $1 AND cl.study_accession = $2 AND cl.experiment_accession = $3
      GROUP BY cl.curve_id",
    .tbl("curve_lookup"), .tbl("xmap_standard"),
    .nk_join_on(STD_NK_JOIN_COLS, cl = "cl", s = "s")),
    params = list(project, study, experiment))
}

# Reduce the per-curve support to a single verdict for the experiment's hint.
# `min_levels`/`min_reps` thresholds are conservative defaults; a curve is thin
# if it has fewer than min_levels dilution levels OR avg replication < min_reps.
standards_support_verdict <- function(support_df, min_levels = 5, min_reps = 2) {
  if (is.null(support_df) || !nrow(support_df))
    return(list(thin = FALSE, n_curves = 0L, n_thin = 0L,
                worst_levels = NA_integer_, worst_reps = NA_real_))
  levels <- suppressWarnings(as.integer(support_df$n_levels))
  reps   <- suppressWarnings(as.numeric(support_df$avg_reps))
  thin_v <- (levels < min_levels) | (reps < min_reps)
  list(thin = any(thin_v, na.rm = TRUE),
       n_curves = nrow(support_df),
       n_thin = sum(thin_v, na.rm = TRUE),
       worst_levels = suppressWarnings(min(levels, na.rm = TRUE)),
       worst_reps   = suppressWarnings(min(reps, na.rm = TRUE)))
}

# =============================================================================
# FDA 2018 standard-curve classification (frequentist-pinned; method-agnostic)
# -----------------------------------------------------------------------------
# Cross-plate CV% + back-calculated recovery per concentration level, classified
# against FDA 2018 LBA calibration-curve criteria. Independent of the stored
# model-selection fits' method split: accuracy is back-calculated through the
# FREQUENTIST fit only, so the verdict is identical whichever method tab is
# shown. See REFACTOR_settings_cascade.md 11.3.
#
# Analytic inverses copied VERBATIM from curveRcore inverses.R (bodies unchanged;
# names prefixed .fda_inv_* so this in-app copy never masks the package's inv_*,
# mirroring how std_curve_compare_module.R inlines the forwards as .cmp_predict).
# Each solves for x on the SAME scale the forward model took x: log10(conc) for
# every model EXCEPT loglogistic4 (Hill form on RAW concentration).

.fda_inv_logistic4 <- function(y, a, b, c, d, tol = 1e-6) {
  lo <- min(a, d) + tol; hi <- max(a, d) - tol
  result <- rep(NA_real_, length(y))
  ok <- !is.na(y) & y > lo & y < hi
  if (any(ok)) result[ok] <- c + b * log((y[ok] - a) / (d - y[ok]))
  result
}
.fda_inv_logistic5 <- function(y, a, b, c, d, g, tol = 1e-6) {
  lo <- min(a, d) + tol; hi <- max(a, d) - tol
  result <- rep(NA_real_, length(y))
  ok <- !is.na(y) & y > lo & y < hi
  if (any(ok)) result[ok] <- c - b * log(((d - a) / (y[ok] - a))^(1 / g) - 1)
  result
}
.fda_inv_loglogistic4 <- function(y, a, b, c, d) {
  c / ((d - y) / (y - a))^(1 / b)
}
.fda_inv_loglogistic5 <- function(y, a, b, c, d, g) {
  c - (1 / b) * (log(((y - a) / (d - a))^(-g) - 1) - log(g))
}
.fda_inv_gompertz4 <- function(y, a, b, c, d) {
  c - (1 / b) * log(-log((y - a) / (d - a)))
}

# Response (model/fit scale) -> back-calculated NATURAL concentration, or NA if
# the response is off the curve / params are unusable. loglogistic4's inverse
# already returns concentration (raw x); the other four return log10(conc), so
# 10^ them. Any non-finite (incl. the NaN the unguarded inverses emit off-range)
# -> NA, i.e. an off-curve standard is uniformly "not invertible".
.fda_backcalc_conc <- function(model, y, a, b, c, d, g = NA_real_) {
  if (!is.finite(y) || !is.finite(a) || !is.finite(b) ||
      !is.finite(c) || !is.finite(d)) return(NA_real_)
  if (model %in% c("logistic5", "loglogistic5") && (!is.finite(g) || g <= 0))
    return(NA_real_)
  x <- suppressWarnings(tryCatch(switch(model,
    logistic4    = .fda_inv_logistic4(y, a, b, c, d),
    logistic5    = .fda_inv_logistic5(y, a, b, c, d, g),
    loglogistic4 = .fda_inv_loglogistic4(y, a, b, c, d),
    loglogistic5 = .fda_inv_loglogistic5(y, a, b, c, d, g),
    gompertz4    = .fda_inv_gompertz4(y, a, b, c, d),
    NA_real_), error = function(e) NA_real_))[1]
  if (!is.finite(x)) return(NA_real_)
  conc <- if (identical(model, "loglogistic4")) x else 10^x
  if (!is.finite(conc) || conc <= 0) NA_real_ else conc
}

# Point estimates (a,b,c,d,g) from a fetch_calib_params() frame; frequentist uses
# `estimate`; g absent on 4-param fits -> NA. Mirrors .cmp_get_params.
.fda_params <- function(pr) {
  if (is.null(pr) || !nrow(pr)) return(NULL)
  g1 <- function(t) {
    v <- suppressWarnings(as.numeric(pr$estimate[tolower(pr$term) == t]))
    if (length(v) && is.finite(v[1])) v[1] else NA_real_
  }
  list(a = g1("a"), b = g1("b"), c = g1("c"), d = g1("d"), g = g1("g"))
}

# FDA 2018 LBA thresholds. Interior levels: CV <= 20%, recovery in [80,120].
# The two EXTREME concentration levels (LLOQ & ULOQ ends): CV <= 25%, recovery in
# [75,125] (FDA 2018 BMV Appendix Table 1, ligand-binding-assay column).
FDA2018_CV_INTERIOR  <- 20
FDA2018_CV_EXTREME   <- 25
FDA2018_ACC_INTERIOR <- c(80, 120)
FDA2018_ACC_EXTREME  <- c(75, 125)

#' Classify each standard concentration level of a curve's multiplate group.
#' Precision = cross-plate CV% of raw response (SD/mean) at each level; accuracy
#' = median per-plate recovery, each plate back-calculated through ITS OWN
#' frequentist fit. A level fails accuracy if its response cannot be inverted
#' (off-curve) -- never NA for that reason. NA is reserved for levels with no
#' measured response at all. Returns list(levels, summary).
fda2018_classify_group <- function(pool, curve_id,
                                   cv_interior  = FDA2018_CV_INTERIOR,
                                   cv_extreme   = FDA2018_CV_EXTREME,
                                   acc_interior = FDA2018_ACC_INTERIOR,
                                   acc_extreme  = FDA2018_ACC_EXTREME) {
  num <- function(x) suppressWarnings(as.numeric(x))
  empty <- list(
    levels = data.frame(conc = numeric(0), log10c = numeric(0),
      n_plates = integer(0), n_invertible = integer(0), cv = numeric(0),
      recovery = numeric(0), cv_pass = logical(0), acc_pass = logical(0),
      flag = character(0), stringsAsFactors = FALSE),
    summary = list(n_pass = 0L, n_total = 0L, pct_pass = NA_real_,
      lloq_conc = NA_real_, uloq_conc = NA_real_, meets_fda_run = FALSE,
      status = "NO_DATA"))
  fail <- function(status) { empty$summary$status <- status; empty }

  members <- tryCatch(curve_group_members(pool, curve_id), error = function(e) integer(0))
  members <- members[!is.na(members)]
  if (!length(members)) return(fail("NO_DATA"))

  # ONE set-based read per table for the WHOLE multiplate group (was 3 queries
  # PER plate). Fewer round-trips, and a DB blip prints a single failure and hides
  # the ribbon instead of cascading N times. members are integers from
  # curve_group_members, so the IN-list is injection-safe (cf. curve_ids_for_blanks).
  idlist <- paste(members, collapse = ",")
  bm_all <- .calib_q(pool, sprintf(
    "SELECT curve_id, model_name FROM %s
      WHERE curve_id IN (%s) AND method = 'frequentist' AND is_best",
    .tbl("calib_fit"), idlist))
  if (!nrow(bm_all)) return(fail("NO_FREQ_FIT"))
  pr_all <- .calib_q(pool, sprintf(
    "SELECT p.curve_id, p.term, p.estimate
       FROM %s p JOIN %s f USING (curve_id, method, model_name)
      WHERE p.method = 'frequentist' AND f.is_best AND p.curve_id IN (%s)",
    .tbl("calib_param"), .tbl("calib_fit"), idlist))
  sp_all <- .calib_q(pool, sprintf(
    "SELECT curve_id, log10_concentration, concentration,
            response_model, assay_response_raw, included
       FROM %s WHERE curve_id IN (%s) AND method = 'frequentist'",
    .tbl("calib_standards"), idlist))
  if (!nrow(sp_all)) return(fail("NO_DATA"))

  key <- function(v) as.character(v)                 # robust int64/int/num match
  bm_all$k <- key(bm_all$curve_id)
  if (nrow(pr_all)) pr_all$k <- key(pr_all$curve_id)
  sp_all$k <- key(sp_all$curve_id)

  # Per plate (= per curve_id): back-calc each level through THAT plate's own
  # frequentist fit; collect a long table of (plate, level, nominal, raw, recovery).
  rows <- list()
  for (kk in unique(bm_all$k)) {
    model <- bm_all$model_name[bm_all$k == kk][1]
    prc   <- if (nrow(pr_all)) pr_all[pr_all$k == kk, , drop = FALSE] else pr_all
    p     <- .fda_params(prc)
    sp    <- sp_all[sp_all$k == kk, , drop = FALSE]
    if (!nrow(sp)) next
    inc <- sp[is.na(sp$included) | as.logical(sp$included), , drop = FALSE]
    if (!nrow(inc)) next
    inc$level <- round(num(inc$log10_concentration), 4)
    for (lv in unique(inc$level[is.finite(inc$level)])) {
      gg   <- inc[inc$level == lv, , drop = FALSE]
      raw  <- mean(num(gg$assay_response_raw), na.rm = TRUE)   # cross-plate CV uses raw
      rmod <- mean(num(gg$response_model),     na.rm = TRUE)   # back-calc uses model scale
      conc <- stats::median(num(gg$concentration), na.rm = TRUE)
      rec  <- if (is.null(p)) NA_real_ else {
        bc <- .fda_backcalc_conc(model, rmod, p$a, p$b, p$c, p$d, p$g)
        if (is.finite(bc) && is.finite(conc) && conc > 0) 100 * bc / conc else NA_real_
      }
      rows[[length(rows) + 1]] <- data.frame(plate = kk, level = lv,
        conc = conc, raw = raw, recovery = rec, stringsAsFactors = FALSE)
    }
  }
  if (!length(rows)) return(fail("NO_DATA"))
  long <- do.call(rbind, rows)

  lv_keys <- sort(unique(long$level))
  lv_conc <- vapply(lv_keys, function(k) stats::median(long$conc[long$level == k], na.rm = TRUE), numeric(1))
  ord <- order(lv_conc)
  is_extreme <- rep(FALSE, length(lv_keys))
  if (length(lv_keys) >= 1) is_extreme[ord[1]] <- TRUE                    # lowest conc
  if (length(lv_keys) >= 2) is_extreme[ord[length(lv_keys)]] <- TRUE      # highest conc

  lev <- lapply(seq_along(lv_keys), function(i) {
    k <- lv_keys[i]
    d <- long[long$level == k, , drop = FALSE]
    raws <- d$raw[is.finite(d$raw)]
    recs <- d$recovery[is.finite(d$recovery)]
    n_plates <- length(raws); n_inv <- length(recs)
    mm <- mean(raws); s <- stats::sd(raws)
    cv <- if (n_plates >= 2 && is.finite(mm) && abs(mm) > .Machine$double.eps) 100 * s / abs(mm) else NA_real_
    recovery <- if (n_inv >= 1) stats::median(recs) else NA_real_
    ext <- is_extreme[i]
    cv_lim <- if (ext) cv_extreme else cv_interior
    acc_lo <- if (ext) acc_extreme[1] else acc_interior[1]
    acc_hi <- if (ext) acc_extreme[2] else acc_interior[2]
    cv_pass <- if (is.na(cv)) NA else cv <= cv_lim
    acc_pass <- if (n_plates == 0) NA else if (n_inv == 0) FALSE else (recovery >= acc_lo & recovery <= acc_hi)
    flag <- if (n_plates == 0 || is.na(acc_pass)) "NA"
            else if (is.na(cv_pass)) { if (isTRUE(acc_pass)) "PASS" else "FAIL_ACC" }
            else if ( acc_pass &&  cv_pass) "PASS"
            else if (!acc_pass &&  cv_pass) "FAIL_ACC"
            else if ( acc_pass && !cv_pass) "FAIL_CV"
            else "FAIL_BOTH"
    data.frame(conc = lv_conc[i], log10c = k, n_plates = n_plates,
      n_invertible = n_inv, cv = cv, recovery = recovery,
      cv_pass = cv_pass, acc_pass = acc_pass, flag = flag, stringsAsFactors = FALSE)
  })
  levels_df <- do.call(rbind, lev)
  levels_df <- levels_df[order(levels_df$conc), , drop = FALSE]

  evaluable <- levels_df[levels_df$flag != "NA", , drop = FALSE]
  passing   <- evaluable[evaluable$flag == "PASS", , drop = FALSE]
  n_total <- nrow(evaluable); n_pass <- nrow(passing)
  summary <- list(
    n_pass = as.integer(n_pass), n_total = as.integer(n_total),
    pct_pass = if (n_total) round(100 * n_pass / n_total, 1) else NA_real_,
    lloq_conc = if (n_pass) min(passing$conc) else NA_real_,
    uloq_conc = if (n_pass) max(passing$conc) else NA_real_,
    meets_fda_run = (n_pass >= 6) && (n_total > 0) && (n_pass / n_total >= 0.75),
    status = if (n_pass > 0) "OK" else "NO_PASSING_LEVELS")
  list(levels = levels_df, summary = summary)
}

# Row counts for scoped Results tables in ONE round-trip (was: full-table loads
# just to test emptiness in the Data-tab status). Every calib_* Results table
# joins curve_lookup by curve_id EXCEPT calib_run (job_id -> calib_fit ->
# curve_lookup). Positional params $1/$2/$3 are reused across the UNION, which
# Postgres allows. Returns a named integer vector (NA for a table that errored).
fetch_scoped_table_counts <- function(pool, project, study, experiment, tables) {
  tables <- tables[!is.na(tables) & nzchar(tables)]
  if (!length(tables)) return(integer(0))
  cl <- .tbl("curve_lookup")
  scope <- "c.project_id = $1 AND c.study_accession = $2 AND c.experiment_accession = $3"
  sub <- vapply(tables, function(tb) {
    if (identical(tb, "calib_run"))
      sprintf("SELECT '%s'::text AS tbl, count(*) AS n FROM %s r JOIN %s f ON f.job_id = r.job_id JOIN %s c ON c.curve_id = f.curve_id WHERE %s",
              tb, .tbl("calib_run"), .tbl("calib_fit"), cl, scope)
    else
      sprintf("SELECT '%s'::text AS tbl, count(*) AS n FROM %s t JOIN %s c ON c.curve_id = t.curve_id WHERE %s",
              tb, .tbl(tb), cl, scope)
  }, character(1))
  df <- .calib_q(pool, paste(sub, collapse = " UNION ALL "),
                 params = list(project, study, experiment))
  out <- setNames(rep(NA_integer_, length(tables)), tables)
  if (nrow(df)) out[df$tbl] <- as.integer(df$n)
  out
}
