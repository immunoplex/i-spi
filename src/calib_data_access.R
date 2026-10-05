
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

# PLATE-scope join keys. Masking a WELL for contamination affects every analyte
# read from that well, so the plate-scope resolvers drop antigen AND feature from
# the natural key and match on the remaining physical-well dimensions + well:
# project/study/experiment, plateid+plate, nominal_sample_dilution, source (for
# standards; blanks already ignore source), and wavelength. Everything the caller
# fixes is held constant; only antigen/feature vary. Swapping these in for the
# *_NK_JOIN_COLS is the ONLY change that turns a one-analyte resolve into a
# whole-well resolve -- the query shape is identical.
STD_PLATE_JOIN_COLS <- setdiff(STD_NK_JOIN_COLS, c("antigen", "feature"))
BLK_PLATE_JOIN_COLS <- setdiff(BLK_NK_JOIN_COLS, c("antigen", "feature"))


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
#'
#' NOTE (both this and fetch_calib_standards above): `included`/`mask_reason`
#' here are a SNAPSHOT written by the worker at fit time, not a live read of
#' xmap_standard.masked / xmap_buffer.masked. They go stale the moment a mask/
#' unmask save defers its recompute (see calib_recalc_flag / delete_fits).
#' Anything that needs to know whether a point is masked RIGHT NOW -- deciding
#' fill-vs-hollow on the plot, routing a click to mask vs. unmask -- must use
#' fetch_live_mask_state() instead; use these two only for the response/
#' concentration VALUES to plot, not for current mask status.
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

# Per-sample curveRweights precision weights (calib_weights) for a study/
# experiment. Same shape/key as calib_samples -- curve_id-keyed, so the
# existing .fetch_calib_scoped() factory applies directly.
fetch_calib_weights_scoped <- function(pool, project, study, experiment) .fetch_calib_scoped(pool, project, study, experiment, "calib_weights")

# Per-(multiplate_group_id, method) weights FIT summary (phi/beta1/...) for a
# study/experiment. Unlike every other *_scoped fetcher above, calib_weights_fit
# is keyed by multiplate_group_id, not curve_id, so it needs its own join
# (.fetch_calib_scoped's `t.curve_id = cl.curve_id` join doesn't apply) --
# DISTINCT because several curve_lookup rows (one per plate) share one group.
fetch_calib_weights_fit_scoped <- function(pool, project, study, experiment) {
  .calib_q(pool, sprintf(
    "SELECT DISTINCT cwf.*
       FROM %s cwf
       JOIN %s cl ON cl.multiplate_group_id = cwf.multiplate_group_id
      WHERE cl.project_id = $1 AND cl.study_accession = $2 AND cl.experiment_accession = $3
      ORDER BY cwf.multiplate_group_id, cwf.method",
    .tbl("calib_weights_fit"), .tbl("curve_lookup")),
    params = list(project, study, experiment))
}

# Weights-computation status for the "Compute weights" status box: one row per
# (curve x method) exactly like fetch_calc_status_scoped(), LEFT JOINed so an
# uncomputed combo shows NULL rather than being absent. Unlike
# fetch_calc_status_scoped(), there is no calib_run join -- the worker never
# writes one for a weights job (see worker_weights.R) -- so "has a
# calib_weights_fit row" (method non-NULL) IS the completion signal;
# computed_at substitutes for finished_at. In-flight (queued/running) weights
# jobs are reported separately, by the shared queue-view box (same
# list_jobs()-based mechanism std_curve_calc_module.R already uses), not here.
fetch_weights_status_scoped <- function(pool, project, study, experiment) {
  .calib_q(pool, sprintf(
    "SELECT cl.curve_id, cl.antigen, cl.plateid, cl.plate, cl.feature,
            cl.source, cl.wavelength, cl.multiplate_group_id,
            cwf.method, cwf.phi, cwf.beta1, cwf.interpretation,
            cwf.design_cols, cwf.n_eff, cwf.weight_ratio, cwf.job_id,
            cwf.created_at AS computed_at
       FROM %s cl
       LEFT JOIN %s cwf ON cwf.multiplate_group_id = cl.multiplate_group_id
      WHERE cl.project_id = $1 AND cl.study_accession = $2 AND cl.experiment_accession = $3
      ORDER BY cl.antigen, cl.plateid, cl.feature, cwf.method",
    .tbl("curve_lookup"), .tbl("calib_weights_fit")),
    params = list(project, study, experiment))
}

# Full calib_samples rows for a study/experiment/method, optionally scoped
# further to one antigen/feature, joined to curve_lookup for antigen/feature/
# plateid context columns. Feeds the "Compute weights" Excel download (the
# analyst-facing calib_samples template for filling in missing agroup) --
# NOT used for fitting (worker_weights.R reads calib_samples directly from
# Postgres itself; this is purely for the UI's download/edit/upload round-trip).
fetch_calib_samples_for_scope <- function(pool, project, study, experiment, method,
                                          feature = NULL, antigen = NULL) {
  where <- "cl.project_id = $1 AND cl.study_accession = $2
            AND cl.experiment_accession = $3 AND cs.method = $4"
  params <- list(project, study, experiment, method)
  if (!is.null(feature)) { params <- c(params, list(feature))
    where <- paste0(where, sprintf(" AND cl.feature = $%d", length(params))) }
  if (!is.null(antigen)) { params <- c(params, list(antigen))
    where <- paste0(where, sprintf(" AND cl.antigen = $%d", length(params))) }
  .calib_q(pool, sprintf(
    "SELECT cl.antigen, cl.feature, cl.plateid, cs.*
       FROM %s cs
       JOIN %s cl ON cl.curve_id = cs.curve_id
      WHERE %s
      ORDER BY cl.antigen, cl.plateid, cs.sampleid",
    .tbl("calib_samples"), .tbl("curve_lookup"), where), params = params)
}

# timeperiod x agroup cross-tab + sample count for a study/experiment/method,
# scoped further to one antigen/feature when given (the weights job's actual
# submission scope). Drives the "Compute weights" design-readiness panel: a
# column with only one group (e.g. agroup all NULL) has no usable variation
# for curveRweights::as_weight_data()'s saturated-cell design.
fetch_design_readiness <- function(pool, project, study, experiment, method,
                                   feature = NULL, antigen = NULL) {
  where <- "cl.project_id = $1 AND cl.study_accession = $2
            AND cl.experiment_accession = $3 AND cs.method = $4"
  params <- list(project, study, experiment, method)
  if (!is.null(feature)) { params <- c(params, list(feature))
    where <- paste0(where, sprintf(" AND cl.feature = $%d", length(params))) }
  if (!is.null(antigen)) { params <- c(params, list(antigen))
    where <- paste0(where, sprintf(" AND cl.antigen = $%d", length(params))) }
  out <- .calib_q(pool, sprintf(
    "SELECT cs.timeperiod, cs.agroup, count(*) AS n
       FROM %s cs
       JOIN %s cl ON cl.curve_id = cs.curve_id
      WHERE %s
      GROUP BY cs.timeperiod, cs.agroup
      ORDER BY cs.timeperiod, cs.agroup",
    .tbl("calib_samples"), .tbl("curve_lookup"), where), params = params)
  # count(*) comes back as Postgres bigint -> RPostgres hands it back as
  # bit64::integer64 (no bigint= override on this app's db_pool). xtabs()'s
  # internal summation isn't bit64-aware and silently zeroes it out while the
  # factor labels (timeperiod/agroup, unaffected) stay correct -- exactly the
  # "right headers, all-zero cells" symptom seen in testing. Plain integer is
  # more than enough range for a sample count.
  if (nrow(out)) out$n <- as.integer(out$n)
  out
}

# timeperiod x agroup x method summary of COMPUTED weights (calib_weights.
# w_norm), scoped the same way as fetch_design_readiness(). One row per
# (method, timeperiod, agroup) actually represented in calib_weights -- a
# cell with no weights computed yet for that method simply has no row (the
# UI pivots this into a grid and leaves those cells blank). Aggregated in
# SQL (not pulled row-by-row into R) since calib_weights can run to
# thousands of rows for a whole-experiment scope.
fetch_weights_distribution <- function(pool, project, study, experiment,
                                       feature = NULL, antigen = NULL) {
  where <- "cl.project_id = $1 AND cl.study_accession = $2
            AND cl.experiment_accession = $3"
  params <- list(project, study, experiment)
  if (!is.null(feature)) { params <- c(params, list(feature))
    where <- paste0(where, sprintf(" AND cl.feature = $%d", length(params))) }
  if (!is.null(antigen)) { params <- c(params, list(antigen))
    where <- paste0(where, sprintf(" AND cl.antigen = $%d", length(params))) }
  out <- .calib_q(pool, sprintf(
    "SELECT cw.method, cw.timeperiod, cw.agroup, count(*) AS n,
            avg(cw.w_norm) AS mean_w_norm, stddev(cw.w_norm) AS sd_w_norm
       FROM %s cw
       JOIN %s cl ON cl.curve_id = cw.curve_id
      WHERE %s
      GROUP BY cw.method, cw.timeperiod, cw.agroup
      ORDER BY cw.method, cw.timeperiod, cw.agroup",
    .tbl("calib_weights"), .tbl("curve_lookup"), where), params = params)
  # Same bigint -> integer64 trap as fetch_design_readiness() -- fix at the
  # source rather than relying on every caller to remember.
  if (nrow(out)) out$n <- as.integer(out$n)
  out
}

# Per-sample weights + predicted_concentration/pcov_pass for the
# precision_weight_panel() figure (ported from std-curver's
# precision_weight_panel_m16.R) -- calib_weights carries neither, so this
# joins calib_samples (same 6-column identity key as everywhere else) and
# curve_lookup (antigen/feature/source/plate). Unlike every other *_scoped
# fetcher, `antigens` is a VECTOR (the Summary tab's multi-select), so the
# filter is an IN-list built the same $n-placeholder-append way the single-
# value filters above do, just looped over antigens instead of one value.
# `source` (singular, e.g. "NIBSC06_140") is an optional single-value filter --
# the Summary tab plots one calibration source at a time.
fetch_weights_panel_data <- function(pool, project, study, experiment, method,
                                     antigens = NULL, source = NULL) {
  where <- "cl.project_id = $1 AND cl.study_accession = $2
            AND cl.experiment_accession = $3 AND cw.method = $4"
  params <- list(project, study, experiment, method)
  if (!is.null(antigens) && length(antigens)) {
    ph <- sprintf("$%d", length(params) + seq_along(antigens))
    where <- paste0(where, sprintf(" AND cl.antigen IN (%s)", paste(ph, collapse = ",")))
    params <- c(params, as.list(antigens))
  }
  if (!is.null(source) && nzchar(source)) { params <- c(params, list(source))
    where <- paste0(where, sprintf(" AND cl.source = $%d", length(params))) }
  .calib_q(pool, sprintf(
    "SELECT cl.antigen, cl.feature, cl.source, cl.plate,
            cw.curve_id, cw.sampleid, cw.w_norm,
            cs.predicted_concentration, cs.pcov_pass
       FROM %s cw
       JOIN %s cs ON cs.curve_id = cw.curve_id AND cs.method = cw.method
                  AND cs.sampleid = cw.sampleid AND cs.patientid = cw.patientid
                  AND cs.timeperiod = cw.timeperiod AND cs.dilution = cw.dilution
       JOIN %s cl ON cl.curve_id = cw.curve_id
      WHERE %s",
    .tbl("calib_weights"), .tbl("calib_samples"), .tbl("curve_lookup"), where),
    params = params)
}

# One row per (multiplate_group_id, method) fit, with `source` appended from
# curve_lookup (calib_weights_fit itself has antigen/feature but not source --
# an antigen can span multiple sources/multiplate-groups, each with its own
# phi/beta1). Same antigen-vector IN-list filter as fetch_weights_panel_data()
# above, plus the same optional single-value `source` filter -- the Summary
# tab plots one method and one source at a time, so in practice this returns
# at most one row per antigen once `source` is given.
fetch_weights_panel_fit <- function(pool, project, study, experiment, method,
                                    antigens = NULL, source = NULL) {
  where <- "cl.project_id = $1 AND cl.study_accession = $2
            AND cl.experiment_accession = $3 AND cwf.method = $4"
  params <- list(project, study, experiment, method)
  if (!is.null(antigens) && length(antigens)) {
    ph <- sprintf("$%d", length(params) + seq_along(antigens))
    where <- paste0(where, sprintf(" AND cl.antigen IN (%s)", paste(ph, collapse = ",")))
    params <- c(params, as.list(antigens))
  }
  if (!is.null(source) && nzchar(source)) { params <- c(params, list(source))
    where <- paste0(where, sprintf(" AND cl.source = $%d", length(params))) }
  .calib_q(pool, sprintf(
    "SELECT DISTINCT cwf.*, cl.source
       FROM %s cwf
       JOIN %s cl ON cl.multiplate_group_id = cwf.multiplate_group_id
      WHERE %s",
    .tbl("calib_weights_fit"), .tbl("curve_lookup"), where),
    params = params)
}

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
# needs_recalc / recalc_reason (method-agnostic, see calib_recalc_flag) flag
# curves whose displayed fit is stale because a batched mask/unmask edit was
# deferred rather than deleted -- recompute clears it (see poll_once()).
fetch_calc_status_scoped <- function(pool, project, study, experiment) {
  tryCatch(.ensure_recalc_flag_table(pool), error = function(e) NULL)
  .calib_q(pool, sprintf(
    "SELECT cl.curve_id, cl.antigen, cl.plateid, cl.plate, cl.feature,
            cl.source, cl.wavelength,
            f.method, f.model_name AS best_model, f.converged, f.eligible,
            f.score_type, f.selection_score, f.job_id,
            r.status AS job_status, r.finished_at,
            (rc.curve_id IS NOT NULL) AS needs_recalc, rc.reason AS recalc_reason
       FROM %s cl
       LEFT JOIN %s f  ON f.curve_id  = cl.curve_id AND f.is_best
       LEFT JOIN %s r  ON r.job_id    = f.job_id
       LEFT JOIN %s rc ON rc.curve_id = cl.curve_id
      WHERE cl.project_id = $1 AND cl.study_accession = $2 AND cl.experiment_accession = $3
      ORDER BY cl.antigen, cl.plateid, cl.feature, f.method",
    .tbl(TBL_CURVE_LOOKUP), .tbl("calib_fit"), .tbl("calib_run"), .tbl(CALIB_RECALC_TABLE)),
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

# Observed BLANK points for ONE curve, LIVE mask status straight from
# xmap_buffer (mirrors fetch_standard_points above). Blanks join the curve's
# NK MINUS source (blank source != curve source), same as the mask resolvers
# (BLK_NK_JOIN_COLS) -- so this returns exactly the blanks that feed this
# curve's fit, with their CURRENT masked flag/reason.
fetch_blank_points <- function(pool, curve_id) {
  .calib_q(pool, sprintf(
    "SELECT b.well, b.antibody_mfi AS assay_response, b.masked, b.mask_reason
       FROM %s b JOIN %s cl ON %s
      WHERE cl.curve_id = $1",
    .tbl("xmap_buffer"), .tbl("curve_lookup"),
    .nk_join_on(BLK_NK_JOIN_COLS, cl = "cl", s = "b")),
    params = list(curve_id))
}

# THE live source of truth for "is this point masked right now". Reads
# directly from xmap_standard / xmap_buffer via fetch_standard_points /
# fetch_blank_points -- NOT from calib_standards.included / calib_blanks.
# included, which are SNAPSHOTS the worker bakes in at fit time and go stale
# the moment a mask/unmask save defers its recompute (apply_mask/apply_unmask
# delete_fits = FALSE, see calib_recalc_flag above). Callers that render or
# stage masking (the Explore-fits plot, masked_keys()) should always use this,
# whether or not the curve happens to be flagged stale -- it costs one extra
# small query per render and is correct in every case, deferred or not.
# Matches on WELL only (same identity the mask resolvers use -- a standard's
# `dilution` can be represented differently between xmap_standard and
# calib_standards, see resolve_std_mask_ids's comment), so results are keyed
# by well, not well+dilution.
# Returns list(std_masked_wells, blk_masked_wells, std_reason, blk_reason);
# the *_reason elements are named character vectors (name = well) for the
# masked rows only. Well-shaped (all-empty) on any error.
fetch_live_mask_state <- function(pool, curve_id) {
  empty <- list(std_masked_wells = character(0), blk_masked_wells = character(0),
                std_reason = character(0), blk_reason = character(0))
  tryCatch({
    std <- fetch_standard_points(pool, curve_id)
    blk <- fetch_blank_points(pool, curve_id)
    std_m <- if (!is.null(std) && nrow(std)) as.logical(std$masked) %in% TRUE else logical(0)
    blk_m <- if (!is.null(blk) && nrow(blk)) as.logical(blk$masked) %in% TRUE else logical(0)
    list(
      std_masked_wells = if (any(std_m)) unique(as.character(std$well[std_m])) else character(0),
      blk_masked_wells = if (any(blk_m)) unique(as.character(blk$well[blk_m])) else character(0),
      std_reason = if (any(std_m)) stats::setNames(as.character(std$mask_reason[std_m]),
                                                    as.character(std$well[std_m])) else character(0),
      blk_reason = if (any(blk_m)) stats::setNames(as.character(blk$mask_reason[blk_m]),
                                                    as.character(blk$well[blk_m])) else character(0))
  }, error = function(e) empty)
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

# Plate-scope standard fan-out: given masked xmap_standard ids, return every
# curve_id in every multiplate group any of those standard rows feeds. Mirrors
# curve_ids_for_blanks but joins on the FULL standard NK (a standard row maps to
# a specific analyte's curve via antigen/feature). Used when a well is masked
# across all analytes ("plate" scope): each analyte's standard row invalidates
# its own group's joint fit, so the delete blast radius is the union of them all.
curve_ids_for_standards <- function(pool, standard_ids) {
  ids <- as.integer(standard_ids[!is.na(standard_ids)])
  if (!length(ids)) return(integer(0))
  idlist <- paste(ids, collapse = ",")
  df <- .calib_q(pool, sprintf(
    "WITH fed AS (
       SELECT DISTINCT cl.multiplate_group_id
         FROM %s s
         JOIN %s cl ON %s
        WHERE s.xmap_standard_id IN (%s))
     SELECT c.curve_id
       FROM %s c JOIN fed USING (multiplate_group_id)",
    .tbl("xmap_standard"), .tbl("curve_lookup"),
    .nk_join_on(STD_NK_JOIN_COLS, cl = "cl", s = "s"),
    idlist, .tbl("curve_lookup")))
  if (!nrow(df)) integer(0) else as.integer(df$curve_id)
}

# The calib_* tables keyed on curve_id (deleted as a set on mask). calib_run is
# job-keyed (may span groups) and is intentionally NOT included.
CALIB_CURVE_TABLES <- c("calib_fit", "calib_param", "calib_gate", "calib_grid",
                        "calib_samples", "calib_diagnostics", "calib_standards",
                        "calib_blanks", "calib_loo")

# ---------------------------------------------------------------------------
# RECALC FLAG -- deferred-recalculation tracking (batched masking).
#
# Historically every mask/unmask save immediately DELETEd the affected group's
# calib_* rows, forcing a recompute before the curve could be viewed again.
# Users masking across several plates in one sitting wanted to stage all of
# that masking first and defer the (expensive) recompute to ONE later "Submit
# fit job" -- without losing the ability to see the still-valid-until-proven-
# otherwise existing fit in the meantime. calib_recalc_flag is the small,
# curve-keyed table that makes that possible: a row means "this curve_id's
# calib_* fits are STALE (masking changed the input set since they were
# computed) but have deliberately NOT been deleted yet". It is consulted by
# the Explore-fits viewer (red border + label, both methods) and by the
# Compute-fits status table, and is cleared once that curve_id is included in
# a job that reaches 'completed'.
#
# GRAIN: one row per curve_id (method-agnostic -- a mask invalidates every
# method's fit for the curve, exactly like the immediate-delete path already
# did). Requires the one-time DDL below; .ensure_recalc_flag_table() creates it
# lazily (CREATE TABLE IF NOT EXISTS) so no separate migration step is needed,
# provided the app's DB role has CREATE privilege on CALIB_SCHEMA:
#
#   CREATE TABLE IF NOT EXISTS <schema>.calib_recalc_flag (
#     curve_id   BIGINT PRIMARY KEY,
#     reason     TEXT NOT NULL,
#     flagged_at TIMESTAMPTZ NOT NULL DEFAULT now()
#   )
CALIB_RECALC_TABLE <- "calib_recalc_flag"

.ensure_recalc_flag_table <- function(pool) {
  DBI::dbExecute(pool, sprintf(
    "CREATE TABLE IF NOT EXISTS %s (
       curve_id   BIGINT PRIMARY KEY,
       reason     TEXT NOT NULL,
       flagged_at TIMESTAMPTZ NOT NULL DEFAULT now()
     )", .tbl(CALIB_RECALC_TABLE)))
  invisible(NULL)
}

# Flag curve_ids as stale (masking change pending recalculation) WITHOUT
# touching calib_*. Upsert: re-flagging an already-stale curve just refreshes
# the reason/timestamp. Safe to call with an existing DBI connection `co`
# (inside a transaction) or with the pool directly.
mark_curves_stale <- function(pool, curve_ids, reason = "Masking changes pending recalculation") {
  ids <- unique(as.integer(curve_ids[!is.na(curve_ids)]))
  if (!length(ids)) return(invisible(0L))
  .ensure_recalc_flag_table(pool)
  idlist <- paste(ids, collapse = ",")
  DBI::dbExecute(pool, sprintf(
    "INSERT INTO %s (curve_id, reason, flagged_at)
       SELECT unnest(ARRAY[%s]::bigint[]), $1, now()
     ON CONFLICT (curve_id) DO UPDATE
       SET reason = EXCLUDED.reason, flagged_at = EXCLUDED.flagged_at",
    .tbl(CALIB_RECALC_TABLE), idlist), params = list(reason))
}

# Clear the stale flag for a set of curve_ids (a fresh fit has landed, or an
# immediate mask/unmask delete has just invalidated + is about to be recomputed
# right away). No-op, not an error, if none of them were flagged.
clear_recalc_flags <- function(pool, curve_ids) {
  ids <- unique(as.integer(curve_ids[!is.na(curve_ids)]))
  if (!length(ids)) return(invisible(0L))
  .ensure_recalc_flag_table(pool)
  idlist <- paste(ids, collapse = ",")
  DBI::dbExecute(pool, sprintf(
    "DELETE FROM %s WHERE curve_id IN (%s)", .tbl(CALIB_RECALC_TABLE), idlist))
}

# Which of the given curve_ids are currently flagged stale. NULL/empty curve_ids
# -> integer(0) (no DB call). Used by the calc-status join and one-off checks.
fetch_stale_curve_ids <- function(pool, curve_ids = NULL) {
  tryCatch({
    .ensure_recalc_flag_table(pool)
    if (is.null(curve_ids) || !length(curve_ids)) {
      df <- .calib_q(pool, sprintf("SELECT curve_id FROM %s", .tbl(CALIB_RECALC_TABLE)))
    } else {
      ids <- unique(as.integer(curve_ids[!is.na(curve_ids)]))
      if (!length(ids)) return(integer(0))
      df <- .calib_q(pool, sprintf(
        "SELECT curve_id FROM %s WHERE curve_id IN (%s)",
        .tbl(CALIB_RECALC_TABLE), paste(ids, collapse = ",")))
    }
    if (!nrow(df)) integer(0) else as.integer(df$curve_id)
  }, error = function(e) integer(0))
}

# Single-curve detail for the Explore-fits banner: is it stale, and why/when.
# Always returns a well-shaped list (stale = FALSE on any error / not flagged)
# so callers never need their own tryCatch.
curve_recalc_flag <- function(pool, curve_id) {
  out <- list(stale = FALSE, reason = NA_character_, flagged_at = NA)
  if (is.null(curve_id) || !length(curve_id) || is.na(curve_id)) return(out)
  tryCatch({
    .ensure_recalc_flag_table(pool)
    df <- .calib_q(pool, sprintf(
      "SELECT reason, flagged_at FROM %s WHERE curve_id = $1",
      .tbl(CALIB_RECALC_TABLE)), params = list(as.integer(curve_id)))
    if (nrow(df)) list(stale = TRUE, reason = df$reason[1], flagged_at = df$flagged_at[1])
    else out
  }, error = function(e) out)
}

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

# Resolve staged STANDARD points (keys "std|well|dilution") to xmap_standard_id.
# scope = "antigen" (default): the curve's FULL NK (source-in), i.e. this one
# analyte -- current behavior, preserved for every existing caller. scope =
# "plate": the reduced NK (antigen/feature dropped), i.e. the same well across
# ALL analytes on the plate. In BOTH cases the query pins the current curve's
# scope values by joining cl.curve_id = $1 on the chosen column set, so no new
# parameters are needed -- only the join key set differs. Match is on well.
resolve_std_mask_ids <- function(pool, curve_id, wells, dilutions = NULL,
                                  scope = c("antigen", "plate")) {
  scope <- match.arg(scope)
  if (!length(wells)) return(integer(0))
  cols <- if (identical(scope, "plate")) STD_PLATE_JOIN_COLS else STD_NK_JOIN_COLS
  df <- .calib_q(pool, sprintf(
    "SELECT s.xmap_standard_id, s.well, s.dilution, s.masked
       FROM %s s JOIN %s cl ON %s
      WHERE cl.curve_id = $1",
    .tbl("xmap_standard"), .tbl("curve_lookup"),
    .nk_join_on(cols, cl = "cl", s = "s")),
    params = list(curve_id))
  if (!nrow(df)) return(integer(0))
  # Match on WELL alone. The join already pins the scope (one curve for
  # "antigen"; one plate/source/wavelength/nominal-dilution across analytes for
  # "plate"), within which `well` identifies the standard point(s) -- exactly as
  # resolve_blk_mask_ids() keys blanks on `well`.
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
  unique(as.integer(df$xmap_standard_id[keep]))
}

# Resolve staged BLANK points (keys "blk|well|") to xmap_buffer_id via the NK
# MINUS source (blank source != curve source), well only, NO dilution. scope =
# "plate" additionally drops antigen/feature so a contaminated buffer well is
# resolved across ALL analytes; "antigen" (default) keeps the current behavior.
resolve_blk_mask_ids <- function(pool, curve_id, wells,
                                  scope = c("antigen", "plate")) {
  scope <- match.arg(scope)
  if (!length(wells)) return(integer(0))
  cols <- if (identical(scope, "plate")) BLK_PLATE_JOIN_COLS else BLK_NK_JOIN_COLS
  df <- .calib_q(pool, sprintf(
    "SELECT b.xmap_buffer_id, b.well, b.masked
       FROM %s b JOIN %s cl ON %s
      WHERE cl.curve_id = $1",
    .tbl("xmap_buffer"), .tbl("curve_lookup"),
    .nk_join_on(cols, cl = "cl", s = "b")),
    params = list(curve_id))
  if (!nrow(df)) return(integer(0))
  unique(as.integer(df$xmap_buffer_id[df$well %in% wells]))
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
                                     blk_wells = character(0),
                                     scope = c("antigen", "plate")) {
  scope <- match.arg(scope)
  std_cols <- if (identical(scope, "plate")) STD_PLATE_JOIN_COLS else STD_NK_JOIN_COLS
  blk_cols <- if (identical(scope, "plate")) BLK_PLATE_JOIN_COLS else BLK_NK_JOIN_COLS
  chr <- function(x) if (length(x)) sort(unique(as.character(x))) else character(0)
  wells_of <- function(df) if (!is.null(df) && nrow(df) && "well" %in% names(df))
                             chr(df$well) else character(0)

  std_join <- .calib_q(pool, sprintf(
    "SELECT s.xmap_standard_id, s.well
       FROM %s s JOIN %s cl ON %s
      WHERE cl.curve_id = $1",
    .tbl("xmap_standard"), .tbl("curve_lookup"),
    .nk_join_on(std_cols, cl = "cl", s = "s")),
    params = list(curve_id))
  blk_join <- .calib_q(pool, sprintf(
    "SELECT b.xmap_buffer_id, b.well
       FROM %s b JOIN %s cl ON %s
      WHERE cl.curve_id = $1",
    .tbl("xmap_buffer"), .tbl("curve_lookup"),
    .nk_join_on(blk_cols, cl = "cl", s = "b")),
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


# MASKING write (TRANSACTIONAL). Sets masked/mask_reason on the resolved xmap
# rows, then EITHER deletes ALL calib_* fits for the affected multiplate group
# (a mask invalidates the joint fit; the historical, still-default behavior) OR,
# when the user has opted to batch several masking edits before recomputing,
# leaves the existing fits in place and marks the group STALE via
# calib_recalc_flag instead (see mark_curves_stale). All-or-nothing: any error
# rolls back so there is never a half-masked / half-deleted / half-flagged state.
#
# std_ids / blk_ids : integer xmap_standard_id / xmap_buffer_id (from the
#   resolvers). group_curve_ids : every curve_id in the group (from
#   curve_group_members). reason : required, written to every masked row.
# delete_fits : TRUE (default) = immediate delete, exactly the original
#   behavior. FALSE = defer: keep calib_* as-is and flag group_curve_ids stale
#   instead, using `reason` as the recalc-flag reason too (masking already
#   requires one, so it doubles as the "why is this stale" note).
# Returns list(ok, masked_std, masked_blk, deleted, marked_stale, group_n) or
# stops on error. `deleted` is all-zero when delete_fits = FALSE; `marked_stale`
# is empty when delete_fits = TRUE.
apply_mask <- function(pool, std_ids, blk_ids, group_curve_ids, reason,
                       set_masked = TRUE, delete_fits = TRUE) {
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
      if (isTRUE(delete_fits)) {
        for (tb in CALIB_CURVE_TABLES) {
          deleted[[tb]] <- DBI::dbExecute(co, sprintf(
            "DELETE FROM %s WHERE curve_id IN (%s)", .tbl(tb), grplist))
        }
        # An immediate delete needs no stale flag (there's nothing stale left to
        # mark -- the group is simply empty until recomputed); clear any leftover
        # flag from an earlier deferred edit on the same group.
        clear_recalc_flags(co, grp)
      } else {
        mark_curves_stale(co, grp, reason = reason)
      }
      DBI::dbCommit(co)
      list(ok = TRUE, masked_std = n_std, masked_blk = n_blk,
           deleted = deleted, marked_stale = if (isTRUE(delete_fits)) integer(0) else grp,
           group_n = length(grp))
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
# mask_reason on the resolved xmap rows, then EITHER delete ALL calib_* fits for
# the affected multiplate group (default, original behavior) OR defer that
# delete and flag the group STALE instead (see apply_mask's delete_fits doc --
# same mechanism, same reason this exists: batching several plates' worth of
# mask/unmask corrections before paying for one recompute). Differences vs
# apply_mask: no reason is required to unmask, and mask_reason is CLEARED rather
# than written; when deferred, the recalc-flag reason defaults to a fixed
# unmask-specific note (there is no user-supplied reason to reuse). All-or-
# nothing: any error rolls back.
#
# std_ids / blk_ids : integer xmap_standard_id / xmap_buffer_id (from the SAME
#   resolvers used for masking -- they match on well regardless of mask state).
#   group_curve_ids : every curve_id in every affected group (curve_group_members
#   plus, for blanks, curve_ids_for_blanks).
# delete_fits : TRUE (default) = immediate delete. FALSE = defer (mark stale).
# Returns list(ok, unmasked_std, unmasked_blk, deleted, marked_stale, group_n).
apply_unmask <- function(pool, std_ids, blk_ids, group_curve_ids, delete_fits = TRUE) {
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
      if (isTRUE(delete_fits)) {
        for (tb in CALIB_CURVE_TABLES) {
          deleted[[tb]] <- DBI::dbExecute(co, sprintf(
            "DELETE FROM %s WHERE curve_id IN (%s)", .tbl(tb), grplist))
        }
        clear_recalc_flags(co, grp)
      } else {
        mark_curves_stale(co, grp, reason = "Unmasking changes pending recalculation")
      }
      DBI::dbCommit(co)
      list(ok = TRUE, unmasked_std = n_std, unmasked_blk = n_blk,
           deleted = deleted, marked_stale = if (isTRUE(delete_fits)) integer(0) else grp,
           group_n = length(grp))
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


# AGROUP write (TRANSACTIONAL). Patches calib_samples.agroup from an analyst-
# filled Excel round-trip (see std_curve_weights_module.R's design-readiness
# panel) when a study's samples were calibrated without a cohort/treatment-arm
# column populated -- curveRweights::fit_precision_weights() needs at least
# one design column with real variation (timeperiod OR agroup) to build its
# saturated-cell location model.
#
# Deliberately agroup-ONLY, never timeperiod: agroup is a plain nullable
# column, so this is a safe, ordinary UPDATE matched on calib_samples' full
# EXISTING primary key (curve_id, method, sampleid, patientid, timeperiod,
# dilution) -- agroup plays no part in that key, so no row can ever collide.
# timeperiod, by contrast, IS part of the primary key; editing it would mean
# re-matching on the other key columns and checking for a resulting key
# collision before committing. No study has been found missing timeperiod
# (only agroup), so that harder case has nothing concrete to build against
# yet and is deliberately out of scope here.
#
# `updates` : data.frame(curve_id, method, sampleid, patientid, timeperiod,
#             dilution, agroup) -- the uploaded file's rows, already validated
#             by the caller (every row matches an existing calib_samples key;
#             see std_curve_weights_module.R's upload-validation step). All-or-
#             nothing: any error rolls back every row, not just the failing one.
update_calib_samples_agroup <- function(pool, updates) {
  req_cols <- c("curve_id", "method", "sampleid", "patientid", "timeperiod",
               "dilution", "agroup")
  missing <- setdiff(req_cols, names(updates))
  if (length(missing))
    stop("update_calib_samples_agroup: updates is missing column(s): ",
         paste(missing, collapse = ", "))
  if (!nrow(updates)) return(invisible(0L))

  do_txn <- function(co) {
    DBI::dbBegin(co)
    tryCatch({
      n <- 0L
      sql <- sprintf(
        "UPDATE %s SET agroup = $1
          WHERE curve_id = $2 AND method = $3 AND sampleid = $4
            AND patientid = $5 AND timeperiod = $6 AND dilution = $7",
        .tbl("calib_samples"))
      for (i in seq_len(nrow(updates))) {
        r <- updates[i, ]
        n <- n + DBI::dbExecute(co, sql, params = list(
          as.character(r$agroup), as.integer(r$curve_id), as.character(r$method),
          as.character(r$sampleid), as.character(r$patientid),
          as.character(r$timeperiod), as.character(r$dilution)))
      }
      DBI::dbCommit(co)
      n
    }, error = function(e) {
      DBI::dbRollback(co)
      stop(sprintf("update_calib_samples_agroup failed (rolled back): %s",
                   conditionMessage(e)), call. = FALSE)
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
# curve_lookup) and calib_weights_fit (keyed by multiplate_group_id, not
# curve_id -- same reason fetch_calib_weights_fit_scoped() needs its own join
# instead of .fetch_calib_scoped()'s factory). Positional params $1/$2/$3 are
# reused across the UNION, which Postgres allows. Returns a named integer
# vector (NA for a table that errored).
fetch_scoped_table_counts <- function(pool, project, study, experiment, tables) {
  tables <- tables[!is.na(tables) & nzchar(tables)]
  if (!length(tables)) return(integer(0))
  cl <- .tbl("curve_lookup")
  scope <- "c.project_id = $1 AND c.study_accession = $2 AND c.experiment_accession = $3"
  sub <- vapply(tables, function(tb) {
    if (identical(tb, "calib_run"))
      sprintf("SELECT '%s'::text AS tbl, count(*) AS n FROM %s r JOIN %s f ON f.job_id = r.job_id JOIN %s c ON c.curve_id = f.curve_id WHERE %s",
              tb, .tbl("calib_run"), .tbl("calib_fit"), cl, scope)
    else if (identical(tb, "calib_weights_fit"))
      sprintf("SELECT '%s'::text AS tbl, count(*) AS n FROM %s t JOIN %s c ON c.multiplate_group_id = t.multiplate_group_id WHERE %s",
              tb, .tbl(tb), cl, scope)
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
