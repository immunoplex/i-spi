# =============================================================================
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
#   * `conn` is a DBI connection or a pool::Pool -- both work with dbGetQuery.
#   * `method` is 'bayesian' | 'frequentist'.
#   * All values are bound as query parameters ($1, $2, ...); only fixed
#     identifiers appear in the SQL text. (Scalar binds work in RPostgres; it is
#     only array binds via = ANY($1) that do not -- hence explicit IN lists.)
#   * curve_id is bigint -> comes back as integer64; pass it straight back in.
# =============================================================================

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
.calib_q <- function(conn, sql, params = list()) {
  out <- tryCatch(
    if (length(params)) DBI::dbGetQuery(conn, sql, params = params)
    else                DBI::dbGetQuery(conn, sql),
    error = function(e) {
      warning("calib_data_access query failed: ", conditionMessage(e), call. = FALSE)
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

# =============================================================================
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

#' Resolve the per-antigen/feature analysis settings for a curve. Rows range
#' from broad (study + antigen; experiment/feature NULL) to specific (study +
#' experiment + antigen + feature); this returns the MOST specific matching row.
#' Besides model_form_list it carries standard_curve_concentration,
#' pcov_threshold, l_asy_* constraints, and concentration_unit_reported -- all
#' inputs to a fit job.
#' NOTE: the specificity/precedence logic is an interpretation of how the table
#' layers defaults vs overrides; confirm it matches your data conventions.
fetch_antigen_feature_settings <- function(conn, project_id, study, antigen,
                                           experiment = NULL, feature = NULL) {
  .calib_q(conn, sprintf(
    "SELECT *,
            ( (experiment_accession IS NOT DISTINCT FROM $4)::int * 2
            + (feature             IS NOT DISTINCT FROM $5)::int ) AS specificity
       FROM %s
      WHERE project_id IS NOT DISTINCT FROM $1
        AND study_accession = $2
        AND antigen = $3
        AND (experiment_accession IS NOT DISTINCT FROM $4 OR experiment_accession IS NULL)
        AND (feature             IS NOT DISTINCT FROM $5 OR feature             IS NULL)
      ORDER BY specificity DESC
      LIMIT 1", .tbl(TBL_ANTIGEN_SETTINGS)),
    params = list(project_id, study, antigen, experiment, feature))
}

# =============================================================================
# 1. Identity: natural key  <->  curve_id
# =============================================================================

# The app resolves and lists curves through the unmasked view, so masked curves
# (rare, and masked as a whole -- curve + all its rows together) never appear in
# the selector and are never fit, matching the worker. Overridable for tests /
# admin views that need to see masked curves too.
TBL_CURVE_LOOKUP <- getOption("ispi.curve_lookup_table", "curve_lookup_unmasked")

#' Resolve one natural key to its curve_id.
#' @param nk named list/vector with the CALIB_NK_COLS elements.
#' @return single curve_id (integer64) or NA if the curve is unknown.
resolve_curve_id <- function(conn, nk) {
  missing <- setdiff(CALIB_NK_COLS, names(nk))
  if (length(missing))
    stop("resolve_curve_id: missing NK fields: ", paste(missing, collapse = ", "))
  where <- paste(sprintf("%s IS NOT DISTINCT FROM $%d", CALIB_NK_COLS,
                         seq_along(CALIB_NK_COLS)), collapse = " AND ")
  sql <- sprintf("SELECT curve_id FROM %s WHERE %s", .tbl(TBL_CURVE_LOOKUP), where)
  res <- .calib_q(conn, sql, params = as.list(unname(nk[CALIB_NK_COLS])))
  if (nrow(res) == 0) NA else res$curve_id[1]
}

#' Batch NK -> curve_id join. Give it a data.frame with the CALIB_NK_COLS and
#' get the same rows back with a curve_id column appended (NA where unmatched).
#' Prefer this over row-by-row resolve_curve_id() for tables of samples/plates.
resolve_curve_ids <- function(conn, nk_df) {
  lk <- fetch_curve_lookup(conn)
  merge(nk_df, lk[, c(CALIB_NK_COLS, "curve_id")],
        by = CALIB_NK_COLS, all.x = TRUE, sort = FALSE)
}

#' The curve registry (NK + curve_id) as the app sees it: unmasked curves only
#' (see TBL_CURVE_LOOKUP). Small enough (~27k rows) to pull once and join in R.
#' The unmasked view omits masked/mask_reason, so those are not returned here;
#' point-level masking counts for a "k of M masked" display come from elsewhere.
fetch_curve_lookup <- function(conn) {
  .calib_q(conn, sprintf(
    "SELECT curve_id, %s FROM %s",
    paste(CALIB_NK_COLS, collapse = ", "), .tbl(TBL_CURVE_LOOKUP)))
}

# =============================================================================
# 2. Fits (candidate models + best selection)
# =============================================================================

#' All candidate model fits for a curve+method, with selection metadata.
#' Columns include is_best, is_fallback, converged, eligible, criterion,
#' score_type ('loo_elpd' bayes / 'aic' freq), selection_score, selection_weight.
fetch_calib_fit <- function(conn, curve_id, method) {
  df <- .calib_q(conn, sprintf(
    "SELECT * FROM %s WHERE curve_id = $1 AND method = $2
      ORDER BY is_best DESC, selection_score DESC", .tbl("calib_fit")),
    params = list(curve_id, method))
  if (nrow(df)) df$family_label <- family_label(df$model_name)
  df
}

#' The single winning model row for a curve+method (uses the is_best index).
#' Returns a 1-row frame, or a 0-row frame if the curve/method is absent.
fetch_calib_best_model <- function(conn, curve_id, method) {
  df <- .calib_q(conn, sprintf(
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
calib_available_models <- function(conn, curve_id = NULL, method = NULL) {
  where <- character(0); params <- list(); i <- 0L
  if (!is.null(curve_id)) { i <- i + 1L; where <- c(where, sprintf("curve_id = $%d", i)); params <- c(params, list(curve_id)) }
  if (!is.null(method))   { i <- i + 1L; where <- c(where, sprintf("method = $%d",   i)); params <- c(params, list(method)) }
  wc  <- if (length(where)) paste("WHERE", paste(where, collapse = " AND ")) else ""
  ord <- if (!is.null(curve_id)) "ORDER BY bool_or(is_best) DESC, model_name" else "ORDER BY model_name"
  df  <- .calib_q(conn, sprintf(
    "SELECT model_name FROM %s %s GROUP BY model_name %s",
    .tbl("calib_fit"), wc, ord), params = params)
  df$model_name
}

# =============================================================================
# 3. Parameters, eligibility gates, LOO
# =============================================================================

#' Per-term parameters (estimate, std_error, q_lo/q_med/q_hi). Defaults to the
#' best model when model_name is NULL, so callers usually don't specify it.
fetch_calib_params <- function(conn, curve_id, method, model_name = NULL) {
  if (is.null(model_name)) {
    best <- fetch_calib_best_model(conn, curve_id, method)
    if (!nrow(best)) return(data.frame())
    model_name <- best$model_name[1]
  }
  .calib_q(conn, sprintf(
    "SELECT term, estimate, std_error, q_lo, q_med, q_hi, model_name
       FROM %s WHERE curve_id = $1 AND method = $2 AND model_name = $3
      ORDER BY term", .tbl("calib_param")),
    params = list(curve_id, method, model_name))
}

#' Eligibility gates (gate, passed, detail). NULL model_name -> best model.
fetch_calib_gates <- function(conn, curve_id, method, model_name = NULL) {
  if (is.null(model_name)) {
    best <- fetch_calib_best_model(conn, curve_id, method)
    if (!nrow(best)) return(data.frame())
    model_name <- best$model_name[1]
  }
  .calib_q(conn, sprintf(
    "SELECT gate, passed, detail, model_name
       FROM %s WHERE curve_id = $1 AND method = $2 AND model_name = $3
      ORDER BY gate", .tbl("calib_gate")),
    params = list(curve_id, method, model_name))
}

#' LOO comparison table. Bayesian only by design; returns a 0-row frame for
#' frequentist curves (calib_loo has no frequentist rows), which callers should
#' treat as "no LOO available", NOT as an error.
fetch_calib_loo <- function(conn, curve_id, method = "bayesian") {
  if (!identical(method, "bayesian")) return(data.frame())
  .calib_q(conn, sprintf(
    "SELECT * FROM %s WHERE curve_id = $1 AND method = $2
      ORDER BY elpd_loo DESC", .tbl("calib_loo")),
    params = list(curve_id, method))
}

# =============================================================================
# 4. Plotting grid  (the ONE curve visualization source)
# =============================================================================

#' The ~200-point fitted grid for a curve+method, ordered for plotting.
#' Carries both scales (log10_concentration + concentration), the response with
#' CI band (predicted_response, ci_lower, ci_upper), the inverse prediction
#' (predicted_concentration + se_concentration), and the pcov QC series.
fetch_calib_grid <- function(conn, curve_id, method) {
  .calib_q(conn, sprintf(
    "SELECT point_index, model_name, log10_concentration, concentration,
            predicted_response, ci_lower, ci_upper,
            predicted_concentration, se_concentration,
            pcov, pcov_rmse, pcov_pass, d2y_dx2, noise_mode
       FROM %s WHERE curve_id = $1 AND method = $2
      ORDER BY point_index", .tbl("calib_grid")),
    params = list(curve_id, method))
}

# =============================================================================
# 5. Back-calculated samples
# =============================================================================

#' Per-sample back-calculated concentrations for a curve+method.
#' predicted_concentration is on the curve; final_concentration is x dilution.
#' The '__none__' identity sentinels are decoded back to NA on the way out.
fetch_calib_samples <- function(conn, curve_id, method) {
  df <- .calib_q(conn, sprintf(
    "SELECT sampleid, patientid, timeperiod, dilution,
            predicted_concentration, final_concentration, se_concentration,
            pcov, pcov_rmse, pcov_pass
       FROM %s WHERE curve_id = $1 AND method = $2", .tbl("calib_samples")),
    params = list(curve_id, method))
  .decode_none(df, c("sampleid", "patientid", "timeperiod", "dilution"))
}

# =============================================================================
# 6. Diagnostics + LOQ/LOD/RDL bounds
# =============================================================================

#' The single diagnostics row for a curve+method (34 cols: LLOQ/ULOQ on both
#' scales, shape-based LOQ, inflection +/- CI, LOD, MDC, RDL, pcov threshold).
fetch_calib_diagnostics <- function(conn, curve_id, method) {
  .calib_q(conn, sprintf(
    "SELECT * FROM %s WHERE curve_id = $1 AND method = $2 LIMIT 1",
    .tbl("calib_diagnostics")),
    params = list(curve_id, method))
}

#' Pull LLOQ/ULOQ on the scale the caller needs. `scale = "conc"` gives a value
#' comparable to the old bayes_curves.lloq/uloq (raw concentration); "log10"
#' gives the values to place on a log10 plot axis. Returns list(lloq, uloq).
#' `diag` is a row from fetch_calib_diagnostics().
calib_loq <- function(diag, scale = c("conc", "log10")) {
  scale <- match.arg(scale)
  if (is.null(diag) || !nrow(diag)) return(list(lloq = NA, uloq = NA))
  suffix <- if (scale == "conc") "_conc" else "_log10"
  list(lloq = diag[[paste0("lloq", suffix)]][1],
       uloq = diag[[paste0("uloq", suffix)]][1])
}

# =============================================================================
# 7. Run / job metadata
# =============================================================================

#' Run-level metadata for a job_id (method, package, version, best_model,
#' params jsonb, status, started_at/finished_at).
fetch_calib_run <- function(conn, job_id) {
  .calib_q(conn, sprintf(
    "SELECT * FROM %s WHERE job_id = $1", .tbl("calib_run")),
    params = list(job_id))
}

# =============================================================================
# 8. One-call bundle for the module
# =============================================================================

#' Everything the standard-curve view needs for one curve+method, in a single
#' list. This is the function the module server should call; it keeps the read
#' pattern in one place and one round of queries.
#' @return list(fit_best, fits, params, gates, grid, samples, diagnostics, loo)
fetch_calib_bundle <- function(conn, curve_id, method) {
  best <- fetch_calib_best_model(conn, curve_id, method)
  mdl  <- if (nrow(best)) best$model_name[1] else NULL
  list(
    curve_id    = curve_id,
    method      = method,
    fit_best    = best,
    fits        = fetch_calib_fit(conn, curve_id, method),
    params      = fetch_calib_params(conn, curve_id, method, mdl),
    gates       = fetch_calib_gates(conn, curve_id, method, mdl),
    grid        = fetch_calib_grid(conn, curve_id, method),
    samples     = fetch_calib_samples(conn, curve_id, method),
    diagnostics = fetch_calib_diagnostics(conn, curve_id, method),
    loo         = fetch_calib_loo(conn, curve_id, method)
  )
}

# =============================================================================
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

.fetch_raw_scoped <- function(conn, tbl, study, experiment, project_id) {
  .calib_q(conn, sprintf(
    "SELECT * FROM %s
      WHERE study_accession = $1 AND experiment_accession = $2
        AND project_id IS NOT DISTINCT FROM $3", .tbl(tbl)),
    params = list(study, experiment, project_id))
}
fetch_raw_header   <- function(conn, s, e, p) .fetch_raw_scoped(conn, "xmap_header",   s, e, p)
fetch_raw_standard <- function(conn, s, e, p) .fetch_raw_scoped(conn, "xmap_standard", s, e, p)
fetch_raw_control  <- function(conn, s, e, p) .fetch_raw_scoped(conn, "xmap_control",  s, e, p)
fetch_raw_blank    <- function(conn, s, e, p) .fetch_raw_scoped(conn, "xmap_buffer",   s, e, p)  # buffer == blank
fetch_raw_sample   <- function(conn, s, e, p) .fetch_raw_scoped(conn, "xmap_sample",   s, e, p)

# calib_* rows for a whole study/experiment, NK-denormalized (curve_lookup
# columns prepended so every row is self-describing). Joins BASE curve_lookup so
# nothing is hidden on the audit surface. calib_run is keyed on job_id, not
# curve_id -- fetch it separately with fetch_calib_run().
.fetch_calib_scoped <- function(conn, tbl, study, experiment, project_id) {
  .calib_q(conn, sprintf(
    "SELECT cl.project_id, cl.study_accession, cl.experiment_accession,
            cl.plateid, cl.plate, cl.nominal_sample_dilution, cl.feature,
            cl.antigen, cl.source, cl.wavelength, t.*
       FROM %s t
       JOIN %s cl ON cl.curve_id = t.curve_id
      WHERE cl.study_accession = $1 AND cl.experiment_accession = $2
        AND cl.project_id IS NOT DISTINCT FROM $3
      ORDER BY cl.antigen, cl.plateid, t.curve_id",
    .tbl(tbl), .tbl("curve_lookup")),
    params = list(study, experiment, project_id))
}
fetch_calib_fit_scoped         <- function(conn, s, e, p) .fetch_calib_scoped(conn, "calib_fit", s, e, p)
fetch_calib_param_scoped       <- function(conn, s, e, p) .fetch_calib_scoped(conn, "calib_param", s, e, p)
fetch_calib_gate_scoped        <- function(conn, s, e, p) .fetch_calib_scoped(conn, "calib_gate", s, e, p)
fetch_calib_grid_scoped        <- function(conn, s, e, p) .fetch_calib_scoped(conn, "calib_grid", s, e, p)
fetch_calib_samples_scoped     <- function(conn, s, e, p) .fetch_calib_scoped(conn, "calib_samples", s, e, p)
fetch_calib_diagnostics_scoped <- function(conn, s, e, p) .fetch_calib_scoped(conn, "calib_diagnostics", s, e, p)
fetch_calib_loo_scoped         <- function(conn, s, e, p) .fetch_calib_scoped(conn, "calib_loo", s, e, p)

# curve_lookup registry rows for a study/experiment (BASE table, masks visible).
fetch_curve_lookup_scoped <- function(conn, study, experiment, project_id) {
  .calib_q(conn, sprintf(
    "SELECT * FROM %s
      WHERE study_accession = $1 AND experiment_accession = $2
        AND project_id IS NOT DISTINCT FROM $3
      ORDER BY antigen, plateid", .tbl("curve_lookup")),
    params = list(study, experiment, project_id))
}

# calib_run rows for a study/experiment. calib_run has no study/experiment/
# curve_id columns (it's per job_id), so reach it through calib_fit -> curve_lookup.
fetch_calib_run_scoped <- function(conn, study, experiment, project_id) {
  .calib_q(conn, sprintf(
    "SELECT DISTINCT r.*
       FROM %s r
       JOIN %s f  ON f.job_id  = r.job_id
       JOIN %s cl ON cl.curve_id = f.curve_id
      WHERE cl.study_accession = $1 AND cl.experiment_accession = $2
        AND cl.project_id IS NOT DISTINCT FROM $3
      ORDER BY r.started_at DESC NULLS LAST",
    .tbl("calib_run"), .tbl("calib_fit"), .tbl("curve_lookup")),
    params = list(study, experiment, project_id))
}
