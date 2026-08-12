# =============================================================================
# study_overview_calib_access.R  --  live fit reads for Study Overview [Phase 3]
# -----------------------------------------------------------------------------
# Replaces the stale best_* reads (pull_fits -> best_glance_all,
# pull_samples -> best_sample_se_all, check_plate -> best_plate_all). Those views
# are decoupled from how curves are fit now and MUST NOT be used. Everything here
# reads the live calib_* tables scoped through curve_lookup, and carries `method`
# (bayesian / frequentist) as a first-class dimension.
#
# best_* -> calib_* mapping
#   best_glance_all (one chosen fit per source)
#       -> curve_lookup  (scope + antigen/plate/source/analyte dimensions)
#          JOIN calib_fit ON is_best            (per-method chosen model)
#          LEFT JOIN calib_diagnostics          (lloq/uloq/lod/pcov limits)
#   best_sample_se_all (per-sample conc + se)
#       -> calib_samples (predicted/final/se_concentration, pcov, pcov_pass)
#   standard points        -> calib_standards
#   model coefficients     -> calib_param (a/b/c/d/g as `term` rows; long)
#
# Scope: STUDY-LEVEL (project_id + study_accession), all experiments -- unchanged
# from the original overview. Masked curves are excluded by default (they are not
# live fits); pass include_masked = TRUE to see them.
#
# Contract note for the fit-summary view: prep_analyte_fit_summary() needs only
# (plateid, antigen, analyte, crit, source); so_fit_glance() returns those plus
# `method`, so the existing prep/plot logic works by adding `method` to the merge
# keys (or filtering to one method). `crit` = calib_fit.model_name for the best
# model; the view maps c("Y5","Yd5","Y4","Yd4","Ygomp4") -> "Model" and treats a
# curve with no is_best row as "No Model" (LEFT JOIN yields NA -> "No Model").
#
# Conventions mirror annotation_access.R / settings_cascade_access.R:
#   * pool first (pool::Pool or bare DBI connection)
#   * schema from getOption("ispi.calib_schema", "madi_results")
#   * params bound as $1,$2 ; project_id integer, study_accession text
# =============================================================================

.so_schema <- function() getOption("ispi.calib_schema", "madi_results")
.so_tbl    <- function(name) sprintf("%s.%s", .so_schema(), name)

# integer project id or NA (never inject unchecked)
.so_proj <- function(project) {
  p <- suppressWarnings(as.integer(project))
  if (length(p) != 1 || is.na(p)) stop("study overview: project_id must be an integer, got: ",
                                        paste(project, collapse = ","))
  p
}
.so_q <- function(pool, sql, params) {
  out <- tryCatch(DBI::dbGetQuery(pool, sql, params = params),
                  error = function(e) {
                    warning("study_overview_calib_access query failed: ",
                            conditionMessage(e), call. = FALSE); NULL })
  if (is.null(out)) data.frame() else out
}
# ---- scope predicate shared by every reader ---------------------------------
.so_scope_sql <- function(include_masked) {
  base <- "cl.project_id = $1 AND cl.study_accession = $2"
  if (isTRUE(include_masked)) base else paste0(base, " AND NOT cl.masked")
}

# =============================================================================
# GLANCE: one row per (curve_id, method) for the SELECTED model (is_best), with
# scope/dimensions and the diagnostic limits. Replaces best_glance_all.
# =============================================================================
so_fit_glance <- function(pool, project, study, methods = NULL, include_masked = FALSE) {
  proj <- .so_proj(project)
  params <- list(proj, as.character(study)[1])
  mth <- ""
  if (!is.null(methods) && length(methods)) { params <- c(params, list(paste0("{", paste(as.character(methods), collapse=","), "}")))
    mth <- sprintf(" AND f.method = ANY($%d)", length(params)) }
  sql <- sprintf(
    "SELECT cl.curve_id, f.method,
            cl.experiment_accession,
            (cl.experiment_accession || '_' || cl.nominal_sample_dilution) AS analyte,
            cl.antigen, cl.plateid, cl.plate, cl.source, cl.feature,
            cl.nominal_sample_dilution,
            f.model_name AS crit, f.converged, f.eligible, f.is_fallback,
            f.criterion, f.selection_score, f.selection_weight,
            f.dynamic_range_log10, f.n_params,
            d.lloq_conc, d.uloq_conc, d.lloq_log10, d.uloq_log10,
            d.lower_lod_conc, d.upper_lod_conc, d.pcov_threshold, d.cv_x_max
       FROM %s cl
       JOIN %s f  ON f.curve_id = cl.curve_id AND f.is_best %s
       LEFT JOIN %s d ON d.curve_id = f.curve_id AND d.method = f.method
      WHERE %s
      ORDER BY cl.experiment_accession, cl.antigen, cl.plateid, f.method",
    .so_tbl("curve_lookup"), .so_tbl("calib_fit"), mth, .so_tbl("calib_diagnostics"),
    .so_scope_sql(include_masked))
  .so_q(pool, sql, params)
}

# =============================================================================
# SAMPLES: per-sample predicted/final concentration + se + pcov gate, with
# scope/dimensions. Replaces best_sample_se_all.
# =============================================================================
so_fit_samples <- function(pool, project, study, methods = NULL, include_masked = FALSE) {
  proj <- .so_proj(project)
  params <- list(proj, as.character(study)[1])
  mth <- ""
  if (!is.null(methods) && length(methods)) { params <- c(params, list(paste0("{", paste(as.character(methods), collapse=","), "}")))
    mth <- sprintf(" AND s.method = ANY($%d)", length(params)) }
  sql <- sprintf(
    "SELECT cl.curve_id, s.method,
            cl.experiment_accession,
            (cl.experiment_accession || '_' || cl.nominal_sample_dilution) AS analyte,
            cl.antigen, cl.plateid, cl.plate, cl.source,
            s.sampleid, s.patientid, s.timeperiod, s.dilution,
            s.predicted_concentration, s.final_concentration, s.se_concentration,
            s.pcov, s.pcov_rmse, s.pcov_pass
       FROM %s cl
       JOIN %s s ON s.curve_id = cl.curve_id %s
      WHERE %s
      ORDER BY cl.antigen, cl.plate, s.method, s.sampleid",
    .so_tbl("curve_lookup"), .so_tbl("calib_samples"), mth, .so_scope_sql(include_masked))
  .so_q(pool, sql, params)
}

# =============================================================================
# STANDARD POINTS: the standards used/excluded in each fit (for curve detail /
# QC drilldowns). Replaces best_standard_all.
# =============================================================================
so_fit_standards <- function(pool, project, study, methods = NULL, include_masked = FALSE) {
  proj <- .so_proj(project)
  params <- list(proj, as.character(study)[1])
  mth <- ""
  if (!is.null(methods) && length(methods)) { params <- c(params, list(paste0("{", paste(as.character(methods), collapse=","), "}")))
    mth <- sprintf(" AND st.method = ANY($%d)", length(params)) }
  sql <- sprintf(
    "SELECT cl.curve_id, st.method,
            (cl.experiment_accession || '_' || cl.nominal_sample_dilution) AS analyte,
            cl.antigen, cl.plateid, cl.plate, cl.source,
            st.well, st.dilution, st.concentration, st.log10_concentration,
            st.response_model, st.assay_response_raw, st.included, st.exclusion_reason
       FROM %s cl
       JOIN %s st ON st.curve_id = cl.curve_id %s
      WHERE %s
      ORDER BY cl.antigen, cl.plate, st.method, st.dilution",
    .so_tbl("curve_lookup"), .so_tbl("calib_standards"), mth, .so_scope_sql(include_masked))
  .so_q(pool, sql, params)
}

# =============================================================================
# COEFFICIENTS (long): the fitted terms for the selected model, one row per
# (curve, method, term). Kept long so callers pivot; term vocabulary is whatever
# the fitter writes (confirm with: SELECT DISTINCT term FROM calib_param). The
# fit-SUMMARY view does not need these -- they are for a curve-detail view.
# =============================================================================
so_fit_params <- function(pool, project, study, methods = NULL, include_masked = FALSE) {
  proj <- .so_proj(project)
  params <- list(proj, as.character(study)[1])
  mth <- ""
  if (!is.null(methods) && length(methods)) { params <- c(params, list(paste0("{", paste(as.character(methods), collapse=","), "}")))
    mth <- sprintf(" AND f.method = ANY($%d)", length(params)) }
  sql <- sprintf(
    "SELECT cl.curve_id, p.method, p.model_name, p.term,
            p.estimate, p.std_error, p.q_lo, p.q_med, p.q_hi,
            cl.antigen, cl.plateid, cl.source,
            (cl.experiment_accession || '_' || cl.nominal_sample_dilution) AS analyte
       FROM %s cl
       JOIN %s f ON f.curve_id = cl.curve_id AND f.is_best %s
       JOIN %s p ON p.curve_id = f.curve_id AND p.method = f.method
                AND p.model_name = f.model_name
      WHERE %s
      ORDER BY cl.antigen, cl.plateid, p.method, p.term",
    .so_tbl("curve_lookup"), .so_tbl("calib_fit"), mth, .so_tbl("calib_param"),
    .so_scope_sql(include_masked))
  .so_q(pool, sql, params)
}

# =============================================================================
# SAMPLE GATING SUMMARY  [fit pane]  -- rebuilds make_summspec's "sample" branch
# from calib_samples + calib_diagnostics (NOT best_sample_se_all). Per method.
# Returns one row per (analyte, antigen, plateid, plate) with specimen_type =
# "sample" and the counts the fit plot stacks:
#   n, ninloq, naboveloq, nbelowloq, nabovelod, nbelowlod, nlowbead, nhighbeadagg
# LOQ/LOD classes are derived by comparing the sample's final (fallback
# predicted) concentration to the diagnostic limits, mirroring the old
# gate_class_loq / gate_class_lod semantics (below = Too Diluted, above = Too
# Concentrated). Bead-count flags come from xmap_sample via so_raw_samples()
# (calib_samples has no bead columns). `raw` may be supplied to avoid re-pulling.
# =============================================================================
so_fit_sample_summary <- function(pool_, project, study, method, raw = NULL) {
  proj <- .so_proj(project)
  sql <- sprintf(
    "SELECT cl.curve_id, s.method,
            (cl.experiment_accession || '_' || cl.nominal_sample_dilution) AS analyte,
            cl.antigen, cl.plateid, cl.plate,
            s.sampleid, s.final_concentration, s.predicted_concentration, s.pcov_pass,
            d.lloq_conc, d.uloq_conc, d.lower_lod_conc, d.upper_lod_conc
       FROM %s cl
       JOIN %s s ON s.curve_id = cl.curve_id AND s.method = $3
       LEFT JOIN %s d ON d.curve_id = cl.curve_id AND d.method = s.method
      WHERE cl.project_id = $1 AND cl.study_accession = $2 AND NOT cl.masked",
    .so_tbl("curve_lookup"), .so_tbl("calib_samples"), .so_tbl("calib_diagnostics"))
  s <- .so_q(pool_, sql, list(proj, as.character(study)[1], as.character(method)[1]))
  if (!nrow(s)) return(data.frame())

  conc  <- ifelse(!is.na(s$final_concentration), s$final_concentration, s$predicted_concentration)
  below <- function(x, lim) !is.na(x) & !is.na(lim) & x < lim
  above <- function(x, lim) !is.na(x) & !is.na(lim) & x > lim
  s$is_belowloq <- below(conc, s$lloq_conc)
  s$is_aboveloq <- above(conc, s$uloq_conc)
  s$is_inloq    <- !(s$is_belowloq | s$is_aboveloq)
  s$is_belowlod <- below(conc, s$lower_lod_conc)
  s$is_abovelod <- above(conc, s$upper_lod_conc)

  agg <- dplyr::summarise(
    dplyr::group_by(s, analyte, antigen, plateid, plate),
    n         = dplyr::n(),
    ninloq    = sum(is_inloq,    na.rm = TRUE),
    naboveloq = sum(is_aboveloq, na.rm = TRUE),
    nbelowloq = sum(is_belowloq, na.rm = TRUE),
    nabovelod = sum(is_abovelod, na.rm = TRUE),
    nbelowlod = sum(is_belowlod, na.rm = TRUE),
    .groups   = "drop")

  # bead-count flags from xmap_sample (per analyte/antigen/plateid)
  if (is.null(raw)) raw <- so_raw_samples(pool_, project, study)
  if (!is.null(raw) && nrow(raw)) {
    bd <- dplyr::summarise(
      dplyr::group_by(raw, analyte, antigen, plateid),
      nlowbead     = sum(lowbeadn    == "LowBeadN",    na.rm = TRUE),
      nhighbeadagg = sum(highbeadagg == "PctAggBeads", na.rm = TRUE),
      .groups      = "drop")
    agg <- merge(agg, bd, by = c("analyte", "antigen", "plateid"), all.x = TRUE)
  }
  if (is.null(agg$nlowbead))     agg$nlowbead     <- 0L
  if (is.null(agg$nhighbeadagg)) agg$nhighbeadagg <- 0L
  agg$nlowbead[is.na(agg$nlowbead)]         <- 0L
  agg$nhighbeadagg[is.na(agg$nhighbeadagg)] <- 0L
  agg$specimen_type <- "sample"
  agg
}

# =============================================================================
# METHOD UNIVERSE: which methods actually have best fits in this study (drives
# the method selector so it only offers populated options).
# =============================================================================
so_fit_methods <- function(pool, project, study, include_masked = FALSE) {
  proj <- .so_proj(project)
  sql <- sprintf(
    "SELECT DISTINCT f.method
       FROM %s cl JOIN %s f ON f.curve_id = cl.curve_id AND f.is_best
      WHERE %s ORDER BY 1",
    .so_tbl("curve_lookup"), .so_tbl("calib_fit"), .so_scope_sql(include_masked))
  df <- .so_q(pool, sql, list(proj, as.character(study)[1]))
  if (!nrow(df)) character(0) else as.character(df$method)
}
