# =============================================================================
# data_dictionary.R  --  plain-English descriptions of every table shown on the
# Data tab, so a user (or a public consumer of the export) understands what each
# one holds. dataTab reads these to render a caption/tooltip above each table.
#
# Each entry: group, one-line what-it-is, grain (one row per ...), and the key
# columns worth pointing at. Keep it human -- this is documentation, not schema.
# =============================================================================

CALIB_TABLE_DOCS <- list(

  # ---- Raw inputs (base xmap_* tables; masks visible here) ----------------
  xmap_header = list(
    group = "Raw inputs", label = "Plates",
    what  = "One row per plate read: the assay run metadata for each plate.",
    grain = "one row per study / experiment / plate",
    keys  = c("plateid", "plate", "nominal_sample_dilution", "wavelengths",
              "masked", "mask_reason")),
  xmap_standard = list(
    group = "Raw inputs", label = "Standard",
    what  = "Calibration/standard-curve points: the reference dilution series used to fit each curve.",
    grain = "one row per plate / antigen / standard well",
    keys  = c("antigen", "dilution", "antibody_mfi", "source", "masked", "mask_reason")),
  xmap_control = list(
    group = "Raw inputs", label = "Control",
    what  = "Positive/negative control wells run alongside the samples.",
    grain = "one row per plate / antigen / control well",
    keys  = c("antigen", "sampleid", "dilution", "antibody_mfi", "masked", "mask_reason")),
  xmap_buffer = list(
    group = "Raw inputs", label = "Blank",
    what  = "Blank / buffer wells used for background estimation.",
    grain = "one row per plate / antigen / blank well",
    keys  = c("antigen", "antibody_mfi", "masked", "mask_reason")),
  xmap_sample = list(
    group = "Raw inputs", label = "Sample",
    what  = "The test (patient) samples measured against the standard curve.",
    grain = "one row per plate / antigen / sample well",
    keys  = c("antigen", "sampleid", "patientid", "timeperiod", "dilution",
              "antibody_mfi", "masked", "mask_reason")),

  # ---- Registry -----------------------------------------------------------
  curve_lookup = list(
    group = "Registry", label = "Curve lookup",
    what  = "The stable registry of calibration curves. Each curve_id is defined by the 10-column natural key; everything in Results joins back here.",
    grain = "one row per curve (natural key)",
    keys  = c("curve_id", "antigen", "feature", "plateid", "plate", "source",
              "wavelength", "nominal_sample_dilution", "masked", "mask_reason")),

  # ---- Results (calib_*; written by the i-spi-compute worker) -------------
  calib_run = list(
    group = "Results", label = "Run",
    what  = "One row per compute job: which engine/version ran, its parameters, status and timing.",
    grain = "one row per job_id",
    keys  = c("job_id", "method", "package", "version", "best_model", "status")),
  calib_fit = list(
    group = "Results", label = "Fit / model selection",
    what  = "Every candidate model fitted for each curve, with the selection outcome. The winning model is the is_best row.",
    grain = "one row per curve / method / model_name",
    keys  = c("method", "model_name", "is_best", "converged", "eligible",
              "score_type", "selection_score")),
  calib_param = list(
    group = "Results", label = "Parameters",
    what  = "Fitted parameter estimates (with uncertainty) per model term.",
    grain = "one row per curve / method / model_name / term",
    keys  = c("model_name", "term", "estimate", "std_error", "q_lo", "q_med", "q_hi")),
  calib_gate = list(
    group = "Results", label = "Eligibility gates",
    what  = "Pass/fail checks that decide whether a fitted model is eligible for selection.",
    grain = "one row per curve / method / model_name / gate",
    keys  = c("model_name", "gate", "passed", "detail")),
  calib_grid = list(
    group = "Results", label = "Fitted grid",
    what  = "The dense (~200-point) fitted curve used for plotting: predicted response with CI band, inverse prediction, and the pcov QC series.",
    grain = "one row per curve / method / grid point",
    keys  = c("method", "log10_concentration", "predicted_response",
              "ci_lower", "ci_upper", "predicted_concentration", "pcov_pass")),
  calib_samples = list(
    group = "Results", label = "Back-calculated samples",
    what  = "Each test sample's concentration read off the fitted curve (predicted, and final = x dilution) with precision, its censoring class/bounds when outside the quantifiable range, a 95% predictive interval, and threshold probabilities. This replaces the old Sample QC tab.",
    grain = "one row per curve / method / sample identity",
    keys  = c("sampleid", "patientid", "timeperiod", "dilution",
              "predicted_concentration", "final_concentration", "pcov_pass",
              "censor_class", "censor_lower", "censor_upper",
              "conc_q_lo", "conc_q_hi", "p_below_cutoff")),
  calib_diagnostics = list(
    group = "Results", label = "Diagnostics / LOQ",
    what  = "Per-curve quality metrics: LLOQ/ULOQ (concentration and log10), limits of detection, RDL, inflection point, and thresholds.",
    grain = "one row per curve / method",
    keys  = c("method", "lloq_conc", "uloq_conc", "lloq_log10", "uloq_log10",
              "lower_lod_conc", "upper_lod_conc", "inflect_x", "pcov_threshold")),
  calib_controls = list(
    group = "Results", label = "Back-calculated controls",
    what  = "Each control well's concentration read off the fitted curve, exactly as for test samples (predicted, final = x dilution, pcov, gate and censoring class, 95% interval). One row per standard source of the plate; masked wells are included with included = false. Needs i-spi-compute 1.1.",
    grain = "one row per curve / method / control well",
    keys  = c("well", "sampleid", "source", "dilution", "included", "predicted_concentration",
              "final_concentration", "pcov", "pcov_gate_class", "censor_class", "conc_q_lo", "conc_q_hi")),
  calib_loo = list(
    group = "Results", label = "LOO comparison",
    what  = "Bayesian leave-one-out model comparison (Bayesian method only; empty for frequentist, which selects by AIC).",
    grain = "one row per curve / model_name (bayesian only)",
    keys  = c("model_name", "elpd_loo", "se_elpd_loo", "looic", "elpd_diff",
              "pareto_k_bad")),
  calib_weights = list(
    group = "Results", label = "Precision weights",
    what  = "curveRweights per-sample precision weights (sigma/w/w_norm). Multiplicative model: sigma_i = phi * se_i^beta1. Additive model: sigma_i^2 = tau^2 + v_meas, with per-sample reliability and censoring flags.",
    grain = "one row per curve / method / sample identity",
    keys  = c("sampleid", "patientid", "timeperiod", "agroup", "dilution",
              "se", "pcov", "sigma", "w", "w_norm", "model", "v_meas", "tau2",
              "reliability", "censor_type", "bound")),
  calib_weights_fit = list(
    group = "Results", label = "Precision weights fit",
    what  = "One curveRweights fit per multiplate group x method: phi/beta1 (multiplicative) or tau/kappa/reliability (additive) estimates, diagnostics, and interpretation.",
    grain = "one row per multiplate_group_id / method",
    keys  = c("method", "design_cols", "scale_predictor", "phi", "beta1",
              "interpretation", "n_fit", "n_eff", "weight_ratio", "model", "tau", "kappa",
              "reliability", "n_censored")),
  control_reference = list(
    group = "Results", label = "Control reference values",
    what  = "Expected values for controls (assigned or established), with limits and the period they apply to. Rows for this study and project-wide rows (study_accession = '__none__') are shown; compare them with Back-calculated controls. Needs i-spi-compute 1.1.",
    grain = "one row per project / study / control source / lot / antigen / feature / valid_from",
    keys  = c("study_accession", "control_source", "control_lot", "antigen", "feature", "units",
              "expected_conc", "expected_sd", "lower_limit", "upper_limit", "basis", "valid_from", "valid_to"))
)

#' Look up the human description for a table (by physical name). Returns a list
#' with $group/$label/$what/$grain/$keys, or a minimal stub for unknown tables.
table_doc <- function(name) {
  d <- CALIB_TABLE_DOCS[[name]]
  if (is.null(d)) list(group = "Other", label = name,
                       what = "(no description available)", grain = "", keys = character(0))
  else d
}

#' The tables to show under the Data tab, in display order, grouped. Drives the
#' grouped tabset. best_* / bayes_* legacy tables are intentionally absent.
CALIB_TABLE_ORDER <- list(
  `Raw inputs` = c("xmap_header", "xmap_standard", "xmap_control", "xmap_buffer", "xmap_sample"),
  Registry     = c("curve_lookup"),
  Results      = c("calib_run", "calib_fit", "calib_param", "calib_gate",
                   "calib_grid", "calib_samples", "calib_controls", "calib_diagnostics", "calib_loo",
                   "calib_weights", "calib_weights_fit", "control_reference")
)

#' Metadata describing HOW the precision weights in a bundle were produced.
#'
#' Included as `weights_meta` in the RData, JSON and Excel bundles (the per-table
#' CSVs already carry `model` and `weights_pkg_version` on every row). One row per
#' (method, model) present in calib_weights; a single "no weights" row when none
#' have been computed. The most recent run for a sample and method is the one
#' stored, so this says which model the weights in the bundle came from.
#'
#' @param weights_df calib_weights rows (any extra columns are ignored).
#' @param fit_df     calib_weights_fit rows (optional).
#' @return a data.frame.
weights_bundle_meta <- function(weights_df, fit_df = NULL) {
  docs <- c(
    multiplicative = "sigma_i = phi * se_i^beta1; w = 1/sigma^2; every sample weighted (no censoring).",
    additive       = "sigma_i^2 = tau^2 + kappa^2 * se_i^(2*beta1) (beta1 fixed at 1 by default); w = 1/sigma^2, bounded by 1/tau^2; reliability = tau^2 / sigma^2; out-of-range samples are censored and have no w_norm.")
  guide <- "https://immunoplex.github.io/curveRweights/articles/measurement-error.html"
  none <- data.frame(method = NA_character_, model = NA_character_,
                     weights_pkg_version = NA_character_, n_samples = 0L,
                     n_curve_groups = 0L, n_censored_samples = 0L,
                     first_computed = NA_character_, last_computed = NA_character_,
                     description = "No precision weights have been computed for this experiment.",
                     guide = guide, stringsAsFactors = FALSE)
  if (is.null(weights_df) || !nrow(weights_df)) return(none)

  w <- weights_df
  if (!"model" %in% names(w)) w$model <- "multiplicative"
  w$model[is.na(w$model) | !nzchar(w$model)] <- "multiplicative"
  if (!"weights_pkg_version" %in% names(w)) w$weights_pkg_version <- NA_character_
  if (!"censor_type" %in% names(w)) w$censor_type <- NA_character_
  w$.ts <- if ("created_at" %in% names(w)) as.character(w$created_at) else NA_character_
  uniq <- function(x) { x <- unique(x[!is.na(x) & nzchar(x)]); if (length(x)) paste(sort(x), collapse = "; ") else NA_character_ }
  tstat <- function(x, f) { x <- x[!is.na(x)]; if (length(x)) f(x) else NA_character_ }

  f <- fit_df
  if (!is.null(f) && nrow(f)) {
    if (!"model" %in% names(f)) f$model <- "multiplicative"
    f$model[is.na(f$model) | !nzchar(f$model)] <- "multiplicative"
  }
  key <- paste(w$method, w$model, sep = "\r")
  out <- lapply(unique(key), function(k) {
    i <- key == k; m <- w$model[i][1L]; me <- w$method[i][1L]
    ng <- if (!is.null(f) && nrow(f) && "multiplate_group_id" %in% names(f))
      length(unique(f$multiplate_group_id[f$method == me & f$model == m])) else NA_integer_
    data.frame(method = me, model = m,
               weights_pkg_version = uniq(w$weights_pkg_version[i]),
               n_samples = sum(i), n_curve_groups = as.integer(ng),
               n_censored_samples = sum(w$censor_type[i] %in% c("left", "right", "interval")),
               first_computed = tstat(w$.ts[i], min), last_computed = tstat(w$.ts[i], max),
               description = unname(docs[m]), guide = guide, stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}
