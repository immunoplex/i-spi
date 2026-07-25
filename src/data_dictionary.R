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
    what  = "Each test sample's concentration read off the fitted curve (predicted, and final = x dilution) with precision. This replaces the old Sample QC tab.",
    grain = "one row per curve / method / sample identity",
    keys  = c("sampleid", "patientid", "timeperiod", "dilution",
              "predicted_concentration", "final_concentration", "pcov_pass")),
  calib_diagnostics = list(
    group = "Results", label = "Diagnostics / LOQ",
    what  = "Per-curve quality metrics: LLOQ/ULOQ (concentration and log10), limits of detection, RDL, inflection point, and thresholds.",
    grain = "one row per curve / method",
    keys  = c("method", "lloq_conc", "uloq_conc", "lloq_log10", "uloq_log10",
              "lower_lod_conc", "upper_lod_conc", "inflect_x", "pcov_threshold")),
  calib_loo = list(
    group = "Results", label = "LOO comparison",
    what  = "Bayesian leave-one-out model comparison (Bayesian method only; empty for frequentist, which selects by AIC).",
    grain = "one row per curve / model_name (bayesian only)",
    keys  = c("model_name", "elpd_loo", "se_elpd_loo", "looic", "elpd_diff",
              "pareto_k_bad"))
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
                   "calib_grid", "calib_samples", "calib_diagnostics", "calib_loo")
)
