# =============================================================================
# weights_data_check.R
# "Check the data" step of the Compute-weights panel: facts about the selected
# calib_samples that decide which weights model and which additive options make
# sense, computed BEFORE a job is submitted. Pure functions (no Shiny, no DB);
# unit-tested in test-weights-data-check.R. Only used when the additive model is
# enabled (additive_weights_enabled()).
#
# Thresholds and wording follow the curveRweights measurement-error vignette
# (#identifiability, #censoring, #repeated).
# =============================================================================

# SE spread (P90 / P10 of se_concentration) below which tau and kappa are hard
# to separate: kappa is then weakly identified and fixing it at 1 is offered.
WEIGHTS_SE_SPREAD_NARROW <- 2

# Approximate run time of one weights fit (one curve group, one method) on the
# compute worker, from the 2026-11 runs (22 groups in 14-18 minutes).
WEIGHTS_SECONDS_PER_GROUP <- 45

.wdc_missing_id <- function(x) {
  x <- as.character(x)
  is.na(x) | !nzchar(trimws(x)) | x == "__none__"
}

# cs: calib_samples rows for the scope and method (fetch_calib_samples_for_scope),
#     with antigen/feature from curve_lookup.
# stored: optional one-row-per-model summary of the weights already stored for
#     the same scope and method: columns model, n, last_computed.
weights_data_check <- function(cs, stored = NULL) {
  empty <- is.null(cs) || !nrow(cs)
  n <- if (empty) 0L else nrow(cs)
  grp <- if (!empty && all(c("antigen", "feature") %in% names(cs)))
    paste(cs$antigen, cs$feature, sep = "|") else rep("all", n)
  n_groups <- length(unique(grp))

  # ---- people and repeated measures ----------------------------------------
  pid_ok <- if (empty || !"patientid" %in% names(cs)) logical(0) else !.wdc_missing_id(cs$patientid)
  has_people <- length(pid_ok) && all(pid_ok)
  n_people <- if (length(pid_ok)) length(unique(cs$patientid[pid_ok])) else 0L
  tp <- if (!empty && "timeperiod" %in% names(cs)) as.character(cs$timeperiod) else rep("", n)
  n_tp <- length(unique(tp[!.wdc_missing_id(tp)]))
  # samples per person within one curve group (a person at several time points)
  per_person <- if (has_people) max(tapply(tp, paste(grp, cs$patientid), function(x) length(unique(x)))) else 0L
  # readings per physical sample (person x time point) within one curve group:
  # > 1 means several dilutions or replicates of the same sample
  per_sample <- if (has_people) max(table(paste(grp, cs$patientid, tp))) else 0L

  # ---- censoring -------------------------------------------------------------
  ct <- if (!empty && "censor_type" %in% names(cs)) as.character(cs$censor_type) else rep(NA_character_, n)
  cens_recorded <- any(!is.na(ct))
  n_left <- sum(ct %in% "left"); n_right <- sum(ct %in% "right"); n_int <- sum(ct %in% "interval")
  n_cens <- n_left + n_right + n_int

  # ---- spread of the measurement SE -----------------------------------------
  se <- if (!empty && "se_concentration" %in% names(cs)) suppressWarnings(as.numeric(cs$se_concentration)) else numeric(0)
  se <- se[is.finite(se) & se > 0]
  se_spread <- if (length(se) >= 10) {
    q <- stats::quantile(se, c(0.1, 0.9), names = FALSE); q[2] / q[1]
  } else NA_real_
  se_narrow <- is.finite(se_spread) && se_spread < WEIGHTS_SE_SPREAD_NARROW

  # ---- facts table (what the panel lists) -----------------------------------
  pct <- function(k) if (n) sprintf("%.0f%%", 100 * k / n) else "0%"
  facts <- data.frame(
    item = c("samples", "people", "censoring", "dilutions", "se_spread", "stored"),
    label = c("Samples", "People", "Out of range", "Readings per sample",
              "SE spread (P90/P10)", "Stored weights"),
    value = c(
      sprintf("%s in %d curve group%s", format(n, big.mark = ","), n_groups,
              if (n_groups == 1) "" else "s"),
      if (has_people) sprintf("%s; up to %d time point%s per person", format(n_people, big.mark = ","),
                              per_person, if (per_person == 1) "" else "s")
      else "no patient IDs on some or all samples",
      if (!cens_recorded) "not recorded (calibrated before censoring was stored)"
      else if (n_cens == 0) "none"
      else sprintf("%s (%s below, %s above, %s interval)", pct(n_cens),
                   format(n_left, big.mark = ","), format(n_right, big.mark = ","),
                   format(n_int, big.mark = ",")),
      if (!has_people) "unknown (needs patient IDs)"
      else if (per_sample > 1) sprintf("up to %d (several dilutions or plates)", per_sample)
      else "1",
      if (is.finite(se_spread)) sprintf("%.1f", se_spread) else "too few samples",
      .wdc_stored_text(stored)),
    status = c(
      if (n) "ok" else "bad",
      if (has_people) "ok" else "info",
      if (!cens_recorded) "warn" else "ok",
      if (!has_people) "info" else if (per_sample > 1) "warn" else "ok",
      if (!is.finite(se_spread)) "warn" else if (se_narrow) "warn" else "ok",
      if (is.null(stored) || !nrow(stored)) "info" else "warn"),
    note = c(
      "",
      if (has_people) "" else "Without patient IDs repeated samples of one person cannot be recognised.",
      if (!cens_recorded) "Out-of-range samples will be treated as measured values with a large SE. Recalibrate this experiment to use censoring."
      else "",
      if (has_people && per_sample > 1)
        "Each reading counts as a separate sample, which overstates the information they carry; a sample term is not available yet."
      else "",
      if (se_narrow) "The SE barely varies between samples, so tau and kappa are hard to separate. Consider fixing kappa at 1."
      else if (!is.finite(se_spread)) "Not enough samples with an SE to judge." else "Wide enough to separate tau and kappa.",
      if (is.null(stored) || !nrow(stored)) "" else "A new run replaces these weights."),
    help = c("", "repeated", "censoring", "repeated", "identifiability", ""),
    stringsAsFactors = FALSE)

  # ---- what the additive options can be, and their defaults -----------------
  available <- list(
    use_censoring  = cens_recorded && n_cens > 0,
    patient_effect = has_people,
    fix_kappa      = TRUE)
  defaults <- list(
    use_censoring  = available$use_censoring,
    patient_effect = has_people && per_person > 1,
    fix_kappa      = se_narrow)
  why_unavailable <- list(
    use_censoring  = if (!cens_recorded) "censoring was not recorded for these samples"
                     else if (n_cens == 0) "no sample is out of range" else "",
    patient_effect = if (!has_people) "some samples have no patient ID" else "",
    fix_kappa      = "")

  # ---- recommendation (a sentence, never an automatic switch) ---------------
  reasons <- c(
    if (has_people && per_person > 1) "people are measured at several time points",
    if (n_cens > 0) sprintf("%s of samples are out of range", pct(n_cens)),
    if (is.finite(se_spread) && !se_narrow) "the SE varies enough to separate signal from measurement error")
  stored_models <- if (is.null(stored) || !nrow(stored)) character(0) else unique(as.character(stored$model))
  recommendation <- if (!n) "No calibrated samples in this selection."
    else if (length(reasons))
      paste0("Additive model recommended for comparing groups or time points: ",
             paste(reasons, collapse = "; "), ".")
    else "Either model is reasonable here; the additive model is still preferred for comparing groups."
  if ("multiplicative" %in% stored_models)
    recommendation <- paste(recommendation,
      "To reproduce an analysis made with the stored weights, keep the multiplicative model.")

  list(facts = facts, available = available, defaults = defaults,
       readings_per_sample = per_sample,
       why_unavailable = why_unavailable, recommendation = recommendation,
       n_samples = n, n_groups = n_groups, se_spread = se_spread,
       stored_models = stored_models)
}

.wdc_stored_text <- function(stored) {
  if (is.null(stored) || !nrow(stored)) return("none")
  paste(vapply(seq_len(nrow(stored)), function(i) {
    when <- stored$last_computed[i]
    sprintf("%s (%s rows, %s)", stored$model[i], format(stored$n[i], big.mark = ","),
            if (is.null(when) || is.na(when)) "date unknown" else format(as.Date(when)))
  }, character(1)), collapse = "; ")
}

# Plain-language line shown above the Submit button.
weights_review_text <- function(model, method, opts = list(), n_groups = NA_integer_,
                                stored_models = character(0)) {
  meth <- if (identical(method, "bayesian")) "Bayesian" else "frequentist"
  mins <- if (is.finite(n_groups) && n_groups > 0)
    max(1L, as.integer(round(n_groups * WEIGHTS_SECONDS_PER_GROUP / 60))) else NA_integer_
  what <- if (identical(model, "additive")) {
    parts <- c(
      if (isTRUE(opts$use_censoring)) "out-of-range samples censored" else "no censoring",
      if (isTRUE(opts$patient_effect)) "repeated samples per person accounted for" else "no person term",
      if (isTRUE(opts$fix_kappa)) "kappa fixed at 1" else "kappa estimated",
      if (isTRUE(opts$estimate_beta1)) "beta1 estimated")
    paste0("Additive model (", paste(parts, collapse = ", "), ")")
  } else "Multiplicative model"
  paste0(what, ", ", meth, " calibration",
         if (is.finite(n_groups)) sprintf(", %d curve group%s", n_groups, if (n_groups == 1) "" else "s") else "",
         if (!is.na(mins)) sprintf(", about %d min", mins) else "",
         ". Replaces: ",
         if (length(stored_models)) paste(stored_models, "weights for this method") else "nothing",
         ".")
}

# Job params for the additive options. Only values that differ from the
# worker's defaults are sent, so a job with default options is identical to
# one sent by I-SPI 1.1.1 (the worker's defaults: censoring on, person term on,
# kappa estimated).
weights_additive_params <- function(opts, available) {
  p <- list()
  if (isTRUE(available$use_censoring) && !isTRUE(opts$use_censoring)) p$use_censoring <- "false"
  if (isTRUE(available$patient_effect) && !isTRUE(opts$patient_effect)) p$patient_effect <- "false"
  if (isTRUE(opts$fix_kappa)) p$fix_kappa <- "true"
  p
}
