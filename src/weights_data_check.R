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

# SE spread (P90 / P10 of se_concentration over the quantified samples of one
# curve group) below which tau and kappa are hard to separate. A rough guide:
# in MADI_01 IgG1 (2026-11-20) kappa was weakly identified in 6 of 12 groups
# with a spread below 3 and in 3 of 11 above it; groups with few quantified
# samples were weak whatever their spread. Each fit's own identifiability note
# is the real test (check column of the fit table).
WEIGHTS_SE_SPREAD_NARROW <- 2
# Fewer quantified samples than this in a curve group: tau and kappa rest on
# little data.
WEIGHTS_MIN_QUANTIFIED <- 100

# Approximate run time of one weights fit (one curve group, one method) on the
# compute worker at the default 4000 iterations x 4 chains, from the 2026-11
# sim runs (wall time / groups, so the worker's parallelism is included):
#   multiplicative, MADI_P3_GAPS IgG1: 22 groups in 13.8-14.9 min  -> ~40 s
#   additive, no censoring, same data: 22 groups in 16.9-18.0 min  -> ~48 s
#   additive, 35% censored, MADI_01 IgG1: 23 groups in 53-57 min   -> ~145 s
# Run time is taken as proportional to iterations x chains (warm-up included);
# that scaling is an assumption, not yet measured.
WEIGHTS_SECONDS_PER_GROUP_MULT <- 40
WEIGHTS_SECONDS_PER_GROUP <- 48
WEIGHTS_SECONDS_PER_GROUP_CENSORED <- 145
WEIGHTS_RUNTIME_REF_DRAWS <- 4000 * 4

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

  # ---- spread of the measurement SE, per curve group ------------------------
  # Each curve group is fitted on its own, so tau and kappa are separated (or
  # not) group by group: a spread over the whole selection hides narrow groups.
  # Quantified samples only (censored ones do not inform the split).
  se_all <- if (!empty && "se_concentration" %in% names(cs)) suppressWarnings(as.numeric(cs$se_concentration)) else rep(NA_real_, n)
  quant <- is.finite(se_all) & se_all > 0 & !(ct %in% c("left", "right", "interval"))
  g_stats <- lapply(split(se_all[quant], grp[quant]), function(s) {
    c(spread = if (length(s) >= 10) {
        q <- stats::quantile(s, c(0.1, 0.9), names = FALSE); q[2] / q[1]
      } else NA_real_,
      n = length(s))
  })
  g_spread <- vapply(g_stats, `[[`, numeric(1), "spread")
  g_n      <- vapply(g_stats, `[[`, numeric(1), "n")
  se_spread <- if (any(is.finite(g_spread))) stats::median(g_spread, na.rm = TRUE) else NA_real_
  se_spread_min <- if (any(is.finite(g_spread))) min(g_spread, na.rm = TRUE) else NA_real_
  n_narrow <- sum(is.finite(g_spread) & g_spread < WEIGHTS_SE_SPREAD_NARROW)
  min_quant <- if (length(g_n)) as.integer(min(g_n)) else 0L
  n_few <- sum(g_n < WEIGHTS_MIN_QUANTIFIED)
  se_narrow <- is.finite(se_spread) && se_spread < WEIGHTS_SE_SPREAD_NARROW

  # ---- facts table (what the panel lists) -----------------------------------
  pct <- function(k) if (n) sprintf("%.0f%%", 100 * k / n) else "0%"
  facts <- data.frame(
    item = c("samples", "people", "censoring", "dilutions", "se_spread", "stored"),
    label = c("Samples", "People", "Out of range", "Readings per sample",
              "SE spread (P90/P10, per curve group)", "Stored weights"),
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
      if (!is.finite(se_spread)) "too few samples"
      else if (length(g_spread) == 1) sprintf("%.1f (%d quantified samples)", se_spread, min_quant)
      else sprintf("median %.1f, lowest %.1f; %d of %d groups below %g; fewest quantified samples in a group: %d",
                   se_spread, se_spread_min, n_narrow, length(g_spread),
                   WEIGHTS_SE_SPREAD_NARROW, min_quant),
      .wdc_stored_text(stored)),
    status = c(
      if (n) "ok" else "bad",
      if (has_people) "ok" else "info",
      if (!cens_recorded) "warn" else "ok",
      if (!has_people) "info" else if (per_sample > 1) "warn" else "ok",
      if (!is.finite(se_spread) || n_narrow > 0 || n_few > 0) "warn" else "ok",
      if (is.null(stored) || !nrow(stored)) "info" else "warn"),
    note = c(
      "",
      if (has_people) "" else "Without patient IDs repeated samples of one person cannot be recognised.",
      if (!cens_recorded) "Out-of-range samples will be treated as measured values with a large SE. Recalibrate this experiment to use censoring."
      else "",
      if (has_people && per_sample > 1)
        "Each reading counts as a separate sample, which overstates the information they carry; a sample term is not available yet."
      else "",
      if (!is.finite(se_spread)) "Not enough samples with an SE to judge."
      else paste0(
        if (se_narrow) "In most groups the SE barely varies, so tau and kappa are hard to separate: consider fixing kappa at 1. "
        else if (n_narrow > 0 || n_few > 0) "Some groups have a narrow spread or few quantified samples; kappa may be weakly identified there. "
        else "",
        "A rough guide only: each fit reports whether kappa was identified (check column of the fit table)."),
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
       readings_per_sample = per_sample, n_censored = n_cens,
       se_spread_min = se_spread_min, n_groups_narrow = n_narrow,
       min_quantified = min_quant,
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
# Seconds per curve group for a model and MCMC length (see the constants above).
weights_seconds_per_group <- function(model, iter = 4000, chains = 4, use_censoring = FALSE) {
  base <- if (!identical(model, "additive")) WEIGHTS_SECONDS_PER_GROUP_MULT
          else if (isTRUE(use_censoring)) WEIGHTS_SECONDS_PER_GROUP_CENSORED
          else WEIGHTS_SECONDS_PER_GROUP
  iter <- suppressWarnings(as.numeric(iter)); chains <- suppressWarnings(as.numeric(chains))
  if (!is.finite(iter) || iter <= 0) iter <- 4000
  if (!is.finite(chains) || chains <= 0) chains <- 4
  base * iter * chains / WEIGHTS_RUNTIME_REF_DRAWS
}

# Whole minutes for n_groups, at least 1; NA when the group count is unknown.
weights_runtime_minutes <- function(model, n_groups, iter = 4000, chains = 4,
                                    use_censoring = FALSE) {
  if (length(n_groups) != 1 || !is.finite(n_groups) || n_groups <= 0) return(NA_integer_)
  max(1L, as.integer(round(n_groups * weights_seconds_per_group(model, iter, chains, use_censoring) / 60)))
}

# The run-time line shown above Submit, for both models.
weights_runtime_text <- function(model, n_groups, iter = 4000, chains = 4,
                                 use_censoring = FALSE) {
  mins <- weights_runtime_minutes(model, n_groups, iter, chains, use_censoring)
  if (is.na(mins)) return("Estimated run time: select curves to see it.")
  s <- weights_seconds_per_group(model, iter, chains, use_censoring)
  per <- if (s < 90) sprintf("%d s", as.integer(round(s))) else sprintf("%.1f min", s / 60)
  fmt_min <- function(m) if (m < 90) sprintf("%d min", m) else sprintf("%.1f h", m / 60)
  paste0("Estimated run time: about ", fmt_min(mins), " (",
         n_groups, " curve group", if (n_groups == 1) "" else "s", " \u00d7 about ", per,
         " each at ", format(as.integer(iter), big.mark = ","), " iterations \u00d7 ",
         as.integer(chains), " chains",
         if (identical(model, "additive") && isTRUE(use_censoring)) ", with censoring" else "",
         "). It grows in proportion to iterations \u00d7 chains.")
}

weights_review_text <- function(model, method, opts = list(), n_groups = NA_integer_,
                                stored_models = character(0)) {
  meth <- if (identical(method, "bayesian")) "Bayesian" else "frequentist"
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
