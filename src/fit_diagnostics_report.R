# =============================================================================
# fit_diagnostics_report.R  --  one experiment's compute-fit diagnostics, as
# something a person OR an LLM can read, plus a rule-based assessment.
#
# Why: a pooled (hierarchical) Bayesian fit can look "odd" for several different
# reasons -- sampler trouble, one plate that fails the quantification gates (a
# model is only eligible when EVERY plate passes), or plates that are not really
# exchangeable (different signal level / day). The numbers that separate those
# causes are all persisted (calib_fit, calib_fit_diag, calib_hyperparam,
# calib_param, calib_gate, calib_loo) but live in six tables nobody reads side by
# side. This puts them in one document, then says what they imply.
#
# Pure functions over plain data frames (so they are testable without a DB);
# only fit_diag_fetch() touches the pool.
#
#   d   <- fit_diag_fetch(pool, project, study, experiment)
#   rep <- fit_diag_build(d, scope)
#   fit_diag_report_text(rep)   # Markdown, written for an LLM to read
#   fit_diag_report_json(rep)   # the same content, structured
#   fit_diag_notice(rep)        # NULL, or list(severity, text) for the UI
#
# FIT UNIT = (multiplate_group_id, job_id), not the group alone. The worker saves
# per curve, so submitting plates 1-5, then 6-10, then 1-10 leaves one group whose
# rows come from several jobs; only the latest job's fit of each curve survives.
# Each unit is therefore assessed on its own, and the report says how many plates
# were pooled in it.
# =============================================================================

FD_THRESH <- list(
  rhat_warn = 1.01, rhat_err = 1.05,     # split R-hat
  ess_warn = 400, ess_err = 100,         # min bulk/tail effective sample size
  ebfmi_warn = 0.3,
  div_pct_err = 1,                       # % divergent draws that makes it an error
  big_group = 8L,                        # pooled plates at/after which size is mentioned
  outlier_z = 3.5, outlier_min_plates = 5L,
  d_spread_warn = 0.5,                   # (max-min)/median|d| across plates
  c_spread_warn = 1.0,                   # range of inflection c, fit-scale units
  rel_se_warn = 0.5,                     # SE/|estimate| of a plate parameter
  hyper_cv_warn = 0.5,                   # sigma_d / |mu_d|
  nu_warn = 3, nu_info = 6,              # Student-t df (bounded below at 2): ~2 = likelihood absorbing misfit
  hess_cond_warn = 1e8                   # frequentist vcov condition number
)

.fd_n  <- function(x) suppressWarnings(as.numeric(x))
.fd_or <- function(a, b) if (is.null(a) || !length(a) || all(is.na(a))) b else a
.fd_has <- function(x, cols = NULL) is.data.frame(x) && nrow(x) > 0L && all(cols %in% names(x))

# bigint ids arrive as integer64; as.character() on those is only right when bit64
# is attached, so go through it explicitly. Applied once, in fit_diag_build().
.fd_id <- function(x) {
  if (inherits(x, "integer64") && requireNamespace("bit64", quietly = TRUE)) bit64::as.character.integer64(x)
  else as.character(x)
}

# Settings that change what a fit means (and so whether two fits are comparable).
FD_SETTINGS <- c("is_log_response", "is_log_independent", "model_form_list", "apply_prozone",
                 "blank_option", "adapt_delta", "bayes_chains", "bayes_warmup", "bayes_sampling",
                 "bayes_single_plate", "pcov_threshold", "include_measurement_error")

# Scale each stored curve was fitted on, from deviations measured in SQL.
.fd_scale <- function(dev_raw, dev_log10, dev_ln, tol = 1e-6) {
  vapply(seq_along(dev_raw), function(i) {
    v <- c(raw = dev_raw[i], log10 = dev_log10[i], ln = dev_ln[i]); v <- v[is.finite(v)]
    if (!length(v)) return(NA_character_)
    k <- names(v)[which.min(v)]
    if (v[[k]] < tol) k else "other"
  }, character(1))
}

.fd_fmt <- function(x, digits = 4) {
  x <- .fd_n(x)
  vapply(x, function(v) if (is.na(v)) "NA" else trimws(formatC(signif(v, digits), format = "fg", digits = digits)),
         character(1))
}

# Robust z (median / MAD); NA when the spread is zero or there is too little to say.
.fd_rz <- function(x) {
  x <- .fd_n(x); m <- stats::median(x, na.rm = TRUE)
  s <- 1.4826 * stats::mad(x, constant = 1, na.rm = TRUE)
  if (!is.finite(s) || s == 0) rep(NA_real_, length(x)) else (x - m) / s
}

.fd_md_table <- function(df, max_rows = 120L) {
  if (!is.data.frame(df) || !nrow(df)) return("_(none)_")
  cut <- nrow(df) > max_rows
  df  <- utils::head(df, max_rows)
  df[] <- lapply(df, function(col) {
    if (is.numeric(col)) .fd_fmt(col)
    else ifelse(is.na(col), "NA", gsub("|", "/", as.character(col), fixed = TRUE))
  })
  c(paste0("| ", paste(names(df), collapse = " | "), " |"),
    paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|"),
    vapply(seq_len(nrow(df)), function(i) paste0("| ", paste(unlist(df[i, ]), collapse = " | "), " |"),
           character(1)),
    if (cut) "_(truncated)_")
}

# ---- fetch ------------------------------------------------------------------
# Every piece is optional: an older database may lack calib_fit_diag /
# calib_hyperparam, and the report should degrade to "not available", not fail.
fit_diag_fetch <- function(pool, project, study, experiment) {
  try1 <- function(f) tryCatch(f(), error = function(e) { message("[fit-diag] ", conditionMessage(e)); NULL })
  groups <- try1(function() .calib_q(pool, sprintf(
    "SELECT curve_id, multiplate_group_id FROM %s
      WHERE project_id = $1 AND study_accession = $2 AND experiment_accession = $3",
    .tbl("curve_lookup")), params = list(project, study, experiment)))
  lookup <- try1(function() fetch_curve_lookup_scoped(pool, project, study, experiment))
  # Effective settings per (feature, antigen): the cascade can differ by antigen.
  settings <- try1(function() {
    fa <- unique(lookup[intersect(c("feature", "antigen"), names(lookup))])
    do.call(rbind, lapply(seq_len(nrow(fa)), function(i) {
      r <- resolve_settings_scoped(pool, project, study, experiment, feature = fa$feature[i], antigen = fa$antigen[i])
      r <- r[r$param_name %in% FD_SETTINGS, , drop = FALSE]
      if (!nrow(r)) return(NULL)
      data.frame(feature = fa$feature[i], antigen = fa$antigen[i], param = r$param_name,
                 value = as.character(r$value_text), stringsAsFactors = FALSE)
    }))
  })
  list(
    lookup = lookup, settings = settings,
    std    = try1(function() fetch_calib_standards_summary_scoped(pool, project, study, experiment)),
    groups = groups,
    fit    = try1(function() fetch_calib_fit_scoped(pool, project, study, experiment)),
    diag   = try1(function() fetch_calib_fit_diag_scoped(pool, project, study, experiment)),
    hyper  = try1(function() fetch_calib_hyperparam_scoped(pool, project, study, experiment)),
    param  = try1(function() fetch_calib_param_scoped(pool, project, study, experiment)),
    gate   = try1(function() fetch_calib_gate_scoped(pool, project, study, experiment)),
    loo    = try1(function() fetch_calib_loo_scoped(pool, project, study, experiment)),
    run    = try1(function() fetch_calib_run_scoped(pool, project, study, experiment)))
}

# ---- fit units --------------------------------------------------------------
# One row per (curve, method): which fit unit it belongs to and its selected model.
.fd_units <- function(d) {
  fit <- d$fit
  if (!.fd_has(fit, c("curve_id", "method"))) return(NULL)
  fit$curve_id <- as.character(fit$curve_id)
  best <- fit[fit$is_best %in% TRUE, , drop = FALSE]
  if (!nrow(best)) return(NULL)
  best <- best[!duplicated(best[c("curve_id", "method")]), , drop = FALSE]
  grp <- if (.fd_has(d$groups, c("curve_id", "multiplate_group_id")))
           stats::setNames(as.character(d$groups$multiplate_group_id), as.character(d$groups$curve_id))
         else character(0)
  g <- unname(grp[best$curve_id]); g[is.na(g)] <- "ungrouped"
  job <- as.character(.fd_or(best$job_id, "unknown")); job[is.na(job)] <- "unknown"
  data.frame(
    curve_id = best$curve_id, method = best$method, model = best$model_name,
    group = g, job_id = job, unit = paste(best$method, substr(g, 1, 8), substr(job, 1, 8), sep = "/"),
    is_fallback = best$is_fallback %in% TRUE,
    plateid  = if ("plateid"  %in% names(best)) as.character(best$plateid)  else NA_character_,
    antigen  = if ("antigen"  %in% names(best)) as.character(best$antigen)  else NA_character_,
    feature  = if ("feature"  %in% names(best)) as.character(best$feature)  else NA_character_,
    source   = if ("source"   %in% names(best)) as.character(best$source)   else NA_character_,
    wavelength = if ("wavelength" %in% names(best)) as.character(best$wavelength) else NA_character_,
    stringsAsFactors = FALSE)
}

# ---- one unit ---------------------------------------------------------------
.fd_unit <- function(u, d, TH) {
  cids <- u$curve_id; n <- length(cids)
  method <- u$method[1]
  model  <- names(sort(table(u$model), decreasing = TRUE))[1]
  plates <- ifelse(is.na(u$plateid), u$curve_id, u$plateid)
  F <- list(); add <- function(id, sev, msg, evidence = "", action = "")
    F[[length(F) + 1L]] <<- data.frame(unit = u$unit[1], finding = id, severity = sev,
      message = msg, evidence = evidence, action = action, stringsAsFactors = FALSE)
  info <- list(unit = u$unit[1], method = method, group = u$group[1], job_id = u$job_id[1],
               model = model, n_plates = n, plates = plates,
               antigen = u$antigen[1], feature = u$feature[1], source = u$source[1],
               wavelength = u$wavelength[1], mixed_models = length(unique(u$model)) > 1L,
               fallback = any(u$is_fallback))
  tabs <- list()
  if (info$mixed_models)
    add("mixed-models", "info", "Plates in this fit unit selected different models.",
        paste(names(table(u$model)), table(u$model), sep = ":", collapse = ", "))

  # -- response scale actually fitted, run parameters, and settings drift ---------------
  sm <- d$std
  if (.fd_has(sm, c("curve_id", "dev_raw"))) {
    sm <- sm[sm$curve_id %in% cids & sm$method == method, , drop = FALSE]
    if (nrow(sm)) {
      sc <- .fd_scale(.fd_n(sm$dev_raw), .fd_n(sm$dev_log10), .fd_n(sm$dev_ln))
      info$scale <- names(sort(table(sc), decreasing = TRUE))[1]
      info$n_std <- sum(.fd_n(sm$n_std)); info$n_std_included <- sum(.fd_n(sm$n_included))
      info$raw_range <- c(min(.fd_n(sm$raw_min), na.rm = TRUE), max(.fd_n(sm$raw_max), na.rm = TRUE))
    }
  }
  rn <- d$run
  if (.fd_has(rn, c("job_id", "params")) && u$job_id[1] %in% as.character(rn$job_id))
    info$job_params <- as.character(rn$params[match(u$job_id[1], as.character(rn$job_id))])
  st <- d$settings
  if (.fd_has(st, c("param", "value"))) {
    st <- st[(is.na(u$antigen[1]) | st$antigen == u$antigen[1]) & (is.na(u$feature[1]) | st$feature == u$feature[1]), , drop = FALSE]
    info$settings <- stats::setNames(st$value, st$param)
    lr <- info$settings[["is_log_response"]]
    if (!is.null(info$scale) && info$scale %in% c("raw", "log10") && !is.null(lr) && !is.na(lr)) {
      want <- if (identical(tolower(lr), "true")) "log10" else "raw"
      if (!identical(info$scale, want))
        add("settings-stale", "warn",
            sprintf("This fit used the %s response scale, but the current setting is_log_response=%s (%s). The stored results do not reflect the current setting.",
                    info$scale, lr, want),
            sprintf("fitted scale=%s; current setting expects=%s", info$scale, want),
            "Recompute so the stored fit matches the intended scale, or restore the setting that produced it.")
    }
  }

  # -- sampler / optimizer -----------------------------------------------------
  dg <- d$diag
  dg <- if (.fd_has(dg, "curve_id")) dg[as.character(dg$curve_id) %in% cids & dg$method == method, , drop = FALSE] else NULL
  if (.fd_has(dg)) {
    key <- paste(dg$fit_seed, round(.fd_n(dg$fit_seconds), 2))
    n_fits <- length(unique(key))
    info$fit_mode <- if (n > 1L && n_fits == 1L) "pooled (one joint fit)"
                     else if (n_fits == n) "per-plate (independent fits)" else "mixed"
    one <- dg[!duplicated(key), , drop = FALSE]
    mx <- function(c) suppressWarnings(max(.fd_n(one[[c]]), na.rm = TRUE))
    mn <- function(c) suppressWarnings(min(.fd_n(one[[c]]), na.rm = TRUE))
    fin <- function(v) if (is.finite(v)) v else NA_real_
    s <- list(n_fits = n_fits, rhat_max = fin(mx("rhat_max")),
              ess_bulk_min = fin(mn("ess_bulk_min")), ess_tail_min = fin(mn("ess_tail_min")),
              n_divergent = fin(sum(.fd_n(one$n_divergent), na.rm = TRUE)),
              pct_divergent = fin(mx("pct_divergent")),
              max_treedepth_hit = fin(sum(.fd_n(one$max_treedepth_hit), na.rm = TRUE)),
              ebfmi_min = fin(mn("ebfmi_min")),
              fit_seconds = fin(mx("fit_seconds")),
              converged = if (any(one$converged %in% FALSE)) FALSE else if (all(is.na(one$converged))) NA else TRUE,
              hessian_condition_number = fin(mx("hessian_condition_number")))
    info$sampler <- s
    if (identical(method, "bayesian")) {
      ev <- function(...) paste(sprintf(...), collapse = "; ")
      if (isFALSE(s$converged)) add("sampler-not-converged", "error", "The fit reports it did not converge.")
      if (!is.na(s$rhat_max) && s$rhat_max > TH$rhat_err)
        add("sampler-rhat", "error", "Chains disagree (R-hat well above 1).", ev("rhat_max=%s", .fd_fmt(s$rhat_max)),
            "Do not trust these estimates; refit with more warmup/sampling and a higher adapt_delta.")
      else if (!is.na(s$rhat_max) && s$rhat_max > TH$rhat_warn)
        add("sampler-rhat", "warn", "R-hat slightly above 1.", ev("rhat_max=%s", .fd_fmt(s$rhat_max)),
            "Refit with more sampling iterations.")
      ess <- suppressWarnings(min(s$ess_bulk_min, s$ess_tail_min, na.rm = TRUE))
      if (is.finite(ess) && ess < TH$ess_err)
        add("sampler-ess", "error", "Effective sample size is very low.", ev("ess_min=%s", .fd_fmt(ess)),
            "Intervals and tail probabilities are unreliable; refit with more iterations.")
      else if (is.finite(ess) && ess < TH$ess_warn)
        add("sampler-ess", "warn", "Effective sample size is low.", ev("ess_min=%s", .fd_fmt(ess)),
            "Refit with more sampling iterations.")
      if (!is.na(s$n_divergent) && s$n_divergent > 0) {
        sev <- if (!is.na(s$pct_divergent) && s$pct_divergent >= TH$div_pct_err) "error" else "warn"
        add("sampler-divergences", sev, "Divergent transitions: the sampler struggled with the posterior geometry.",
            ev("n_divergent=%s; pct_divergent=%s", .fd_fmt(s$n_divergent), .fd_fmt(s$pct_divergent)),
            "Raise adapt_delta (0.95-0.99). If it persists the data do not support one pooled hierarchy (see heterogeneity).")
      }
      if (!is.na(s$max_treedepth_hit) && s$max_treedepth_hit > 0)
        add("sampler-treedepth", "warn", "Sampler hit the maximum tree depth (inefficient exploration).",
            ev("max_treedepth_hit=%s", .fd_fmt(s$max_treedepth_hit)),
            "Usually a symptom of weakly identified or conflicting plates; check heterogeneity and weak plates.")
      if (!is.na(s$ebfmi_min) && s$ebfmi_min < TH$ebfmi_warn)
        add("sampler-ebfmi", "warn", "Low E-BFMI: energy transitions are poorly explored.", ev("ebfmi_min=%s", .fd_fmt(s$ebfmi_min)))
    } else {
      if (isFALSE(s$converged)) add("freq-not-converged", "error", "The optimizer did not converge.")
      if (!is.na(s$hessian_condition_number) && s$hessian_condition_number > TH$hess_cond_warn)
        add("freq-hessian", "warn", "Near-singular covariance matrix (standard errors unreliable).",
            sprintf("hessian_condition_number=%s", .fd_fmt(s$hessian_condition_number)))
    }
  } else {
    info$fit_mode <- NA_character_
    add("no-sampler-diagnostics", "info", "No sampler/optimizer diagnostics are stored for this unit (older run, or table missing).")
  }

  # curveRfreq fits every plate on its own (no shared population, fit_seed/fit_seconds
  # are NA), so the "same seed + runtime = one joint fit" inference only holds for
  # Bayesian. Pooling-specific findings below are gated on pooled_fit.
  if (identical(method, "frequentist")) info$fit_mode <- "per-plate (independent fits)"
  pooled_fit <- !identical(method, "frequentist") && !grepl("^per-plate", .fd_or(info$fit_mode, ""))

  # -- eligibility: a model passes only if EVERY plate passes ---------------------
  ft <- d$fit
  ft <- ft[as.character(ft$curve_id) %in% cids & ft$method == method & ft$model_name == model, , drop = FALSE]
  if (.fd_has(ft, "eligible") && n > 0L) {
    bad <- ft[ft$eligible %in% FALSE, , drop = FALSE]
    info$n_ineligible <- nrow(bad)
    if (nrow(bad)) {
      gt <- d$gate
      fails <- if (.fd_has(gt, c("curve_id", "gate", "passed")))
        gt[as.character(gt$curve_id) %in% as.character(bad$curve_id) & gt$method == method &
           gt$model_name == model & gt$passed %in% FALSE, , drop = FALSE] else NULL
      pf <- if (.fd_has(fails)) sprintf("%s: %s", plates[match(as.character(fails$curve_id), cids)], fails$gate) else character(0)
      info$gate_counts <- if (.fd_has(fails)) { tb <- table(fails$gate); stats::setNames(as.integer(tb), names(tb)) } else NULL
      # Many failing plates: lead with counts per gate, list only the first few.
      gtxt <- if (!length(pf)) "gate detail unavailable" else
        paste0("by gate: ", paste(sprintf("%s=%d", names(info$gate_counts), info$gate_counts), collapse = ", "),
               "; ", if (length(pf) > 12L) paste0(paste(utils::head(pf, 12L), collapse = ", "), ", ... (+", length(pf) - 12L, " more, see table)")
                     else paste(pf, collapse = ", "))
      tabs$gate_failures <- if (.fd_has(fails)) data.frame(
        plate = plates[match(as.character(fails$curve_id), cids)], gate = fails$gate,
        detail = if ("detail" %in% names(fails)) fails$detail else NA_character_, stringsAsFactors = FALSE) else NULL
      add("eligibility", if (nrow(bad) == n) "error" else "warn",
          sprintf("%d of %d plates fail the quantification gates for the selected model%s.", nrow(bad), n,
                  if (pooled_fit) "; a pooled model counts as ineligible if any plate fails" else ""),
          gtxt, "Inspect or mask the failing plate(s)' standards, or fit them separately from the rest.")
      info$ineligible_plates <- plates[match(as.character(bad$curve_id), cids)]
    }
    if (any(u$is_fallback)) add("fallback", "warn",
      "No model passed eligibility on all plates, so the best available model was used as a fallback.")
  }

  # -- per-plate parameters: heterogeneity and weak plates (Bayesian) -----------------
  pm <- d$param
  pm <- if (.fd_has(pm, c("curve_id", "term"))) pm[as.character(pm$curve_id) %in% cids & pm$method == method &
                                                    pm$model_name == model & pm$term %in% c("a", "b", "c", "d", "g"), , drop = FALSE] else NULL
  if (.fd_has(pm)) {
    pm$plate <- plates[match(as.character(pm$curve_id), cids)]
    terms <- intersect(c("a", "b", "c", "d", "g"), unique(pm$term))
    wide <- data.frame(plate = plates, stringsAsFactors = FALSE)
    for (t in terms) wide[[t]] <- .fd_n(pm$estimate[match(paste(plates, t), paste(pm$plate, pm$term))])
    pm$rel_se <- abs(.fd_n(pm$std_error) / .fd_n(pm$estimate))
    wide$max_rel_se <- vapply(plates, function(p) suppressWarnings(max(pm$rel_se[pm$plate == p & is.finite(pm$rel_se)], -Inf)), numeric(1))
    wide$max_rel_se[!is.finite(wide$max_rel_se)] <- NA_real_
    for (t in intersect(c("a", "c", "d"), terms)) wide[[paste0("z_", t)]] <- .fd_rz(wide[[t]])
    tabs$plate_params <- wide
    if (pooled_fit && n >= TH$outlier_min_plates) {
      zc <- intersect(paste0("z_", c("a", "c", "d")), names(wide))
      out <- which(apply(wide[zc], 1, function(r) any(abs(r) > TH$outlier_z, na.rm = TRUE)))
      if (length(out)) {
        info$outlier_plates <- wide$plate[out]
        add("heterogeneity-outlier", "warn",
            sprintf("%d plate(s) differ sharply from the rest on a curve parameter (robust z > %.1f).", length(out), TH$outlier_z),
            paste(vapply(out, function(i) sprintf("%s: %s", wide$plate[i],
                  paste(sprintf("%s z=%s", sub("^z_", "", zc), .fd_fmt(unlist(wide[i, zc]), 3)), collapse = ", ")),
                  character(1)), collapse = " | "),
            "A pooled hierarchy shrinks every plate toward a shared mean; exclude or separately fit the outlier plate(s).")
      }
    }
    if (pooled_fit && n >= 2L && "d" %in% terms) {
      dd <- wide$d[is.finite(wide$d)]
      sp <- if (length(dd) >= 2L && stats::median(abs(dd)) > 0) (max(dd) - min(dd)) / stats::median(abs(dd)) else NA_real_
      info$d_spread <- sp
      if (!is.na(sp) && sp > TH$d_spread_warn)
        add("heterogeneity-signal-level", "warn",
            "Upper asymptote differs a lot between plates (different signal level / run).",
            sprintf("(max-min)/median of d = %s across %d plates", .fd_fmt(sp, 3), length(dd)),
            "Plates from different days/lots are not exchangeable; fit similar plates together or per plate.")
    }
    if (pooled_fit && n >= 2L && "c" %in% terms) {
      cc <- wide$c[is.finite(wide$c)]
      if (length(cc) >= 2L && diff(range(cc)) > TH$c_spread_warn)
        add("heterogeneity-inflection", "warn", "Inflection point shifts a lot between plates.",
            sprintf("range of c = %s (fit-scale units)", .fd_fmt(diff(range(cc)), 3)))
    }
    weak <- which(is.finite(wide$max_rel_se) & wide$max_rel_se > TH$rel_se_warn)
    if (length(weak)) {
      info$weak_plates <- wide$plate[weak]
      add("weak-plate", "warn", "Some plates have poorly determined parameters (SE comparable to the estimate).",
          paste(sprintf("%s: max SE/estimate=%s", wide$plate[weak], .fd_fmt(wide$max_rel_se[weak], 3)), collapse = "; "),
          "Check that plate's standards (range, masked wells, saturation).")
    }
  }

  # -- population parameters + noise (Bayesian) -----------------------------------
  hy <- d$hyper
  hy <- if (.fd_has(hy, c("curve_id", "term"))) hy[as.character(hy$curve_id) %in% cids & hy$method == method, , drop = FALSE] else NULL
  if (.fd_has(hy)) {
    hy <- hy[as.character(hy$curve_id) == hy$curve_id[1], , drop = FALSE]
    tabs$population <- data.frame(term = hy$term, estimate = .fd_n(hy$estimate), sd = .fd_n(hy$std_error),
                                  q_lo = .fd_n(hy$q_lo), q_hi = .fd_n(hy$q_hi), stringsAsFactors = FALSE)
    g <- function(t) .fd_n(hy$estimate[hy$term == t])[1]
    if (all(is.finite(c(g("sigma_d"), g("mu_d")))) && g("mu_d") != 0 && g("sigma_d") / abs(g("mu_d")) > TH$hyper_cv_warn)
      add("population-wide", "warn", "Between-plate spread of the upper asymptote is large relative to its mean.",
          sprintf("sigma_d/mu_d = %s", .fd_fmt(g("sigma_d") / abs(g("mu_d")), 3)),
          "The pooled population is describing plates that differ more than a single hierarchy should.")
    nu <- g("nu")
    if (is.finite(nu) && nu < TH$nu_warn)
      add("noise-heavy-tails", "warn",
          "Student-t degrees of freedom sit at their lower bound (2): the noise model cannot describe the scatter, so the fit is absorbing misfit as 'outliers'.",
          sprintf("nu=%s (bounded below at 2)", .fd_fmt(nu, 3)),
          if (identical(info$scale, "raw"))
            "The response was fitted on the raw scale, where noise grows with the signal (typical for OD). Fit on the log scale (is_log_response = true) or use heteroscedastic noise."
          else "Check masked/saturated or mislabeled standard wells, and whether noise grows with the signal (heteroscedastic noise).")
    else if (is.finite(nu) && nu < TH$nu_info)
      add("noise-heavy-tails", "info", "Student-t degrees of freedom are low: some outlying wells.",
          sprintf("nu=%s", .fd_fmt(nu, 3)))
  }

  # -- LOO ----------------------------------------------------------------------------
  lo <- d$loo
  lo <- if (.fd_has(lo, c("curve_id", "pareto_k_bad"))) lo[as.character(lo$curve_id) %in% cids & lo$model_name == model, , drop = FALSE] else NULL
  if (.fd_has(lo)) {
    kb <- sum(.fd_n(lo$pareto_k_bad), na.rm = TRUE) + sum(.fd_n(lo$pareto_k_vbad), na.rm = TRUE)
    info$pareto_k_bad <- kb
    if (kb > 0) add("loo-pareto-k", "info", "Some observations have high Pareto-k (LOO model comparison is unreliable for them).",
                    sprintf("bad+very_bad=%s", .fd_fmt(kb)))
  }

  # -- size: only noteworthy when something else is already wrong ---------------------
  if (identical(method, "bayesian") && n >= TH$big_group && length(F) &&
      any(vapply(F, function(r) r$severity %in% c("warn", "error"), logical(1))))
    add("pooled-size", "info", sprintf("%d plates are pooled in one fit: one discordant plate affects all of them.", n))

  info$findings <- if (length(F)) do.call(rbind, F) else NULL
  info$tables <- tabs
  info
}

# ---- strategy ---------------------------------------------------------------
# Turns a unit's findings into ordered actions. The causes are not exclusive; the
# order puts the cheapest, most targeted remedy first.
.fd_strategy <- function(info) {
  f <- info$findings; ids <- if (is.null(f)) character(0) else f$finding
  sev <- if (is.null(f)) character(0) else f$severity
  has <- function(p) any(grepl(p, ids))
  A <- list(); act <- function(pri, what, why) A[[length(A) + 1L]] <<- data.frame(
    unit = info$unit, priority = pri, action = what, why = why, stringsAsFactors = FALSE)
  pooled <- grepl("^pooled", .fd_or(info$fit_mode, ""))
  het <- has("^heterogeneity|^population-wide|^weak-plate|^eligibility")
  if (!any(sev %in% c("warn", "error"))) return(NULL)

  # Scale first: if the model is fitted on the wrong scale, nothing downstream
  # (heterogeneity, sampler knobs, plate count) is interpretable.
  if (any(ids == "noise-heavy-tails" & sev == "warn") && identical(info$scale, "raw"))
    act(0, "Fit on the log scale: set is_log_response = true for this experiment, then recompute the Bayesian fit and rebuild this report.",
        "The Student-t df is pinned at its lower bound on the raw response scale, the signature of noise that grows with the signal. Plate count is not the cause.")
  if (has("^settings-stale"))
    act(0, "Recompute so the stored fit matches the intended response scale (or restore the previous setting).",
        "Stored results and current settings disagree, so comparisons with other runs are not like-for-like.")

  many <- length(info$ineligible_plates) > max(5L, 0.25 * info$n_plates)
  if (many && !is.null(info$gate_counts)) {
    # A systematic failure, not a few bad plates: say what the gate means.
    gc <- info$gate_counts
    if ("at_bound" %in% names(gc))
      act(1, sprintf("%d gate failure(s) are 'at_bound' (usually the upper asymptote d at its allowed maximum): the standard series does not reach an upper plateau on those plates. Check whether the top standards are masked or the top concentration is too low to saturate; the curve above the last standard is extrapolation.", gc[["at_bound"]]),
          "A parameter pinned at its bound is not identified, so its uncertainty and the quantification limits are unreliable. This is a property of the standards, not of pooling.")
    if ("dynamic_range" %in% names(gc))
      act(1, sprintf("%d plate(s) fail 'dynamic_range': no concentration range meets the precision (pcov) threshold. Inspect those plates' standards (flat, noisy or mostly masked) before trusting their sample estimates.", gc[["dynamic_range"]]),
          "Zero dynamic range means the fitted curve cannot quantify any sample on that plate.")
  } else if (!is.null(info$outlier_plates) || !is.null(info$ineligible_plates)) {
    bad <- unique(c(info$outlier_plates, info$ineligible_plates))
    act(1, sprintf("Review plate(s) %s: unmask/inspect their standards, or exclude them and refit.", paste(bad, collapse = ", ")),
        "These plates are discordant with the rest or fail the gates; in a pooled fit they affect all plates.")
  }
  if (pooled && info$n_plates >= 2L && het)
    act(2, "Fit similar plates together (same run/day/lot) in separate batches, or use independent per-plate Bayesian fits (bayes_single_plate).",
        "The hierarchy assumes plates are exchangeable; the per-plate parameters show they are not. Per-plate fits cannot be dragged by another plate.")
  if (has("^sampler-(divergences|treedepth|ebfmi)"))
    act(3, "Raise adapt_delta to 0.95-0.99 and rerun; keep warmup >= 1000.",
        "Divergences/treedepth hits indicate hard posterior geometry; this is cheap to try, but does not fix real plate heterogeneity.")
  if (has("^sampler-(rhat|ess)|not-converged"))
    act(4, "Rerun with more warmup/sampling iterations (e.g. 2000/2000) and check R-hat < 1.01.",
        "Chains have not mixed or the effective sample size is too small for reliable intervals.")
  if (pooled && info$n_plates >= FD_THRESH$big_group && !het && any(sev %in% c("warn", "error")))
    act(5, "Try fitting the plates in two smaller batches and compare with the full run.",
        "Pooled size increases sampler difficulty even when plates look comparable.")
  if (!length(A)) act(9, "Review the findings above.", "No automatic remedy applies.")
  do.call(rbind, A)
}

# ---- build ------------------------------------------------------------------
fit_diag_build <- function(d, scope = list()) {
  d <- lapply(d, function(x) {
    if (is.data.frame(x) && "curve_id" %in% names(x)) x$curve_id <- .fd_id(x$curve_id)
    x
  })
  units <- .fd_units(d)
  out <- list(scope = scope, generated = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
              thresholds = FD_THRESH, units = list(), findings = NULL, strategy = NULL,
              available = vapply(d, .fd_has, logical(1)), run = d$run)
  if (is.null(units)) return(out)
  for (k in unique(units$unit)) {
    info <- .fd_unit(units[units$unit == k, , drop = FALSE], d, FD_THRESH)
    info$strategy <- .fd_strategy(info)
    out$units[[k]] <- info
  }
  # Experiment-level: units fitted on different response scales are not comparable.
  sc <- vapply(out$units, function(i) .fd_or(i$scale, NA_character_), character(1))
  sc <- sc[!is.na(sc)]
  extra <- NULL
  if (length(unique(sc)) > 1L) {
    extra <- data.frame(unit = "(experiment)", finding = "scale-mixed", severity = "warn",
      message = "Fits in this experiment were computed on different response scales, so their parameters and intervals are not comparable.",
      evidence = paste(sprintf("%s=%s", names(sc), sc), collapse = "; "),
      action = "Recompute everything under one is_log_response setting.", stringsAsFactors = FALSE)
    out$strategy <- data.frame(unit = "(experiment)", priority = 0,
      action = "Recompute all fits under one is_log_response setting.",
      why = "Mixed response scales within one experiment.", stringsAsFactors = FALSE)
  }
  out$findings <- rbind(extra, do.call(rbind, lapply(out$units, `[[`, "findings")))
  out$strategy <- rbind(out$strategy, do.call(rbind, lapply(out$units, `[[`, "strategy")))
  out
}

.fd_sev_rank <- function(s) match(s, c("info", "warn", "error"), nomatch = 0L)

fit_diag_severity <- function(rep) {
  f <- rep$findings
  if (is.null(f) || !nrow(f)) return("ok")
  c("ok", "info", "warn", "error")[max(.fd_sev_rank(f$severity)) + 1L]
}

# Short, user-facing. NULL when nothing at warn/error level was found. With
# `job_id`, only that job's fit units count (a finished job should not be blamed
# for an older job's problems); experiment-level findings are then left out too.
fit_diag_notice <- function(rep, job_id = NULL) {
  if (!is.null(job_id)) {
    rep$units <- Filter(function(i) identical(as.character(i$job_id), as.character(job_id)), rep$units)
    rep$findings <- do.call(rbind, lapply(rep$units, `[[`, "findings"))
  }
  sev <- fit_diag_severity(rep)
  if (!sev %in% c("warn", "error")) return(NULL)
  lines <- unlist(lapply(rep$units, function(i) {
    f <- i$findings; if (is.null(f) || !any(f$severity %in% c("warn", "error"))) return(NULL)
    top <- f[f$severity %in% c("warn", "error"), , drop = FALSE]
    lab <- sprintf("%s plate(s), %s%s", i$n_plates, i$method,
                   if (!is.na(.fd_or(i$antigen, NA))) sprintf(", %s", i$antigen) else "")
    first <- if (!is.null(i$strategy)) i$strategy$action[1] else ""
    sprintf("%s: %s %s", lab, paste(unique(top$message), collapse = " "), first)
  }))
  list(severity = sev, text = lines)
}

# ---- text / json ------------------------------------------------------------
fit_diag_report_text <- function(rep) {
  sc <- rep$scope
  L <- c(
    "# I-SPI compute-fit diagnostics report", "",
    sprintf("- Project: %s | Study: %s | Experiment: %s", .fd_or(sc$project_id, "?"), .fd_or(sc$study, "?"), .fd_or(sc$experiment, "?")),
    sprintf("- Generated: %s", rep$generated),
    sprintf("- Overall severity: **%s**", fit_diag_severity(rep)), "",
    "## How to read this (for the reviewing assistant)", "",
    "A *fit unit* is one (multiplate group, job): the plates that were fitted together in one job. A pooled Bayesian fit is ONE joint hierarchical model over all plates in the unit: priors, the noise terms (sigma_obs, nu) and the population means/spreads are shared, and a model is called eligible only if EVERY plate passes the quantification gates (relative SE, dynamic range). Results are stored per curve and each new job overwrites the previous fit of the same curves, so earlier runs of a different plate subset are not recoverable from this data.",
    "Thresholds used by the automated assessment: R-hat > 1.01 warn / > 1.05 error; min ESS < 400 warn / < 100 error; any divergence warns, >= 1% of draws is an error; E-BFMI < 0.3 warns; |robust z| > 3.5 flags an outlier plate (needs >= 5 plates); (max-min)/median of d > 0.5 flags signal-level heterogeneity; SE/|estimate| > 0.5 flags a weak plate.",
    "The response scale each fit used (raw / log10 / ln) is MEASURED from calib_standards, not assumed. Settings shown are those in effect now; the run record does not store them, so a mismatch with the fitted scale means the setting changed after the fit ran.",
    "Not available here: the data-adaptive priors, per-plate sampler statistics when the fit was pooled (one set for the unit), and the raw standard-curve wells. Do not infer values that are not shown; say what extra data you would need.", "")

  if (!length(rep$units)) return(paste(c(L, "## Result", "", "No fitted curves were found for this experiment.", "",
    "Tables available: ", paste(names(rep$available)[rep$available], collapse = ", ")), collapse = "\n"))

  L <- c(L, "## Automated assessment", "")
  if (is.null(rep$findings) || !nrow(rep$findings)) L <- c(L, "No findings: all checks passed.", "")
  else {
    ord <- order(-.fd_sev_rank(rep$findings$severity))
    L <- c(L, .fd_md_table(rep$findings[ord, c("severity", "unit", "finding", "message", "evidence"), drop = FALSE]), "")
  }
  L <- c(L, "## Recommended strategy (ordered)", "")
  L <- c(L, if (is.null(rep$strategy)) "No action needed." else .fd_md_table(rep$strategy[order(rep$strategy$unit, rep$strategy$priority), ]), "")

  L <- c(L, "## Fit units", "")
  su <- do.call(rbind, lapply(rep$units, function(i) data.frame(
    unit = i$unit, method = i$method, plates = i$n_plates, model = i$model,
    fit_mode = .fd_or(i$fit_mode, NA_character_), response_scale = .fd_or(i$scale, NA_character_),
    ineligible = .fd_or(i$n_ineligible, NA_integer_), fallback = i$fallback,
    antigen = .fd_or(i$antigen, NA_character_), feature = .fd_or(i$feature, NA_character_),
    wavelength = .fd_or(i$wavelength, NA_character_), stringsAsFactors = FALSE)))
  L <- c(L, .fd_md_table(su), "")

  L <- c(L, "## Sampler / optimizer diagnostics (per unit)", "")
  sd_ <- do.call(rbind, lapply(rep$units, function(i) if (is.null(i$sampler)) NULL else
    data.frame(unit = i$unit, i$sampler, stringsAsFactors = FALSE)))
  L <- c(L, .fd_md_table(sd_), "")

  for (i in rep$units) {
    L <- c(L, sprintf("## Unit `%s` (%d plates: %s)", i$unit, i$n_plates, paste(i$plates, collapse = ", ")), "")
    if (!is.null(i$job_params)) L <- c(L, sprintf("Job parameters recorded for this run: `%s`", i$job_params))
    if (!is.null(i$raw_range)) L <- c(L, sprintf("Raw standard responses span %s to %s (%s standard wells, %s included); fitted on the **%s** scale.",
                                                 .fd_fmt(i$raw_range[1]), .fd_fmt(i$raw_range[2]),
                                                 .fd_fmt(i$n_std), .fd_fmt(i$n_std_included), .fd_or(i$scale, "unknown")))
    if (!is.null(i$settings)) L <- c(L, sprintf("Settings in effect NOW for this antigen/feature (the run record does not store them): %s",
                                                paste(sprintf("%s=%s", names(i$settings), i$settings), collapse = ", ")))
    L <- c(L, "")
    if (!is.null(i$tables$population)) L <- c(L, "Population / noise parameters (selected model):", .fd_md_table(i$tables$population), "")
    if (!is.null(i$tables$plate_params)) L <- c(L, "Per-plate curve parameters (z_* = robust z across plates):", .fd_md_table(i$tables$plate_params), "")
    if (!is.null(i$tables$gate_failures)) L <- c(L, "Gate failures:", .fd_md_table(i$tables$gate_failures), "")
  }
  paste(L, collapse = "\n")
}

fit_diag_report_json <- function(rep) {
  slim <- list(scope = rep$scope, generated = rep$generated, severity = fit_diag_severity(rep),
               thresholds = rep$thresholds, findings = rep$findings, strategy = rep$strategy,
               units = lapply(rep$units, function(i) i[setdiff(names(i), c("findings", "strategy"))]))
  jsonlite::toJSON(slim, auto_unbox = TRUE, na = "null", digits = 6, pretty = TRUE, dataframe = "rows")
}
