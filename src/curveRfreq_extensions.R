## Quality Control extensions to the curveRfreq package
compute_dil_series_se <- function(
    standards_data,
    response_col  = "mfi",
    dilution_col  = "dilution",
    plate_col     = "plate_nom",
    grouping_cols = c("project_id",
                      "study_accession",
                      "experiment_accession",
                      "source_nom",
                      "antigen",
                      "feature"),
    min_reps = 2,
    verbose  = FALSE) {
  
  # ------------------------------------------------------------------
  # 1. Input validation
  # ------------------------------------------------------------------
  # Only require grouping_cols that are actually present — project_id
  # may not exist in every dataset, so we warn rather than stop.
  present_grouping_cols <- intersect(grouping_cols, colnames(standards_data))
  missing_grouping_cols <- setdiff(grouping_cols, colnames(standards_data))
  
  if (length(missing_grouping_cols) > 0) {
    warning(sprintf(
      "[compute_dil_series_se] The following grouping_cols are absent and will be ignored: %s",
      paste(missing_grouping_cols, collapse = ", ")
    ))
  }
  
  if (length(present_grouping_cols) == 0) {
    stop("[compute_dil_series_se] No grouping_cols found in standards_data.")
  }
  
  required_cols <- unique(c(present_grouping_cols, response_col, dilution_col, plate_col))
  missing_cols  <- setdiff(required_cols, colnames(standards_data))
  if (length(missing_cols) > 0) {
    stop("[compute_dil_series_se] Missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }
  
  if (!is.numeric(standards_data[[response_col]])) {
    stop("[compute_dil_series_se] response_col '", response_col, "' must be numeric.")
  }
  
  # Use only the grouping cols that are present from here on
  grouping_cols <- present_grouping_cols
  
  if (verbose) {
    message(sprintf(
      "[compute_dil_series_se] Using grouping cols: %s",
      paste(grouping_cols, collapse = ", ")
    ))
  }
  
  # ------------------------------------------------------------------
  # 2. Build a per-row group key so we can merge stats back onto
  #    every original row at the end.
  # ------------------------------------------------------------------
  n_rows <- nrow(standards_data)
  
  # ------------------------------------------------------------------
  # 3. Identify unique grouping × dilution combinations
  # ------------------------------------------------------------------
  key_cols       <- c(grouping_cols, dilution_col)
  unique_keys    <- unique(standards_data[, key_cols, drop = FALSE])
  unique_keys    <- unique_keys[do.call(order, unique_keys), , drop = FALSE]
  rownames(unique_keys) <- NULL
  
  if (verbose) {
    message(sprintf(
      "[compute_dil_series_se] %d unique grouping × dilution combinations found.",
      nrow(unique_keys)
    ))
  }
  
  # ------------------------------------------------------------------
  # 4. For every unique grouping × dilution, compute mean, median, SE
  #    across all plates.
  # ------------------------------------------------------------------
  stats_list <- lapply(seq_len(nrow(unique_keys)), function(i) {
    
    key <- unique_keys[i, , drop = FALSE]
    
    # Build row mask for this grouping × dilution
    mask <- rep(TRUE, n_rows)
    for (col in key_cols) {
      val  <- key[[col]]
      mask <- mask &
        !is.na(standards_data[[col]]) &
        standards_data[[col]] == val
    }
    
    vals <- standards_data[[response_col]][mask]
    vals <- vals[!is.na(vals)]           # drop NA responses
    n    <- length(vals)
    
    mean_resp   <- if (n >= 1L) mean(vals)           else NA_real_
    median_resp <- if (n >= 1L) median(vals)         else NA_real_
    se_resp     <- if (n >= min_reps) sd(vals) / sqrt(n) else NA_real_
    
    cv_resp <- if (!is.na(se_resp)    && is.finite(se_resp) &&
                   !is.na(mean_resp)  && is.finite(mean_resp) &&
                   abs(mean_resp) > .Machine$double.eps) {
      (se_resp / abs(mean_resp)) * 100
    } else {
      NA_real_
    }
    
    
    n_plates    <- if (plate_col %in% colnames(standards_data)) {
      length(unique(standards_data[[plate_col]][mask &
                                                  !is.na(standards_data[[plate_col]])]))
    } else {
      NA_integer_
    }
    
    data.frame(
      key,
      dil_mean_response   = mean_resp,
      dil_median_response = median_resp,
      dil_se_response     = se_resp,
      dil_cv_response     = cv_resp,
      dil_n_obs           = n,
      dil_n_plates        = n_plates,
      stringsAsFactors    = FALSE,
      row.names           = NULL
    )
  })
  
  stats_df <- do.call(rbind, stats_list)
  rownames(stats_df) <- NULL
  
  if (verbose) {
    n_valid_se <- sum(!is.na(stats_df$dil_se_response))
    message(sprintf(
      "[compute_dil_series_se] SE computable for %d / %d grouping × dilution combinations.",
      n_valid_se, nrow(stats_df)
    ))
  }
  
  # ------------------------------------------------------------------
  # 5. Left-join the per-(grouping × dilution) stats back onto every
  #    original row of standards_data so the output has the same number
  #    of rows as the input.
  # ------------------------------------------------------------------
  # Use base R merge with all.x = TRUE to preserve row count and order.
  # We add a temporary index to guarantee the original row order is
  # restored after the merge.
  standards_data$.tmp_row_order <- seq_len(n_rows)
  
  out <- merge(
    standards_data,
    stats_df,
    by     = key_cols,
    all.x  = TRUE,
    sort   = FALSE
  )
  
  # Restore original row order
  out <- out[order(out$.tmp_row_order), , drop = FALSE]
  out$.tmp_row_order <- NULL
  rownames(out) <- NULL
  
  if (verbose) {
    message(sprintf(
      "[compute_dil_series_se] Output has %d rows (input had %d rows).",
      nrow(out), n_rows
    ))
    n_se_na <- sum(is.na(out$dil_se_response))
    message(sprintf(
      "[compute_dil_series_se] %d / %d rows have NA dil_se_response (< %d replicates at that dilution level).",
      n_se_na, nrow(out), min_reps
    ))
  }
  
  return(out)
}

#' compute_dil_series_accuracy
#'
#' For each dilution level in a pooled dilution-series data frame (the output
#' of compute_dil_series_se()), back-calculates the concentration that
#' corresponds to the POOLED MEAN response at that level using the fitted
#' sigmoid model, then expresses it as a percent recovery vs. the nominal
#' concentration.  Per-row (per-plate) accuracy is then computed by the same
#' logic applied to each individual plate's observed response.
#'
#' FDA 2018 acceptance criteria applied:
#'   CV%      <= cv_threshold      (default 20%) at standard levels
#'   CV%      <= lloq_cv_threshold (default 25%) at the LLOQ level
#'   Accuracy  in [accuracy_lo, accuracy_hi]     (default 80-120%)
#'
#' @param best_fit            List returned by select_model_fit_AIC() /
#'                            fit_qc_glance().  Must contain:
#'                              $best_fit       – nlsLM object
#'                              $best_model_name – character model name
#'                              $best_data      – data used to fit the model
#' @param dil_series_df       data.frame – output of compute_dil_series_se().
#'                            Must contain dil_mean_response and the
#'                            dilution_col column.
#' @param response_col        Character. Name of the raw response column.
#' @param independent_variable Character. Name of the concentration column
#'                            in best_data (e.g. "concentration").
#' @param dilution_col        Character. Dilution column name (default "dilution").
#' @param fixed_a_result      Numeric scalar or NULL. Fixed lower asymptote
#'                            on the SAME scale as the fitted model
#'                            (i.e. log10-transformed if is_log_response=TRUE).
#' @param is_log_response     Logical. Was the response log10-transformed
#'                            before fitting? (default TRUE)
#' @param is_log_concentration Logical. Is the independent variable on the
#'                            log10 concentration scale? (default TRUE)
#' @param undiluted_sc_concentration Numeric. Concentration of the undiluted
#'                            standard (e.g. 10000). Used to convert dilution
#'                            to nominal concentration for the recovery ratio.
#' @param cv_threshold        Numeric. CV% acceptance limit (default 20).
#' @param lloq_cv_threshold   Numeric. CV% acceptance limit at LLOQ (default 25).
#' @param accuracy_lo         Numeric. Lower accuracy bound % (default 80).
#' @param accuracy_hi         Numeric. Upper accuracy bound % (default 120).
#' @param verbose             Logical (default TRUE).
#'
#' @return dil_series_df with additional columns:
#'   dil_nominal_concentration  – nominal concentration at this dilution level
#'   dil_backcalc_mean_conc     – back-calculated concentration from pooled mean response
#'   dil_accuracy_pct           – (back-calc / nominal) * 100  [pooled-mean level]
#'   dil_accuracy_pct_row       – per-row (per-plate) accuracy from observed response
#'   dil_passes_cv              – logical: CV% <= threshold
#'   dil_passes_accuracy        – logical: accuracy in [accuracy_lo, accuracy_hi]
#'   dil_passes_fda             – logical: passes BOTH cv and accuracy
#'   dil_fda_flag               – character label ("PASS","FAIL_CV","FAIL_ACC","FAIL_BOTH","NA")

compute_dil_series_accuracy <- function(
    best_fit,
    dil_series_df,
    response_col           = "mfi",
    independent_variable   = "concentration",
    dilution_col           = "dilution",
    fixed_a_result         = NULL,
    is_log_response        = TRUE,
    is_log_concentration   = TRUE,
    undiluted_sc_concentration = 10000,
    cv_threshold           = 20,
    lloq_cv_threshold      = 25,
    accuracy_lo            = 80,
    accuracy_hi            = 120,
    verbose                = TRUE) {
  
  # ── 0. Extract model components ──────────────────────────────────────
  fit        <- best_fit$best_fit
  model_name <- best_fit$best_model_name
  
  if (is.null(fit) || !inherits(fit, "nls")) {
    warning("[compute_dil_series_accuracy] best_fit$best_fit is NULL or not an nls object.")
    return(dil_series_df)
  }
  
  if (!"dil_mean_response" %in% names(dil_series_df)) {
    stop("[compute_dil_series_accuracy] dil_series_df must contain 'dil_mean_response'. ",
         "Run compute_dil_series_se() first.")
  }
  
  if (!dilution_col %in% names(dil_series_df)) {
    stop("[compute_dil_series_accuracy] dilution_col '", dilution_col,
         "' not found in dil_series_df.")
  }
  
  if (!response_col %in% names(dil_series_df)) {
    stop("[compute_dil_series_accuracy] response_col '", response_col,
         "' not found in dil_series_df.")
  }
  
  # ── 1. Helper: transform a raw response to model scale ───────────────
  # The model was fitted on (possibly) log10-transformed response.
  # Inverse prediction requires y on the same scale as the model.
  to_model_scale <- function(y_raw) {
    if (is_log_response) {
      y_raw[y_raw <= 0] <- NA_real_   # log10 of non-positive is undefined
      log10(y_raw)
    } else {
      y_raw
    }
  }
  
  # ── 2. Helper: safe inverse prediction ───────────────────────────────
  # Returns back-calculated x (on the model's concentration scale) or NA.
  safe_backcalc <- function(y_model_scale) {
    if (is.na(y_model_scale) || !is.finite(y_model_scale)) return(NA_real_)
    tryCatch({
      if (!is.null(fixed_a_result)) {
        switch(model_name,
               Y5     = inv_Y5_fixed(y_model_scale,
                                     fixed_a = fixed_a_result,
                                     b = coef(fit)["b"], c = coef(fit)["c"],
                                     d = coef(fit)["d"], g = coef(fit)["g"]),
               Yd5    = inv_Yd5_fixed(y_model_scale,
                                      fixed_a = fixed_a_result,
                                      b = coef(fit)["b"], c = coef(fit)["c"],
                                      d = coef(fit)["d"], g = coef(fit)["g"]),
               Y4     = inv_Y4_fixed(y_model_scale,
                                     fixed_a = fixed_a_result,
                                     b = coef(fit)["b"], c = coef(fit)["c"],
                                     d = coef(fit)["d"]),
               Yd4    = inv_Yd4_fixed(y_model_scale,
                                      fixed_a = fixed_a_result,
                                      b = coef(fit)["b"], c = coef(fit)["c"],
                                      d = coef(fit)["d"]),
               Ygomp4 = inv_Ygomp4_fixed(y_model_scale,
                                         fixed_a = fixed_a_result,
                                         b = coef(fit)["b"], c = coef(fit)["c"],
                                         d = coef(fit)["d"]),
               NA_real_
        )
      } else {
        switch(model_name,
               Y5     = inv_Y5(y_model_scale,
                               a = coef(fit)["a"], b = coef(fit)["b"],
                               c = coef(fit)["c"], d = coef(fit)["d"],
                               g = coef(fit)["g"]),
               Yd5    = inv_Yd5(y_model_scale,
                                a = coef(fit)["a"], b = coef(fit)["b"],
                                c = coef(fit)["c"], d = coef(fit)["d"],
                                g = coef(fit)["g"]),
               Y4     = inv_Y4(y_model_scale,
                               a = coef(fit)["a"], b = coef(fit)["b"],
                               c = coef(fit)["c"], d = coef(fit)["d"]),
               Yd4    = inv_Yd4(y_model_scale,
                                a = coef(fit)["a"], b = coef(fit)["b"],
                                c = coef(fit)["c"], d = coef(fit)["d"]),
               Ygomp4 = inv_Ygomp4(y_model_scale,
                                   a = coef(fit)["a"], b = coef(fit)["b"],
                                   c = coef(fit)["c"], d = coef(fit)["d"]),
               NA_real_
        )
      }
    }, error = function(e) NA_real_)
  }
  
  # ── 3. Helper: convert model x back to linear concentration ──────────
  to_linear_conc <- function(x_model) {
    if (is_log_concentration) 10^x_model else x_model
  }
  
  # ── 4. Nominal concentration for each row from dilution ──────────────
  # nominal_conc (linear) = (1 / dilution) * undiluted_sc_concentration
  dil_series_df$dil_nominal_concentration <- tryCatch({
    nom <- (1 / dil_series_df[[dilution_col]]) * undiluted_sc_concentration
    ifelse(is.finite(nom) & nom > 0, nom, NA_real_)
  }, error = function(e) rep(NA_real_, nrow(dil_series_df)))
  
  # ── 5. Back-calculate from POOLED MEAN response ───────────────────────
  # One unique back-calc per grouping × dilution level
  # (dil_mean_response is constant within each group × dilution after the join)
  pooled_mean_raw <- dil_series_df$dil_mean_response
  
  dil_series_df$dil_backcalc_mean_conc <- vapply(
    to_model_scale(pooled_mean_raw),
    function(y_ms) {
      x_bc <- safe_backcalc(y_ms)
      as.numeric(to_linear_conc(x_bc))
    },
    numeric(1L)
  )
  
  if (verbose) {
    n_bc_ok <- sum(is.finite(dil_series_df$dil_backcalc_mean_conc))
    message(sprintf(
      "[compute_dil_series_accuracy] Pooled-mean back-calc succeeded for %d / %d rows.",
      n_bc_ok, nrow(dil_series_df)
    ))
  }
  
  # ── 6. Pooled-mean accuracy: (back-calc / nominal) * 100 ─────────────
  dil_series_df$dil_accuracy_pct <- {
    bc  <- dil_series_df$dil_backcalc_mean_conc
    nom <- dil_series_df$dil_nominal_concentration
    ok  <- is.finite(bc) & is.finite(nom) & nom > .Machine$double.eps
    ifelse(ok, (bc / nom) * 100, NA_real_)
  }
  
  # ── 7. Per-row (per-plate) accuracy from individual observed response ─
  obs_raw <- dil_series_df[[response_col]]
  
  dil_series_df$dil_accuracy_pct_row <- vapply(
    seq_len(nrow(dil_series_df)),
    function(i) {
      y_ms  <- to_model_scale(obs_raw[i])
      x_bc  <- safe_backcalc(y_ms)
      x_lin <- to_linear_conc(x_bc)
      nom   <- dil_series_df$dil_nominal_concentration[i]
      if (is.finite(x_lin) && is.finite(nom) && nom > .Machine$double.eps) {
        (x_lin / nom) * 100
      } else {
        NA_real_
      }
    },
    numeric(1L)
  )
  
  if (verbose) {
    n_row_ok <- sum(is.finite(dil_series_df$dil_accuracy_pct_row))
    message(sprintf(
      "[compute_dil_series_accuracy] Per-row back-calc succeeded for %d / %d rows.",
      n_row_ok, nrow(dil_series_df)
    ))
  }
  
  # ── 8. FDA pass/fail flags ────────────────────────────────────────────
  # Use the LLOQ CV threshold for the lowest dilution level (highest conc),
  # and standard CV threshold for all others.
  min_dilution <- min(dil_series_df[[dilution_col]], na.rm = TRUE)
  is_lloq_row  <- dil_series_df[[dilution_col]] == min_dilution
  
  cv_limit <- ifelse(is_lloq_row, lloq_cv_threshold, cv_threshold)
  
  dil_series_df$dil_passes_cv <- {
    cv  <- dil_series_df$dil_cv_response
    ok  <- !is.na(cv) & is.finite(cv)
    ifelse(ok, cv <= cv_limit, NA)
  }
  
  dil_series_df$dil_passes_accuracy <- {
    acc <- dil_series_df$dil_accuracy_pct
    ok  <- !is.na(acc) & is.finite(acc)
    ifelse(ok, acc >= accuracy_lo & acc <= accuracy_hi, NA)
  }
  
  dil_series_df$dil_passes_fda <- {
    passes_cv  <- dil_series_df$dil_passes_cv
    passes_acc <- dil_series_df$dil_passes_accuracy
    both_known <- !is.na(passes_cv) & !is.na(passes_acc)
    ifelse(both_known, passes_cv & passes_acc, NA)
  }
  
  # ── 9. Human-readable flag ────────────────────────────────────────────
  dil_series_df$dil_fda_flag <- {
    p_cv  <- dil_series_df$dil_passes_cv
    p_acc <- dil_series_df$dil_passes_accuracy
    dplyr::case_when(
      is.na(p_cv) | is.na(p_acc)   ~ "NA",
      p_cv  & p_acc                 ~ "PASS",
      !p_cv & p_acc                 ~ "FAIL_CV",
      p_cv  & !p_acc                ~ "FAIL_ACC",
      !p_cv & !p_acc                ~ "FAIL_BOTH",
      TRUE                          ~ "NA"
    )
  }
  
  if (verbose) {
    flag_tbl <- table(dil_series_df$dil_fda_flag, useNA = "ifany")
    message("[compute_dil_series_accuracy] FDA flag summary:")
    for (nm in names(flag_tbl)) {
      message(sprintf("  %-12s : %d rows", nm, flag_tbl[[nm]]))
    }
  }
  
  return(dil_series_df)
}


# ============================================================================
# .extract_fda_loqs_from_dil_series
#
# Extracts FDA 2018 LLOQ/ULOQ scalars from the pre-computed dilution-series
# accuracy table (output of compute_dil_series_accuracy()).
#
# This replaces the internal back-calculation approach in
# .compute_fda2018_scalars() which was failing because its inverse-prediction
# path did not reliably handle all model types.
#
# The dilution-series table already contains:
#   dil_nominal_concentration  – nominal conc at each dilution level
#   dil_backcalc_mean_conc     – back-calculated conc from pooled mean response
#   dil_accuracy_pct           – (backcalc / nominal) * 100
#   dil_cv_response            – CV% of response across plates
#   dil_passes_fda             – logical: passes both CV and accuracy
#   dil_fda_flag               – character flag
#
# @param dil_series_se_plate_source  data.frame from compute_dil_series_accuracy()
# @param fit                         nlsLM fit object (for predicting response at LOQ)
# @param independent_variable        character name of concentration column
# @param is_log_x                    logical; TRUE if concentration is log10-scaled
# @param cv_threshold                numeric; CV% limit for standard levels (default 20)
# @param lloq_cv_threshold           numeric; CV% limit at LLOQ (default 25)
# @param accuracy_lo                 numeric; lower accuracy bound (default 80)
# @param accuracy_hi                 numeric; upper accuracy bound (default 120)
# @param verbose                     logical
#
# @return Named list of FDA 2018 LOQ scalars matching the glance column names.
# ============================================================================
.extract_fda_loqs_from_dil_series <- function(
    dil_series_se_plate_source = NULL,
    fit                        = NULL,
    independent_variable       = "concentration",
    is_log_x                   = TRUE,
    verbose                    = TRUE) {
  
  # ── NA scaffold ────────────────────────────────────────────────────────
  na_result <- list(
    lloq_fda2018_concentration = NA_real_,
    lloq_fda2018_response      = NA_real_,
    uloq_fda2018_concentration = NA_real_,
    uloq_fda2018_response      = NA_real_,
    lloq_cv                    = NA_real_,
    uloq_cv                    = NA_real_,
    lloq_accuracy              = NA_real_,
    uloq_accuracy              = NA_real_,
    n_passing_std              = NA_integer_,
    n_total_std                = NA_integer_,
    pct_passing_std            = NA_real_,
    fda2018_status             = "FAILED"
  )
  
  # ── Guard: no dilution-series data ─────────────────────────────────────
  if (is.null(dil_series_se_plate_source) ||
      !is.data.frame(dil_series_se_plate_source) ||
      nrow(dil_series_se_plate_source) == 0) {
    if (verbose) message("[.extract_fda_loqs_from_dil_series] No dil_series data available.")
    return(na_result)
  }
  
  ds <- dil_series_se_plate_source
  
  # ── Guard: required columns ────────────────────────────────────────────
  required_cols <- c("dil_nominal_concentration", "dil_accuracy_pct",
                     "dil_cv_response", "dil_passes_fda", "dil_fda_flag",
                     "dilution")
  missing <- setdiff(required_cols, names(ds))
  if (length(missing) > 0) {
    if (verbose) message(sprintf(
      "[.extract_fda_loqs_from_dil_series] Missing columns: %s. Run compute_dil_series_accuracy() first.",
      paste(missing, collapse = ", ")
    ))
    return(na_result)
  }
  
  # ── Deduplicate to one row per dilution level ──────────────────────────
  # The dil_series table has one row per plate × dilution.
  # The pooled stats (dil_mean_response, dil_cv_response, dil_accuracy_pct,
  # dil_passes_fda) are identical within each dilution level, so we just
  
  # take the first row per dilution.
  ds_dedup <- ds[!duplicated(ds$dilution), , drop = FALSE]
  ds_dedup <- ds_dedup[order(ds_dedup$dil_nominal_concentration), , drop = FALSE]
  
  if (verbose) {
    message(sprintf(
      "[.extract_fda_loqs_from_dil_series] %d unique dilution levels from %d total rows.",
      nrow(ds_dedup), nrow(ds)
    ))
  }
  
  # ── Identify evaluable rows (dil_passes_fda is not NA) ─────────────────
  evaluable <- ds_dedup[!is.na(ds_dedup$dil_passes_fda), , drop = FALSE]
  n_total   <- nrow(evaluable)
  
  if (n_total == 0) {
    if (verbose) message("[.extract_fda_loqs_from_dil_series] No evaluable dilution levels.")
    result <- na_result
    result$n_total_std     <- nrow(ds_dedup)
    result$n_passing_std   <- 0L
    result$pct_passing_std <- 0
    result$fda2018_status  <- "NO_PASSING_LEVELS"
    return(result)
  }
  
  # ── Count passing levels ───────────────────────────────────────────────
  passing <- evaluable[evaluable$dil_passes_fda == TRUE, , drop = FALSE]
  n_pass  <- nrow(passing)
  
  if (verbose) {
    message(sprintf(
      "[.extract_fda_loqs_from_dil_series] %d / %d evaluable levels pass FDA criteria.",
      n_pass, n_total
    ))
  }
  
  if (n_pass == 0) {
    result <- na_result
    result$n_total_std     <- as.integer(n_total)
    result$n_passing_std   <- 0L
    result$pct_passing_std <- 0
    result$fda2018_status  <- "NO_PASSING_LEVELS"
    
    if (verbose) {
      # Show why each level failed
      for (i in seq_len(nrow(evaluable))) {
        row <- evaluable[i, ]
        message(sprintf(
          "  Dilution %s: nom_conc=%.2f, CV=%.2f%%, Acc=%.2f%%, flag=%s",
          as.character(row$dilution),
          row$dil_nominal_concentration,
          ifelse(is.na(row$dil_cv_response), NaN, row$dil_cv_response),
          ifelse(is.na(row$dil_accuracy_pct), NaN, row$dil_accuracy_pct),
          row$dil_fda_flag
        ))
      }
    }
    return(result)
  }
  
  # ── Sort passing levels by nominal concentration (ascending) ───────────
  passing <- passing[order(passing$dil_nominal_concentration), , drop = FALSE]
  
  # ── LLOQ: lowest passing nominal concentration ─────────────────────────
  lloq_row <- passing[1, , drop = FALSE]
  lloq_fda2018_concentration <- lloq_row$dil_nominal_concentration
  lloq_cv                    <- lloq_row$dil_cv_response
  lloq_accuracy              <- lloq_row$dil_accuracy_pct
  
  # Predict model response at LLOQ concentration
  lloq_fda2018_response <- tryCatch({
    if (!is.null(fit) && inherits(fit, "nls")) {
      lloq_x <- if (is_log_x) log10(lloq_fda2018_concentration) else lloq_fda2018_concentration
      nd <- setNames(data.frame(lloq_x), independent_variable)
      as.numeric(predict(fit, newdata = nd))
    } else {
      NA_real_
    }
  }, error = function(e) NA_real_)
  
  # ── ULOQ: highest passing nominal concentration ────────────────────────
  uloq_row <- passing[nrow(passing), , drop = FALSE]
  uloq_fda2018_concentration <- uloq_row$dil_nominal_concentration
  uloq_cv                    <- uloq_row$dil_cv_response
  uloq_accuracy              <- uloq_row$dil_accuracy_pct
  
  # Predict model response at ULOQ concentration
  uloq_fda2018_response <- tryCatch({
    if (!is.null(fit) && inherits(fit, "nls")) {
      uloq_x <- if (is_log_x) log10(uloq_fda2018_concentration) else uloq_fda2018_concentration
      nd <- setNames(data.frame(uloq_x), independent_variable)
      as.numeric(predict(fit, newdata = nd))
    } else {
      NA_real_
    }
  }, error = function(e) NA_real_)
  
  pct_pass <- round(100 * n_pass / n_total, 2)
  
  if (verbose) {
    message(sprintf(
      "[.extract_fda_loqs_from_dil_series] LLOQ: conc=%.4g, CV=%.2f%%, Acc=%.2f%%",
      lloq_fda2018_concentration, lloq_cv, lloq_accuracy
    ))
    message(sprintf(
      "[.extract_fda_loqs_from_dil_series] ULOQ: conc=%.4g, CV=%.2f%%, Acc=%.2f%%",
      uloq_fda2018_concentration, uloq_cv, uloq_accuracy
    ))
    message(sprintf(
      "[.extract_fda_loqs_from_dil_series] %d/%d passing (%.1f%%)",
      n_pass, n_total, pct_pass
    ))
  }
  
  # ── Assemble result ────────────────────────────────────────────────────
  list(
    lloq_fda2018_concentration = as.numeric(lloq_fda2018_concentration),
    lloq_fda2018_response      = as.numeric(lloq_fda2018_response),
    uloq_fda2018_concentration = as.numeric(uloq_fda2018_concentration),
    uloq_fda2018_response      = as.numeric(uloq_fda2018_response),
    lloq_cv                    = as.numeric(lloq_cv),
    uloq_cv                    = as.numeric(uloq_cv),
    lloq_accuracy              = as.numeric(lloq_accuracy),
    uloq_accuracy              = as.numeric(uloq_accuracy),
    n_passing_std              = as.integer(n_pass),
    n_total_std                = as.integer(n_total),
    pct_passing_std            = pct_pass,
    fda2018_status             = "OK"
  )
}


# ============================================================================
# .summarize_dil_series_accuracy
#
# Summarises the per-dilution accuracy/precision table produced by
# compute_dil_series_accuracy() into scalar QC metrics that fit into a
# single-row glance data.frame.
#
# @param dil_series_se_plate_source  data.frame from compute_dil_series_accuracy(),
#        or NULL if unavailable.
# @param verbose  logical; emit diagnostic messages.
# @return Named list of scalar values (all NA when input is NULL or empty).
# ============================================================================
.summarize_dil_series_accuracy <- function(dil_series_se_plate_source = NULL,
                                           verbose = TRUE) {
  
  # ── NA scaffold returned when there is nothing to summarise ──────────
  na_result <- list(
    dil_n_points_total       = NA_integer_,
    dil_n_points_evaluable   = NA_integer_,
    dil_n_points_pass_fda    = NA_integer_,
    dil_n_points_fail_fda    = NA_integer_,
    dil_pct_pass_fda         = NA_real_,
    dil_median_accuracy_pct  = NA_real_,
    dil_mean_accuracy_pct    = NA_real_,
    dil_max_cv_response      = NA_real_,
    dil_mean_cv_response     = NA_real_,
    dil_fda_overall_pass     = NA,
    dil_fda_flags_summary    = NA_character_,
    dil_accuracy_range_lo    = NA_real_,
    dil_accuracy_range_hi    = NA_real_
  )
  
  if (is.null(dil_series_se_plate_source) ||
      !is.data.frame(dil_series_se_plate_source) ||
      nrow(dil_series_se_plate_source) == 0) {
    if (verbose) message("[.summarize_dil_series_accuracy] No dilution-series data — returning NAs")
    return(na_result)
  }
  
  ds <- dil_series_se_plate_source
  
  # ── Total number of dilution points ─────────────────────────────────
  n_total <- nrow(ds)
  
  # ── Evaluable = rows where dil_passes_fda is not NA ─────────────────
  evaluable_mask <- !is.na(ds$dil_passes_fda)
  n_evaluable    <- sum(evaluable_mask)
  
  if (n_evaluable == 0) {
    if (verbose) message("[.summarize_dil_series_accuracy] No evaluable dilution points")
    result        <- na_result
    result$dil_n_points_total     <- n_total
    result$dil_n_points_evaluable <- 0L
    return(result)
  }
  
  ds_eval <- ds[evaluable_mask, , drop = FALSE]
  
  # ── Pass / fail counts ──────────────────────────────────────────────
  n_pass <- sum(ds_eval$dil_passes_fda == TRUE,  na.rm = TRUE)
  n_fail <- sum(ds_eval$dil_passes_fda == FALSE, na.rm = TRUE)
  pct_pass <- if (n_evaluable > 0) round(100 * n_pass / n_evaluable, 2) else NA_real_
  
  # ── Accuracy summary (using dil_accuracy_pct, excluding NAs) ────────
  acc_vals <- ds_eval$dil_accuracy_pct[!is.na(ds_eval$dil_accuracy_pct)]
  median_acc <- if (length(acc_vals) > 0) median(acc_vals)    else NA_real_
  mean_acc   <- if (length(acc_vals) > 0) mean(acc_vals)      else NA_real_
  acc_lo     <- if (length(acc_vals) > 0) min(acc_vals)       else NA_real_
  acc_hi     <- if (length(acc_vals) > 0) max(acc_vals)       else NA_real_
  
  # ── Precision summary (using dil_cv_response) ──────────────────────
  cv_vals  <- ds_eval$dil_cv_response[!is.na(ds_eval$dil_cv_response)]
  max_cv   <- if (length(cv_vals) > 0) max(cv_vals)  else NA_real_
  mean_cv  <- if (length(cv_vals) > 0) mean(cv_vals) else NA_real_
  
  # ── Overall FDA pass: TRUE only if every evaluable point passes ─────
  overall_pass <- (n_fail == 0L && n_pass > 0L)
  
  # ── Flag summary: compact string of unique dil_fda_flag values ──────
  flags_summary <- if ("dil_fda_flag" %in% names(ds_eval)) {
    flags <- unique(ds_eval$dil_fda_flag[!is.na(ds_eval$dil_fda_flag)])
    if (length(flags) > 0) paste(sort(flags), collapse = "; ") else NA_character_
  } else {
    NA_character_
  }
  
  if (verbose) {
    message(sprintf(
      "[.summarize_dil_series_accuracy] %d total, %d evaluable, %d pass, %d fail (%.1f%% pass)",
      n_total, n_evaluable, n_pass, n_fail, pct_pass
    ))
    message(sprintf(
      "[.summarize_dil_series_accuracy] Accuracy: median=%.2f%%, mean=%.2f%%, range=[%.2f%%, %.2f%%]",
      median_acc, mean_acc, acc_lo, acc_hi
    ))
    message(sprintf(
      "[.summarize_dil_series_accuracy] Precision: mean CV=%.2f%%, max CV=%.2f%%",
      mean_cv, max_cv
    ))
  }
  
  list(
    dil_n_points_total       = as.integer(n_total),
    dil_n_points_evaluable   = as.integer(n_evaluable),
    dil_n_points_pass_fda    = as.integer(n_pass),
    dil_n_points_fail_fda    = as.integer(n_fail),
    dil_pct_pass_fda         = pct_pass,
    dil_median_accuracy_pct  = median_acc,
    dil_mean_accuracy_pct    = mean_acc,
    dil_max_cv_response      = max_cv,
    dil_mean_cv_response     = mean_cv,
    dil_fda_overall_pass     = overall_pass,
    dil_fda_flags_summary    = flags_summary,
    dil_accuracy_range_lo    = acc_lo,
    dil_accuracy_range_hi    = acc_hi
  )
}
