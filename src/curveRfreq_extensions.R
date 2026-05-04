filter_by_curve_id <- function(loaded_data, curve_id,
                               target_names = c("standards", "blanks",
                                                "samples", "curve_id_lookup"),
                               verbose = FALSE) {
  filtered <- loaded_data
  filtered$curve_id_whole_lookup <- filtered$curve_id_lookup
  filtered$whole_standards        <- filtered$standards
  filtered[target_names] <- lapply(filtered[target_names], function(df) {
    if (!is.data.frame(df) || nrow(df) == 0 || !"curve_id" %in% names(df)) return(df)
    df[as.character(df$curve_id) == as.character(curve_id), , drop = FALSE]
  })
  filtered
}


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
  parameters <- best_fit$best_fit_summary[, c("a", "b", "c", "d", "g")]
  
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
    
    mn <- switch(model_name,
                 logistic5    = "Y5",    loglogistic5 = "Yd5",
                 logistic4    = "Y4",    loglogistic4 = "Yd4",
                 gompertz4    = "Ygomp4",
                 model_name   # already legacy — pass through
    )
    
    
      a <- as.numeric(parameters$a[1])
      b <- as.numeric(parameters$b[1])
      c <- as.numeric(parameters$c[1])
      d <- as.numeric(parameters$d[1])
      g <- if ("g" %in% names(parameters) && !is.na(parameters$g[1])) as.numeric(parameters$g[1]) else NULL
      
      tryCatch(
        switch(mn,
               Y5     = inv_logistic5(y_model_scale,     a=a, b=b, c=c, d=d, g=g),
               Yd5    = inv_logistic5(y_model_scale,    a=a, b=b, c=c, d=d, g=g),
               Y4     = inv_logistic4(y_model_scale,     a=a, b=b, c=c, d=d),
               Yd4    = inv_loglogistic5(y_model_scale,    a=a, b=b, c=c, d=d),
               Ygomp4 = inv_gompertz4(y_model_scale, a=a, b=b, c=c, d=d),
               { warning("[safe_backcalc] Unknown model: ", mn); NA_real_ }
        ),
        error = function(e) {
          message("[safe_backcalc] ", mn, " failed: ", e$message)
          NA_real_
        }
      )
      
    }
  #     if (!is.null(fixed_a_result)) {
  #       cat("model_name free a ")
  #       print(mn)
  #       switch(mn, #model_name,
  #              Y5     = inv_Y5_fixed(y_model_scale,
  #                                    fixed_a = fixed_a_result,
  #                                    b = coef(fit)["b"], c = coef(fit)["c"],
  #                                    d = coef(fit)["d"], g = coef(fit)["g"]),
  #              Yd5    = inv_Yd5_fixed(y_model_scale,
  #                                     fixed_a = fixed_a_result,
  #                                     b = coef(fit)["b"], c = coef(fit)["c"],
  #                                     d = coef(fit)["d"], g = coef(fit)["g"]),
  #              Y4     = inv_Y4_fixed(y_model_scale,
  #                                    fixed_a = fixed_a_result,
  #                                    b = coef(fit)["b"], c = coef(fit)["c"],
  #                                    d = coef(fit)["d"]),
  #              Yd4    = inv_Yd4_fixed(y_model_scale,
  #                                     fixed_a = fixed_a_result,
  #                                     b = coef(fit)["b"], c = coef(fit)["c"],
  #                                     d = coef(fit)["d"]),
  #              Ygomp4 = inv_Ygomp4_fixed(y_model_scale,
  #                                        fixed_a = fixed_a_result,
  #                                        b = coef(fit)["b"], c = coef(fit)["c"],
  #                                        d = coef(fit)["d"]),
  #              NA_real_
  #       )
  #     } else {
  #       cat("model_name with fixed result")
  #       print(mn)
  #       
  #       switch(mn, # model_name 
  #              Y5     = inv_Y5(y_model_scale,
  #                              a = coef(fit)["a"], b = coef(fit)["b"],
  #                              c = coef(fit)["c"], d = coef(fit)["d"],
  #                              g = coef(fit)["g"]),
  #              Yd5    = inv_Yd5(y_model_scale,
  #                               a = coef(fit)["a"], b = coef(fit)["b"],
  #                               c = coef(fit)["c"], d = coef(fit)["d"],
  #                               g = coef(fit)["g"]),
  #              Y4     = inv_Y4(y_model_scale,
  #                              a = coef(fit)["a"], b = coef(fit)["b"],
  #                              c = coef(fit)["c"], d = coef(fit)["d"]),
  #              Yd4    = inv_Yd4(y_model_scale,
  #                               a = coef(fit)["a"], b = coef(fit)["b"],
  #                               c = coef(fit)["c"], d = coef(fit)["d"]),
  #              Ygomp4 = inv_Ygomp4(y_model_scale,
  #                                  a = coef(fit)["a"], b = coef(fit)["b"],
  #                                  c = coef(fit)["c"], d = coef(fit)["d"]),
  #              NA_real_
  #       )
  #     }
  #   }, error = function(e) NA_real_)
  # }
  
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



plot_ISPI_standard_curve <- function(best_fit,
                                is_display_log_response,
                                is_display_log_independent,
                                pcov_threshold,
                                curve_id_lookup,
                                response_variable = "mfi",
                                independent_variable = "concentration",
                                mcmc_samples = NULL,
                                mcmc_pred = NULL) {
  p <- plotly::plot_ly()
  # best_fit_v <<- best_fit
  # mcmc_samples_in <<- mcmc_samples
  # mcmc_pred_in <<- mcmc_pred
  
  # ── Resolve response column ────────────────────────────────────────
  resolved <- ensure_response_column(
    df           = best_fit$best_data,
    response_var = response_variable,
    coerce_numeric = TRUE,
    context      = "plot_standard_curve/best_data"
  )
  best_fit$best_data <- resolved$df
  response_variable  <- resolved$response_var
  
  if (!resolved$ok) {
    return(
      plotly::plot_ly() %>%
        plotly::layout(
          title = "Cannot plot: response variable not found",
          annotations = list(
            text = paste0(
              "Column '", response_variable,
              "' not found or has no finite values in standard data.<br>",
              "Available columns: ",
              paste(names(best_fit$best_data), collapse = ", ")
            ),
            xref = "paper", yref = "paper",
            x = 0.5, y = 0.5, showarrow = FALSE
          )
        )
    )
  }
  
  # ── Resolve independent variable ───────────────────────────────────
  if (!independent_variable %in% names(best_fit$best_data)) {
    if ("concentration" %in% names(best_fit$best_data)) {
      independent_variable <- "concentration"
    } else {
      return(plotly::plot_ly() %>%
               plotly::layout(title = "Missing independent variable column"))
    }
  }
  
  # ── Ensure stype exists ────────────────────────────────────────────
  if (!"stype" %in% names(best_fit$best_data)) {
    best_fit$best_data$stype <- "S"
  }
  
  # ── Resolve response column in sample_se too ───────────────────────
  samples_predicted_conc <- best_fit$sample_se
  if (!is.null(samples_predicted_conc) && nrow(samples_predicted_conc) > 0) {
    samp_resolved <- ensure_response_column(
      df           = samples_predicted_conc,
      response_var = response_variable,
      coerce_numeric = TRUE,
      context      = "plot_standard_curve/sample_se"
    )
    samples_predicted_conc <- samp_resolved$df
    if (samp_resolved$ok && samp_resolved$response_var != response_variable) {
      samples_predicted_conc[[response_variable]] <-
        samples_predicted_conc[[samp_resolved$response_var]]
    }
    samples_predicted_conc <- samples_predicted_conc[
      !is.nan(samples_predicted_conc$raw_predicted_concentration) &
        is.finite(samples_predicted_conc$raw_predicted_concentration), ,
      drop = FALSE
    ]
  } else {
    samples_predicted_conc <- data.frame(
      raw_predicted_concentration = numeric(0),
      pcov = numeric(0),
      stringsAsFactors = FALSE
    )
    samples_predicted_conc[[response_variable]] <- numeric(0)
  }
  
  best_fit$best_pred$pcov_threshold <- pcov_threshold
  
  safe_glance <- function(field, default = NA_real_) {
    val <- best_fit$best_fit_summary[[field]]
    if (is.null(val) || length(val) == 0) return(default)
    val <- unlist(val)
    if (all(is.na(val))) return(default)
    val[1]
  }
  
  ### 1. RESPONSE VARIABLE (Y) log transform
  log_response_status <- isTRUE(as.logical(safe_glance("is_log_response", FALSE)))
  if (log_response_status && !isTRUE(is_display_log_response)) {
    best_fit$best_data[[response_variable]] <- 10^best_fit$best_data[[response_variable]]
    best_fit$best_pred$yhat_response               <- 10^best_fit$best_pred$yhat_response
    best_fit$best_fit_summary$llod             <- 10^safe_glance("llod")
    best_fit$best_fit_summary$ulod             <- 10^safe_glance("ulod")
    best_fit$best_fit_summary$lloq_y           <- 10^safe_glance("lloq_y")
    best_fit$best_fit_summary$uloq_y           <- 10^safe_glance("uloq_y")
    best_fit$best_fit_summary$inflect_y        <- 10^safe_glance("inflect_y")
    best_fit$best_d2xy$d2x_y              <- 10^best_fit$best_d2xy$d2x_y
    if (!is.null(best_fit$best_curve_ci)) {
      best_fit$best_curve_ci$ci_lo        <- 10^best_fit$best_curve_ci$ci_lo
      best_fit$best_curve_ci$ci_hi        <- 10^best_fit$best_curve_ci$ci_hi
    }
    if (nrow(samples_predicted_conc) > 0 &&
        response_variable %in% names(samples_predicted_conc)) {
      samples_predicted_conc[[response_variable]] <-
        10^samples_predicted_conc[[response_variable]]
    }
  }
  
  ### 2. INDEPENDENT VARIABLE (X) log transform
  log_x_status <- isTRUE(as.logical(safe_glance("is_log_x", FALSE)))
  if (log_x_status && !isTRUE(is_display_log_independent)) {
    best_fit$best_data$concentration       <- 10^best_fit$best_data$concentration
    best_fit$best_pred$predicted_concentration                   <- 10^best_fit$best_pred$predicted_concentration
    best_fit$best_fit_summary$lloq              <- 10^safe_glance("lloq")
    best_fit$best_fit_summary$uloq              <- 10^safe_glance("uloq")
    best_fit$best_fit_summary$inflect_x         <- 10^safe_glance("inflect_x")
    best_fit$best_d2xy$x                   <- 10^best_fit$best_d2xy$x
    if (nrow(samples_predicted_conc) > 0) {
      samples_predicted_conc$raw_predicted_concentration <-
        10^samples_predicted_conc$raw_predicted_concentration
    }
    best_fit$best_fit_summary$mindc             <- 10^safe_glance("mindc")
    best_fit$best_fit_summary$maxdc             <- 10^safe_glance("maxdc")
    best_fit$best_fit_summary$minrdl            <- 10^safe_glance("minrdl")
    best_fit$best_fit_summary$maxrdl            <- 10^safe_glance("maxrdl")
    if (!is.null(best_fit$best_curve_ci)) {
      best_fit$best_curve_ci$x             <- 10^best_fit$best_curve_ci$x
    }
  }
  
  y3_label <- "Precision Coefficient of Variation (pCoV %)"
  
  if (is_display_log_response) {
    response_formatted <- format_assay_terms(response_variable)
    cat("FORMATTED:", response_formatted, "\n")
    y_label <- paste0("log<sub>10</sub> ", response_formatted)
  } else {
    y_label <- format_assay_terms(response_variable)
  }
  # y_label_v <<- y_label
  # 
  if (is_display_log_independent) {
    x_label <- paste0("log<sub>10</sub> ", format_assay_terms(independent_variable))
  } else {
    x_label <- format_assay_terms(independent_variable)
  }
  
  ### 3. MODEL NAME
  model_name <- best_fit$best_model_name
  title_model_name <- switch(
    model_name,
    "logistic4" = "4-parameter Logistic",
    "loglogistic4" = "4-parameter Log-Logistic",
    "gompertz4" = "4-parameter Gompertz type",
    "logistic5" = "5-parameter Logistic",
    "loglogistic5" = "5-parameter Log-Logistic",
    model_name
  )
  
  ## 3b. PREPARE SAMPLE-UNCERTAINTY (single scaling)
  print(names(best_fit$best_pred))
  se_model   <- best_fit$best_pred$pcov
  se_samples <- samples_predicted_conc$pcov
  se_all     <- c(best_fit$best_pred$pcov, samples_predicted_conc$pcov)
  se_range   <- range(se_all, na.rm = TRUE)
  
  se_max <- 125
  se_min <- -2
  se_axis_limits <- c(se_min, se_max * 1.1)
  dtick <- ifelse(se_max > 19, ifelse(se_max > 35, 10, 5), 1)
  
  ### 4. RAW POINTS
  plot_std <- best_fit$best_data
  
  if (is_display_log_independent) {
    glance_fda_lloq_conc <- log10(best_fit$best_fit_summary$lloq_fda2018_concentration)
    glance_fda_2018_uloq_conc <- log10(best_fit$best_fit_summary$uloq_fda2018_concentration)
  } else {
    glance_fda_lloq_conc <- best_fit$best_fit_summary$lloq_fda2018_concentration
    glance_fda_2018_uloq_conc <- best_fit$best_fit_summary$uloq_fda2018_concentration
  }
  
  plot_std$fda2018_class <- ifelse(
    plot_std[[independent_variable]] >= glance_fda_lloq_conc &
      plot_std[[independent_variable]] <= glance_fda_2018_uloq_conc,
    "Standards (+ FDA 2018)",
    "Standards (- FDA 2018)"
  )
  
  std_in <- plot_std[
    plot_std$stype == "S" &
      plot_std$fda2018_class == "Standards (+ FDA 2018)", ]
  std_out <- plot_std[
    plot_std$stype == "S" &
      plot_std$fda2018_class == "Standards (- FDA 2018)", ]
  blanks <- plot_std[plot_std$stype == "B", ]
  
  ### Standards inside FDA range (circle)
  p <- p %>% plotly::add_trace(
    data = std_in,
    x = std_in[[independent_variable]],
    y = std_in[[response_variable]],
    type = "scatter",
    mode = "markers",
    name = "Standards (+ FDA 2018)",
    legendgroup = "standards",
    marker = list(color = "#2b3d26", symbol = "circle"),
    text = ~paste0(
      "<br>", format_assay_terms(independent_variable), ": ",
      std_in[[independent_variable]],
      "<br>Dilution Factor: ", dilution,
      "<br>", format_assay_terms(response_variable), ": ",
      std_in[[response_variable]],
      "<br> FDA 2018 Status: ", gsub("Standards ", "", std_in$fda2018_class)
    ),
    hoverinfo = "text"
  )
  
  ### Standards outside FDA range (triangle)
  p <- p %>% plotly::add_trace(
    data = std_out,
    x = std_out[[independent_variable]],
    y = std_out[[response_variable]],
    type = "scatter",
    mode = "markers",
    name = "Standards (- FDA 2018)",
    legendgroup = "standards",
    marker = list(color = "#2b3d26", symbol = "triangle-up", size = 8),
    text = ~paste0(
      "<br>", format_assay_terms(independent_variable), ": ",
      std_out[[independent_variable]],
      "<br>Dilution Factor: ", dilution,
      "<br>", format_assay_terms(response_variable), ": ",
      std_out[[response_variable]],
      "<br>FDA 2018 Status: ", gsub("Standards ", "", std_out$fda2018_class)
    ),
    hoverinfo = "text"
  )
  
  ### Blanks
  p <- p %>% plotly::add_trace(
    data = blanks,
    x = blanks[[independent_variable]],
    y = blanks[[response_variable]],
    type = "scatter",
    mode = "markers",
    name = "Geometric Mean of Blanks",
    marker = list(color = "#c2b280", symbol = "circle"),
    text = ~paste0(
      "<br>", format_assay_terms(independent_variable), ": ",
      blanks[[independent_variable]],
      "<br>Dilution Factor: ", dilution,
      "<br>", format_assay_terms(response_variable), ": ",
      blanks[[response_variable]]
    ),
    hoverinfo = "text"
  )
  
  ### 5. FITTED CURVE
  p <- p %>% add_lines(
    x = best_fit$best_pred$predicted_concentration,
    y = best_fit$best_pred$yhat,
    name = "Fitted Curve",
    legendgroup = "fitted_curve",
    showlegend = TRUE,
    line = list(color = "#2b3d26")
  )
  
  ### 5b. 95% CI BANDS (delta method)
  if (!is.null(best_fit$best_curve_ci)) {
    p <- p %>% add_lines(
      x           = best_fit$best_curve_ci$x,
      y           = best_fit$best_curve_ci$ci_lo,
      name        = "95% CI",
      line        = list(color = "#2b3d26", dash = "dash"),
      legendgroup = "fitted_curve"
    ) %>% add_lines(
      x           = best_fit$best_curve_ci$x,
      y           = best_fit$best_curve_ci$ci_hi,
      name        = "",
      line        = list(color = "#2b3d26", dash = "dash"),
      legendgroup = "fitted_curve",
      showlegend  = FALSE
    )
  }
  
  ### 6. LOD lines (horizontal)
  p <- p %>% add_lines(
    x = best_fit$best_pred$predicted_concentration,
    y = best_fit$best_fit_summary$ulod,
    name = paste("Upper LOD: (",
                 round(best_fit$best_fit_summary$maxdc, 3), ",",
                 round(best_fit$best_fit_summary$ulod, 3), ")"),
    line = list(color = "#e25822", dash = "dash"),
    legendgroup = "linked_ulod",
    visible = "legendonly"
  )
  
  p <- p %>% add_lines(
    x = best_fit$best_pred$predicted_concentration,
    y = best_fit$best_fit_summary$llod,
    name = paste("Lower LOD: (",
                 round(best_fit$best_fit_summary$mindc, 3), ",",
                 round(best_fit$best_fit_summary$llod, 3), ")"),
    line = list(color = "#e25822", dash = "dash"),
    legendgroup = "linked_llod",
    visible = "legendonly"
  )
  
  ### 7. LOQ (vertical + horizontal)
  y_min <- min(best_fit$best_data[[response_variable]], na.rm = TRUE)
  y_max <- max(best_fit$best_data[[response_variable]], na.rm = TRUE)
  
  ### 6b. MDC / RDL vertical lines
  if (!is.na(best_fit$best_fit_summary$mindc)) {
    p <- p %>% add_lines(
      x = c(best_fit$best_fit_summary$mindc, best_fit$best_fit_summary$mindc),
      y = c(y_min, y_max),
      name = paste("Lower DC:", round(best_fit$best_fit_summary$mindc, 3)),
      line = list(color = "#e25822", dash = "dash"),
      legendgroup = "linked_llod",
      showlegend = FALSE, hoverinfo = "text", visible = "legendonly"
    )
  }
  
  if (!is.na(best_fit$best_fit_summary$minrdl)) {
    p <- p %>% add_lines(
      x = c(best_fit$best_fit_summary$minrdl, best_fit$best_fit_summary$minrdl),
      y = c(y_min, y_max),
      name = paste("Lower RDL:", round(best_fit$best_fit_summary$minrdl, 3)),
      line = list(color = "#e25822"),
      legendgroup = "linked_llod",
      showlegend = TRUE, hoverinfo = "text", visible = "legendonly"
    )
  }
  
  if (!is.na(best_fit$best_fit_summary$maxdc)) {
    p <- p %>% add_lines(
      x = c(best_fit$best_fit_summary$maxdc, best_fit$best_fit_summary$maxdc),
      y = c(y_min, y_max),
      name = paste("Upper DC:", round(best_fit$best_fit_summary$maxdc, 3)),
      line = list(color = "#e25822", dash = "dash"),
      legendgroup = "linked_ulod",
      showlegend = FALSE, hoverinfo = "text", visible = "legendonly"
    )
  }
  
  if (!is.na(best_fit$best_fit_summary$maxrdl)) {
    p <- p %>% add_lines(
      x = c(best_fit$best_fit_summary$maxrdl, best_fit$best_fit_summary$maxrdl),
      y = c(y_min, y_max),
      name = paste("Upper RDL:", round(best_fit$best_fit_summary$maxrdl, 3)),
      line = list(color = "#e25822"),
      legendgroup = "linked_ulod",
      showlegend = TRUE, hoverinfo = "text", visible = "legendonly"
    )
  }
  
  ### LLOQ vertical line
  p <- p %>% add_lines(
    x = c(best_fit$best_fit_summary$lloq),
    y = c(y_min, y_max),
    name = paste("Lower LOQ: (",
                 round(best_fit$best_fit_summary$lloq, 3), ",",
                 round(best_fit$best_fit_summary$lloq_y, 3), ")"),
    line = list(color = "#875692"),
    legendgroup = "linked_lloq",
    hoverinfo = "text", visible = "legendonly"
  )
  
  ### ULOQ vertical line
  p <- p %>% add_lines(
    x = c(best_fit$best_fit_summary$uloq),
    y = c(y_min, y_max),
    name = paste("Upper LOQ: (",
                 round(best_fit$best_fit_summary$uloq, 3), ",",
                 round(best_fit$best_fit_summary$uloq_y, 3), ")"),
    line = list(color = "#875692"),
    legendgroup = "linked_uloq",
    hoverinfo = "text", visible = "legendonly"
  )
  
  ### Horizontal LOQ lines
  p <- p %>% add_lines(
    x = best_fit$best_pred$predicted_concentration,
    y = best_fit$best_fit_summary$uloq_y,
    name = "",
    legendgroup = "linked_uloq", showlegend = FALSE,
    line = list(color = "#875692"), visible = "legendonly"
  )
  
  p <- p %>% add_lines(
    x = best_fit$best_pred$predicted_concentration,
    y = best_fit$best_fit_summary$lloq_y,
    name = "",
    legendgroup = "linked_lloq", showlegend = FALSE,
    line = list(color = "#875692"), visible = "legendonly"
  )
  
  ### 8a. SECOND DERIVATIVE (y2 axis)
  p <- p %>% add_lines(
    x = best_fit$best_d2xy$x,
    y = best_fit$best_d2xy$d2x_y,
    name = "2nd Derivative of x given y",
    yaxis = "y2",
    line = list(color = "#604e97"),
    visible = "legendonly"
  )
  
  ## 9. Samples - interpolated
  p <- p %>% add_trace(
    data = samples_predicted_conc,
    x = ~raw_predicted_concentration,
    y = samples_predicted_conc[[response_variable]],
    type = "scatter",
    mode = "markers",
    name = "Samples",
    marker = list(color = "#d1992a", symbol = "circle"),
    text = ~paste("Predicted", x_label, ":", raw_predicted_concentration,
                  "<br>", y_label, ":", samples_predicted_conc[[response_variable]],
                  "<br>Patient ID:", patientid,
                  "<br> Timepoint:", timeperiod,
                  "<br>Well:", well,
                  "<br>LOQ Gate Class:", samples_predicted_conc$gate_class_loq,
                  "<br>LOD Gate Class:", samples_predicted_conc$gate_class_lod,
                  "<br> PCOV Gate Class:", samples_predicted_conc$gate_class_pcov),
    hovertemplate = "%{text}<extra></extra>"
  )
  ### 8b. Sample uncertainty (y3 axis) — interpolated
  unc_col <- list(color = "#e68fac")
  p <- p %>% add_lines(
    x = best_fit$best_pred$predicted_concentration,
    y = best_fit$best_pred$pcov,
    name = "Measurement Uncertainty",
    yaxis = "y3",
    line = unc_col,
    legendgroup = "linked_interp_uncertainty",
    visible = "legendonly"
  ) %>% add_trace(
    data = samples_predicted_conc,
    x = ~raw_predicted_concentration,
    y = ~pcov,
    type = "scatter",
    mode = "markers",
    name = "",
    marker = list(color = "#800032", symbol = "circle"),
    text = ~paste("Predicted", x_label, ":", raw_predicted_concentration,
                  "<br>Coefficient of Variation (pCoV):", round(pcov, 2), "%"),
    yaxis = "y3",
    legendgroup = "linked_interp_uncertainty",
    showlegend = FALSE,
    hovertemplate = "%{text}<extra></extra>",
    visible = "legendonly"
  ) %>% add_lines(
    x = best_fit$best_pred$predicted_concentration,
    y = best_fit$best_pred$pcov_threshold,
    name = paste0("pCoV Threshold: ", best_fit$best_pred$pcov_threshold, "%"),
    yaxis = "y3",
    line = list(color = "#e68fac", dash = "dash"),
    legendgroup = "linked_interp_uncertainty",
    visible = "legendonly"
  )
  
  # MCMC ROBUST SAMPLES
  if (!is.null(mcmc_samples) && nrow(mcmc_samples) > 0) {
    # Map assay_response to the plot's response variable if needed
    if ("assay_response" %in% names(mcmc_samples) &&
        !response_variable %in% names(mcmc_samples)) {
      mcmc_samples[[response_variable]] <- mcmc_samples$assay_response
    }
    
    mcmc_x <- mcmc_samples$raw_robust_concentration
    mcmc_y <- mcmc_samples[[response_variable]]
    
    # Apply same x-axis transform as all other traces (Section 2)
    if (log_x_status && !isTRUE(is_display_log_independent)) {
      mcmc_x <- 10^mcmc_x
    }
    
    # Build hover text
    mcmc_hover <- paste0(
      "MCMC Robust Concentration: ", round(mcmc_x, 4),
      "<br>", format_assay_terms(response_variable), ": ", round(mcmc_y, 2),
      if ("patientid" %in% names(mcmc_samples))
        paste0("<br>Patient ID: ", mcmc_samples$patientid) else "",
      if ("timeperiod" %in% names(mcmc_samples))
        paste0("<br>Timepoint: ", mcmc_samples$timeperiod) else "",
      if ("well" %in% names(mcmc_samples))
        paste0("<br>Well: ", mcmc_samples$well) else "",
      if ("pcov_robust_concentration" %in% names(mcmc_samples))
        paste0("<br>MCMC pCoV: ",
               formatC(mcmc_samples$pcov_robust_concentration, format = "g", digits = 4), "%") else "",
      if ("gate_class_loq" %in% names(mcmc_samples))
        paste0("<br>LOQ Gate Class: ", mcmc_samples$gate_class_loq) else "",
      if ("gate_class_lod" %in% names(mcmc_samples))
        paste0("<br>LOD Gate Class: ", mcmc_samples$gate_class_lod) else "",
      if ("source_nom" %in% names(mcmc_samples)) 
        paste0("<br>Source: ", mcmc_samples$source_nom) else ""
    )
    
    p <- p %>% plotly::add_trace(
      x = mcmc_x,
      y = mcmc_y,
      type = "scatter",
      mode = "markers",
      name = "MCMC Samples",
      marker = list(
        color = "#d1992a",
        symbol = "diamond",
        size = 7,
        opacity = 0.8
      ),
      text = mcmc_hover,
      hovertemplate = "%{text}<extra></extra>"
    )
    
    ### 9c. MCMC pCoV scatter points at sample locations (y3 axis)
    if ("pcov_robust_concentration" %in% names(mcmc_samples)) {
      pcov_valid <- is.finite(mcmc_samples$pcov_robust_concentration) &
        is.finite(mcmc_x)
      
      if (any(pcov_valid)) {
        p <- p %>% plotly::add_trace(
          x = mcmc_x[pcov_valid],
          y = mcmc_samples$pcov_robust_concentration[pcov_valid],
          type = "scatter",
          mode = "markers",
          name = "",
          marker = list(color = "#800032", symbol = "diamond", size = 5),
          text = paste0(
            "MCMC ", x_label, ": ", round(mcmc_x[pcov_valid], 4),
            "<br>MCMC pCoV: ",
            formatC(mcmc_samples$pcov_robust_concentration[pcov_valid], format = "g", digits = 4), "%"
          ),
          yaxis = "y3",
          legendgroup = "linked_mcmc_uncertainty",
          showlegend = FALSE,
          hovertemplate = "%{text}<extra></extra>",
          visible = "legendonly"
        )
      }
    }
  }
  
  ### 8c. MCMC pCoV uncertainty smooth line (y3 axis) — dense pred grid
  if (!is.null(mcmc_pred) && nrow(mcmc_pred) > 0 &&
      "pcov_robust_concentration" %in% names(mcmc_pred) &&
      "raw_robust_concentration" %in% names(mcmc_pred)) {
    
    pred_valid <- is.finite(mcmc_pred$pcov_robust_concentration) &
      is.finite(mcmc_pred$raw_robust_concentration)
    
    if (any(pred_valid)) {
      mcmc_pred_x    <- mcmc_pred$raw_robust_concentration[pred_valid]
      mcmc_pred_pcov <- mcmc_pred$pcov_robust_concentration[pred_valid]
      
      # Apply same x-axis transform as all other traces (Section 2)
      if (log_x_status && !isTRUE(is_display_log_independent)) {
        mcmc_pred_x <- 10^mcmc_pred_x
      }
      
      # Sort by x for smooth line
      sort_idx       <- order(mcmc_pred_x)
      mcmc_pred_x    <- mcmc_pred_x[sort_idx]
      mcmc_pred_pcov <- mcmc_pred_pcov[sort_idx]
      
      # MCMC uncertainty smooth line
      p <- p %>% plotly::add_lines(
        x = mcmc_pred_x,
        y = mcmc_pred_pcov,
        name = "MCMC Measurement Uncertainty",
        yaxis = "y3",
        line = list(color = "#e68fac"),
        legendgroup = "linked_mcmc_uncertainty",
        showlegend = TRUE,
        visible = "legendonly"
      )
      
      # pCoV threshold line — same threshold, shown with MCMC group
      p <- p %>% plotly::add_lines(
        x = mcmc_pred_x,
        y = rep(pcov_threshold, length(mcmc_pred_x)),
        name = paste0("pCoV Threshold: ", pcov_threshold, "%"),
        yaxis = "y3",
        line = list(color = "#e68fac", dash = "dash"),
        legendgroup = "linked_mcmc_uncertainty",
        showlegend = TRUE,
        visible = "legendonly"
      )
    }
  }
  
  
  ### 9b. MCMC ROBUST SAMPLES
  
  ### 10. INFLECTION POINT
  p <- p %>% add_trace(
    x = best_fit$best_fit_summary$inflect_x,
    y = best_fit$best_fit_summary$inflect_y,
    type = "scatter",
    mode = "markers",
    name = paste("Inflection Point: (",
                 round(best_fit$best_fit_summary$inflect_x, 3), ",",
                 round(best_fit$best_fit_summary$inflect_y, 3), ")"),
    legendgroup = "fitted_curve",
    showlegend = TRUE,
    marker = list(color = "#2724F0", size = 8)
  )
  
  ### 11. LAYOUT
  p <- p %>% layout(
    title = paste(
      "Fitted", title_model_name, "Model (",
      unique(curve_id_lookup$plate), ",",
      unique(curve_id_lookup$antigen), ")"
    ),
    xaxis = list(
      title    = x_label,
      showgrid = TRUE,
      zeroline = FALSE
    ),
    yaxis = list(
      title    = y_label,
      showgrid = TRUE,
      zeroline = TRUE
    ),
    legend = list(
      x       = 1.1,
      y       = 1,
      xanchor = "left"
    ),
    font = list(size = 12),
    yaxis2 = list(
      showticklabels = FALSE,
      title          = "",
      tickmode       = "linear",
      dtick          = 10,
      overlaying     = "y",
      side           = "right",
      showgrid       = FALSE,
      zeroline       = FALSE
    ),
    yaxis3 = list(
      overlaying     = "y",
      side           = "right",
      title          = y3_label,
      range          = se_axis_limits,
      tickmode       = "linear",
      type           = "linear",
      dtick          = dtick,
      showgrid       = FALSE,
      zeroline       = FALSE,
      showticklabels = TRUE
    )
  )
  
  return(p)
}

#' Ensure the response variable column exists in a data frame.
#' If the named column is missing, attempts to find it via
#' assay_response_variable metadata or common response column names.
#' Optionally coerces to numeric.
#'
#' @param df            Data frame to check
#' @param response_var  Expected column name (e.g. "mfi", "absorbance")
#' @param coerce_numeric Logical; if TRUE, coerce the column to numeric
#' @param context       Character label for diagnostic messages
#' @return A list with:
#'   \item{df}{The (possibly modified) data frame}
#'   \item{response_var}{The resolved column name (may differ from input)}
#'   \item{ok}{Logical: TRUE if a valid numeric response column was found}
ensure_response_column <- function(df, 
                                   response_var, 
                                   coerce_numeric = TRUE,
                                   context = "") {
  
  prefix <- if (nzchar(context)) paste0("[", context, "] ") else ""
  
  # Guard: NULL or empty data frame
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    message(sprintf("%sData frame is NULL or empty.", prefix))
    return(list(df = df, response_var = response_var, ok = FALSE))
  }
  
  # Case 1: Column exists by name
  if (response_var %in% names(df)) {
    if (coerce_numeric && !is.numeric(df[[response_var]])) {
      message(sprintf(
        "%sCoercing '%s' from %s to numeric.",
        prefix, response_var, class(df[[response_var]])[1]
      ))
      df[[response_var]] <- suppressWarnings(as.numeric(df[[response_var]]))
    }
    n_finite <- sum(is.finite(df[[response_var]]))
    if (n_finite == 0) {
      message(sprintf("%s'%s' exists but has 0 finite values.", prefix, response_var))
      return(list(df = df, response_var = response_var, ok = FALSE))
    }
    return(list(df = df, response_var = response_var, ok = TRUE))
  }
  
  # Case 2: Try assay_response_variable metadata
  if ("assay_response_variable" %in% names(df)) {
    arv <- unique(df$assay_response_variable)
    arv <- arv[!is.na(arv) & arv != ""]
    for (candidate in arv) {
      if (candidate %in% names(df)) {
        message(sprintf(
          "%s'%s' not found; using '%s' from assay_response_variable.",
          prefix, response_var, candidate
        ))
        response_var <- candidate
        if (coerce_numeric && !is.numeric(df[[response_var]])) {
          df[[response_var]] <- suppressWarnings(as.numeric(df[[response_var]]))
        }
        return(list(df = df, response_var = response_var, ok = TRUE))
      }
    }
  }
  
  # Case 3: Try common response column names
  common_names <- c("mfi", "absorbance", "fluorescence", "od",
                    "MFI", "Absorbance", "Fluorescence", "OD")
  found <- intersect(common_names, names(df))
  if (length(found) > 0) {
    candidate <- found[1]
    message(sprintf(
      "%s'%s' not found; falling back to '%s'.",
      prefix, response_var, candidate
    ))
    response_var <- candidate
    if (coerce_numeric && !is.numeric(df[[response_var]])) {
      df[[response_var]] <- suppressWarnings(as.numeric(df[[response_var]]))
    }
    return(list(df = df, response_var = response_var, ok = TRUE))
  }
  
  # Case 4: Try to extract from the NLS formula LHS
  # (If there's a formula stored somewhere, we could parse it)
  
  message(sprintf(
    "%sCannot find response column '%s'. Available columns: %s",
    prefix, response_var, paste(names(df), collapse = ", ")
  ))
  return(list(df = df, response_var = response_var, ok = FALSE))
}