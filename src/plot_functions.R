
# ---------------------------------------------------------------------------
# compute_curve_ci
#
# Fast delta-method 95 % CI bands for the fitted curve.
#
# Performance: performs exactly p_free vectorised formula evaluations over the
# full x_new grid (one per free parameter) rather than the naive n × p_free
# scalar predict() calls used in generate_mdc_rdl.  For a 200-point grid and
# a 5-parameter model this is ~40× fewer evaluations.
#
# Args
#   fit     : converged nls / nlsLM object
#   x_new   : numeric vector of x values at which to evaluate
#   x_var   : name of the independent variable in the model formula
#   fixed_a : numeric scalar or NULL  – fixed lower asymptote not in vcov()
#   level   : confidence level (default 0.95)
#
# Returns data.frame with columns: x, ci_lo, ci_hi
# ---------------------------------------------------------------------------
compute_curve_ci <- function(fit, x_new, x_var, fixed_a = NULL, level = 0.95) {
  n      <- length(x_new)
  nd     <- setNames(data.frame(x_new), x_var)
  theta  <- coef(fit)                      # free parameters only
  V      <- tryCatch(vcov(fit), error = function(e) NULL)
  if (is.null(V)) return(data.frame(x = x_new, ci_lo = NA_real_, ci_hi = NA_real_))

  p_free <- length(theta)
  rhs    <- as.list(formula(fit))[[3]]     # model formula RHS

  # Baseline predictions via predict() for reliability
  yhat <- tryCatch(
    as.numeric(predict(fit, newdata = nd)),
    error = function(e) rep(NA_real_, n)
  )

  # Reference predictions via formula eval (used as base for finite differences)
  base_env <- c(as.list(theta),
                if (!is.null(fixed_a)) list(a = fixed_a) else list(),
                as.list(nd))
  yhat_ref <- tryCatch(
    as.numeric(eval(rhs, envir = base_env)),
    error = function(e) yhat
  )

  # Jacobian (n x p_free): one vectorised formula eval per free parameter
  # sqrt(.Machine$double.eps) is the near-optimal step for forward differences
  eps <- sqrt(.Machine$double.eps)
  J <- matrix(NA_real_, nrow = n, ncol = p_free)
  for (j in seq_len(p_free)) {
    theta_j    <- theta
    theta_j[j] <- theta[j] + eps
    env_j <- c(as.list(theta_j),
               if (!is.null(fixed_a)) list(a = fixed_a) else list(),
               as.list(nd))
    yhat_j <- tryCatch(
      as.numeric(eval(rhs, envir = env_j)),
      error = function(e) rep(NA_real_, n)
    )
    J[, j] <- (yhat_j - yhat_ref) / eps
  }

  # SE via efficient diag(J V J') = rowSums((J %*% V) * J)
  JV      <- J %*% V
  se_pred <- sqrt(pmax(rowSums(JV * J), 0))

  df_resid <- max(length(residuals(fit)) - p_free, 1)
  t_crit   <- qt((1 + level) / 2, df = df_resid)

  data.frame(
    x     = x_new,
    ci_lo = yhat - t_crit * se_pred,
    ci_hi = yhat + t_crit * se_pred
  )
}

get_plot_data <- function(models_fit_list,
                          prepped_data,
                          fit_params,
                          fixed_a_result,
                          model_names = c("Y5","Yd5","Y4","Yd4","Ygomp4"),
                          x_var = "concentration",
                          y_var = "mfi",
                          verbose = TRUE) {
  # Extract data
  dat <- prepped_data
  dat$x <- dat[[x_var]]
  dat$y <- dat[[y_var]]
  x <- dat[[x_var]]
  y <- dat[[y_var]]

  ## 1. Build prediction data for all models
  # A fine grid over the range of x for smooth curves
  x_new <- seq(min(x, na.rm = TRUE),
               max(x, na.rm = TRUE),
               length.out = 200)

  pred_list <- list()
  resid_list <- list()
  d2xy_list <- list()
  dydx_list <- list()
  ci_list   <- list()

  for (mname in model_names) {
    entry <- models_fit_list[[mname]]
    fit_obj <- if (is.list(entry)) entry$fit else entry

    if (is.null(fit_obj) || !inherits(fit_obj, "nls")) {
      next
    }

    # prediction data.frame must have same column name as x_var
    newdata <- data.frame(x_new)
    names(newdata) <- x_var

    # Predictions + residuals
    y_pred <- tryCatch({
      predict(fit_obj, newdata = newdata)
    }, error = function(e) rep(NA_real_, length(x_new)))

    if (is.null(fixed_a_result)) {
      fit_obj_coef <- coef(fit_obj)
    } else {
      fit_obj_coef <- c(a = fixed_a_result, coef(fit_obj))
    }

    d2x_y <- tryCatch({
      if (mname == "Y5") {
        do.call(d2xY5, c(list(x = x_new), fit_obj_coef))
      } else if (mname == "Y4") {
        do.call(d2xY4, c(list(x = x_new), fit_obj_coef))
      } else if (mname == "Yd5") {
        do.call(d2xYd5, c(list(x = x_new), fit_obj_coef))
      } else if (mname == "Yd4") {
        do.call(d2xYd4, c(list(x = x_new), fit_obj_coef))
      } else if (mname == "Ygomp4") {
        do.call(d2xYgomp4, c(list(x = x_new), fit_obj_coef))
      }
    }, error = function(e) rep (NA_real_, length(x_new)))

    dydx <- tryCatch({
      if (mname == "Y5") {
        do.call(dydxY5, c(list(x = x_new), fit_obj_coef))
      } else if (mname == "Y4") {
        do.call(dydxY4, c(list(x = x_new), fit_obj_coef))
      } else if (mname == "Yd5") {
        do.call(dydxYd5, c(list(x = x_new), fit_obj_coef))
      } else if (mname == "Yd4") {
        do.call(dydxYd4, c(list(x = x_new), fit_obj_coef))
      } else if (mname == "Ygomp4") {
        do.call(dydxYgomp4, c(list(x = x_new), fit_obj_coef))
      }
    }, error = function(e) rep (NA_real_, length(x_new)))

    y_fit <- tryCatch({
      fitted(fit_obj)
    }, error = function(e) rep(NA_real_, length(x)))

    resid <- tryCatch({
      residuals(fit_obj)
    }, error = function(e) rep(NA_real_, length(x)))

    pred_list[[mname]] <- data.frame(
      model = mname,
      x     = x_new,
      yhat  = y_pred
    )

    resid_list[[mname]] <- data.frame(
      model     = mname,
      fitted    = y_fit,
      residuals = resid
    )

    d2xy_list[[mname]] <- data.frame(
      model = mname,
      x = x_new,
      d2x_y = d2x_y
    )

    dydx_list[[mname]] <- data.frame(
      model = mname,
      x = x_new,
      dydx = dydx
    )

    ci_raw <- tryCatch(
      compute_curve_ci(fit_obj, x_new, x_var, fixed_a = fixed_a_result),
      error = function(e) data.frame(x = x_new, ci_lo = NA_real_, ci_hi = NA_real_)
    )
    ci_list[[mname]] <- data.frame(model = mname, ci_raw)
  }


  pred_df  <- do.call(rbind, pred_list)
  pred_df$yhat <- as.numeric(pred_df$yhat)
  resid_df <- do.call(rbind, resid_list)
  d2xy_df  <- do.call(rbind, d2xy_list)
  dydx_df  <- do.call(rbind, dydx_list)
  ci_df    <- do.call(rbind, ci_list)


  fit_summary <- summarize_model_fits(models_fit_list, model_names)
  fit_summary_long <- tidyr::pivot_longer(
    fit_summary[, c("model", "converged", "AIC")],
    cols = "AIC",
    names_to = "criterion",
    values_to = "value"
  )
  fit_summary_long <- as.data.frame(fit_summary_long, stringsAsFactors = FALSE)
  fit_summary_long <- subset(fit_summary_long, converged & is.finite(value))
  names(fit_summary_long)[names(fit_summary_long) == "criterion"] <- "parameter"
  names(fit_summary_long)[names(fit_summary_long) == "value"] <- "estimate"
  fit_summary_long$conf.low <- fit_summary_long$estimate
  fit_summary_long$conf.high <- fit_summary_long$estimate

  fit_params_aic <- rbind(fit_summary_long, fit_params)
  # ── Drop rows where parameter is NA (from non-converged models) ──
  fit_params_aic <- fit_params_aic[!is.na(fit_params_aic$parameter), , drop = FALSE]

  if (verbose) {
    message("Plot Data Completed")
  }
  return(list(dat = dat, pred_df = pred_df,
              resid_df = resid_df,
              fit_params = fit_params,
              d2xy_df = d2xy_df,
              dydx_df = dydx_df,
              ci_df = ci_df,
              fit_params_aic = fit_params_aic))
}

plot_model_comparisons <- function(plot_data,
                                   model_names = c("Y5","Yd5","Y4","Yd4","Ygomp4"),
                                   x_var = "concentration",
                                   y_var = "mfi",
                                   is_display_log_response = TRUE,
                                   is_display_log_independent = TRUE,
                                   use_patchwork = TRUE) {
  ## 1.Extract data
  pred_df <- plot_data$pred_df
  resid_df <- plot_data$resid_df
  fit_params_df <- plot_data$fit_params
  fit_params_aic <- plot_data$fit_params_aic
  dat <- plot_data$dat
  x <- dat[[x_var]]
  y <- dat[[y_var]]

 ## 2. format names of cols
 x_var <- format_assay_terms(x_var)
 y_var <- format_assay_terms(y_var)
 if (is_display_log_independent) {
   x_var <- bquote(log[10]~.(x_var))

 } else {
   x_var <- x_var
 }
 if(is_display_log_response) {
   y_var <- bquote(log[10]~.(y_var))

 } else {
   y_var <- y_var
 }


  ## 2. Plot: data + fitted curves
  p_data_fit <- ggplot(dat[ , c("x","y")], aes(x = x, y = y)) +
    geom_point(alpha = 0.7) +
    geom_line(data = pred_df, aes(x = x, y = yhat, color = model, group = model)) +
    labs(title = "Observed data with fitted curves",
         x = x_var,
         y = y_var,
         color = "Model") +
    theme_bw()

  ## 3. Residual vs fitted
  p_resid <- ggplot(resid_df,
                    aes(x = fitted, y = residuals, color = model)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(alpha = 0.6) +
    labs(title = "Residuals vs Fitted",
         x = "Fitted values",
         y = "Residuals",
         color = "Model") +
    theme_bw()

  # ## 4. QQ plot of residuals
  # # Make a combined QQ plot by model (facets)
  # p_qq <- ggplot(resid_df, aes(sample = residuals)) +
  #   stat_qq(alpha = 0.6) +
  #   stat_qq_line() +
  #   facet_wrap(~ model, scales = "free") +
  #   labs(title = "Normal Q-Q plots of residuals") +
  #   theme_bw()



  # ── Drop any rows with NA parameter to prevent phantom facet panels ──
  fit_params_aic <- fit_params_aic[!is.na(fit_params_aic$parameter), , drop = FALSE]

  p_ci_params <- ggplot(fit_params_aic) +
    geom_point(aes(as.factor(model), estimate), color = "black") +
    facet_wrap(~ parameter, scale = 'free_x', ncol = 6) +
    geom_linerange(aes(as.factor(model), ymin = conf.low, ymax = conf.high)) +
    coord_flip() +
    scale_y_continuous(
      breaks = function(x) pretty(x, n = 3),
      expand = expansion(mult = c(0.1, 0.2))
    ) +
    # scale_color_manual(values = c('green4', 'black')) +
    theme_bw(base_size = 12) +
    theme(legend.position = 'top') +
    xlab('Model') +
    ylab('Parameter Estimate')

  ## 5. Optional: AIC/BIC barplot
  # fit_summary <- summarize_model_fits(models_fit_list, model_names)
  # fit_summary_long <- tidyr::pivot_longer(
  #   fit_summary[, c("model", "converged", "AIC", "BIC")],
  #   cols = c("AIC", "BIC"),
  #   names_to = "criterion",
  #   values_to = "value"
  # )

  # p_info <- ggplot(
  #   subset(fit_summary_long, converged & is.finite(value)),
  #   aes(x = model, y = value, fill = criterion)
  # ) +
  #   geom_col(position = position_dodge(width = 0.7)) +
  #   labs(title = "Information criteria (lower is better)",
  #        x = "Model",
  #        y = "Criterion value") +
  #   theme_bw()

  if (!use_patchwork) {
    # Return a list of plots so caller can arrange as desired
    return(list(
      data_fit = p_data_fit,
      resid    = p_resid,
      p_ci_params = p_ci_params
      # info     = p_info
    ))
  }

  # Use patchwork for a single panel display
  # install.packages("patchwork") if needed
  library(patchwork)

  # Arrange: top row = data+fits, info; bottom row = resid, qq
  combined <- (p_data_fit) /
    (p_resid)/
    (p_ci_params) +
    plot_annotation(title = paste("Comparision of Model Fits for", unique(dat$antigen), "on", unique(dat$plate_nom)), tag_levels = "A",
                    theme = theme(
                      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                      plot.tag   = element_text(size = 12, face = "bold")
                    ))

  combined
}


# format terms such as mfi, absorbance, and concentration for display
format_assay_terms <- function(x) {
  lookup <- c(
    MFI = "MFI",
    Absorbance = "Absorbance",
    Fluorescence = "Fluorescence",
    OD = "OD",
    Concentration = "Concentration"
  )

  # normalize lookup keys for matching
  names(lookup) <- tolower(names(lookup))
  
  sapply(x, function(v) {
    key <- trimws(tolower(v))
    if (key %in% names(lookup)) lookup[[key]] else v
  }, USE.NAMES = FALSE)
}



# # best fit must contain sample_se
# plot_standard_curve <- function(best_fit,
#                                 is_display_log_response,
#                                 is_display_log_independent,
#                                 pcov_threshold,
#                                 response_variable = "mfi",
#                                 independent_variable = "concentration",
#                                 mcmc_samples = NULL,
#                                 mcmc_pred = NULL) {
#   p <- plotly::plot_ly()
#   best_fit_v <<- best_fit
#   mcmc_samples_in <<- mcmc_samples
#   mcmc_pred_in <<- mcmc_pred
#   
#   # ── Resolve response column ────────────────────────────────────────
#   resolved <- ensure_response_column(
#     df           = best_fit$best_data,
#     response_var = response_variable,
#     coerce_numeric = TRUE,
#     context      = "plot_standard_curve/best_data"
#   )
#   best_fit$best_data <- resolved$df
#   response_variable  <- resolved$response_var
#   
#   if (!resolved$ok) {
#     return(
#       plotly::plot_ly() %>%
#         plotly::layout(
#           title = "Cannot plot: response variable not found",
#           annotations = list(
#             text = paste0(
#               "Column '", response_variable,
#               "' not found or has no finite values in standard data.<br>",
#               "Available columns: ",
#               paste(names(best_fit$best_data), collapse = ", ")
#             ),
#             xref = "paper", yref = "paper",
#             x = 0.5, y = 0.5, showarrow = FALSE
#           )
#         )
#     )
#   }
#   
#   # ── Resolve independent variable ───────────────────────────────────
#   if (!independent_variable %in% names(best_fit$best_data)) {
#     if ("concentration" %in% names(best_fit$best_data)) {
#       independent_variable <- "concentration"
#     } else {
#       return(plotly::plot_ly() %>%
#                plotly::layout(title = "Missing independent variable column"))
#     }
#   }
#   
#   # ── Ensure stype exists ────────────────────────────────────────────
#   if (!"stype" %in% names(best_fit$best_data)) {
#     best_fit$best_data$stype <- "S"
#   }
#   
#   # ── Resolve response column in sample_se too ───────────────────────
#   samples_predicted_conc <- best_fit$sample_se
#   if (!is.null(samples_predicted_conc) && nrow(samples_predicted_conc) > 0) {
#     samp_resolved <- ensure_response_column(
#       df           = samples_predicted_conc,
#       response_var = response_variable,
#       coerce_numeric = TRUE,
#       context      = "plot_standard_curve/sample_se"
#     )
#     samples_predicted_conc <- samp_resolved$df
#     if (samp_resolved$ok && samp_resolved$response_var != response_variable) {
#       samples_predicted_conc[[response_variable]] <-
#         samples_predicted_conc[[samp_resolved$response_var]]
#     }
#     samples_predicted_conc <- samples_predicted_conc[
#       !is.nan(samples_predicted_conc$raw_predicted_concentration) &
#         is.finite(samples_predicted_conc$raw_predicted_concentration), ,
#       drop = FALSE
#     ]
#   } else {
#     samples_predicted_conc <- data.frame(
#       raw_predicted_concentration = numeric(0),
#       pcov = numeric(0),
#       stringsAsFactors = FALSE
#     )
#     samples_predicted_conc[[response_variable]] <- numeric(0)
#   }
#   
#   best_fit$best_pred$pcov_threshold <- pcov_threshold
#   
#   safe_glance <- function(field, default = NA_real_) {
#     val <- best_fit$best_glance[[field]]
#     if (is.null(val) || length(val) == 0) return(default)
#     val <- unlist(val)
#     if (all(is.na(val))) return(default)
#     val[1]
#   }
#   
#   ### 1. RESPONSE VARIABLE (Y) log transform
#   log_response_status <- isTRUE(as.logical(safe_glance("is_log_response", FALSE)))
#   if (log_response_status && !isTRUE(is_display_log_response)) {
#     best_fit$best_data[[response_variable]] <- 10^best_fit$best_data[[response_variable]]
#     best_fit$best_pred$yhat               <- 10^best_fit$best_pred$yhat
#     best_fit$best_glance$llod             <- 10^safe_glance("llod")
#     best_fit$best_glance$ulod             <- 10^safe_glance("ulod")
#     best_fit$best_glance$lloq_y           <- 10^safe_glance("lloq_y")
#     best_fit$best_glance$uloq_y           <- 10^safe_glance("uloq_y")
#     best_fit$best_glance$inflect_y        <- 10^safe_glance("inflect_y")
#     best_fit$best_d2xy$d2x_y              <- 10^best_fit$best_d2xy$d2x_y
#     if (!is.null(best_fit$best_curve_ci)) {
#       best_fit$best_curve_ci$ci_lo        <- 10^best_fit$best_curve_ci$ci_lo
#       best_fit$best_curve_ci$ci_hi        <- 10^best_fit$best_curve_ci$ci_hi
#     }
#     if (nrow(samples_predicted_conc) > 0 &&
#         response_variable %in% names(samples_predicted_conc)) {
#       samples_predicted_conc[[response_variable]] <-
#         10^samples_predicted_conc[[response_variable]]
#     }
#   }
#   
#   ### 2. INDEPENDENT VARIABLE (X) log transform
#   log_x_status <- isTRUE(as.logical(safe_glance("is_log_x", FALSE)))
#   if (log_x_status && !isTRUE(is_display_log_independent)) {
#     best_fit$best_data$concentration       <- 10^best_fit$best_data$concentration
#     best_fit$best_pred$x                   <- 10^best_fit$best_pred$x
#     best_fit$best_glance$lloq              <- 10^safe_glance("lloq")
#     best_fit$best_glance$uloq              <- 10^safe_glance("uloq")
#     best_fit$best_glance$inflect_x         <- 10^safe_glance("inflect_x")
#     best_fit$best_d2xy$x                   <- 10^best_fit$best_d2xy$x
#     if (nrow(samples_predicted_conc) > 0) {
#       samples_predicted_conc$raw_predicted_concentration <-
#         10^samples_predicted_conc$raw_predicted_concentration
#     }
#     best_fit$best_glance$mindc             <- 10^safe_glance("mindc")
#     best_fit$best_glance$maxdc             <- 10^safe_glance("maxdc")
#     best_fit$best_glance$minrdl            <- 10^safe_glance("minrdl")
#     best_fit$best_glance$maxrdl            <- 10^safe_glance("maxrdl")
#     if (!is.null(best_fit$best_curve_ci)) {
#       best_fit$best_curve_ci$x             <- 10^best_fit$best_curve_ci$x
#     }
#   }
#   
#   y3_label <- "Precision Coefficient of Variation (pCoV %)"
#   
#   if (is_display_log_response) {
#     y_label <- paste("log<sub>10</sub>", format_assay_terms(response_variable))
#   } else {
#     y_label <- format_assay_terms(response_variable)
#   }
#   
#   if (is_display_log_independent) {
#     x_label <- paste("log<sub>10</sub>", format_assay_terms(independent_variable))
#   } else {
#     x_label <- format_assay_terms(independent_variable)
#   }
#   
#   ### 3. MODEL NAME
#   model_name <- best_fit$best_model_name
#   title_model_name <- switch(
#     model_name,
#     "Y4" = "4-parameter Logistic",
#     "Yd4" = "4-parameter Log-Logistic",
#     "Ygomp4" = "4-parameter Gompertz type",
#     "Y5" = "5-parameter Logistic",
#     "Yd5" = "5-parameter Log-Logistic",
#     model_name
#   )
#   
#   ## 3b. PREPARE SAMPLE-UNCERTAINTY
#   print(names(best_fit$best_pred))
#   se_model   <- best_fit$best_pred$pcov
#   se_samples <- samples_predicted_conc$pcov
#   se_all     <- c(best_fit$best_pred$pcov, samples_predicted_conc$pcov)
#   se_range   <- range(se_all, na.rm = TRUE)
#   
#   ### 4. RAW POINTS
#   plot_std <- best_fit$best_data
#   
#   if (is_display_log_independent) {
#     glance_fda_lloq_conc <- log10(best_fit$best_glance$lloq_fda2018_concentration)
#     glance_fda_2018_uloq_conc <- log10(best_fit$best_glance$uloq_fda2018_concentration)
#   } else {
#     glance_fda_lloq_conc <- best_fit$best_glance$lloq_fda2018_concentration
#     glance_fda_2018_uloq_conc <- best_fit$best_glance$uloq_fda2018_concentration
#   }
#   
#   plot_std$fda2018_class <- ifelse(
#     plot_std[[independent_variable]] >= glance_fda_lloq_conc &
#       plot_std[[independent_variable]] <= glance_fda_2018_uloq_conc,
#     "Standards (+ FDA 2018)",
#     "Standards (- FDA 2018)"
#   )
#   
#   std_in <- plot_std[
#     plot_std$stype == "S" &
#       plot_std$fda2018_class == "Standards (+ FDA 2018)", ]
#   std_out <- plot_std[
#     plot_std$stype == "S" &
#       plot_std$fda2018_class == "Standards (- FDA 2018)", ]
#   blanks <- plot_std[plot_std$stype == "B", ]
#   
#   ### Standards inside FDA range (circle)
#   p <- p %>% plotly::add_trace(
#     data = std_in,
#     x = std_in[[independent_variable]],
#     y = std_in[[response_variable]],
#     type = "scatter",
#     mode = "markers",
#     name = "Standards (+ FDA 2018)",
#     legendgroup = "standards",
#     marker = list(color = "#2b3d26", symbol = "circle"),
#     text = ~paste0(
#       "<br>", format_assay_terms(independent_variable), ": ",
#       std_in[[independent_variable]],
#       "<br>Dilution Factor: ", dilution,
#       "<br>", format_assay_terms(response_variable), ": ",
#       std_in[[response_variable]],
#       "<br> FDA 2018 Status: ", gsub("Standards ", "", std_in$fda2018_class)
#     ),
#     hoverinfo = "text"
#   )
#   
#   ### Standards outside FDA range (triangle)
#   p <- p %>% plotly::add_trace(
#     data = std_out,
#     x = std_out[[independent_variable]],
#     y = std_out[[response_variable]],
#     type = "scatter",
#     mode = "markers",
#     name = "Standards (- FDA 2018)",
#     legendgroup = "standards",
#     marker = list(color = "#2b3d26", symbol = "triangle-up", size = 8),
#     text = ~paste0(
#       "<br>", format_assay_terms(independent_variable), ": ",
#       std_out[[independent_variable]],
#       "<br>Dilution Factor: ", dilution,
#       "<br>", format_assay_terms(response_variable), ": ",
#       std_out[[response_variable]],
#       "<br>FDA 2018 Status: ", gsub("Standards ", "", std_out$fda2018_class)
#     ),
#     hoverinfo = "text"
#   )
#   
#   ### Blanks
#   p <- p %>% plotly::add_trace(
#     data = blanks,
#     x = blanks[[independent_variable]],
#     y = blanks[[response_variable]],
#     type = "scatter",
#     mode = "markers",
#     name = "Geometric Mean of Blanks",
#     marker = list(color = "#c2b280", symbol = "circle"),
#     text = ~paste0(
#       "<br>", format_assay_terms(independent_variable), ": ",
#       blanks[[independent_variable]],
#       "<br>Dilution Factor: ", dilution,
#       "<br>", format_assay_terms(response_variable), ": ",
#       blanks[[response_variable]]
#     ),
#     hoverinfo = "text"
#   )
#   
#   ### 5. FITTED CURVE
#   p <- p %>% add_lines(
#     x = best_fit$best_pred$x,
#     y = best_fit$best_pred$yhat,
#     name = "Fitted Curve",
#     legendgroup = "fitted_curve",
#     showlegend = TRUE,
#     line = list(color = "#2b3d26")
#   )
#   
#   ### 5b. 95% CI BANDS (delta method)
#   if (!is.null(best_fit$best_curve_ci)) {
#     p <- p %>% add_lines(
#       x           = best_fit$best_curve_ci$x,
#       y           = best_fit$best_curve_ci$ci_lo,
#       name        = "95% CI",
#       line        = list(color = "#2b3d26", dash = "dash"),
#       legendgroup = "fitted_curve"
#     ) %>% add_lines(
#       x           = best_fit$best_curve_ci$x,
#       y           = best_fit$best_curve_ci$ci_hi,
#       name        = "",
#       line        = list(color = "#2b3d26", dash = "dash"),
#       legendgroup = "fitted_curve",
#       showlegend  = FALSE
#     )
#   }
#   
#   ### 6. LOD lines (horizontal)
#   p <- p %>% add_lines(
#     x = best_fit$best_pred$x,
#     y = best_fit$best_glance$ulod,
#     name = paste("Upper LOD: (",
#                  round(best_fit$best_glance$maxdc, 3), ",",
#                  round(best_fit$best_glance$ulod, 3), ")"),
#     line = list(color = "#e25822", dash = "dash"),
#     legendgroup = "linked_ulod",
#     visible = "legendonly"
#   )
#   
#   p <- p %>% add_lines(
#     x = best_fit$best_pred$x,
#     y = best_fit$best_glance$llod,
#     name = paste("Lower LOD: (",
#                  round(best_fit$best_glance$mindc, 3), ",",
#                  round(best_fit$best_glance$llod, 3), ")"),
#     line = list(color = "#e25822", dash = "dash"),
#     legendgroup = "linked_llod",
#     visible = "legendonly"
#   )
#   
#   ### 7. LOQ (vertical + horizontal)
#   y_min <- min(best_fit$best_data[[response_variable]], na.rm = TRUE)
#   y_max <- max(best_fit$best_data[[response_variable]], na.rm = TRUE)
#   
#   ### 6b. MDC / RDL vertical lines
#   if (!is.na(best_fit$best_glance$mindc)) {
#     p <- p %>% add_lines(
#       x = c(best_fit$best_glance$mindc, best_fit$best_glance$mindc),
#       y = c(y_min, y_max),
#       name = paste("Lower DC:", round(best_fit$best_glance$mindc, 3)),
#       line = list(color = "#e25822", dash = "dash"),
#       legendgroup = "linked_llod",
#       showlegend = FALSE, hoverinfo = "text", visible = "legendonly"
#     )
#   }
#   
#   if (!is.na(best_fit$best_glance$minrdl)) {
#     p <- p %>% add_lines(
#       x = c(best_fit$best_glance$minrdl, best_fit$best_glance$minrdl),
#       y = c(y_min, y_max),
#       name = paste("Lower RDL:", round(best_fit$best_glance$minrdl, 3)),
#       line = list(color = "#e25822"),
#       legendgroup = "linked_llod",
#       showlegend = TRUE, hoverinfo = "text", visible = "legendonly"
#     )
#   }
#   
#   if (!is.na(best_fit$best_glance$maxdc)) {
#     p <- p %>% add_lines(
#       x = c(best_fit$best_glance$maxdc, best_fit$best_glance$maxdc),
#       y = c(y_min, y_max),
#       name = paste("Upper DC:", round(best_fit$best_glance$maxdc, 3)),
#       line = list(color = "#e25822", dash = "dash"),
#       legendgroup = "linked_ulod",
#       showlegend = FALSE, hoverinfo = "text", visible = "legendonly"
#     )
#   }
#   
#   if (!is.na(best_fit$best_glance$maxrdl)) {
#     p <- p %>% add_lines(
#       x = c(best_fit$best_glance$maxrdl, best_fit$best_glance$maxrdl),
#       y = c(y_min, y_max),
#       name = paste("Upper RDL:", round(best_fit$best_glance$maxrdl, 3)),
#       line = list(color = "#e25822"),
#       legendgroup = "linked_ulod",
#       showlegend = TRUE, hoverinfo = "text", visible = "legendonly"
#     )
#   }
#   
#   ### LLOQ vertical line
#   p <- p %>% add_lines(
#     x = c(best_fit$best_glance$lloq),
#     y = c(y_min, y_max),
#     name = paste("Lower LOQ: (",
#                  round(best_fit$best_glance$lloq, 3), ",",
#                  round(best_fit$best_glance$lloq_y, 3), ")"),
#     line = list(color = "#875692"),
#     legendgroup = "linked_lloq",
#     hoverinfo = "text", visible = "legendonly"
#   )
#   
#   ### ULOQ vertical line
#   p <- p %>% add_lines(
#     x = c(best_fit$best_glance$uloq),
#     y = c(y_min, y_max),
#     name = paste("Upper LOQ: (",
#                  round(best_fit$best_glance$uloq, 3), ",",
#                  round(best_fit$best_glance$uloq_y, 3), ")"),
#     line = list(color = "#875692"),
#     legendgroup = "linked_uloq",
#     hoverinfo = "text", visible = "legendonly"
#   )
#   
#   ### Horizontal LOQ lines
#   p <- p %>% add_lines(
#     x = best_fit$best_pred$x,
#     y = best_fit$best_glance$uloq_y,
#     name = "",
#     legendgroup = "linked_uloq", showlegend = FALSE,
#     line = list(color = "#875692"), visible = "legendonly"
#   )
#   
#   p <- p %>% add_lines(
#     x = best_fit$best_pred$x,
#     y = best_fit$best_glance$lloq_y,
#     name = "",
#     legendgroup = "linked_lloq", showlegend = FALSE,
#     line = list(color = "#875692"), visible = "legendonly"
#   )
#   
#   ### 8a. SECOND DERIVATIVE (y2 axis)
#   p <- p %>% add_lines(
#     x = best_fit$best_d2xy$x,
#     y = best_fit$best_d2xy$d2x_y,
#     name = "2nd Derivative of x given y",
#     yaxis = "y2",
#     line = list(color = "#604e97"),
#     visible = "legendonly"
#   )
#   
#   ### 8b. Sample uncertainty (y3 axis) — interpolated
#   unc_col <- list(color = "#e68fac")
#   p <- p %>% add_lines(
#     x = best_fit$best_pred$x,
#     y = best_fit$best_pred$pcov,
#     name = "Measurement Uncertainty",
#     yaxis = "y3",
#     line = unc_col,
#     legendgroup = "linked_uncertainty",
#     visible = "legendonly"
#   ) %>% add_trace(
#     data = samples_predicted_conc,
#     x = ~raw_predicted_concentration,
#     y = ~pcov,
#     type = "scatter",
#     mode = "markers",
#     name = "",
#     marker = list(color = "#800032", symbol = "circle"),
#     text = ~paste("Predicted", x_label, ":", raw_predicted_concentration,
#                   "<br>Coefficient of Variation (pCoV):", round(pcov, 2), "%"),
#     yaxis = "y3",
#     legendgroup = "linked_uncertainty",
#     showlegend = FALSE,
#     hovertemplate = "%{text}<extra></extra>",
#     visible = "legendonly"
#   ) %>% add_lines(
#     x = best_fit$best_pred$x,
#     y = best_fit$best_pred$pcov_threshold,
#     name = paste0("pCoV Threshold: ", best_fit$best_pred$pcov_threshold, "%"),
#     yaxis = "y3",
#     line = list(color = "#e68fac", dash = "dash"),
#     legendgroup = "linked_uncertainty",
#     visible = "legendonly"
#   )
#   
#   ### 8c. MCMC pCoV uncertainty smooth line (y3 axis) — dense pred grid
#   if (!is.null(mcmc_pred) && nrow(mcmc_pred) > 0 &&
#       "pcov_robust_concentration" %in% names(mcmc_pred) &&
#       "raw_robust_concentration" %in% names(mcmc_pred)) {
#     
#     pred_valid <- is.finite(mcmc_pred$pcov_robust_concentration) &
#       is.finite(mcmc_pred$raw_robust_concentration)
#     
#     if (any(pred_valid)) {
#       mcmc_pred_x    <- mcmc_pred$raw_robust_concentration[pred_valid]
#       mcmc_pred_pcov <- mcmc_pred$pcov_robust_concentration[pred_valid]
#       
#       # Apply same x-axis transform as all other traces (Section 2)
#       if (log_x_status && !isTRUE(is_display_log_independent)) {
#         mcmc_pred_x <- 10^mcmc_pred_x
#       }
#       
#       # Sort by x for smooth line
#       sort_idx       <- order(mcmc_pred_x)
#       mcmc_pred_x    <- mcmc_pred_x[sort_idx]
#       mcmc_pred_pcov <- mcmc_pred_pcov[sort_idx]
#       
#       p <- p %>% plotly::add_lines(
#         x = mcmc_pred_x,
#         y = mcmc_pred_pcov,
#         name = "MCMC Uncertainty",
#         yaxis = "y3",
#         line = list(color = "#800032"),
#         legendgroup = "linked_uncertainty",
#         showlegend = TRUE,
#         visible = "legendonly"
#       )
#     }
#   }
#   
#   ### 9a. SAMPLES
#   p <- p %>% add_trace(
#     data = samples_predicted_conc,
#     x = ~raw_predicted_concentration,
#     y = samples_predicted_conc[[response_variable]],
#     type = "scatter",
#     mode = "markers",
#     name = "Samples",
#     marker = list(color = "#d1992a", symbol = "circle"),
#     text = ~paste("Predicted", x_label, ":", raw_predicted_concentration,
#                   "<br>", y_label, ":", samples_predicted_conc[[response_variable]],
#                   "<br>Patient ID:", patientid,
#                   "<br> Timepoint:", timeperiod,
#                   "<br>Well:", well,
#                   "<br>LOQ Gate Class:", samples_predicted_conc$gate_class_loq,
#                   "<br>LOD Gate Class:", samples_predicted_conc$gate_class_lod,
#                   "<br> PCOV Gate Class:", samples_predicted_conc$gate_class_pcov),
#     hovertemplate = "%{text}<extra></extra>"
#   )
#   
#   ### 9b. MCMC ROBUST SAMPLES
#   if (!is.null(mcmc_samples) && nrow(mcmc_samples) > 0) {
#     # Map assay_response to the plot's response variable if needed
#     if ("assay_response" %in% names(mcmc_samples) &&
#         !response_variable %in% names(mcmc_samples)) {
#       mcmc_samples[[response_variable]] <- mcmc_samples$assay_response
#     }
#     
#     mcmc_x <- mcmc_samples$raw_robust_concentration
#     mcmc_y <- mcmc_samples[[response_variable]]
#     
#     # Apply same x-axis transform as all other traces (Section 2)
#     if (log_x_status && !isTRUE(is_display_log_independent)) {
#       mcmc_x <- 10^mcmc_x
#     }
#     
#     # Build hover text
#     mcmc_hover <- paste0(
#       "MCMC Robust Concentration: ", round(mcmc_x, 4),
#       "<br>", format_assay_terms(response_variable), ": ", round(mcmc_y, 2),
#       if ("patientid" %in% names(mcmc_samples))
#         paste0("<br>Patient ID: ", mcmc_samples$patientid) else "",
#       if ("timeperiod" %in% names(mcmc_samples))
#         paste0("<br>Timepoint: ", mcmc_samples$timeperiod) else "",
#       if ("well" %in% names(mcmc_samples))
#         paste0("<br>Well: ", mcmc_samples$well) else "",
#       if ("pcov_robust_concentration" %in% names(mcmc_samples))
#         paste0("<br>MCMC pCoV: ",
#                round(mcmc_samples$pcov_robust_concentration, 2), "%") else "",
#       if ("gate_class_loq" %in% names(mcmc_samples))
#         paste0("<br>LOQ Gate Class: ", mcmc_samples$gate_class_loq) else "",
#       if ("gate_class_lod" %in% names(mcmc_samples))
#         paste0("<br>LOD Gate Class: ", mcmc_samples$gate_class_lod) else ""
#     )
#     
#     p <- p %>% plotly::add_trace(
#       x = mcmc_x,
#       y = mcmc_y,
#       type = "scatter",
#       mode = "markers",
#       name = "MCMC Samples",
#       marker = list(
#         color = "#d1992a",
#         symbol = "diamond",
#         size = 7,
#         opacity = 0.8
#       ),
#       text = mcmc_hover,
#       hovertemplate = "%{text}<extra></extra>"
#     )
#     
#     ### 9c. MCMC pCoV scatter points at sample locations (y3 axis)
#     if ("pcov_robust_concentration" %in% names(mcmc_samples)) {
#       pcov_valid <- is.finite(mcmc_samples$pcov_robust_concentration) &
#         is.finite(mcmc_x)
#       
#       if (any(pcov_valid)) {
#         p <- p %>% plotly::add_trace(
#           x = mcmc_x[pcov_valid],
#           y = mcmc_samples$pcov_robust_concentration[pcov_valid],
#           type = "scatter",
#           mode = "markers",
#           name = "",
#           marker = list(color = "#800032", symbol = "diamond", size = 5),
#           text = paste0(
#             "MCMC ", x_label, ": ", round(mcmc_x[pcov_valid], 4),
#             "<br>MCMC pCoV: ",
#             round(mcmc_samples$pcov_robust_concentration[pcov_valid], 2), "%"
#           ),
#           yaxis = "y3",
#           legendgroup = "linked_uncertainty",
#           showlegend = FALSE,
#           hovertemplate = "%{text}<extra></extra>",
#           visible = "legendonly"
#         )
#       }
#     }
#   }
#   
#   ### 10. INFLECTION POINT
#   p <- p %>% add_trace(
#     x = best_fit$best_glance$inflect_x,
#     y = best_fit$best_glance$inflect_y,
#     type = "scatter",
#     mode = "markers",
#     name = paste("Inflection Point: (",
#                  round(best_fit$best_glance$inflect_x, 3), ",",
#                  round(best_fit$best_glance$inflect_y, 3), ")"),
#     legendgroup = "fitted_curve",
#     showlegend = TRUE,
#     marker = list(color = "#2724F0", size = 8)
#   )
#   
#   ### 10b. COMPUTE AXIS RANGES ─────────────────────────────────────────
#   # Collect all x-values from every trace
#   all_x_values <- c(
#     best_fit$best_data[[independent_variable]],
#     best_fit$best_pred$x,
#     samples_predicted_conc$raw_predicted_concentration
#   )
#   
#   # Include MCMC sample x-values if present
#   mcmc_x_all <- NULL
#   if (!is.null(mcmc_samples) && nrow(mcmc_samples) > 0 &&
#       "raw_robust_concentration" %in% names(mcmc_samples)) {
#     mcmc_x_for_range <- mcmc_samples$raw_robust_concentration
#     if (log_x_status && !isTRUE(is_display_log_independent)) {
#       mcmc_x_for_range <- 10^mcmc_x_for_range
#     }
#     mcmc_x_all <- c(mcmc_x_all, mcmc_x_for_range)
#   }
#   
#   # Include MCMC pred x-values if present
#   if (!is.null(mcmc_pred) && nrow(mcmc_pred) > 0 &&
#       "raw_robust_concentration" %in% names(mcmc_pred)) {
#     mcmc_pred_x_for_range <- mcmc_pred$raw_robust_concentration[
#       is.finite(mcmc_pred$raw_robust_concentration)
#     ]
#     if (log_x_status && !isTRUE(is_display_log_independent)) {
#       mcmc_pred_x_for_range <- 10^mcmc_pred_x_for_range
#     }
#     mcmc_x_all <- c(mcmc_x_all, mcmc_pred_x_for_range)
#   }
#   
#   all_x_values <- c(all_x_values, mcmc_x_all)
#   all_x_values <- all_x_values[is.finite(all_x_values)]
#   
#   if (length(all_x_values) > 0) {
#     # Use the TRUE minimum so points near 0 are visible
#     # Use 95th percentile for upper end to avoid extreme outlier stretch
#     x_min <- min(all_x_values, na.rm = TRUE)
#     x_p95 <- quantile(all_x_values, probs = 0.95, na.rm = TRUE)
#     
#     # Extend to include MCMC points beyond p95 with a cap
#     x_max_mcmc <- if (!is.null(mcmc_x_all) && length(mcmc_x_all) > 0) {
#       max(mcmc_x_all[is.finite(mcmc_x_all)], na.rm = TRUE)
#     } else {
#       x_p95
#     }
#     
#     # Cap at 2x the p95 to prevent single extreme outlier from dominating
#     x_upper <- min(x_max_mcmc, x_p95 * 2)
#     
#     x_padding    <- (x_upper - x_min) * 0.05
#     x_axis_range <- c(x_min - x_padding, x_upper + x_padding)
#   } else {
#     x_axis_range <- NULL
#   }
#   
#   # Dynamically compute y3 (pCoV) axis range including MCMC values
#   se_all_combined <- c(
#     best_fit$best_pred$pcov,
#     samples_predicted_conc$pcov
#   )
#   if (!is.null(mcmc_pred) && nrow(mcmc_pred) > 0 &&
#       "pcov_robust_concentration" %in% names(mcmc_pred)) {
#     se_all_combined <- c(se_all_combined,
#                          mcmc_pred$pcov_robust_concentration)
#   }
#   if (!is.null(mcmc_samples) && nrow(mcmc_samples) > 0 &&
#       "pcov_robust_concentration" %in% names(mcmc_samples)) {
#     se_all_combined <- c(se_all_combined,
#                          mcmc_samples$pcov_robust_concentration)
#   }
#   se_all_combined <- se_all_combined[is.finite(se_all_combined)]
#   
#   # Start y3 axis at 0 so low-pCoV dots are visible
#   se_min <- 0
#   
#   se_max <- if (length(se_all_combined) > 0) {
#     ceiling(max(se_all_combined, na.rm = TRUE))
#   } else {
#     125
#   }
#   # Ensure at least 125 so the axis isn't too cramped
#   se_max <- max(se_max, 125)
#   
#   se_axis_limits <- c(se_min, se_max * 1.05)
#   dtick <- ifelse(se_max > 100, 20,
#                   ifelse(se_max > 35, 10,
#                          ifelse(se_max > 19, 5, 1)))
#   
#   ### 11. LAYOUT
#   p <- p %>% layout(
#     title = paste(
#       "Fitted", title_model_name, "Model (",
#       unique(best_fit$best_data$plate), ",",
#       unique(best_fit$best_data$antigen), ")"
#     ),
#     xaxis = list(
#       title    = x_label,
#       range    = x_axis_range,
#       showgrid = TRUE,
#       zeroline = FALSE
#     ),
#     yaxis = list(
#       title    = y_label,
#       showgrid = TRUE,
#       zeroline = TRUE
#     ),
#     legend = list(
#       x       = 1.1,
#       y       = 1,
#       xanchor = "left"
#     ),
#     font = list(size = 12),
#     yaxis2 = list(
#       showticklabels = FALSE,
#       title          = "",
#       tickmode       = "linear",
#       dtick          = 10,
#       overlaying     = "y",
#       side           = "right",
#       showgrid       = FALSE,
#       zeroline       = FALSE
#     ),
#     yaxis3 = list(
#       overlaying     = "y",
#       side           = "right",
#       title          = y3_label,
#       range          = se_axis_limits,
#       tickmode       = "linear",
#       type           = "linear",
#       dtick          = dtick,
#       showgrid       = FALSE,
#       zeroline       = FALSE,
#       showticklabels = TRUE
#     )
#   )
#   
#   return(p)
# }

plot_standard_curve <- function(best_fit,
                                is_display_log_response,
                                is_display_log_independent,
                                pcov_threshold,
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
    val <- best_fit$best_glance[[field]]
    if (is.null(val) || length(val) == 0) return(default)
    val <- unlist(val)
    if (all(is.na(val))) return(default)
    val[1]
  }
  
  ### 1. RESPONSE VARIABLE (Y) log transform
  log_response_status <- isTRUE(as.logical(safe_glance("is_log_response", FALSE)))
  if (log_response_status && !isTRUE(is_display_log_response)) {
    best_fit$best_data[[response_variable]] <- 10^best_fit$best_data[[response_variable]]
    best_fit$best_pred$yhat               <- 10^best_fit$best_pred$yhat
    best_fit$best_glance$llod             <- 10^safe_glance("llod")
    best_fit$best_glance$ulod             <- 10^safe_glance("ulod")
    best_fit$best_glance$lloq_y           <- 10^safe_glance("lloq_y")
    best_fit$best_glance$uloq_y           <- 10^safe_glance("uloq_y")
    best_fit$best_glance$inflect_y        <- 10^safe_glance("inflect_y")
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
    best_fit$best_pred$x                   <- 10^best_fit$best_pred$x
    best_fit$best_glance$lloq              <- 10^safe_glance("lloq")
    best_fit$best_glance$uloq              <- 10^safe_glance("uloq")
    best_fit$best_glance$inflect_x         <- 10^safe_glance("inflect_x")
    best_fit$best_d2xy$x                   <- 10^best_fit$best_d2xy$x
    if (nrow(samples_predicted_conc) > 0) {
      samples_predicted_conc$raw_predicted_concentration <-
        10^samples_predicted_conc$raw_predicted_concentration
    }
    best_fit$best_glance$mindc             <- 10^safe_glance("mindc")
    best_fit$best_glance$maxdc             <- 10^safe_glance("maxdc")
    best_fit$best_glance$minrdl            <- 10^safe_glance("minrdl")
    best_fit$best_glance$maxrdl            <- 10^safe_glance("maxrdl")
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
    "Y4" = "4-parameter Logistic",
    "Yd4" = "4-parameter Log-Logistic",
    "Ygomp4" = "4-parameter Gompertz type",
    "Y5" = "5-parameter Logistic",
    "Yd5" = "5-parameter Log-Logistic",
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
    glance_fda_lloq_conc <- log10(best_fit$best_glance$lloq_fda2018_concentration)
    glance_fda_2018_uloq_conc <- log10(best_fit$best_glance$uloq_fda2018_concentration)
  } else {
    glance_fda_lloq_conc <- best_fit$best_glance$lloq_fda2018_concentration
    glance_fda_2018_uloq_conc <- best_fit$best_glance$uloq_fda2018_concentration
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
    x = best_fit$best_pred$x,
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
    x = best_fit$best_pred$x,
    y = best_fit$best_glance$ulod,
    name = paste("Upper LOD: (",
                 round(best_fit$best_glance$maxdc, 3), ",",
                 round(best_fit$best_glance$ulod, 3), ")"),
    line = list(color = "#e25822", dash = "dash"),
    legendgroup = "linked_ulod",
    visible = "legendonly"
  )
  
  p <- p %>% add_lines(
    x = best_fit$best_pred$x,
    y = best_fit$best_glance$llod,
    name = paste("Lower LOD: (",
                 round(best_fit$best_glance$mindc, 3), ",",
                 round(best_fit$best_glance$llod, 3), ")"),
    line = list(color = "#e25822", dash = "dash"),
    legendgroup = "linked_llod",
    visible = "legendonly"
  )
  
  ### 7. LOQ (vertical + horizontal)
  y_min <- min(best_fit$best_data[[response_variable]], na.rm = TRUE)
  y_max <- max(best_fit$best_data[[response_variable]], na.rm = TRUE)
  
  ### 6b. MDC / RDL vertical lines
  if (!is.na(best_fit$best_glance$mindc)) {
    p <- p %>% add_lines(
      x = c(best_fit$best_glance$mindc, best_fit$best_glance$mindc),
      y = c(y_min, y_max),
      name = paste("Lower DC:", round(best_fit$best_glance$mindc, 3)),
      line = list(color = "#e25822", dash = "dash"),
      legendgroup = "linked_llod",
      showlegend = FALSE, hoverinfo = "text", visible = "legendonly"
    )
  }
  
  if (!is.na(best_fit$best_glance$minrdl)) {
    p <- p %>% add_lines(
      x = c(best_fit$best_glance$minrdl, best_fit$best_glance$minrdl),
      y = c(y_min, y_max),
      name = paste("Lower RDL:", round(best_fit$best_glance$minrdl, 3)),
      line = list(color = "#e25822"),
      legendgroup = "linked_llod",
      showlegend = TRUE, hoverinfo = "text", visible = "legendonly"
    )
  }
  
  if (!is.na(best_fit$best_glance$maxdc)) {
    p <- p %>% add_lines(
      x = c(best_fit$best_glance$maxdc, best_fit$best_glance$maxdc),
      y = c(y_min, y_max),
      name = paste("Upper DC:", round(best_fit$best_glance$maxdc, 3)),
      line = list(color = "#e25822", dash = "dash"),
      legendgroup = "linked_ulod",
      showlegend = FALSE, hoverinfo = "text", visible = "legendonly"
    )
  }
  
  if (!is.na(best_fit$best_glance$maxrdl)) {
    p <- p %>% add_lines(
      x = c(best_fit$best_glance$maxrdl, best_fit$best_glance$maxrdl),
      y = c(y_min, y_max),
      name = paste("Upper RDL:", round(best_fit$best_glance$maxrdl, 3)),
      line = list(color = "#e25822"),
      legendgroup = "linked_ulod",
      showlegend = TRUE, hoverinfo = "text", visible = "legendonly"
    )
  }
  
  ### LLOQ vertical line
  p <- p %>% add_lines(
    x = c(best_fit$best_glance$lloq),
    y = c(y_min, y_max),
    name = paste("Lower LOQ: (",
                 round(best_fit$best_glance$lloq, 3), ",",
                 round(best_fit$best_glance$lloq_y, 3), ")"),
    line = list(color = "#875692"),
    legendgroup = "linked_lloq",
    hoverinfo = "text", visible = "legendonly"
  )
  
  ### ULOQ vertical line
  p <- p %>% add_lines(
    x = c(best_fit$best_glance$uloq),
    y = c(y_min, y_max),
    name = paste("Upper LOQ: (",
                 round(best_fit$best_glance$uloq, 3), ",",
                 round(best_fit$best_glance$uloq_y, 3), ")"),
    line = list(color = "#875692"),
    legendgroup = "linked_uloq",
    hoverinfo = "text", visible = "legendonly"
  )
  
  ### Horizontal LOQ lines
  p <- p %>% add_lines(
    x = best_fit$best_pred$x,
    y = best_fit$best_glance$uloq_y,
    name = "",
    legendgroup = "linked_uloq", showlegend = FALSE,
    line = list(color = "#875692"), visible = "legendonly"
  )
  
  p <- p %>% add_lines(
    x = best_fit$best_pred$x,
    y = best_fit$best_glance$lloq_y,
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
    x = best_fit$best_pred$x,
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
    x = best_fit$best_pred$x,
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
    x = best_fit$best_glance$inflect_x,
    y = best_fit$best_glance$inflect_y,
    type = "scatter",
    mode = "markers",
    name = paste("Inflection Point: (",
                 round(best_fit$best_glance$inflect_x, 3), ",",
                 round(best_fit$best_glance$inflect_y, 3), ")"),
    legendgroup = "fitted_curve",
    showlegend = TRUE,
    marker = list(color = "#2724F0", size = 8)
  )
  
  ### 11. LAYOUT
  p <- p %>% layout(
    title = paste(
      "Fitted", title_model_name, "Model (",
      unique(best_fit$best_data$plate), ",",
      unique(best_fit$best_data$antigen), ")"
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
# plot_standard_curve <- function(best_fit,
#                                 is_display_log_response,
#                                 is_display_log_independent,
#                                 pcov_threshold,
#                                 response_variable = "mfi",
#                                 independent_variable = "concentration",
#                                 mcmc_samples = NULL,
#                                 mcmc_pred = NULL) {
#   p <- plotly::plot_ly()
#   best_fit_v <<- best_fit
#   mcmc_samples_in <<- mcmc_samples
#   mcmc_pred_in <<- mcmc_pred
#   # ── Resolve response column ────────────────────────────────────────
#   resolved <- ensure_response_column(
#     df           = best_fit$best_data,
#     response_var = response_variable,
#     coerce_numeric = TRUE,
#     context      = "plot_standard_curve/best_data"
#   )
#   best_fit$best_data <- resolved$df
#   response_variable  <- resolved$response_var
# 
#   if (!resolved$ok) {
#     return(
#       plotly::plot_ly() %>%
#         plotly::layout(
#           title = "Cannot plot: response variable not found",
#           annotations = list(
#             text = paste0(
#               "Column '", response_variable,
#               "' not found or has no finite values in standard data.<br>",
#               "Available columns: ",
#               paste(names(best_fit$best_data), collapse = ", ")
#             ),
#             xref = "paper", yref = "paper",
#             x = 0.5, y = 0.5, showarrow = FALSE
#           )
#         )
#     )
#   }
# 
#   # ── Resolve independent variable ───────────────────────────────────
#   if (!independent_variable %in% names(best_fit$best_data)) {
#     if ("concentration" %in% names(best_fit$best_data)) {
#       independent_variable <- "concentration"
#     } else {
#       return(plotly::plot_ly() %>%
#                plotly::layout(title = "Missing independent variable column"))
#     }
#   }
# 
#   # ── Ensure stype exists ────────────────────────────────────────────
#   if (!"stype" %in% names(best_fit$best_data)) {
#     best_fit$best_data$stype <- "S"
#   }
# 
#   # ── Resolve response column in sample_se too ───────────────────────
#   samples_predicted_conc <- best_fit$sample_se
#   if (!is.null(samples_predicted_conc) && nrow(samples_predicted_conc) > 0) {
#     samp_resolved <- ensure_response_column(
#       df           = samples_predicted_conc,
#       response_var = response_variable,
#       coerce_numeric = TRUE,
#       context      = "plot_standard_curve/sample_se"
#     )
#     samples_predicted_conc <- samp_resolved$df
#     # If the response column was found under a different name in sample_se,
#     # we need the SAME name for consistency
#     if (samp_resolved$ok && samp_resolved$response_var != response_variable) {
#       samples_predicted_conc[[response_variable]] <-
#         samples_predicted_conc[[samp_resolved$response_var]]
#     }
#     samples_predicted_conc <- samples_predicted_conc[
#       !is.nan(samples_predicted_conc$raw_predicted_concentration) &
#         is.finite(samples_predicted_conc$raw_predicted_concentration), ,
#       drop = FALSE
#     ]
#   } else {
#     samples_predicted_conc <- data.frame(
#       raw_predicted_concentration = numeric(0),
#       pcov = numeric(0),
#       stringsAsFactors = FALSE
#     )
#     samples_predicted_conc[[response_variable]] <- numeric(0)
#   }
# 
#   best_fit$best_pred$pcov_threshold <- pcov_threshold
#   safe_glance <- function(field, default = NA_real_) {
#     val <- best_fit$best_glance[[field]]
#     if (is.null(val) || length(val) == 0) return(default)
#     val <- unlist(val)
#     if (all(is.na(val))) return(default)
#     val[1]
#   }
# 
#   ### 1. RESPONSE VARIABLE (Y) log transform
#   log_response_status <- isTRUE(as.logical(safe_glance("is_log_response", FALSE)))
# 
#   if (log_response_status && !isTRUE(is_display_log_response)) {
#     best_fit$best_data[[response_variable]] <- 10^best_fit$best_data[[response_variable]]
#     best_fit$best_pred$yhat               <- 10^best_fit$best_pred$yhat
#     best_fit$best_glance$llod             <- 10^safe_glance("llod")
#     best_fit$best_glance$ulod             <- 10^safe_glance("ulod")
#     best_fit$best_glance$lloq_y           <- 10^safe_glance("lloq_y")
#     best_fit$best_glance$uloq_y           <- 10^safe_glance("uloq_y")
#     best_fit$best_glance$inflect_y        <- 10^safe_glance("inflect_y")
#     best_fit$best_d2xy$d2x_y              <- 10^best_fit$best_d2xy$d2x_y
#     if (!is.null(best_fit$best_curve_ci)) {
#       best_fit$best_curve_ci$ci_lo        <- 10^best_fit$best_curve_ci$ci_lo
#       best_fit$best_curve_ci$ci_hi        <- 10^best_fit$best_curve_ci$ci_hi
#     }
#     if (nrow(samples_predicted_conc) > 0 &&
#         response_variable %in% names(samples_predicted_conc)) {
#       samples_predicted_conc[[response_variable]] <-
#         10^samples_predicted_conc[[response_variable]]
#     }
#   }
# 
#   ### 2. INDEPENDENT VARIABLE (X) log transform
#   log_x_status <- isTRUE(as.logical(safe_glance("is_log_x", FALSE)))
# 
#   if (log_x_status && !isTRUE(is_display_log_independent)) {
#     best_fit$best_data$concentration       <- 10^best_fit$best_data$concentration
#     best_fit$best_pred$x                   <- 10^best_fit$best_pred$x
#     best_fit$best_glance$lloq              <- 10^safe_glance("lloq")
#     best_fit$best_glance$uloq              <- 10^safe_glance("uloq")
#     best_fit$best_glance$inflect_x         <- 10^safe_glance("inflect_x")
#     best_fit$best_d2xy$x                   <- 10^best_fit$best_d2xy$x
#     if (nrow(samples_predicted_conc) > 0) {
#       samples_predicted_conc$raw_predicted_concentration <-
#         10^samples_predicted_conc$raw_predicted_concentration
#     }
#     best_fit$best_glance$mindc             <- 10^safe_glance("mindc")
#     best_fit$best_glance$maxdc             <- 10^safe_glance("maxdc")
#     best_fit$best_glance$minrdl            <- 10^safe_glance("minrdl")
#     best_fit$best_glance$maxrdl            <- 10^safe_glance("maxrdl")
#     if (!is.null(best_fit$best_curve_ci)) {
#       best_fit$best_curve_ci$x             <- 10^best_fit$best_curve_ci$x
#     }
#   }
# 
#   # y3_label <- paste(stringr::str_to_title(independent_variable), "Uncertainty (pCoV %)")
#   y3_label <- "Precision Coefficient of Variation (pCoV %)"
#   if (is_display_log_response) {
#     y_label <- paste("log<sub>10</sub>", format_assay_terms(response_variable))
# 
#   } else {
#     y_label <- format_assay_terms(response_variable)
#   }
# 
#   if (is_display_log_independent) {
#     x_label <- paste("log<sub>10</sub>", format_assay_terms(independent_variable))
#   } else {
#     x_label <- format_assay_terms(independent_variable)
#   }
#   ### 3. MODEL NAME
# 
#   model_name <- best_fit$best_model_name
#   title_model_name <- switch(
#     model_name,
#     "Y4" = "4-parameter Logistic",
#     "Yd4" = "4-parameter Log-Logistic",
#     "Ygomp4" = "4-parameter Gompertz type",
#     "Y5" = "5-parameter Logistic",
#     "Yd5" = "5-parameter Log-Logistic",
#     model_name
#   )
# 
#   ## 3b.  PREPARE SAMPLE‑UNCERTAINTY (single scaling) -------------------
# 
#   ##   • Convert everything to log10(SE) *once*.
#   ##   • Combine model‑derived SE and sample‑specific SE to get a
#   ##     common axis range.
# 
#   print(names(best_fit$best_pred))
#   se_model   <- best_fit$best_pred$pcov
#   se_samples <- samples_predicted_conc$pcov
#   se_all <- c(best_fit$best_pred$pcov,  samples_predicted_conc$pcov)
#   se_range      <- range(se_all, na.rm = TRUE)
# 
#   # se_max <- min(ceiling(quantile(se_samples ^ 2, probs = 0.95, na.rm = TRUE) * 1000), 100)
# 
# 
#   # se_margin <- (se_max - 0) * 0.02
#   # # se_margin <- (100 - se_range[1]) * 0.05
#   # se_axis_limits <- c(1 - se_margin, se_max + se_margin)
#   # # se_axis_limits <- c(se_range[1] - se_margin, 100 + se_margin)
# 
#   # se_max <- min(ceiling(quantile(se_samples ^ 2, probs = 0.95, na.rm = TRUE) * 1000), 100)
#   # se_min <- max(min(se_all, na.rm = TRUE), 0.1)  # Ensure positive minimum for log scale
# 
#   se_max <- 125
#   se_min <- -2#0.1
# 
#   # For log scale, work with the actual values (Plotly will handle the log transform)
#   se_axis_limits <- c(se_min * 0.9, se_max * 1.1)  # Add some padding
#   dtick <- ifelse(se_max > 19, ifelse(se_max > 35,10, 5), 1)
#   # dtick <- 0.301
# 
#   # microviz_kelly_pallete <-  c("#f3c300","#875692","#f38400","#a1caf1","#be0032","#c2b280","#848482",
#   #                              "#008856","#e68fac","#0067a5","#f99379","#604e97", "#f6a600",  "#b3446c" ,
#   #                              "#dcd300","#882d17","#8db600", "#654522", "#e25822","#2b3d26","lightgrey")
# 
#   ### 4. RAW POINTS — pre-compute to avoid plotly lazy-eval scope issues
#   plot_std <- best_fit$best_data
#   # on correct scale
#   if (is_display_log_independent) {
# 
#     glance_fda_lloq_conc <- log10(best_fit$best_glance$lloq_fda2018_concentration)
#     glance_fda_2018_uloq_conc <- log10(best_fit$best_glance$uloq_fda2018_concentration)
# 
#   } else {
# 
#     glance_fda_lloq_conc <- best_fit$best_glance$lloq_fda2018_concentration
#     glance_fda_2018_uloq_conc <- best_fit$best_glance$uloq_fda2018_concentration
# 
#   }
# 
#   plot_std$fda2018_class <- ifelse(
#     plot_std[[independent_variable]] >= glance_fda_lloq_conc &
#       plot_std[[independent_variable]] <= glance_fda_2018_uloq_conc,
#     "Standards (+ FDA 2018)",
#     "Standards (- FDA 2018)"
#   )
# 
#   std_in <- plot_std[
#     plot_std$stype == "S" &
#       plot_std$fda2018_class == "Standards (+ FDA 2018)", ]
# 
#   std_out <- plot_std[
#     plot_std$stype == "S" &
#       plot_std$fda2018_class == "Standards (- FDA 2018)", ]
# 
#   blanks <- plot_std[
#     plot_std$stype == "B", ]
# 
#   ### Standards inside FDA range (circle)
#   p <- p %>% plotly::add_trace(
#     data = std_in,
#     x = std_in[[independent_variable]],
#     y = std_in[[response_variable]],
#     type = "scatter",
#     mode = "markers",
#     name = "Standards (+ FDA 2018)",
#     legendgroup = "standards",
#     marker = list(
#       color = "#2b3d26",
#       symbol = "circle"
#     ),
#     text = ~paste0(
#       "<br>", format_assay_terms(independent_variable), ": ",
#       std_in[[independent_variable]],
#       "<br>Dilution Factor: ", dilution,
#       "<br>", format_assay_terms(response_variable), ": ",
#       std_in[[response_variable]],
#       "<br> FDA 2018 Status: ",  gsub("Standards ", "", std_in$fda2018_class)
#     ),
#     hoverinfo = "text"
#   )
# 
#   ### Standards outside FDA range (triangle)
#   p <- p %>% plotly::add_trace(
#     data = std_out,
#     x = std_out[[independent_variable]],
#     y = std_out[[response_variable]],
#     type = "scatter",
#     mode = "markers",
#     name = "Standards (- FDA 2018)",
#     legendgroup = "standards",
#     marker = list(
#       color = "#2b3d26",
#       symbol = "triangle-up",
#       size = 8
#     ),
#     text = ~paste0(
#       "<br>", format_assay_terms(independent_variable), ": ",
#       std_out[[independent_variable]],
#       "<br>Dilution Factor: ", dilution,
#       "<br>", format_assay_terms(response_variable), ": ",
#       std_out[[response_variable]],
#       "<br>FDA 2018 Status: ",  gsub("Standards ", "", std_out$fda2018_class)
#     ),
#     hoverinfo = "text"
#   )
# 
#   # blanks
#   p <- p %>% plotly::add_trace(
#     data = blanks,
#     x = blanks[[independent_variable]],
#     y = blanks[[response_variable]],
#     type = "scatter",
#     mode = "markers",
#     name = "Geometric Mean of Blanks",
#     marker = list(
#       color = "#c2b280",
#       symbol = "circle"
#     ),
#     text = ~paste0(
#       "<br>", format_assay_terms(independent_variable), ": ",
#       blanks[[independent_variable]],
#       "<br>Dilution Factor: ", dilution,
#       "<br>", format_assay_terms(response_variable), ": ",
#       blanks[[response_variable]]
#     ),
#     hoverinfo = "text"
#   )
#   # plot_std$marker_symbol <- "circle"
#   #
#   # plot_std$marker_symbol[
#   #   plot_std$stype == "S" & plot_std$fda2018_class == "Standards - FDA 2018"
#   # ] <- "triangle-up"
#   #
#   # plot_std$.trace_name <- ifelse(
#   #   plot_std$stype == "S", "Standards",
#   #   ifelse(plot_std$stype == "B",
#   #          "Geometric Mean of Blanks",
#   #          as.character(plot_std$stype))
#   # )
#   # plot_std$.trace_name <- ifelse(
#   #   plot_std$stype == "S", "Standards",
#   #   plot_std$fda2018_class,
#   #   ifelse(plot_std$stype == "B", "Geometric Mean of Blanks",
#   #          as.character(plot_std$stype))
#   # )
# 
#   # p <- p %>% plotly::add_trace(
#   #   data   = plot_std,
#   #   x      = plot_std[[independent_variable]],
#   #   y      = plot_std[[response_variable]],
#   #   type   = "scatter",
#   #   mode   = "markers",
#   #   name   = ~.trace_name,
#   #   color  = ~stype,
#   #   colors = c("B" = "#c2b280", "S" = "#2b3d26"),
#   #   text   = ~paste0(
#   #     "<br>", format_assay_terms(independent_variable), ": ",
#   #     plot_std[[independent_variable]],
#   #     "<br>Dilution Factor: ", dilution,
#   #     "<br>", format_assay_terms(response_variable), ": ",
#   #     plot_std[[response_variable]]
#   #   ),
#   #   hoverinfo = "text"
#   # )
#   # p <- p %>% plotly::add_trace(
#   #   data   = plot_std,
#   #   x      = plot_std[[independent_variable]],
#   #   y      = plot_std[[response_variable]],
#   #   type   = "scatter",
#   #   mode   = "markers",
#   #   name   = ~.trace_name,
#   #   color  = ~stype,
#   #   symbol = ~marker_symbol,
#   #   colors = c("B" = "#c2b280", "S" = "#2b3d26"),
#   #   symbols = c(
#   #     "circle" = "circle",
#   #     "triangle-up" = "triangle-up"
#   #   ),
#   #   text   = ~paste0(
#   #     "<br>", format_assay_terms(independent_variable), ": ",
#   #     plot_std[[independent_variable]],
#   #     "<br>Dilution Factor: ", dilution,
#   #     "<br>", format_assay_terms(response_variable), ": ",
#   #     plot_std[[response_variable]],
#   #     "<br>FDA 2018 Range: ", fda2018_class
#   #   ),
#   #   hoverinfo = "text"
#   # )
#   #
# 
#   ### 5. FITTED CURVE
# 
#   p <- p %>% add_lines(
#     x = best_fit$best_pred$x,
#     y = best_fit$best_pred$yhat,
#     name = "Fitted Curve",
#     legendgroup = "fitted_curve",
#     showlegend = TRUE,
#     line = list(color = "#2b3d26")
#   )
# 
#   ### 5b. 95% CI BANDS (delta method)
# 
#   if (!is.null(best_fit$best_curve_ci)) {
#     p <- p %>% add_lines(
#       x           = best_fit$best_curve_ci$x,
#       y           = best_fit$best_curve_ci$ci_lo,
#       name        = "95% CI",
#       line        = list(color = "#2b3d26", dash = "dash"),
#       #legendgroup = "linked_curve_ci"
#       legendgroup = "fitted_curve",
#       # visible     = "legendonly"
#     ) %>% add_lines(
#       x           = best_fit$best_curve_ci$x,
#       y           = best_fit$best_curve_ci$ci_hi,
#       name        = "",
#       line        = list(color = "#2b3d26", dash = "dash"),
#       legendgroup = "fitted_curve",
#       # legendgroup = "linked_curve_ci",
#       showlegend  = FALSE
#       # visible     = "legendonly"
#     )
#   }
# 
#   ### 6. LOD lines (horizontal)
# 
#   p <- p %>% add_lines(
#     x = best_fit$best_pred$x,
#     y = best_fit$best_glance$ulod,
#     name = paste(
#       "Upper LOD: (",
#       round(best_fit$best_glance$maxdc, 3), ",",
#       round(best_fit$best_glance$ulod, 3), ")"
#     ),
#     # name = paste("Upper LOD:", round(best_fit$best_glance$ulod, 3)),
#     line = list(color = "#e25822", dash = "dash"),
#     legendgroup = "linked_ulod",
#     visible     = "legendonly"
#   )
# 
#   p <- p %>% add_lines(
#     x = best_fit$best_pred$x,
#     y = best_fit$best_glance$llod,
#     name = paste(
#       "Lower LOD: (",
#       round(best_fit$best_glance$mindc, 3), ",",
#       round(best_fit$best_glance$llod, 3), ")"
#     ),
#     # name = paste("Lower LOD:", round(best_fit$best_glance$llod, 3)),
#     line = list(color = "#e25822", dash = "dash"),
#     legendgroup = "linked_llod",
#     visible     = "legendonly"
#   )
# 
# 
#   ### 7. LOQ (vertical + horizontal)
# 
#   y_min <- min(best_fit$best_data[[response_variable]], na.rm = TRUE)
#   y_max <- max(best_fit$best_data[[response_variable]], na.rm = TRUE)
# 
# 
#   ### 6b. MDC / RDL vertical lines
# 
#   ### mindc – vertical dashed line at x where fitted curve == llod
#   if (!is.na(best_fit$best_glance$mindc)) {
#     p <- p %>% add_lines(
#       x = c(best_fit$best_glance$mindc, best_fit$best_glance$mindc),
#       y = c(y_min, y_max),
#       name = paste("Lower DC:", round(best_fit$best_glance$mindc, 3)),
#       line = list(color = "#e25822", dash = "dash"),
#       legendgroup = "linked_llod",
#       showlegend = FALSE,
#       hoverinfo = "text",
#       visible     = "legendonly"
#     )
#   }
# 
#   ### minrdl – vertical solid line at x where lower CI of fitted curve == llod
#   if (!is.na(best_fit$best_glance$minrdl)) {
#     p <- p %>% add_lines(
#       x = c(best_fit$best_glance$minrdl, best_fit$best_glance$minrdl),
#       y = c(y_min, y_max),
#       name = paste("Lower RDL:", round(best_fit$best_glance$minrdl, 3)),
#       line = list(color = "#e25822"),
#       legendgroup = "linked_llod",
#       showlegend = TRUE,
#       hoverinfo = "text",
#       visible     = "legendonly"
#     )
#   }
# 
#   ### maxdc – vertical dashed line at x where fitted curve == ulod
#   if (!is.na(best_fit$best_glance$maxdc)) {
#     p <- p %>% add_lines(
#       x = c(best_fit$best_glance$maxdc, best_fit$best_glance$maxdc),
#       y = c(y_min, y_max),
#       name = paste("Upper DC:", round(best_fit$best_glance$maxdc, 3)),
#       line = list(color = "#e25822", dash = "dash"),
#       legendgroup = "linked_ulod",
#       showlegend = FALSE,
#       hoverinfo = "text",
#       visible     = "legendonly"
#     )
#   }
# 
#   ### maxrdl – vertical solid line at x where upper CI of fitted curve == ulod
#   if (!is.na(best_fit$best_glance$maxrdl)) {
#     p <- p %>% add_lines(
#       x = c(best_fit$best_glance$maxrdl, best_fit$best_glance$maxrdl),
#       y = c(y_min, y_max),
#       name = paste("Upper RDL:", round(best_fit$best_glance$maxrdl, 3)),
#       line = list(color = "#e25822"),
#       legendgroup = "linked_ulod",
#       showlegend = TRUE,
#       hoverinfo = "text",
#       visible     = "legendonly"
#     )
#   }
# 
#   ### LLOQ – vertical line
#   p <- p %>% add_lines(
#     x = c(best_fit$best_glance$lloq),
#     y = c(y_min, y_max),
#     name = paste(
#       "Lower LOQ: (",
#       round(best_fit$best_glance$lloq, 3), ",",
#       round(best_fit$best_glance$lloq_y, 3), ")"
#     ),
#     line = list(color = "#875692"),
#     legendgroup = "linked_lloq",
#     hoverinfo = "text",
#     visible     = "legendonly"
#   )
# 
#   ### ULOQ – vertical line
#   p <- p %>% add_lines(
#     x = c(best_fit$best_glance$uloq),
#     y = c(y_min, y_max),
#     name = paste(
#       "Upper LOQ: (",
#       round(best_fit$best_glance$uloq, 3), ",",
#       round(best_fit$best_glance$uloq_y, 3), ")"
#     ),
#     line = list(color = "#875692"),
#     legendgroup = "linked_uloq",
#     hoverinfo = "text",
#     visible     = "legendonly"
#   )
# 
#   ### Horizontal LOQ lines
#   p <- p %>% add_lines(
#     x = best_fit$best_pred$x,
#     y = best_fit$best_glance$uloq_y,
#     name = "",
#     legendgroup = "linked_uloq", showlegend = FALSE,
#     line = list(color = "#875692"),
#     visible     = "legendonly"
#   )
# 
#   p <- p %>% add_lines(
#     x = best_fit$best_pred$x,
#     y = best_fit$best_glance$lloq_y,
#     name = "",
#     legendgroup = "linked_lloq", showlegend = FALSE,
#     line = list(color = "#875692"),
#     visible     = "legendonly"
#   )
# 
# 
# 
#   ### 8a. SECOND DERIVATIVE (y2 axis)
# 
#   p <- p %>% add_lines(
#     x = best_fit$best_d2xy$x,
#     y = best_fit$best_d2xy$d2x_y,
#     name = "2nd Derivative of x given y",
#     yaxis = "y2",
#     line = list(color = "#604e97"),
#     visible = "legendonly"
#   )
# 
# 
#   ### 8b. Sample uncertainty (y3 axis)
# 
#   unc_col <- list(color = "#e68fac")
#   p <- p %>% add_lines(
#     x = best_fit$best_pred$x,
#     y = best_fit$best_pred$pcov,
#     name = "Measurement Uncertainty",
#     yaxis = "y3",
#     line = unc_col,
#     legendgroup = "linked_uncertainty",
#     visible = "legendonly"
#   ) %>% add_trace(
#     data = samples_predicted_conc,
#     x = ~raw_predicted_concentration,
#     y = ~pcov,
#     type = "scatter",
#     mode = "markers",
#     name = "",                         ## no extra legend entry
#     marker = list(color = "#800032", symbol = "circle"),
#     text = ~paste("Predicted", x_label, ":", raw_predicted_concentration,
#                   "<br>Coefficient of Variation (pCoV):", round(pcov,2), "%"),
#     yaxis = "y3",
#     legendgroup = "linked_uncertainty",
#     showlegend = FALSE,
#     hovertemplate = "%{text}<extra></extra>",
#     visible = "legendonly"
#   ) %>% add_lines(
#     x = best_fit$best_pred$x,
#     y = best_fit$best_pred$pcov_threshold,
#     name = paste0("pCoV Threshold: ",best_fit$best_pred$pcov_threshold,"%"),
#     yaxis = "y3",
#     line = list(color = "#e68fac", dash = "dash"),
#     legendgroup = "linked_uncertainty",
#     visible = "legendonly"
#   )
# 
#   ### 8c. MCMC pCoV uncertainty smooth line (y3 axis) — dense pred grid
#   if (!is.null(mcmc_pred) && nrow(mcmc_pred) > 0 &&
#       "pcov_robust_concentration" %in% names(mcmc_pred)) {
# 
#     pred_valid <- is.finite(mcmc_pred$pcov_robust_concentration) &
#       is.finite(mcmc_pred$raw_robust_concentration)
# 
#     if (any(pred_valid)) {
#       mcmc_pred_x    <- mcmc_pred$raw_robust_concentration[pred_valid]
#       mcmc_pred_pcov <- mcmc_pred$pcov_robust_concentration[pred_valid]
# 
#       # Apply same x-axis transform as all other traces (Section 2)
#       if (log_x_status && !isTRUE(is_display_log_independent)) {
#         mcmc_pred_x <- 10^mcmc_pred_x
#       }
# 
#       # Sort by x for smooth line
#       sort_idx       <- order(mcmc_pred_x)
#       mcmc_pred_x    <- mcmc_pred_x[sort_idx]
#       mcmc_pred_pcov <- mcmc_pred_pcov[sort_idx]
# 
#       p <- p %>% plotly::add_lines(
#         x = mcmc_pred_x,
#         y = mcmc_pred_pcov,
#         name = "MCMC Uncertainty",
#         yaxis = "y3",
#         line = list(color = "#800032"),
#         legendgroup = "linked_uncertainty",
#         showlegend = TRUE,
#         visible = "legendonly"
#       )
#     }
#   }
# 
#   ### 9a. SAMPLES
# 
#   p <- p %>% add_trace(
#     data = samples_predicted_conc,
#     x = ~raw_predicted_concentration,
#     y = samples_predicted_conc[[response_variable]],
#     type = "scatter",
#     mode = "markers",
#     name = "Samples",
#     marker = list(color = "#d1992a", symbol = "circle"),
#     text = ~paste("Predicted", x_label, ":", raw_predicted_concentration,
#                   "<br>",y_label , ":", samples_predicted_conc[[response_variable]],
#                   "<br>Patient ID:", patientid,
#                   "<br> Timepoint:", timeperiod,
#                   "<br>Well:", well,
#                   "<br>LOQ Gate Class:", samples_predicted_conc$gate_class_loq,
#                   "<br>LOD Gate Class:", samples_predicted_conc$gate_class_lod,
#                   "<br> PCOV Gate Class:",samples_predicted_conc$gate_class_pcov ),
#     #"<br>Timeperiod:", timeperiod),
#     hovertemplate = "%{text}<extra></extra>"
#   )
# 
#   ### 9b. MCMC ROBUST SAMPLES
#   if (!is.null(mcmc_samples) && nrow(mcmc_samples) > 0) {
# 
#     # Determine the response column name in mcmc_samples
#     # The DB stores it as assay_response; map to the plot's response variable
#     if ("assay_response" %in% names(mcmc_samples) &&
#         !response_variable %in% names(mcmc_samples)) {
#       mcmc_samples[[response_variable]] <- mcmc_samples$assay_response
#     }
# 
#     # Use raw_robust_concentration for x-axis (on model scale)
#     # If displaying on log scale and values are on linear scale, transform
#     mcmc_x <- mcmc_samples$raw_robust_concentration
#     mcmc_y <- mcmc_samples[[response_variable]]
# 
#     # Build hover text
#     mcmc_hover <- paste0(
#       "MCMC Robust Concentration: ", round(mcmc_x, 4),
#       "<br>", format_assay_terms(response_variable), ": ", round(mcmc_y, 2),
#       if ("patientid" %in% names(mcmc_samples))
#         paste0("<br>Patient ID: ", mcmc_samples$patientid) else "",
#       if ("timeperiod" %in% names(mcmc_samples))
#         paste0("<br>Timepoint: ", mcmc_samples$timeperiod) else "",
#       if ("well" %in% names(mcmc_samples))
#         paste0("<br>Well: ", mcmc_samples$well) else "",
#       if ("pcov_robust_concentration" %in% names(mcmc_samples))
#         paste0("<br>MCMC pCoV: ",
#                round(mcmc_samples$pcov_robust_concentration, 2), "%") else "",
#       if ("gate_class_loq" %in% names(mcmc_samples))
#         paste0("<br>LOQ Gate Class: ", mcmc_samples$gate_class_loq) else "",
#       if ("gate_class_lod" %in% names(mcmc_samples))
#         paste0("<br>LOD Gate Class: ", mcmc_samples$gate_class_lod) else ""
#     )
# 
#     p <- p %>% plotly::add_trace(
#       x = mcmc_x,
#       y = mcmc_y,
#       type = "scatter",
#       mode = "markers",
#       name = "MCMC Samples",
#       marker = list(
#         color = "#d1992a",
#         symbol = "diamond",
#         size = 7,
#         opacity = 0.8
#         # line = list(color = "##d1992a", width = 1)
#       ),
#       text = mcmc_hover,
#       hovertemplate = "%{text}<extra></extra>"
#     )
# 
#     ### 9c. MCMC pCoV uncertainty (y3 axis)
#     if ("pcov_robust_concentration" %in% names(mcmc_samples)) {
#       p <- p %>% plotly::add_trace(
#         x = mcmc_x,
#         y = mcmc_samples$pcov_robust_concentration,
#         type = "scatter",
#         mode = "markers",
#         name = "",
#         marker = list(color = "#800032", symbol = "diamond", size = 5),
#         text = paste0(
#           "MCMC ", x_label, ": ", round(mcmc_x, 4),
#           "<br>MCMC pCoV: ",
#           round(mcmc_samples$pcov_robust_concentration, 2), "%"
#         ),
#         yaxis = "y3",
#         legendgroup = "linked_uncertainty",
#         showlegend = FALSE,
#         hovertemplate = "%{text}<extra></extra>",
#         visible = "legendonly"
#       )
#     }
#   }
# 
# 
#   ### 10. INFLECTION POINT
# 
#   p <- p %>% add_trace(
#     x = best_fit$best_glance$inflect_x,
#     y = best_fit$best_glance$inflect_y,
#     type = "scatter",
#     mode = "markers",
#     name = paste(
#       "Inflection Point: (",
#       round(best_fit$best_glance$inflect_x, 3), ",",
#       round(best_fit$best_glance$inflect_y, 3), ")"
#     ),
#     legendgroup = "fitted_curve",
#     showlegend = TRUE,
#     marker = list(color = "#2724F0", size = 8)
#     # visible     = "legendonly"
#   )
# 
# 
#   ### 11. LAYOUT
# 
#   p <- p %>% layout(
#     title = paste(
#       "Fitted", title_model_name, "Model (",
#       unique(best_fit$best_data$plate), ",",
#       unique(best_fit$best_data$antigen), ")"
#     ),
#     xaxis = list(title = x_label,
#                  showgrid = TRUE,
#                  zeroline = FALSE),
#     yaxis = list(title = y_label,
#                  showgrid = TRUE,
#                  zeroline = TRUE),
#     legend = list(x = 1.1,
#                   y = 1,
#                   xanchor = "left"),
#     font = list(size = 12),
# 
#     yaxis2 = list(
#       showticklabels = FALSE,
#       title = "",
#       tickmode = "linear",
#       dtick = 10,
#       overlaying = "y",
#       side = "right",
#       showgrid = FALSE,
#       zeroline = FALSE
#     ),
# 
#     yaxis3 = list(
#       overlaying = "y",
#       side = "right",
#       title = y3_label,
#       range = se_axis_limits,
#       tickmode = "linear",
#       type = "linear",
#       # range = log10(se_axis_limits),
#       dtick = dtick,
#       showgrid = FALSE,
#       zeroline = FALSE,
#       showticklabels = TRUE
#     )
#   )
# 
#   return(p)
# }


# Old with no mcmc
# plot_standard_curve <- function(best_fit,
#                                 is_display_log_response,
#                                 is_display_log_independent,
#                                 pcov_threshold,
#                                 response_variable = "mfi",
#                                 independent_variable = "concentration") {
#   p <- plotly::plot_ly()
# 
#   samples_predicted_conc <- best_fit$sample_se
#   samples_predicted_conc <- samples_predicted_conc[!is.nan(samples_predicted_conc$raw_predicted_concentration),]
#   best_fit$best_pred$pcov_threshold <- pcov_threshold
# 
#   ### 1. RESPONSE VARIABLE (Y)
# 
#   log_response_status <- best_fit$best_glance$is_log_response
# 
#   if (log_response_status && !is_display_log_response) {
#     best_fit$best_data[[response_variable]] <- 10^best_fit$best_data[[response_variable]]
#     best_fit$best_pred$yhat               <- 10^best_fit$best_pred$yhat
#     best_fit$best_glance$llod             <- 10^best_fit$best_glance$llod
#     best_fit$best_glance$ulod             <- 10^best_fit$best_glance$ulod
#     best_fit$best_glance$lloq_y           <- 10^best_fit$best_glance$lloq_y
#     best_fit$best_glance$uloq_y           <- 10^best_fit$best_glance$uloq_y
#     best_fit$best_glance$inflect_y        <- 10^best_fit$best_glance$inflect_y
#     best_fit$best_d2xy$d2x_y              <- 10^best_fit$best_d2xy$d2x_y
#     if (!is.null(best_fit$best_curve_ci)) {
#       best_fit$best_curve_ci$ci_lo        <- 10^best_fit$best_curve_ci$ci_lo
#       best_fit$best_curve_ci$ci_hi        <- 10^best_fit$best_curve_ci$ci_hi
#     }
# 
#     samples_predicted_conc[[response_variable]] <- 10^samples_predicted_conc[[response_variable]]
#   }
# 
# 
#   ### 2. INDEPENDENT VARIABLE (X)
# 
#   log_independent_variable_status <- best_fit$best_glance$is_log_x
# 
#   if (log_independent_variable_status && !is_display_log_independent) {
#     best_fit$best_data$concentration              <- 10^best_fit$best_data$concentration
#     best_fit$best_pred$x                          <- 10^best_fit$best_pred$x
#     best_fit$best_glance$lloq                     <- 10^best_fit$best_glance$lloq
#     best_fit$best_glance$uloq                     <- 10^best_fit$best_glance$uloq
#     best_fit$best_glance$inflect_x                <- 10^best_fit$best_glance$inflect_x
#     best_fit$best_d2xy$x                          <- 10^best_fit$best_d2xy$x
#     samples_predicted_conc$raw_predicted_concentration <- 10^samples_predicted_conc$raw_predicted_concentration
#     best_fit$best_glance$mindc                    <- 10^best_fit$best_glance$mindc
#     best_fit$best_glance$maxdc                    <- 10^best_fit$best_glance$maxdc
#     best_fit$best_glance$minrdl                   <- 10^best_fit$best_glance$minrdl
#     best_fit$best_glance$maxrdl                   <- 10^best_fit$best_glance$maxrdl
#     if (!is.null(best_fit$best_curve_ci)) {
#       best_fit$best_curve_ci$x                   <- 10^best_fit$best_curve_ci$x
#     }
#   }
#   # y3_label <- paste(stringr::str_to_title(independent_variable), "Uncertainty (pCoV %)")
#   y3_label <- "Precision Coefficient of Variation (pCoV %)"
#   if (is_display_log_response) {
#     y_label <- paste("log<sub>10</sub>", format_assay_terms(response_variable))
# 
#   } else {
#     y_label <- format_assay_terms(response_variable)
#   }
# 
#   if (is_display_log_independent) {
#     x_label <- paste("log<sub>10</sub>", format_assay_terms(independent_variable))
#   } else {
#     x_label <- format_assay_terms(independent_variable)
#   }
#   ### 3. MODEL NAME
# 
#   model_name <- best_fit$best_model_name
#   title_model_name <- switch(
#     model_name,
#     "Y4" = "4-parameter Logistic",
#     "Yd4" = "4-parameter Log-Logistic",
#     "Ygomp4" = "4-parameter Gompertz type",
#     "Y5" = "5-parameter Logistic",
#     "Yd5" = "5-parameter Log-Logistic",
#     model_name
#   )
# 
#   ## 3b.  PREPARE SAMPLE‑UNCERTAINTY (single scaling) -------------------
# 
#   ##   • Convert everything to log10(SE) *once*.
#   ##   • Combine model‑derived SE and sample‑specific SE to get a
#   ##     common axis range.
# 
#   print(names(best_fit$best_pred))
#   se_model   <- best_fit$best_pred$pcov
#   se_samples <- samples_predicted_conc$pcov
#   se_all <- c(best_fit$best_pred$pcov,  samples_predicted_conc$pcov)
#   se_range      <- range(se_all, na.rm = TRUE)
# 
#   # se_max <- min(ceiling(quantile(se_samples ^ 2, probs = 0.95, na.rm = TRUE) * 1000), 100)
# 
# 
#   # se_margin <- (se_max - 0) * 0.02
#   # # se_margin <- (100 - se_range[1]) * 0.05
#   # se_axis_limits <- c(1 - se_margin, se_max + se_margin)
#   # # se_axis_limits <- c(se_range[1] - se_margin, 100 + se_margin)
# 
#   # se_max <- min(ceiling(quantile(se_samples ^ 2, probs = 0.95, na.rm = TRUE) * 1000), 100)
#   # se_min <- max(min(se_all, na.rm = TRUE), 0.1)  # Ensure positive minimum for log scale
# 
#   se_max <- 125
#   se_min <- 0.1
# 
#   # For log scale, work with the actual values (Plotly will handle the log transform)
#   se_axis_limits <- c(se_min * 0.9, se_max * 1.1)  # Add some padding
#   dtick <- ifelse(se_max > 19, ifelse(se_max > 35,10, 5), 1)
#   # dtick <- 0.301
# 
#     # microviz_kelly_pallete <-  c("#f3c300","#875692","#f38400","#a1caf1","#be0032","#c2b280","#848482",
#   #                              "#008856","#e68fac","#0067a5","#f99379","#604e97", "#f6a600",  "#b3446c" ,
#   #                              "#dcd300","#882d17","#8db600", "#654522", "#e25822","#2b3d26","lightgrey")
# 
#   ### 4. RAW POINTS (standards + blanks)
# 
#   p <- p %>% add_trace(
#     data = best_fit$best_data,
#     x = best_fit$best_data[[independent_variable]],
#     y = best_fit$best_data[[response_variable]],
#     type = "scatter",
#     mode = "markers",
#     name = ~ifelse(stype == "S", "Standards",
#                    ifelse(stype == "B", "Geometric Mean of Blanks", stype)),
#     color = ~stype,
#     colors = c("B" = "#c2b280", "S" = "#2b3d26"),
#     text = ~paste0(
#       "<br>", format_assay_terms(independent_variable), ": ", best_fit$best_data[[independent_variable]],
#       "<br>Dilution Factor: ", dilution,
#       "<br>", format_assay_terms(response_variable), ": ", best_fit$best_data[[response_variable]]
#     ),
#     hoverinfo = "text"
#   )
# 
# 
#   ### 5. FITTED CURVE
# 
#   p <- p %>% add_lines(
#     x = best_fit$best_pred$x,
#     y = best_fit$best_pred$yhat,
#     name = "Fitted Curve",
#     legendgroup = "fitted_curve",
#     showlegend = TRUE,
#     line = list(color = "#2b3d26")
#   )
# 
#   ### 5b. 95% CI BANDS (delta method)
# 
#   if (!is.null(best_fit$best_curve_ci)) {
#     p <- p %>% add_lines(
#       x           = best_fit$best_curve_ci$x,
#       y           = best_fit$best_curve_ci$ci_lo,
#       name        = "95% CI",
#       line        = list(color = "#2b3d26", dash = "dash"),
#       #legendgroup = "linked_curve_ci"
#       legendgroup = "fitted_curve",
#       # visible     = "legendonly"
#     ) %>% add_lines(
#       x           = best_fit$best_curve_ci$x,
#       y           = best_fit$best_curve_ci$ci_hi,
#       name        = "",
#       line        = list(color = "#2b3d26", dash = "dash"),
#       legendgroup = "fitted_curve",
#      # legendgroup = "linked_curve_ci",
#       showlegend  = FALSE
#       # visible     = "legendonly"
#     )
#   }
# 
#   ### 6. LOD lines (horizontal)
# 
#   p <- p %>% add_lines(
#     x = best_fit$best_pred$x,
#     y = best_fit$best_glance$ulod,
#     name = paste(
#       "Upper LOD: (",
#       round(best_fit$best_glance$maxdc, 3), ",",
#       round(best_fit$best_glance$ulod, 3), ")"
#     ),
#     # name = paste("Upper LOD:", round(best_fit$best_glance$ulod, 3)),
#     line = list(color = "#e25822", dash = "dash"),
#     legendgroup = "linked_ulod",
#     visible     = "legendonly"
#   )
# 
#   p <- p %>% add_lines(
#     x = best_fit$best_pred$x,
#     y = best_fit$best_glance$llod,
#     name = paste(
#       "Lower LOD: (",
#       round(best_fit$best_glance$mindc, 3), ",",
#       round(best_fit$best_glance$llod, 3), ")"
#     ),
#     # name = paste("Lower LOD:", round(best_fit$best_glance$llod, 3)),
#     line = list(color = "#e25822", dash = "dash"),
#     legendgroup = "linked_llod",
#     visible     = "legendonly"
#   )
# 
# 
#   ### 7. LOQ (vertical + horizontal)
# 
#   y_min <- min(best_fit$best_data[[response_variable]], na.rm = TRUE)
#   y_max <- max(best_fit$best_data[[response_variable]], na.rm = TRUE)
# 
# 
#   ### 6b. MDC / RDL vertical lines
# 
#   ### mindc – vertical dashed line at x where fitted curve == llod
#   if (!is.na(best_fit$best_glance$mindc)) {
#     p <- p %>% add_lines(
#       x = c(best_fit$best_glance$mindc, best_fit$best_glance$mindc),
#       y = c(y_min, y_max),
#       name = paste("Lower DC:", round(best_fit$best_glance$mindc, 3)),
#       line = list(color = "#e25822", dash = "dash"),
#       legendgroup = "linked_llod",
#       showlegend = FALSE,
#       hoverinfo = "text",
#       visible     = "legendonly"
#     )
#   }
# 
#   ### minrdl – vertical solid line at x where lower CI of fitted curve == llod
#   if (!is.na(best_fit$best_glance$minrdl)) {
#     p <- p %>% add_lines(
#       x = c(best_fit$best_glance$minrdl, best_fit$best_glance$minrdl),
#       y = c(y_min, y_max),
#       name = paste("Lower RDL:", round(best_fit$best_glance$minrdl, 3)),
#       line = list(color = "#e25822"),
#       legendgroup = "linked_llod",
#       showlegend = TRUE,
#       hoverinfo = "text",
#       visible     = "legendonly"
#     )
#   }
# 
#   ### maxdc – vertical dashed line at x where fitted curve == ulod
#   if (!is.na(best_fit$best_glance$maxdc)) {
#     p <- p %>% add_lines(
#       x = c(best_fit$best_glance$maxdc, best_fit$best_glance$maxdc),
#       y = c(y_min, y_max),
#       name = paste("Upper DC:", round(best_fit$best_glance$maxdc, 3)),
#       line = list(color = "#e25822", dash = "dash"),
#       legendgroup = "linked_ulod",
#       showlegend = FALSE,
#       hoverinfo = "text",
#       visible     = "legendonly"
#     )
#   }
# 
#   ### maxrdl – vertical solid line at x where upper CI of fitted curve == ulod
#   if (!is.na(best_fit$best_glance$maxrdl)) {
#     p <- p %>% add_lines(
#       x = c(best_fit$best_glance$maxrdl, best_fit$best_glance$maxrdl),
#       y = c(y_min, y_max),
#       name = paste("Upper RDL:", round(best_fit$best_glance$maxrdl, 3)),
#       line = list(color = "#e25822"),
#       legendgroup = "linked_ulod",
#       showlegend = TRUE,
#       hoverinfo = "text",
#       visible     = "legendonly"
#     )
#   }
# 
#   ### LLOQ – vertical line
#   p <- p %>% add_lines(
#     x = c(best_fit$best_glance$lloq),
#     y = c(y_min, y_max),
#     name = paste(
#       "Lower LOQ: (",
#       round(best_fit$best_glance$lloq, 3), ",",
#       round(best_fit$best_glance$lloq_y, 3), ")"
#     ),
#     line = list(color = "#875692"),
#     legendgroup = "linked_lloq",
#     hoverinfo = "text",
#     visible     = "legendonly"
#   )
# 
#   ### ULOQ – vertical line
#   p <- p %>% add_lines(
#     x = c(best_fit$best_glance$uloq),
#     y = c(y_min, y_max),
#     name = paste(
#       "Upper LOQ: (",
#       round(best_fit$best_glance$uloq, 3), ",",
#       round(best_fit$best_glance$uloq_y, 3), ")"
#     ),
#     line = list(color = "#875692"),
#     legendgroup = "linked_uloq",
#     hoverinfo = "text",
#     visible     = "legendonly"
#   )
# 
#   ### Horizontal LOQ lines
#   p <- p %>% add_lines(
#     x = best_fit$best_pred$x,
#     y = best_fit$best_glance$uloq_y,
#     name = "",
#     legendgroup = "linked_uloq", showlegend = FALSE,
#     line = list(color = "#875692"),
#     visible     = "legendonly"
#   )
# 
#   p <- p %>% add_lines(
#     x = best_fit$best_pred$x,
#     y = best_fit$best_glance$lloq_y,
#     name = "",
#     legendgroup = "linked_lloq", showlegend = FALSE,
#     line = list(color = "#875692"),
#     visible     = "legendonly"
#   )
# 
# 
# 
#   ### 8a. SECOND DERIVATIVE (y2 axis)
# 
#   p <- p %>% add_lines(
#     x = best_fit$best_d2xy$x,
#     y = best_fit$best_d2xy$d2x_y,
#     name = "2nd Derivative of x given y",
#     yaxis = "y2",
#     line = list(color = "#604e97"),
#     visible = "legendonly"
#   )
# 
# 
#   ### 8b. Sample uncertainty (y3 axis)
# 
#   unc_col <- list(color = "#e68fac")
#   p <- p %>% add_lines(
#     x = best_fit$best_pred$x,
#     y = best_fit$best_pred$pcov,
#     name = "Measurement Uncertainty",
#     yaxis = "y3",
#     line = unc_col,
#     legendgroup = "linked_uncertainty",
#     visible = "legendonly"
#   ) %>% add_trace(
#     data = samples_predicted_conc,
#     x = ~raw_predicted_concentration,
#     y = ~pcov,
#     type = "scatter",
#     mode = "markers",
#     name = "",                         ## no extra legend entry
#     marker = list(color = "#800032", symbol = "circle"),
#     text = ~paste("Predicted", x_label, ":", raw_predicted_concentration,
#                   "<br>Coefficient of Variation (pCoV):", round(pcov,2), "%"),
#     yaxis = "y3",
#     legendgroup = "linked_uncertainty",
#     showlegend = FALSE,
#     hovertemplate = "%{text}<extra></extra>",
#     visible = "legendonly"
#   ) %>% add_lines(
#     x = best_fit$best_pred$x,
#     y = best_fit$best_pred$pcov_threshold,
#     name = paste0("pCoV Threshold: ",best_fit$best_pred$pcov_threshold,"%"),
#     yaxis = "y3",
#     line = list(color = "#e68fac", dash = "dash"),
#     legendgroup = "linked_uncertainty",
#     visible = "legendonly"
#   )
# 
# 
#   ### 9a. SAMPLES
# 
#   p <- p %>% add_trace(
#     data = samples_predicted_conc,
#     x = ~raw_predicted_concentration,
#     y = samples_predicted_conc[[response_variable]],
#     type = "scatter",
#     mode = "markers",
#     name = "Samples",
#     marker = list(color = "#d1992a", symbol = "circle"),
#     text = ~paste("Predicted", x_label, ":", raw_predicted_concentration,
#                   "<br>",y_label , ":", samples_predicted_conc[[response_variable]],
#                   "<br>Patient ID:", patientid,
#                   "<br> Timepoint:", timeperiod,
#                   "<br>Well:", well,
#                   "<br>LOQ Gate Class:", samples_predicted_conc$gate_class_loq,
#                   "<br>LOD Gate Class:", samples_predicted_conc$gate_class_lod,
#                   "<br> PCOV Gate Class:",samples_predicted_conc$gate_class_pcov ),
#     #"<br>Timeperiod:", timeperiod),
#     hovertemplate = "%{text}<extra></extra>"
#   )
# 
# 
#   ### 10. INFLECTION POINT
# 
#   p <- p %>% add_trace(
#     x = best_fit$best_glance$inflect_x,
#     y = best_fit$best_glance$inflect_y,
#     type = "scatter",
#     mode = "markers",
#     name = paste(
#       "Inflection Point: (",
#       round(best_fit$best_glance$inflect_x, 3), ",",
#       round(best_fit$best_glance$inflect_y, 3), ")"
#     ),
#     legendgroup = "fitted_curve",
#     showlegend = TRUE,
#     marker = list(color = "#2724F0", size = 8)
#    # visible     = "legendonly"
#   )
# 
# 
#   ### 11. LAYOUT
# 
#   p <- p %>% layout(
#     title = paste(
#       "Fitted", title_model_name, "Model (",
#       unique(best_fit$best_data$plate), ",",
#       unique(best_fit$best_data$antigen), ")"
#     ),
#     xaxis = list(title = x_label,
#                  showgrid = TRUE,
#                  zeroline = FALSE),
#     yaxis = list(title = y_label,
#                  showgrid = TRUE,
#                  zeroline = TRUE),
#     legend = list(x = 1.1,
#                   y = 1,
#                   xanchor = "left"),
#     font = list(size = 12),
# 
#     yaxis2 = list(
#       showticklabels = FALSE,
#       title = "",
#       tickmode = "linear",
#       dtick = 10,
#       overlaying = "y",
#       side = "right",
#       showgrid = FALSE,
#       zeroline = FALSE
#     ),
# 
#     yaxis3 = list(
#       overlaying = "y",
#       side = "right",
#       title = y3_label,
#       range = se_axis_limits,
#       tickmode = "linear",
#       type = "linear",
#       # range = log10(se_axis_limits),
#       dtick = dtick,
#       showgrid = FALSE,
#       zeroline = FALSE,
#       showticklabels = TRUE
#     )
#   )
# 
#   return(p)
# }

glance_plot_data <- function(best_glance_all,
                             best_plate_all,
                             verbose = TRUE) {
  datf <- dplyr::inner_join(best_glance_all[ , c("study_accession", "experiment_accession","antigen","plateid","source","is_log_x","status","crit","lloq","uloq","inflect_x","dydx_inflect")],
                            best_plate_all[, c("feature","plateid","plate","nominal_sample_dilution","assay_response_variable","assay_independent_variable")], by = "plateid"
  )
  datf$lloq <- ifelse(datf$is_log_x, 10^datf$lloq, datf$lloq)
  datf$uloq <- ifelse(datf$is_log_x, 10^datf$uloq, datf$uloq)
  datf$inflect_x <- ifelse(datf$is_log_x, 10^datf$inflect_x, datf$inflect_x)
  datf$lloq <- ifelse(is.na(datf$lloq), 0, datf$lloq)
  datf$uloq <- ifelse(is.na(datf$uloq), 2* datf$inflect_x, datf$uloq)
  datf$feature <- ifelse(datf$experiment_accession == "ADCD", paste0(datf$experiment_accession, "_", datf$nominal_sample_dilution, "x"), datf$feature)
  datf$feature <- factor(datf$feature)
  datf$plate <- factor(datf$plate)
  datf$experiment_accession <- factor(datf$experiment_accession)

  datf$source <- factor(datf$source)
  datf$nominal_sample_dilution <- factor(datf$nominal_sample_dilution)
  datf$antigen <- factor(datf$antigen)
  datf$status <- factor(datf$status)
  datf$crit <- factor(datf$crit)
  return(datf)
}


make_feature_plot <- function(datf, feature_name) {

  df <- datf %>%
    filter(feature == feature_name)

  # Define hover text
  df$hover <- paste0(
    "experiment_accession: ", df$experiment_accession, "<br>",
    "feature: ", df$feature, "<br>",
    "antigen: ", df$antigen, "<br>",
    "plate: ", df$plate, "<br>",
    "source: ", df$source, "<br>",
    "nominal_sample_dilution: ", df$nominal_sample_dilution, "<br>",
    "best_fit_model: ", df$crit, "<br>",
    "range of quantification: ", round(df$lloq,2), "-", round(df$uloq,2),  "<br>",
    "sensitivity (slope @inflection): ", round(df$dydx_inflect,2)
  )

  p <- plotly::plot_ly()

  # Add horizontal line segments
  antigens <- unique(df$antigen)
  pal <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(length(antigens))
  names(pal) <- antigens

  p <- p %>% add_segments(
    data = df,
    x = ~lloq,
    xend = ~uloq,
    y = ~dydx_inflect,
    yend = ~dydx_inflect,
    color = ~antigen,
    colors = pal,
    text = ~hover,
    hoverinfo = "text",
    line = list(width = 3)
  )

  # Layout for Nature‑style clarity
  p <- p %>% layout(
    title = list(
      title = paste(
        "Feature:", feature_name
      )
    ),
    xaxis = list(
      title = "Concentration",
      type = "log",
      tickvals = c(1,10,100,1000,10000,100000),
      ticktext = c("1","10","100","1e3","1e4","1e5"),
      showgrid = TRUE,
      zeroline = FALSE
    ),
    yaxis = list(
      title = "Sensitivity (slope at inflection)",
      type = "log",
      showgrid = TRUE,
      zeroline = FALSE
    ),
    legend = list(
      title = list(text = "Antigen"),
      orientation = "v"
    ),
    plot_bgcolor = 'white',
    paper_bgcolor = 'white'
  )

  return(p)
}

