
build_antigen_list <- function(exp_list, loaded_data_list, study_accession, verbose = TRUE) {
  antigen_list <- list()
  antigen_list_ids <- c()
  for (experiment_accession in exp_list) {
    loaded_data <- loaded_data_list[[experiment_accession]]
    standards <- loaded_data$standards
    print("standards antigen_list")
    print(names(standards))
    # Use source_nom instead of source for unique combos
    source_col <- if ("source_nom" %in% names(standards)) "source_nom" else "source"
    
    # ── Include wavelength in combo grouping so ELISA plates with
    #    multiple wavelengths produce separate entries ──────────────
    has_wavelength <- "wavelength" %in% names(standards)
    if (has_wavelength) {
      # Normalize before grouping so NA / "" / NA_character_ all become WL_NONE
      standards$wavelength <- normalize_wavelength(standards$wavelength)
      combo_cols <- c("experiment_accession", "plate", source_col, "plate_nom", "wavelength")
    } else {
      combo_cols <- c("experiment_accession", "plate", source_col, "plate_nom")
    }
    
    valid_combos <- unique(standards[, combo_cols, drop = FALSE])
    print(valid_combos)
    for (i in seq_len(nrow(valid_combos))) {
      current_combo <- valid_combos[i, ]
      
      combo_filter <- standards$experiment_accession == current_combo$experiment_accession &
                        standards[[source_col]] == current_combo[[source_col]] &
                        standards$plate_nom == current_combo$plate_nom
      if (has_wavelength) {
        combo_filter <- combo_filter & standards$wavelength == current_combo$wavelength
      }
      exp_standards <- standards[combo_filter, ]

      antigens <- unique(exp_standards$antigen)
      antigens <- antigens[!is.na(antigens) & antigens != ""]

      current_wl <- if (has_wavelength) current_combo$wavelength else WL_NONE

      if (length(antigens) > 0) {
        id <- paste0(experiment_accession, "_",
                     current_combo$plate_nom, "_",
                     current_combo[[source_col]], "_",
                     current_wl)
        antigen_list[[id]] <- list(
          antigens = unique(exp_standards$antigen),
          study_accession = study_accession,
          experiment_accession = experiment_accession,
          plate = current_combo$plate,
          plate_nom = current_combo$plate_nom,
          source = current_combo[[source_col]],        # source_nom value
          source_original = if ("source" %in% names(current_combo)) current_combo$source else NA_character_,
          wavelength = current_wl
        )

        antigen_list_ids <- c(antigen_list_ids, id)
      }
    }
  }

  return(list(antigen_list = antigen_list,
              antigen_list_ids = antigen_list_ids))

}

build_antigen_plate_list <- function(antigen_list_result, loaded_data_list, verbose = TRUE) {
  antigen_list <- antigen_list_result$antigen_list
  antigen_list_ids <- antigen_list_result$antigen_list_ids

  antigen_plate_list <- list()
  antigen_plate_list_ids <- c()
  for (antigen_id in antigen_list_ids) {
    info <- antigen_list[[antigen_id]]
    print("info diagnositic:\n")
    print(info)
    for (antigen in info$antigens) {
      print(antigen)
      antigen_constraints <- loaded_data_list[[info$experiment_accession]]$antigen_constraints[
        loaded_data_list[[info$experiment_accession]]$antigen_constraints$antigen == antigen,
      ]

      current_unique_id <- paste0(
        info$study_accession, "|",
        info$experiment_accession, "|",
        info$plate_nom, "|",
        info$source, "|",
       #info$nominal_sample_dilution, "|",
        antigen, "|",
        info$wavelength
      )

      antigen_plate_list[[current_unique_id]] <- select_antigen_plate(
        loaded_data = loaded_data_list[[info$experiment_accession]],
        study_accession = info$study_accession,
        experiment_accession = info$experiment_accession,
        source = info$source,
        antigen = antigen,
        plate = info$plate_nom,
        wavelength = info$wavelength,
       # nominal_sample_dilution = info$nominal_sample_dilution,
        antigen_constraints = antigen_constraints
      )

      antigen_plate_list_ids <- c(antigen_plate_list_ids, current_unique_id)
    }
  }

  return(list(antigen_plate_list = antigen_plate_list,
              antigen_plate_list_ids = antigen_plate_list_ids)
  )

}

prep_plate_data_batch <- function(antigen_plate_list_res, study_params, verbose = TRUE) {
  antigen_plate_list <- antigen_plate_list_res$antigen_plate_list
  antigen_plate_list_ids <-  antigen_plate_list_res$antigen_plate_list_ids

  prepped_data_list <- list()
  formula_list <- list()

  antigen_plate_name_list <- list()

  for (id in antigen_plate_list_ids) {
    antigen_plate_current <- antigen_plate_list[[id]]
    if (verbose) print(id)
    prepped_data_list[[id]] <- preprocess_robust_curves(data = antigen_plate_current$plate_standard,
                                                        antigen_settings = antigen_plate_current$antigen_settings,
                                                        response_variable = unique(antigen_plate_current$plate_standard$assay_response_variable),
                                                        independent_variable = unique(antigen_plate_current$plate_standard$assay_independent_variable),
                                                        is_log_response = study_params$is_log_response,
                                                        blank_data = antigen_plate_current$plate_blanks,
                                                        blank_option = study_params$blank_option,
                                                        is_log_independent = study_params$is_log_independent,
                                                        apply_prozone = study_params$applyProzone,
                                                        verbose = TRUE)
    if (nrow(prepped_data_list[[id]]$data) < 6) {
      prepped_data_list[[id]] <- NULL
      next
    }

    formula_list[[id]] <- select_model_formulas(fixed_constraint = antigen_plate_current$fixed_a_result,
                                                response_variable = unique(antigen_plate_current$plate_standard$assay_response_variable),
                                                is_log_response = study_params$is_log_response)

    antigen_plate_name_list[[id]] <- id

  }

  return(list(prepped_data_list = prepped_data_list,
              formula_list = formula_list,
              antigen_plate_name_list = antigen_plate_name_list))
}


fit_experiment_plate_batch <- function(prepped_data_list_res,
                                       antigen_plate_list_res,
                                       model_names,
                                       study_params,
                                       se_antigen_table    = NULL,
                                       dil_series_se_table = NULL,   # ← NEW
                                       prog_file           = NULL,
                                       dil_series_response_col = NULL,
                                       verbose             = TRUE) {
  prepped_data_list  <- prepped_data_list_res$prepped_data_list
  formula_list       <- prepped_data_list_res$formula_list
  antigen_plate_list <- antigen_plate_list_res$antigen_plate_list
  plate_model_constraints_list <- list()
  plate_start_lists            <- list()
  plate_robust_fit_list        <- list()
  fit_summary_list             <- list()
  fit_params_list              <- list()
  plot_data_list               <- list()
  candidate_best_fit_list      <- list()
  best_fit_list                <- list()
  all_ids <- names(prepped_data_list)
  n_total <- length(all_ids)
  if (!is.null(prog_file)) {
    tryCatch(
      writeLines(
        paste0(
          "Interpolated: starting batch fitting...\n",
          "Total antigens to fit: ", n_total
        ),
        prog_file
      ),
      error = function(e) NULL
    )
  }
  for (i in seq_along(all_ids)) {
    prep_dat_name <- all_ids[[i]]
    components <- strsplit(prep_dat_name, "\\|")[[1]]
    progress_text <- paste0(
      "Interpolated: ", i, " / ", n_total, "\n",
      "Study:      ", components[1],       "\n",
      "Experiment: ", components[2],       "\n",
      "Plate:      ", components[3],       "\n",
      "Source:     ", components[4],       "\n",
      "Antigen:    ", components[5]
    )
    if (!is.null(prog_file))
      tryCatch(writeLines(progress_text, prog_file), error = function(e) NULL)
    message(progress_text)
    if (verbose) print(prep_dat_name)
    plate_prepped_data   <- prepped_data_list[[prep_dat_name]]
    formulas             <- formula_list[[prep_dat_name]]
    response_variable    <- unique(antigen_plate_list[[prep_dat_name]]$plate_standard$assay_response_variable)
    independent_variable <- unique(antigen_plate_list[[prep_dat_name]]$plate_standard$assay_independent_variable)
    fixed_a_result       <- antigen_plate_list[[prep_dat_name]]$fixed_a_result
    antigen_settings     <- antigen_plate_list[[prep_dat_name]]$antigen_settings
    if (verbose) print(independent_variable)
    y_range <- range(plate_prepped_data$data[[response_variable]], na.rm = TRUE)
    if (verbose) {
      message(sprintf(
        "\n[batch] %s — response range: [%.4f, %.4f], dynamic_range: %.4f",
        prep_dat_name, y_range[1], y_range[2], diff(y_range)
      ))
    }
    plate_model_constraints_list[[prep_dat_name]] <- obtain_model_constraints(
      data                 = plate_prepped_data$data,
      formulas             = formulas,
      independent_variable = independent_variable,
      response_variable    = response_variable,
      is_log_response      = TRUE,
      is_log_concentration = TRUE,
      antigen_settings     = antigen_settings,
      max_response         = y_range[2],
      min_response         = y_range[1],
      verbose              = verbose
    )
    plate_start_lists[[prep_dat_name]] <- make_start_lists(
      model_constraints = plate_model_constraints_list[[prep_dat_name]],
      frac_generate     = 0.8,
      quants            = c(low = 0.2, mid = 0.5, high = 0.8)
    )
    plate_robust_fit_list[[prep_dat_name]] <- compute_robust_curves(
      prepped_data         = plate_prepped_data$data,
      response_variable    = response_variable,
      independent_variable = independent_variable,
      formulas             = formulas,
      model_constraints    = plate_model_constraints_list[[prep_dat_name]],
      start_lists          = plate_start_lists[[prep_dat_name]],
      verbose              = verbose
    )
    fit_summary_list[[prep_dat_name]] <- summarize_model_fits(
      plate_robust_fit_list[[prep_dat_name]],
      verbose = verbose
    )
    fit_params_list[[prep_dat_name]] <- summarize_model_parameters(
      models_fit_list = plate_robust_fit_list[[prep_dat_name]],
      level           = 0.95,
      model_names     = model_names,
      verbose         = verbose
    )
    plot_data_list[[prep_dat_name]] <- get_plot_data(
      models_fit_list  = plate_robust_fit_list[[prep_dat_name]],
      prepped_data     = plate_prepped_data$data,
      fit_params       = fit_params_list[[prep_dat_name]],
      fixed_a_result   = fixed_a_result,
      model_names      = model_names,
      x_var            = independent_variable,
      y_var            = response_variable,
      verbose          = verbose
    )
    candidate_best_fit_list[[prep_dat_name]] <- select_model_fit_AIC(
      fit_summary   = fit_summary_list[[prep_dat_name]],
      fit_robust_lm = plate_robust_fit_list[[prep_dat_name]],
      fit_params    = fit_params_list[[prep_dat_name]],
      plot_data     = plot_data_list[[prep_dat_name]],
      verbose       = verbose
    )
    # ── NEW: filter dil_series_se_table for this curve — mirrors UI best_fit reactive [2] ──
    current_plate_standard <- antigen_plate_list[[prep_dat_name]]$plate_standard
    current_plate_nom  <- unique(current_plate_standard$plate_nom)[1]
    current_source_nom <- if ("source_nom" %in% names(current_plate_standard)) {
      unique(current_plate_standard$source_nom)[1]
    } else {
      unique(current_plate_standard$source)[1]
    }
    current_antigen <- unique(current_plate_standard$antigen)[1]
    # Add right after extracting current_plate_nom, current_source_nom, current_antigen
    message(sprintf(
      "[batch filter DEBUG] prep_dat_name='%s'",
      prep_dat_name
    ))
    message(sprintf(
      "[batch filter DEBUG] current_plate_nom='%s'  current_source_nom='%s'  current_antigen='%s'",
      current_plate_nom, current_source_nom, current_antigen
    ))
    if (!is.null(dil_series_se_table)) {
      message(sprintf(
        "[batch filter DEBUG] dil_series plate_nom unique: %s",
        paste(unique(dil_series_se_table$plate_nom), collapse=" | ")
      ))
      message(sprintf(
        "[batch filter DEBUG] dil_series source_nom unique: %s",
        paste(unique(dil_series_se_table$source_nom), collapse=" | ")
      ))
    }
    message("current dil series se table:\n")
    print(head(dil_series_se_table))
    dil_series_df_filtered <- NULL
    if (!is.null(dil_series_se_table) && nrow(dil_series_se_table) > 0) {
      dil_series_df_filtered <- tryCatch({
        mask <- (
          dil_series_se_table$plate_nom  == current_plate_nom  &
            dil_series_se_table$source_nom == current_source_nom &
            dil_series_se_table$antigen    == current_antigen
        )
        sub <- dil_series_se_table[mask, , drop = FALSE]
        if (nrow(sub) == 0L) {
          if (verbose) message(sprintf(
            "[batch] dil_series_se_table: no rows after filter for %s — skipping accuracy",
            prep_dat_name
          ))
          NULL
        } else {
          sub
        }
      }, error = function(e) {
        message(sprintf("[batch] dil_series_se_table filter error (%s): %s",
                        prep_dat_name, e$message))
        NULL
      })
    }
    
    # ── NEW: compute_dil_series_accuracy — mirrors UI best_fit reactive [2] ──
    dil_series_resp <- if (!is.null(dil_series_response_col)) {
      dil_series_response_col
    } else {
      response_variable
    }
    
    dil_series_acc <- if (!is.null(dil_series_df_filtered)) {
      tryCatch(
        compute_dil_series_accuracy(
          best_fit                   = candidate_best_fit_list[[prep_dat_name]],
          dil_series_df              = dil_series_df_filtered,
          response_col               = dil_series_resp,
          independent_variable       = independent_variable,
          dilution_col               = "dilution",
          fixed_a_result             = fixed_a_result,
          is_log_response            = study_params$is_log_response,
          is_log_concentration       = study_params$is_log_independent,
          undiluted_sc_concentration = antigen_settings$standard_curve_concentration,
          cv_threshold               = 20, #antigen_settings$pcov_threshold,#15,
          lloq_cv_threshold          = 25,
          accuracy_lo                = 80,
          accuracy_hi                = 120,
          verbose                    = verbose
        ),
        error = function(e) {
          message(sprintf("[batch] compute_dil_series_accuracy error (%s): %s",
                          prep_dat_name, e$message))
          dil_series_df_filtered  # return unmodified on error, same as UI [2]
        }
      )
    } else {
      NULL
    }
    
    candidate_best_fit_list[[prep_dat_name]] <- fit_qc_glance(
      best_fit                   = candidate_best_fit_list[[prep_dat_name]],
      response_variable          = response_variable,
      independent_variable       = independent_variable,
      fixed_a_result             = fixed_a_result,
      antigen_settings           = antigen_settings,
      antigen_fit_options        = prepped_data_list[[prep_dat_name]]$antigen_fit_options,
      dil_series_se_plate_source = dil_series_acc,   # ← NEW: was missing [3]
      verbose                    = verbose
    )
    candidate_best_fit_list[[prep_dat_name]] <- tidy.nlsLM(
      best_fit            = candidate_best_fit_list[[prep_dat_name]],
      fixed_a_result      = fixed_a_result,
      model_constraints   = plate_model_constraints_list[[prep_dat_name]],
      antigen_settings    = antigen_settings,
      antigen_fit_options = prepped_data_list[[prep_dat_name]]$antigen_fit_options,
      verbose             = verbose
    )
    current_plate <- antigen_plate_list[[prep_dat_name]]
    current_se <- if (!is.null(se_antigen_table)) {
      lookup_antigen_se(
        se_table             = se_antigen_table,
        study_accession      = unique(current_plate$plate_standard$study_accession),
        experiment_accession = unique(current_plate$plate_standard$experiment_accession),
        source = if ("source_nom" %in% names(current_plate$plate_standard)) {
          unique(current_plate$plate_standard$source_nom)
        } else {
          unique(current_plate$plate_standard$source)
        },
        antigen = unique(current_plate$plate_standard$antigen),
        feature = unique(current_plate$plate_standard$feature)
      )
    } else {
      NA_real_
    }
    current_response_var <- unique(current_plate$plate_standard$assay_response_variable)
    if (length(current_response_var) == 0 || is.na(current_response_var)) current_response_var <- "mfi"
    candidate_best_fit_list[[prep_dat_name]] <- predict_and_propagate_error(
      best_fit        = candidate_best_fit_list[[prep_dat_name]],
      response_var    = current_response_var,
      antigen_plate   = antigen_plate_list[[prep_dat_name]],
      study_params    = study_params,
      se_std_response = current_se,
      verbose         = verbose
    )
    candidate_best_fit_list[[prep_dat_name]] <- gate_samples(
      best_fit          = candidate_best_fit_list[[prep_dat_name]],
      response_variable = current_response_var,
      pcov_threshold    = antigen_settings$pcov_threshold,
      verbose           = verbose
    )
  }
  return(candidate_best_fit_list)
}
# fit_experiment_plate_batch <- function(prepped_data_list_res,
#                                       antigen_plate_list_res,
#                                       model_names,
#                                       study_params,
#                                       se_antigen_table = NULL,
#                                       prog_file        = NULL,   # <-- NEW: IPC progress file
#                                       verbose          = TRUE) {
#   
#   prepped_data_list  <- prepped_data_list_res$prepped_data_list
#   formula_list       <- prepped_data_list_res$formula_list
#   antigen_plate_list <- antigen_plate_list_res$antigen_plate_list
#   
#   plate_model_constraints_list <- list()
#   plate_start_lists            <- list()
#   plate_robust_fit_list        <- list()
#   fit_summary_list             <- list()
#   fit_params_list              <- list()
#   plot_data_list               <- list()
#   candidate_best_fit_list      <- list()
#   best_fit_list                <- list()
#   
#   all_ids <- names(prepped_data_list)
#   n_total <- length(all_ids)
#   
#   if (!is.null(prog_file)) {
#     tryCatch(
#       writeLines(
#         paste0(
#           "Interpolated: starting batch fitting...\n",
#           "Total antigens to fit: ", n_total
#         ),
#         prog_file
#       ),
#       error = function(e) NULL
#     )
#   }
#   
#   for (i in seq_along(all_ids)) {
#     prep_dat_name <- all_ids[[i]]
#     
#     # ── Write progress to file so main-session poller can display it ─────
#     # (replaces the old showNotification call which cannot run in a future)
#     components <- strsplit(prep_dat_name, "\\|")[[1]]
#     progress_text <- paste0(
#       "Interpolated: ", i, " / ", n_total, "\n",
#       "Study:      ", components[1],       "\n",
#       "Experiment: ", components[2],       "\n",
#       "Plate:      ", components[3],       "\n",
#       "Source:     ", components[4],       "\n",
#       "Antigen:    ", components[5]
#     )
#     if (!is.null(prog_file))
#       tryCatch(writeLines(progress_text, prog_file), error = function(e) NULL)
#     message(progress_text)
#     # ─────────────────────────────────────────────────────────────────────
#     
#     if (verbose) print(prep_dat_name)
#     
#     plate_prepped_data   <- prepped_data_list[[prep_dat_name]]
#     formulas             <- formula_list[[prep_dat_name]]
#     response_variable    <- unique(antigen_plate_list[[prep_dat_name]]$plate_standard$assay_response_variable)
#     independent_variable <- unique(antigen_plate_list[[prep_dat_name]]$plate_standard$assay_independent_variable)
#     fixed_a_result       <- antigen_plate_list[[prep_dat_name]]$fixed_a_result
#     antigen_settings     <- antigen_plate_list[[prep_dat_name]]$antigen_settings
#     
#     if (verbose) print(independent_variable)
#     ##from elisa
#     # Compute data range for diagnostics
#     y_range <- range(plate_prepped_data$data[[response_variable]], na.rm = TRUE)
#     
#     if (verbose) {
#       message(sprintf(
#         "\n[batch] %s — response range: [%.4f, %.4f], dynamic_range: %.4f",
#         prep_dat_name, y_range[1], y_range[2], diff(y_range)
#       ))
#     }
#     ##end from elisa
#     
#     plate_model_constraints_list[[prep_dat_name]] <- obtain_model_constraints(
#       data                 = plate_prepped_data$data,
#       formulas             = formulas,
#       independent_variable = independent_variable,
#       response_variable    = response_variable,
#       is_log_response      = TRUE,
#       is_log_concentration = TRUE,
#       antigen_settings     = antigen_settings,
#       max_response         = y_range[2],
#       min_response         = y_range[1],
#       verbose              = verbose
#     )
#     
#     plate_start_lists[[prep_dat_name]] <- make_start_lists(
#       model_constraints = plate_model_constraints_list[[prep_dat_name]],
#       frac_generate     = 0.8,
#       quants            = c(low = 0.2, mid = 0.5, high = 0.8)
#     )
#     
#     plate_robust_fit_list[[prep_dat_name]] <- compute_robust_curves(
#       prepped_data         = plate_prepped_data$data,
#       response_variable    = response_variable,
#       independent_variable = independent_variable,
#       formulas             = formulas,
#       model_constraints    = plate_model_constraints_list[[prep_dat_name]],
#       start_lists          = plate_start_lists[[prep_dat_name]],
#       verbose              = verbose
#     )
#     
#     fit_summary_list[[prep_dat_name]] <- summarize_model_fits(
#       plate_robust_fit_list[[prep_dat_name]],
#       verbose = verbose
#     )
#     
#     fit_params_list[[prep_dat_name]] <- summarize_model_parameters(
#       models_fit_list = plate_robust_fit_list[[prep_dat_name]],
#       level           = 0.95,
#       model_names     = model_names,
#       verbose         = verbose
#     )
#     
#     plot_data_list[[prep_dat_name]] <- get_plot_data(
#       models_fit_list  = plate_robust_fit_list[[prep_dat_name]],
#       prepped_data     = plate_prepped_data$data,
#       fit_params       = fit_params_list[[prep_dat_name]],
#       fixed_a_result   = fixed_a_result,
#       model_names      = model_names,
#       x_var            = independent_variable,
#       y_var            = response_variable,
#       verbose          = verbose
#     )
#     
#     candidate_best_fit_list[[prep_dat_name]] <- select_model_fit_AIC(
#       fit_summary   = fit_summary_list[[prep_dat_name]],
#       fit_robust_lm = plate_robust_fit_list[[prep_dat_name]],
#       fit_params    = fit_params_list[[prep_dat_name]],
#       plot_data     = plot_data_list[[prep_dat_name]],
#       verbose       = verbose
#     )
#     
#     candidate_best_fit_list[[prep_dat_name]] <- fit_qc_glance(
#       best_fit             = candidate_best_fit_list[[prep_dat_name]],
#       response_variable    = response_variable,
#       independent_variable = independent_variable,
#       fixed_a_result       = fixed_a_result,
#       antigen_settings     = antigen_settings,
#       antigen_fit_options  = prepped_data_list[[prep_dat_name]]$antigen_fit_options,
#       verbose              = verbose
#     )
#     
#     candidate_best_fit_list[[prep_dat_name]] <- tidy.nlsLM(
#       best_fit            = candidate_best_fit_list[[prep_dat_name]],
#       fixed_a_result      = fixed_a_result,
#       model_constraints   = plate_model_constraints_list[[prep_dat_name]],
#       antigen_settings    = antigen_settings,
#       antigen_fit_options = prepped_data_list[[prep_dat_name]]$antigen_fit_options,
#       verbose             = verbose
#     )
# 
#     current_plate <- antigen_plate_list[[prep_dat_name]]
#     current_se <- if (!is.null(se_antigen_table)) {
#       lookup_antigen_se(
#         se_table             = se_antigen_table,
#         study_accession      = unique(current_plate$plate_standard$study_accession),
#         experiment_accession = unique(current_plate$plate_standard$experiment_accession),
#         source = if ("source_nom" %in% names(current_plate$plate_standard)) unique(current_plate$plate_standard$source_nom) else unique(current_plate$plate_standard$source),
#         antigen = unique(current_plate$plate_standard$antigen),
#         feature = unique(current_plate$plate_standard$feature)
#       )
#     } else {
#       NA_real_
#     }
#     
#     # Use response_var from data (not hardcoded "mfi") for ELISA compatibility
#     current_response_var <- unique(current_plate$plate_standard$assay_response_variable)
#     if (length(current_response_var) == 0 || is.na(current_response_var)) current_response_var <- "mfi"
#     
#     candidate_best_fit_list[[prep_dat_name]]  <- predict_and_propagate_error(best_fit = candidate_best_fit_list[[prep_dat_name]],
#                                                                              response_var = current_response_var,
#                                                                              antigen_plate = antigen_plate_list[[prep_dat_name]],
#                                                                              study_params = study_params,
#                                                                              se_std_response = current_se,
#                                                                              verbose = verbose)
# 
#     candidate_best_fit_list[[prep_dat_name]] <- gate_samples(best_fit = candidate_best_fit_list[[prep_dat_name]],
#                                                              response_variable = current_response_var,
#                                                              pcov_threshold = antigen_settings$pcov_threshold, 
#                                                              verbose = verbose
#     )
#   }
#   
#   return(candidate_best_fit_list)
# }


create_batch_fit_outputs <- function(batch_fit_res, antigen_plate_list_res) {
  
  antigen_plate_list <- antigen_plate_list_res$antigen_plate_list
  
  # Helper: extract the single feature value from plate standard data
  get_feature_for <- function(nm) {
    plate_info <- antigen_plate_list[[nm]]
    if (!is.null(plate_info$plate_standard) && "feature" %in% names(plate_info$plate_standard)) {
      feat <- unique(plate_info$plate_standard$feature)
      feat <- feat[!is.na(feat) & feat != ""]
      if (length(feat) >= 1) return(feat[1])
    }
    NA_character_
  }
  
  # Helper: ensure feature column exists on a data frame
  ensure_feature <- function(df, nm) {
    if (!is.null(df) && is.data.frame(df) && !"feature" %in% names(df)) {
      df$feature <- get_feature_for(nm)
    }
    df
  }
  
  best_tidy_all <- do.call(rbind, lapply(names(batch_fit_res), function(nm) {
    bt <- batch_fit_res[[nm]]$best_tidy
    if (is.null(bt)) return(NULL)
    ensure_feature(bt, nm)
  }))
  rownames(best_tidy_all) <- NULL
  
  best_glance_all <- dplyr::bind_rows(
    lapply(names(batch_fit_res), function(nm) {
      bg <- batch_fit_res[[nm]]$best_glance
      if (is.null(bg)) return(NULL)
      bg$g <- if ("g" %in% names(bg)) dplyr::coalesce(bg$g, 1) else 1
      bg <- ensure_feature(bg, nm)
      if (!"wavelength" %in% names(bg)) bg$wavelength <- WL_NONE
      bg$wavelength <- normalize_wavelength(bg$wavelength)
      bg
    })
  )
  
  best_pred_all <- dplyr::bind_rows(
    lapply(names(batch_fit_res), function(nm) {
      bp <- batch_fit_res[[nm]]$best_pred
      if (is.null(bp)) return(NULL)
      bp <- ensure_feature(bp, nm)
      if (!"wavelength" %in% names(bp)) bp$wavelength <- WL_NONE
      bp$wavelength <- normalize_wavelength(bp$wavelength)
      bp
    })
  )
  
  best_sample_se_all <- dplyr::bind_rows(
    lapply(names(batch_fit_res), function(nm) {
      ss <- batch_fit_res[[nm]]$sample_se
      if (is.null(ss)) return(NULL)
      ss <- ensure_feature(ss, nm)
      if (!"wavelength" %in% names(ss)) ss$wavelength <- WL_NONE
      ss$wavelength <- normalize_wavelength(ss$wavelength)
      ss
    })
  )
  
  best_standard_all <- dplyr::bind_rows(
    lapply(names(batch_fit_res), function(nm) {
      bg <- batch_fit_res[[nm]]$best_data
      if (is.null(bg)) return(NULL)
      bg$g <- if ("g" %in% names(bg)) dplyr::coalesce(bg$g, 1) else 1
      bg <- ensure_feature(bg, nm)
      if (!"wavelength" %in% names(bg)) bg$wavelength <- WL_NONE
      bg$wavelength <- normalize_wavelength(bg$wavelength)
      bg
    })
  )
  
  # Build best_plate_all
  plate_cols <- c("project_id","study_accession", "experiment_accession", "feature", "source",
                  "plateid", "plate", "nominal_sample_dilution",
                  "assay_response_variable", "assay_independent_variable",
                  "wavelength")
  available_plate_cols <- intersect(plate_cols, names(best_standard_all))
  best_plate_all <- dplyr::distinct(best_standard_all[, available_plate_cols, drop = FALSE])
  
  # ── Diagnostic: output table row counts and wavelength values ──
  for (tbl_nm in c("best_tidy_all", "best_glance_all", "best_pred_all",
                    "best_sample_se_all", "best_standard_all", "best_plate_all")) {
    tbl <- get(tbl_nm)
    n <- if (is.data.frame(tbl)) nrow(tbl) else 0
    wl <- if (is.data.frame(tbl) && "wavelength" %in% names(tbl))
      paste(unique(tbl$wavelength), collapse = ", ") else "NO_COL"
    message(sprintf("[create_batch_fit_outputs] %s: %d rows, wavelengths: %s",
                    tbl_nm, n, wl))
  }
  
  return(list(
    best_tidy_all      = best_tidy_all,
    best_glance_all    = best_glance_all,
    best_pred_all      = best_pred_all,
    best_sample_se_all = best_sample_se_all,
    best_standard_all  = best_standard_all,
    best_plate_all     = best_plate_all,
    antigen_plate_list = antigen_plate_list_res
  ))
}

# add unique identifiers and rename the response variable to be generic for saving
# as well as project_id
process_batch_outputs <- function(batch_outputs, response_var, project_id) {
  
  # Ensure wavelength column exists in all output tables and normalize values
  ensure_wavelength <- function(df) {
    if (!is.null(df) && is.data.frame(df)) {
      if (!"wavelength" %in% names(df)) {
        df$wavelength <- WL_NONE
      } else {
        df$wavelength <- normalize_wavelength(df$wavelength)
      }
    }
    df
  }
  
  batch_outputs$best_tidy_all      <- ensure_wavelength(batch_outputs$best_tidy_all)
  batch_outputs$best_glance_all    <- ensure_wavelength(batch_outputs$best_glance_all)
  batch_outputs$best_pred_all      <- ensure_wavelength(batch_outputs$best_pred_all)
  batch_outputs$best_sample_se_all <- ensure_wavelength(batch_outputs$best_sample_se_all)
  batch_outputs$best_standard_all  <- ensure_wavelength(batch_outputs$best_standard_all)
  batch_outputs$best_plate_all     <- ensure_wavelength(batch_outputs$best_plate_all)
  
  # Ensure cv_x is present; fill NA if somehow missing
  ensure_cv_x <- function(df, context = "") {
    if (!"cv_x" %in% names(df)) {
      message(sprintf("[process_batch_outputs] cv_x missing from %s — filling NA.", context))
      df$cv_x <- NA_real_
    }
    # Clamp extreme values for database storage sanity
    df$cv_x <- ifelse(is.finite(df$cv_x) & df$cv_x > 500, NA_real_, df$cv_x)
    df
  }

  

  batch_outputs$best_pred_all       <- if (!is.null(batch_outputs$best_pred_all) && nrow(batch_outputs$best_pred_all) > 0) {
    ensure_cv_x(batch_outputs$best_pred_all, "best_pred_all")
  } else {
    message("[process_batch_outputs] best_pred_all is NULL or empty — skipping cv_x.")
    batch_outputs$best_pred_all
  }

  batch_outputs$best_sample_se_all  <- if (!is.null(batch_outputs$best_sample_se_all) && nrow(batch_outputs$best_sample_se_all) > 0) {
    ensure_cv_x(batch_outputs$best_sample_se_all, "best_sample_se_all")
  } else {
    message("[process_batch_outputs] best_sample_se_all is NULL or empty — skipping cv_x.")
    batch_outputs$best_sample_se_all
  }
  normalize_output_columns <- function(df, context = "") {
    if (!is.null(df) && is.data.frame(df)) {
      old_names <- names(df)
      new_names <- tolower(old_names)
      
      # Log any columns that were actually renamed
      changed <- old_names != new_names
      if (any(changed)) {
        message(sprintf(
          "[process_batch_outputs] Lowercased %d column(s) in %s: %s",
          sum(changed), context,
          paste(
            sprintf("%s -> %s", old_names[changed], new_names[changed]),
            collapse = ", "
          )
        ))
      }
      
      names(df) <- new_names
      
      # Remove obsolete 'sample_dilution_factor' column if present
      if ("sample_dilution_factor" %in% names(df)) {
        message(sprintf(
          "[process_batch_outputs] Removing obsolete 'sample_dilution_factor' from %s",
          context
        ))
        df$sample_dilution_factor <- NULL
      }
    }
    df
  }
  
  batch_outputs$best_tidy_all      <- normalize_output_columns(batch_outputs$best_tidy_all,      "best_tidy_all")
  batch_outputs$best_glance_all    <- normalize_output_columns(batch_outputs$best_glance_all,    "best_glance_all")
  batch_outputs$best_pred_all      <- normalize_output_columns(batch_outputs$best_pred_all,      "best_pred_all")
  batch_outputs$best_sample_se_all <- normalize_output_columns(batch_outputs$best_sample_se_all, "best_sample_se_all")
  batch_outputs$best_standard_all  <- normalize_output_columns(batch_outputs$best_standard_all,  "best_standard_all")
  batch_outputs$best_plate_all     <- normalize_output_columns(batch_outputs$best_plate_all,     "best_plate_all")
  
  # ensure those out of range are null not inf for database storing.

  if (!is.null(batch_outputs$best_sample_se_all) && nrow(batch_outputs$best_sample_se_all) > 0) {
    batch_outputs$best_sample_se_all <-
      batch_outputs$best_sample_se_all %>%
      mutate(raw_predicted_concentration =
               if_else(is.finite(raw_predicted_concentration),
                       raw_predicted_concentration,
                       NA_real_))

    batch_outputs$best_sample_se_all <- batch_outputs$best_sample_se_all %>%
      dplyr::rename(assay_response = all_of(response_var)) %>%
      dplyr::distinct()
  } else {
    message("[process_batch_outputs] best_sample_se_all is NULL or empty — skipping rename/mutate.")
  }

  if (!is.null(batch_outputs$best_standard_all) && nrow(batch_outputs$best_standard_all) > 0) {
    batch_outputs$best_standard_all <- batch_outputs$best_standard_all %>%
      dplyr::rename(assay_response = all_of(response_var)) %>%
      dplyr::distinct()
  } else {
    message("[process_batch_outputs] best_standard_all is NULL or empty — skipping rename/mutate.")
  }
  
  # ── Ensure feature column exists in all output tables ──
  ensure_feature_col <- function(df, context = "") {
    if (!is.null(df) && is.data.frame(df) && !"feature" %in% names(df)) {
      message(sprintf("[process_batch_outputs] 'feature' missing from %s — filling sentinel.", context))
      df$feature <- FEAT_NONE
    }
    df
  }
  
  batch_outputs$best_tidy_all      <- ensure_feature_col(batch_outputs$best_tidy_all,      "best_tidy_all")
  batch_outputs$best_glance_all    <- ensure_feature_col(batch_outputs$best_glance_all,    "best_glance_all")
  batch_outputs$best_pred_all      <- ensure_feature_col(batch_outputs$best_pred_all,      "best_pred_all")
  batch_outputs$best_sample_se_all <- ensure_feature_col(batch_outputs$best_sample_se_all, "best_sample_se_all")
  batch_outputs$best_standard_all  <- ensure_feature_col(batch_outputs$best_standard_all,  "best_standard_all")
  batch_outputs$best_plate_all     <- ensure_feature_col(batch_outputs$best_plate_all,     "best_plate_all")

  
  # add project_id
 ensure_project_id <- function(df, project_id) {
    if (!is.null(df) && is.data.frame(df) && !"project_id" %in% names(df)) {
      df$project_id <- as.numeric(project_id)
    }
    df
 }
 
 batch_outputs$best_tidy_all      <- ensure_project_id(batch_outputs$best_tidy_all,      project_id)
 batch_outputs$best_glance_all    <- ensure_project_id(batch_outputs$best_glance_all,    project_id)
 batch_outputs$best_pred_all      <- ensure_project_id(batch_outputs$best_pred_all,      project_id)
 batch_outputs$best_sample_se_all <- ensure_project_id(batch_outputs$best_sample_se_all, project_id)
 batch_outputs$best_standard_all  <- ensure_project_id(batch_outputs$best_standard_all,  project_id)
 batch_outputs$best_plate_all     <- ensure_project_id(batch_outputs$best_plate_all,     project_id)

  # ── Strip internal routing columns that do not exist in the DB ──
  internal_cols <- c("plate_nom", "source_nom", "source_original")

  batch_outputs <- lapply(batch_outputs, function(x) {
    if (is.data.frame(x)) {
      drop <- intersect(names(x), internal_cols)
      if (length(drop) > 0) {
        message(sprintf("[process_batch_outputs] Stripping internal columns: %s",
                        paste(drop, collapse = ", ")))
        x <- x[, !names(x) %in% internal_cols, drop = FALSE]
      }
      # Normalize feature: NA/empty → '__none__' sentinel for consistent NK matching
      if ("feature" %in% names(x)) {
        x$feature <- normalize_feature(x$feature)
      }
      x
    } else x
  })

  return(batch_outputs)
}

