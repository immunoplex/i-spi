### Functions for nonlinear standard curve fitting and visualization
## R package by Seamus, Scot, and Annie

### ---- HELPERS: ----

#' Resolve the assay response column name from a data frame
#' Determines the correct response column name by checking the
#' assay_response_variable metadata column in the data frame.
#' Falls back to "mfi" for backward compatibility with bead array data.
#'
#' @param df Data frame (standards, blanks, samples, etc.)
#' @param default Fallback column name (default: "mfi")
#' @return Character string: the column name to use for response values
resolve_response_col <- function(df, default = "mfi") {
  if (!is.null(df) && "assay_response_variable" %in% names(df)) {
    rv <- unique(df$assay_response_variable)
    rv <- rv[!is.na(rv) & rv != ""]
    if (length(rv) == 1 && rv %in% names(df)) return(rv)
  }
  # Fall back to default if the column exists
  if (!is.null(df) && default %in% names(df)) return(default)
  # Last resort: find any response-like column
  if (!is.null(df)) {
    candidates <- intersect(c("mfi", "absorbance", "fluorescence", "od"), names(df))
    if (length(candidates) > 0) return(candidates[1])
  }
  return(default)
}

#' Return an all-NA row for a grouping
.empty_se_row <- function(grouping, grouping_cols) {
  data.frame(
    grouping,
    median_se        = NA_real_,
    n_dilutions_used = 0L,
    n_plates         = 0L,
    total_obs        = 0L,
    stringsAsFactors = FALSE,
    row.names        = NULL
  )
}


#' small helper (if not already defined)
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---- attach_grouping_keys ----
#' Ensure wavelength and feature columns exist on any output data frame,
#' sourced from the best_data that produced it.
#' Call this at the end of fit_qc_glance, tidy.nlsLM, and predict_and_propagate_error
#' so every per-regression output carries the full natural key.
attach_grouping_keys <- function(df, best_data, context = "") {
  if (!"wavelength" %in% names(df)) {
    wl <- if ("wavelength" %in% names(best_data)) unique(best_data$wavelength)[1] else WL_NONE
    df$wavelength <- wl
  }
  df$wavelength <- normalize_wavelength(df$wavelength)

  if (!"feature" %in% names(df)) {
    feat <- if ("feature" %in% names(best_data)) unique(best_data$feature)[1] else FEAT_NONE
    df$feature <- feat
  }

  if (context != "") {
    message(sprintf("[attach_grouping_keys] %s: wavelength=%s, feature=%s",
                    context,
                    paste(unique(df$wavelength), collapse = ","),
                    paste(unique(df$feature), collapse = ",")))
  }
  df
}

# ---- obtain_lower_constraint ----
#' This function returns the lower and upper constraints for an antigen given its method
#' methods are  ['default','user_defined','range_of_blanks', 'geometric_mean_of_blanks']
obtain_lower_constraint <- function(dat, antigen, study_accession, experiment_accession,
                                    plate, plateid, plate_blanks, antigen_constraints,
                                    response_col = NULL) {

  # Resolve the response column name dynamically (mfi for bead array, absorbance for ELISA)
  if (is.null(response_col)) response_col <- resolve_response_col(dat)

  # Handle case where antigen_constraints is a dataframe with multiple rows
  # Take the first row to ensure scalar values for all constraint parameters
  if (is.data.frame(antigen_constraints) && nrow(antigen_constraints) > 1) {
    warning(paste("Multiple constraint rows found for antigen:", antigen,
                  "- using first row. Consider deduplicating antigen_constraints."))
    antigen_constraints <- antigen_constraints[1, , drop = FALSE]
  }

  # Extract scalar values from antigen_constraints to avoid "condition has length > 1" errors
  # Use helper function to safely extract first non-NA value
  safe_extract <- function(x, default = NA) {
    if (is.null(x) || length(x) == 0) return(default)
    x <- x[!is.na(x)]
    if (length(x) == 0) return(default)
    return(x[1])
  }

  constraint_method <- safe_extract(trimws(antigen_constraints$l_asy_constraint_method), "default")
  l_asy_min <- safe_extract(antigen_constraints$l_asy_min_constraint, 0)
  l_asy_max <- safe_extract(antigen_constraints$l_asy_max_constraint, NA)
  std_curve_conc <- safe_extract(antigen_constraints$standard_curve_concentration, 10000)
  pcov_thresh <- safe_extract(antigen_constraints$pcov_threshold, 20)

  # Blank SE calculation using dynamic response column
  blank_response_col <- resolve_response_col(plate_blanks, default = response_col)
  if (nrow(plate_blanks) > 1) {
    se_blank_response <- sd(plate_blanks[[blank_response_col]], na.rm = TRUE) /
                         sqrt(sum(!is.na(plate_blanks[[blank_response_col]])))
  } else {
    se_blank_response <- 0
  }

  if (constraint_method == "user_defined") {
    l_asy_constraints <- list(
      study_accession = study_accession,
      experiment_accession = experiment_accession,
      plate = plate,
      antigen = antigen,
      l_asy_min_constraint = l_asy_min,
      l_asy_max_constraint = l_asy_max,
      l_asy_constraint_method = constraint_method,
      std_error_blank = se_blank_response,
      standard_curve_concentration = std_curve_conc,
      pcov_threshold = pcov_thresh
    )
  } else if (constraint_method == "default") {
    l_asy_constraints <- list(
      study_accession = study_accession,
      experiment_accession = experiment_accession,
      plate = plate,
      antigen = antigen,
      l_asy_min_constraint = 0, # lower bound is set to 0
      l_asy_max_constraint = max(dat[[response_col]], na.rm = TRUE),
      l_asy_constraint_method = constraint_method,
      std_error_blank = se_blank_response,
      standard_curve_concentration = std_curve_conc,
      pcov_threshold = pcov_thresh
    )
  } else if (constraint_method == "range_of_blanks") {
    l_asy_constraints <- list(
      study_accession = study_accession,
      experiment_accession = experiment_accession,
      plate = plate,
      antigen = antigen,
      l_asy_min_constraint = min(plate_blanks[[blank_response_col]], na.rm = TRUE),
      l_asy_max_constraint = max(plate_blanks[[blank_response_col]], na.rm = TRUE),
      l_asy_constraint_method = constraint_method,
      std_error_blank = se_blank_response,
      standard_curve_concentration = std_curve_conc,
      pcov_threshold = pcov_thresh
    )
  } else if (constraint_method == 'geometric_mean_of_blanks') {
    geometric_mean <- exp(mean(log(plate_blanks[[blank_response_col]]), na.rm = TRUE))
    l_asy_constraints <- list(
      study_accession = study_accession,
      experiment_accession = experiment_accession,
      #plateid = plateid,
      plate = plate,
      antigen = antigen,
      l_asy_min_constraint = geometric_mean,
      l_asy_max_constraint = geometric_mean,
      l_asy_constraint_method = constraint_method,
      std_error_blank = se_blank_response,
      standard_curve_concentration = std_curve_conc,
      pcov_threshold = pcov_thresh
    )
  } else {
    return(NULL)
  }
  return(l_asy_constraints)
}

# ---- get_study_exp_antigen_plate_param ----
# 1.	get_study_exp_antigen_plate_params return standard curve concentration for undiluted standard curve sample.
# 2. get_study_params return study parameters, such as the blank options, prozone correction, aggregate repeated measures etc
# Result is a number (10000 for example) and is passed in to compute concentration (undiluted_sc_concentration).
get_study_exp_antigen_plate_param <- function(l_asy_constraints) {
  undiluted_sc_concentration <- l_asy_constraints$standard_curve_concentration
  return(undiluted_sc_concentration)
}


# ---- compute concentration column  with an option to log10 the concentration ----
# the independent variable is given to be a string (which is usually concentration)
# read in the undiluted standard curve sample's concentration value
compute_concentration <- function(data,
                                  undiluted_sc_concentration,
                                  independent_variable,
                                  is_log_concentration = TRUE) {
  independent_variable <- unique(independent_variable)
  data[[independent_variable]] <- (1 / data$dilution) * undiluted_sc_concentration

  if (is_log_concentration) {
    data[[independent_variable]] <- log10(data[[independent_variable]])
  }

  return(data)
}

# ---- Correct for the Prozone Effect ----
### Prozone Correction function
correct_prozone <- function(stdframe = NULL, prop_diff = NULL, dil_scale = NULL,
                            response_variable = "mfi",
                            independent_variable = "concentration",
                            verbose = TRUE) {
  ## stdframe must contain the columns labelled mfi and log_dilution for one set of standard curve data i.e. one dilution series
  ##
  ### correct for prozone effect by correcting the values past the peak by raising them to a neutral asymptote based on an assumed measured C90.
  ## ACS Meas. Sci. Au 2024, 4, 4, 452–458
  ## https://pubs.acs.org/doi/10.1021/acsmeasuresciau.4c00010
  ## The hook effect, also known as the prozone effect, is a phenomenon that commonly occurs in
  ## antibody-based sandwich immunoassay biosensors. (1,2) In a typical immunoassay, the binding
  ## of antibodies to the analyte leads to the formation of a visible signal, such as a color
  ## change or a fluorescent signal where the intensity of this signal is directly proportional
  ## to the concentration of the analyte in the sample being tested. (3,4) However, the prozone
  ## effect happens when the concentration of the analyte becomes so high that it exceeds the
  ## capacity of the antibodies in the assay. (5) In this situation, the excess analyte can
  ## saturate or overwhelm the binding sites on the antibodies, and as a result, the sensor
  ## response is inhibited, leading to a false-low or even false-negative test result. (6)
  ##
  ## 1 Chen, W.; Shan, S.; Peng, J.; Liu, D.; Xia, J.; Shao, B.; Lai, W. Sensitive and hook effect-free lateral flow assay integrated with cascade signal transduction system. Sens. Actuators, B 2020, 321, 128465,  DOI: 10.1016/j.snb.2020.128465
  ## 2 Selby, C. Interference in immunoassay. Ann. Clin. Biochem. 1999, 36, 704– 721,  DOI: 10.1177/000456329903600603
  ## 3 Hessick, E. R.; Dannemiller, K.; Gouma, P. Development of a Novel Lateral Flow Immunoassay for Detection of Harmful Allergens Found in Dust. Meet. Abstr. 2022, 241, 2333,  DOI: 10.1149/ma2022-01552333mtgabs
  ## 4 Poudineh, M.; Maikawa, C. L.; Ma, E. Y.; Pan, J.; Mamerow, D.; Hang, Y.; Baker, S. W.; Beirami, A.; Yoshikawa, A.; Eisenstein, M. A fluorescence sandwich immunoassay for the real-time continuous detection of glucose and insulin in live animals. Nat. Biomed. Eng. 2020, 5, 53– 63,  DOI: 10.1038/s41551-020-00661-1
  ## 5 Bravin, C.; Amendola, V. Wide range detection of C-Reactive protein with a homogeneous immunofluorimetric assay based on cooperative fluorescence quenching assisted by gold nanoparticles. Biosens. Bioelectron. 2020, 169, 112591,  DOI: 10.1016/j.bios.2020.112591
  ## 6 Raverot, V.; Perrin, P.; Chanson, P.; Jouanneau, E.; Brue, T.; Raverot, G. Prolactin immunoassay: does the high-dose hook effect still exist?. Pituitary 2022, 25, 653– 657,  DOI: 10.1007/s11102-022-01246-8
  ##

  ## following modelling of prozone effects in:
  ## Development of an experimental method to overcome the hook effect in sandwich-type lateral flow immunoassays guided by computational modelling
  ## Sensors and Actuators B: Chemical Volume 324, 1 December 2020, 128756
  ## and
  ## Hook effect detection and detection-range-controllable one-step immunosensor for inflammation monitoring
  ## Sensors and Actuators B: Chemical Volume 304, 1 February 2020, 127408

  # 0. Filter out NA mfi and log dilution
  response_variable <- unique(response_variable)
  stdframe <- stdframe[!is.na(stdframe[[response_variable]]) & !is.na(stdframe[[independent_variable]]),]


  # 1. identify the highest mfi and corresponding log_dilution in stdframe
  max_response <- max(stdframe[[response_variable]], na.rm = TRUE)
  logc_at_max_response <- max(stdframe[stdframe[[response_variable]]==max_response, ][[independent_variable]])
  if (verbose) cat("Peak MFI =", max_response, "at concentration =", logc_at_max_response, "\n")

  post_peak <- stdframe[[independent_variable]] > logc_at_max_response
  if (verbose) cat("Number of points beyond the peak:", sum(post_peak), "\n")
  # 2. identify the mfis lower than the max_response at higher concentrations and dampen the delta mfis to compensate for
  stdframe[stdframe[[independent_variable]] > logc_at_max_response, ][[response_variable]] <- max_response +
    (
      (max_response - stdframe[stdframe[[independent_variable]] > logc_at_max_response, ][[response_variable]]) * prop_diff /
        ((stdframe[stdframe[[independent_variable]] > logc_at_max_response, ][[independent_variable]]-logc_at_max_response) * dil_scale)
    )
  return(stdframe)
  ### end correct for prozone effect
}

# ---- Blank Handling ----

## get the standard error of the blanks for later use
get_blank_se <- function(antigen_settings) {
  std_error_blank <- antigen_settings$std_error_blank
  return(std_error_blank)
}

#helper function to compute geometric mean
geom_mean <- function (x, na.rm = TRUE) {
  ans <- exp(mean(log(x), na.rm = TRUE))
  ans
}

# Include Blanks as an extra point in the standard curve data
# Estimation of the standard curve takes into account the mean of the background
# of the values as another point of the standard curve. The median fluorescence intensity and the
# expected concentration for this new point by analyte is estimated as follows:
# MFI: geometric mean value of the blank controls.
# EC: the minimum expected concentration value of the standard points divided by 2.
# On the log dilution scale we subtract log10(2) which is equivalent to dividing by 2.
include_blanks_conc <- function(blank_data, data, response_variable, independent_variable = "concentration") {
  data <- data[, !(names(data) %in% c("dilution_factor", "log_dilution"))]

  #plateid, antigen, response_variable, independent_variable = "concentration") {
  # # filter the plate and antigen from the buffer and standard curve data
  # buffer_data_filtered <- buffer_data[buffer_data$plateid == plateid & buffer_data$antigen == antigen, ]
  # std_curve_data_filtered <- std_curve_data[std_curve_data$plateid == plateid & std_curve_data$antigen == antigen,]
  #
  # # calculate the geometric mean of the buffer/blanks by analyte
  # if (is_log_response) {
  #   response_blank <- log10(geom_mean(blank_data[[response_variable]]))
  # } else {
  response_blank <- geom_mean(blank_data[[response_variable]])

  # }



  # calculate the log dilution of the buffer (Dr Lumi uses (1/(min(dilution_factor))/2)
  # cat("\nIn include blanks\n")
  # print(head(std_curve_data_filtered))
  # the minimum expected concentration value of the standard points divided by 2.
  min_concentration <- min(data[[independent_variable]], na.rm = T)
  conc_blank <- min_concentration - log10(2)

  #conc_blank <- min_concentration / 2
  #
  # min_log_dilution <- min(std_curve_data_filtered$log_dilution)
  # log_dilution_buffer <- min_log_dilution - log10(2)
  # min_dilution_factor <- min(std_curve_data_filtered$dilution)
  #
  #data$antibody_mfi <- data$mfi

  # Create new blank/mean point
  new_point <- tibble::tibble(
    project_id = unique(data$project_id),
    study_accession = unique(data$study_accession),
    experiment_accession = unique(data$experiment_accession),
    feature = unique(data$feature),
    source = unique(data$source),
    plateid = unique(data$plateid),
    plate  =  unique(data$plate),
    stype = "B", # blanks are B and recognized in standard curve plot as such
    nominal_sample_dilution = unique(data$nominal_sample_dilution),
    sampleid = "blank_mean",
    well = "geometric_mean_blank",
    dilution = NA_real_,
    antigen = unique(data$antigen),
    !!response_variable := response_blank,
    assay_response_variable = unique(data$assay_response_variable),
    assay_independent_variable = unique(data$assay_independent_variable),
    concentration = conc_blank

  )
  
  # Carry forward source_nom and wavelength if present in the data
  if ("source_nom" %in% names(data)) {
    new_point$source_nom <- unique(data$source_nom)[1]
  }
  if ("wavelength" %in% names(data)) {
    new_point$wavelength <- unique(data$wavelength)[1]
  }

  # if plate nom is present place in correct spot
  if ("plate_nom" %in% names(data)) {
    new_point$plate_nom <- unique(data$plate_nom)[1]

    nm <- names(new_point)
    i  <- match("assay_independent_variable", nm)

    new_point <- new_point[
      , c(nm[1:i], "plate_nom", nm[(i + 1):(length(nm) - 1)])
    ]
  }


  # new_point <- data.frame(
  #   plateid = unique(data$plateid),
  #   antigen = unique(data$antigen),
  #   mfi = response_blank,
  #   study_accession = unique(data$study_accession),
  #   experiment_accession = unique(data$experiment_accession),
  #   well = "geometric_mean_buffer",
  #   stype = unique(blank_data$stype),
  #   sampleid = "buffer_mean",
  #   source = unique(data$source),
  #   dilution = NA_real_,
  #   pctaggbeads = NA_real_,
  #   samplingerrors = NA_character_,
  #   n = NA_integer_,
  #   feature = unique(data$feature),
  #   predicted_mfi = response_blank,
  #   #selected_str = unique(data$selected_str),
  #   concentration = conc_blank
  # )


  # cat("\nnames of standard curve filtered\n")
  # print(names(std_curve_data_filtered))
  # cat("\n names of  new point\n")
  # print(names(new_point))
  #
  
  
  data_with_blank <- rbind(data, new_point)
  return(data_with_blank)

}

# The geometric mean or a multiple of the geometric mean of the blank controls is subtracted from all the standard points if that options are selected.
# pass in the buffer data, standards as data
# blank_option: ignored,included,subtracted,subtracted_3x,subtracted_10x
perform_blank_operation <- function(blank_data, data, response_variable, independent_variable, is_log_response, blank_option = "ignored", verbose = TRUE) {
  if (verbose) {
    message("Blank Option Used: ", blank_option)
  }
  valid_options <- c("ignored","included","subtracted","subtracted_3x","subtracted_10x")

  if (!(blank_option %in% valid_options)) {
    message("Invalid value for blank_option. Must be one of: 'ignored', 'included', 'subtracted', 'subtracted_3x', or 'subtracted_10x'.")
    return(data)
  }

  if (blank_option != "ignored" && (is.null(blank_data) || nrow(blank_data) == 0)) {
    message("Blank data must be supplied when blank_option is not 'ignored'.")
    return(data)
  }
  if (blank_option == "included") {
    data <- include_blanks_conc(blank_data = blank_data, data = data, response_variable = response_variable,
                                independent_variable = independent_variable)

    if (verbose) message("Geometric mean of blanks included as an extra point in the standard curve.")

  }
  if (blank_option %in% c("subtracted", "subtracted_3x", "subtracted_10x")) {

    factor <- switch(blank_option,
                     "subtracted" = 1,
                     "subtracted_3x" = 3,
                     "subtracted_10x" = 10)

    # if (is_log_response) {
    #   data_linear <- 10^(data[[response_variable]])
    #   blank_linear <- geom_mean(blank_data[[response_variable]])
    #
    #   adjusted_lin <- data_linear - factor * blank_linear
    #
    #   smallest_pos <- min(data_linear[data_linear > 0], na.rm = TRUE)
    #   floor_val <- smallest_pos * 0.1   # e.g. 10% of smallest positive observed
    #
    #   adjusted_lin[adjusted_lin <= 0] <- floor_val
    #   # if (any(adjusted_lin <= 0, na.rm = TRUE)) {
    #   #   message("Setting values <= 0 after subtraction to a small positive number before log-transforming.")
    #   #   adjusted_lin[adjusted_lin <= 0] <- min(adjusted_lin[adjusted_lin > 0], na.rm = TRUE) * 1e-3
    #   # }
    #   # adjusted_lin[adjusted_lin < 0] <- 0  # prevent negatives
    #
    #   # re-log after subtraction
    #   data[[response_variable]] <- log10(adjusted_lin)
    #   if (verbose) {
    #     message("Performed blank subtraction (×", factor, ") in linear space, then log-transformed back.")
    #   }
    # } else {
    # Direct subtraction in linear space
    blank_mean <- geom_mean(blank_data[[response_variable]])
    dat <- data
    data[[response_variable]] <- data[[response_variable]] - factor * blank_mean

    if (is_log_response) {
      data[data[[response_variable]] < 0, response_variable] <- 1
    } else {
      data[data[[response_variable]] < 0, response_variable] <- 0
    }
    if (verbose) {
      message("Performed blank subtraction (×", factor, ") in linear space.")
    }
    # }



  }


  return(data)
}

# ---- compute_log_response logs the response variable for fitting if flag is set to true ----
# the response variable is set to be a given string such as mfi
# is log response is a boolean flag to decide to take log response if true or not if false.
compute_log_response <- function(data, response_variable, is_log_response = TRUE) {
  if (is_log_response) {
    data[[response_variable]] <- log10(data[[response_variable]])
  }

  return(data)
}

### ---- Model Fitting ----
# RENAMED from test_fixed_lower_asymptote → resolve_fixed_lower_asymptote
# Responsibility: Given the constraint list, determine WHETHER 'a' should be
# fixed (min == max) and return the raw constraint value, or NULL if free.
# Called by: select_antigen_plate() in db_functions.R
resolve_fixed_lower_asymptote <- function(l_asy_constraints) {
  if (l_asy_constraints$l_asy_min_constraint == l_asy_constraints$l_asy_max_constraint) {
    fixed_constraint <- l_asy_constraints$l_asy_min_constraint
    return(fixed_constraint)
  } else {
    return(NULL)
  }
}
# Responsibility: Validate that a raw fixed_a_result value is safe to use
# (positive, finite, non-zero) before it is log10-transformed downstream.
# Returns the original value if valid, NULL if it should be treated as free.
# Called by: predict_and_propagate_error(), select_model_formulas() callers
validate_fixed_lower_asymptote <- function(fixed_a_result_raw, verbose = TRUE) {
  if (is.null(fixed_a_result_raw)) {
    return(NULL)
  }
  
  if (!is.numeric(fixed_a_result_raw) || length(fixed_a_result_raw) != 1) {
    if (verbose) message(
      "[validate_fixed_lower_asymptote] fixed_a_result is not a scalar numeric — treating as NULL (free)."
    )
    return(NULL)
  }
  
  if (!is.finite(fixed_a_result_raw)) {
    if (verbose) message(sprintf(
      "[validate_fixed_lower_asymptote] fixed_a_result = %s is not finite — treating as NULL (free).",
      as.character(fixed_a_result_raw)
    ))
    return(NULL)
  }
  
  if (fixed_a_result_raw <= 0) {
    if (verbose) message(sprintf(
      "[validate_fixed_lower_asymptote] fixed_a_result = %.6f is <= 0; log10() would be undefined or extreme — treating as NULL (free lower asymptote).",
      fixed_a_result_raw
    ))
    return(NULL)
  }
  
  # Value is safe: positive and finite
  return(fixed_a_result_raw)
}

generate_start <- function(bounds, frac = 0.90) {
  start_offset <- (1-frac)/2 # 0.05 by default
  lower <- bounds$lower
  upper <- bounds$upper
  if (!all(names(lower) == names(upper))) {
    stop("Lower and upper bounds must have identical parameter names")
  }

  # start_lower <-  lower  * (1 + start_offset) #frac * (upper - lower)
  # start_upper <- upper * (1- start_offset) #(1- frac) * (upper - lower)

  width <- upper - lower

  start_lower <- lower + start_offset * width
  start_upper <- upper - start_offset * width

  start_list <- list(
    start_lower = start_lower,
    start_upper = start_upper

  )
  return(start_list)

}

## Functions for upper and lower bounds in fitting
.make_bounds <- function(param_names, lower_vals, upper_vals) {
  if (length(param_names) != length(lower_vals) || length(param_names) != length(upper_vals)) {
    stop("Lengths must match")
  }
  # lower <- setNames(as.numeric(lower_vals), param_names)
  # upper <- setNames(as.numeric(upper_vals), param_names)
  list(lower = lower_vals, upper = upper_vals)
}

#Utility to compute middle-90% bounds of y
.y_mid_bounds <- function(ymin, ymax) {
  span <- ymax - ymin
  low <- ymin + 0.05 * span
  high <- ymin + 0.95 * span
  c(low = low, high = high)
}

# obtain the free parameters names in each model
# dependent variable is the assay response (mfi for bead array, absorbance for ELISA)
# independent variable is concentration; free variables are returned in alphabetical order
obtain_free_variables <- function(formulas, dep = "mfi", indep = "concentration") {
  lapply(formulas, function(f) {
    vars <- all.vars(f)
    sort(setdiff(vars, c(dep, indep)))
  })
}

#obtain the response variable name for all the models.
# all formulas should have the same response variable
obtain_response_variable <- function(formulas) {
  response_vars <- sapply(formulas, function(f) {
    vars <- all.vars(f)
    # response_idx <- attr(stats::terms(f), "response")
    # vars[response_idx]
    as.character(f[[2]])
  },
  USE.NAMES = TRUE)

  response_variable <- unique(response_vars)
  return(response_variable)
}

# ---- obtain_model_constraints: response variable based on formula, researcher provides independent variable for obtain_free_variables called internally ----
obtain_model_constraints <- function(data, formulas,
                                     response_variable,
                                     independent_variable,
                                     is_log_response,
                                     is_log_concentration,
                                     antigen_settings,
                                     max_response,
                                     min_response,
                                     verbose = TRUE) {
  
  # Build the profile ONCE, share across all model variants
  constraint_profile <- adaptive_constraint_profile(
    data               = data,
    response_variable  = response_variable,
    is_log_response    = is_log_response,
    antigen_settings   = antigen_settings
  )
  
  if (verbose) {
    message(sprintf(
      "[obtain_model_constraints] scale_class=%s, dynamic_range=%.3f, slope=[%.3f, %.3f], g=[%.2f, %.2f]",
      constraint_profile$scale_class,
      constraint_profile$dynamic_range,
      constraint_profile$slope_min, constraint_profile$slope_max,
      constraint_profile$g_min, constraint_profile$g_max
    ))
  }
  
  free_variables <- obtain_free_variables(formulas = formulas, dep = response_variable , indep = independent_variable)
  # nls 5
  Y5_nls_constraint <- Y5_safe_constraint(data = data,
                                          y_min = min_response,
                                          y_max = max_response,
                                          Y5_formula = formulas$Y5,
                                          Y5_free_vars = free_variables$Y5,
                                          is_log_response = is_log_response,
                                          is_log_concentration = is_log_concentration,
                                          antigen_settings = antigen_settings,
                                          constraint_profile = constraint_profile
  )
  # drda_5 Hill
  Yd5_constraint <- Yd5_safe_constraint(data = data, y_min = min_response, y_max = max_response, Yd5_formula = formulas$Yd5,
                                        Yd5_free_vars = free_variables$Yd5, is_log_response = is_log_response, is_log_concentration = is_log_concentration,
                                        antigen_settings = antigen_settings,
                                        constraint_profile = constraint_profile)
  # nls_4
  Y4_nls_constraint <- Y4_safe_constraint(data = data, y_min = min_response, y_max = max_response, Y4_formula = formulas$Y4,
                                          Y4_free_vars = free_variables$Y4, is_log_response = is_log_response, is_log_concentration = is_log_concentration,
                                          antigen_settings = antigen_settings,
                                          constraint_profile = constraint_profile)
  #nlslm_4 Hill
  Yd4_constraint <- Yd4_safe_constraint(data = data, y_min = min_response, y_max = max_response, Yd4_formula = formulas$Yd4,
                                        Yd4_free_vars = free_variables$Yd4, is_log_response = is_log_response, is_log_concentration = is_log_concentration,
                                        antigen_settings = antigen_settings,
                                        constraint_profile = constraint_profile)
  #Ygomp4
  Ygomp4_constraint <- Ygomp4_safe_constraint(data = data, y_min = min_response, y_max = max_response, Ygomp4_formula = formulas$Ygomp4,
                                              Ygomp4_free_vars = free_variables$Ygomp4, is_log_response = is_log_response, is_log_concentration = is_log_concentration,
                                              antigen_settings = antigen_settings,
                                              constraint_profile = constraint_profile)
  constraint_models <- list(
    Y5 = Y5_nls_constraint,
    Yd5 = Yd5_constraint,
    Y4 = Y4_nls_constraint,
    Yd4 = Yd4_constraint,
    Ygomp4 = Ygomp4_constraint
  )
  
  # Attach profile for downstream use (start list generation, diagnostics)
  attr(constraint_models, "constraint_profile") <- constraint_profile
  
  if (verbose) {
    print(constraint_models)
  }

  return(constraint_models)
}
# ---- make start lists ----
make_start_lists <- function(model_constraints,
                             frac_generate = 0.8,
                             quants = c(low = 0.2, mid = 0.5, high = 0.8)) {
  profile <- attr(model_constraints, "constraint_profile")
  
  start_lists <- list()
  for (model_name in names(model_constraints)) {
    mc     <- model_constraints[[model_name]]
    lower  <- mc$lower
    upper  <- mc$upper
    params <- names(lower)
    
    # Generate multiple start points via Latin Hypercube-style sampling
    # For low-response, add extra starts near the data midpoints
    n_starts <- if (!is.null(profile) && profile$scale_class == "low") 5 else 3
    
    starts <- list()
    for (i in seq_len(n_starts)) {
      frac <- (i - 0.5) / n_starts  # spread across [0,1]
      start_i <- setNames(
        lower + frac * (upper - lower),
        params
      )
      
      # For parameter b (slope), bias toward smaller values for low-response
      if ("b" %in% params && !is.null(profile) && profile$scale_class == "low") {
        b_range <- upper["b"] - lower["b"]
        start_i["b"] <- lower["b"] + b_range * frac^2  # quadratic bias toward low
      }
      
      starts[[i]] <- as.list(start_i)
    }
    
    start_lists[[model_name]] <- starts
  }
  
  return(start_lists)
}


# ---- compute robust curves ----
compute_robust_curves <- function(prepped_data,
                                  response_variable,
                                  independent_variable,
                                  formulas,
                                  model_constraints,
                                  start_lists,
                                  verbose = TRUE) {
  
  profile <- attr(model_constraints, "constraint_profile")
  is_low_response <- !is.null(profile) && profile$scale_class == "low"
  
  models_fit_list <- list()
  
  for (formula_name in names(formulas)) {
    name   <- formula_name
    lower  <- model_constraints[[name]]$lower
    upper  <- model_constraints[[name]]$upper
    starts <- start_lists[[name]]
    
    if (verbose) message("\n Trying model: ", name)
    
    best_fit   <- NULL
    best_aic   <- Inf
    
    # Try each start list
    for (sl in starts) {
      fit <- tryCatch({
        minpack.lm::nlsLM(
          formula   = formulas[[name]],
          data      = prepped_data,
          start     = sl,
          lower     = lower,
          upper     = upper,
          control   = nls.lm.control(
            maxiter = if (is_low_response) 200 else 100,
            ftol    = if (is_low_response) 1e-8 else 1e-6,
            ptol    = if (is_low_response) 1e-8 else 1e-6
          )
        )
      }, error = function(e) NULL)
      
      if (!is.null(fit)) {
        current_aic <- tryCatch(AIC(fit), error = function(e) Inf)
        if (is.finite(current_aic) && current_aic < best_aic) {
          best_aic <- current_aic
          best_fit <- fit
        }
      }
    }
    
    # FALLBACK STRATEGY 1: Relax bounds by 50% for low-response
    if (is.null(best_fit) && is_low_response) {
      if (verbose) message("  [fallback-1] Relaxing bounds for ", name)
      
      relaxed_lower <- lower - 0.5 * abs(lower)
      relaxed_upper <- upper + 0.5 * abs(upper)
      # Keep slope positive
      if ("b" %in% names(relaxed_lower)) relaxed_lower["b"] <- max(relaxed_lower["b"], 1e-6)
      
      mid_start <- as.list((relaxed_lower + relaxed_upper) / 2)
      
      best_fit <- tryCatch({
        minpack.lm::nlsLM(
          formula = formulas[[name]],
          data    = prepped_data,
          start   = mid_start,
          lower   = relaxed_lower,
          upper   = relaxed_upper,
          control = nls.lm.control(maxiter = 300, ftol = 1e-10, ptol = 1e-10)
        )
      }, error = function(e) NULL)
    }
    
    # FALLBACK STRATEGY 2: Use port algorithm (base nls) which handles
    # box constraints differently
    if (is.null(best_fit) && is_low_response) {
      if (verbose) message("  [fallback-2] Trying port algorithm for ", name)
      mid_start <- as.list((lower + upper) / 2)
      
      best_fit <- tryCatch({
        nls(
          formula   = formulas[[name]],
          data      = prepped_data,
          start     = mid_start,
          algorithm = "port",
          lower     = lower,
          upper     = upper,
          control   = nls.control(maxiter = 200, tol = 1e-6)
        )
      }, error = function(e) NULL)
    }
    
    if (!is.null(best_fit)) {
      models_fit_list[[name]] <- list(fit = best_fit, data = prepped_data)
      if (verbose) message("  ✓ ", name, " converged (AIC=", round(AIC(best_fit), 2), ")")
    } else {
      models_fit_list[[name]] <- list(fit = NULL, data = prepped_data)
      if (verbose) message("  ✗ ", name, " failed to converge")
    }
  }
  
  return(models_fit_list)
}
### ---- select NLSLM AIC ----
select_nlsLM_aic <- function(prepped_data,
                             response_variable,
                             independent_variable,
                             formula,
                             lower_model_constraints,
                             upper_model_constraints,
                             start_lists,  verbose = TRUE) {


  fits <- lapply(names(start_lists), function(nm) {
    if (verbose) message("Fitting with start: ", nm)

    fit_obj <- tryCatch(
      {
        nlsLM_fit(
          formula      = formula,
          data         = prepped_data,
          start_values = start_lists[[nm]],
          lower        = lower_model_constraints,
          upper        = upper_model_constraints,
          verbose      = verbose
        )
      },
      error = function(e) {
        if (verbose) message("  Start '", nm, "' failed: ", e$message)
        NULL
      }
    )

    fit_obj
  })

  names(fits) <- names(start_lists)

  #fits_v <<- fits

  fits <- Filter(Negate(is.null), fits)

  if (length(fits) == 0) {
    if (verbose) message("  All starts failed for this formula.")
    return(NULL)
  }

  aic_vals <- sapply(fits, function(x) AIC(x))

  # 2. Find the best (lowest AIC)
  best_name <- names(which.min(aic_vals))
  if (verbose) {
    message("Best fit")
    print(best_name)
    print(aic_vals)
  }
  best_fit  <- fits[[best_name]]


  return(best_fit)


}

nlsLM_fit <- function(formula, data, start_values, lower = -Inf, upper = Inf, verbose = TRUE) {
  library(minpack.lm)
  if (verbose) {
    message("nlsLM lower constraints")
    print(lower)
    message("nlsLM start values")
    print(start_values)
    message("nlsLM upper constraints")
    print(upper)
  }
  # start_v <<- start_values
  fit <- tryCatch({
    minpack.lm::nlsLM(
      formula = formula,
      data    = data,
      start   = start_values,
      lower   = lower,
      upper   = upper,
      control = nls.lm.control(maxiter = 200)
    )
  }, error = function(e) {
    if (verbose) message("nlsLM failed: ", conditionMessage(e))
    NULL
  })

  if (!is.null(fit) && verbose)  {
    message("Fit successful.")
    print(fit)
  }
  return(fit)

}

summarize_model_fits <- function(models_fit_list,
                                 model_names = c("Y5","Yd5","Y4","Yd4","Ygomp4"),
                                 verbose = TRUE) {
  # Ensure all 5 models appear in the summary, even if not fit
  all_models <- unique(c(model_names, names(models_fit_list)))

  summary_list <- lapply(all_models, function(mname) {
    fit_obj <- models_fit_list[[mname]]$fit %||% models_fit_list[[mname]]  # just in case stored differently

    if (is.null(fit_obj) || !inherits(fit_obj, "nls")) {
      return(data.frame(
        model       = mname,
        converged   = FALSE,
        rss         = NA_real_,
        df_resid    = NA_integer_,
        n_params    = NA_integer_,
        AIC         = NA_real_,
        BIC         = NA_real_,
        stringsAsFactors = FALSE
      ))
    }

    # Residual sum of squares
    rss_val <- tryCatch({
      sum(residuals(fit_obj)^2)
    }, error = function(e) NA_real_)

    # Number of parameters: length of coefficient vector
    n_params <- tryCatch({
      length(coef(fit_obj))
    }, error = function(e) NA_integer_)

    # Degrees of freedom (n - p); nls objects have df.residual
    df_resid <- tryCatch({
      df.residual(fit_obj)
    }, error = function(e) NA_integer_)

    aic_val <- tryCatch(AIC(fit_obj), error = function(e) NA_real_)
    bic_val <- tryCatch(BIC(fit_obj), error = function(e) NA_real_)

    data.frame(
      model       = mname,
      converged   = TRUE,
      rss         = rss_val,
      df_resid    = df_resid,
      n_params    = n_params,
      AIC         = aic_val,
      BIC         = bic_val,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, summary_list)
}



summarize_model_parameters <- function(models_fit_list,
                                       level = 0.95,
                                       model_names = c("Y5", "Yd5", "Y4", "Yd4", "Ygomp4"),
                                       verbose = TRUE) {
  # level: confidence level for intervals (default 95%)
  # Ensure all 5 models appear in the summary, even if not fit
  all_models <- unique(names(models_fit_list))

  summary_list <- lapply(all_models, function(mname) {
    fit_obj <- models_fit_list[[mname]]$fit %||% models_fit_list[[mname]]
    # just in case stored differently

    if (is.null(fit_obj) || !inherits(fit_obj, "nls")) {
      # Return a 0-row frame so failed models don't inject an NA parameter facet
      return(data.frame(
        model     = character(0),
        parameter = character(0),
        estimate  = numeric(0),
        conf.low  = numeric(0),
        conf.high = numeric(0),
        converged = logical(0),
        stringsAsFactors = FALSE
      ))
    }

    # Safely try to get confidence intervals
    ci <- tryCatch(
      {
        nlstools::confint2(fit_obj, level = level)
      },
      error = function(e) {
        # If confint fails (e.g., non-converged Hessian), fall back to NA CIs
        NULL
      }
    )

    if (verbose) {
      cat("confint2 output:")
      print(mname)
      print(ci)
      cat("\n\n")
    }


    coefs <- coef(fit_obj)
    par_names <- names(coefs)

    if (!is.null(ci)) {
      # confint returns a matrix with rows as parameters
      # Ensure rownames align with coefs
      ci <- ci[par_names, , drop = FALSE]
      conf.low  <- ci[, 1]
      conf.high <- ci[, 2]
    } else {
      conf.low  <- rep(NA_real_, length(coefs))
      conf.high <- rep(NA_real_, length(coefs))
    }

    data.frame(
      model     = mname,
      parameter = par_names,
      estimate  = as.numeric(coefs),
      conf.low  = as.numeric(conf.low),
      conf.high = as.numeric(conf.high),
      converged = TRUE,
      stringsAsFactors = FALSE
    )
  })

  if (verbose) {
    message("Summarized Parameters completed")
  }
  do.call(rbind, summary_list)

}

select_model_fit_AIC <- function(fit_summary,
                                 fit_robust_lm,
                                 fit_params,
                                 plot_data,
                                 verbose = TRUE) {
  selected_model_name <- fit_summary[which.min(fit_summary$AIC),]$model
  selected_fit <- fit_robust_lm[[selected_model_name]]$fit
  selected_data <-  fit_robust_lm[[selected_model_name]]$data
  selected_params <- fit_params[fit_params$model == selected_model_name,]
  pred_df    <- plot_data$pred_df[plot_data$pred_df$model == selected_model_name,]
  d2xy_df    <- plot_data$d2xy_df[plot_data$d2xy_df$model == selected_model_name,]
  dydx_df    <- plot_data$dydx_df[plot_data$dydx_df$model == selected_model_name,]
  curve_ci_df <- if (!is.null(plot_data$ci_df))
    plot_data$ci_df[plot_data$ci_df$model == selected_model_name,]
  else NULL

  return(list(best_model_name = selected_model_name, best_fit = selected_fit, best_data = selected_data,
              best_ci = selected_params, best_pred = pred_df,
              best_d2xy = d2xy_df, best_dydx = dydx_df, best_curve_ci = curve_ci_df))

}

# Helper: dispatch to the correct dydx<model> function by name
dispatch_dydx <- function(model_name, x, theta) {
  fn <- match.fun(paste0("dydx", model_name))
  args <- c(list(x = x), as.list(theta[intersect(names(theta), c("a","b","c","d","g"))]))
  do.call(fn, args)
}

# .compute_fda2018_scalars()
#
# Internal helper – all FDA-2018 LOQ arithmetic lives here.
# Returns a plain named list of exactly 6 scalars (NA_real_ on failure).
#
# Arguments mirror the relevant subset of fit_fda2018_loq(); see that
# function's header for full protocol description.
# .compute_fda2018_scalars <- function(fit,
#                                      best_data,
#                                      best_fit,
#                                      response_variable,
#                                      independent_variable,
#                                      antigen_settings,
#                                      antigen_fit_options,
#                                      dil_series_se_plate_source,
#                                      verbose = TRUE) {
#   
#   na_scalars <- list(
#     lloq_fda2018_concentration = NA_real_,
#     lloq_fda2018_response      = NA_real_,
#     uloq_fda2018_concentration = NA_real_,
#     uloq_fda2018_response      = NA_real_,
#     lloq_cv         = NA_real_,
#     uloq_cv         = NA_real_,
#     lloq_accuracy   = NA_real_,
#     uloq_accuracy   = NA_real_,
#     n_passing_std   = NA_integer_,
#     n_total_std     = NA_integer_,
#     pct_passing_std = NA_real_,
#     fda2018_status  = "FAILED",
#     blank_mean      = NA_real_,
#     blank_sd        = NA_real_,
#     llod            = NA_real_,
#     ulod            = NA_real_,
#     inflect_x       = NA_real_,
#     inflect_y       = NA_real_,
#     mindc           = NA_real_,
#     maxdc           = NA_real_,
#     minrdl          = NA_real_,
#     maxrdl          = NA_real_,
#     dydx_inflect    = NA_real_
#   )
#   
#   # ── Guard clauses ──
#   if (is.null(fit) || is.null(best_data) || nrow(best_data) == 0) {
#     if (verbose) message("[FDA2018] NULL fit or empty best_data -> returning NA scalars")
#     return(na_scalars)
#   }
#   
#   if (!response_variable %in% names(best_data)) {
#     if (verbose) message("[FDA2018] response_variable '", response_variable, "' not in best_data")
#     return(na_scalars)
#   }
#   
#   if (!independent_variable %in% names(best_data)) {
#     if (verbose) message("[FDA2018] independent_variable '", independent_variable, "' not in best_data")
#     return(na_scalars)
#   }
#   
#   has_finite_response <- any(is.finite(best_data[[response_variable]]))
#   has_finite_indep    <- any(is.finite(best_data[[independent_variable]]))
#   if (!has_finite_response || !has_finite_indep) {
#     if (verbose) message("[FDA2018] No finite response or independent values")
#     return(na_scalars)
#   }
#   
#   # ══════════════════════════════════════════════════════════════════════
#   # ── Blanks ──
#   # ══════════════════════════════════════════════════════════════════════
#   
#   plate_blanks <- tryCatch({
#     if ("stype" %in% names(best_data)) {
#       best_data[best_data$stype == "B", , drop = FALSE]
#     } else {
#       data.frame()
#     }
#   }, error = function(e) data.frame())
#   
#   blank_responses <- if (nrow(plate_blanks) > 0 && response_variable %in% names(plate_blanks)) {
#     plate_blanks[[response_variable]]
#   } else {
#     numeric(0)
#   }
#   
#   Blank_mean <- if (length(blank_responses) > 0) mean(blank_responses, na.rm = TRUE) else NA_real_
#   Blank_sd   <- if (length(blank_responses) > 1) sd(blank_responses, na.rm = TRUE)   else NA_real_
#   
#   # ══════════════════════════════════════════════════════════════════════
#   # ── Extract model_name and fixed_a_result ──
#   # ══════════════════════════════════════════════════════════════════════
#   
#   model_name <- tryCatch({
#     best_fit$best_model_name
#   }, error = function(e) NA_character_)
#   
#   fixed_a_result <- tryCatch({
#     fa <- antigen_settings$l_asy_min_constraint
#     if (!is.null(fa) && is.finite(fa) && fa > 0 &&
#         isTRUE(antigen_settings$l_asy_min_constraint == antigen_settings$l_asy_max_constraint)) {
#       if (isTRUE(antigen_fit_options$is_log_response) && fa > 0) {
#         log10(fa + 0.000005)
#       } else {
#         fa
#       }
#     } else {
#       NULL
#     }
#   }, error = function(e) NULL)
#   
#   # ══════════════════════════════════════════════════════════════════════
#   # ── Inflection Point (generate_inflection_point) ──
#   # ══════════════════════════════════════════════════════════════════════
#   
#   inflection <- tryCatch({
#     generate_inflection_point(
#       model_name           = model_name,
#       fit                  = fit,
#       fixed_a_result       = fixed_a_result,
#       independent_variable = independent_variable,
#       verbose              = verbose
#     )
#   }, error = function(e) {
#     if (verbose) message("[FDA2018] generate_inflection_point error: ", conditionMessage(e))
#     list(inflect_x = NA_real_, inflect_y = NA_real_)
#   })
#   
#   inflect_x <- if (!is.null(inflection$inflect_x) && length(inflection$inflect_x) > 0) {
#     as.numeric(inflection$inflect_x)
#   } else {
#     NA_real_
#   }
#   
#   inflect_y <- if (!is.null(inflection$inflect_y) && length(inflection$inflect_y) > 0) {
#     as.numeric(inflection$inflect_y)
#   } else {
#     NA_real_
#   }
#   
#   # ══════════════════════════════════════════════════════════════════════
#   # ── LODs (generate_lods) ──
#   # ══════════════════════════════════════════════════════════════════════
#   
#   std_error_blank <- tryCatch({
#     antigen_settings$std_error_blank %||% {
#       if (nrow(plate_blanks) > 1 && response_variable %in% names(plate_blanks)) {
#         sd(plate_blanks[[response_variable]], na.rm = TRUE) /
#           sqrt(sum(!is.na(plate_blanks[[response_variable]])))
#       } else {
#         0
#       }
#     }
#   }, error = function(e) 0)
#   
#   lods <- tryCatch({
#     generate_lods(
#       best_fit        = best_fit,
#       fixed_a_result  = fixed_a_result,
#       std_error_blank = std_error_blank,
#       verbose         = verbose
#     )
#   }, error = function(e) {
#     if (verbose) message("[FDA2018] generate_lods error: ", conditionMessage(e))
#     list(llod = NA_real_, ulod = NA_real_)
#   })
#   
#   llod <- as.numeric(lods$llod)
#   ulod <- as.numeric(lods$ulod)
#   
#   # ══════════════════════════════════════════════════════════════════════
#   # ── Sensitivity (dy/dx at inflection via dispatch_dydx) ──
#   # ══════════════════════════════════════════════════════════════════════
#   
#   dydx_inflect <- tryCatch({
#     if (!is.na(inflect_x) && !is.na(model_name)) {
#       all_params <- if (!is.null(fixed_a_result)) {
#         c(a = fixed_a_result, coef(fit))
#       } else {
#         coef(fit)
#       }
#       dispatch_dydx(model_name, inflect_x, all_params)
#     } else {
#       NA_real_
#     }
#   }, error = function(e) {
#     # Fallback: numerical differentiation
#     tryCatch({
#       if (!is.na(inflect_x)) {
#         h <- abs(inflect_x) * 1e-5
#         if (h == 0) h <- 1e-8
#         y_plus  <- predict(fit, newdata = setNames(data.frame(inflect_x + h), independent_variable))
#         y_minus <- predict(fit, newdata = setNames(data.frame(inflect_x - h), independent_variable))
#         as.numeric((y_plus - y_minus) / (2 * h))
#       } else {
#         NA_real_
#       }
#     }, error = function(e2) NA_real_)
#   })
#   
#   # ══════════════════════════════════════════════════════════════════════
#   # ── MDC / RDL (generate_mdc_rdl) ──
#   # ══════════════════════════════════════════════════════════════════════
#   
#   mdc_rdl <- tryCatch({
#     generate_mdc_rdl(
#       best_fit = list(
#         best_fit    = fit,
#         best_data   = best_data,
#         best_glance = list(llod = llod, ulod = ulod)
#       ),
#       lods = list(llod = llod, ulod = ulod),
#       independent_variable = independent_variable,
#       verbose              = verbose
#     )
#   }, error = function(e) {
#     if (verbose) message("[FDA2018] generate_mdc_rdl error: ", conditionMessage(e))
#     list(mindc = NA_real_, maxdc = NA_real_, minrdl = NA_real_, maxrdl = NA_real_)
#   })
#   
#   # ══════════════════════════════════════════════════════════════════════
#   # ── FDA 2018 LOQ Computation (CV + Accuracy on Standards) ──
#   # ══════════════════════════════════════════════════════════════════════
#   
#   cv_threshold  <- 20
#   acc_low       <- 80
#   acc_high      <- 120
#   lloq_cv_threshold <- 25
#   
#   # Identify standard samples
#   std_data <- tryCatch({
#     best_data[!is.na(best_data[[independent_variable]]) &
#                 is.finite(best_data[[independent_variable]]) &
#                 best_data[[independent_variable]] > 0, ]
#   }, error = function(e) data.frame())
#   
#   if (nrow(std_data) == 0) {
#     if (verbose) message("[FDA2018] No valid standard data for LOQ computation")
#     return(list(
#       lloq_fda2018_concentration = NA_real_,
#       lloq_fda2018_response      = NA_real_,
#       uloq_fda2018_concentration = NA_real_,
#       uloq_fda2018_response      = NA_real_,
#       lloq_cv = NA_real_, uloq_cv = NA_real_,
#       lloq_accuracy = NA_real_, uloq_accuracy = NA_real_,
#       n_passing_std = 0L, n_total_std = 0L,
#       pct_passing_std = 0, fda2018_status = "NO_PASSING_LEVELS",
#       blank_mean = Blank_mean, blank_sd = Blank_sd,
#       llod = llod, ulod = ulod,
#       inflect_x = inflect_x, inflect_y = inflect_y,
#       mindc = mdc_rdl$mindc %||% NA_real_,
#       maxdc = mdc_rdl$maxdc %||% NA_real_,
#       minrdl = mdc_rdl$minrdl %||% NA_real_,
#       maxrdl = mdc_rdl$maxrdl %||% NA_real_,
#       dydx_inflect = dydx_inflect
#     ))
#   }
#   
#   # Back-calculate concentrations from the fit
#   std_data$predicted_response <- tryCatch({
#     predict(fit, newdata = std_data)
#   }, error = function(e) rep(NA_real_, nrow(std_data)))
#   
#   std_data$nominal_conc <- std_data[[independent_variable]]
#   
#   # Compute CV% and accuracy per concentration level
#   loq_table <- tryCatch({
#     agg <- aggregate(
#       predicted_response ~ nominal_conc,
#       data = std_data,
#       FUN = function(x) {
#         x <- x[is.finite(x)]
#         c(mean = mean(x), sd = sd(x), n = length(x))
#       }
#     )
#     result_df <- data.frame(
#       nominal_conc  = agg$nominal_conc,
#       mean_response = agg$predicted_response[, "mean"],
#       sd_response   = agg$predicted_response[, "sd"],
#       n_reps        = agg$predicted_response[, "n"]
#     )
#     result_df$cv_pct <- (result_df$sd_response / abs(result_df$mean_response)) * 100
#     
#     # Accuracy: back-calculate concentration from mean response
#     result_df$backcalc_conc <- tryCatch({
#       sapply(result_df$mean_response, function(resp) {
#         tryCatch({
#           inv <- get_inverse_prediction(
#             fit = fit,
#             response_value = resp,
#             independent_variable = independent_variable,
#             verbose = FALSE
#           )
#           as.numeric(inv$x_est)
#         }, error = function(e) NA_real_)
#       })
#     }, error = function(e) rep(NA_real_, nrow(result_df)))
#     
#     result_df$pct_recovery <- (result_df$backcalc_conc / result_df$nominal_conc) * 100
#     result_df
#   }, error = function(e) {
#     if (verbose) message("[FDA2018] LOQ table construction failed: ", conditionMessage(e))
#     data.frame(nominal_conc = numeric(0), cv_pct = numeric(0),
#                pct_recovery = numeric(0))
#   })
#   
#   if (nrow(loq_table) == 0) {
#     if (verbose) message("[FDA2018] Empty LOQ table")
#     return(list(
#       lloq_fda2018_concentration = NA_real_,
#       lloq_fda2018_response      = NA_real_,
#       uloq_fda2018_concentration = NA_real_,
#       uloq_fda2018_response      = NA_real_,
#       lloq_cv = NA_real_, uloq_cv = NA_real_,
#       lloq_accuracy = NA_real_, uloq_accuracy = NA_real_,
#       n_passing_std = 0L, n_total_std = 0L,
#       pct_passing_std = 0, fda2018_status = "NO_PASSING_LEVELS",
#       Blank_mean = Blank_mean, Blank_sd = Blank_sd,
#       llod = llod, ulod = ulod,
#       inflect_x = inflect_x, inflect_y = inflect_y,
#       mindc = mdc_rdl$mindc %||% NA_real_,
#       maxdc = mdc_rdl$maxdc %||% NA_real_,
#       minrdl = mdc_rdl$minrdl %||% NA_real_,
#       maxrdl = mdc_rdl$maxrdl %||% NA_real_,
#       dydx_inflect = dydx_inflect
#     ))
#   }
#   
#   # Determine passing levels
#   passes_accuracy <- function(pct) {
#     !is.na(pct) & is.finite(pct) & pct >= acc_low & pct <= acc_high
#   }
#   
#   loq_table$passes_std <- !is.na(loq_table$cv_pct) &
#     is.finite(loq_table$cv_pct) &
#     loq_table$cv_pct <= cv_threshold &
#     passes_accuracy(loq_table$pct_recovery)
#   
#   loq_table$passes_lloq <- !is.na(loq_table$cv_pct) &
#     is.finite(loq_table$cv_pct) &
#     loq_table$cv_pct <= lloq_cv_threshold &
#     passes_accuracy(loq_table$pct_recovery)
#   
#   passing_std     <- which(loq_table$passes_std)
#   lloq_candidates <- which(loq_table$passes_lloq)
#   n_passing       <- length(passing_std)
#   n_total         <- nrow(loq_table)
#   
#   # Extract FDA LLOQ and ULOQ
#   lloq_fda <- if (length(lloq_candidates) > 0) {
#     loq_table$nominal_conc[min(lloq_candidates)]
#   } else { NA_real_ }
#   
#   uloq_fda <- if (length(passing_std) > 0) {
#     loq_table$nominal_conc[max(passing_std)]
#   } else { NA_real_ }
#   
#   # Get response values for FDA LOQs
#   lloq_fda_response <- tryCatch({
#     if (!is.na(lloq_fda)) {
#       as.numeric(predict(fit, newdata = setNames(data.frame(lloq_fda), independent_variable)))
#     } else NA_real_
#   }, error = function(e) NA_real_)
#   
#   uloq_fda_response <- tryCatch({
#     if (!is.na(uloq_fda)) {
#       as.numeric(predict(fit, newdata = setNames(data.frame(uloq_fda), independent_variable)))
#     } else NA_real_
#   }, error = function(e) NA_real_)
#   
#   # CV and accuracy at the LOQ levels
#   lloq_cv <- if (!is.na(lloq_fda) && lloq_fda %in% loq_table$nominal_conc) {
#     loq_table$cv_pct[loq_table$nominal_conc == lloq_fda][1]
#   } else NA_real_
#   
#   uloq_cv <- if (!is.na(uloq_fda) && uloq_fda %in% loq_table$nominal_conc) {
#     loq_table$cv_pct[loq_table$nominal_conc == uloq_fda][1]
#   } else NA_real_
#   
#   lloq_accuracy <- if (!is.na(lloq_fda) && lloq_fda %in% loq_table$nominal_conc) {
#     loq_table$pct_recovery[loq_table$nominal_conc == lloq_fda][1]
#   } else NA_real_
#   
#   uloq_accuracy <- if (!is.na(uloq_fda) && uloq_fda %in% loq_table$nominal_conc) {
#     loq_table$pct_recovery[loq_table$nominal_conc == uloq_fda][1]
#   } else NA_real_
#   
#   if (verbose) {
#     message(sprintf("[FDA2018] %d/%d levels passing. LLOQ=%.4g, ULOQ=%.4g",
#                     n_passing, n_total,
#                     ifelse(is.na(lloq_fda), NaN, lloq_fda),
#                     ifelse(is.na(uloq_fda), NaN, uloq_fda)))
#   }
#   
#   # ══════════════════════════════════════════════════════════════════════
#   # ── Assemble result ──
#   # ══════════════════════════════════════════════════════════════════════
#   
#   result <- list(
#     lloq_fda2018_concentration = lloq_fda,
#     lloq_fda2018_response      = lloq_fda_response,
#     uloq_fda2018_concentration = uloq_fda,
#     uloq_fda2018_response      = uloq_fda_response,
#     lloq_cv         = lloq_cv,
#     uloq_cv         = uloq_cv,
#     lloq_accuracy   = lloq_accuracy,
#     uloq_accuracy   = uloq_accuracy,
#     n_passing_std   = n_passing,
#     n_total_std     = n_total,
#     pct_passing_std = if (n_total > 0) (n_passing / n_total) * 100 else NA_real_,
#     fda2018_status  = if (n_passing > 0) "OK" else "NO_PASSING_LEVELS",
#     blank_mean      = Blank_mean,
#     blank_sd        = Blank_sd,
#     llod            = llod,
#     ulod            = ulod,
#     inflect_x       = inflect_x,
#     inflect_y       = inflect_y,
#     mindc           = mdc_rdl$mindc  %||% NA_real_,
#     maxdc           = mdc_rdl$maxdc  %||% NA_real_,
#     minrdl          = mdc_rdl$minrdl %||% NA_real_,
#     maxrdl          = mdc_rdl$maxrdl %||% NA_real_,
#     dydx_inflect    = dydx_inflect
#   )
#   
#   return(result)
# }
# .compute_fda2018_scalars <- function(fit,
#                                      best_data,
#                                      model_name,
#                                      plate_blanks,
#                                      response_variable,
#                                      independent_variable,
#                                      fixed_a_result    = NULL,
#                                      is_log_response   = FALSE,
#                                      cv_threshold      = 20,
#                                      lloq_cv_threshold = 25,
#                                      accuracy_lo       = 80,
#                                      accuracy_hi       = 120,
#                                      verbose           = TRUE) {
#   
#   # ── 0. Define NA scaffold ─────────────────────────────────────────────
#   #    Every early-return path hands back the same named list so that
#   
#   #    downstream code (glance_df assembly) never sees NULL columns.
#   na_scalars <- list(
#     lloq            = NA_real_,
#     uloq            = NA_real_,
#     lloq_y          = NA_real_,
#     uloq_y          = NA_real_,
#     lloq_cv         = NA_real_,
#     uloq_cv         = NA_real_,
#     lloq_accuracy   = NA_real_,
#     uloq_accuracy   = NA_real_,
#     n_passing_std   = NA_integer_,
#     n_total_std     = NA_integer_,
#     pct_passing_std = NA_real_,
#     fda2018_status  = "FAILED",
#     Blank_mean      = NA_real_,
#     Blank_sd        = NA_real_,
#     llod            = NA_real_,
#     ulod            = NA_real_,
#     inflect_x       = NA_real_,
#     inflect_y       = NA_real_,
#     lloq_method     = "fda2018",
#     uloq_method     = "fda2018",
#     mindc           = NA_real_,
#     maxdc           = NA_real_,
#     minrdl          = NA_real_,
#     maxrdl          = NA_real_,
#     dydx_inflect    = NA_real_
#   )
#   
#   # ── 1. Guard: minimum data requirements ───────────────────────────────
#   if (is.null(fit) || is.null(best_data) || nrow(best_data) == 0) {
#     if (verbose) message("[FDA2018] fit or best_data is NULL/empty. Returning NA scalars.")
#     return(na_scalars)
#   }
#   
#   if (!response_variable %in% names(best_data)) {
#     if (verbose) message(sprintf(
#       "[FDA2018] response_variable '%s' not found in best_data columns: %s",
#       response_variable, paste(names(best_data), collapse = ", ")
#     ))
#     return(na_scalars)
#   }
#   
#   if (!independent_variable %in% names(best_data)) {
#     if (verbose) message(sprintf(
#       "[FDA2018] independent_variable '%s' not found in best_data columns: %s",
#       independent_variable, paste(names(best_data), collapse = ", ")
#     ))
#     return(na_scalars)
#   }
#   
#   # ── 2. Prepare standard data for back-calculation ─────────────────────
#   std_data <- best_data[best_data$stype == "S", , drop = FALSE]
#   
#   if (nrow(std_data) == 0) {
#     if (verbose) message("[FDA2018] No standard (stype=='S') rows in best_data.")
#     return(na_scalars)
#   }
#   
#   # Verify response and independent columns have finite data
#   has_finite_response <- any(is.finite(std_data[[response_variable]]))
#   has_finite_indep    <- any(is.finite(std_data[[independent_variable]]))
#   
#   if (!has_finite_response || !has_finite_indep) {
#     if (verbose) message(sprintf(
#       "[FDA2018] Standards lack finite values. response finite: %s, independent finite: %s",
#       has_finite_response, has_finite_indep
#     ))
#     return(na_scalars)
#   }
#   
#   if (verbose) {
#     message(sprintf(
#       "[FDA2018] %d standard rows, response range [%.4f, %.4f], indep range [%.4f, %.4f]",
#       nrow(std_data),
#       min(std_data[[response_variable]], na.rm = TRUE),
#       max(std_data[[response_variable]], na.rm = TRUE),
#       min(std_data[[independent_variable]], na.rm = TRUE),
#       max(std_data[[independent_variable]], na.rm = TRUE)
#     ))
#   }
#   
#   # ── 3. Back-calculate predicted concentrations ────────────────────────
#   std_data_bc <- tryCatch(
#     calculate_predicted_concentration(
#       model_name        = model_name,
#       fit               = fit,
#       plate_samples     = std_data,
#       fixed_constraint  = fixed_a_result,
#       response_variable = response_variable,
#       is_log_response   = FALSE,
#       verbose           = verbose
#     ),
#     error = function(e) {
#       if (verbose) message("[FDA2018] back-calculation failed: ", conditionMessage(e))
#       NULL
#     }
#   )
#   
#   if (is.null(std_data_bc)) {
#     if (verbose) message("[FDA2018] Back-calculation returned NULL. Returning NA scalars.")
#     return(na_scalars)
#   }
#   
#   if (!"predicted_concentration" %in% names(std_data_bc)) {
#     if (verbose) message(sprintf(
#       "[FDA2018] No 'predicted_concentration' column. Columns: %s",
#       paste(names(std_data_bc), collapse = ", ")
#     ))
#     return(na_scalars)
#   }
#   
#   # Filter to finite predicted concentrations
#   finite_mask <- is.finite(std_data_bc$predicted_concentration)
#   std_data_bc <- std_data_bc[finite_mask, , drop = FALSE]
#   
#   if (nrow(std_data_bc) == 0) {
#     if (verbose) message("[FDA2018] Zero rows with finite predicted_concentration after filtering.")
#     return(na_scalars)
#   }
#   
#   if (verbose) {
#     message(sprintf(
#       "[FDA2018] %d rows with finite predicted_concentration (of %d total standards)",
#       nrow(std_data_bc), nrow(std_data)
#     ))
#   }
#   
#   # ── 4. Extract x and y data safely ───────────────────────────────────
#   x_data <- std_data_bc[[independent_variable]]
#   x_data <- x_data[is.finite(x_data)]
#   
#   y_data <- std_data_bc[[response_variable]]
#   y_data <- y_data[is.finite(y_data)]
#   
#   pred_conc <- std_data_bc$predicted_concentration
#   
#   if (length(x_data) == 0 || length(y_data) == 0 || length(pred_conc) == 0) {
#     if (verbose) message(sprintf(
#       "[FDA2018] Insufficient finite data after filtering. x_data=%d, y_data=%d, pred_conc=%d",
#       length(x_data), length(y_data), length(pred_conc)
#     ))
#     return(na_scalars)
#   }
#   
#   # ── 5. Compute accuracy (percent recovery) ───────────────────────────
#   #    pct_recovery = (predicted / nominal) * 100
#   nominal_conc <- std_data_bc[[independent_variable]]
#   std_data_bc$pct_recovery <- (std_data_bc$predicted_concentration / nominal_conc) * 100
#   
#   # ── 6. Compute CV per dilution level ─────────────────────────────────
#   #    Group by dilution (or independent variable) and compute CV
#   dilution_col <- if ("dilution" %in% names(std_data_bc)) "dilution" else independent_variable
#   
#   loq_table <- tryCatch({
#     dil_groups <- split(std_data_bc, std_data_bc[[dilution_col]])
#     
#     do.call(rbind, lapply(names(dil_groups), function(dname) {
#       grp <- dil_groups[[dname]]
#       pc  <- grp$predicted_concentration
#       pc  <- pc[is.finite(pc)]
#       
#       n_finite <- length(pc)
#       mean_pc  <- if (n_finite > 0) mean(pc, na.rm = TRUE) else NA_real_
#       sd_pc    <- if (n_finite > 1) sd(pc, na.rm = TRUE)   else NA_real_
#       cv_pct   <- if (!is.na(mean_pc) && mean_pc != 0) (sd_pc / abs(mean_pc)) * 100 else NA_real_
#       
#       # Use the mean pct_recovery for the group
#       mean_recovery <- mean(grp$pct_recovery, na.rm = TRUE)
#       
#       # Nominal concentration (should be same within group)
#       nom <- mean(grp[[independent_variable]], na.rm = TRUE)
#       
#       data.frame(
#         dilution_level  = dname,
#         nominal_conc    = nom,
#         n               = n_finite,
#         mean_pred       = mean_pc,
#         sd_pred         = sd_pc,
#         cv_pct          = cv_pct,
#         pct_recovery    = mean_recovery,
#         stringsAsFactors = FALSE
#       )
#     }))
#   }, error = function(e) {
#     if (verbose) message("[FDA2018] CV computation failed: ", conditionMessage(e))
#     NULL
#   })
#   
#   if (is.null(loq_table) || nrow(loq_table) == 0) {
#     if (verbose) message("[FDA2018] Could not compute CV table. Returning NA scalars.")
#     return(na_scalars)
#   }
#   
#   # ── 7. Determine passing standards ───────────────────────────────────
#   passes_accuracy <- function(r) {
#     !is.na(r) & is.finite(r) & r >= accuracy_lo & r <= accuracy_hi
#   }
#   
#   loq_table$passes_std <- !is.na(loq_table$cv_pct) &
#     is.finite(loq_table$cv_pct) &
#     loq_table$cv_pct <= cv_threshold &
#     passes_accuracy(loq_table$pct_recovery)
#   
#   loq_table$passes_lloq <- !is.na(loq_table$cv_pct) &
#     is.finite(loq_table$cv_pct) &
#     loq_table$cv_pct <= lloq_cv_threshold &
#     passes_accuracy(loq_table$pct_recovery)
#   
#   if (verbose) {
#     message(sprintf(
#       "[FDA2018] LOQ table: %d dilution levels, %d pass standard criteria, %d pass LLOQ criteria",
#       nrow(loq_table), sum(loq_table$passes_std), sum(loq_table$passes_lloq)
#     ))
#   }
#   
#   # ── 8. Determine LLOQ and ULOQ ──────────────────────────────────────
#   # Sort by nominal concentration (ascending)
#   loq_table <- loq_table[order(loq_table$nominal_conc), ]
#   
#   lloq_candidates <- which(loq_table$passes_lloq)
#   passing_std     <- which(loq_table$passes_std)
#   
#   # LLOQ = lowest passing dilution level
#   
#   # ULOQ = highest passing dilution level
#   lloq <- if (length(lloq_candidates) > 0) {
#     loq_table$nominal_conc[min(lloq_candidates)]
#   } else {
#     NA_real_
#   }
#   
#   uloq <- if (length(passing_std) > 0) {
#     loq_table$nominal_conc[max(passing_std)]
#   } else {
#     NA_real_
#   }
#   
#   # Get corresponding y-values and CVs
#   lloq_y  <- NA_real_
#   uloq_y  <- NA_real_
#   lloq_cv <- NA_real_
#   uloq_cv <- NA_real_
#   lloq_accuracy <- NA_real_
#   uloq_accuracy <- NA_real_
#   
#   if (!is.na(lloq)) {
#     lloq_row <- loq_table[loq_table$nominal_conc == lloq, , drop = FALSE]
#     if (nrow(lloq_row) > 0) {
#       lloq_cv <- lloq_row$cv_pct[1]
#       lloq_accuracy <- lloq_row$pct_recovery[1]
#       # Predict y at LLOQ
#       lloq_y <- tryCatch({
#         as.numeric(predict(fit, newdata = setNames(data.frame(lloq), independent_variable)))
#       }, error = function(e) NA_real_)
#     }
#   }
#   
#   if (!is.na(uloq)) {
#     uloq_row <- loq_table[loq_table$nominal_conc == uloq, , drop = FALSE]
#     if (nrow(uloq_row) > 0) {
#       uloq_cv <- uloq_row$cv_pct[1]
#       uloq_accuracy <- uloq_row$pct_recovery[1]
#       uloq_y <- tryCatch({
#         as.numeric(predict(fit, newdata = setNames(data.frame(uloq), independent_variable)))
#       }, error = function(e) NA_real_)
#     }
#   }
#   
#   # ── 9. Blank statistics ──────────────────────────────────────────────
#   blank_response_col <- if (!is.null(plate_blanks) && nrow(plate_blanks) > 0) {
#     resolve_response_col(plate_blanks, default = response_variable)
#   } else {
#     response_variable
#   }
#   
#   Blank_mean <- if (!is.null(plate_blanks) && nrow(plate_blanks) > 0 &&
#                     blank_response_col %in% names(plate_blanks)) {
#     mean(plate_blanks[[blank_response_col]], na.rm = TRUE)
#   } else {
#     NA_real_
#   }
#   
#   Blank_sd <- if (!is.null(plate_blanks) && nrow(plate_blanks) > 1 &&
#                   blank_response_col %in% names(plate_blanks)) {
#     sd(plate_blanks[[blank_response_col]], na.rm = TRUE)
#   } else {
#     NA_real_
#   }
#   
#   # ── 10. LODs (from existing generate_lods logic) ─────────────────────
#   llod <- NA_real_
#   ulod <- NA_real_
#   
#   tryCatch({
#     params <- coef(fit)
#     d_val <- if ("d" %in% names(params)) params["d"] else NA_real_
#     
#     # ULOD: lower confidence bound of d (upper asymptote)
#     ci <- tryCatch(nlstools::confint2(fit, level = 0.95), error = function(e) NULL)
#     
#     if (!is.null(ci)) {
#       par_names <- rownames(ci)
#       if ("d" %in% par_names) {
#         ulod <- ci["d", 1]  # 2.5% bound
#       }
#       
#       if (!is.null(fixed_a_result)) {
#         # LLOD from fixed a + margin of error
#         se_blank <- if (!is.na(Blank_sd) && !is.null(plate_blanks) && nrow(plate_blanks) > 0) {
#           Blank_sd / sqrt(sum(!is.na(plate_blanks[[blank_response_col]])))
#         } else {
#           0
#         }
#         crit_val <- qt(0.975, df = max(nrow(best_data) - length(par_names), 1))
#         llod <- fixed_a_result + crit_val * se_blank
#       } else if ("a" %in% par_names) {
#         llod <- ci["a", 2]  # 97.5% bound
#       }
#     }
#     
#     # Sanitize
#     if (!is.na(ulod) && !is.na(llod) && (ulod < 0 || ulod < llod)) {
#       ulod <- NA_real_
#     }
#   }, error = function(e) {
#     if (verbose) message("[FDA2018] LOD computation error: ", conditionMessage(e))
#   })
#   
#   # ── 11. Inflection point ─────────────────────────────────────────────
#   # inflect <- tryCatch(
#   #   generate_inflection_point(fit, model_name, fixed_a_result, verbose = verbose),
#   #   error = function(e) list(inflect_x = NA_real_, inflect_y = NA_real_)
#   # )
#   inflect <- tryCatch(
#     generate_inflection_point(model_name = model_name, fit = fit,
#       fixed_a_result       = fixed_a_result,
#       independent_variable = independent_variable,
#       verbose              = verbose
#     ),
#     error = function(e) list(inflect_x = NA_real_, inflect_y = NA_real_)
#   )
#   
#   inflect_x <- if (!is.null(inflect$inflect_x) && length(inflect$inflect_x) > 0) {
#     as.numeric(inflect$inflect_x)
#   } else {
#     NA_real_
#   }
#   
#   inflect_y <- if (!is.null(inflect$inflect_y) && length(inflect$inflect_y) > 0) {
#     as.numeric(inflect$inflect_y)
#   } else {
#     NA_real_
#   }
#   
#   # ── 12. Sensitivity (dy/dx at inflection) ───────────────────────────
#   dydx_inflect <- tryCatch({
#     all_params <- if (!is.null(fixed_a_result)) {
#       c(a = fixed_a_result, coef(fit))
#     } else {
#       coef(fit)
#     }
#     
#     switch(model_name,
#            "Y5"    = do.call(dydxY5,    c(list(x = inflect_x), as.list(all_params))),
#            "Y4"    = do.call(dydxY4,    c(list(x = inflect_x), as.list(all_params))),
#            "Yd5"   = do.call(dydxYd5,   c(list(x = inflect_x), as.list(all_params))),
#            "Yd4"   = do.call(dydxYd4,   c(list(x = inflect_x), as.list(all_params))),
#            "Ygomp4"= do.call(dydxYgomp4,c(list(x = inflect_x), as.list(all_params))),
#            NA_real_
#     )
#   }, error = function(e) NA_real_)
#   
#   # ── 13. MDC / RDL ───────────────────────────────────────────────────
#   mdc_rdl <- tryCatch(
#     generate_mdc_rdl(
#       best_fit = list(
#         best_fit   = fit,
#         best_data  = best_data,
#         best_glance = list(llod = llod, ulod = ulod)
#       ),
#       lods = list(llod = llod, ulod = ulod),
#       independent_variable = independent_variable,
#       verbose = verbose
#     ),
#     error = function(e) {
#       if (verbose) message("[FDA2018] MDC/RDL computation error: ", conditionMessage(e))
#       list(mindc = NA_real_, maxdc = NA_real_, minrdl = NA_real_, maxrdl = NA_real_)
#     }
#   )
#   
#   # ── 14. Assemble and return ─────────────────────────────────────────
#   n_total   <- nrow(loq_table)
#   n_passing <- sum(loq_table$passes_std, na.rm = TRUE)
#   
#   result <- list(
#     lloq            = lloq,
#     uloq            = uloq,
#     lloq_y          = lloq_y,
#     uloq_y          = uloq_y,
#     lloq_cv         = lloq_cv,
#     uloq_cv         = uloq_cv,
#     lloq_accuracy   = lloq_accuracy,
#     uloq_accuracy   = uloq_accuracy,
#     n_passing_std   = n_passing,
#     n_total_std     = n_total,
#     pct_passing_std = if (n_total > 0) (n_passing / n_total) * 100 else NA_real_,
#     fda2018_status  = if (n_passing > 0) "OK" else "NO_PASSING_LEVELS",
#     Blank_mean      = Blank_mean,
#     Blank_sd        = Blank_sd,
#     llod            = llod,
#     ulod            = ulod,
#     inflect_x       = inflect_x,
#     inflect_y       = inflect_y,
#     lloq_method     = "fda2018",
#     uloq_method     = "fda2018",
#     mindc           = mdc_rdl$mindc  %||% NA_real_,
#     maxdc           = mdc_rdl$maxdc  %||% NA_real_,
#     minrdl          = mdc_rdl$minrdl %||% NA_real_,
#     maxrdl          = mdc_rdl$maxrdl %||% NA_real_,
#     dydx_inflect    = dydx_inflect
#   )
#   
#   if (verbose) {
#     message(sprintf(
#       "[FDA2018] Done. LLOQ=%.4f, ULOQ=%.4f, LLOD=%.4f, ULOD=%.4f, status=%s",
#       result$lloq, result$uloq, result$llod, result$ulod, result$fda2018_status
#     ))
#   }
#   
#   return(result)
# }


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
# .compute_curve_diagnostics
#
# Computes curve-level diagnostics that do NOT depend on the dilution-series
# accuracy table: LODs, inflection point, blanks, MDC/RDL, sensitivity.
#
# This was refactored out of the old .compute_fda2018_scalars() which tried
# to do both curve diagnostics AND FDA LOQ computation internally.
# The FDA LOQ columns are now populated by .extract_fda_loqs_from_dil_series().
# ============================================================================
.compute_curve_diagnostics <- function(fit,
                                       best_data,
                                       best_fit,
                                       response_variable,
                                       independent_variable,
                                       antigen_settings,
                                       antigen_fit_options,
                                       verbose = TRUE) {
  na_scalars <- list(
    blank_mean      = NA_real_,
    blank_sd        = NA_real_,
    llod            = NA_real_,
    ulod            = NA_real_,
    inflect_x       = NA_real_,
    inflect_y       = NA_real_,
    mindc           = NA_real_,
    maxdc           = NA_real_,
    minrdl          = NA_real_,
    maxrdl          = NA_real_,
    dydx_inflect    = NA_real_,
    # FDA LOQ placeholders — will be overridden by .extract_fda_loqs_from_dil_series
    lloq_fda2018_concentration = NA_real_,
    lloq_fda2018_response      = NA_real_,
    uloq_fda2018_concentration = NA_real_,
    uloq_fda2018_response      = NA_real_,
    lloq_cv         = NA_real_,
    uloq_cv         = NA_real_,
    lloq_accuracy   = NA_real_,
    uloq_accuracy   = NA_real_,
    n_passing_std   = NA_integer_,
    n_total_std     = NA_integer_,
    pct_passing_std = NA_real_,
    fda2018_status  = "PENDING"
  )
  
  # ── Guard clauses ──
  if (is.null(fit) || is.null(best_data) || nrow(best_data) == 0) {
    if (verbose) message("[.compute_curve_diagnostics] NULL fit or empty best_data")
    na_scalars$fda2018_status <- "FAILED"
    return(na_scalars)
  }
  
  if (!response_variable %in% names(best_data)) {
    if (verbose) message("[.compute_curve_diagnostics] response_variable '",
                         response_variable, "' not in best_data")
    na_scalars$fda2018_status <- "FAILED"
    return(na_scalars)
  }
  
  if (!independent_variable %in% names(best_data)) {
    if (verbose) message("[.compute_curve_diagnostics] independent_variable '",
                         independent_variable, "' not in best_data")
    na_scalars$fda2018_status <- "FAILED"
    return(na_scalars)
  }
  
  has_finite_response <- any(is.finite(best_data[[response_variable]]))
  has_finite_indep    <- any(is.finite(best_data[[independent_variable]]))
  if (!has_finite_response || !has_finite_indep) {
    if (verbose) message("[.compute_curve_diagnostics] No finite response or independent values")
    na_scalars$fda2018_status <- "FAILED"
    return(na_scalars)
  }
  
  # ══════════════════════════════════════════════════════════════════════
  # ── Blanks ──
  # ══════════════════════════════════════════════════════════════════════
  plate_blanks <- tryCatch({
    if ("stype" %in% names(best_data)) {
      best_data[best_data$stype == "B", , drop = FALSE]
    } else {
      data.frame()
    }
  }, error = function(e) data.frame())
  
  blank_responses <- if (nrow(plate_blanks) > 0 && response_variable %in% names(plate_blanks)) {
    plate_blanks[[response_variable]]
  } else {
    numeric(0)
  }
  
  Blank_mean <- if (length(blank_responses) > 0) mean(blank_responses, na.rm = TRUE) else NA_real_
  Blank_sd   <- if (length(blank_responses) > 1) sd(blank_responses, na.rm = TRUE)   else NA_real_
  
  # ══════════════════════════════════════════════════════════════════════
  # ── Extract model_name and fixed_a_result ──
  # ══════════════════════════════════════════════════════════════════════
  model_name <- tryCatch({
    best_fit$best_model_name
  }, error = function(e) NA_character_)
  
  fixed_a_result <- tryCatch({
    fa <- antigen_settings$l_asy_min_constraint
    if (!is.null(fa) && is.finite(fa) && fa > 0 &&
        isTRUE(antigen_settings$l_asy_min_constraint == antigen_settings$l_asy_max_constraint)) {
      if (isTRUE(antigen_fit_options$is_log_response) && fa > 0) {
        log10(fa + 0.000005)
      } else {
        fa
      }
    } else {
      NULL
    }
  }, error = function(e) NULL)
  
  # ══════════════════════════════════════════════════════════════════════
  # ── Inflection Point ──
  # ══════════════════════════════════════════════════════════════════════
  inflection <- tryCatch({
    generate_inflection_point(
      model_name           = model_name,
      fit                  = fit,
      fixed_a_result       = fixed_a_result,
      independent_variable = independent_variable,
      verbose              = verbose
    )
  }, error = function(e) {
    if (verbose) message("[.compute_curve_diagnostics] inflection error: ", conditionMessage(e))
    list(inflect_x = NA_real_, inflect_y = NA_real_)
  })
  
  inflect_x <- if (!is.null(inflection$inflect_x) && length(inflection$inflect_x) > 0) {
    as.numeric(inflection$inflect_x)
  } else {
    NA_real_
  }
  
  inflect_y <- if (!is.null(inflection$inflect_y) && length(inflection$inflect_y) > 0) {
    as.numeric(inflection$inflect_y)
  } else {
    NA_real_
  }
  
  # ══════════════════════════════════════════════════════════════════════
  # ── LODs ──
  # ══════════════════════════════════════════════════════════════════════
  std_error_blank <- tryCatch({
    antigen_settings$std_error_blank %||% {
      if (nrow(plate_blanks) > 1 && response_variable %in% names(plate_blanks)) {
        sd(plate_blanks[[response_variable]], na.rm = TRUE) /
          sqrt(sum(!is.na(plate_blanks[[response_variable]])))
      } else {
        0
      }
    }
  }, error = function(e) 0)
  
  lods <- tryCatch({
    generate_lods(
      best_fit        = best_fit,
      fixed_a_result  = fixed_a_result,
      std_error_blank = std_error_blank,
      verbose         = verbose
    )
  }, error = function(e) {
    if (verbose) message("[.compute_curve_diagnostics] LODs error: ", conditionMessage(e))
    list(llod = NA_real_, ulod = NA_real_)
  })
  
  llod <- as.numeric(lods$llod)
  ulod <- as.numeric(lods$ulod)
  
  # ══════════════════════════════════════════════════════════════════════
  # ── Sensitivity (dy/dx at inflection) ──
  # ══════════════════════════════════════════════════════════════════════
  dydx_inflect <- tryCatch({
    if (!is.na(inflect_x) && !is.na(model_name)) {
      all_params <- if (!is.null(fixed_a_result)) {
        c(a = fixed_a_result, coef(fit))
      } else {
        coef(fit)
      }
      dispatch_dydx(model_name, inflect_x, all_params)
    } else {
      NA_real_
    }
  }, error = function(e) {
    tryCatch({
      if (!is.na(inflect_x)) {
        h <- abs(inflect_x) * 1e-5
        if (h == 0) h <- 1e-8
        y_plus  <- predict(fit, newdata = setNames(data.frame(inflect_x + h), independent_variable))
        y_minus <- predict(fit, newdata = setNames(data.frame(inflect_x - h), independent_variable))
        as.numeric((y_plus - y_minus) / (2 * h))
      } else {
        NA_real_
      }
    }, error = function(e2) NA_real_)
  })
  
  # ══════════════════════════════════════════════════════════════════════
  # ── MDC / RDL ──
  # ══════════════════════════════════════════════════════════════════════
  mdc_rdl <- tryCatch({
    generate_mdc_rdl(
      best_fit = list(
        best_fit    = fit,
        best_data   = best_data,
        best_glance = list(llod = llod, ulod = ulod)
      ),
      lods = list(llod = llod, ulod = ulod),
      independent_variable = independent_variable,
      verbose              = verbose
    )
  }, error = function(e) {
    if (verbose) message("[.compute_curve_diagnostics] MDC/RDL error: ", conditionMessage(e))
    list(mindc = NA_real_, maxdc = NA_real_, minrdl = NA_real_, maxrdl = NA_real_)
  })
  
  # ══════════════════════════════════════════════════════════════════════
  # ── Assemble result ──
  # ══════════════════════════════════════════════════════════════════════
  list(
    std_error_blank = antigen_settings$std_error_blank,
    blank_mean      = Blank_mean,
    blank_sd        = Blank_sd,
    llod            = llod,
    ulod            = ulod,
    inflect_x       = inflect_x,
    inflect_y       = inflect_y,
    mindc           = mdc_rdl$mindc  %||% NA_real_,
    maxdc           = mdc_rdl$maxdc  %||% NA_real_,
    minrdl          = mdc_rdl$minrdl %||% NA_real_,
    maxrdl          = mdc_rdl$maxrdl %||% NA_real_,
    dydx_inflect    = dydx_inflect,
    # FDA LOQ placeholders — .extract_fda_loqs_from_dil_series will override
    lloq_fda2018_concentration = NA_real_,
    lloq_fda2018_response      = NA_real_,
    uloq_fda2018_concentration = NA_real_,
    uloq_fda2018_response      = NA_real_,
    lloq_cv         = NA_real_,
    uloq_cv         = NA_real_,
    lloq_accuracy   = NA_real_,
    uloq_accuracy   = NA_real_,
    n_passing_std   = NA_integer_,
    n_total_std     = NA_integer_,
    pct_passing_std = NA_real_,
    fda2018_status  = "PENDING"
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
# This function depends on get_loqs, generate_inflection_point, generate_lods as it is a wrapper and calls them
# fit_qc_glance()
#
# Wrapper that assembles the QC-glance data-frame for a single plate fit.
# Optionally computes the six FDA-2018 LOQ scalars in the same pass so that
# no second call is required.
#
# New / changed arguments vs. the original:
#   plate_blanks    – data.frame of blank wells with column `mfi` (raw,
#                     linear scale). Pass NULL to skip FDA-2018 computation.
#   fda2018_options – named list with any of:
#                       cv_threshold      (default 20)
#                       lloq_cv_threshold (default 25)
#                       accuracy_lo       (default 80)
#                       accuracy_hi       (default 120)
#                     Pass NULL (default) to use all defaults.
#
# All other arguments are unchanged from the original.
fit_qc_glance <- function(best_fit,
                          response_variable,
                          independent_variable,
                          fixed_a_result,
                          antigen_settings,
                          antigen_fit_options,
                          dil_series_se_plate_source = NULL,
                          verbose = TRUE) {
  
  fit        <- best_fit$best_fit
  best_data  <- best_fit$best_data
  model_name <- best_fit$best_model_name
  
  # ── Early exit if no valid fit ──
  if (is.null(fit) || is.null(best_data) || nrow(best_data) == 0) {
    if (verbose) message("[fit_qc_glance] No valid fit or data -> NA glance")
    best_fit$best_glance <- .make_na_glance(best_data, model_name,
                                            fixed_a_result, antigen_fit_options)
    return(best_fit)
  }
  
  # ══════════════════════════════════════════════════════════════════════
  # ── Step 1: Compute LODs, inflection, MDC/RDL, blanks ──
  # ══════════════════════════════════════════════════════════════════════
  qc_glance <- tryCatch(
    .compute_curve_diagnostics(
      fit                  = fit,
      best_data            = best_data,
      best_fit             = best_fit,
      response_variable    = response_variable,
      independent_variable = independent_variable,
      antigen_settings     = antigen_settings,
      antigen_fit_options  = antigen_fit_options,
      verbose              = verbose
    ),
    error = function(e) {
      if (verbose) message("[fit_qc_glance] .compute_curve_diagnostics error: ",
                           conditionMessage(e))
      .na_qc_glance_list()
    }
  )
  
  cat("Head of QC glance\n")
  print(head(qc_glance))
  
  # ══════════════════════════════════════════════════════════════════════
  # ── Step 2: Shape/curvature-based LOQs (from second derivative) ──
  # ══════════════════════════════════════════════════════════════════════
  curv_loqs <- tryCatch({
    if (!is.null(best_fit$best_d2xy) && nrow(best_fit$best_d2xy) >= 3) {
      get_loqs(
        best_d2xy            = best_fit$best_d2xy,
        fit                  = fit,
        independent_variable = independent_variable,
        verbose              = verbose
      )
    } else {
      if (verbose) message("[fit_qc_glance] No best_d2xy available for curvature LOQs")
      list(lloq = NA_real_, uloq = NA_real_, lloq_y = NA_real_, uloq_y = NA_real_)
    }
  }, error = function(e) {
    if (verbose) message("[fit_qc_glance] get_loqs error: ", conditionMessage(e))
    list(lloq = NA_real_, uloq = NA_real_, lloq_y = NA_real_, uloq_y = NA_real_)
  })
  
  # ══════════════════════════════════════════════════════════════════════
  # ── Step 3: Merge shape-based LOQs into qc_glance ──
  # ══════════════════════════════════════════════════════════════════════
  qc_glance$lloq   <- curv_loqs$lloq
  qc_glance$uloq   <- curv_loqs$uloq
  qc_glance$lloq_y <- curv_loqs$lloq_y
  qc_glance$uloq_y <- curv_loqs$uloq_y
  
  if (verbose) {
    message(sprintf("[fit_qc_glance] Shape LOQs: LLOQ=%.4g, ULOQ=%.4g",
                    ifelse(is.na(curv_loqs$lloq), NaN, curv_loqs$lloq),
                    ifelse(is.na(curv_loqs$uloq), NaN, curv_loqs$uloq)))
  }
  
  # ══════════════════════════════════════════════════════════════════════
  # ── Step 3b: Dilution-series accuracy/precision summary ──
  # ══════════════════════════════════════════════════════════════════════
  dil_summary <- .summarize_dil_series_accuracy(
    dil_series_se_plate_source = dil_series_se_plate_source,
    verbose                    = verbose
  )
  
  # Merge dilution-series summary scalars into qc_glance
  for (nm in names(dil_summary)) {
    qc_glance[[nm]] <- dil_summary[[nm]]
  }
  
  # ══════════════════════════════════════════════════════════════════════
  # ── Step 3c: Populate FDA 2018 LOQ columns from dil_series results ──
  # ══════════════════════════════════════════════════════════════════════
  fda_from_dil <- .extract_fda_loqs_from_dil_series(
    dil_series_se_plate_source = dil_series_se_plate_source,
    fit                        = fit,
    independent_variable       = independent_variable,
    is_log_x                   = isTRUE(antigen_fit_options$is_log_concentration),
    verbose                    = verbose
  )
  
  # Override the FDA columns with dilution-series-derived values
  for (nm in names(fda_from_dil)) {
    qc_glance[[nm]] <- fda_from_dil[[nm]]
  }
  
  if (verbose) {
    message(sprintf(
      "[fit_qc_glance] FDA LOQs (from dil_series): LLOQ=%.4g, ULOQ=%.4g, status=%s",
      ifelse(is.na(qc_glance$lloq_fda2018_concentration), NaN,
             qc_glance$lloq_fda2018_concentration),
      ifelse(is.na(qc_glance$uloq_fda2018_concentration), NaN,
             qc_glance$uloq_fda2018_concentration),
      qc_glance$fda2018_status
    ))
  }
  
  # ══════════════════════════════════════════════════════════════════════
  # ── Step 4: Build the glance data.frame ──
  # ══════════════════════════════════════════════════════════════════════
  qc_df <- as.data.frame(
    lapply(qc_glance, function(v) if (is.null(v) || length(v) == 0) NA_real_ else v[[1]]),
    stringsAsFactors = FALSE
  )
  
  safe_unique <- function(x) {
    u <- unique(x); u <- u[!is.na(u)]
    if (length(u) == 0) NA_character_ else paste(u, collapse = ";")
  }
  
  # Extract model coefficients
  coefs <- tryCatch(coef(fit), error = function(e) c(a=NA, b=NA, c=NA, d=NA, g=NA))
  
  # Goodness-of-fit
  rss       <- tryCatch(sum(residuals(fit)^2), error = function(e) NA_real_)
  df_resid  <- tryCatch(df.residual(fit), error = function(e) NA_real_)
  nobs_fit  <- tryCatch(nobs(fit), error = function(e) NA_integer_)
  aic_val   <- tryCatch(AIC(fit), error = function(e) NA_real_)
  bic_val   <- tryCatch(BIC(fit), error = function(e) NA_real_)
  loglik_val <- tryCatch(as.numeric(logLik(fit)), error = function(e) NA_real_)
  converged   <- fit$convInfo$isConv
  iter        <- fit$convInfo$finIter
  mse_val   <- if (!is.na(rss) && !is.na(df_resid) && df_resid > 0) rss / df_resid else NA_real_
  
  # R-squared
  y_obs     <- tryCatch(best_data[[response_variable]], error = function(e) numeric(0))
  y_pred    <- tryCatch(predict(fit, newdata = best_data), error = function(e) numeric(0))
  rsq       <- if (length(y_obs) > 1 && length(y_pred) == length(y_obs)) {
    ss_res <- sum((y_obs - y_pred)^2, na.rm = TRUE)
    ss_tot <- sum((y_obs - mean(y_obs, na.rm = TRUE))^2, na.rm = TRUE)
    if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_
  } else NA_real_
  
  cv_val <- if (!is.na(mse_val) && length(y_obs) > 0) {
    sqrt(mse_val) / mean(y_obs, na.rm = TRUE) * 100
  } else NA_real_
  
  glance_df <- data.frame(
    study_accession         = safe_unique(best_data$study_accession),
    experiment_accession    = safe_unique(best_data$experiment_accession),
    plateid                 = safe_unique(best_data$plateid),
    plate                   = safe_unique(best_data$plate),
    nominal_sample_dilution = safe_unique(best_data$nominal_sample_dilution),
    antigen                 = safe_unique(best_data$antigen),
    iter                    = iter,
    status                  = converged,
    crit                    = as.character(model_name),
    a  = as.numeric(coefs["a"] %||% fixed_a_result %||% NA_real_),
    b  = as.numeric(coefs["b"] %||% NA_real_),
    c  = as.numeric(coefs["c"] %||% NA_real_),
    d  = as.numeric(coefs["d"] %||% NA_real_),
    g  = as.numeric(coefs["g"] %||% NA_real_),
    stringsAsFactors = FALSE
  )
  
  # Bind the QC columns
  glance_df <- cbind(glance_df, qc_df)
  
  # Add remaining fit stats
  glance_df$dfresidual  <- df_resid
  glance_df$nobs        <- nobs_fit
  glance_df$rsquare_fit <- rsq
  glance_df$aic         <- aic_val
  glance_df$bic         <- bic_val
  glance_df$loglik      <- loglik_val
  glance_df$mse         <- mse_val
  glance_df$cv          <- cv_val
  glance_df$source      <- safe_unique(best_data$source)
  glance_df$bkg_method  <- antigen_fit_options$blank_option %||% NA_character_
  glance_df$is_log_response <- antigen_fit_options$is_log_response %||% NA
  glance_df$is_log_x    <- antigen_fit_options$is_log_concentration %||% NA
  glance_df$apply_prozone <- antigen_fit_options$apply_prozone %||% NA
  glance_df$formula     <- tryCatch(sub("I\\((.*)\\)", "\\1", paste(deparse(formula(fit)), collapse = " ")), error = function(e) NA_character_)
  glance_df$last_concentration_calc_method <- "interpolated"
  
  glance_df <- attach_grouping_keys(glance_df, best_data, context = "fit_qc_glance")
  
  drop_cols <- c(
    "lloq_cv", "uloq_cv", "lloq_accuracy", "uloq_accuracy",
    "n_passing_std", "n_total_std", "pct_passing_std", "fda2018_status",
    "dil_n_points_total", "dil_n_points_evaluable",
    "dil_n_points_pass_fda", "dil_n_points_fail_fda",
    "dil_pct_pass_fda", "dil_median_accuracy_pct", "dil_mean_accuracy_pct",
    "dil_max_cv_response", "dil_mean_cv_response",
    "dil_fda_overall_pass", "dil_fda_flags_summary",
    "dil_accuracy_range_lo", "dil_accuracy_range_hi"
  )
  
  glance_df <- glance_df[, !names(glance_df) %in% drop_cols, drop = FALSE]
  
  best_fit$best_glance <- glance_df
  
  # # ══════════════════════════════════════════════════════════════════════
  # # ── Step 5: Attach the full dilution-series detail table ──
  # # ══════════════════════════════════════════════════════════════════════
  # best_fit$dil_series_accuracy <- dil_series_se_plate_source
  # 
  return(best_fit)
}
# fit_qc_glance <- function(best_fit,
#                           response_variable,
#                           independent_variable,
#                           fixed_a_result,
#                           antigen_settings,
#                           antigen_fit_options,
#                           dil_series_se_plate_source = NULL,
#                           verbose = TRUE) {
#   
#   fit        <- best_fit$best_fit
#   best_data  <- best_fit$best_data
#   model_name <- best_fit$best_model_name
#   
#   # ── Early exit if no valid fit ──
#   if (is.null(fit) || is.null(best_data) || nrow(best_data) == 0) {
#     if (verbose) message("[fit_qc_glance] No valid fit or data -> NA glance")
#     best_fit$best_glance <- .make_na_glance(best_data, model_name,
#                                             fixed_a_result, antigen_fit_options)
#     return(best_fit)
#   }
#   
#   # ══════════════════════════════════════════════════════════════════════
#   # ── Step 1: FDA 2018 scalars (LODs, FDA LOQs, blanks, MDC/RDL, etc.) ──
#   # ══════════════════════════════════════════════════════════════════════
#   qc_glance <- tryCatch(
#     .compute_fda2018_scalars(
#       fit                  = fit,
#       best_data            = best_data,
#       best_fit             = best_fit,
#       response_variable    = response_variable,
#       independent_variable = independent_variable,
#       antigen_settings     = antigen_settings,
#       antigen_fit_options  = antigen_fit_options,
#       dil_series_se_plate_source = dil_series_se_plate_source,
#       verbose              = verbose
#     ),
#     error = function(e) {
#       if (verbose) message("[fit_qc_glance] .compute_fda2018_scalars error: ",
#                            conditionMessage(e))
#       .na_qc_glance_list()
#     }
#   )
#   
#   cat("Head of QC glance\n")
#   print(head(qc_glance))
#   
#   # ══════════════════════════════════════════════════════════════════════
#   # ── Step 2: Shape/curvature-based LOQs (from second derivative) ──
#   # ══════════════════════════════════════════════════════════════════════
#   curv_loqs <- tryCatch({
#     if (!is.null(best_fit$best_d2xy) && nrow(best_fit$best_d2xy) >= 3) {
#       get_loqs(
#         best_d2xy            = best_fit$best_d2xy,
#         fit                  = fit,
#         independent_variable = independent_variable,
#         verbose              = verbose
#       )
#     } else {
#       if (verbose) message("[fit_qc_glance] No best_d2xy available for curvature LOQs")
#       list(lloq = NA_real_, uloq = NA_real_, lloq_y = NA_real_, uloq_y = NA_real_)
#     }
#   }, error = function(e) {
#     if (verbose) message("[fit_qc_glance] get_loqs error: ", conditionMessage(e))
#     list(lloq = NA_real_, uloq = NA_real_, lloq_y = NA_real_, uloq_y = NA_real_)
#   })
#   
#   # ══════════════════════════════════════════════════════════════════════
#   # ── Step 3: Merge shape-based LOQs into qc_glance ──
#   # ══════════════════════════════════════════════════════════════════════
#   qc_glance$lloq   <- curv_loqs$lloq
#   qc_glance$uloq   <- curv_loqs$uloq
#   qc_glance$lloq_y <- curv_loqs$lloq_y
#   qc_glance$uloq_y <- curv_loqs$uloq_y
#   
#   if (verbose) {
#     message(sprintf("[fit_qc_glance] Shape LOQs: LLOQ=%.4g, ULOQ=%.4g",
#                     ifelse(is.na(curv_loqs$lloq), NaN, curv_loqs$lloq),
#                     ifelse(is.na(curv_loqs$uloq), NaN, curv_loqs$uloq)))
#     message(sprintf("[fit_qc_glance] FDA LOQs:   LLOQ=%.4g, ULOQ=%.4g",
#                     ifelse(is.na(qc_glance$lloq_fda2018_concentration), NaN,
#                            qc_glance$lloq_fda2018_concentration),
#                     ifelse(is.na(qc_glance$uloq_fda2018_concentration), NaN,
#                            qc_glance$uloq_fda2018_concentration)))
#   }
#   
#   # ══════════════════════════════════════════════════════════════════════
#   # ── Step 3b: Dilution-series accuracy/precision summary ──
#   # ══════════════════════════════════════════════════════════════════════
#   dil_summary <- .summarize_dil_series_accuracy(
#     dil_series_se_plate_source = dil_series_se_plate_source,
#     verbose                    = verbose
#   )
#   # Merge dilution-series summary scalars into qc_glance
#   for (nm in names(dil_summary)) {
#     qc_glance[[nm]] <- dil_summary[[nm]]
#   }
#   
#   # ══════════════════════════════════════════════════════════════════════
#   # ── Step 4: Build the glance data.frame ──
#   # ══════════════════════════════════════════════════════════════════════
#   qc_df <- as.data.frame(
#     lapply(qc_glance, function(v) if (is.null(v) || length(v) == 0) NA_real_ else v[[1]]),
#     stringsAsFactors = FALSE
#   )
#   
#   safe_unique <- function(x) {
#     u <- unique(x); u <- u[!is.na(u)]
#     if (length(u) == 0) NA_character_ else paste(u, collapse = ";")
#   }
#   
#   # Extract model coefficients
#   coefs <- tryCatch(coef(fit), error = function(e) c(a=NA, b=NA, c=NA, d=NA, g=NA))
#   
#   # Goodness-of-fit
#   rss       <- tryCatch(sum(residuals(fit)^2), error = function(e) NA_real_)
#   df_resid  <- tryCatch(df.residual(fit), error = function(e) NA_real_)
#   nobs_fit  <- tryCatch(nobs(fit), error = function(e) NA_integer_)
#   aic_val   <- tryCatch(AIC(fit), error = function(e) NA_real_)
#   bic_val   <- tryCatch(BIC(fit), error = function(e) NA_real_)
#   loglik_val<- tryCatch(as.numeric(logLik(fit)), error = function(e) NA_real_)
#   mse_val   <- if (!is.na(rss) && !is.na(df_resid) && df_resid > 0) rss / df_resid else NA_real_
#   
#   # R-squared
#   y_obs     <- tryCatch(best_data[[response_variable]], error = function(e) numeric(0))
#   y_pred    <- tryCatch(predict(fit, newdata = best_data), error = function(e) numeric(0))
#   rsq       <- if (length(y_obs) > 1 && length(y_pred) == length(y_obs)) {
#     ss_res <- sum((y_obs - y_pred)^2, na.rm = TRUE)
#     ss_tot <- sum((y_obs - mean(y_obs, na.rm = TRUE))^2, na.rm = TRUE)
#     if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_
#   } else NA_real_
#   
#   cv_val <- if (!is.na(mse_val) && length(y_obs) > 0) {
#     sqrt(mse_val) / mean(y_obs, na.rm = TRUE) * 100
#   } else NA_real_
#   
#   glance_df <- data.frame(
#     study_accession         = safe_unique(best_data$study_accession),
#     experiment_accession    = safe_unique(best_data$experiment_accession),
#     plateid                 = safe_unique(best_data$plateid),
#     plate                   = safe_unique(best_data$plate),
#     nominal_sample_dilution = safe_unique(best_data$nominal_sample_dilution),
#     antigen                 = safe_unique(best_data$antigen),
#     iter                    = NA_integer_,
#     status                  = TRUE,
#     crit                    = as.character(model_name),
#     a  = as.numeric(coefs["a"] %||% fixed_a_result %||% NA_real_),
#     b  = as.numeric(coefs["b"] %||% NA_real_),
#     c  = as.numeric(coefs["c"] %||% NA_real_),
#     d  = as.numeric(coefs["d"] %||% NA_real_),
#     g  = as.numeric(coefs["g"] %||% NA_real_),
#     stringsAsFactors = FALSE
#   )
#   
#   # Bind the QC columns
#   glance_df <- cbind(glance_df, qc_df)
#   
#   # Add remaining fit stats
#   glance_df$dfresidual  <- df_resid
#   glance_df$nobs        <- nobs_fit
#   glance_df$rsquare_fit <- rsq
#   glance_df$aic         <- aic_val
#   glance_df$bic         <- bic_val
#   glance_df$loglik      <- loglik_val
#   glance_df$mse         <- mse_val
#   glance_df$cv          <- cv_val
#   glance_df$source      <- safe_unique(best_data$source)
#   glance_df$bkg_method  <- antigen_fit_options$bkg_method %||% NA_character_
#   glance_df$is_log_response <- antigen_fit_options$is_log_response %||% NA
#   glance_df$is_log_x    <- antigen_fit_options$is_log_x %||% NA
#   glance_df$apply_prozone <- antigen_fit_options$apply_prozone %||% NA
#   glance_df$formula     <- tryCatch(deparse(formula(fit)), error = function(e) NA_character_)
#   
#   best_fit$best_glance <- glance_df
#   
#   # ══════════════════════════════════════════════════════════════════════
#   # ── Step 5: Attach the full dilution-series detail table ──
#   # ══════════════════════════════════════════════════════════════════════
#   best_fit$dil_series_accuracy <- dil_series_se_plate_source
#   
#   return(best_fit)
# }


# ── Helper: NA scaffold for qc_glance list ──────────────────────────────
.na_qc_glance_list <- function() {
  list(
    lloq            = NA_real_,
    uloq            = NA_real_,
    lloq_y          = NA_real_,
    uloq_y          = NA_real_,
    # ── FDA 2018-based LOQs (populated from dil_series) ──
    lloq_fda2018_concentration = NA_real_,
    lloq_fda2018_response      = NA_real_,
    uloq_fda2018_concentration = NA_real_,
    uloq_fda2018_response      = NA_real_,
    lloq_cv         = NA_real_,
    uloq_cv         = NA_real_,
    lloq_accuracy   = NA_real_,
    uloq_accuracy   = NA_real_,
    n_passing_std   = NA_integer_,
    n_total_std     = NA_integer_,
    pct_passing_std = NA_real_,
    fda2018_status  = "FAILED",
    # ── Curve diagnostics ──
    blank_mean      = NA_real_,
    blank_sd        = NA_real_,
    llod            = NA_real_,
    ulod            = NA_real_,
    inflect_x       = NA_real_,
    inflect_y       = NA_real_,
    mindc           = NA_real_,
    maxdc           = NA_real_,
    minrdl          = NA_real_,
    maxrdl          = NA_real_,
    dydx_inflect    = NA_real_,
    # ── Dilution-series summary (populated from .summarize_dil_series_accuracy) ──
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
}
# .na_qc_glance_list <- function() {
#   list(
#     lloq            = NA_real_,
#     uloq            = NA_real_,
#     lloq_y          = NA_real_,
#     uloq_y          = NA_real_,
#     # ── FDA 2018-based LOQs ──
#     lloq_fda2018_concentration = NA_real_,
#     lloq_fda2018_response      = NA_real_,
#     uloq_fda2018_concentration = NA_real_,
#     uloq_fda2018_response      = NA_real_,
#     lloq_cv         = NA_real_,
#     uloq_cv         = NA_real_,
#     lloq_accuracy   = NA_real_,
#     uloq_accuracy   = NA_real_,
#     n_passing_std   = NA_integer_,
#     n_total_std     = NA_integer_,
#     pct_passing_std = NA_real_,
#     fda2018_status  = "FAILED",
#     Blank_mean      = NA_real_,
#     Blank_sd        = NA_real_,
#     llod            = NA_real_,
#     ulod            = NA_real_,
#     inflect_x       = NA_real_,
#     inflect_y       = NA_real_,
#     lloq_method     = "fda2018",
#     uloq_method     = "fda2018",
#     mindc           = NA_real_,
#     maxdc           = NA_real_,
#     minrdl          = NA_real_,
#     maxrdl          = NA_real_,
#     dydx_inflect    = NA_real_
#   )
# }


# ── Helper: full NA glance data.frame for early-exit paths ───────────────
.make_na_glance <- function(best_data,
                            model_name,
                            fixed_a_result,
                            antigen_fit_options,
                            qc_glance = NULL) {
  safe_unique <- function(x) {
    u <- unique(x); u <- u[!is.na(u)]
    if (length(u) == 0) NA_character_ else paste(u, collapse = ";")
  }
  
  if (is.null(qc_glance)) qc_glance <- .na_qc_glance_list()
  
  qc_df <- as.data.frame(
    lapply(qc_glance, function(v) if (is.null(v) || length(v) == 0) NA_real_ else v[[1]]),
    stringsAsFactors = FALSE
  )
  
  base_df <- data.frame(
    study_accession         = safe_unique(best_data$study_accession),
    experiment_accession    = safe_unique(best_data$experiment_accession),
    plateid                 = safe_unique(best_data$plateid),
    plate                   = safe_unique(best_data$plate),
    nominal_sample_dilution = safe_unique(best_data$nominal_sample_dilution),
    antigen                 = safe_unique(best_data$antigen),
    iter                    = NA_integer_,
    status                  = FALSE,
    crit                    = as.character(model_name),
    a                       = if (!is.null(fixed_a_result)) as.numeric(fixed_a_result) else NA_real_,
    b = NA_real_, c = NA_real_, d = NA_real_, g = NA_real_,
    stringsAsFactors        = FALSE
  )
  
  base_df <- cbind(base_df, qc_df)
  
  base_df$dfresidual      <- NA_real_
  base_df$nobs            <- NA_integer_
  base_df$rsquare_fit     <- NA_real_
  base_df$aic             <- NA_real_
  base_df$bic             <- NA_real_
  base_df$loglik          <- NA_real_
  base_df$mse             <- NA_real_
  base_df$cv              <- NA_real_
  base_df$source          <- safe_unique(best_data$source)
  base_df$bkg_method      <- NA_character_
  base_df$is_log_response <- NA
  base_df$is_log_x        <- NA
  base_df$apply_prozone   <- NA
  base_df$formula         <- NA_character_
  
  # ── Ensure wavelength/feature are carried from best_data ──
  base_df <- attach_grouping_keys(base_df, best_data, context = ".make_na_glance")
  
  base_df
}
# .make_na_glance <- function(best_data,
#                             model_name,
#                             fixed_a_result,
#                             antigen_fit_options,
#                             qc_glance = NULL) {
#   
#   safe_unique <- function(x) {
#     u <- unique(x); u <- u[!is.na(u)]
#     if (length(u) == 0) NA_character_ else paste(u, collapse = ";")
#   }
#   
#   if (is.null(qc_glance)) qc_glance <- .na_qc_glance_list()
#   
#   qc_df <- as.data.frame(
#     lapply(qc_glance, function(v) if (is.null(v) || length(v) == 0) NA_real_ else v[[1]]),
#     stringsAsFactors = FALSE
#   )
#   
#   base_df <- data.frame(
#     study_accession         = safe_unique(best_data$study_accession),
#     experiment_accession    = safe_unique(best_data$experiment_accession),
#     plateid                 = safe_unique(best_data$plateid),
#     plate                   = safe_unique(best_data$plate),
#     nominal_sample_dilution = safe_unique(best_data$nominal_sample_dilution),
#     antigen                 = safe_unique(best_data$antigen),
#     iter                    = NA_integer_,
#     status                  = FALSE,
#     crit                    = as.character(model_name),
#     a                       = if (!is.null(fixed_a_result)) as.numeric(fixed_a_result) else NA_real_,
#     b = NA_real_, c = NA_real_, d = NA_real_, g = NA_real_,
#     stringsAsFactors        = FALSE
#   )
#   
#   base_df <- cbind(base_df, qc_df)
#   
#   base_df$dfresidual      <- NA_real_
#   base_df$nobs            <- NA_integer_
#   base_df$rsquare_fit     <- NA_real_
#   base_df$aic             <- NA_real_
#   base_df$bic             <- NA_real_
#   base_df$loglik          <- NA_real_
#   base_df$mse             <- NA_real_
#   base_df$cv              <- NA_real_
#   base_df$source          <- safe_unique(best_data$source)
#   base_df$bkg_method      <- NA_character_
#   base_df$is_log_response <- NA
#   base_df$is_log_x        <- NA
#   base_df$apply_prozone   <- NA
#   base_df$formula         <- NA_character_
#   
#   # ── Ensure wavelength/feature are carried from best_data ──
#   base_df <- attach_grouping_keys(base_df, best_data, context = ".make_na_glance")
#   
#   base_df
# }

# Compute FDA (2018) LLOQ, ULOQ, Blank_mean, and Blank_SD for a single plate.
#
# Protocol (FDA Bioanalytical Method Validation Guidance, 2018 / DeSilva 2003):
#   LLOQ: lowest nominal concentration at which %CV ≤ lloq_cv_threshold (25%)
#         AND % recovery is within [accuracy_lo, accuracy_hi] (80–120%).
#   ULOQ: highest nominal concentration at which %CV ≤ cv_threshold (20%)
#         AND % recovery is within [accuracy_lo, accuracy_hi].
#   Blank_mean / Blank_SD: mean and SD of raw blank (PBS) MFI values.
#
# Arguments:
#   best_fit           – list returned by select_model_fit_AIC() (and fit_qc_glance()).
#   plate_blanks       – data.frame of blank wells with column `mfi` (raw, linear scale).
#   response_variable  – character name of the MFI column in best_data.
#   independent_variable – character name of the log10-concentration column in best_data.
#   fixed_a_result     – fixed lower-asymptote value (NULL if not fixed).
#   is_log_response    – logical; TRUE if the response was log10-transformed before fitting.
#   cv_threshold       – %CV acceptance limit for all levels except LLOQ (default 20).
#   lloq_cv_threshold  – %CV acceptance limit at the LLOQ level (default 25, per FDA 2018).
#   accuracy_lo / accuracy_hi – % recovery acceptance window (default 80–120%).
#
# Returns:
#   best_fit with a new element $fda2018 containing a named list of the six scalars
#   plus a fda2018_loq_table data.frame with per-level diagnostics.
# fit_fda2018_loq <- function(best_fit,
#                              plate_blanks,
#                              response_variable,
#                              independent_variable,
#                              fixed_a_result    = NULL,
#                              is_log_response   = FALSE,
#                              cv_threshold      = 20,
#                              lloq_cv_threshold = 25,
#                              accuracy_lo       = 80,
#                              accuracy_hi       = 120,
#                              verbose           = TRUE) {
#
#   fit        <- best_fit$best_fit
#   best_data  <- best_fit$best_data
#   model_name <- best_fit$best_model_name
#
#   # --- 1. Blank statistics from raw plate_blanks (mfi column, linear scale) ---
#   if (!is.null(plate_blanks) && nrow(plate_blanks) > 0) {
#     blank_vals <- plate_blanks$mfi
#     Blank_mean <- mean(blank_vals, na.rm = TRUE)
#     Blank_SD   <- sd(blank_vals,   na.rm = TRUE)
#     if (is.na(Blank_SD)) Blank_SD <- 0
#   } else {
#     Blank_mean <- NA_real_
#     Blank_SD   <- NA_real_
#   }
#
#   # --- 2. Strip the geometric-mean blank row (stype == "B") from best_data ---
#   if ("stype" %in% names(best_data)) {
#     std_data <- best_data[is.na(best_data$stype) | best_data$stype != "B", ]
#   } else {
#     std_data <- best_data
#   }
#
#   na_result <- list(
#     LLOQ_FDA2018_response      = NA_real_,
#     LLOQ_FDA2018_concentration = NA_real_,
#     ULOQ_FDA2018_response      = NA_real_,
#     ULOQ_FDA2018_concentration = NA_real_,
#     Blank_mean                 = Blank_mean,
#     Blank_SD                   = Blank_SD,
#     fda2018_loq_table          = NULL
#   )
#
#   if (nrow(std_data) == 0) {
#     best_fit$fda2018 <- na_result
#     return(best_fit)
#   }
#
#   # --- 3. Back-calculate concentration for every standard well ---
#   std_data_bc <- calculate_predicted_concentration(
#     model_name        = model_name,
#     fit               = fit,
#     plate_samples     = std_data,
#     fixed_constraint  = fixed_a_result,
#     response_variable = response_variable,
#     is_log_response   = is_log_response,
#     verbose           = verbose
#   )
#   # predicted_concentration is on the log10 concentration scale
#
#   # --- 4. Per-level %CV and % recovery ---
#   nominal_concs <- sort(unique(std_data_bc[[independent_variable]]))
#
#   loq_rows <- lapply(nominal_concs, function(nom_x) {
#     rows <- std_data_bc[std_data_bc[[independent_variable]] == nom_x, ]
#     bc   <- rows$predicted_concentration   # log10-scale
#     bc   <- bc[!is.na(bc)]
#     n    <- length(bc)
#     if (n < 1) return(NULL)
#
#     mean_bc      <- mean(bc)
#     sd_bc        <- if (n > 1) sd(bc) else NA_real_
#     cv_pct       <- if (!is.na(sd_bc) && mean_bc != 0) (sd_bc / abs(mean_bc)) * 100 else NA_real_
#     pct_recovery <- (10^mean_bc / 10^nom_x) * 100   # both converted to linear scale
#
#     data.frame(
#       nominal_log_x = nom_x,
#       nominal_conc  = 10^nom_x,
#       n_reps        = n,
#       mean_log_bc   = mean_bc,
#       sd_log_bc     = sd_bc,
#       cv_pct        = cv_pct,
#       pct_recovery  = pct_recovery,
#       stringsAsFactors = FALSE
#     )
#   })
#
#   loq_table <- do.call(rbind, Filter(Negate(is.null), loq_rows))
#
#   if (is.null(loq_table) || nrow(loq_table) == 0) {
#     best_fit$fda2018 <- na_result
#     return(best_fit)
#   }
#
#   # --- 5. Flag passing levels ---
#   passes_accuracy <- function(pct_rec) !is.na(pct_rec) & pct_rec >= accuracy_lo & pct_rec <= accuracy_hi
#
#   loq_table$passes_std  <- !is.na(loq_table$cv_pct) & loq_table$cv_pct <= cv_threshold      & passes_accuracy(loq_table$pct_recovery)
#   loq_table$passes_lloq <- !is.na(loq_table$cv_pct) & loq_table$cv_pct <= lloq_cv_threshold & passes_accuracy(loq_table$pct_recovery)
#
#   lloq_candidates <- which(loq_table$passes_lloq)
#   passing_std     <- which(loq_table$passes_std)
#
#   # Helper: fitted response (linear scale) at a given log10-concentration
#   pred_response_at <- function(log_x) {
#     nd   <- setNames(data.frame(log_x), independent_variable)
#     yhat <- as.numeric(predict(fit, newdata = nd))
#     if (is_log_response) 10^yhat else yhat
#   }
#
#   lloq_conc     <- NA_real_
#   lloq_response <- NA_real_
#   uloq_conc     <- NA_real_
#   uloq_response <- NA_real_
#
#   if (length(lloq_candidates) > 0) {
#     lloq_row      <- loq_table[min(lloq_candidates), ]
#     lloq_conc     <- lloq_row$nominal_conc
#     lloq_response <- pred_response_at(lloq_row$nominal_log_x)
#   }
#
#   if (length(passing_std) > 0) {
#     uloq_row      <- loq_table[max(passing_std), ]
#     uloq_conc     <- uloq_row$nominal_conc
#     uloq_response <- pred_response_at(uloq_row$nominal_log_x)
#   }
#
#   if (verbose) {
#     message(sprintf(
#       "FDA2018 LOQ — LLOQ: conc=%s, response=%s | ULOQ: conc=%s, response=%s",
#       format(lloq_conc, digits = 4), format(lloq_response, digits = 4),
#       format(uloq_conc, digits = 4), format(uloq_response, digits = 4)
#     ))
#   }
#
#   best_fit$fda2018 <- list(
#     LLOQ_FDA2018_response      = lloq_response,
#     LLOQ_FDA2018_concentration = lloq_conc,
#     ULOQ_FDA2018_response      = uloq_response,
#     ULOQ_FDA2018_concentration = uloq_conc,
#     Blank_mean                 = Blank_mean,
#     Blank_SD                   = Blank_SD
#     # ,
#     # fda2018_loq_table          = loq_table
#   )
#
#   return(best_fit)
# }

get_loqs <- function(best_d2xy, fit, independent_variable,  verbose = TRUE) {
  y <- as.numeric(best_d2xy$d2x_y)
  x <- as.numeric(best_d2xy$x)

  n <- length(y)
  if (n < 3) stop("Need at least 3 points to detect local extrema.")

  # first differences
  dy <- diff(y)

  # candidate interior indices where slope changes sign
  idx_max <- which(dy[-1] < 0 & dy[-length(dy)] > 0) + 1  # local max neighborhood
  idx_min <- which(dy[-1] > 0 & dy[-length(dy)] < 0) + 1  # local min neighborhood

  interpolate_vertex <- function(i) {
    # use points (i-1, i, i+1)
    xi <- x[(i-1):(i+1)]
    yi <- y[(i-1):(i+1)]

    # fit quadratic: y = a*x^2 + b*x + c
    X <- cbind(xi^2, xi, 1)
    coef <- solve(t(X) %*% X, t(X) %*% yi)  # least squares for robustness
    a <- coef[1]; b <- coef[2]; c <- coef[3]

    if (a == 0) {
      # fallback: no curvature, just return middle point
      return(list(x = xi[2], y = yi[2]))
    }

    # vertex of parabola: x* = -b / (2a)
    xv <- -b / (2 * a)

    # clamp to local interval [x_{i-1}, x_{i+1}] to avoid nonsense extrapolation
    xv <- max(min(xv, max(xi)), min(xi))

    yv <- a * xv^2 + b * xv + c
    list(x = xv, y = yv)
  }

  # interpolate for each candidate
  if (length(idx_max) > 0) {
    max_list <- lapply(idx_max, interpolate_vertex)
    max_df <- data.frame(
      x = vapply(max_list, `[[`, numeric(1), "x"),
      y = vapply(max_list, `[[`, numeric(1), "y"),
      i_center = idx_max
    )
  } else {
    max_df <- data.frame(x = numeric(0), y = numeric(0), i_center = integer(0))
  }

  if (length(idx_min) > 0) {
    min_list <- lapply(idx_min, interpolate_vertex)
    min_df <- data.frame(
      x = vapply(min_list, `[[`, numeric(1), "x"),
      y = vapply(min_list, `[[`, numeric(1), "y"),
      i_center = idx_min
    )
  } else {
    min_df <- data.frame(x = numeric(0), y = numeric(0), i_center = integer(0))
  }

  # # also give global (approximate) max/min among these
  # global_max <- if (nrow(max_df) > 0) max_df[which.max(max_df$y), ] else NULL

  # all_x <- c(max_df$x,min_df$x)
  # Handle empty dataframes and ensure scalar values are returned
  if (nrow(max_df) > 0) {
    lloq_x <- as.numeric(max_df[which.max(max_df$y), "x"][1])
  } else {
    lloq_x <- NA_real_
  }

  if (nrow(min_df) > 0) {
    uloq_x <- as.numeric(min_df[which.min(min_df$y), "x"][1])
  } else {
    uloq_x <- NA_real_
  }

  # global_min <- if (nrow(min_df) > 0) min_df[which.min(min_df$y), ] else NULL

  y_loq <- tryCatch({
    predict(fit, newdata = setNames(data.frame(x = c(lloq_x, uloq_x)), independent_variable))
  }, error = function(e) rep(NA_real_, 2))

  # Ensure lloq_y and uloq_y are scalar numeric values
  lloq_y <- if (length(y_loq) >= 1 && !all(is.na(y_loq))) as.numeric(min(y_loq, na.rm = TRUE)) else NA_real_
  uloq_y <- if (length(y_loq) >= 1 && !all(is.na(y_loq))) as.numeric(max(y_loq, na.rm = TRUE)) else NA_real_

  # print(inflection_point$inflect_x)
  # if (uloq_x < inflection_point$inflect_x) {
  #   uloq_x <- NA_real_
  #   uloq_y <- NA_real_
  # } else {
  #   uloq_y <- max(y_loq)
  # }
  return(list(
    lloq = lloq_x,
    uloq = uloq_x,
    lloq_y = lloq_y,
    uloq_y = uloq_y
  ))
}

generate_inflection_point <- function(model_name, fit, fixed_a_result, independent_variable,  verbose = TRUE) {
  params <- coef(fit)
  g <- as.numeric(if ("g" %in% names(params)) params["g"] else 1)# auto default
  a <-  as.numeric(ifelse(!is.null(fixed_a_result), fixed_a_result, params["a"]))
  b <- as.numeric(params["b"])
  c <- as.numeric(params["c"])
  d <- as.numeric(params["d"])

  # Calculate x-coordinate of inflection point analytically
  # The inflection point is where the second derivative equals zero
  inflect_x  <- tryCatch({
    if (model_name == "Y5") {
      # For 5 parameter model: x_inflect = c - b*ln(g)
      c - b * log(g)
    } else if (model_name == "Y4") {
      # For 4 parameter logistic: x_inflect = c
      c
    } else if (model_name == "Yd5") {
      # For 5 parameter decreasing logistic: x_inflect = c + ln(g)/b
      c + (log(g) / b)
    } else if (model_name == "Yd4") {
      # For 4 parameter log-logistic: x_inflect = c
      c
    } else if (model_name == "Ygomp4") {
      # For Gompertz: x_inflect = c (where second derivative = 0)
      c
    }
  }, error = function(e) NA)

  inflect_x <- as.numeric(inflect_x)

  # Calculate y-coordinate by evaluating the fitted model at inflect_x
  # This ensures the inflection point lies exactly on the fitted curve
  inflect_y <- tryCatch({
    # Use predict() to evaluate the fitted model at the inflection point
    # This is more robust than analytical formulas as it uses the actual fitted model
    newdata <- setNames(data.frame(x = inflect_x), independent_variable)
    predicted_y <- predict(fit, newdata = newdata)
    as.numeric(predicted_y)
  }, error = function(e) {
    # Fallback to analytical calculation if predict fails
    if (verbose) message("predict() failed, using analytical formula for inflect_y")
    tryCatch({
      switch(model_name,
             "Y5" = d + (a - d) / (1 + exp((inflect_x - c) / b))^g,
             "Y4" = d + (a - d) / (1 + exp((inflect_x - c) / b)),
             "Yd5" = a + (d - a) * (1 + g * exp(-b * (inflect_x - c)))^(-1/g),
             "Yd4" = a + (d - a) / (1 + exp(-b * (inflect_x - c))),
             "Ygomp4" = a + (d - a) * exp(-exp(-b * (inflect_x - c))),
             NA
      )
    }, error = function(e2) NA)
  })

  inflect_y <- as.numeric(inflect_y)

  return(list(inflect_x = inflect_x, inflect_y = inflect_y))
}

generate_lods <- function(best_fit, fixed_a_result, std_error_blank,  verbose = TRUE) {

  best_ci <- best_fit$best_ci
  best_data <- best_fit$best_data

  ulod <- best_ci[best_ci$parameter == "d",]$conf.low

  if (!is.null(fixed_a_result)) {
    if (is.null(std_error_blank) || is.na(std_error_blank)) {
      std_error_blank <- 0
    }
    critical_value <- qt(0.975, df = nrow(best_data) - length(best_ci$parameter))
    cat("critical value:\n")
    print(critical_value)
    cat("Blank SE:\n")
    print(std_error_blank)
    margin_of_error <- critical_value * std_error_blank
    llod <- fixed_a_result + margin_of_error
  } else {
    llod <- best_ci[best_ci$parameter == "a",]$conf.high
  }

  if (ulod < 0 || ulod < llod) {
    ulod  <- NA_real_
  }
  return(list(llod = llod, ulod = ulod))

}

generate_mdc_rdl <- function(best_fit, lods,
                             independent_variable, verbose = TRUE) {

  # Refactor 1: accept pre-computed lods instead of recomputing
  llod <- as.numeric(lods$llod)
  ulod <- as.numeric(lods$ulod)

  fit        <- best_fit$best_fit
  best_data  <- best_fit$best_data

  # x-range of the standards (search bounds for uniroot)
  x_data <- best_data[[independent_variable]]
  x_lo   <- min(x_data, na.rm = TRUE)
  x_hi   <- max(x_data, na.rm = TRUE)

  # Degrees of freedom for t-quantile (used by CI calculations)
  n_params <- length(coef(fit))
  n_obs    <- nrow(best_data)
  t_crit   <- qt(0.975, df = n_obs - n_params)

  # Variance-covariance matrix of fitted parameters
  V <- vcov(fit)

  # Refactor 2: hoist invariants out of pred_se inner loop
  theta <- coef(fit)
  p     <- length(theta)
  rhs   <- as.list(formula(fit))[[3]]

  # --- helper: predicted y at a single x ----------------------------------
  pred_y <- function(x_val) {
    nd <- setNames(data.frame(x_val), independent_variable)
    as.numeric(predict(fit, newdata = nd))
  }

  # --- helper: SE of predicted y via delta method -------------------------
  #     grad(theta) evaluated numerically; se = sqrt(grad' V grad)
  #     theta, p, rhs, V are captured from enclosing scope (hoisted)
  pred_se <- function(x_val, eps = 1e-6) {
    y0   <- pred_y(x_val)
    nd   <- setNames(data.frame(x_val), independent_variable)
    grad <- vapply(seq_len(p), function(j) {
      theta_j    <- theta
      theta_j[j] <- theta[j] + eps
      env <- c(as.list(theta_j), as.list(nd))
      (as.numeric(eval(rhs, envir = env)) - y0) / eps
    }, numeric(1))
    sqrt(as.numeric(crossprod(grad, V %*% grad)))
  }

  # --- helper: safe uniroot wrapper --------------------------------------
  safe_uniroot <- function(f, lower, upper) {
    tryCatch({
      f_lo <- f(lower)
      f_hi <- f(upper)
      if (is.na(f_lo) || is.na(f_hi)) return(NA_real_)
      if (sign(f_lo) == sign(f_hi)) return(NA_real_)
      uniroot(f, lower = lower, upper = upper, tol = .Machine$double.eps^0.5)$root
    }, error = function(e) NA_real_)
  }

  # --- 1. mindc: fitted curve == llod -------------------------------------
  mindc <- NA_real_
  if (!is.na(llod)) {
    mindc <- safe_uniroot(function(x_val) pred_y(x_val) - llod,
                          lower = x_lo, upper = x_hi)
  }

  # --- 2. maxdc: fitted curve == ulod -------------------------------------
  maxdc <- NA_real_
  if (!is.na(ulod)) {
    maxdc <- safe_uniroot(function(x_val) pred_y(x_val) - ulod,
                          lower = x_lo, upper = x_hi)
  }

  # --- 3. minrdl: 2.5% CI of fitted curve == llod ------------------------
  #        lower CI = pred_y(x) - t * se(x)
  minrdl <- NA_real_
  if (!is.na(llod)) {
    minrdl <- safe_uniroot(
      function(x_val) (pred_y(x_val) - t_crit * pred_se(x_val)) - llod,
      lower = x_lo, upper = x_hi
    )
  }

  # --- 4. maxrdl: 97.5% CI of fitted curve == ulod -----------------------
  #        upper CI = pred_y(x) + t * se(x)
  maxrdl <- NA_real_
  if (!is.na(ulod)) {
    maxrdl <- safe_uniroot(
      function(x_val) (pred_y(x_val) + t_crit * pred_se(x_val)) - ulod,
      lower = x_lo, upper = x_hi
    )
  }

  if (verbose) {
    message(sprintf("MDC/RDL — mindc: %s, maxdc: %s, minrdl: %s, maxrdl: %s",
                    format(mindc, digits = 4), format(maxdc, digits = 4),
                    format(minrdl, digits = 4), format(maxrdl, digits = 4)))
  }

  list(
    mindc  = as.numeric(mindc),
    maxdc  = as.numeric(maxdc),
    minrdl = as.numeric(minrdl),
    maxrdl = as.numeric(maxrdl)
  )
}

tidy.nlsLM <- function(best_fit, fixed_a_result, model_constraints, antigen_settings, antigen_fit_options,  verbose = TRUE) {

  if (antigen_settings$l_asy_constraint_method == "range_of_blanks" &&
      antigen_fit_options$is_log_response) {
    
    # Validate before log-transforming
    fixed_a_result_validated <- validate_fixed_lower_asymptote(
      fixed_a_result, verbose = verbose
    )
    
    if (!is.null(fixed_a_result_validated)) {
      .eps <- 0.000005
      fixed_a_result <- log10(fixed_a_result_validated + .eps)
    } else {
      fixed_a_result <- NULL  # treat as free if invalid
    }
    
    # Guard constraint bounds too — only log-transform if positive
    min_c <- antigen_settings$l_asy_min_constraint
    max_c <- antigen_settings$l_asy_max_constraint
    
    antigen_settings$l_asy_min_constraint <- if (
      is.numeric(min_c) && is.finite(min_c) && min_c > 0
    ) log10(min_c) else NA_real_
    
    antigen_settings$l_asy_max_constraint <- if (
      is.numeric(max_c) && is.finite(max_c) && max_c > 0
    ) log10(max_c) else NA_real_
  }
  
  m_constraints <- model_constraints[[best_fit$best_model_name]]
  m_constraints_df <- as.data.frame(m_constraints)
  m_constraints_df$term <- rownames(m_constraints_df)
  rownames(m_constraints_df) <- NULL
  m_constraints_df <- m_constraints_df[, c("term", "lower", "upper")]
  if (!is.null(fixed_a_result)) {
    a_fixed_constraint <- tibble::tibble(
      term = "a",
      lower = antigen_settings$l_asy_min_constraint,
      upper = antigen_settings$l_asy_max_constraint,
    )
    m_constraints_df <-  rbind(a_fixed_constraint, m_constraints_df)
  }

  s <- summary(best_fit$best_fit)
  out <- as.data.frame(s$coefficients)
  tidy_df <- tibble::tibble(
    term = rownames(out),
    estimate = out[, "Estimate"],
    std.error = out[, "Std. Error"],
    statistic = out[, "t value"],
    p.value = out[, "Pr(>|t|)"]
  )

  tidy_df$study_accession <- unique(best_fit$best_data$study_accession)
  tidy_df$experiment_accession <- unique(best_fit$best_data$experiment_accession)
  tidy_df$nominal_sample_dilution <- unique(best_fit$best_data$nominal_sample_dilution)
  tidy_df$antigen <- unique(best_fit$best_data$antigen)
  tidy_df$plateid <- unique(best_fit$best_data$plateid)
  tidy_df$plate <- unique(best_fit$best_data$plate)
  tidy_df$source <- unique(best_fit$best_data$source)

  if (!is.null(fixed_a_result)) {
    a_fixed <- tibble::tibble(
      term = "a",
      estimate = fixed_a_result,
      std.error = 0,
      statistic = NA_real_,
      p.value = NA_real_,
      study_accession = unique(best_fit$best_data$study_accession),
      experiment_accession = unique(best_fit$best_data$experiment_accession),
      nominal_sample_dilution = unique(best_fit$best_data$nominal_sample_dilution),
      antigen = unique(best_fit$best_data$antigen),
      plateid = unique(best_fit$best_data$plateid),
      plate = unique(best_fit$best_data$plate),
      source = unique(best_fit$best_data$source)
    )
    tidy_df <- rbind(a_fixed, tidy_df)
  }

  # rename standard error column And p-value column
  names(tidy_df)[names(tidy_df) == "std.error"] <- "std_error"
  names(tidy_df)[names(tidy_df) == "p.value"] <- "p_value"


  tidy_df <- merge(tidy_df, m_constraints_df, by = "term", all.x = TRUE)
  other_cols <- setdiff(colnames(tidy_df), c("term", c("lower", "upper")))

  # New order: term → lower, upper → rest
  tidy_df <- tidy_df[, c("term", "lower", "upper", other_cols)]

  tidy_df <- attach_grouping_keys(tidy_df, best_fit$best_data, context = "tidy.nlsLM")

  best_fit$best_tidy <- tidy_df
  if (verbose) {
    message("Finished tidy.nlsLM")
  }
  return(best_fit)
}

calculate_predicted_concentration <- function(model_name,fit,
                                              plate_samples,
                                              fixed_constraint,
                                              response_variable,
                                              is_log_response,
                                              verbose = TRUE) {
  if (is_log_response) {
    raw_vals <- plate_samples[[response_variable]]
    # Guard: if values are already negative, they're likely already log-transformed
    if (any(raw_vals < 0, na.rm = TRUE)) {
      if (verbose) message(
        "[calculate_predicted_concentration] WARNING: response contains negative values ",
        "but is_log_response=TRUE. Data may already be log-transformed. ",
        "Skipping log10 transform."
      )
    } else {
      plate_samples[[response_variable]] <- log10(plate_samples[[response_variable]])
    }
  }

  params <- coef(fit)
  g <- if ("g" %in% names(params)) params["g"] else 1  # auto default
  b <- params["b"]
  c <- params["c"]
  d <- params["d"]

  if (!is.null(fixed_constraint)){
    message("Lower asymptote is fixed at", fixed_constraint)
    fixed_value <- fixed_constraint
    a  <-  fixed_value
    plate_samples$predicted_concentration  <- tryCatch({
      if (model_name == "Y5") {
        inv_Y5_fixed(y = plate_samples[[response_variable]] , fixed_a = a, b = b, c = c, d = d , g = g)
      } else if (model_name == "Yd5") {
        inv_Yd5_fixed(y = plate_samples[[response_variable]], fixed_a = a, b = b, c = c, d = d, g = g)
      } else if (model_name == "Y4") {
        inv_Y4_fixed(y = plate_samples[[response_variable]], fixed_a = a, b = b, c = c, d = d)
      } else if (model_name == "Yd4") {
        inv_Yd4_fixed(y = plate_samples[[response_variable]], fixed_a = a, b = b, c = c, d = d)
      } else if (model_name == "Ygomp4") {
        inv_Ygomp4_fixed(plate_samples[[response_variable]], fixed_a = a, b = b, c = c, d = d)
      }
    }, error = function(e)  {
      message("Error: ", e$message)
      rep(NA_real_, nrow(plate_samples))
    }
    )

  } else {
    a <- params["a"]
    plate_samples$predicted_concentration  <- tryCatch({
      if (model_name == "Y5") {
        inv_Y5(y = plate_samples[[response_variable]] , a = a, b = b, c = c, d = d , g = g)
      } else if (model_name == "Yd5") {
        inv_Yd5(y = plate_samples[[response_variable]], a = a, b = b, c = c, d = d, g = g)
      } else if (model_name == "Y4") {
        inv_Y4(y = plate_samples[[response_variable]], a = a, b = b, c = c, d = d)
      } else if (model_name == "Yd4") {
        inv_Yd4(y = plate_samples[[response_variable]], a = a, b = b, c = c, d = d)
      } else if (model_name == "Ygomp4") {
        message("Ygomp4 predicted")
        inv_Ygomp4(plate_samples[[response_variable]], a = a, b = b, c = c, d = d)
      }
    }, error = function(e)  {
      message("Error: ", e$message)
      rep(NA_real_, nrow(plate_samples))
    }
    )

  }

  return(plate_samples)
}

# For a plate and and antigen in a study prepare data for compute_robust_curves
preprocess_robust_curves <- function(data, antigen_settings, response_variable,
                                     independent_variable,
                                     is_log_response,
                                     blank_data = NULL,
                                     blank_option = "ignored",
                                     is_log_independent = TRUE,
                                     apply_prozone = TRUE,
                                     verbose = TRUE) {

  ## compute standard curve concentration for undiluted standard curve sample.
  undiluted_sc_concentration <- get_study_exp_antigen_plate_param(antigen_settings)

  data <- compute_concentration(data = data,
                                undiluted_sc_concentration = undiluted_sc_concentration,
                                independent_variable = independent_variable,
                                is_log_concentration = TRUE)

  if (apply_prozone) {
    if (verbose) {
      message("applying prozone correction")
    }
    data <- correct_prozone(stdframe = data,
                            prop_diff = 0.1,
                            dil_scale = 2,
                            response_variable = response_variable,
                            independent_variable = independent_variable,
                            verbose = verbose
    )

  }

  ## Blank Operations (in linear space)
  data <- perform_blank_operation(
    blank_data = blank_data,
    data = data,
    response_variable = response_variable,
    independent_variable = independent_variable,
    is_log_response = is_log_response,
    blank_option = blank_option,
    verbose = verbose
  )

  if (is_log_response) {
    raw_vals <- data[[response_variable]]
    
    # Adaptive floor based on non-zero minimum rather than hardcoded 1
    positive_vals <- raw_vals[is.finite(raw_vals) & raw_vals > 0]
    
    if (length(positive_vals) > 0) {
      # Floor at 1% of the smallest positive observed value
      adaptive_floor <- min(positive_vals) * 0.01
    } else {
      adaptive_floor <- 1e-6
    }
    
    n_floored <- sum(raw_vals <= 0, na.rm = TRUE)
    if (n_floored > 0 && verbose) {
      message(sprintf(
        "[preprocess] %d/%d values <= 0 floored to %.2e before log10",
        n_floored, length(raw_vals), adaptive_floor
      ))
    }
    
    data[[response_variable]][is.na(raw_vals) | raw_vals <= 0] <- adaptive_floor
    data[[response_variable]] <- log10(data[[response_variable]])
  }
  
  antigen_fit_options <- list(
    is_log_response    = is_log_response,
    blank_option       = blank_option,
    is_log_concentration = is_log_independent,
    apply_prozone      = apply_prozone
  )
  
  return(list(data = data, antigen_fit_options = antigen_fit_options))
}

propagate_error_analytic <- function(model,         # character: "Y4","Yd4","Ygomp4","Y5","Yd5"
                                     fit,           # nlsLM object (already fitted)
                                     y,             # observed response
                                     se_y = 0,     # standard error of y (0 if unknown)
                                     fixed_a,  # the constrained lower asymptote
                                     verbose = TRUE
) {
  # ----- 1. Extract coefficients & covariance -------------------------------
  theta    <- coef(fit)          # named vector
  vcov_mat <- vcov(fit)          # covariance matrix

  if(!is.null(fixed_a)) {


    # ----- 2. Analytic gradient w.r.t. parameters & y -----------------------
    inv_and_grad <- make_inv_and_grad_fixed(model, y, fixed_a)
    # ----- 3. Evaluate inverse (point estimate) ------------------------------
    x_hat <- inv_and_grad$inv(theta)
    grad_theta <- inv_and_grad$grad(theta)   # named numeric vector
    grad_y <- inv_and_grad$grad_y(theta)    # scalar
  } else {
    # ----- 2. Evaluate inverse (point estimate) ------------------------------
    x_hat <- switch(model,
                    Y4      = inv_Y4(y, a = theta["a"], b = theta["b"], c = theta["c"], d = theta["d"]),
                    Yd4     = inv_Yd4(y, a = theta["a"], b = theta["b"], c = theta["c"], d = theta["d"]),
                    Ygomp4  = inv_Ygomp4(y, a = theta["a"], b = theta["b"], c = theta["c"], d = theta["d"]),
                    Y5      = inv_Y5(y, a = theta["a"], b = theta["b"], c = theta["c"], d = theta["d"], g = theta["g"]),
                    Yd5     = inv_Yd5(y, a = theta["a"], b = theta["b"], c = theta["c"], d = theta["d"], g = theta["g"]),
                    stop("Unsupported model name"))

    # ----- 3. Analytic gradient w.r.t. parameters & y -----------------------
    grads <- switch(model,
                    Y4     = grad_Y4(y, a = theta["a"], b = theta["b"], c = theta["c"], d = theta["d"]),
                    Yd4    = grad_Yd4(y, a = theta["a"], b = theta["b"], c = theta["c"], d = theta["d"]),
                    Ygomp4 = grad_Ygomp4(y, a = theta["a"], b = theta["b"], c = theta["c"], d = theta["d"]),
                    Y5     = grad_Y5(y, a = theta["a"], b = theta["b"], c = theta["c"], d = theta["d"], g = theta["g"]),
                    Yd5    = grad_Yd5(y, a = theta["a"], b = theta["b"], c = theta["c"], d = theta["d"], g = theta["g"]))

    grad_theta <- grads$grad_theta   # named vector (same order as theta)
    grad_y     <- grads$grad_y
  }


  # ----- 4. Delta‑method variance -----------------------------------------
  var_par <- as.numeric(t(grad_theta) %*% vcov_mat %*% grad_theta)
  var_y   <- (grad_y^2) * (se_y^2)
  var_x   <- var_par + var_y
  se_x    <- sqrt(var_x)

  # ----- 5. Return ---------------------------------------------------------
  list(x_est      = x_hat,
       se_x       = se_x,
       var_x      = var_x,
       grad_theta = grad_theta,
       grad_y     = grad_y)
}

# ── Run this BEFORE calling propagate_error_dataframe ──────────────────────
diagnose_propagation_inputs <- function(fit, model, fixed_a, y_test = NULL) {

  params <- coef(fit)
  Sigma  <- vcov(fit)

  cat("\n═══ Propagation Input Diagnosis ═══\n")
  cat("Model         :", model, "\n")
  cat("coef(fit)     :", paste(names(params), "=", round(params, 5), collapse = ", "), "\n")
  cat("vcov dim      :", paste(dim(Sigma), collapse = " x "), "\n")
  cat("vcov rownames :", paste(rownames(Sigma), collapse = ", "), "\n")
  cat("fixed_a       :", if (is.null(fixed_a)) "NULL" else round(fixed_a, 6), "\n")

  # If fixed_a is supplied, 'a' should NOT be in coef(fit)
  if (!is.null(fixed_a) && "a" %in% names(params)) {
    cat("⚠️  WARNING: fixed_a is supplied BUT 'a' is ALSO in coef(fit).\n")
    cat("   This causes the augmented-Sigma path to corrupt the gradient alignment.\n")
    cat("   Solution: fit the model WITHOUT 'a' as a free parameter when fixed_a is used.\n")
  }

  # Test inv and grad at a sample y value
  if (!is.null(y_test)) {
    cat("\nTesting inv/grad at y =", y_test, "\n")
    fns <- tryCatch(
      make_inv_and_grad_fixed(model = model, y = y_test,
                              fixed_a = if (!is.null(fixed_a)) fixed_a else params["a"]),
      error = function(e) { cat("make_inv_and_grad_fixed ERROR:", e$message, "\n"); NULL }
    )
    if (!is.null(fns)) {
      x_est <- tryCatch(fns$inv(params),    error = function(e) { cat("inv() ERROR:", e$message,"\n"); NA })
      g_t   <- tryCatch(fns$grad(params),   error = function(e) { cat("grad() ERROR:", e$message,"\n"); NULL })
      g_y   <- tryCatch(fns$grad_y(params), error = function(e) { cat("grad_y() ERROR:", e$message,"\n"); NA })

      cat("  x_est    :", x_est, "\n")
      cat("  grad_t   :", if (!is.null(g_t)) paste(names(g_t), "=", round(g_t,5), collapse=", ") else "NULL", "\n")
      cat("  grad_y   :", g_y, "\n")

      # Check alignment
      if (!is.null(g_t)) {
        common <- intersect(names(g_t), rownames(Sigma))
        cat("  grad_t names  :", paste(names(g_t),   collapse=", "), "\n")
        cat("  Sigma rownames:", paste(rownames(Sigma), collapse=", "), "\n")
        cat("  Common names  :", paste(common, collapse=", "), "\n")
        if (length(common) == length(g_t) && length(common) == nrow(Sigma)) {
          g_vec  <- g_t[common]
          S_sub  <- Sigma[common, common, drop=FALSE]
          var_x  <- as.numeric(t(g_vec) %*% S_sub %*% g_vec)
          cat("  var_x (param contribution) :", var_x, "\n")
          cat("  se_x  (param contribution) :", sqrt(max(var_x, 0)), "\n")
        } else {
          cat("  ⚠️  NAME MISMATCH — this is the root cause of NA se_x!\n")
        }
      }
    }
  }
  cat("═══════════════════════════════════\n\n")
}

#  Propagation for a whole data‑frame
#' Propagate the error of a fitted sigmoid model to many new samples
#'
#' @param pred_df   data‑frame that contains the new measurements.
#' @param fit       an **nlsLM** object that was used to fit the
#'                  chosen sigmoid model (already has coef() and vcov()).
#' @param model     character, one of: "Y4","Yd4","Ygomp4","Y5","Yd5".
#' @param y_col     name (character) of the column that stores the observed response.
#' @param se_col    name (character) of the column that stores the standard error of the response.
#' @param quiet     logical. If TRUE suppresses the progress bar.
#'
#' @return the input data‑frame with two extra columns:
#'         * `x_est` – inverse‑predicted concentration,
#'         * `se_x`  – propagated standard error of that concentration.
#' @examples
#' ##  simulate a small example (3 rows) -----------------
#' library(minpack.lm)
#' ## (fit a 4‑parameter logistic first)
#' set.seed(123)
#' x_cal  <- seq(0,10,length.out=25)
#' a<-0.2; b<-0.9; c<-4.5; d<-9.8
#' y_cal  <- a + (d-a)/(1+exp((x_cal-c)/b)) + rnorm(length(x_cal),sd=0.08)
#' fit4   <- nlsLM(y ~ a + (d-a)/(1+exp((x-c)/b)),
#'                data=data.frame(x=x_cal,y=y_cal),
#'                start=list(a=0,b=1,c=5,d=10))
#'
#' pred_df <- data.frame(
#'   response_var = c(6.7,7.2,5.9),
#'   se_std_response = c(0.09,0.12,0.07)
#' )
#' out <- propagate_error_dataframe(pred_df, fit4,
#'                                  model = "Y4",
#'                                  y_col = "response_var",
#'                                  se_col = "se_std_response")
#' print(out)
#'
propagate_error_dataframe <- function(pred_df,
                                      fit,
                                      model = c("Y4","Yd4","Ygomp4","Y5","Yd5"),
                                      y_col,
                                      se_col,
                                      fixed_a,
                                      cv_x_max = 125,
                                      is_log_x  = TRUE,   # is x_est on log10 scale?
                                      quiet = FALSE) {
  model <- match.arg(model)

  # Validate is_log_x
  is_log_x <- isTRUE(is_log_x)

  if (!quiet) {
    message("[propagate] CV formula   : ",
            if (is_log_x) "LINEAR-scale (se_x * ln(10) * 100) — avoids /0 at log10(conc)=0"
            else           "LOG-scale    (se_x / |x_est| * 100)")
  }

  # ── 1. Validate cv_x_max ────────────────────────────────────
  cv_x_max <- if (isTRUE(is.finite(cv_x_max)) && cv_x_max > 0) {
    as.numeric(cv_x_max)[1]
  } else {
    message("[propagate] cv_x_max invalid; defaulting to 125.")
    125
  }

  # ── 2. Extract params and Sigma from fit ─────────────────────
  params <- coef(fit)
  Sigma  <- vcov(fit)

  if (!quiet) {
    message("[propagate] Model       : ", model)
    message("[propagate] Free params : ", paste(names(params), collapse = ", "))
    message("[propagate] Sigma rows  : ", paste(rownames(Sigma), collapse = ", "))
    message("[propagate] fixed_a     : ",
            if (is.null(fixed_a)) "NULL (a is free in coef)" else round(as.numeric(fixed_a), 6))
  }

  # ── 3. Decide which branch make_inv_and_grad_fixed will use ──
  #
  #  fixed_a supplied as a real scalar → truly fixed, pass as-is (as.numeric)
  #  fixed_a is NULL                   → 'a' is free, pass NULL
  #
  #  We do NOT read 'a' from params and pass it as fixed_a here.
  #  When fixed_a = NULL, make_inv_and_grad_fixed Branch B reads
  #  p["a"] internally from coef(fit) and uses the full grad_* functions.

  use_fixed_a <- !is.null(fixed_a) && isTRUE(is.finite(as.numeric(fixed_a)))
  fna         <- if (use_fixed_a) as.numeric(fixed_a) else NULL

  # Sanity: when a is free it must be in coef(fit)
  if (!use_fixed_a && !"a" %in% names(params)) {
    stop("[propagate] fixed_a is NULL but 'a' not found in coef(fit).")
  }
  # Sanity: when a is fixed it must NOT be in coef(fit)
  if (use_fixed_a && "a" %in% names(params)) {
    warning("[propagate] fixed_a supplied but 'a' also in coef(fit). ",
            "The coef 'a' will be ignored; fixed_a will be used.")
    params <- params[names(params) != "a"]
    Sigma  <- Sigma[rownames(Sigma) != "a", colnames(Sigma) != "a", drop = FALSE]
  }

  # ── 4. Loop ──────────────────────────────────────────────────
  n   <- nrow(pred_df)
  res <- vector("list", n)
  pb  <- if (!quiet) txtProgressBar(min = 0, max = n, style = 3) else NULL

  n_na_inv  <- 0L
  n_na_grad <- 0L
  n_na_vpar <- 0L
  n_capped  <- 0L
  n_ok      <- 0L

  for (i in seq_len(n)) {

    y_i    <- pred_df[[y_col]][i]
    se_y_i <- if (isTRUE(is.finite(pred_df[[se_col]][i]))) pred_df[[se_col]][i] else 0

    # Scale sanity check (log10 response only — warn once)
    if (i == 1 && !quiet) {
      y_range <- diff(range(pred_df[[y_col]], na.rm = TRUE))
      se_median <- median(pred_df[[se_col]], na.rm = TRUE)
      if (isTRUE(is.finite(se_median)) && isTRUE(is.finite(y_range)) && y_range > 0) {
        se_to_range_ratio <- se_median / y_range
        if (se_to_range_ratio > 0.5) {
          message(sprintf(paste0(
            "\n[propagate] WARNING: median se_col (%.4f) is %.1f%% of the y_col range (%.4f).\n",
            "  This suggests se_col may be on a different scale than y_col.\n",
            "  If y_col is log10-transformed, se_col must also be in log10 units.\n",
            "  Convert: se_log10 = se_raw / (raw_value * log(10))"),
            se_median, se_to_range_ratio * 100, y_range
          ))
        }
      }
    }

    # Guard: skip non-finite y
    if (!isTRUE(is.finite(y_i))) {
      res[[i]] <- list(x_est = NA_real_, se_x = NA_real_, cv_x = cv_x_max)
      n_na_inv <- n_na_inv + 1L
      if (!quiet) setTxtProgressBar(pb, i)
      next
    }

    # Build closures — pass fna (NULL or scalar)
    fns <- tryCatch(
      make_inv_and_grad_fixed(model = model, y = y_i, fixed_a = fna),
      error = function(e) {
        if (!quiet) message(sprintf("[propagate] closure failed row %d: %s", i, e$message))
        NULL
      }
    )
    if (is.null(fns)) {
      res[[i]] <- list(x_est = NA_real_, se_x = NA_real_, cv_x = cv_x_max)
      n_na_inv <- n_na_inv + 1L
      if (!quiet) setTxtProgressBar(pb, i)
      next
    }

    # Inverse prediction
    x_est <- tryCatch(fns$inv(params), error = function(e) NA_real_)
    if (!isTRUE(is.finite(x_est))) {
      res[[i]] <- list(x_est = x_est, se_x = NA_real_, cv_x = cv_x_max)
      n_na_inv <- n_na_inv + 1L
      if (!quiet) setTxtProgressBar(pb, i)
      next
    }

    # Gradient w.r.t. free parameters ∂x/∂θ
    grad_t <- tryCatch(
      fns$grad(params),
      error = function(e) { n_na_grad <<- n_na_grad + 1L; rep(NA_real_, length(params)) }
    )

    # Gradient w.r.t. response ∂x/∂y
    grad_y_val <- tryCatch(fns$grad_y(params), error = function(e) NA_real_)

    # ── Delta-method variance ─────────────────────────────────
    # Align grad_t names with Sigma — they MUST match now that Branch B
    # returns a,b,c,d and Branch A returns b,c,d.
    var_par <- NA_real_

    if (all(is.finite(grad_t))) {
      common <- intersect(names(grad_t), rownames(Sigma))

      if (length(common) > 0) {
        if (length(common) < length(grad_t) && !quiet && i == 1) {
          message("[propagate] NOTE: grad_t has ", length(grad_t),
                  " names but only ", length(common),
                  " align with Sigma. Using: ", paste(common, collapse = ", "))
        }
        g_sub  <- grad_t[common]
        S_sub  <- Sigma[common, common, drop = FALSE]
        var_par <- tryCatch(
          as.numeric(t(g_sub) %*% S_sub %*% g_sub),
          error = function(e) NA_real_
        )
      } else {
        # Still a mismatch — emit a clear one-time diagnostic
        if (!quiet && i == 1) {
          message("[propagate] WARNING: zero common names between grad_t and Sigma!")
          message("  grad_t names : ", paste(names(grad_t), collapse = ", "))
          message("  Sigma rows   : ", paste(rownames(Sigma), collapse = ", "))
          message("  This means 'a' is neither in Sigma (fixed) nor returned by grad_t (free).")
          message("  Check that fixed_a is correctly NULL or a scalar.")
        }
      }
    } else {
      n_na_grad <- n_na_grad + 1L
    }

    if (!isTRUE(is.finite(var_par))) n_na_vpar <- n_na_vpar + 1L

    # Measurement-error contribution  (∂x/∂y)^2 * se_y^2
    var_y <- if (isTRUE(is.finite(grad_y_val)) && se_y_i > 0)
      (grad_y_val^2) * (se_y_i^2) else 0

    var_x <- if (isTRUE(is.finite(var_par))) var_par + var_y else NA_real_
    se_x  <- if (isTRUE(is.finite(var_x)) && var_x >= 0) sqrt(var_x) else NA_real_

    # CV_x
    # ── CV_x computation ─────────────────────────────────────────────────────
    #
    # STRATEGY: when x_est is on the log10 concentration scale, the standard
    # ratio cv = (se_x / |x_est|) * 100 diverges as x_est -> 0 (i.e. conc -> 1).
    # This is a mathematical artefact of the log scale passing through zero —
    # NOT a real increase in uncertainty.
    #
    # Correct approach: propagate se_x to the LINEAR concentration scale first,
    # then compute the CV there.
    #
    #   x_linear     = 10^x_est
    #   se_x_linear  = se_x * 10^x_est * log(10)      [delta method]
    #   cv_x_linear  = (se_x_linear / x_linear) * 100
    #                = se_x * log(10) * 100
    #                = se_x * 230.259...
    #
    # This is independent of x_est, so it never diverges at x_est = 0.
    # The is_log_x flag controls which formula is used.
    #
    cv_x <- if (isTRUE(is.finite(se_x))) {

      if (is_log_x) {
        # Log10-scale x_est: use linear-scale CV (avoids /0 at x_est=0)
        raw_cv <- se_x * log(10) * 100   # = se_x * 230.26
      } else {
        # Linear-scale x_est: use standard ratio CV
        if (isTRUE(abs(x_est) > 1e-10)) {
          raw_cv <- (se_x / abs(x_est)) * 100
        } else {
          raw_cv <- Inf
        }
      }

      if (isTRUE(is.finite(raw_cv))) {
        if (raw_cv < cv_x_max) {
          n_ok <- n_ok + 1L
        } else {
          n_capped <- n_capped + 1L
        }
        min(raw_cv, cv_x_max)
      } else {
        n_capped <- n_capped + 1L
        cv_x_max
      }

    } else {
      n_capped <- n_capped + 1L
      cv_x_max
    }

    res[[i]] <- list(x_est = x_est, se_x = se_x, cv_x = cv_x)
    if (!quiet) setTxtProgressBar(pb, i)
  }

  if (!quiet) close(pb)

  # ── 5. Unpack ─────────────────────────────────────────────────
  pred_df$predicted_concentration <- sapply(res, `[[`, "x_est")
  pred_df$se_x                    <- sapply(res, `[[`, "se_x")
  pred_df$cv_x                    <- sapply(res, `[[`, "cv_x")
  pred_df$cv_x[!is.finite(pred_df$cv_x)] <- cv_x_max

  # ── 6. Summary ────────────────────────────────────────────────
  if (!quiet) {
    message("\n── propagate_error_dataframe summary ──────────────────")
    message(sprintf("  Rows processed     : %d", n))
    message(sprintf("  x_est finite       : %d", sum(is.finite(pred_df$predicted_concentration))))
    message(sprintf("  x_est NA           : %d", sum(!is.finite(pred_df$predicted_concentration))))
    message(sprintf("  se_x finite        : %d", sum(is.finite(pred_df$se_x))))
    message(sprintf("  se_x NA            : %d  (grad or var_par issue)", sum(!is.finite(pred_df$se_x))))
    message(sprintf("  cv_x < cap         : %d", n_ok))
    message(sprintf("  cv_x at cap (%3.0f) : %d", cv_x_max, n_capped))
    message(sprintf("  grad_t NA rows     : %d", n_na_grad))
    message(sprintf("  var_par NA rows    : %d", n_na_vpar))

    xv <- pred_df$predicted_concentration[is.finite(pred_df$predicted_concentration)]
    sv <- pred_df$se_x[is.finite(pred_df$se_x)]
    cv <- pred_df$cv_x[is.finite(pred_df$cv_x) & pred_df$cv_x < cv_x_max]

    if (length(xv)) message(sprintf("  x_est range        : [%.4f, %.4f]", min(xv), max(xv)))
    if (length(sv)) message(sprintf("  se_x  range        : [%.4f, %.4f]", min(sv), max(sv)))
    if (length(cv)) message(sprintf("  cv_x  range (excl cap): [%.2f, %.2f]", min(cv), max(cv)))
    message("───────────────────────────────────────────────────────")
  }

  # pred_df_v <<- pred_df
  pred_df
}



diagnose_cv_x <- function(df, label = "pred_se",
                          lloq = NULL, uloq = NULL,
                          cv_x_max = 125,     # cap parameter
                          verbose = TRUE) {
  if (!verbose) return(invisible(NULL))
  if (!"cv_x" %in% names(df)) {
    message(sprintf("[cv_x diagnostic] '%s': cv_x column not found.", label))
    return(invisible(NULL))
  }

  cv <- df$cv_x
  xc <- df$predicted_concentration

  finite_mask <- is.finite(cv) & is.finite(xc)
  cv_f  <- cv[finite_mask]
  xc_f  <- xc[finite_mask]

  if (length(cv_f) == 0) {
    message(sprintf("[cv_x diagnostic] '%s': no finite cv_x values.", label))
    return(invisible(NULL))
  }

  min_idx  <- which.min(cv_f)
  min_cv   <- cv_f[min_idx]
  min_x    <- xc_f[min_idx]
  max_cv   <- max(cv_f, na.rm = TRUE)
  mean_cv  <- mean(cv_f, na.rm = TRUE)

  # Count rows that hit the cap exactly (were clamped) vs genuinely > 20
  n_at_cap <- sum(cv_f >= cv_x_max, na.rm = TRUE)
  n_gt_20  <- sum(cv_f >  20,       na.rm = TRUE)
  n_na_raw <- sum(!is.finite(df$cv_x))   # residual NAs before cap applied

  message(sprintf(
    "\n[cv_x diagnostic] --- %s ---
  cv_x_max (cap)   : %.1f
  N total          : %d
  N finite cv_x    : %d
  N non-finite raw : %d  (replaced with cap)
  Min  cv_x        : %.3f  at predicted_concentration = %.4f
  Max  cv_x        : %.3f
  Mean cv_x        : %.3f
  N cv_x > 20      : %d
  N cv_x at cap    : %d",
    label, cv_x_max,
    nrow(df), length(cv_f), n_na_raw,
    min_cv, min_x, max_cv, mean_cv,
    n_gt_20, n_at_cap
  ))

  if (!is.null(lloq) && !is.null(uloq) && isTRUE(is.finite(lloq)) && isTRUE(is.finite(uloq))) {
    in_loq    <- finite_mask &
      df$predicted_concentration >= lloq &
      df$predicted_concentration <= uloq
    cv_in_loq <- df$cv_x[in_loq]
    n_loq_cap <- sum(cv_in_loq >= cv_x_max, na.rm = TRUE)

    message(sprintf(
      "  Within [lloq=%.4f, uloq=%.4f]:
    N            = %d
    mean cv_x    = %.3f
    max  cv_x    = %.3f
    N at cap     = %d  %s",
      lloq, uloq,
      length(cv_in_loq),
      mean(cv_in_loq, na.rm = TRUE),
      max(cv_in_loq,  na.rm = TRUE),
      n_loq_cap,
      if (n_loq_cap > 0)
        "[WARNING] capped values inside LOQ window — check curve fit near limits"
      else ""
    ))
  }

  if (n_at_cap > 0) {
    message(sprintf(
      "  [INFO] %d point(s) capped at cv_x_max=%.1f (asymptote proximity or failed propagation).",
      n_at_cap, cv_x_max
    ))
  }

  invisible(list(
    min_cv   = min_cv,
    min_x    = min_x,
    max_cv   = max_cv,
    mean_cv  = mean_cv,
    n_gt_20  = n_gt_20,
    n_at_cap = n_at_cap
  ))
}

#' Estimate Assay Response Standard Error from Standard Curve Replicates
#'
#' Computes an estimate of the standard error in assay response (y) by
#' aggregating within-dilution variability from standard samples collected
#' across multiple plates. Within each dilution level, standard samples
#' from different plates are treated as replicates for estimating measurement error.
#'
#' @param data        data.frame or tibble containing standard curve data
#' @param dilution_col name (character) of the column identifying dilution level
#'                     (default = "dilution")
#' @param response_col name (character) of the response column (e.g., "mfi")
#' @param plate_col   name (character) of the column identifying plate
#'                     (default = "plate"). Used to pool across plates when
#'                     single replicates per plate.
#' @param method      character, one of:
#'                     - "pooled_within" (default): pools within-dilution variance
#'                       weighted by degrees of freedom (recommended)
#'                     - "median_se": median of per-dilution SEs (robust)
#'                     - "mean_se": mean of per-dilution SEs
#' @param min_reps    minimum replicates per dilution to include in estimate
#'                     (default = 2)
#' @param na.rm       logical – drop NA values (default = TRUE)
#'
#' @return A list with:
#'   \item{overall_se}{single numeric: pooled SE of response measurement}
#'   \item{by_dilution}{tibble with per-dilution statistics}
#'   \item{pooling_method}{character: method used for pooling}
#'   \item{pooling_strategy}{character: "within_plate" or "across_plates"}
#'   \item{total_df}{total degrees of freedom in the pooled estimate}
#'   \item{n_dilutions_used}{number of dilution levels contributing to estimate}
#'   \item{n_plates}{number of plates in the data}
#'
#' @details
#' The function automatically detects whether replicates exist within plates
#' or whether pooling across plates is needed:
#'
#' **Within-plate replicates**: When multiple measurements exist at the same
#' dilution on the same plate, variance is computed within each plate-dilution
#' combination and then pooled.
#'
#' **Across-plate pooling**: When only single measurements exist per dilution
#' per plate (common in many assay designs), the function pools measurements
#' from different plates at each dilution level to estimate variability.
#'
#' The pooled within-dilution method computes:
#' \deqn{s_{pooled}^2 = \frac{\sum_{i} (n_i - 1) s_i^2}{\sum_{i} (n_i - 1)}}
#' where \eqn{s_i^2} is the variance at dilution level \eqn{i} with \eqn{n_i}
#' replicates (either within-plate or across-plates).
#'
#' @examples
#' # Example with standard curve data (single replicate per plate per dilution)
#' se_result <- assay_se(
#'   data = standards_df,
#'   dilution_col = "dilution",
#'   response_col = "mfi",
#'   plate_col = "plate",
#'   method = "pooled_within"
#' )
#' print(se_result$overall_se)
#' print(se_result$pooling_strategy)
#'
#' @export
# assay_se <- function(data,
#                      dilution_col = "dilution",
#                      response_col = "mfi",
#                      plate_col    = "plate",
#                      method       = c("pooled_within", "median_se", "mean_se"),
#                      min_reps     = 2,
#                      na.rm        = TRUE) {
# 
#   ## 0. Input validation
#   method <- match.arg(method)
#   data <- dplyr::as_tibble(data)
# 
#   required_cols <- c(dilution_col, response_col)
#   missing_cols <- setdiff(required_cols, colnames(data))
#   if (length(missing_cols) > 0) {
#     stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
#   }
# 
#   # Check if plate column exists
# 
#   has_plate_col <- !is.null(plate_col) && plate_col %in% colnames(data)
# 
#   ## 1. Remove NA responses
#   if (na.rm) {
#     data <- data[!is.na(data[[response_col]]), ]
#   }
# 
#   if (nrow(data) == 0) {
#     warning("No valid response data after removing NAs")
#     return(list(
#       overall_se       = NA_real_,
#       by_dilution      = NULL,
#       pooling_method   = method,
#       pooling_strategy = NA_character_,
#       total_df         = 0,
#       n_dilutions_used = 0,
#       n_plates         = 0
#     ))
#   }
# 
#   ## 2. Determine pooling strategy
#   #    Check if we have within-plate replicates or need to pool across plates
# 
#   if (has_plate_col) {
#     # Count replicates per plate-dilution combination
#     rep_counts <- data %>%
#       dplyr::group_by(
#         !!rlang::sym(plate_col),
#         !!rlang::sym(dilution_col)
#       ) %>%
#       dplyr::summarise(n = dplyr::n(), .groups = "drop")
# 
#     # Determine if we have within-plate replicates
#     # If median reps per plate-dilution is > 1, use within-plate strategy
#     median_reps_per_plate_dilution <- stats::median(rep_counts$n)
#     has_within_plate_reps <- median_reps_per_plate_dilution >= min_reps
# 
#     n_plates <- length(unique(data[[plate_col]]))
#   } else {
#     has_within_plate_reps <- TRUE  # No plate info, treat all as one group
#     n_plates <- 1
#   }
# 
#   ## 3. Compute within-dilution statistics based on strategy
# 
#   if (has_within_plate_reps || !has_plate_col) {
#     # STRATEGY A: Within-plate replicates exist
#     # Pool variance within each dilution level (original approach)
#     pooling_strategy <- "within_plate"
# 
#     by_dilution <- data %>%
#       dplyr::group_by(!!rlang::sym(dilution_col)) %>%
#       dplyr::summarise(
#         n        = dplyr::n(),
#         mean     = mean(!!rlang::sym(response_col), na.rm = TRUE),
#         sd       = stats::sd(!!rlang::sym(response_col), na.rm = TRUE),
#         variance = stats::var(!!rlang::sym(response_col), na.rm = TRUE),
#         se       = sd / sqrt(n),
#         df       = n - 1,
#         .groups  = "drop"
#       ) %>%
#       dplyr::arrange(!!rlang::sym(dilution_col))
# 
#   } else {
#     # STRATEGY B: Single replicate per plate per dilution
#     # Pool ACROSS plates at each dilution level
#     pooling_strategy <- "across_plates"
# 
#     # Each plate provides one measurement per dilution
#     # Treat different plates as replicates at each dilution level
#     by_dilution <- data %>%
#       dplyr::group_by(!!rlang::sym(dilution_col)) %>%
#       dplyr::summarise(
#         n        = dplyr::n(),  # Number of plates with this dilution
#         n_plates_at_dilution = dplyr::n_distinct(!!rlang::sym(plate_col)),
#         mean     = mean(!!rlang::sym(response_col), na.rm = TRUE),
#         sd       = stats::sd(!!rlang::sym(response_col), na.rm = TRUE),
#         variance = stats::var(!!rlang::sym(response_col), na.rm = TRUE),
#         se       = sd / sqrt(n),
#         df       = n - 1,
#         .groups  = "drop"
#       ) %>%
#       dplyr::arrange(!!rlang::sym(dilution_col))
#   }
# 
#   ## 4. Filter to dilutions with sufficient replicates
#   by_dilution_valid <- by_dilution %>%
#     dplyr::filter(n >= min_reps, !is.na(variance), variance > 0)
# 
#   n_dilutions_used <- nrow(by_dilution_valid)
# 
#   if (n_dilutions_used == 0) {
#     # Provide more informative warning
#     if (pooling_strategy == "across_plates") {
#       warning(
#         "No dilution levels with sufficient replicates (min_reps = ", min_reps, "). ",
#         "You have ", n_plates, " plates. Need at least ", min_reps,
#         " plates with measurements at the same dilution to estimate SE."
#       )
#     } else {
#       warning("No dilution levels with sufficient replicates (min_reps = ", min_reps, ")")
#     }
# 
#     return(list(
#       overall_se       = NA_real_,
#       by_dilution      = by_dilution,
#       pooling_method   = method,
#       pooling_strategy = pooling_strategy,
#       total_df         = 0,
#       n_dilutions_used = 0,
#       n_plates         = n_plates
#     ))
#   }
# 
#   ## 5. Compute pooled SE estimate based on method
#   overall_se <- switch(method,
# 
#                        # Method 1: Pooled within-group variance (recommended)
#                        "pooled_within" = {
#                          total_df <- sum(by_dilution_valid$df)
#                          pooled_var <- sum(by_dilution_valid$df * by_dilution_valid$variance) / total_df
#                          pooled_sd <- sqrt(pooled_var)
# 
#                          # SE for a single observation
#                          # Use harmonic mean of n for typical replicate count
#                          n_harmonic <- n_dilutions_used / sum(1 / by_dilution_valid$n)
#                          pooled_sd / sqrt(n_harmonic)
#                        },
# 
#                        # Method 2: Median of per-dilution SEs (robust to outliers)
#                        "median_se" = {
#                          stats::median(by_dilution_valid$se, na.rm = TRUE)
#                        },
# 
#                        # Method 3: Mean of per-dilution SEs
#                        "mean_se" = {
#                          mean(by_dilution_valid$se, na.rm = TRUE)
#                        }
#   )
# 
#   ## 6. Compute total degrees of freedom
#   total_df <- sum(by_dilution_valid$df)
# 
#   ## 7. Return results
#   list(
#     overall_se       = overall_se,
#     by_dilution      = by_dilution,
#     pooling_method   = method,
#     pooling_strategy = pooling_strategy,
#     total_df         = total_df,
#     n_dilutions_used = n_dilutions_used,
#     n_plates         = n_plates
#   )
# }

#' Compute Median Assay SE for Each Antigen/Feature Across All Plates
#'
#' For each unique combination of study_accession, experiment_accession,
#' source, antigen, and feature, computes the standard error of assay response
#' at every dilution level across all plates, then returns the median of those
#' per-dilution SEs. This pooled median SE can be reused for error propagation
#' on each individual plate.
#'
#' @param standards_data data.frame containing all standard curve data
#' @param response_col name of the response column (e.g., "mfi")
#' @param dilution_col name of the dilution column (default = "dilution")
#' @param plate_col name of the plate identifier column (default = "plate_nom")
#' @param grouping_cols character vector of columns defining the grouping
#'        (default = c("study_accession", "experiment_accession",
#'                     "source", "antigen", "feature"))
#' @param min_reps minimum number of non-missing plate replicates required at a
#'        dilution level for that dilution's SE to be included (default = 2)
#' @param verbose logical; if TRUE emit progress messages (default = FALSE)
#'
#' @return A data.frame with one row per unique grouping containing:
#'   \item{grouping_cols}{the grouping columns}
#'   \item{median_se}{median SE across all qualifying dilution levels}
#'   \item{n_dilutions_used}{number of dilution levels with >= min_reps
#'         non-missing observations that contributed to the median}
#'   \item{n_plates}{number of distinct plates in the group}
#'   \item{total_obs}{total number of non-missing response observations used}
#'
#' @export
compute_antigen_se_table <- function(
    standards_data,
    response_col  = "mfi",
    dilution_col  = "dilution",
    plate_col     = "plate_nom",
    grouping_cols = c("study_accession",
                      "experiment_accession",
                      "source_nom",
                      "antigen",
                      "feature"),
    min_reps = 2,
    verbose  = FALSE) {
  
  # ------------------------------------------------------------------
  # 1. Input validation
  # ------------------------------------------------------------------
  required_cols <- unique(c(grouping_cols, response_col, dilution_col, plate_col))
  missing_cols  <- setdiff(required_cols, colnames(standards_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  if (!is.numeric(standards_data[[response_col]])) {
    stop("response_col '", response_col, "' must be numeric.")
  }
  
  # ------------------------------------------------------------------
  # 2. Identify unique groupings
  # ------------------------------------------------------------------
  unique_groupings <- unique(standards_data[, grouping_cols, drop = FALSE])
  unique_groupings <- unique_groupings[do.call(order, unique_groupings), , drop = FALSE]
  rownames(unique_groupings) <- NULL
  
  if (verbose) {
    message(sprintf("Computing median SE for %d unique groupings ...",
                    nrow(unique_groupings)))
  }
  
  # ------------------------------------------------------------------
  # 3. Compute median SE for each grouping
  # ------------------------------------------------------------------
  se_results <- lapply(seq_len(nrow(unique_groupings)), function(i) {
    
    grouping <- unique_groupings[i, , drop = FALSE]
    
    # --- 3a. Subset to this grouping across ALL plates ----------------
    mask <- rep(TRUE, nrow(standards_data))
    for (col in grouping_cols) {
      val  <- grouping[[col]]
      mask <- mask & (!is.na(standards_data[[col]]) &
                        standards_data[[col]] == val)
    }
    grp_data <- standards_data[mask, , drop = FALSE]
    
    # --- 3b. Early exit if no data ------------------------------------
    if (nrow(grp_data) == 0L) {
      return(.empty_se_row(grouping, grouping_cols))
    }
    
    # --- 3c. Pull relevant vectors (drop rows where key cols are NA) --
    keep <- !is.na(grp_data[[dilution_col]]) &
      !is.na(grp_data[[plate_col]])
    grp_data <- grp_data[keep, , drop = FALSE]
    
    if (nrow(grp_data) == 0L) {
      return(.empty_se_row(grouping, grouping_cols))
    }
    
    dilutions  <- grp_data[[dilution_col]]
    responses  <- grp_data[[response_col]]   # may contain NA
    plates     <- grp_data[[plate_col]]
    
    unique_dilutions <- sort(unique(dilutions))
    n_plates         <- length(unique(plates))
    
    # --- 3d. SE per dilution level ------------------------------------
    # SE = sd(response across plates) / sqrt(n_non_missing)
    per_dil_se <- vapply(unique_dilutions, function(dil) {
      idx    <- dilutions == dil          # rows for this dilution
      vals   <- responses[idx]            # may include NA
      vals   <- vals[!is.na(vals)]        # drop NA responses
      n      <- length(vals)
      if (n < min_reps) return(NA_real_)  # insufficient replicates
      if (n == 1L)      return(NA_real_)  # sd undefined
      sd(vals) / sqrt(n)
    }, numeric(1L))
    
    # --- 3e. Median SE across dilutions (ignore NA) -------------------
    valid_se       <- per_dil_se[!is.na(per_dil_se)]
    n_dil_used     <- length(valid_se)
    total_obs      <- sum(!is.na(responses))
    
    median_se <- if (n_dil_used == 0L) NA_real_ else median(valid_se)
    
    data.frame(
      grouping,
      median_se       = median_se,
      n_dilutions_used = n_dil_used,
      n_plates        = n_plates,
      total_obs       = total_obs,
      stringsAsFactors = FALSE,
      row.names        = NULL
    )
  })
  
  # ------------------------------------------------------------------
  # 4. Combine and return
  # ------------------------------------------------------------------
  se_table <- do.call(rbind, se_results)
  rownames(se_table) <- NULL
  
  if (verbose) {
    n_valid <- sum(!is.na(se_table$median_se))
    message(sprintf(
      "Done. %d / %d groupings have a valid median SE.",
      n_valid, nrow(se_table)
    ))
  }
  
  return(se_table)
}

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


#' Look Up SE for a Specific Antigen from the SE Table
#'
#' @param se_table data.frame from compute_antigen_se_table()
#' @param study_accession study identifier
#' @param experiment_accession experiment identifier
#' @param source source identifier
#' @param antigen antigen identifier
#'
#' @return numeric SE value, or NA if not found
#' @export
lookup_antigen_se <- function(se_table,
                              study_accession,
                              experiment_accession,
                              source,
                              antigen, feature) {

  if (is.null(se_table) || nrow(se_table) == 0) {
    return(NA_real_)
  }

  # Use source_nom column if available (for ELISA wavelength support),
  # fall back to source column for backward compatibility
  source_col <- if ("source_nom" %in% names(se_table)) "source_nom" else "source"

  idx <- which(
    se_table$study_accession == study_accession &
      se_table$experiment_accession == experiment_accession &
      se_table[[source_col]] == source &
      se_table$antigen == antigen & 
      se_table$feature == feature
  )

  if (length(idx) == 0) {
    return(NA_real_)
  }

  # return(se_table$overall_se[idx[1]])
  return(se_table$median_se[idx[1]])
}



# ---- predict_and_propagate_error: The best fit must contain best_pred and antigen_plate containing plate_samples ----
predict_and_propagate_error <- function(best_fit,
                                        response_var,
                                        antigen_plate,
                                        study_params,
                                        se_std_response,
                                        cv_x_max = 150,
                                        verbose = TRUE) {
  if (study_params$is_log_response) {
    
    # Step 1: validate the raw value before any log transform
    fixed_a_result_raw <- antigen_plate$fixed_a_result
    fixed_a_result_validated <- validate_fixed_lower_asymptote(
      fixed_a_result_raw, verbose = verbose
    )
    
    # Step 2: only apply log10 transform if validation passed
    if (is.null(fixed_a_result_validated)) {
      fixed_a_result <- NULL
    } else {
      .eps <- 0.000005
      fixed_a_result <- log10(fixed_a_result_validated + .eps)
    }
    
    # ── Consistency check: fixed_a_result must agree with what's in coef(fit) ──
    consistency <- check_fixed_a_fit_consistency(
      fit            = best_fit$best_fit,
      fixed_a_result = fixed_a_result,
      context        = paste("predict_and_propagate_error",
                             unique(best_fit$best_data$antigen),
                             unique(best_fit$best_data$plate)),
      verbose        = verbose
    )
    
    if (!consistency$consistent) {
      if (!consistency$correctable) {
        # Cannot propagate — skip gracefully rather than crashing
        warning(sprintf(
          "[predict_and_propagate_error] Skipping propagation for antigen '%s' plate '%s': fixed_a/coef mismatch is not correctable.",
          unique(best_fit$best_data$antigen),
          unique(best_fit$best_data$plate)
        ))
        # Return best_fit with empty sample_se to avoid downstream crash
        best_fit$sample_se <- data.frame()
        return(best_fit)
      }
      fixed_a_result <- consistency$fixed_a_result
    }
    
    log_plate_samples <- log10(antigen_plate$plate_samples[[response_var]])
  }

  # ── Compute overall_se_value ────────────────────────────────────────────
  # se_std_response is on the RAW response scale (e.g. MFI units).
  # When is_log_response is TRUE the model and all y values are on the
  # log10 scale.  The delta method needs se_y on the SAME scale as y.
  #
  # Conversion via the log10 derivative:
  #   if Y = log10(Z)  then  dY = dZ / (Z * ln(10))
  #   => se_log10 = se_mfi / (mean_mfi * ln(10))
  #
  # We use the geometric mean of the raw responses as the reference MFI
  # for the conversion, which is appropriate for a log-transformed variable.

  if (isTRUE(is.finite(se_std_response)) && se_std_response > 0) {

    if (study_params$is_log_response) {
      # Estimate mean raw MFI from standards (geometric mean on raw scale)
      raw_standards <- antigen_plate$plate_standard[[response_var]]
      raw_standards <- raw_standards[is.finite(raw_standards) & raw_standards > 0]

      if (length(raw_standards) > 0) {
        ref_mfi <- exp(mean(log(raw_standards)))   # geometric mean
      } else {
        # Fallback: back-transform from the log10 mean of plate samples
        raw_plate <- antigen_plate$plate_samples[[response_var]]
        raw_plate  <- raw_plate[is.finite(raw_plate) & raw_plate > 0]
        ref_mfi    <- if (length(raw_plate) > 0) exp(mean(log(raw_plate))) else 1
      }

      # Convert SE from raw MFI units to log10 units
      overall_se_value <- sqrt(se_std_response / (ref_mfi * log(10) * 100))

      if (verbose) {
        message(sprintf(
          "[predict_and_propagate] SE conversion: se_mfi=%.4f, ref_mfi=%.4f -> se_log10=%.6f",
          se_std_response, ref_mfi, overall_se_value
        ))
      }

    } else {
      # Response is NOT log-transformed — use se_std_response directly
      overall_se_value <- sqrt(se_std_response / 100)
    }

  } else {
    # se_std_response is NA / non-finite / zero — use a small fallback
    # based on the spread of the log-scale standards if available
    if (study_params$is_log_response) {
      log_stds <- log10(antigen_plate$plate_standard[[response_var]])
      log_stds <- log_stds[is.finite(log_stds)]
      overall_se_value <- if (length(log_stds) > 1) sd(log_stds) * 0.01 else 0.01
    } else {
      overall_se_value <- 0
    }

    if (verbose) {
      message(sprintf(
        "[predict_and_propagate] se_std_response not usable (%.4f); using fallback se=%.6f",
        if (is.finite(se_std_response)) se_std_response else NA_real_,
        overall_se_value
      ))
    }
  }

  # ── Validate that overall_se_value is now on the right scale ────────────
  # On log10 scale, a realistic se_y is << 1 (typically 0.01 – 0.15).
  # Warn loudly if it still looks like raw MFI units.
  if (study_params$is_log_response && overall_se_value > 1) {
    warning(sprintf(
      "[predict_and_propagate] overall_se_value=%.4f is > 1 on the log10 scale. ",
      overall_se_value,
      "This will inflate se_x. Check that se_std_response is in raw response units."
    ))
  }

  lloq  <- if (!is.null(best_fit$best_glance$lloq)) as.numeric(best_fit$best_glance$lloq)[1] else NA_real_
  uloq  <- if (!is.null(best_fit$best_glance$uloq)) as.numeric(best_fit$best_glance$uloq)[1] else NA_real_

  # ── Standards prediction curve ──────────────────────────────────────────
  pred_se <- best_fit$best_pred

  if (verbose) {
    message(paste("pred_se has", nrow(pred_se), "row(s)"))
  }

  z <- qnorm(0.975)
  max_conc_standard <- ifelse(
    study_params$is_log_independent,
    log10(antigen_plate$antigen_settings$standard_curve_concentration),
    antigen_plate$antigen_settings$standard_curve_concentration
  )

  pred_se$overall_se <- overall_se_value

  diagnose_propagation_inputs(
    fit     = best_fit$best_fit,
    model   = best_fit$best_model_name,
    fixed_a = fixed_a_result,
    y_test  = NULL
  )

  pred_se <- propagate_error_dataframe(
    pred_df  = pred_se,
    fit      = best_fit$best_fit,
    model    = best_fit$best_model_name,
    y_col    = "yhat",
    se_col   = "overall_se",
    fixed_a  = fixed_a_result,
    cv_x_max = cv_x_max,
    is_log_x = study_params$is_log_independent   # TRUE when x is log10(conc)
  )

  diagnose_cv_x(
    df       = pred_se,
    label    = "standards pred_se",
    lloq     = lloq,
    uloq     = uloq,
    cv_x_max = cv_x_max,
    verbose  = verbose
  )

  pred_se$predicted_concentration <- ifelse(
    !is.infinite(pred_se$predicted_concentration),
    pred_se$predicted_concentration,
    ifelse(pred_se$predicted_concentration > 0, max_conc_standard, 0)
  )

  pred_se$pcov                     <- pred_se$cv_x
  pred_se$study_accession          <- unique(best_fit$best_data$study_accession)
  pred_se$experiment_accession     <- unique(best_fit$best_data$experiment_accession)
  pred_se$nominal_sample_dilution  <- unique(best_fit$best_data$nominal_sample_dilution)
  pred_se$plateid                  <- unique(best_fit$best_data$plateid)
  pred_se$plate                    <- unique(best_fit$best_data$plate)
  pred_se$antigen                  <- unique(best_fit$best_data$antigen)
  pred_se$source                   <- unique(best_fit$best_data$source)

  pred_se <- attach_grouping_keys(pred_se, best_fit$best_data, context = "predict_and_propagate_error/pred_se")

  # pred_se_v <<- pred_se
  best_fit$best_pred <- pred_se

  # ── Sample propagation ──────────────────────────────────────────────────
  raw_assay_response <- antigen_plate$plate_samples[[response_var]]

  if (verbose) print(head(antigen_plate$plate_samples))

  sample_se <- data.frame(
    .row_id            = seq_len(length(log_plate_samples)),
    y_new              = log_plate_samples,
    raw_assay_response = raw_assay_response,
    dilution           = antigen_plate$plate_samples$dilution,
    well               = antigen_plate$plate_samples$well
  )

  if (verbose) {
    message(paste("sample_se has", nrow(sample_se), "row(s)"))
  }

  sample_se$overall_se      <- overall_se_value   # already on log10 scale
  sample_se[[response_var]] <- sample_se$y_new

  diagnose_propagation_inputs(
    fit     = best_fit$best_fit,
    model   = best_fit$best_model_name,
    fixed_a = fixed_a_result,
    y_test  = NULL
  )

  sample_se <- propagate_error_dataframe(
    pred_df  = sample_se,
    fit      = best_fit$best_fit,
    model    = best_fit$best_model_name,
    y_col    = response_var,
    se_col   = "overall_se",
    fixed_a  = fixed_a_result,
    cv_x_max = cv_x_max,
    is_log_x = study_params$is_log_independent
  )

  diagnose_cv_x(
    df       = sample_se,
    label    = "samples sample_se",
    lloq     = lloq,
    uloq     = uloq,
    cv_x_max = cv_x_max,
    verbose  = verbose
  )

  if (study_params$is_log_independent) {
    sample_se$final_predicted_concentration <-
      10^sample_se$predicted_concentration * sample_se$dilution
  } else {
    sample_se$final_predicted_concentration <-
      sample_se$predicted_concentration * sample_se$dilution
  }

  # Build the left-hand side for the join: plate_samples minus response_var and
  # dilution (already in sample_se), plus a .row_id to guarantee 1:1 matching.
  # Joining only on "well" causes many-to-many when multiple patients/timeperiods
  # share the same well label within the plate.
  plate_samples_for_join <- antigen_plate$plate_samples[
    , !(names(antigen_plate$plate_samples) %in% c(response_var, "dilution")),
    drop = FALSE
  ]
  plate_samples_for_join$.row_id <- seq_len(nrow(plate_samples_for_join))

  sample_se <- dplyr::inner_join(
    plate_samples_for_join,
    sample_se,
    by = c(".row_id", "well")
  )
  sample_se$.row_id <- NULL

  sample_se$pcov    <- sample_se$cv_x
  sample_se$source  <- unique(best_fit$best_data$source)

  # source_nom is an internal routing column (source + wavelength composite);
  # do NOT propagate it — source and wavelength are stored as separate DB columns.
  # wavelength and feature are handled by attach_grouping_keys below.
  sample_se <- attach_grouping_keys(sample_se, best_fit$best_data, context = "predict_and_propagate_error/sample_se")

  # Remove intermediate columns, rename se_x
  sample_se <- sample_se[, !names(sample_se) %in% c("y_new")]
  names(sample_se)[names(sample_se) == "se_x"] <- "se_concentration"

  names(sample_se)[names(sample_se) == "predicted_concentration"] <-
    "raw_predicted_concentration"

  # sample_se$raw_robust_concentration   <- NA_real_
  # sample_se$final_robust_concentration <- NA_real_
  # sample_se$se_robust_concentration    <- NA_real_
  # sample_se$pcov_robust_concentration  <- NA_real_
  # 
  # sample_se_v  <<- sample_se
  best_fit$sample_se <- sample_se

  if (verbose) message("Finished predict_and_propagate_error")

  return(best_fit)
}

# ---- Test that se_x is minimum near inflection point ----
test_se_at_inflection <- function(best_fit) {
  pred <- best_fit$best_pred
  inflect_x <- best_fit$best_glance$inflect_x

  # Find the x value closest to inflection point
  closest_idx <- which.min(abs(pred$x - inflect_x))
  se_at_inflect <- pred$se_x[closest_idx]

  # SE should be minimum (or very close to minimum) at inflection
  min_se <- min(pred$se_x, na.rm = TRUE)

  cat("SE at inflection point:", se_at_inflect, "\n")
  cat("Minimum SE in curve:", min_se, "\n")
  cat("Ratio (should be ~1.0):", se_at_inflect / min_se, "\n")

  # Visual check
  plot(pred$x, pred$se_x, type = "l",
       xlab = "Concentration", ylab = "SE(x)")
  abline(v = inflect_x, col = "red", lty = 2)
  points(pred$x[closest_idx], se_at_inflect, col = "red", pch = 19)
}

test_cv_x_at_inflection <- function(best_fit, verbose = TRUE) {
  pred       <- best_fit$best_pred
  inflect_x  <- best_fit$best_glance$inflect_x

  if (!"cv_x" %in% names(pred)) {
    if (verbose) message("[test_cv_x] cv_x not found in best_pred.")
    return(invisible(NULL))
  }

  finite_mask  <- is.finite(pred$cv_x) & is.finite(pred$predicted_concentration)
  cv_f  <- pred$cv_x[finite_mask]
  xc_f  <- pred$predicted_concentration[finite_mask]

  min_cv      <- min(cv_f, na.rm = TRUE)
  min_idx     <- which.min(cv_f)
  x_at_min_cv <- xc_f[min_idx]

  closest_idx    <- which.min(abs(xc_f - inflect_x))
  cv_at_inflect  <- cv_f[closest_idx]

  ratio <- cv_at_inflect / min_cv

  if (verbose) {
    cat("\n--- cv_x inflection test ---\n")
    cat("Inflection point (x)        :", inflect_x, "\n")
    cat("cv_x at inflection point    :", cv_at_inflect, "\n")
    cat("Minimum cv_x in curve       :", min_cv, "\n")
    cat("x at minimum cv_x           :", x_at_min_cv, "\n")
    cat("Ratio cv_at_inflect/min_cv  :", ratio,
        " (expected ~1.0 if inflection == minimum cv)\n")

    # Visual check
    plot(xc_f, cv_f, type = "l",
         xlab = "Predicted Concentration (log10)", ylab = "CV_x (%)",
         main = "cv_x across concentration range")
    abline(h  = 20,         col = "orange", lty = 2)
    abline(h  = 125,        col = "red",    lty = 2)
    abline(v  = inflect_x,  col = "blue",   lty = 2)
    points(x_at_min_cv, min_cv, col = "darkgreen", pch = 19, cex = 1.5)
    points(xc_f[closest_idx], cv_at_inflect, col = "blue", pch = 19, cex = 1.5)
    legend("topright",
           legend = c("cv_x = 20 threshold", "cv_x = 125 ceiling",
                      "Inflection point", "Min cv_x"),
           col    = c("orange", "red", "blue", "darkgreen"),
           lty    = c(2, 2, 2, NA), pch = c(NA, NA, NA, 19))
  }

  invisible(list(
    cv_at_inflect  = cv_at_inflect,
    min_cv         = min_cv,
    x_at_min_cv    = x_at_min_cv,
    ratio          = ratio
  ))
}

### ---- gate_samples ----
gate_samples <- function(best_fit,
                         response_variable,
                         pcov_threshold,
                         verbose = TRUE) {
  sample_se <- best_fit$sample_se

  # Ensure all threshold values are scalar numerics (not lists)
  lloq_x <- as.numeric(best_fit$best_glance$lloq)[1]
  uloq_x <- as.numeric(best_fit$best_glance$uloq)[1]
  ulod <- as.numeric(best_fit$best_glance$ulod)[1]
  llod <- as.numeric(best_fit$best_glance$llod)[1]
  inflect_x <- as.numeric(best_fit$best_glance$inflect_x)[1]

  sample_se$gate_class_loq <- ifelse(sample_se$raw_predicted_concentration >= lloq_x &sample_se$raw_predicted_concentration <= uloq_x,
                                     "Acceptable",
                                     ifelse(sample_se$raw_predicted_concentration > uloq_x, "Too Concentrated", "Too Diluted"))

  sample_se$gate_class_lod <- ifelse(sample_se[[response_variable]] >= llod & sample_se[[response_variable]] <= ulod,
                                     "Acceptable",
                                     ifelse(sample_se[[response_variable]] > ulod, "Too Concentrated", "Too Diluted"))

  sample_se$gate_class_pcov <- ifelse(sample_se$pcov <= pcov_threshold,
                                      "Acceptable",
                                      ifelse(sample_se$raw_predicted_concentration < inflect_x, "Too Diluted", "Too Concentrated"))

  best_fit$sample_se <- sample_se

  return(best_fit)


}


# ---- Consistency guard: formula vs fixed_a vs coef(fit) ----
# Call this just before propagate_error_dataframe to catch mismatches early.
check_fixed_a_fit_consistency <- function(fit, fixed_a_result, context = "", verbose = TRUE) {
  
  has_a_in_coef  <- "a" %in% names(coef(fit))
  fixed_a_is_set <- !is.null(fixed_a_result) && is.finite(as.numeric(fixed_a_result))
  
  consistent <- (fixed_a_is_set && !has_a_in_coef) ||   # fixed: a baked in, not in coef
    (!fixed_a_is_set && has_a_in_coef)       # free:  a estimated, in coef
  
  if (!consistent) {
    msg <- sprintf(
      "[check_fixed_a_fit_consistency] MISMATCH in '%s':\n  fixed_a_result = %s\n  'a' in coef(fit) = %s\n  This will cause propagation to fail.\n  Likely cause: validate_fixed_lower_asymptote() nullified fixed_a AFTER fitting.\n  Fix: validate fixed_a_result BEFORE select_model_formulas() is called.",
      context,
      if (fixed_a_is_set) format(round(as.numeric(fixed_a_result), 5)) else "NULL",
      if (has_a_in_coef) "TRUE (free parameter)" else "FALSE (baked into formula)"
    )
    warning(msg)
    
    # Auto-correct: if 'a' is not in coef and fixed_a is NULL,
    # this means the formula was built with a fixed value but we lost it.
    # We cannot recover the original fixed value, so return a correction instruction.
    if (!fixed_a_is_set && !has_a_in_coef) {
      if (verbose) message(
        "[check_fixed_a_fit_consistency] Cannot propagate: 'a' is neither free nor provided as fixed_a.\n",
        "  Returning fixed_a = NULL with a warning. This plate will have NA propagation results."
      )
      return(list(fixed_a_result = NULL, consistent = FALSE, correctable = FALSE))
    }
    
    # If fixed_a is set but 'a' is also in coef — use fixed_a, drop 'a' from coef
    # (propagate_error_dataframe already handles this case with a warning)
    return(list(fixed_a_result = fixed_a_result, consistent = FALSE, correctable = TRUE))
  }
  
  return(list(fixed_a_result = fixed_a_result, consistent = TRUE, correctable = TRUE))
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