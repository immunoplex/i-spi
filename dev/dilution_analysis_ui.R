# ============================================================================
# DILUTION ANALYSIS MODULE - COMPLETE IMPLEMENTATION
# ============================================================================
# Structure:
# - PART 1: Configuration Layer (pure data + rules)
# - PART 2: Business Logic Functions (stateless helpers)
# - PART 3: Orchestration (reactive coordinator)
# ============================================================================

# ============================================================================
# PART 1: CONFIGURATION LAYER
# ============================================================================

DILUTION_ANALYSIS_CONFIG <- list(
  required_params = c(
    "blank_option",
    "is_log_mfi_axis",
    "node_order",
    "valid_gate_class",
    "is_binary_gc",
    "antigen_family_order",
    "antigen_order",
    "timeperiod_order",
    "zero_pass_diluted_Tx",
    "zero_pass_concentrated_diluted_Tx",
    "zero_pass_concentrated_Tx",
    "two_plus_pass_acceptable_Tx",
    "one_pass_acceptable_Tx"
  ),

  table_columns = c(
    "study_accession", "experiment_accession", "plateid", "timeperiod",
    "patientid", "agroup", "dilution", "antigen", "n_pass_dilutions",
    "concentration_status", "au_treatment", "decision_nodes", "bkg_method",
    "processed_au"
  ),

  au_treatment_types = c(
    "all_au", "passing_au", "geom_all_au", "geom_passing_au",
    "exclude_au", "replace_positive_control", "replace_blank"
  ),

  color_mappings = list(
    all_au = "#a1caf1",
    passing_au = "lightgrey",
    geom_all_au = "#c2b280",
    geom_passing_au = "#875692",
    replace_blank = "#008856",
    replace_positive_control = "#dcd300",
    exclude_au = "#b3446c",
    Blank = "white"
  ),

  node_name_replacements = c(
    limits_of_detection = "limits_of_detection",
    linear_region = "linear",
    limits_of_quantification = "limits_of_quantification"
  )
)

parse_study_configuration <- function(study_config, config_schema = DILUTION_ANALYSIS_CONFIG) {
  missing_params <- setdiff(
    config_schema$required_params,
    study_config$param_name
  )

  if (length(missing_params) > 0) {
    stop(paste("Missing required configuration parameters:",
               paste(missing_params, collapse = ", ")))
  }

  get_param <- function(name, type = "character") {
    row <- study_config[study_config$param_name == name, ]
    if (nrow(row) == 0) return(NULL)

    value <- switch(type,
                    "character" = row$param_character_value,
                    "boolean" = as.logical(toupper(row$param_boolean_value)),
                    "numeric" = as.numeric(row$param_numeric_value),
                    row$param_character_value
    )

    if (type == "character" && grepl(",", value)) {
      value <- strsplit(value, ",")[[1]]
    }

    return(value)
  }

  node_order_raw <- get_param("node_order")
  replacements <- config_schema$node_name_replacements

  list(
    background_method = get_param("blank_option"),
    is_log_mfi = get_param("is_log_mfi_axis", "boolean"),
    node_order = node_order_raw,
    sufficient_gc = get_param("valid_gate_class"),
    is_binary_gc = get_param("is_binary_gc", "boolean"),
    antigen_family_order = get_param("antigen_family_order"),
    antigen_order = get_param("antigen_order"),
    zero_pass_diluted_Tx               = get_param("zero_pass_diluted_Tx"),
    zero_pass_concentrated_diluted_Tx  = get_param("zero_pass_concentrated_diluted_Tx"),
    zero_pass_concentrated_Tx          = get_param("zero_pass_concentrated_Tx"),
    one_pass_acceptable_Tx             = get_param("one_pass_acceptable_Tx"),
    two_plus_pass_acceptable_Tx        = get_param("two_plus_pass_acceptable_Tx"),
    timeperiod_order = get_param("timeperiod_order"),
    normalized_node_order = ifelse(
      node_order_raw %in% names(replacements),
      replacements[node_order_raw],
      node_order_raw
    )
  )
}

create_config_signature <- function(parsed_config) {
  config_string <- paste(
    parsed_config$background_method,
    parsed_config$is_log_mfi,
    paste(parsed_config$node_order, collapse = ","),
    paste(parsed_config$sufficient_gc, collapse = ","),
    parsed_config$is_binary_gc,
    paste(parsed_config$antigen_family_order, collapse = ","),
    paste(parsed_config$antigen_order, collapse = ","),
    paste(parsed_config$timeperiod_order, collapse = ","),
    paste(parsed_config$zero_pass_diluted_Tx, collapse = ","),
    paste(parsed_config$zero_pass_concentrated_diluted_Tx, collapse = ","),
    paste(parsed_config$zero_pass_concentrated_Tx, collapse = ","),
    paste(parsed_config$two_plus_pass_acceptable_Tx, collapse = ","),
    paste(parsed_config$one_pass_acceptable_Tx, collapse = ","),
    sep = "::"
  )

  digest::digest(config_string, algo = "md5")
}

# ============================================================================
# PART 2: BUSINESS LOGIC FUNCTIONS
# ============================================================================

load_dilution_analysis_data <- function(study_accession,
                                        experiment_accession,
                                        project_id,
                                        conn,
                                        parsed_config) {

  cat("\n=== LOADING DATA ===\n")
  cat("Study:", study_accession, "\n")
  cat("Experiment:", experiment_accession, "\n")

  standard_data <- fetch_db_standards(
    study_accession = study_accession,
    experiment_accession = experiment_accession,
    project_id = project_id,
    conn = conn
  )

  controls <- fetch_db_controls(
    study_accession = study_accession,
    experiment_accession = experiment_accession,
    project_id = project_id,
    conn = conn
  )

  buffer_data <- fetch_db_buffer(
    study_accession = study_accession,
    experiment_accession = experiment_accession,
    project_id = project_id,
    conn = conn
  )

  std_curve_data <- NULL
  if (!is.null(standard_data) && nrow(standard_data) > 0) {
    standard_data$selected_str <- paste0(
      standard_data$study_accession,
      standard_data$experiment_accession
    )
    standard_data <- standard_data[
      standard_data$selected_str == paste0(study_accession, experiment_accession),
    ]
    std_curve_data <- calculate_log_dilution(standard_data)
    std_curve_data$subject_accession <- std_curve_data$patientid
  }

  if (!is.null(buffer_data) && nrow(buffer_data) > 0) {
    buffer_data$selected_str <- paste0(
      buffer_data$study_accession,
      buffer_data$experiment_accession
    )
    buffer_data <- buffer_data[
      buffer_data$selected_str == paste0(study_accession, experiment_accession),
    ]
  }

  cat("Data loaded successfully\n")
  cat("Standard curve rows:", if(!is.null(std_curve_data)) nrow(std_curve_data) else 0, "\n")
  cat("Controls rows:", if(!is.null(controls)) nrow(controls) else 0, "\n\n")

  return(list(
    std_curve = std_curve_data,
    controls = controls,
    buffer = buffer_data
  ))
}

compute_gated_data <- function(study_accession,
                               experiment_accession,
                               project_id,
                               conn,
                               parsed_config) {

  cat("\n=== COMPUTING GATED DATA ===\n")
  print(parsed_config)

  gated_data <- calculate_sample_concentration_status_new(
    conn = conn,
    study_accession = study_accession,
    experiment_accession = experiment_accession,
    project_id = project_id,
    node_order = parsed_config$normalized_node_order
  )

  cat("Gated data rows:", nrow(gated_data), "\n")

  validation_results <- validate_gated_data_by_nodes(
    gated_data,
    parsed_config$normalized_node_order
  )

  cat("=== GATED DATA COMPLETE ===\n\n")

  return(list(
    data = gated_data,
    validation = validation_results
  ))
}

validate_gated_data_by_nodes <- function(gated_data, node_order) {

  results <- list()

  if ("linear" %in% node_order) {
    has_linear <- sum(!is.na(gated_data$in_linear_region)) > 0
    results$linear <- list(
      present = has_linear,
      message = if (!has_linear) "No samples found in the linear region" else NULL
    )
  }

  if ("quantifiable" %in% node_order) {
    has_quant <- sum(!is.na(gated_data$in_quantifiable_range)) > 0
    results$quantifiable <- list(
      present = has_quant,
      message = if (!has_quant) "No samples found in the quantifiable range" else NULL
    )
  }

  if ("gate" %in% node_order) {
    has_gate <- sum(!is.na(gated_data$llod) & !is.na(gated_data$ulod)) > 0
    results$gate <- list(
      present = has_gate,
      message = if (!has_gate) "No samples found passing Limit of Detection" else NULL
    )
  }

  return(results)
}

build_decision_tree <- function(parsed_config) {
  truth_table <- create_truth_table(
    binary_gate = parsed_config$is_binary_gc,
    exclude_linear = FALSE,
    exclude_quantifiable = FALSE,
    exclude_gate = FALSE
  )

  create_decision_tree_tt(
    truth_table = truth_table,
    binary_gate = parsed_config$is_binary_gc,
    sufficient_gc_vector = parsed_config$sufficient_gc,
    node_order = node_to_tree_col(parsed_config$normalized_node_order)
  )
}

compute_classified_data <- function(gated_data,
                                    selected_dilutions,
                                    parsed_config) {

  study_config_raw <- data.frame(
    param_name = names(parsed_config),
    param_character_value = sapply(parsed_config, function(x) {
      if (is.vector(x)) paste(x, collapse = ",") else as.character(x)
    }),
    stringsAsFactors = FALSE
  )

  result_update <- compute_classified_merged_update(
    classified_sample = gated_data,
    selectedDilutions = selected_dilutions,
    study_configuration = study_config_raw
  )

  if ("n_pass_d" %in% names(result_update) && "n_pass_dilutions" %in% names(result_update)) {
    cat("Both n_pass_d and n_pass_dilutions exist. Removing n_pass_d.\n")
    result_update$n_pass_d <- NULL
  } else if ("n_pass_d" %in% names(result_update)) {
    cat("Renaming 'n_pass_d' to 'n_pass_dilutions'\n")
    names(result_update)[names(result_update) == "n_pass_d"] <- "n_pass_dilutions"
  }

  return(result_update)
}

process_final_au_table <- function(classified_data,
                                   std_curve_filtered,
                                   controls,
                                   parsed_config,
                                   selected_antigen,
                                   table_columns) {

  classified_data$decision_nodes <- paste(parsed_config$node_order, collapse = ",")
  classified_data$bkg_method <- parsed_config$background_method

  final_table <- data.frame()

  for (treatment in unique(classified_data$au_treatment)) {
    treatment_subset <- classified_data[classified_data$au_treatment == treatment, ]

    average_au <- switch(
      treatment,
      "all_au" = preserve_all_au(treatment_subset),
      "passing_au" = preserve_passing_au(treatment_subset),
      "geom_all_au" = geometric_mean_all_au_2(treatment_subset),
      "geom_passing_au" = geometric_mean_passing_au_2(treatment_subset),
      "exclude_au" = preserve_all_au(treatment_subset),
      "replace_positive_control" = {
        if (is.null(controls)) {
          stop("Controls data required for replace_positive_control treatment")
        }
        positive_controls <- controls[controls$antigen %in% selected_antigen, ]
        geometric_mean_positive_controls(treatment_subset, positive_controls)
      },
      "replace_blank" = {
        if (is.null(std_curve_filtered)) {
          stop("Standard curve data required for replace_blank treatment")
        }
        geometric_mean_blanks(treatment_subset, std_curve_filtered)
      }
    )

    final_table <- rbind(final_table, average_au[, table_columns])
  }

  final_table$dilution[!(final_table$au_treatment %in% c("all_au", "exclude_au"))] <- NA
  final_table$processed_au[final_table$au_treatment == "exclude_au"] <- NA
  final_table$plateid[final_table$au_treatment %in%
                        c("geom_all_au", "geom_passing_au",
                          "replace_positive_control", "replace_blank")] <- NA

  return(final_table)
}

# ============================================================================
# PART 3: ORCHESTRATION
# ============================================================================

# Helper to clear all dilution analysis notifications
clear_all_da_notifications <- function() {
  removeNotification(id = "load_quality_classification")
  removeNotification(id = "load_subject_inspection")
  removeNotification(id = "load_sample_measurement")
}

# Track state
dilution_analysis_active <- reactiveVal(FALSE)
current_context <- reactiveVal(NULL)
current_config_signature <- reactiveVal(NULL)

selected_study_dilution_analysis_rv <- reactiveVal(NULL)
selected_experiment_dilution_analysis_rv <- reactiveVal(NULL)

# Cached data stores
parsed_config_cache <- reactiveVal(NULL)
loaded_data_cache <- reactiveVal(NULL)
gated_data_cache <- reactiveVal(NULL)

analysis_context <- reactive({
  if (
    is.null(input$advanced_qc_component) ||
    length(input$advanced_qc_component) == 0 ||
    input$advanced_qc_component != "Dilution Analysis" ||
    is.null(input$study_level_tabs) ||
    input$study_level_tabs != "Experiments" ||
    is.null(input$main_tabs) ||
    input$main_tabs != "view_files_tab" ||
    is.null(input$readxMap_study_accession) ||
    input$readxMap_study_accession == "Click here" ||
    is.null(input$readxMap_experiment_accession) ||
    input$readxMap_experiment_accession == "Click here"
  ) {
    return(NULL)
  }

  paste(
    input$readxMap_study_accession,
    input$readxMap_experiment_accession,
    sep = "::"
  )
})

study_config_reactive <- reactive({
  req(analysis_context())

  context_parts <- strsplit(analysis_context(), "::")[[1]]
  selected_study <- context_parts[1]

  fetch_study_configuration(db_pool, study = selected_study, project_id = userWorkSpaceID())
})

parsed_config_with_signature <- reactive({
  req(analysis_context())
  req(study_config_reactive())

  parsed <- parse_study_configuration(
    study_config_reactive(),
    DILUTION_ANALYSIS_CONFIG
  )

  list(
    config = parsed,
    signature = create_config_signature(parsed)
  )
})

# ============================================================================
# MAIN OBSERVER
# ============================================================================
observeEvent(
  list(
    analysis_context(),
    parsed_config_with_signature()$signature
  ),
  {
    # ========================================================================
    # GUARD: Not on Dilution Analysis tab
    # ========================================================================
    if (is.null(input$advanced_qc_component) ||
        length(input$advanced_qc_component) == 0 ||
        input$advanced_qc_component != "Dilution Analysis") {

      if (isolate(dilution_analysis_active())) {
        cat("\n=== CLEANING UP DILUTION ANALYSIS ===\n")
        cat("Reason: Navigated away\n\n")

        clear_all_da_notifications()

        output$dilutionAnalysisUI <- NULL
        output$dilution_summary_barplot <- NULL
        output$download_dilution_contingency_summary <- NULL
        output$download_dilution_contingency_summary_handle <- NULL
        output$dilution_selector_UI <- NULL
        output$antigen_selector_UI <- NULL
        output$margin_table_antigen <- NULL
        output$color_legend <- NULL
        output$download_classified_sample_UI <- NULL
        output$download_classifed_sample_handle <- NULL
        output$patient_dilution_series_plot <- NULL
        output$passing_subject_selection <- NULL
        output$final_average_au_table <- NULL
        output$download_average_au_table_UI <- NULL
        output$decision_tree <- NULL
        output$parameter_dependencies <- NULL
        output$linear_message <- NULL
        output$quantifiable_message <- NULL
        output$gate_message <- NULL
        output$dilution_subject_selector_UI <- NULL
        output$antigen_subject_selector_UI <- NULL

        parsed_config_cache(NULL)
        loaded_data_cache(NULL)
        gated_data_cache(NULL)
        current_config_signature(NULL)
        dilution_analysis_active(FALSE)
        current_context(NULL)
      }
      return()
    }

    new_context <- analysis_context()
    old_context <- isolate(current_context())

    new_config_sig <- isolate(parsed_config_with_signature()$signature)
    old_config_sig <- isolate(current_config_signature())

    # ========================================================================
    # CLEANUP: Context is NULL (left tab)
    # ========================================================================
    if (is.null(new_context)) {
      if (dilution_analysis_active()) {
        cat("\n=== CLEANING UP DILUTION ANALYSIS ===\n")
        cat("Reason: Left Dilution Analysis tab\n\n")

        clear_all_da_notifications()

        output$dilutionAnalysisUI <- NULL
        output$dilution_summary_barplot <- NULL
        output$download_dilution_contingency_summary <- NULL
        output$download_dilution_contingency_summary_handle <- NULL
        output$dilution_selector_UI <- NULL
        output$antigen_selector_UI <- NULL
        output$margin_table_antigen <- NULL
        output$color_legend <- NULL
        output$download_classified_sample_UI <- NULL
        output$download_classifed_sample_handle <- NULL
        output$patient_dilution_series_plot <- NULL
        output$passing_subject_selection <- NULL
        output$final_average_au_table <- NULL
        output$download_average_au_table_UI <- NULL
        output$decision_tree <- NULL
        output$parameter_dependencies <- NULL
        output$linear_message <- NULL
        output$quantifiable_message <- NULL
        output$gate_message <- NULL
        output$dilution_subject_selector_UI <- NULL
        output$antigen_subject_selector_UI <- NULL

        parsed_config_cache(NULL)
        loaded_data_cache(NULL)
        gated_data_cache(NULL)
        current_config_signature(NULL)
        dilution_analysis_active(FALSE)
        current_context(NULL)
      }
      return()
    }

    # ========================================================================
    # DETECT WHAT CHANGED
    # ========================================================================
    context_changed <- !identical(old_context, new_context)
    config_changed <- !is.null(old_config_sig) &&
      !identical(old_config_sig, new_config_sig)

    # ========================================================================
    # SKIP: Nothing changed
    # ========================================================================
    if (!is.null(old_context) && !context_changed && !config_changed) {
      cat("\n=== NO CHANGES DETECTED — SKIP ===\n")
      cat("Context:", new_context, "\n")
      cat("Config signature:", new_config_sig, "\n\n")
      return()
    }

    if (!is.null(new_context) == FALSE) {
      cat("\n=== SKIPPING SETUP: Not in Dilution Analysis tab ===\n")
      return()
    }

    # ========================================================================
    # LOG CHANGES
    # ========================================================================
    if (context_changed) {
      cat("\n=== CONTEXT CHANGED ===\n")
      cat("Old context:", old_context, "\n")
      cat("New context:", new_context, "\n\n")
    }

    if (config_changed) {
      cat("\n=== CONFIGURATION CHANGED ===\n")
      cat("Old signature:", old_config_sig, "\n")
      cat("New signature:", new_config_sig, "\n")

      if (is.null(input$advanced_qc_component) ||
          length(input$advanced_qc_component) == 0 ||
          input$advanced_qc_component != "Dilution Analysis") {
        cat("Configuration changed but not on Dilution Analysis tab - skipping reload\n\n")
        return()
      }

      cat("Reloading analysis with new configuration...\n\n")
      showNotification("Study configuration changed.", duration = 3, type = "warning")
    }

    # Clean up before reload
    if (!is.null(old_context)) {
      clear_all_da_notifications()
      dilution_analysis_active(FALSE)
    }

    if (input$readxMap_experiment_accession == "Click here") {
      clear_all_da_notifications()
      return()
    }

    # ========================================================================
    # SETUP NEW CONTEXT
    # ========================================================================
    cat("\n=== SETTING UP DILUTION ANALYSIS ===\n")
    if (context_changed) cat("Reason: Context changed\n")
    if (config_changed) cat("Reason: Configuration changed\n")

    current_context(new_context)
    current_config_signature(new_config_sig)
    dilution_analysis_active(TRUE)

    context_parts <- strsplit(new_context, "::")[[1]]
    selected_study <- context_parts[1]
    selected_experiment <- context_parts[2]
    selected_study_dilution_analysis_rv(selected_study)
    selected_experiment_dilution_analysis_rv(selected_experiment)

    # ========================================================================
    # STEP 1: Configuration
    # ========================================================================
    parsed_config <- isolate(parsed_config_with_signature()$config)
    parsed_config_cache(parsed_config)

    # ========================================================================
    # STEP 2: Load data
    # ========================================================================
    loaded_data <- load_dilution_analysis_data(
      study_accession = selected_study,
      experiment_accession = selected_experiment,
      project_id = userWorkSpaceID(),
      conn = conn,
      parsed_config = parsed_config
    )
    loaded_data_cache(loaded_data)

    # ========================================================================
    # STEP 3: Compute gated data
    # Quality Classification notification wraps this blocking call
    # ========================================================================
    showNotification(
      id = "load_quality_classification",
      HTML("Loading Quality Classification<span class='dots'>"),
      duration = NULL,
      type = "message"
    )

    gated_result <- compute_gated_data(
      study_accession = selected_study,
      experiment_accession = selected_experiment,
      project_id = userWorkSpaceID(),
      conn = conn,
      parsed_config = parsed_config
    )
    gated_data_cache(gated_result$data)

    removeNotification(id = "load_quality_classification")

    # ========================================================================
    # STEP 4: Validation messages
    # ========================================================================
    output$linear_message <- renderUI({
      if (!is.null(gated_result$validation$linear) &&
          !gated_result$validation$linear$present) {
        HTML(paste0(
          "<span style='font-size: 1.5em;'>",
          gated_result$validation$linear$message,
          " for ", selected_experiment, " in ", selected_study,
          " using the ", parsed_config$background_method, " method for the blanks.</span>"
        ))
      } else {
        NULL
      }
    })

    output$quantifiable_message <- renderUI({
      if (!is.null(gated_result$validation$quantifiable) &&
          !gated_result$validation$quantifiable$present) {
        HTML(paste0(
          "<span style='font-size: 1.5em;'>",
          gated_result$validation$quantifiable$message,
          " for ", selected_experiment, " in ", selected_study,
          " using the ", parsed_config$background_method, " method for the blanks.</span>"
        ))
      } else {
        NULL
      }
    })

    output$gate_message <- renderUI({
      if (!is.null(gated_result$validation$gate) &&
          !gated_result$validation$gate$present) {
        HTML(paste0(
          "<span style='font-size: 1.5em;'>",
          gated_result$validation$gate$message,
          " for ", selected_experiment, " in ", selected_study,
          " using the ", parsed_config$background_method, " method for the blanks.</span>"
        ))
      } else {
        NULL
      }
    })

    # ========================================================================
    # STEP 5: Main UI
    # ========================================================================
    output$dilutionAnalysisUI <- renderUI({
      req(dilution_analysis_active())
      req(parsed_config_cache())

      tagList(
        fluidRow(
          column(12,
                 bsCollapse(
                   id = "dilutionAnalysisMethods",
                   bsCollapsePanel(
                     title = "Dilution Analysis Methods",
                     tagList(
                       tags$p("Use 'Select Antigen' dropdown menu to select one or more antigens of interest to conduct dilution analysis on in the sample data.
                       When the Dilution Analysis tab is loaded, all of the available dilutions are initially selected and shown in the 'Select Dilutions' dropdown menu.
                       Dilutions can be manually excluded or included before running the dilution analysis by changing this selection."),
                       tags$p("The dilution analysis for the selected antigens begins by following a decision tree structure.
                              When a standard curve is saved, sample values are gated and have attributes of being in the linear range and in the quantifiable range of the standard curve or not.
                              Using these characteristics, a decision tree can be made to classify the sample points as either passing classification or not passing classification."),
                       tags$p("Figure 1 depicts the decision tree that is produced based on the decisions that are selected above.
                       Since a decision tree structure is used, when there are multiple decision nodes selected samples will pass classification
                       if they are in the path where all the nodes are true."),
                       tags$p("Figure 2 depicts a barplot entitled with the selected experiment's name followed by 'Proportion of Subjects by Antigen, Dilution Factor, and Concentration Status.
                      The antigens are sorted by the antigen order as defined by the researcher in the study configuration options."),
                       tags$p("Table 1, titled 'Number of Subjects Passing Classification by Timepoint, Concentration Status, and Number of Passing Dilutions'.
                        This contingency table lists the number of passing dilutions in the first column, with the number of subjects
                               having that number of passing dilutions across the rows at each timepoint in the experiment for each concentration status.
                               To view which subjects have a particular number of passing dilutions and concentration status at a particular time point,
                               either double click a cell in the table or select a time point, concentration status, number of dilutions from the dropdown menus below the table titled 'Number of Passing Dilutions',
                               'Select Concentration Status', and 'Select Timepoint' respectively. The color of each cell with patient counts represents how the arbitrary units are treated in the final calculation of the
                               analysis and correspond to the options which are colored by the option selected in the study configuration tab. A legend is provided with this color mapping."),
                       tags$p("Figure 3 is displayed when the subjects who are passing are populated in the passing subjects selector.
                       Figure 3 depicts the plate dilution series, with log10 plate dilution on the x-axis and arbitrary units on the y-axis.
                       All of the subjects who are passing are selected by default and each subject's data is represented by its own line.
                       The color of the points indicates whether the dilution passes classification: blue for passing and red for not passing."),
                       tags$p("Table 2, entitled 'Dilution Analysis Sample Output' displays the sample data for all timepoints for the selected antigen and includes a row for how the
                       processed AU is calculated (au_treatment) which corresponds to the color of the cell, the decision nodes used in the decision tree (decision_nodes),
                       the background method which is selected from study overview in the standard curve options and the processed au (procesed_au).")
                     ),
                     style = "success"
                   )
                 ),
                 mainPanel(
                   uiOutput("parameter_dependencies"),
                   tabsetPanel(
                     id = "dilution_analysis_tabs",  # needed for tab-aware notifications
                     tabPanel(
                       title = "Quality Classification",
                       br(),
                       grVizOutput("decision_tree", width = "75vw"),
                       br(),
                       uiOutput("linear_message"),
                       uiOutput("quantifiable_message"),
                       uiOutput("gate_message"),
                       br(),
                       plotlyOutput("dilution_summary_barplot", width = "75vw"),
                       br(),
                       uiOutput("download_dilution_contingency_summary")
                     ),
                     tabPanel(
                       title = "Subject Level Inspection",
                       fluidRow(
                         column(6, uiOutput("dilution_subject_selector_UI")),
                         column(6, uiOutput("antigen_subject_selector_UI"))
                       ),
                       select_group_ui(
                         id = "da_filters",
                         params = list(
                           list(inputId = "n_pass_dilutions", label = "Number of Passing Dilutions", multiple = FALSE),
                           list(inputId = "concentration_status", label = "Select Concentration Status", multiple = FALSE),
                           timeperiod = list(inputId = "timeperiod", label = "Select Timepoint", multiple = FALSE)
                         )
                       ),
                       uiOutput("passing_subject_selection"),
                       plotlyOutput("patient_dilution_series_plot", width = "75vw")
                     ),
                     tabPanel(
                       title = "Sample Measurement Processing",
                       br(),
                       fluidRow(
                         column(6, uiOutput("dilution_selector_UI")),
                         column(6, uiOutput("antigen_selector_UI"))
                       ),
                       div(style = "overflow-x: auto; width: 75vw;",
                           DT::dataTableOutput("margin_table_antigen")
                       ),
                       div(style = "width: 75vw;", uiOutput("color_legend")),
                       uiOutput("download_classified_sample_UI")
                     ),
                     tabPanel(
                       title = "Dilution Analysis Output Dataset",
                       DT::dataTableOutput("final_average_au_table"),
                       br(),
                       downloadButton("download_average_au_table_UI",
                                      label = "Download Processed Dilution Data")
                     )
                   )
                 )
          )
        )
      )
    })

    # ========================================================================
    # STEP 6: Decision tree
    # ========================================================================
    decision_tree_reactive <- reactive({
      req(dilution_analysis_active())
      req(parsed_config_cache())
      build_decision_tree(parsed_config_cache())
    })

    output$decision_tree <- renderGrViz({
      req(decision_tree_reactive())
      decision_tree <- decision_tree_reactive()
      dot_string <- paste(
        "digraph tree {",
        paste(get_edges(decision_tree), collapse = "; "),
        "}",
        sep = "\n"
      )
      grViz(dot_string)
    })

    # ========================================================================
    # STEP 7: Summary barplot
    # ========================================================================
    output$dilution_summary_barplot <- renderPlotly({
      req(dilution_analysis_active())
      req(gated_data_cache())
      req(parsed_config_cache())

      isolate({
        gated_data_n <- gated_data_cache()

        antigen_family_df <- fetch_antigen_family_df(
          study_accession = selected_study,
          project_id = userWorkSpaceID()
        )

        contigency_summary_dilution <- produce_contigency_summary(gated_data_n)

        summary_dilution_plot(
          dilution_summary_df = contigency_summary_dilution,
          antigen_families = antigen_family_df,
          antigen_family_order = parsed_config_cache()$antigen_family_order
        )
      })
    })

    # ========================================================================
    # STEP 8: Download handlers
    # ========================================================================
    output$download_dilution_contingency_summary <- renderUI({
      req(selected_study, selected_experiment)
      button_label <- paste0("Download Dilution Summary ", selected_experiment, "-", selected_study)
      downloadButton("download_dilution_contingency_summary_handle", button_label)
    })

    output$download_dilution_contingency_summary_handle <- downloadHandler(
      filename = function() {
        paste(selected_study, selected_experiment, "dilution_summary.csv", sep = "_")
      },
      content = function(file) {
        req(dilution_analysis_active())
        req(gated_data_cache())
        contigency_summary_dilution <- produce_contigency_summary(gated_data_cache())
        write.csv(contigency_summary_dilution, file, row.names = FALSE)
      }
    )

    # ========================================================================
    # STEP 9: Selectors
    # ========================================================================

    # Sample Measurement tab selectors
    output$dilution_selector_UI <- renderUI({
      req(dilution_analysis_active())
      req(gated_data_cache())

      gated_data <- gated_data_cache()
      if (nrow(gated_data) == 0) return(NULL)

      selectInput(
        "dilution_da_selector",
        label = "Select Dilutions",
        choices = unique(na.omit(gated_data$dilution)),
        multiple = TRUE,
        selected = unique(na.omit(gated_data$dilution))
      )
    })

    output$antigen_selector_UI <- renderUI({
      req(dilution_analysis_active())
      req(parsed_config_cache())
      req(gated_data_cache())

      antigen_choices <- parsed_config_cache()$antigen_order
      if (all(is.na(antigen_choices))) {
        antigen_choices <- unique(gated_data_cache()$antigen)
      }

      selectInput(
        "antigen_da_selector",
        label = "Select Antigen",
        choices = antigen_choices,
        multiple = FALSE
      )
    })

    # Subject Level Inspection tab selectors
    output$dilution_subject_selector_UI <- renderUI({
      req(dilution_analysis_active())
      req(gated_data_cache())

      gated_data <- gated_data_cache()
      if (nrow(gated_data) == 0) return(NULL)

      selectInput(
        "dilution_subject_selector",
        label = "Select Dilutions",
        choices = unique(na.omit(gated_data$dilution)),
        multiple = TRUE,
        selected = unique(na.omit(gated_data$dilution))
      )
    })

    output$antigen_subject_selector_UI <- renderUI({
      req(dilution_analysis_active())
      req(parsed_config_cache())
      req(gated_data_cache())

      antigen_choices <- parsed_config_cache()$antigen_order
      if (all(is.na(antigen_choices))) {
        antigen_choices <- unique(gated_data_cache()$antigen)
      }

      selectInput(
        "antigen_da_subject_selector",
        label = "Select Antigen",
        choices = antigen_choices,
        multiple = FALSE
      )
    })

    # ========================================================================
    # STEP 10: Filtered standard curve data
    # ========================================================================
    std_curve_data_filtered_rv <- reactive({
      req(loaded_data_cache()$std_curve)
      req(input$antigen_da_selector)

      std_curve <- loaded_data_cache()$std_curve
      std_curve[std_curve$antigen %in% input$antigen_da_selector, ]
    })

    # ========================================================================
    # STEP 11: Classified merged data
    # Always uses all dilutions — each tab filters output independently
    # ========================================================================
    classified_merged_rv <- reactive({
      req(input$advanced_qc_component == "Dilution Analysis")
      req(gated_data_cache())
      req(parsed_config_cache())

      gated_data <- gated_data_cache()
      if (nrow(gated_data) == 0) return(NULL)

      all_dilutions <- unique(na.omit(gated_data$dilution))

      clm <- compute_classified_data(
        gated_data = gated_data,
        selected_dilutions = all_dilutions,
        parsed_config = parsed_config_cache()
      )

      if ("n_pass_d" %in% names(clm)) {
        names(clm)[names(clm) == "n_pass_d"] <- "n_pass_dilutions"
      }

      return(clm)
    })

    # ========================================================================
    # STEP 11a: Sample Measurement tab — filters by dilution_da_selector
    # ========================================================================
    sample_measurement_rv <- reactive({
      req(classified_merged_rv())
      req(input$dilution_da_selector)

      classified_merged_rv()[classified_merged_rv()$dilution %in% input$dilution_da_selector, ]
    })

    # ========================================================================
    # STEP 11b: Subject tab base — filters by subject antigen + dilution selectors
    # Fed into da_filter_module so its dropdowns reflect only relevant options
    # ========================================================================
    subject_tab_base_rv <- reactive({
      req(classified_merged_rv())
      req(input$antigen_da_subject_selector)
      req(input$dilution_subject_selector)

      df <- classified_merged_rv()
      df <- df[df$antigen == input$antigen_da_subject_selector, ]
      df <- df[df$dilution %in% input$dilution_subject_selector, ]
      return(df)
    })

    # ========================================================================
    # STEP 12: Margin table (Sample Measurement tab)
    # Notification shown on render, removed when table is ready
    # ========================================================================
    antigen_margin_table <- reactive({
      req(sample_measurement_rv())
      req(input$antigen_da_selector)
      req(input$dilution_da_selector)
      req(parsed_config_cache())

      cat("\n=== PRODUCING MARGIN TABLE ===\n")
      cat("Selected antigen:", input$antigen_da_selector, "\n")

      produce_margin_table(
        classified_merged = sample_measurement_rv(),
        selectedAntigen = input$antigen_da_selector,
        selectedDilutions = input$dilution_da_selector,
        time_order = parsed_config_cache()$timeperiod_order
      )
    })

    output$margin_table_antigen <- renderDataTable({
      req(dilution_analysis_active())
      req(antigen_margin_table())

      # Show notification only when user is on this tab
      if (isTRUE(input$dilution_analysis_tabs == "Sample Measurement Proccessing")) {
        showNotification(
          id = "load_sample_measurement",
          HTML("Loading Sample Measurement Processing<span class='dots'>"),
          duration = NULL,
          type = "message"
        )
      }

      timeperiod_colors <- names(antigen_margin_table())[grepl(pattern = "color", names(antigen_margin_table()))]

      dt <- DT::datatable(
        antigen_margin_table(),
        caption = 'Number of Subjects Passing Classification by Timepoint, Concentration Status, and Number of Acceptable Dilutions',
        selection = list(target = 'cell', mode = "single"),
        rownames = FALSE,
        options = list(
          columnDefs = list(list(visible = FALSE, targets = timeperiod_colors)),
          rowCallback = JS(
            'function(row, data, index) {
                $("td:nth-child(1)", row).css("background-color", "#f2f2f2");
                $("td:nth-child(1)", row).addClass("disabled");
                $("td:nth-child(1)", row).on("click", function(e) {
                  e.stopPropagation();
                  return false;
                });
                $("td:nth-child(2)", row).css("background-color", "#f2f2f2");
                $("td:nth-child(2)", row).addClass("disabled");
                $("td:nth-child(2)", row).on("click", function(e) {
                  e.stopPropagation();
                  return false;
                });
                if (data[0] === "Total") {
                  $(row).find("td").css("background-color", "#f2f2f2");
                  $(row).find("td").addClass("disabled");
                  $(row).find("td").on("click", function(e) {
                    e.stopPropagation();
                    return false;
                  });
                }
              }'
          )
        )
      )

      id_cols <- c("Number of Acceptable Dilutions", "Concentration Status")
      timeperiods <- setdiff(names(antigen_margin_table()), c(id_cols, timeperiod_colors))
      timeperiod_colors <- timeperiod_colors[match(timeperiods, sub("_color$", "", timeperiod_colors))]

      for(i in 1:length(timeperiods)) {
        dil_col <- timeperiods[i]
        dil_color <- timeperiod_colors[i]

        dt <- dt %>% formatStyle(
          dil_col,
          valueColumns = dil_color,
          backgroundColor = styleEqual(
            c("all_au", "passing_au", "geom_all_au", "geom_passing_au",
              "replace_blank", "replace_positive_control", "exclude_au", "Blank"),
            c("#a1caf1", "lightgrey", '#c2b280', '#875692', '#008856',
              '#dcd300', '#b3446c', 'white')
          )
        )
      }

      removeNotification(id = "load_sample_measurement")
      return(dt)
    })

    output$color_legend <- renderUI({
      div(
        style = "margin-bottom: 15px;",
        tags$b("Cell Color Legend:  "),
        tags$span(style = "display: inline-block; width: 15px; height: 15px; background-color: #a1caf1; border: 1px solid black; margin-right: 5px;"),
        "Keep all measurements",
        tags$span(style = "display: inline-block; width: 15px; height: 15px; background-color: lightgrey; border: 1px solid black; margin: 0 5px 0 15px;"),
        "Keep acceptable measurements, i.e. within the sample limits selected",
        tags$span(style = "display: inline-block; width: 15px; height: 15px; background-color: #c2b280; border: 1px solid black; margin: 0 5px 0 15px;"),
        "Geometric mean of all measurements",
        tags$span(style = "display: inline-block; width: 15px; height: 15px; background-color: #875692; border: 1px solid black; margin: 0 5px 0 15px;"),
        "Geometric mean of acceptable measurements",
        tags$span(style = "display: inline-block; width: 15px; height: 15px; background-color: #008856; border: 1px solid black; margin: 0 5px 0 15px;"),
        "Replace measurements with geometric mean of blank control measurements",
        tags$span(style = "display: inline-block; width: 15px; height: 15px; background-color: #b3446c; border: 1px solid black; margin: 0 5px 0 15px;"),
        "Exclude measurements",
        tags$span(style = "display: inline-block; width: 15px; height: 15px; background-color: white; border: 1px solid black; margin: 0 5px 0 15px;"),
        "No Subjects Present. No processing rule applied."
      )
    })

    # ========================================================================
    # STEP 13: Subject level inspection
    # Notification shown on render, removed when plot is ready
    # ========================================================================
    da_filter_module <- datamods::select_group_server(
      id = "da_filters",
      data = subject_tab_base_rv,
      vars = c("n_pass_dilutions", "concentration_status", "timeperiod"),
      selected_r = reactive(NULL)
    )

    filtered_classified_merged <- reactive({
      req(dilution_analysis_active())
      req(da_filter_module())

      df <- da_filter_module()

      if (!is.null(da_filter_module()$n_pass_dilutions)) {
        df <- df[df$n_pass_dilutions %in% unique(da_filter_module()$n_pass_dilutions), ]
      }
      if (!is.null(da_filter_module()$concentration_status)) {
        df <- df[df$concentration_status %in% unique(da_filter_module()$concentration_status), ]
      }
      if (!is.null(da_filter_module()$timeperiod)) {
        df <- df[df$timeperiod %in% unique(da_filter_module()$timeperiod), ]
      }

      return(df)
    })

    output$passing_subject_selection <- renderUI({
      req(dilution_analysis_active())
      req(input$antigen_da_subject_selector)
      req(filtered_classified_merged())

      choices_string <- unique(filtered_classified_merged()$patientid)

      selectInput(
        "Passing_subjects_da",
        label = "Select Subjects",
        choices = choices_string,
        multiple = TRUE,
        selected = choices_string,
        width = "75vw"
      )
    })

    output$patient_dilution_series_plot <- renderPlotly({
      req(dilution_analysis_active())
      req(filtered_classified_merged())
      req(input$antigen_da_subject_selector)
      req(input$Passing_subjects_da)
      req(input$dilution_subject_selector)

      # Show notification only when user is on this tab
      if (isTRUE(input$dilution_analysis_tabs == "Subject Level Inspection")) {
        showNotification(
          id = "load_subject_inspection",
          HTML("Loading Subject Level Inspection<span class='dots'>"),
          duration = NULL,
          type = "message"
        )
      }

      selected_timepoint <- as.character(unique(filtered_classified_merged()$timeperiod))
      selected_status <- unique(filtered_classified_merged()$concentration_status)

      p <- plot_patient_dilution_series(
        sample_data = filtered_classified_merged(),
        selectedAntigen = input$antigen_da_subject_selector,
        selectedPatient = input$Passing_subjects_da,
        selectedTimepoints = selected_timepoint,
        selectedDilutions = input$dilution_subject_selector
      )

      removeNotification(id = "load_subject_inspection")
      return(p)
    })

    # ========================================================================
    # STEP 14: Final output table
    # ========================================================================
    final_average_au_table_rv <- reactiveVal(NULL)

    output$final_average_au_table <- renderDT({
      req(dilution_analysis_active())
      req(sample_measurement_rv())
      req(parsed_config_cache())

      std_curve_filtered <- NULL
      if (!is.null(loaded_data_cache()$std_curve) && !is.null(input$antigen_da_selector)) {
        std_curve_filtered <- loaded_data_cache()$std_curve[
          loaded_data_cache()$std_curve$antigen %in% input$antigen_da_selector,
        ]
      }

      final_table <- process_final_au_table(
        classified_data = sample_measurement_rv(),
        std_curve_filtered = std_curve_filtered,
        controls = loaded_data_cache()$controls,
        parsed_config = parsed_config_cache(),
        selected_antigen = input$antigen_da_selector,
        table_columns = DILUTION_ANALYSIS_CONFIG$table_columns
      )

      final_average_au_table_rv(final_table)

      DT::datatable(
        final_table,
        caption = "Dilution Analysis Sample Output",
        colnames = colnames(final_table),
        filter = "top",
        options = list(scrollX = TRUE, autoWidth = FALSE)
      )
    })

    observe({
      req(final_average_au_table_rv())
      isolate({
        final_average_df <- final_average_au_table_rv()
        save_average_au(conn, final_average_df, DILUTION_ANALYSIS_CONFIG$table_columns)
      })
    })

    # ========================================================================
    # STEP 15: Download buttons
    # ========================================================================
    output$download_classified_sample_UI <- renderUI({
      req(dilution_analysis_active())
      req(sample_measurement_rv())
      req(selected_study, selected_experiment)

      button_label <- paste0("Download Classified Sample Data: ",
                             selected_experiment, "-", selected_study)
      downloadButton("download_classifed_sample_handle", button_label)
    })

    output$download_classifed_sample_handle <- downloadHandler(
      filename = function() {
        paste(selected_study, selected_experiment, "classified_sample.csv", sep = "_")
      },
      content = function(file) {
        req(dilution_analysis_active())
        req(sample_measurement_rv())
        write.csv(sample_measurement_rv(), file, row.names = FALSE)
      }
    )

    output$download_average_au_table_UI <- downloadHandler(
      filename = function() {
        paste(selected_study, selected_experiment, "sample_data_processed.csv", sep = "_")
      },
      content = function(file) {
        req(dilution_analysis_active())
        req(final_average_au_table_rv())
        write.csv(final_average_au_table_rv(), file, row.names = FALSE)
      }
    )

    # ========================================================================
    # STEP 16: Parameter dependencies & navigation
    # ========================================================================
    output$parameter_dependencies <- renderUI({
      req(dilution_analysis_active())
      tagList(
        bsCollapse(
          id = "param_dependencies",
          bsCollapsePanel(
            title = "Parameter Dependencies",
            HTML(
              "The decision tree is dependent on the selection of the type of sample limit and the order in which they are chosen.<br>
                 In addition, the method selected for Blank Control influences the classification of the concentration status, as parameter estimates can vary depending on how the blanks
                 are treated when fitting standard curves.<br>
                 The final treatment of the arbitrary units is dependent on the selection chosen corresponding to the treatment for the combination of number of passing dilutions and concentration status.<br>
                 The order of the timeperiods in the table 'Number of Subjects Passing Classification by Timepoint, Concentration Status, and Number of Passing Dilutions' is ordered by the timeperiod order set in the study parameters.<br>
                 For dilutional linearity when the response is MFI the determination of if it is log10 of MFI or raw MFI is made in the study configuration for use log units for MFI control."
            ),
            style = "info"
          )
        ),
        actionButton("to_study_parameters", label = "Return to Change Study Settings")
      )
    })

    observeEvent(input$to_study_parameters, {
      updateTabItems(session, inputId = "study_tabs", selected = "study_settings")
    })

  }
) # End main observeEvent

# ============================================================================
# DEBUG: Monitor tab changes
# ============================================================================
observeEvent(list(
  input$study_level_tabs,
  input$advanced_qc_component,
  input$readxMap_experiment_accession
), {
  cat("\n=== TAB/CONTEXT CHANGE DETECTED ===\n")
  cat("study_level_tabs:", input$study_level_tabs, "\n")
  cat("advanced_qc_component:", input$advanced_qc_component, "\n")
  cat("experiment:", input$readxMap_experiment_accession, "\n")
  cat("dilution_analysis_active:", dilution_analysis_active(), "\n")
  cat("current_context:", current_context(), "\n")
  cat("current_config_signature:", current_config_signature(), "\n\n")
  if (input$readxMap_experiment_accession == "Click here") {
    clear_all_da_notifications()
    return()
  }
}, ignoreInit = TRUE)
