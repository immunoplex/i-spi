# =============================================================================
# flowjo_reader.R
# Server module: Post-gating Flow Cytometry (FlowJo) import pipeline
#
# Sourced inside the gated observe() block in app.R, so all reactive values
# declared there are in scope.
#
# Pipeline stages mirrored from the bead-array workflow:
#   1. Upload & parse raw FlowJo .xlsx  → flowjo_data_state_rv
#   2. Download generated layout template (download handler)
#   3. Upload completed layout template  → validate → flowjo_validation_rv
#   4. View any layout sheet in a DT table
#   5. Upload validated data to database
#
# Requires in app.R (see app.R additions):
#   flowjo_data_state_rv  <- reactiveVal(list(flowjo_long=NULL, dilutions=NULL, source_file=NULL))
#   flowjo_layout_sheets_rv <- reactiveVal(list())
#   flowjo_validation_rv  <- reactiveVal(list(...))
#
# Requires sourced:
#   flowjo_read_functions.R          (load_flowjo_file, pivot_flowjo_long)
#   generate_flowjo_layout_template.R
# =============================================================================

source("flowjo_read_functions.R",           local = TRUE)
source("generate_flowjo_layout_template.R", local = TRUE)


# ── REACTIVE FLAGS used by conditionalPanel() JS expressions ──────────────────

output$hasFlowjoFile <- reactive({
  state <- flowjo_data_state_rv()
  isTRUE(!is.null(state$flowjo_long) && nrow(state$flowjo_long) > 0)
})
outputOptions(output, "hasFlowjoFile", suspendWhenHidden = FALSE)

output$hasFlowjoLayoutSheets <- reactive({
  sheets <- flowjo_layout_sheets_rv()
  isTRUE(length(sheets) > 0 && "plates_map" %in% names(sheets))
})
outputOptions(output, "hasFlowjoLayoutSheets", suspendWhenHidden = FALSE)


# ── UI output – injected into the conditionalPanel in import_lumifile.R ───────
#
# Rendered once the study/experiment context is set.
# Mirrors the ELISA two-column layout: left=controls, right=display.

output$flowjo_import_ui <- renderUI({
  req(input$readxMap_study_accession)
  req(input$readxMap_study_accession != "Click here")

  fluidRow(

    # ── LEFT PANEL: upload controls ──────────────────────────────────────────
    column(3,
      wellPanel(
        h4(icon("filter"), " FlowJo Post-Gating Import"),
        p(style = "font-size:13px; color:#555;",
          "Upload the FlowJo MFI output (.xlsx) with ",
          code("Sheet1"), " and ", code("dilutions"), " tabs.",
          " The 384-well plate is treated as 4 × 96-well subplates."),

        # ── Step 1 ──────────────────────────────────────────────────────────
        tags$div(
          class = "element-controls",
          tags$span(style = "font-weight:600;", "Step 1 — Raw FlowJo file"),
          fileInput("upload_flowjo_file",
                    label    = NULL,
                    accept   = c(".xlsx", ".xls"),
                    multiple = FALSE,
                    placeholder = "MFI-values_OPT_3.1_4.1.xlsx")
        ),

        # ── Step 2: feature label + generate template ────────────────────────
        conditionalPanel(
          condition = "output.hasFlowjoFile",
          hr(),
          tags$div(
            class = "element-controls",
            tags$span(style = "font-weight:600;", "Step 2 — Feature label"),
            textInput("flowjo_feature_value",
                      label       = NULL,
                      value       = "MFI",
                      placeholder = "e.g. MFI, Total_MFI (\u226415 chars)")
          ),

          # Warn clearly if experiment hasn't been selected yet
          conditionalPanel(
            condition = paste0("input.readxMap_experiment_accession_import == 'Click here'",
                               " || input.readxMap_experiment_accession_import == ''",
                               " || input.readxMap_experiment_accession_import == null"),
            div(class = "alert alert-warning", style = "padding:8px; margin-bottom:6px;",
                icon("exclamation-triangle"),
                " Select an ", strong("Experiment"), " above before generating the template.")
          ),

          downloadButton("flowjo_blank_layout_file",
                         label = tagList(icon("download"),
                                         " Generate FlowJo Layout Template"),
                         class = "btn-info btn-block"),
          p(style = "font-size:12px; color:#777; margin-top:6px;",
            "Download \u2192 review/edit \u2192 re-upload below."),

          # \u2500\u2500 Step 3: upload completed layout \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
          hr(),
          tags$span(style = "font-weight:600;",
                    "Step 3 \u2014 Upload completed layout file"),
          fileInput("upload_flowjo_layout_file",
                    label    = NULL,
                    accept   = c(".xlsx", ".xls"),
                    multiple = FALSE)
        )
      )
    ),

    # ── RIGHT PANEL: feedback displays ──────────────────────────────────────
    column(9,

      # Context banner (study / experiment)
      conditionalPanel(
        condition = paste0("input.readxMap_experiment_accession_import != 'Click here'",
                           " && input.readxMap_experiment_accession_import != ''"),
        div(
          style = paste0("background-color:#f0f8ff; border:1px solid #4a90e2;",
                         " padding:10px; margin-bottom:15px; border-radius:5px;"),
          tags$h4("Current Import Context",
                  style = "margin-top:0; color:#2c5aa0;"),
          textOutput("flowjo_context_display")
        )
      ),

      # Parse summary (shown after step 1)
      conditionalPanel(
        condition = "output.hasFlowjoFile",
        wellPanel(
          h4("FlowJo File Summary"),
          verbatimTextOutput("flowjo_file_summary")
        )
      ),

      # Validation status (step 3 feedback)
      uiOutput("flowjo_validation_status"),

      # Layout sheet viewer + upload-to-DB button (step 3+)
      conditionalPanel(
        condition = "output.hasFlowjoLayoutSheets",
        wellPanel(
          h4("Layout File Contents"),
          fluidRow(
            column(2, actionButton("view_flowjo_plate_id",   "Plate ID",    class = "btn-sm btn-default")),
            column(2, actionButton("view_flowjo_plates_map", "Plates Map",  class = "btn-sm btn-default")),
            column(2, actionButton("view_flowjo_subjects",   "Subjects",    class = "btn-sm btn-default")),
            column(2, actionButton("view_flowjo_timepoint",  "Timepoint",   class = "btn-sm btn-default")),
            column(2, actionButton("view_flowjo_antigens",   "Antigens",    class = "btn-sm btn-default")),
            column(2, actionButton("view_flowjo_assay_resp", "Assay Resp.", class = "btn-sm btn-default"))
          ),
          br(),
          DT::dataTableOutput("flowjo_layout_sheet_table")
        )
      ),

      # Upload-to-DB button (conditional on validation)
      uiOutput("upload_flowjo_batch_button_ui")
    )
  )
})


# ── Context banner text ────────────────────────────────────────────────────────

output$flowjo_context_display <- renderText({
  req(input$readxMap_study_accession)
  req(input$readxMap_experiment_accession_import)
  paste0(
    "Project ID: ", userWorkSpaceID(), "  |  ",
    "Study: ",      input$readxMap_study_accession, "  |  ",
    "Experiment: ", input$readxMap_experiment_accession_import
  )
})


# =============================================================================
# STAGE 1 — Parse raw FlowJo file
# =============================================================================

observeEvent(input$upload_flowjo_file, {
  req(input$upload_flowjo_file)

  # Study must be set; experiment is NOT required here — the file parse is
  # independent of experiment context.  Experiment is required only at the
  # template-generation and DB-upload steps.
  if (is.null(input$readxMap_study_accession) ||
      input$readxMap_study_accession %in% c("", "Click here")) {
    showNotification(
      "Please select a Study before uploading a FlowJo file.",
      type = "warning", duration = 6
    )
    return()
  }

  experiment_set <- !is.null(input$readxMap_experiment_accession_import) &&
    nzchar(input$readxMap_experiment_accession_import) &&
    input$readxMap_experiment_accession_import != "Click here"

  cat("\n╔══════════════════════════════════════════════════════════╗\n")
  cat("║         PARSING FLOWJO FILE                              ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n")
  cat("Study     :", input$readxMap_study_accession, "\n")
  cat("Experiment:", if (experiment_set) input$readxMap_experiment_accession_import else "(not yet selected)", "\n")

  # Reset all downstream state
  flowjo_data_state_rv(list(flowjo_long = NULL, dilutions = NULL, source_file = NULL))
  flowjo_layout_sheets_rv(list())
  flowjo_validation_rv(list(
    is_validated    = FALSE, is_uploaded = FALSE,
    validation_time = NULL,  upload_time = NULL,
    metadata_result = NULL,  bead_array_result = NULL
  ))

  f <- input$upload_flowjo_file
  cat("File:", f$name, "\n\n")

  withProgress(message = "Parsing FlowJo file…", value = 0.2, {

    tryCatch({

      # Parse raw file
      result    <- load_flowjo_file(f$datapath)
      flowjo_df <- result$flowjo_df
      dilutions <- result$dilutions

      # Override source_file with the original uploaded filename (f$name).
      # load_flowjo_file() sets source_file = basename(f$datapath) which is
      # a Shiny temp name like '0.xlsx', making plateid = '0_plate_1'.
      # Using f$name gives 'MFI-values_OPT_3.1_4.1_dilute_plate_1' instead.
      flowjo_df$source_file <- f$name

      # Attach app context — experiment may not be set yet at parse time; that is OK.
      # It will be required (and validated) at template-generation and DB-upload time.
      flowjo_df$project_id      <- userWorkSpaceID()
      flowjo_df$study_name      <- input$readxMap_study_accession
      flowjo_df$experiment_name <- if (experiment_set)
        input$readxMap_experiment_accession_import else NA_character_

      setProgress(0.6, message = "Pivoting to long format…")
      flowjo_long <- pivot_flowjo_long(flowjo_df, dilutions)

      flowjo_data_state_rv(list(
        flowjo_long     = flowjo_long,
        dilutions       = dilutions,
        source_file     = f$name,
        source_filepath = f$datapath   # full temp path — used as file_name in plate_id sheet
      ))

      setProgress(1.0, message = "Done")

      n_plates <- length(unique(flowjo_long$plate))
      n_ab     <- length(unique(flowjo_long$antibody))
      n_rows   <- nrow(flowjo_long)

      cat("✓ Parsed", n_rows, "rows,", n_plates, "subplates,", n_ab, "antibodies\n")
      cat("╚══════════════════════════════════════════════════════════╝\n\n")

      showNotification(
        paste0("FlowJo file parsed: ", n_plates, " subplates × ",
               n_ab, " antibodies (", n_rows, " observations)."),
        type = "message", duration = 6
      )

    }, error = function(e) {
      cat("✗ Error:", e$message, "\n")
      cat("╚══════════════════════════════════════════════════════════╝\n\n")
      showNotification(
        paste("Error parsing FlowJo file:", e$message),
        type = "error", duration = NULL
      )
    })
  })
})


# ── Parse summary text ────────────────────────────────────────────────────────

output$flowjo_file_summary <- renderPrint({
  state <- flowjo_data_state_rv()
  req(!is.null(state$flowjo_long))
  fl <- state$flowjo_long

  tp_col <- if ("timepoint"  %in% names(fl)) "timepoint"
            else if ("timeperiod" %in% names(fl)) "timeperiod"
            else NULL

  cat("File       :", state$source_file, "\n")
  cat("Subplates  :", paste(sort(unique(fl$plate)),    collapse = ", "), "\n")
  cat("Antibodies :", paste(sort(unique(fl$antibody)), collapse = ", "), "\n")
  cat("Antigen(s) :", paste(sort(unique(fl$antigen)),  collapse = ", "), "\n")
  cat("Total rows :", nrow(fl), "\n")
  cat("\nSample-type counts:\n")
  print(table(fl$stype, useNA = "ifany"))

  if (!is.null(tp_col)) {
    cat("\nTimepoints (X samples only):\n")
    print(table(fl[[tp_col]][fl$stype == "X"], useNA = "ifany"))
  }

  cat("\nDilution factors per antibody (X samples):\n")
  dil_tbl <- fl %>%
    dplyr::filter(stype == "X") %>%
    dplyr::group_by(antibody) %>%
    dplyr::summarise(
      dilution_factor = paste(sort(unique(dilution_factor[!is.na(dilution_factor)])),
                              collapse = " | "),
      .groups = "drop"
    )
  print(as.data.frame(dil_tbl))
})


# =============================================================================
# STAGE 2 — Download generated layout template
# =============================================================================

output$flowjo_blank_layout_file <- downloadHandler(

  filename = function() {
    paste0(input$readxMap_study_accession, "_",
           input$readxMap_experiment_accession_import,
           "_flowjo_layout_template.xlsx")
  },

  content = function(file) {
    state <- flowjo_data_state_rv()
    req(!is.null(state$flowjo_long))

    # Experiment MUST be set before generating the template
    exp_val <- input$readxMap_experiment_accession_import
    if (is.null(exp_val) || !nzchar(exp_val) || exp_val == "Click here") {
      showNotification(
        "Please select an Experiment before generating the layout template.",
        type = "warning", duration = 8
      )
      req(FALSE)   # abort download gracefully
    }

    feature_val <- trimws(input$flowjo_feature_value %||% "MFI")
    if (!nzchar(feature_val) || feature_val %in% c("Up to 15 chars", "e.g. MFI, Total_MFI (\u226415 chars)")) {
      feature_val <- "MFI"
    }
    feature_val <- substr(feature_val, 1, 15)

    # Re-stamp experiment onto the long data now that it is known
    flowjo_long_stamped <- state$flowjo_long
    flowjo_long_stamped$experiment_name <- exp_val

    cat("\n=== Generating FlowJo layout template ===\n")
    cat("  Study:     ", input$readxMap_study_accession, "\n")
    cat("  Experiment:", exp_val, "\n")
    cat("  Feature:   ", feature_val, "\n")

    withProgress(message = "Generating layout template\u2026", value = 0.5, {
      tryCatch({
        generate_flowjo_layout_template(
          flowjo_long     = flowjo_long_stamped,
          dilutions       = state$dilutions,
          project_id      = userWorkSpaceID(),
          study_name      = input$readxMap_study_accession,
          experiment_name = exp_val,
          output_file     = file,
          source_filepath = state$source_filepath,
          feature         = feature_val
        )
        setProgress(1.0, message = "Done")
        cat("  \u2713 Template saved\n")
      }, error = function(e) {
        showNotification(
          paste("Error generating layout template:", e$message),
          type = "error", duration = NULL
        )
        stop(e)
      })
    })
  }
)


# =============================================================================
# STAGE 3 — Upload completed layout file → validate
# =============================================================================

observeEvent(input$upload_flowjo_layout_file, {
  req(input$upload_flowjo_layout_file)

  # Both study AND experiment must be set before a layout file can be validated
  if (is.null(input$readxMap_study_accession) ||
      input$readxMap_study_accession %in% c("", "Click here")) {
    showNotification("Please select a Study before uploading the layout file.",
                     type = "warning", duration = 6)
    return()
  }
  exp_val <- input$readxMap_experiment_accession_import
  if (is.null(exp_val) || !nzchar(exp_val) || exp_val == "Click here") {
    showNotification(
      "Please select an Experiment before uploading the layout file.",
      type = "warning", duration = 6
    )
    return()
  }

  cat("\n╔══════════════════════════════════════════════════════════╗\n")
  cat("║         UPLOADING FLOWJO LAYOUT FILE                     ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n")

  # Reset layout and validation state, keep parsed data
  flowjo_layout_sheets_rv(list())
  flowjo_validation_rv(list(
    is_validated    = FALSE, is_uploaded = FALSE,
    validation_time = NULL,  upload_time = NULL,
    metadata_result = NULL,  bead_array_result = NULL
  ))

  f <- input$upload_flowjo_layout_file
  cat("Layout file:", f$name, "\n\n")

  # Check required sheet names
  sheet_check <- check_sheet_names(f$datapath, exact_match = FALSE)
  if (!sheet_check$valid) {
    showNotification(sheet_check$message, type = "error", duration = NULL)
    cat("✗ Sheet name check failed\n")
    return()
  }

  # Read all sheets
  all_sheets <- import_layout_file(f$datapath)
  if (!all_sheets$success) {
    showNotification(
      paste(all_sheets$messages, collapse = "\n"),
      type = "error", duration = NULL
    )
    cat("✗ import_layout_file() failed\n")
    return()
  }

  sheets <- all_sheets$data

  # Require the four sheets critical for FlowJo data
  required <- c("plates_map", "plate_id", "antigen_list", "assay_response_long")
  missing  <- setdiff(required, names(sheets))
  if (length(missing) > 0) {
    showNotification(
      paste("Missing required sheets:", paste(missing, collapse = ", ")),
      type = "error", duration = NULL
    )
    cat("\u2717 Missing sheets:", paste(missing, collapse = ", "), "\n")
    return()
  }

  # Cross-check: the study/experiment embedded in the layout file should match
  # the current UI selection.
  layout_study <- unique(sheets[["plates_map"]]$study_name)
  layout_exp   <- unique(sheets[["plates_map"]]$experiment_name)
  if (!any(input$readxMap_study_accession == layout_study, na.rm = TRUE)) {
    showNotification(
      paste0("Study mismatch: UI shows '", input$readxMap_study_accession,
             "' but layout file contains '", paste(layout_study, collapse = "/"), "'."),
      type = "warning", duration = 10
    )
  }
  if (!any(exp_val == layout_exp, na.rm = TRUE)) {
    showNotification(
      paste0("Experiment mismatch: UI shows '", exp_val,
             "' but layout file contains '", paste(layout_exp, collapse = "/"), "'."),
      type = "warning", duration = 10
    )
  }

  flowjo_layout_sheets_rv(sheets)

  plates_map          <- sheets[["plates_map"]]
  plate_id_sheet      <- sheets[["plate_id"]]
  antigen_list        <- sheets[["antigen_list"]]
  assay_response_long <- sheets[["assay_response_long"]]

  cat("  plate_id rows:            ", nrow(plate_id_sheet), "\n")
  cat("  plates_map rows:          ", nrow(plates_map), "\n")
  cat("  antigen_list rows:        ", nrow(antigen_list), "\n")
  cat("  assay_response_long rows: ", nrow(assay_response_long), "\n\n")

  # ── Validate ──────────────────────────────────────────────────────────────
  meta_result  <- validate_batch_plate_metadata(
    plate_metadata = plate_id_sheet,
    plate_id_data  = plate_id_sheet
  )
  assay_result <- validate_assay_response_data(
    assay_response_long = assay_response_long,
    antigen_import_list = antigen_list,
    plates_map          = plates_map
  )

  both_valid <- isTRUE(meta_result$is_valid) && isTRUE(assay_result$is_valid)

  flowjo_validation_rv(list(
    is_validated    = both_valid,
    is_uploaded     = FALSE,
    validation_time = Sys.time(),
    upload_time     = NULL,
    metadata_result = meta_result,
    bead_array_result = assay_result
  ))

  if (both_valid) {
    cat("✓ VALIDATION PASSED\n")
    showNotification(
      "FlowJo layout validated — ready to upload to database.",
      type = "message", duration = 5
    )
  } else {
    cat("✗ VALIDATION FAILED\n")
    cat("  Metadata valid:   ", meta_result$is_valid, "\n")
    cat("  Assay data valid: ", assay_result$is_valid, "\n")
    showNotification(
      "Layout validation failed. Review errors below.",
      type = "error", duration = 10
    )
  }

  cat("╚══════════════════════════════════════════════════════════╝\n\n")
})


# ── Validation status banner ──────────────────────────────────────────────────

output$flowjo_validation_status <- renderUI({
  sheets <- flowjo_layout_sheets_rv()
  if (length(sheets) == 0) return(NULL)

  v <- flowjo_validation_rv()

  if (isTRUE(v$is_uploaded)) {
    div(class = "alert alert-success",
        icon("check-circle"),
        strong(" Uploaded to database"),
        paste0(" at ", format(v$upload_time, "%Y-%m-%d %H:%M:%S")))

  } else if (isTRUE(v$is_validated)) {
    div(class = "alert alert-info",
        icon("check"),
        strong(" Validated"),
        paste0(" at ", format(v$validation_time, "%Y-%m-%d %H:%M:%S")),
        " — ready to upload to database.")

  } else {
    issues <- c(
      if (!is.null(v$metadata_result)   && !isTRUE(v$metadata_result$is_valid))
        paste("Metadata:", v$metadata_result$message),
      if (!is.null(v$bead_array_result) && !isTRUE(v$bead_array_result$is_valid))
        paste("Assay data:", v$bead_array_result$message)
    )
    div(
      class = "alert alert-danger",
      icon("times-circle"), strong(" Validation failed"),
      if (length(issues) > 0) tags$ul(lapply(issues, tags$li))
    )
  }
})


# =============================================================================
# STAGE 4 — Sheet viewer
# =============================================================================

flowjo_active_sheet_view <- reactiveVal(NULL)

observeEvent(input$view_flowjo_plate_id,   { flowjo_active_sheet_view("plate_id") })
observeEvent(input$view_flowjo_plates_map, { flowjo_active_sheet_view("plates_map") })
observeEvent(input$view_flowjo_subjects,   { flowjo_active_sheet_view("subject_groups") })
observeEvent(input$view_flowjo_timepoint,  { flowjo_active_sheet_view("timepoint") })
observeEvent(input$view_flowjo_antigens,   { flowjo_active_sheet_view("antigen_list") })
observeEvent(input$view_flowjo_assay_resp, { flowjo_active_sheet_view("assay_response_long") })

output$flowjo_layout_sheet_table <- DT::renderDataTable({
  sheet_name <- req(flowjo_active_sheet_view())
  sheets     <- flowjo_layout_sheets_rv()
  df         <- req(sheets[[sheet_name]])

  DT::datatable(
    df,
    options  = list(scrollX = TRUE, pageLength = 10, dom = "lrtip"),
    rownames = FALSE,
    caption  = htmltools::tags$caption(
      style = "color:#333; font-weight:bold;",
      paste("Sheet:", sheet_name)
    )
  )
})


# =============================================================================
# STAGE 5 — Upload to database
# =============================================================================

# Conditional "Upload to DB" button
output$upload_flowjo_batch_button_ui <- renderUI({
  v <- flowjo_validation_rv()
  if (!isTRUE(v$is_validated)) return(NULL)

  if (isTRUE(v$is_uploaded)) {
    div(class = "alert alert-success",
        icon("database"),
        " FlowJo data has already been uploaded to the database.")
  } else {
    wellPanel(
      h4("Upload to Database"),
      p("Layout validated. Click to load FlowJo data into the database."),
      actionButton(
        "upload_flowjo_batch_button",
        label = tagList(icon("database"), " Upload FlowJo Data to Database"),
        class = "btn-success btn-block"
      )
    )
  }
})


observeEvent(input$upload_flowjo_batch_button, {

  cat("\n╔══════════════════════════════════════════════════════════╗\n")
  cat("║         UPLOADING FLOWJO BATCH TO DATABASE               ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n")

  v <- flowjo_validation_rv()
  if (!isTRUE(v$is_validated)) {
    showNotification("Validate the layout file first.", type = "error", duration = 5)
    return(NULL)
  }
  if (isTRUE(v$is_uploaded)) {
    showNotification("Batch already uploaded.", type = "warning", duration = 5)
    return(NULL)
  }

  sheets <- flowjo_layout_sheets_rv()
  if (is.null(sheets[["assay_response_long"]])) {
    showNotification(
      "assay_response_long not found — re-upload the layout file.",
      type = "error", duration = 10
    )
    return(NULL)
  }

  # Pull sheets
  assay_response_raw  <- sheets[["assay_response_long"]]
  plates_map          <- sheets[["plates_map"]]
  plate_id_sheet      <- sheets[["plate_id"]]
  antigen_list        <- sheets[["antigen_list"]]
  subject_map         <- sheets[["subject_groups"]]
  timepoint_map       <- sheets[["timepoint"]]

  project_id           <- userWorkSpaceID()
  workspace_id         <- userWorkSpaceID()
  auth0_user           <- currentuser()
  study_accession      <- unique(plates_map$study_name)[1]
  experiment_accession <- unique(plates_map$experiment_name)[1]

  cat("  Study:      ", study_accession, "\n")
  cat("  Experiment: ", experiment_accession, "\n")
  cat("  ARL rows:   ", nrow(assay_response_raw), "\n")
  cat("  Map rows:   ", nrow(plates_map), "\n\n")

  # Rename flow-specific columns to match the bead-array DB schema
  # pct_agg → pctaggbeads  (same role as % Agg Beads in bead arrays)
  assay_response <- assay_response_raw
  if ("pct_agg" %in% names(assay_response)) {
    names(assay_response)[names(assay_response) == "pct_agg"] <- "pctaggbeads"
    cat("  → Renamed pct_agg → pctaggbeads\n")
  }

  # plate_ids used per-feature inside the upload loop
  plate_ids <- unique(plate_id_sheet$plate_id)

  col_mapping <- create_column_mapping()
  natural_key <- c("study_name", "experiment_name", "plateid", "well")

  # Local helper: fill mandatory sampleid
  fill_missing_sampleid <- function(df, specimen_type = c("X", "S", "C")) {
    specimen_type <- match.arg(specimen_type)
    if (!"sampleid" %in% names(df)) df$sampleid <- NA_character_
    needs_fill <- is.na(df$sampleid) | trimws(df$sampleid) == ""
    if (!any(needs_fill)) return(df)
    if (specimen_type == "X") {
      if ("patientid" %in% names(df))
        df$sampleid[needs_fill] <- df$patientid[needs_fill]
    } else {
      if ("dilution" %in% names(df))
        df$sampleid[needs_fill] <- as.character(df$dilution[needs_fill])
    }
    still_empty <- is.na(df$sampleid) | trimws(df$sampleid) == ""
    if (any(still_empty) && "well" %in% names(df))
      df$sampleid[still_empty] <- df$well[still_empty]
    df
  }

  # Intersect of assay columns available in this layout
  # assay_response_long from flowjo templates uses "feature" (isotype) as the
  # analyte column; bead-array templates use "antigen".  Include both so the
  # intersect() picks up whichever is actually present.
  assay_cols <- intersect(
    c(natural_key, "feature", "antigen", "assay_response", "assay_bead_count", "pctaggbeads"),
    names(assay_response)
  )

      # Extract dilutions reference for per-feature dilution resolution
    flowjo_dilutions_ref <- flowjo_data_state_rv()$dilutions

    # Resolve pipe-separated 'dilution' (e.g. "500|1000") to a single numeric
    # per row using the feature column to look up the correct dilution factor.
    resolve_feature_dilution <- function(df, dil_ref) {
      if (!"dilution" %in% names(df)) return(df)
      needs_resolve <- any(grepl("|", as.character(df$dilution), fixed = TRUE), na.rm = TRUE)
      if (!needs_resolve) return(df)
      if (!is.null(dil_ref) && "feature" %in% names(df) && "stype" %in% names(df)) {
        lkp <- unique(dil_ref[, c("antibody", "stype", "dilution_factor"), drop = FALSE])
        names(lkp) <- c("feature", "stype", ".dil_resolved")
        df <- merge(df, lkp, by = c("feature", "stype"), all.x = TRUE)
        df$dilution <- ifelse(
          !is.na(df$.dil_resolved),
          as.numeric(df$.dil_resolved),
          suppressWarnings(as.numeric(gsub("\\|.*", "", as.character(df$dilution))))
        )
        df$.dil_resolved <- NULL
      } else {
        df$dilution <- suppressWarnings(
          as.numeric(gsub("\\|.*", "", as.character(df$dilution)))
        )
      }
      df
    }

    withProgress(message = "Uploading FlowJo data to database…", value = 0.05, {

    # ── Header ──────────────────────────────────────────────────────────────
    setProgress(0.15, message = "Preparing plate header…")
    header_data <- plate_id_sheet
    header_data$workspace_id               <- workspace_id
    header_data$auth0_user                 <- auth0_user
    header_data$assay_response_variable    <- "mfi"
    header_data$assay_independent_variable <- "concentration"
    header_data <- apply_column_mapping(header_data, col_mapping)
    if ("plate_filename" %in% names(header_data))
      names(header_data)[names(header_data) == "plate_filename"] <- "file_name"
    header_cols <- c(
      "study_accession", "experiment_accession", "plate_id", "file_name",
      "acquisition_date", "reader_serial_number", "rp1_pmt_volts", "rp1_target",
      "auth0_user", "workspace_id", "plateid", "plate",
      "n_wells", "assay_response_variable", "assay_independent_variable",
      "nominal_sample_dilution", "project_id"
    )
    header_data <- header_data[, intersect(header_cols, names(header_data)), drop = FALSE]
    nk_cols     <- intersect(
      c("project_id", "study_accession", "experiment_accession",
        "plate_id", "nominal_sample_dilution"),
      names(header_data)
    )
    header_data <- header_data[!duplicated(header_data[, nk_cols, drop = FALSE]), ]
    cat("  Header rows:", nrow(header_data), "\n")

    # ── Helper: build one specimen-type block ────────────────────────────────
    build_specimen_block <- function(stype_chr, extra_cols, fill_type = stype_chr) {
      sub_map <- plates_map[substr(plates_map$specimen_type, 1, 1) == stype_chr, ]
      if (nrow(sub_map) == 0) return(NULL)

      out <- merge(
        sub_map,
        assay_response[, assay_cols, drop = FALSE],
        by = natural_key, all.x = TRUE
      )
      if (!"plate_id" %in% names(out) && "plate_id" %in% names(plate_id_sheet)) {
        pid_lookup <- unique(plate_id_sheet[, c("plateid", "plate_id"), drop = FALSE])
        out        <- merge(out, pid_lookup, by = "plateid", all.x = TRUE)
      }
      out <- apply_column_mapping(out, col_mapping)
      if (fill_type %in% c("X", "S", "C"))
        out <- fill_missing_sampleid(out, fill_type)
      out[, intersect(extra_cols, names(out)), drop = FALSE]
    }

    # ── Samples (X) ─────────────────────────────────────────────────────────
    setProgress(0.30, message = "Preparing samples…")
    sample_map <- plates_map[substr(plates_map$specimen_type, 1, 1) == "X", ]
    if (nrow(sample_map) > 0 && !is.null(subject_map)) {
      sample_map <- merge(
        sample_map, subject_map,
        by = c("study_name", "subject_id"), all.x = TRUE
      )
      sample_map$agroup <- dplyr::if_else(
        is.na(sample_map$groupb),
        sample_map$groupa,
        paste(sample_map$groupa, sample_map$groupb, sep = "_")
      )
    }
    samples_cols <- c(
      "project_id", "study_accession", "experiment_accession", "timeperiod",
      "patientid", "well", "stype", "sampleid", "agroup", "dilution",
      "pctaggbeads", "samplingerrors", "antigen", "feature", "antibody_mfi", "antibody_n",
      "plate", "nominal_sample_dilution", "plateid", "plate_id"
    )
    samples_to_upload <- if (nrow(sample_map) > 0) {
      tmp <- merge(
        sample_map,
        assay_response[, assay_cols, drop = FALSE],
        by = natural_key, all.x = TRUE
      )
      if (!"plate_id" %in% names(tmp) && "plate_id" %in% names(plate_id_sheet)) {
        pid_l <- unique(plate_id_sheet[, c("plateid", "plate_id"), drop = FALSE])
        tmp   <- merge(tmp, pid_l, by = "plateid", all.x = TRUE)
      }
      tmp <- apply_column_mapping(tmp, col_mapping)
      tmp <- fill_missing_sampleid(tmp, "X")
      tmp[, intersect(samples_cols, names(tmp)), drop = FALSE]
    } else NULL
    if (!is.null(samples_to_upload))
      samples_to_upload <- resolve_feature_dilution(samples_to_upload, flowjo_dilutions_ref)
    cat("  Sample rows:  ", if (is.null(samples_to_upload)) 0 else nrow(samples_to_upload), "\n")

    # ── Standards (S) ───────────────────────────────────────────────────────
    setProgress(0.45, message = "Preparing standards…")
    std_cols <- c(
      "project_id", "study_accession", "experiment_accession", "plate_id", "well",
      "stype", "sampleid", "source", "dilution", "pctaggbeads", "samplingerrors",
      "antigen", "antibody_mfi", "antibody_n", "feature",
      "plateid", "nominal_sample_dilution", "plate"
    )
    standards_to_upload <- build_specimen_block("S", std_cols, "S")
    cat("  Standard rows:", if (is.null(standards_to_upload)) 0 else nrow(standards_to_upload), "\n")

    # ── Controls (C) ────────────────────────────────────────────────────────
    setProgress(0.55, message = "Preparing controls…")
    controls_to_upload <- build_specimen_block("C", std_cols, "C")
    if (!is.null(controls_to_upload))
      controls_to_upload <- resolve_feature_dilution(controls_to_upload, flowjo_dilutions_ref)
    cat("  Control rows: ", if (is.null(controls_to_upload)) 0 else nrow(controls_to_upload), "\n")

    # ── Blanks (B) ──────────────────────────────────────────────────────────
    setProgress(0.65, message = "Preparing blanks…")
    blank_cols <- c(
      "project_id", "study_accession", "experiment_accession", "plate_id", "well",
      "stype", "source", "dilution", "pctaggbeads", "samplingerrors",
      "antigen", "antibody_mfi", "antibody_n", "feature",
      "plateid", "nominal_sample_dilution", "plate"
    )
    blanks_to_upload <- build_specimen_block("B", blank_cols)
    if (!is.null(blanks_to_upload))
      blanks_to_upload <- resolve_feature_dilution(blanks_to_upload, flowjo_dilutions_ref)
    cat("  Blank rows:   ", if (is.null(blanks_to_upload)) 0 else nrow(blanks_to_upload), "\n")

    # ── Antigens ────────────────────────────────────────────────────────────
    setProgress(0.72, message = "Preparing antigen list…")
    # Select columns BEFORE col_mapping so antigen_abbreviation -> antigen
    # (the insert_new_rows join key) survives into antigen_data.
    antigen_cols_needed <- c(
      "project_id", "study_name", "experiment_name", "antigen_abbreviation",
      "antigen_family", "standard_curve_max_concentration", "antigen_name",
      "virus_bacterial_strain", "antigen_source", "catalog_number",
      "l_asy_min_constraint", "l_asy_max_constraint", "l_asy_constraint_method"
    )
    antigen_data <- antigen_list[, intersect(antigen_cols_needed, names(antigen_list)), drop = FALSE]
    if ("standard_curve_max_concentration" %in% names(antigen_data))
      names(antigen_data)[names(antigen_data) == "standard_curve_max_concentration"] <- "standard_curve_concentration"
    antigen_data <- apply_column_mapping(antigen_data, col_mapping)
    cat("  Antigen cols:", paste(names(antigen_data), collapse=", "), "\n")

    # ── Subjects & Timepoints ───────────────────────────────────────────────
    setProgress(0.78, message = "Preparing subjects and timepoints…")

    # Planned visits (timepoint_map -> xmap_planned_visit)
    visits_to_upload <- timepoint_map
    if (!is.null(visits_to_upload)) {
      names(visits_to_upload)[names(visits_to_upload) == "timepoint_tissue_abbreviation"] <- "timepoint_name"
      names(visits_to_upload)[names(visits_to_upload) == "tissue_type"]    <- "type"
      names(visits_to_upload)[names(visits_to_upload) == "tissue_subtype"] <- "subtype"
      names(visits_to_upload)[names(visits_to_upload) == "description"]    <- "end_rule"
      names(visits_to_upload)[names(visits_to_upload) == "min_time_since_day_0"] <- "min_start_day"
      names(visits_to_upload)[names(visits_to_upload) == "max_time_since_day_0"] <- "max_start_day"
      visits_to_upload <- apply_column_mapping(visits_to_upload, col_mapping)
    }

    # ══════════════════════════════════════════════════════════════════════════
    # SPLIT BY FEATURE — one experiment_accession per isotype
    # e.g. "PTa_IgMGA" → "PTa_IgMGA_IgA", "PTa_IgMGA_IgG", "PTa_IgMGA_IgM"
    # ══════════════════════════════════════════════════════════════════════════
    setProgress(0.85, message = "Writing to database…")

    all_features <- sort(unique(assay_response$feature))
    n_feat       <- length(all_features)
    cat("\n  Features detected:", paste(all_features, collapse = ", "), "\n")
    cat("  Creating", n_feat, "experiment accessions:\n")
    for (feat in all_features)
      cat("    -", paste0(experiment_accession, "_", feat), "\n")

    db_result <- list(
      success = TRUE,
      errors  = list(),
      counts  = list(header=0, samples=0, standards=0,
                     blanks=0, controls=0, antigens=0, visits=0, curves=0L)
    )

    # Planned visits are study-level (no experiment in join key) — insert once
    if (!is.null(visits_to_upload) && nrow(visits_to_upload) > 0) {
      cat("  Inserting planned visits (study-level)…\n")
      existing_visits <- get_existing_visits(conn, study_accession)
      db_result$counts$visits <- insert_new_rows(
        conn, "madi_results", "xmap_planned_visit",
        new_data = visits_to_upload, existing_data = existing_visits,
        join_keys = c("study_accession", "timepoint_name"), label = "planned visit"
      )
      cat("    ✓ Visits:", db_result$counts$visits, "rows\n")
    }

    tryCatch({

      for (fi in seq_along(all_features)) {
        feat         <- all_features[[fi]]
        exp_acc_feat <- paste0(experiment_accession, "_", feat)
        prog_base    <- 0.87 + (fi - 1) * (0.12 / n_feat)

        cat("\n  ┌──────────────────────────────────────────────────────\n")
        cat("  │ Feature", fi, "of", n_feat, ":", feat,
            "→", exp_acc_feat, "\n")
        cat("  └──────────────────────────────────────────────────────\n")
        setProgress(prog_base, message = paste0("Feature ", feat,
                                                 " (", fi, "/", n_feat, ")…"))

        # Per-feature duplicate check — skip gracefully if already uploaded
        existing_feat <- check_existing_plates(
          conn = conn, project_id = project_id,
          study_accession      = study_accession,
          experiment_accession = exp_acc_feat,
          plateids             = plate_ids
        )
        if (nrow(existing_feat) > 0) {
          cat("    ⚠ Plates already exist for", exp_acc_feat, "— skipping\n")
          showNotification(
            paste("Skipped", feat, "— plates already in DB for", exp_acc_feat),
            type = "warning", duration = 4
          )
          next
        }

        # ── Slice this feature's rows from each prepared data frame ──────────
        feat_subset <- function(df) {
          if (is.null(df) || nrow(df) == 0) return(df)
          out <- if ("feature" %in% names(df)) df[df$feature == feat, , drop = FALSE] else df
          rownames(out) <- NULL
          if ("experiment_accession" %in% names(out)) out$experiment_accession <- exp_acc_feat
          out
        }

        hdr_f  <- header_data; hdr_f$experiment_accession <- exp_acc_feat
        samp_f <- feat_subset(samples_to_upload)
        std_f  <- feat_subset(standards_to_upload)
        ctrl_f <- feat_subset(controls_to_upload)
        blnk_f <- feat_subset(blanks_to_upload)
        ant_f  <- antigen_data; ant_f$experiment_accession <- exp_acc_feat

        # ── Header (4 rows per feature) ──────────────────────────────────────
        if ("acquisition_date" %in% names(hdr_f))
          hdr_f$acquisition_date <- standardize_date_for_postgres(hdr_f$acquisition_date)
        h_res <- insert_to_table(
          conn, "madi_results", "xmap_header", hdr_f, "header",
          required_cols = c("project_id", "study_accession", "plate_id")
        )
        db_result$counts$header <- db_result$counts$header + h_res$rows_inserted
        if (!h_res$success) {
          db_result$errors[[paste0("header_", feat)]] <- h_res$message
          cat("    ✗ Header failed:", h_res$message, "\n"); next
        }
        cat("    ✓ Header:", h_res$rows_inserted, "rows\n")

        # ── Samples ──────────────────────────────────────────────────────────
        if (!is.null(samp_f) && nrow(samp_f) > 0) {
          s_res <- insert_to_table(
            conn, "madi_results", "xmap_sample", samp_f, "sample",
            required_cols = c("project_id", "study_accession", "plate_id", "well")
          )
          db_result$counts$samples <- db_result$counts$samples + s_res$rows_inserted
          if (!s_res$success) { db_result$errors[[paste0("samples_", feat)]] <- s_res$message
            cat("    ✗ Samples failed:", s_res$message, "\n")
          } else cat("    ✓ Samples:", s_res$rows_inserted, "rows\n")
        }

        # ── Standards + curve_lookup ──────────────────────────────────────────
        if (!is.null(std_f) && nrow(std_f) > 0) {
          std_res <- insert_to_table(
            conn, "madi_results", "xmap_standard", std_f, "standard",
            required_cols = c("project_id", "study_accession", "plate_id", "well")
          )
          db_result$counts$standards <- db_result$counts$standards + std_res$rows_inserted
          if (!std_res$success) {
            db_result$errors[[paste0("standards_", feat)]] <- std_res$message
            cat("    ✗ Standards failed:", std_res$message, "\n")
          } else {
            cat("    ✓ Standards:", std_res$rows_inserted, "rows\n")
            tryCatch({
              cl_result <- register_curve_lookup(
                conn = conn, standards_df = std_f, project_id = workspace_id
              )
              db_result$counts$curves <- db_result$counts$curves + cl_result$rows_inserted
              cat("    ✓ curve_lookup:", cl_result$message, "\n")
            }, error = function(e_cl) {
              cat("    ⚠ curve_lookup non-fatal:", conditionMessage(e_cl), "\n")
              showNotification(paste("curve_lookup (non-fatal):", conditionMessage(e_cl)),
                               type = "warning", duration = 6)
            })
          }
        }

        # ── Blanks ────────────────────────────────────────────────────────────
        if (!is.null(blnk_f) && nrow(blnk_f) > 0) {
          b_res <- insert_to_table(
            conn, "madi_results", "xmap_buffer", blnk_f, "blank",
            required_cols = c("project_id", "study_accession", "plate_id", "well")
          )
          db_result$counts$blanks <- db_result$counts$blanks + b_res$rows_inserted
          if (!b_res$success) { db_result$errors[[paste0("blanks_", feat)]] <- b_res$message
            cat("    ✗ Blanks failed:", b_res$message, "\n")
          } else cat("    ✓ Blanks:", b_res$rows_inserted, "rows\n")
        }

        # ── Controls ──────────────────────────────────────────────────────────
        if (!is.null(ctrl_f) && nrow(ctrl_f) > 0) {
          c_res <- insert_to_table(
            conn, "madi_results", "xmap_control", ctrl_f, "control",
            required_cols = c("project_id", "study_accession", "plate_id", "well")
          )
          db_result$counts$controls <- db_result$counts$controls + c_res$rows_inserted
          if (!c_res$success) { db_result$errors[[paste0("controls_", feat)]] <- c_res$message
            cat("    ✗ Controls failed:", c_res$message, "\n")
          } else cat("    ✓ Controls:", c_res$rows_inserted, "rows\n")
        }

        # ── Antigen family (PT) — one row per feature-experiment ──────────────
        if (!is.null(ant_f) && nrow(ant_f) > 0) {
          existing_antigens <- get_existing_antigens(conn, study_accession, exp_acc_feat)
          a_n <- insert_new_rows(
            conn, "madi_results", "xmap_antigen_family",
            new_data = ant_f, existing_data = existing_antigens,
            join_keys = c("study_accession", "experiment_accession", "antigen"),
            label = "antigen family"
          )
          db_result$counts$antigens <- db_result$counts$antigens + a_n
          cat("    ✓ Antigens:", a_n, "rows\n")
        }

      } # ── end feature loop ──────────────────────────────────────────────────

      db_result$success <- length(db_result$errors) == 0
      setProgress(1.0, message = "Complete!")

      current_v <- flowjo_validation_rv()
      current_v$is_uploaded <- TRUE
      current_v$upload_time  <- Sys.time()
      flowjo_validation_rv(current_v)

      counts <- db_result$counts
      exp_list <- paste(paste0(experiment_accession, "_", all_features), collapse = ", ")
      detail <- sprintf(
        paste0("Uploaded %d feature-experiments (%s): ",
               "%d headers, %d samples, %d standards, %d blanks, ",
               "%d controls, %d antigens, %d visits, %d curves"),
        n_feat, exp_list,
        counts$header, counts$samples, counts$standards, counts$blanks,
        counts$controls, counts$antigens, counts$visits, counts$curves
      )
      cat("\n✓ UPLOAD COMPLETE —", detail, "\n")
      showNotification("FlowJo data uploaded successfully", type = "message", duration = 5)
      showNotification(detail, type = "message", duration = 12)

    }, error = function(e) {
      cat("✗ DB upload error:", e$message, "\n")
      showNotification(paste("Database upload error:", e$message),
                       type = "error", duration = NULL)
    })

  }) # end withProgress

  cat("╚══════════════════════════════════════════════════════════╝\n\n")
})
