# =============================================================================
# assay_import_module.R  --  the generic assay import module
# -----------------------------------------------------------------------------
# ONE assay-agnostic Shiny module implementing the import standard:
#   1 upload raw files
#   2 CONFIRM THE LAYOUT        (plate grid: specimen type + description per well)
#   3 STANDARDS DILUTION REFERENCE (label -> dilution, for wells neither the
#                                   instrument file nor the text can resolve)
#   4 CONFIGURE THE DESCRIPTION (delimiters, shape groups, component bindings)
#   5 download layout template
#   6 upload completed template -> validate (issues shown + highlighted)
#   7 preview
#   8 commit (enabled only when there are no errors)
#
# Steps 2-4 are new and replace the descriptor-level flat controls that used
# to sit next to the upload box (delimiter, optional-element toggles, two
# drag-to-order element lists). Those asked for an element order before any
# string had been parsed and showed the consequence only in a downloaded
# workbook, so each new submitter cost a round of trial and error. The
# pre-processor shows resolved values as the choices are made.
#
# All three new steps GATE what follows: the template download stays disabled
# until every plate has samples, standards and blanks, every Standards well
# the instrument/text cannot resolve has a reference dilution entered, AND
# every description shape of every present specimen type is bound and
# approved. A descriptor that omits `preprocess` skips all three and behaves
# exactly as before.
#
# Depends on: assay_import_contract.R (readers/registry/validator),
# assay_import_backend.R (run_assay_commit), assay_well_inventory.R,
# assay_shape_rules.R, assay_std_reference_rules.R, assay_plate_grid.R,
# assay_shape_ui.R, assay_std_reference_ui.R. Source AFTER all.
#
# scope: a reactive returning list(project_id, study, experiment, user).
# =============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a


# ---- UI ---------------------------------------------------------------------

#' assay ("bead"/"elisa"/"flow") -> the per-format upload-procedure help_id.
#' "bead" has three formats with materially different upload behavior (only
#' .rbx/.srbx carries an instrument-reported dilution -- see
#' assay_well_inventory.R's .ai_inventory_bead()), so it's keyed one level
#' deeper than the other two assays, which have exactly one format each.
.ASSAY_UPLOAD_HELP_ID_BY_FORMAT <- list(
  bead = c(raw = "import.bead.raw.upload", xponent = "import.bead.xponent.upload",
          rbx = "import.bead.rbx.upload")
)
.ASSAY_UPLOAD_HELP_ID <- c(elisa = "import.elisa.upload", flow = "import.flow.upload")

#' Resolve the upload-step help_id for an assay + (possibly NULL/not-yet-
#' chosen) format_id. Falls back to the assay-level id when the assay has no
#' per-format entries or the format isn't recognized, so this never errors on
#' a stale/unset format_id.
.upload_help_id <- function(assay, format_id = NULL) {
  by_fmt <- .ASSAY_UPLOAD_HELP_ID_BY_FORMAT[[assay]]   # [[ on a LIST: NULL-safe on a miss
  if (!is.null(by_fmt) && !is.null(format_id) && format_id %in% names(by_fmt))
    return(by_fmt[[format_id]])
  # [[ on .ASSAY_UPLOAD_HELP_ID would NOT be safe here -- it's a plain named
  # character VECTOR, and [[ on a vector throws "subscript out of bounds" for
  # a missing name instead of returning NULL the way a list does (caught by
  # a direct unit test, not by this file's own parse/syntax checks).
  if (assay %in% names(.ASSAY_UPLOAD_HELP_ID)) .ASSAY_UPLOAD_HELP_ID[[assay]] else NULL
}

# One accent color per numbered step, keyed by the step's STABLE conceptual
# position (1-9), not its displayed number -- a non-preprocessor descriptor
# (step() below collapses 2-4 away) still colors "Layout template" the same
# color whether it displays as "5." or "2.", so a box's color always means
# the same section regardless of which descriptor is rendering it.
.IMPORT_STEP_COLORS <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
  "#8c564b", "#e377c2", "#7f7f7f", "#17becf"
)
#' Wrap one numbered import-step's content in a colored box (a tinted left
#' border, not a full-saturation fill, so the section reads as distinct
#' without fighting the content inside it for attention). `step_key` is the
#' STABLE 1-9 index into .IMPORT_STEP_COLORS, independent of the step's
#' displayed number.
.import_step_box <- function(step_key, ...) {
  color <- .IMPORT_STEP_COLORS[[step_key]]
  tags$div(
    style = sprintf(
      "border-left: 5px solid %s; background-color: %s14; padding: 12px 15px; margin-bottom: 15px; border-radius: 4px;",
      color, color),
    ...)
}

assay_import_ui <- function(id, descriptor) {
  ns <- NS(id)
  formats <- list_assay_formats(descriptor$assay)
  accept  <- unique(unlist(lapply(formats$format_id, function(f)
    get_assay_reader(descriptor$assay, f)$accept)))
  pre <- isTRUE(descriptor$preprocess)
  step <- function(n_pre, n_plain) if (pre) n_pre else n_plain

  tagList(
    if (nrow(formats) > 1)
      selectInput(ns("format_id"), "File format",
                  choices = stats::setNames(formats$format_id, formats$label)),

    .import_step_box(1,
      tags$h4("1. Upload instrument file(s)", uiOutput(ns("upload_help_icon"), inline = TRUE)),
      fileInput(ns("raw_files"), NULL, multiple = TRUE, accept = accept),
      if (!is.null(descriptor$assay_controls)) descriptor$assay_controls(ns),
      actionButton(ns("parse_btn"), "Parse uploaded file(s)", class = "btn-primary"),
      tags$span(style = "margin-left:12px;",
                textOutput(ns("parse_status"), inline = TRUE)),
      # Plate geometry is detected, not entered -- report what was found.
      uiOutput(ns("plate_info"))
    ),

    if (pre)
      conditionalPanel(
        condition = sprintf("output['%s']", ns("has_raw")),
        .import_step_box(2,
          tags$h4("2. Confirm the plate layout", help_icon("import.plate_grid.confirm", ns)),
          ai_plate_grid_ui(ns("grid"))),
        .import_step_box(3,
          tags$h4("3. Standards dilution reference",
                  help_icon("compute.import.dilution_source_precedence", ns)),
          ai_std_reference_ui(ns("stdref"))),
        .import_step_box(4,
          tags$h4("4. Configure the description field",
                  help_icon("compute.import.description_shape_binding", ns)),
          ai_shape_ui(ns("shape")))),

    .import_step_box(5,
      tags$h4(sprintf("%d. Layout template", step(5L, 2L)),
              help_icon("import.wizard.layout_template", ns)),
      uiOutput(ns("template_state")),
      downloadButton(ns("template"), "Download layout template"),
      tags$p(tags$small(
        "The template is built from the layout and description rules confirmed ",
        "above. Edit it if anything still needs changing, then upload it below."))
    ),

    .import_step_box(6,
      tags$h4(sprintf("%d. Upload completed layout template", step(6L, 3L)),
              help_icon("import.wizard.upload_completed", ns)),
      fileInput(ns("layout_file"), NULL, accept = c(".xlsx", ".xls"))
    ),

    .import_step_box(7,
      tags$h4(sprintf("%d. Validation", step(7L, 4L)),
              help_icon("import.wizard.validation", ns)),
      textOutput(ns("issue_summary")),
      DT::dataTableOutput(ns("issues"))
    ),

    .import_step_box(8,
      tags$h4(sprintf("%d. Preview", step(8L, 5L)),
              help_icon("import.wizard.preview", ns)),
      tableOutput(ns("preview"))
    ),

    .import_step_box(9,
      tags$h4(sprintf("%d. Commit", step(9L, 6L)),
              help_icon("import.wizard.commit", ns)),
      conditionalPanel(
        condition = sprintf("output['%s']", ns("ready")),
        actionButton(ns("commit"), "Upload to database", class = "btn-primary")
      ),
      conditionalPanel(
        condition = sprintf("!output['%s']", ns("ready")),
        tags$em("Upload a completed layout file with no errors to enable commit.")
      ),
      textOutput(ns("status"))
    )
  )
}


# ---- Server -----------------------------------------------------------------

assay_import_server <- function(id, pool, descriptor, scope) {
  # force promises immediately so this instance binds ITS descriptor/id/pool/scope
  # (guards against lazy loop-capture -- see mount_assay_import).
  force(id); force(pool); force(descriptor); force(scope)
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$help_show, {
      hid <- input$help_show
      body <- help_modal_body(hid, ns)
      if (is.null(body)) return()
      showModal(modalDialog(
        title = help_modal_title(hid), body,
        easyClose = TRUE, size = "l", footer = modalButton("Close")))
    })

    rv <- reactiveValues(raw = NULL, sheets = NULL, issues = NULL,
                         status = "", committed = FALSE, parse_status = "")

    use_pre      <- isTRUE(descriptor$preprocess)
    inventory_rv <- reactiveVal(NULL)

    current_reader <- reactive({
      fmt <- input$format_id %||% descriptor$default_format %||%
        list_assay_formats(descriptor$assay)$format_id[1]
      get_assay_reader(descriptor$assay, fmt)
    })

    # Step 1's help icon depends on the chosen FORMAT, not just the assay --
    # "bead" has three formats with materially different upload behavior
    # (.upload_help_id()) -- so it's a renderUI keyed on input$format_id
    # rather than the static help_icon() every other step uses.
    output$upload_help_icon <- renderUI({
      fmt <- input$format_id %||% descriptor$default_format %||%
        list_assay_formats(descriptor$assay)$format_id[1]
      hid <- .upload_help_id(descriptor$assay, fmt)
      if (is.null(hid)) return(NULL)
      help_icon(hid, ns)
    })

    # ── pre-processor: stage 1 (grid), 1.5 (standards reference), 2-3 (shapes) ──
    # Mounted unconditionally so the server shape is static; the UI renders only
    # when the descriptor opts in, and with no inventory all three modules idle.
    grid   <- ai_plate_grid_server("grid", inventory_rv, detected_n_wells)
    stdref <- ai_std_reference_server("stdref", inventory_rv, pool, scope,
                                      enabled = reactive(isTRUE(grid$ready())),
                                      specimen_type = "S",
                                      refresh = reactive(rv$raw))
    shape  <- ai_shape_server("shape", inventory_rv,
                             enabled   = reactive(isTRUE(grid$ready()) && isTRUE(stdref$ready())),
                             assay     = reactive(descriptor$assay),
                             reference = stdref$reference)

    pre_ready <- reactive({
      if (!use_pre) return(TRUE)
      isTRUE(grid$ready()) && isTRUE(stdref$ready()) && isTRUE(shape$ready())
    })

    # Plate size, in priority order: inferred by the inventory (which already
    # prefers a reader's own report, e.g. .rbx doc$geometry$n_wells) -> a
    # descriptor that still offers the control (flow) -> 96. Bead and ELISA no
    # longer have a field at all.
    detected_n_wells <- reactive({
      if (use_pre) {
        a <- attr(inventory_rv(), "n_wells")
        if (!is.null(a) && !is.na(a)) return(as.integer(a))
      }
      as.integer(input$n_wells %||% 96L)
    })

    output$plate_info <- renderUI({
      if (!use_pre || is.null(rv$raw)) return(NULL)
      iv <- inventory_rv(); if (is.null(iv)) return(NULL)
      tags$p(style = "color:#5f6368;font-size:12px;margin-top:8px;",
             sprintf("Plate size detected from the file(s): %d wells (%d \u00d7 %d).%s",
                     attr(iv, "n_wells") %||% 96L,
                     attr(iv, "plate_rows") %||% 8L,
                     attr(iv, "plate_cols") %||% 12L,
                     if (isTRUE(attr(iv, "plate_upgraded")))
                       " Wells were found beyond the size first assumed." else ""))
    })

    build_opts <- reactive({
      s <- scope()
      list(
        project_id     = s$project_id,
        study          = s$study,
        experiment     = s$experiment,
        user           = s$user,
        n_wells        = detected_n_wells(),
        feature_value  = input$feature_value,
        combine_experiment = isTRUE(input$combine_experiment),
        # The description is no longer described by a delimiter plus one element
        # order per type. The pre-processor hands over already-resolved identity
        # per well, plus the ruleset for the workbook's audit sheet.
        resolved_wells      = if (use_pre) shape$resolved() else NULL,
        description_ruleset = if (use_pre) shape$ruleset() else NULL,
        well_inventory      = inventory_rv(),
        raw_preview    = if (!is.null(rv$raw)) rv$raw$preview else NULL,
        dilutions_ref  = if (!is.null(rv$raw)) rv$raw$template_seed$dilutions else NULL,
        dilution_map   = if (!is.null(rv$raw)) rv$raw$template_seed$dilution_map else NULL
      )
    })

    # ── 1. parse raw files (explicit button; deterministic) ──────────────────
    observeEvent(input$parse_btn, {
      if (is.null(input$raw_files)) {
        showNotification("Select instrument file(s) first.", type = "warning")
        return()
      }
      rdr    <- current_reader()
      n_file <- if (is.data.frame(input$raw_files)) nrow(input$raw_files) else 1L
      cat(sprintf("[assay_import] parse triggered: %s/%s, %d file(s)\n",
                  descriptor$assay, rdr$format_id, n_file))

      # Parsing a batch takes long enough that a silent button reads as a crash.
      # The button is disabled and relabelled for the duration (so a second
      # click cannot queue a duplicate parse) and withProgress() names the phase
      # that is running.
      #
      # The stages are honest about their granularity: parse_raw() is one call
      # over the whole batch -- the readers loop files internally -- so there is
      # no per-file hook to report against. What the user gets is WHICH PHASE is
      # running, which is what answers "has it crashed".
      shinyjs::disable("parse_btn")
      updateActionButton(session, "parse_btn", label = "Parsing\u2026")
      on.exit({
        shinyjs::enable("parse_btn")
        updateActionButton(session, "parse_btn", label = "Parse uploaded file(s)")
      }, add = TRUE)

      withProgress(
        message = sprintf("Parsing %d file(s)", n_file),
        detail  = "Reading instrument data\u2026", value = 0.05, {
      tryCatch({
        rv$raw    <- rdr$parse_raw(input$raw_files, build_opts())
        rv$sheets <- NULL; rv$issues <- NULL; rv$committed <- FALSE

        # Auto-seed the persisted Standards Dilution Reference from a reader's
        # own authoritative source, when it supplies one (currently: flow's
        # uploaded "dilutions" tab, reader_flow.R's std_reference_seed). Fills
        # the SAME cascade setting ai_std_reference_server()'s own "Save"
        # button writes, using the SAME merge -- the seed overwrites any
        # existing saved value for a description it covers (the uploaded tab
        # is the source of truth, refreshed on every upload). Optional and
        # generic: a reader that doesn't set this is unaffected. Non-fatal --
        # a failure here just leaves the step asking for manual entry.
        seed <- rv$raw$template_seed$std_reference_seed
        if (use_pre && !is.null(seed) && nrow(seed)) {
          tryCatch({
            s <- scope()
            if (!is.null(s$project_id) && !is.null(s$study) && nzchar(s$experiment %||% "")) {
              PARAM    <- "standard_dilution_reference"
              existing <- resolve_settings_scoped(pool, project = s$project_id,
                                                  study = s$study, experiment = s$experiment)
              erow     <- existing[existing$param_name == PARAM, , drop = FALSE]
              saved    <- ai_std_reference_parse(if (nrow(erow)) erow$value_text[1] else NULL)
              merged   <- ai_std_reference_merge(saved, seed[, c("description", "dilution")])
              set_setting(pool, project = s$project_id, study = s$study,
                         param_name = PARAM, value = ai_std_reference_serialize(merged),
                         user = s$user, experiment = s$experiment)
              cat(sprintf(
                "[assay_import] auto-seeded %d standard dilution reference value(s) from the uploaded dilutions tab\n",
                nrow(seed)))
            }
          }, error = function(e)
            cat("[assay_import] std-reference auto-seed skipped:", conditionMessage(e), "\n"))
        }

        setProgress(value = 0.70, detail = "Building the well inventory\u2026")

        # Normalise the reader's preview into the editable well inventory. A
        # failure here is fatal to the pre-processor, so surface it rather than
        # quietly falling back to the old blind path.
        if (use_pre) {
          # 96 is only a floor here: ai_well_inventory() prefers the reader's
          # own geometry report and otherwise infers the size from the wells
          # present, so nothing depends on a user-entered value.
          inventory_rv(ai_well_inventory(rv$raw, descriptor$assay, n_wells = 96L))
          inv  <- inventory_rv()
          np   <- length(unique(inv$plate_key))
          nocc <- sum(!is.na(inv$type_code))

          setProgress(value = 0.90, detail = "Grouping the description field\u2026")
          rv$parse_status <- sprintf(
            "Parsed %d plate(s), %d occupied well(s), %d-well plate \u2014 confirm the layout below.",
            np, nocc, attr(inv, "n_wells") %||% 96L)
        } else {
          rv$parse_status <- sprintf("Parsed %d file(s) \u2014 template ready.", n_file)
        }
        setProgress(value = 1, detail = "Done")
        showNotification(rv$parse_status, type = "message")
        cat("[assay_import] parse OK; rv$raw set\n")
      }, error = function(e) {
        rv$raw <- NULL; inventory_rv(NULL)
        rv$parse_status <- paste("Parse failed:", conditionMessage(e))
        showNotification(rv$parse_status, type = "error", duration = NULL)
        cat("[assay_import] parse ERROR:", conditionMessage(e), "\n")
      })
      })  # withProgress
    })

    output$parse_status <- renderText(rv$parse_status)

    output$has_raw <- reactive({ !is.null(rv$raw) })
    outputOptions(output, "has_raw", suspendWhenHidden = FALSE)

    # ── 4. template download, gated on the pre-processor ─────────────────────
    output$template_state <- renderUI({
      if (is.null(rv$raw))
        return(tags$em("Parse instrument file(s) first."))
      if (!use_pre) return(NULL)
      if (isTRUE(pre_ready()))
        return(tags$div(style = "color:#2e7d32;font-weight:600;margin-bottom:8px;",
                        "Layout and description rules confirmed."))
      tags$div(style = "color:#b02a37;margin-bottom:8px;",
        "Finish steps 2 and 3 first. The template is built from what you ",
        "confirm there, so downloading now would only produce a workbook you ",
        "have to redo.")
    })

    observe({
      shinyjs::toggleState(id = "template",
                           condition = !is.null(rv$raw) && isTRUE(pre_ready()))
    })

    output$template <- downloadHandler(
      filename = function()
        sprintf("%s_%s_%s_layout_template.xlsx",
                scope()$study %||% "study", scope()$experiment %||% "exp",
                descriptor$assay),
      content = function(file) {
        if (is.null(rv$raw) || !isTRUE(pre_ready())) {
          showNotification("Finish the layout and description steps first.",
                           type = "warning", duration = 8)
          openxlsx::write.xlsx(
            data.frame(Note = "Confirm the plate layout and description rules first, then download the template."),
            file)
          return(invisible())
        }
        current_reader()$make_template(
          rv$raw$template_seed, modifyList(build_opts(), list(output_file = file)))
      }
    )

    # ── 5. parse completed layout + validate ─────────────────────────────────
    observeEvent(input$layout_file, {
      req(input$layout_file)
      opts <- build_opts()
      tryCatch({
        cat("[assay_import] parse_layout starting\n")
        rv$sheets <- current_reader()$parse_layout(input$layout_file, opts)
        cat("[assay_import] parse_layout done; validating\n")
        iss <- current_reader()$validate_sheets(rv$sheets, opts)

        # Carry the pre-processor's own findings into the same table, so one
        # place lists every reason an import is not ready.
        if (use_pre) {
          extra <- list(iss)
          gi <- tryCatch(grid$issues(),  error = function(e) NULL)
          si <- tryCatch(shape$issues(), error = function(e) NULL)
          if (!is.null(gi) && nrow(gi)) extra[[length(extra) + 1L]] <- gi
          if (!is.null(si) && nrow(si)) extra[[length(extra) + 1L]] <- si
          iss <- do.call(rbind, extra)
        }
        rv$issues    <- iss
        rv$committed <- FALSE
        n_err <- sum(rv$issues$severity == "error")
        showNotification(
          if (n_err) sprintf("Validation found %d error(s).", n_err)
          else "Validation passed \u2014 ready to commit.",
          type = if (n_err) "warning" else "message", duration = 6)
      }, error = function(e) {
        msg <- conditionMessage(e)
        cat("[assay_import] layout ERROR:", msg, "\n")
        # surface the failure IN the validation table (not just a transient toast)
        rv$issues <- data.frame(
          sheet = "layout_file", severity = "error", column = NA_character_,
          message = paste("Layout processing failed:", msg),
          stringsAsFactors = FALSE)
        showNotification(paste("Layout processing failed:", msg),
                         type = "error", duration = NULL)
      })
    })

    output$issue_summary <- renderText({
      iss <- rv$issues
      if (is.null(iss)) return("No layout file validated yet.")
      if (!nrow(iss)) return("No issues found.")
      sprintf("%d error(s), %d warning(s).",
              sum(iss$severity == "error"), sum(iss$severity == "warning"))
    })

    output$issues <- DT::renderDataTable({
      iss <- rv$issues
      req(!is.null(iss))
      if (!nrow(iss)) return(DT::datatable(
        data.frame(message = "No issues"), options = list(dom = "t"), rownames = FALSE))
      DT::formatStyle(
        DT::datatable(iss, rownames = FALSE,
                      options = list(dom = "tp", pageLength = 10)),
        "severity",
        backgroundColor = DT::styleEqual(
          c("error", "warning"), c("#f8d7da", "#fff3cd")))
    })

    output$preview <- renderTable({
      req(!is.null(rv$sheets), length(rv$sheets) > 0)
      data.frame(
        sheet = names(rv$sheets),
        rows  = vapply(rv$sheets, function(d)
          if (is.data.frame(d)) nrow(d) else 0L, integer(1)),
        row.names = NULL, stringsAsFactors = FALSE)
    })

    output$ready <- reactive({
      !is.null(rv$sheets) && length(rv$sheets) > 0 &&
        layout_sheets_ok(rv$issues) && !isTRUE(rv$committed)
    })
    outputOptions(output, "ready", suspendWhenHidden = FALSE)

    # ── 8. commit ────────────────────────────────────────────────────────────
    observeEvent(input$commit, {
      req(rv$sheets, layout_sheets_ok(rv$issues))
      s <- scope()
      if (is.null(s$study) || !nzchar(s$study) ||
          is.null(s$experiment) || !nzchar(s$experiment)) {
        showNotification("Select a study and experiment before committing.",
                         type = "error"); return()
      }
      withProgress(message = "Uploading to database\u2026", value = 0.4, {
        res <- tryCatch(
          run_assay_commit(pool, current_reader(), rv$sheets, s, build_opts()),
          error = function(e)
            list(success = FALSE, message = conditionMessage(e)))
      })
      rv$status    <- res$message
      rv$committed <- isTRUE(res$success)
      showNotification(res$message,
                       type = if (isTRUE(res$success)) "message" else "error",
                       duration = if (isTRUE(res$success)) 8 else NULL)
    })

    output$status <- renderText(rv$status)
  })
}
