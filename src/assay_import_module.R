# =============================================================================
# assay_import_module.R  —  11.10 Assay Import Refactor, Phase 3
# -----------------------------------------------------------------------------
# ONE generic Shiny module implementing the single import UI standard:
#   1 upload raw files  ->  2 download layout template  ->  3 upload completed
#   template  ->  validate (issues shown + highlighted)  ->  4 preview  ->
#   5 commit (enabled only when there are no errors).
#
# The module is assay-agnostic: it is parameterised by a descriptor (assay id,
# label, assay-specific template controls) and dispatches to the registered
# format readers (assay_import_contract.R). It is mounted once per assay by
# mount_assay_import() (assay_import_mount.R), replacing the monolithic
# output$readxMapData renderUI and the ~30 app.R import reactiveVals — those
# become the module-internal reactiveValues below.
#
# Depends on: assay_import_contract.R (readers/registry/validator) and
# assay_import_backend.R (run_assay_commit). Source AFTER both.
#
# scope: a reactive returning list(project_id, study, experiment, user).
# =============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

# split a comma/pipe string control into a character vector (element orders)
.ai_split_order <- function(x, default) {
  if (is.null(x) || !length(x)) return(default)
  if (length(x) > 1) return(x)
  parts <- trimws(strsplit(as.character(x), "[,|]")[[1]])
  parts <- parts[nzchar(parts)]
  if (!length(parts)) default else parts
}


# ---- UI ---------------------------------------------------------------------

assay_import_ui <- function(id, descriptor) {
  ns <- NS(id)
  formats <- list_assay_formats(descriptor$assay)
  accept  <- unique(unlist(lapply(formats$format_id, function(f)
    get_assay_reader(descriptor$assay, f)$accept)))

  tagList(
    if (nrow(formats) > 1)
      selectInput(ns("format_id"), "File format",
                  choices = stats::setNames(formats$format_id, formats$label)),

    wellPanel(
      tags$h4("1. Upload instrument file(s)"),
      fileInput(ns("raw_files"), NULL, multiple = TRUE, accept = accept),
      if (!is.null(descriptor$assay_controls)) descriptor$assay_controls(ns),
      actionButton(ns("parse_btn"), "Parse uploaded file(s)", class = "btn-primary"),
      tags$span(style = "margin-left:12px;", textOutput(ns("parse_status"), inline = TRUE)),
      tags$hr(),
      downloadButton(ns("template"), "Download layout template"),
      tags$p(tags$small(
        "Parse the instrument file(s) first, then download the template, edit it, ",
        "and upload the completed file below."))
    ),

    wellPanel(
      tags$h4("2. Upload completed layout template"),
      fileInput(ns("layout_file"), NULL, accept = c(".xlsx", ".xls"))
    ),

    wellPanel(
      tags$h4("3. Validation"),
      textOutput(ns("issue_summary")),
      DT::dataTableOutput(ns("issues"))
    ),

    wellPanel(
      tags$h4("4. Preview"),
      tableOutput(ns("preview"))
    ),

    wellPanel(
      tags$h4("5. Commit"),
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
  # (guards against lazy loop-capture — see mount_assay_import).
  force(id); force(pool); force(descriptor); force(scope)
  moduleServer(id, function(input, output, session) {

    rv <- reactiveValues(raw = NULL, sheets = NULL, issues = NULL,
                         status = "", committed = FALSE, parse_status = "")

    current_reader <- reactive({
      fmt <- input$format_id %||% descriptor$default_format %||%
        list_assay_formats(descriptor$assay)$format_id[1]
      get_assay_reader(descriptor$assay, fmt)
    })

    build_opts <- reactive({
      s <- scope()
      list(
        project_id        = s$project_id,
        study             = s$study,
        experiment        = s$experiment,
        user              = s$user,
        n_wells           = input$n_wells %||% 96,
        feature_value     = input$feature_value,
        delimiter         = input$delimiter %||% "_",
        element_order     = .ai_split_order(input$x_element_order,
                              c("PatientID", "TimePeriod", "DilutionFactor")),
        bcs_element_order = .ai_split_order(input$bcs_element_order,
                              c("Source", "DilutionFactor")),
        raw_preview       = if (!is.null(rv$raw)) rv$raw$preview else NULL,
        dilutions_ref     = if (!is.null(rv$raw)) rv$raw$template_seed$dilutions else NULL,
        dilution_map      = if (!is.null(rv$raw)) rv$raw$template_seed$dilution_map else NULL
      )
    })

    # ── description-element ordering (bead/ELISA): drag-to-order sample
    #    elements = base + any optional elements toggled on. Rebuilt on toggle,
    #    matching the retired import UI. Flow has no description_elements -> skipped.
    if (!is.null(descriptor$description_elements)) {
      de <- descriptor$description_elements
      output$x_element_order_ui <- renderUI({
        opt   <- input$optional_elements
        items <- c(de$base, opt[opt %in% de$optional])
        shinyjqui::orderInput(
          inputId    = session$ns("x_element_order"),
          label      = "Description Label: Sample Elements (drag to reorder)",
          items      = items, width = "100%", item_class = "primary")
      })
    }

    # ── 1. parse raw files (explicit button; deterministic) ──────────────────
    observeEvent(input$parse_btn, {
      if (is.null(input$raw_files)) {
        showNotification("Select instrument file(s) first.", type = "warning")
        return()
      }
      rdr <- current_reader()
      cat(sprintf("[assay_import] parse triggered: %s/%s, %d file(s)\n",
                  descriptor$assay, rdr$format_id,
                  if (is.data.frame(input$raw_files)) nrow(input$raw_files) else 1L))
      tryCatch({
        rv$raw       <- rdr$parse_raw(input$raw_files, build_opts())
        rv$sheets    <- NULL; rv$issues <- NULL; rv$committed <- FALSE
        n <- if (is.data.frame(input$raw_files)) nrow(input$raw_files) else 1L
        rv$parse_status <- sprintf("Parsed %d file(s) — template ready.", n)
        showNotification(rv$parse_status, type = "message")
        cat("[assay_import] parse OK; rv$raw set\n")
      }, error = function(e) {
        rv$raw <- NULL
        rv$parse_status <- paste("Parse failed:", conditionMessage(e))
        showNotification(rv$parse_status, type = "error", duration = 12)
        cat("[assay_import] parse ERROR:", conditionMessage(e), "\n")
      })
    })

    output$parse_status <- renderText(rv$parse_status)

    # ── 2. template download ─────────────────────────────────────────────────
    output$template <- downloadHandler(
      filename = function()
        sprintf("%s_%s_%s_layout_template.xlsx",
                scope()$study %||% "study", scope()$experiment %||% "exp",
                descriptor$assay),
      content = function(file) {
        if (is.null(rv$raw)) {
          showNotification("Parse the instrument file(s) first, then download.",
                           type = "warning", duration = 8)
          openxlsx::write.xlsx(
            data.frame(Note = "Upload and parse instrument file(s) first, then download the template."),
            file)
          return(invisible())
        }
        current_reader()$make_template(
          rv$raw$template_seed, modifyList(build_opts(), list(output_file = file)))
      }
    )

    # ── 3. parse completed layout + validate ─────────────────────────────────
    observeEvent(input$layout_file, {
      req(input$layout_file)
      opts <- build_opts()
      tryCatch({
        cat("[assay_import] parse_layout starting\n")
        rv$sheets    <- current_reader()$parse_layout(input$layout_file, opts)
        cat("[assay_import] parse_layout done; validating\n")
        rv$issues    <- current_reader()$validate_sheets(rv$sheets, opts)
        rv$committed <- FALSE
        n_err <- sum(rv$issues$severity == "error")
        showNotification(
          if (n_err) sprintf("Validation found %d error(s).", n_err)
          else "Validation passed — ready to commit.",
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

    output$has_raw <- reactive({ !is.null(rv$raw) })
    outputOptions(output, "has_raw", suspendWhenHidden = FALSE)

    output$ready <- reactive({
      !is.null(rv$sheets) && length(rv$sheets) > 0 &&
        layout_sheets_ok(rv$issues) && !isTRUE(rv$committed)
    })
    outputOptions(output, "ready", suspendWhenHidden = FALSE)

    # ── 5. commit ─────────────────────────────────────────────────────────────
    observeEvent(input$commit, {
      req(rv$sheets, layout_sheets_ok(rv$issues))
      s <- scope()
      if (is.null(s$study) || !nzchar(s$study) ||
          is.null(s$experiment) || !nzchar(s$experiment)) {
        showNotification("Select a study and experiment before committing.",
                         type = "error"); return()
      }
      withProgress(message = "Uploading to database…", value = 0.4, {
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
