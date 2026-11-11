# =============================================================================
# fit_diagnostics_module.R  --  Data > Results > Fit diagnostics
#
# One button builds the diagnostics report for the selected experiment (see
# fit_diagnostics_report.R), shows the verdict in plain language, previews the
# full text and offers it as Markdown / JSON, for a person or an LLM to read.
# Built on demand rather than on tab open: it reads seven result tables.
# =============================================================================

fitDiagnosticsUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(class = "help-block", style = "margin:8px 0;",
      shiny::strong("Why a fit looks odd, in one place."),
      " Gathers the sampler statistics, population parameters, per-plate curve parameters, ",
      "quantification gates and LOO results for the selected experiment, assesses them, ",
      "and suggests what to do. The download is written so it can be pasted straight into an LLM."),
    shiny::actionButton(ns("build"), "Build diagnostics report", icon = shiny::icon("stethoscope"),
                        class = "btn-primary"),
    shiny::uiOutput(ns("verdict")),
    shiny::uiOutput(ns("actions")),
    shiny::uiOutput(ns("preview_ui")))
}

#' @param scope  reactive -> list(study, experiment, project_id)
#' @param fetch  function(pool, project, study, experiment) -> list of frames
#'               (injectable for tests)
fitDiagnosticsServer <- function(id, scope, pool, reload_trigger = NULL,
                                 fetch = fit_diag_fetch) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rpt <- shiny::reactiveVal(NULL)
    # A new experiment or a reload makes the previous report stale: clear it.
    shiny::observeEvent(list(scope(), if (!is.null(reload_trigger)) reload_trigger()), rpt(NULL),
                        ignoreInit = TRUE)

    shiny::observeEvent(input$build, {
      s <- scope()
      shiny::withProgress(message = "Collecting fit diagnostics...", value = 0.3, {
        res <- tryCatch({
          d <- fetch(pool, s$project_id, s$study, s$experiment)
          shiny::incProgress(0.4)
          fit_diag_build(d, list(project_id = s$project_id, study = s$study, experiment = s$experiment))
        }, error = function(e) e)
      })
      if (inherits(res, "error"))
        shiny::showNotification(paste("Diagnostics failed:", conditionMessage(res)), type = "error")
      else rpt(res)
    })

    text <- shiny::reactive({ r <- rpt(); if (is.null(r)) NULL else fit_diag_report_text(r) })

    output$verdict <- shiny::renderUI({
      r <- rpt(); if (is.null(r)) return(NULL)
      if (!length(r$units))
        return(shiny::div(class = "alert alert-info", "No fitted curves were found for this experiment."))
      n <- fit_diag_notice(r)
      if (is.null(n))
        return(shiny::div(class = "alert alert-success",
          shiny::icon("check"), " No problems detected in the stored fit diagnostics."))
      shiny::div(class = if (identical(n$severity, "error")) "alert alert-danger" else "alert alert-warning",
        shiny::strong(if (identical(n$severity, "error")) "Problems found" else "Check these fits"),
        shiny::tags$ul(lapply(n$text, shiny::tags$li)))
    })

    output$actions <- shiny::renderUI({
      if (is.null(rpt())) return(NULL)
      shiny::div(style = "margin:6px 0;",
        shiny::downloadButton(ns("dl_md"), "Report (.md)", class = "btn-xs"),
        shiny::downloadButton(ns("dl_json"), "Report (.json)", class = "btn-xs"),
        shiny::tags$button(type = "button", class = "btn btn-default btn-xs",
          onclick = sprintf("navigator.clipboard.writeText(document.getElementById('%s').innerText)", ns("preview")),
          shiny::icon("copy"), " Copy text"))
    })

    output$preview_ui <- shiny::renderUI({
      if (is.null(text())) return(NULL)
      shiny::tags$pre(id = ns("preview"),
        style = "max-height:480px; overflow:auto; white-space:pre-wrap; font-size:11px;", text())
    })

    fname <- function(ext) {
      s <- scope()
      sprintf("fit_diagnostics_%s_%s_%s.%s", gsub("[^A-Za-z0-9]+", "_", s$study),
              gsub("[^A-Za-z0-9]+", "_", s$experiment), format(Sys.Date(), "%Y%m%d"), ext)
    }
    output$dl_md   <- shiny::downloadHandler(filename = function() fname("md"),
      content = function(file) writeLines(text(), file, useBytes = TRUE))
    output$dl_json <- shiny::downloadHandler(filename = function() fname("json"),
      content = function(file) writeLines(as.character(fit_diag_report_json(rpt())), file, useBytes = TRUE))

    list(report = rpt)
  })
}
