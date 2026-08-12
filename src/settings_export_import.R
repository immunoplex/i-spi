# =============================================================================
# settings_export_import.R
# -----------------------------------------------------------------------------
# Server for the scoped settings-cascade Export/Import tab (UI in
# settings_export_import_ui.R). Isolated Shiny module: a bug here can only affect
# this tab. All DB access goes through settings_cascade_access.R
# (export_settings_scoped / import_settings_scoped).
#
#   settingsExportImportServer(id, pool, scope, user)
#     scope : reactive -> list(study, experiment, project_id)   (the app's calib_scope)
#     user  : reactive/reactiveVal -> current username
# =============================================================================

.nrow0 <- function(df) if (is.null(df) || !is.data.frame(df)) 0L else nrow(df)

# Read a settings export (.RData or .json) -> list(overrides = df,
# annotations = list(analyte, level, order) | NULL). Pre-annotation exports have
# no annotations block, so it may be NULL.
.read_settings_export <- function(path, name) {
  if (grepl("\\.json$", tolower(name))) {
    b <- jsonlite::fromJSON(path, simplifyVector = TRUE)
  } else {
    env <- new.env()
    load(path, envir = env)
    b <- if (exists("settings_bundle", envir = env)) get("settings_bundle", envir = env)
         else                                          get(ls(env)[1], envir = env)
  }
  ov <- b$overrides
  if (is.null(ov)) stop("no 'overrides' block found in this file")
  list(overrides   = as.data.frame(ov, stringsAsFactors = FALSE),
       annotations = b$annotations)
}

settingsExportImportServer <- function(id, pool, scope, user) {
  shiny::moduleServer(id, function(input, output, session) {

    cur <- shiny::reactive({
      s <- scope()
      list(project = s$project_id, study = s$study)
    })
    valid <- shiny::reactive({
      c <- cur()
      !is.null(c$study) && !is.na(c$study) && !identical(c$study, "Click here") &&
        !is.null(c$project) && !is.na(c$project)
    })

    # ---- Export ----------------------------------------------------------
    overrides <- shiny::reactive({
      shiny::req(valid())
      c <- cur()
      tryCatch(export_settings_scoped(pool, project = c$project, study = c$study),
               error = function(e) NULL)
    })

    annotations_export <- shiny::reactive({
      shiny::req(valid())
      c <- cur()
      tryCatch(export_annotations_scoped(pool, project = c$project, study = c$study),
               error = function(e) NULL)
    })

    output$export_summary <- shiny::renderUI({
      if (!valid()) return(shiny::em("Choose a study first."))
      n <- .nrow0(overrides()); a <- annotations_export()
      shiny::tagList(
        shiny::div(sprintf("%d setting override row%s stored for %s.",
                           n, if (identical(n, 1L)) "" else "s", cur()$study)),
        shiny::div(sprintf("annotations: %d analyte, %d level, %d order row(s).",
                           .nrow0(a$analyte), .nrow0(a$level), .nrow0(a$order)))
      )
    })

    bundle <- function() {
      c <- cur()
      list(format = "ispi-settings-cascade/2",
           project_id = c$project, study = c$study,
           generated_at = as.character(Sys.time()),
           overrides   = overrides(),
           annotations = annotations_export())
    }
    stem <- function() { st <- cur()$study; if (is.null(st) || is.na(st)) "study" else st }

    output$dl_rdata <- shiny::downloadHandler(
      filename = function() sprintf("%s_settings.RData", stem()),
      content  = function(file) { settings_bundle <- bundle(); save(settings_bundle, file = file) }
    )
    output$dl_json <- shiny::downloadHandler(
      filename = function() sprintf("%s_settings.json", stem()),
      content  = function(file) {
        json <- tryCatch(
          jsonlite::toJSON(bundle(), dataframe = "rows", na = "null", null = "null",
                           auto_unbox = TRUE, POSIXt = "ISO8601", force = TRUE),
          error = function(e)
            jsonlite::toJSON(list(error = conditionMessage(e)), auto_unbox = TRUE))
        writeLines(json, file)
      }
    )

    # ---- Import ----------------------------------------------------------
    parsed <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$upload, {
      f <- input$upload; shiny::req(f)
      b <- tryCatch(.read_settings_export(f$datapath, f$name), error = function(e) e)
      if (inherits(b, "error")) {
        parsed(NULL)
        output$import_status <- shiny::renderUI(shiny::span(
          style = "color:#b71c1c;", paste("Could not read file:", conditionMessage(b))))
        return()
      }
      parsed(b)
      nov <- .nrow0(b$overrides); a <- b$annotations
      ann_msg <- if (is.null(a)) "" else sprintf(
        " + annotations (analyte %d, level %d, order %d)",
        .nrow0(a$analyte), .nrow0(a$level), .nrow0(a$order))
      output$import_status <- shiny::renderUI(shiny::span(
        style = "color:#1b5e20;",
        sprintf("Parsed %d setting row%s%s. Review the preview, then Apply.",
                nov, if (identical(nov, 1L)) "" else "s", ann_msg)))
    })

    output$preview <- DT::renderDataTable({
      b <- parsed(); rows <- if (is.null(b)) NULL else b$overrides
      if (is.null(rows) || !nrow(rows))
        rows <- data.frame(Message = "Upload a settings export to preview it here.")
      DT::datatable(rows, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
    })

    shiny::observeEvent(input$apply, {
      b    <- parsed()
      rows <- if (is.null(b)) NULL else b$overrides
      ann  <- if (is.null(b)) NULL else b$annotations
      has_ann <- !is.null(ann) &&
        (.nrow0(ann$analyte) + .nrow0(ann$level) + .nrow0(ann$order)) > 0
      if ((is.null(rows) || !nrow(rows)) && !has_ann) {
        shiny::showNotification("Nothing to apply -- upload a settings export first.",
                                type = "warning"); return()
      }
      if (!valid()) {
        shiny::showNotification("Choose a study first.", type = "error"); return()
      }
      c <- cur(); reset <- isTRUE(input$reset_first)
      tryCatch({
        n  <- if (!is.null(rows) && nrow(rows))
                import_settings_scoped(pool, rows, project = c$project, study = c$study,
                                       user = user(), reset_first = reset) else 0L
        na <- import_annotations_scoped(pool, ann, project = c$project, study = c$study,
                                        user = user(), reset_first = reset)
        shiny::showNotification(
          sprintf("Applied %d setting%s and %d annotation row%s to %s.",
                  n,  if (identical(n,  1L)) "" else "s",
                  na, if (identical(na, 1L)) "" else "s", c$study), type = "message")
      }, error = function(e)
        shiny::showNotification(paste("Import failed:", conditionMessage(e)),
                                type = "error", duration = NULL))
    })
  })
}
