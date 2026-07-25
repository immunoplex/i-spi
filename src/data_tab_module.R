# =============================================================================
# data_tab_module.R  --  the Data tab: transparent, per-table view of everything
#                        stored for the selected study/experiment, plus export.
# -----------------------------------------------------------------------------
# DESIGN
#   * FLAT, not a Shiny NS module -- deliberately. The plate-operations slice
#     reads input$stored_header_rows_selected and the app-level
#     stored_plates_data / selected_studyexpplate directly, so we keep the ids
#     un-namespaced and leave plate-ops untouched. (When plate-ops is extracted
#     later, this can become a proper module.)
#   * STATIC SNAPSHOT, not fine-grained reactivity. One load per {scope,
#     reload_trigger}; everything renders/exports from that frozen snapshot with
#     no re-querying. reload_trigger is bumped by: experiment change (via scope),
#     the Refresh button, and -- later -- a mask save.
#   * Reads BASE xmap_* tables so masked/mask_reason are visible; calib_* rows
#     come NK-denormalized. No best_*/bayes_* anywhere.
#
# DEPENDS: shiny, DT; calib_data_access.R (fetch_raw_*, fetch_calib_*_scoped,
#          fetch_curve_lookup_scoped, fetch_calib_run_scoped), data_dictionary.R
#          (CALIB_TABLE_ORDER, table_doc).
#
# The Plates table keeps the id "stored_header" with single-row selection so the
# plate-ops observers keep firing on input$stored_header_rows_selected.
# =============================================================================

# bigint (integer64) -> character for safe CSV/RData export.
.dt_coerce_int64 <- function(df) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(df)
  df[] <- lapply(df, function(col)
    if (inherits(col, "integer64")) bit64::as.character.integer64(col) else col)
  df
}

# Physical table -> the output id used to render it. The Plates table is special
# (keeps "stored_header" + selection for plate-ops); everything else is dt_<tbl>.
.dt_output_id <- function(tb) if (identical(tb, "xmap_header")) "stored_header" else paste0("dt_", tb)

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
#' Build the Data-tab UI: Refresh + RData Bundle, a status line, then a grouped
#' tabset (Raw inputs / Registry / Results) driven by CALIB_TABLE_ORDER, each
#' table captioned from the data dictionary.
dataTabUI <- function() {
  groups <- lapply(names(CALIB_TABLE_ORDER), function(grp) {
    inner <- lapply(CALIB_TABLE_ORDER[[grp]], function(tb) {
      doc <- table_doc(tb)
      # The Plates tab also hosts the plate-ops UI hooks (split / wavelength
      # subtraction / header actions) so that slice keeps working untouched.
      extra <- if (identical(tb, "xmap_header")) shiny::tagList(
        shiny::uiOutput("header_actions"),
        shiny::uiOutput("split_plate_nominal_UI"),
        shiny::uiOutput("wavelength_subtraction_UI")
      ) else NULL
      shiny::tabPanel(
        title = doc$label,
        shiny::div(class = "help-block", style = "margin:8px 0;",
          shiny::strong(doc$what), shiny::br(),
          shiny::tags$small(sprintf("Grain: %s", doc$grain))),
        DT::dataTableOutput(.dt_output_id(tb)),
        shiny::div(style = "margin-top:6px;",
          shiny::downloadButton(paste0("dl_", tb), "Download CSV", class = "btn-xs")),
        extra
      )
    })
    shiny::tabPanel(grp, do.call(shiny::tabsetPanel, inner))
  })

  shiny::tagList(
    shiny::div(style = "margin:6px 0 10px;",
      shiny::actionButton("data_refresh", "Refresh Data", icon = shiny::icon("sync")),
      shiny::downloadButton("download_rdata_bundle", "RData Bundle")),
    shiny::uiOutput("data_snapshot_status"),
    do.call(shiny::tabsetPanel, groups)
  )
}

# ---------------------------------------------------------------------------
# Server (flat controller)
# ---------------------------------------------------------------------------
#' @param scope          reactive -> list(study, experiment, project_id)
#' @param reload_trigger app-level reactiveVal; bump to force a reload (Refresh
#'                        button here; mask-save later)
#' @param stored_plates_data optional app-level reactiveValues; if given, the raw
#'                        frames are mirrored into it to preserve the plate-ops
#'                        contract (stored_header/standard/control/buffer/sample)
dataTabServer <- function(input, output, session, conn, scope, reload_trigger,
                          stored_plates_data = NULL) {

  # ---- the one static snapshot ------------------------------------------
  snapshot <- shiny::eventReactive(list(scope(), reload_trigger()), {
    s <- scope(); shiny::req(s$study, s$experiment)
    p <- s$project_id
    snap <- list(
      xmap_header       = fetch_raw_header(conn,   s$study, s$experiment, p),
      xmap_standard     = fetch_raw_standard(conn, s$study, s$experiment, p),
      xmap_control      = fetch_raw_control(conn,  s$study, s$experiment, p),
      xmap_buffer       = fetch_raw_blank(conn,    s$study, s$experiment, p),
      xmap_sample       = fetch_raw_sample(conn,   s$study, s$experiment, p),
      curve_lookup      = fetch_curve_lookup_scoped(conn,      s$study, s$experiment, p),
      calib_run         = fetch_calib_run_scoped(conn,         s$study, s$experiment, p),
      calib_fit         = fetch_calib_fit_scoped(conn,         s$study, s$experiment, p),
      calib_param       = fetch_calib_param_scoped(conn,       s$study, s$experiment, p),
      calib_gate        = fetch_calib_gate_scoped(conn,        s$study, s$experiment, p),
      calib_grid        = fetch_calib_grid_scoped(conn,        s$study, s$experiment, p),
      calib_samples     = fetch_calib_samples_scoped(conn,     s$study, s$experiment, p),
      calib_diagnostics = fetch_calib_diagnostics_scoped(conn, s$study, s$experiment, p),
      calib_loo         = fetch_calib_loo_scoped(conn,         s$study, s$experiment, p)
    )
    # Preserve the plate-ops contract (raw frames only).
    if (!is.null(stored_plates_data)) {
      stored_plates_data$stored_header   <- snap$xmap_header
      stored_plates_data$stored_standard <- snap$xmap_standard
      stored_plates_data$stored_control  <- snap$xmap_control
      stored_plates_data$stored_buffer   <- snap$xmap_buffer
      stored_plates_data$stored_sample   <- snap$xmap_sample
    }
    snap
  }, ignoreNULL = FALSE)

  # Refresh button just bumps the shared trigger (same path a mask-save will use).
  shiny::observeEvent(input$data_refresh, {
    reload_trigger(reload_trigger() + 1)
  })

  # ---- status: what's loaded, and which Results tables are empty --------
  output$data_snapshot_status <- shiny::renderUI({
    snap <- snapshot(); s <- scope()
    results <- CALIB_TABLE_ORDER$Results
    empty <- results[vapply(results, function(t) {
      d <- snap[[t]]; is.null(d) || !nrow(d) }, logical(1))]
    tagList <- shiny::tagList(
      shiny::tags$b(sprintf("%s / %s", s$study, s$experiment)),
      shiny::tags$small(sprintf("  \u00b7  %s rows in Sample, %s curves registered",
        format(nrow(snap$xmap_sample), big.mark = ","), nrow(snap$curve_lookup))))
    if (length(empty))
      tagList <- shiny::tagAppendChild(tagList,
        shiny::div(class = "text-muted", style = "margin-top:4px;",
          sprintf("Not computed yet (empty): %s. Run an i-spi-compute job to populate.",
                  paste(vapply(empty, function(t) table_doc(t)$label, character(1)),
                        collapse = ", "))))
    tagList
  })

  # ---- render every table + its CSV download ----------------------------
  render_table <- function(tb) {
    oid <- .dt_output_id(tb)
    is_header <- identical(tb, "xmap_header")
    output[[oid]] <- DT::renderDataTable({
      df <- snapshot()[[tb]]
      if (is.null(df) || !nrow(df))
        df <- data.frame(Message = "No rows for this study/experiment (not computed yet).")
      DT::datatable(
        df, rownames = FALSE, filter = "top",
        selection = if (is_header) "single" else "none",   # Plates keeps selection for plate-ops
        options = list(scrollX = TRUE, pageLength = 15, lengthChange = TRUE)
      )
    }, server = TRUE)                                       # server-side paging (big tables)

    output[[paste0("dl_", tb)]] <- shiny::downloadHandler(
      filename = function() { s <- scope(); paste0(s$study, "_", s$experiment, "_", tb, ".csv") },
      content  = function(file)
        utils::write.csv(.dt_coerce_int64(snapshot()[[tb]]), file, row.names = FALSE)
    )
  }
  for (grp in names(CALIB_TABLE_ORDER)) for (tb in CALIB_TABLE_ORDER[[grp]]) render_table(tb)

  # ---- RData bundle: denormalized frames + manifest ---------------------
  output$download_rdata_bundle <- shiny::downloadHandler(
    filename = function() { s <- scope(); paste0(s$study, "_", s$experiment, ".RData") },
    content  = function(file) {
      snap <- snapshot(); s <- scope()
      tables <- lapply(snap, .dt_coerce_int64)          # one frame per table, NK already prepended on calib_*
      manifest <- data.frame(
        table       = names(tables),
        label       = vapply(names(tables), function(t) table_doc(t)$label, character(1)),
        n_rows      = vapply(tables, function(d) if (is.null(d)) 0L else nrow(d), integer(1)),
        description = vapply(names(tables), function(t) table_doc(t)$what, character(1)),
        stringsAsFactors = FALSE)
      bundle <- list(
        study       = s$study,
        experiment  = s$experiment,
        project_id  = s$project_id,
        generated_at = as.character(Sys.time()),
        manifest    = manifest,
        tables      = tables)
      save(bundle, file = file)
    }
  )

  invisible(NULL)
}
