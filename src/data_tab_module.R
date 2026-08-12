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

# Coerce a frame to JSON-safe columns: integer64 -> character (above), then any
# POSIXct/Date, factor, or list-column (Postgres arrays/jsonb come back as
# lists) -> character. jsonlite::toJSON() defaults to force=FALSE and ABORTS on
# a class it doesn't recognise -- that thrown error is what made the download
# return Shiny's HTML error page (the empty .htm) instead of a file.
.json_safe_df <- function(df) {
  df <- .dt_coerce_int64(df)
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(df)
  df[] <- lapply(df, function(col) {
    if (inherits(col, c("POSIXct", "POSIXt", "Date"))) return(as.character(col))
    if (is.factor(col)) return(as.character(col))
    if (is.list(col)) return(vapply(col, function(x)
      if (length(x)) paste(as.character(x), collapse = ",") else NA_character_,
      character(1)))
    col
  })
  df
}

# Physical table -> the output id used to render it. The Plates table is special
# (keeps "stored_header" + selection for plate-ops); everything else is dt_<tbl>.
.dt_output_id <- function(tb) if (identical(tb, "xmap_header")) "stored_header" else paste0("dt_", tb)

# The settings cascade for export: settings_editor_view() gives one row per
# editable param (set or not) with BOTH metadata (group, label, control type,
# choices, data type) AND data (effective value + text), plus provenance
# (tier_rank = which tier the value came from) and is_overridden_here. Resolved
# at the Data-tab scope (project + study + experiment). Any list-columns (the
# derived effective_value, array-valued choices) are flattened to strings so
# the frame serializes identically into RData and JSON. Returns NULL on error
# so a settings hiccup never blocks a data download.
.settings_for_export <- function(pool, s) {
  ev <- tryCatch(
    settings_editor_view(pool, project = s$project_id, study = s$study,
                         experiment = s$experiment),
    error = function(e) NULL)
  if (is.null(ev) || !nrow(ev)) return(ev)
  ev[] <- lapply(ev, function(col)
    if (is.list(col))
      vapply(col, function(x)
        if (length(x)) paste(as.character(x), collapse = ",") else NA_character_,
        character(1))
    else col)
  ev
}

# Annotation rows for the Data-tab bundle, scoped to (project, study, experiment)
# — the same scope as .settings_for_export. Returns list(analyte, level, order).
# NULL-safe: a hiccup yields empty blocks, never blocks a data download.
.annotations_for_export <- function(pool, s) {
  ann <- tryCatch(export_annotations_scoped(pool, project = s$project_id, study = s$study),
                  error = function(e) NULL)
  if (is.null(ann)) return(list(analyte = NULL, level = NULL, order = NULL))
  exp <- s$experiment
  lapply(ann, function(df)
    if (is.data.frame(df) && "experiment_accession" %in% names(df) &&
        !is.null(exp) && !is.na(exp))
      df[df$experiment_accession == exp, , drop = FALSE] else df)
}

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
        shinycssloaders::withSpinner(
          DT::dataTableOutput(.dt_output_id(tb)), type = 4, color = "#337ab7"),
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
      shiny::downloadButton("download_rdata_bundle", "RData Bundle"),
      shiny::downloadButton("download_json_bundle", "Export (JSON + settings)")),
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

  # ---- scope signature: everything below reloads when this changes ------
  scope_sig <- shiny::reactive({
    s <- scope()
    paste(c(s$project_id, s$study, s$experiment, reload_trigger()), collapse = "|")
  })

  # ---- per-table loaders (one closure each, built for the current scope) -
  make_steps <- function(s, p) list(
    xmap_header       = function() fetch_raw_header(db_pool,               project = p, study = s$study, experiment = s$experiment),
    xmap_standard     = function() fetch_raw_standard(db_pool,             project = p, study = s$study, experiment = s$experiment),
    xmap_control      = function() fetch_raw_control(db_pool,              project = p, study = s$study, experiment = s$experiment),
    xmap_buffer       = function() fetch_raw_blank(db_pool,                project = p, study = s$study, experiment = s$experiment),
    xmap_sample       = function() fetch_raw_sample(db_pool,               project = p, study = s$study, experiment = s$experiment),
    curve_lookup      = function() fetch_curve_lookup_scoped(db_pool,      project = p, study = s$study, experiment = s$experiment),
    calib_run         = function() fetch_calib_run_scoped(db_pool,         project = p, study = s$study, experiment = s$experiment),
    calib_fit         = function() fetch_calib_fit_scoped(db_pool,         project = p, study = s$study, experiment = s$experiment),
    calib_param       = function() fetch_calib_param_scoped(db_pool,       project = p, study = s$study, experiment = s$experiment),
    calib_gate        = function() fetch_calib_gate_scoped(db_pool,        project = p, study = s$study, experiment = s$experiment),
    calib_grid        = function() fetch_calib_grid_scoped(db_pool,        project = p, study = s$study, experiment = s$experiment),
    calib_samples     = function() fetch_calib_samples_scoped(db_pool,     project = p, study = s$study, experiment = s$experiment),
    calib_diagnostics = function() fetch_calib_diagnostics_scoped(db_pool, project = p, study = s$study, experiment = s$experiment),
    calib_loo         = function() fetch_calib_loo_scoped(db_pool,         project = p, study = s$study, experiment = s$experiment)
  )

  # Loaded UP FRONT: the raw frames feed the plate-ops contract
  # (stored_plates_data) and curve_lookup feeds the status header. Everything
  # else -- the calib_* Results tables, incl. the heavy calib_grid join -- loads
  # LAZILY on first view (see get_table), which took grid loading off the
  # critical path. Shiny suspends hidden outputs, so only the viewed table loads.
  EAGER_TABLES <- c("xmap_header", "xmap_standard", "xmap_control",
                    "xmap_buffer", "xmap_sample", "curve_lookup")

  # ---- one timed fetch, one console line per table (perf instrumentation) -
  .load_timed <- function(tb, s, p) {
    t0  <- Sys.time()
    val <- tryCatch(make_steps(s, p)[[tb]](), error = function(e) {
      message(sprintf("[data-tab] load %-18s FAILED: %s", tb, conditionMessage(e))); NULL })
    message(sprintf("[data-tab] load %-18s %6.2fs  %12s rows", tb,
                    as.numeric(difftime(Sys.time(), t0, units = "secs")),
                    if (is.null(val)) "0" else format(nrow(val), big.mark = ",")))
    val
  }

  # ---- eager core: raw frames + curve_lookup, behind the progress bar ----
  core <- shiny::eventReactive(list(scope(), reload_trigger()), {
    s <- scope(); shiny::req(s$study, s$experiment); p <- s$project_id
    n <- length(EAGER_TABLES); t0 <- Sys.time()
    out <- vector("list", n); names(out) <- EAGER_TABLES
    shiny::withProgress(message = sprintf("Loading %s / %s", s$study, s$experiment),
      detail = "starting\u2026", value = 0, {
        for (i in seq_along(EAGER_TABLES)) {
          el <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")))
          shiny::setProgress(value = (i - 1) / n,
            detail = sprintf("%s  (%d of %d \u00b7 %ds elapsed)", EAGER_TABLES[i], i, n, el))
          out[[i]] <- .load_timed(EAGER_TABLES[i], s, p)
        }
        shiny::setProgress(value = 1, detail = "done")
      })
    if (!is.null(stored_plates_data)) {
      stored_plates_data$stored_header   <- out$xmap_header
      stored_plates_data$stored_standard <- out$xmap_standard
      stored_plates_data$stored_control  <- out$xmap_control
      stored_plates_data$stored_buffer   <- out$xmap_buffer
      stored_plates_data$stored_sample   <- out$xmap_sample
    }
    out
  }, ignoreNULL = FALSE)

  # ---- lazy per-table accessor, cached per scope in a plain env ----------
  .cache <- new.env(parent = emptyenv())
  get_table <- function(tb) {
    if (tb %in% EAGER_TABLES) return(core()[[tb]])
    key <- paste0(scope_sig(), "||", tb)     # scope_sig() -> re-render on scope/reload
    if (!exists(key, envir = .cache, inherits = FALSE)) {
      s <- scope(); assign(key, list(v = .load_timed(tb, s, s$project_id)), envir = .cache)
    }
    get(key, envir = .cache)$v
  }
  # Force-load every table (used ONLY by the RData/JSON bundle export).
  full_snapshot <- function() {
    tabs <- unlist(CALIB_TABLE_ORDER, use.names = FALSE)
    stats::setNames(lapply(tabs, get_table), tabs)
  }

  # Refresh button just bumps the shared trigger (same path a mask-save will use).
  shiny::observeEvent(input$data_refresh, {
    reload_trigger(reload_trigger() + 1)
  })

  # ---- status: what's loaded, and which Results tables are empty --------
  output$data_snapshot_status <- shiny::renderUI({
    s <- scope(); shiny::req(s$study, s$experiment)
    core_tbls <- core()                                  # raw frames + curve_lookup (eager)
    results <- CALIB_TABLE_ORDER$Results
    # Emptiness via ONE server-side COUNT probe (was: materialise every Results
    # table just to test nrow -- why the status blocked on grid/samples).
    cnts <- tryCatch(
      fetch_scoped_table_counts(db_pool, s$project_id, s$study, s$experiment, results),
      error = function(e) stats::setNames(rep(NA_integer_, length(results)), results))
    empty <- results[vapply(results, function(t)
      isTRUE(!is.na(cnts[[t]]) && cnts[[t]] == 0), logical(1))]
    tagList <- shiny::tagList(
      shiny::tags$b(sprintf("%s / %s", s$study, s$experiment)),
      shiny::tags$small(sprintf("  \u00b7  %s rows in Sample, %s curves registered",
        format(nrow(core_tbls$xmap_sample), big.mark = ","), nrow(core_tbls$curve_lookup))))
    if (length(empty))
      tagList <- shiny::tagAppendChild(tagList,
        shiny::div(class = "text-muted", style = "margin-top:4px;",
          sprintf("Not computed yet (empty): %s. Run an i-spi-compute job to populate.",
                  paste(vapply(empty, function(t) table_doc(t)$label, character(1)),
                        collapse = ", "))))
    tagList
  })

  # ---- render every table + its CSV download ----------------------------
  DISPLAY_CAP <- 5000L             # interactive-preview row cap for heavy audit tables
  CAP_TABLES  <- c("calib_grid")   # exports (CSV / RData / JSON) still pull the full table
  render_table <- function(tb) {
    oid <- .dt_output_id(tb)
    is_header <- identical(tb, "xmap_header")
    output[[oid]] <- DT::renderDataTable({
      cap_note <- NULL
      if (tb %in% CAP_TABLES) {
        # Heavy audit table (calib_grid: 132k x 28 ~= 40s over the VPN, all in the
        # column WIDTH). Show a capped preview for the interactive view; Download
        # CSV and the RData/JSON bundles still pull the COMPLETE table (get_table).
        s <- scope(); scope_sig()                       # deps: scope + reload
        df <- switch(tb,
          calib_grid = fetch_calib_grid_scoped(db_pool, s$project_id, s$study,
                         s$experiment, display_limit = DISPLAY_CAP),
          get_table(tb))
        total <- tryCatch(
          fetch_scoped_table_counts(db_pool, s$project_id, s$study, s$experiment, tb)[[tb]],
          error = function(e) NA_integer_)
        if (!is.null(df) && is.finite(total) && total > nrow(df))
          cap_note <- sprintf(
            "Showing first %s of %s rows \u2014 use Download CSV (or the RData/JSON bundle) for the full table.",
            format(nrow(df), big.mark = ","), format(total, big.mark = ","))
      } else {
        df <- get_table(tb)
      }
      if (is.null(df) || !nrow(df))
        df <- data.frame(Message = "No rows for this study/experiment (not computed yet).")
      DT::datatable(
        df, rownames = FALSE, filter = "top", caption = cap_note,
        selection = if (is_header) "single" else "none",   # Plates keeps selection for plate-ops
        options = list(scrollX = TRUE, pageLength = 15, lengthChange = TRUE)
      )
    }, server = TRUE)                                       # server-side paging (big tables)

    output[[paste0("dl_", tb)]] <- shiny::downloadHandler(
      filename = function() { s <- scope(); paste0(s$study, "_", s$experiment, "_", tb, ".csv") },
      content  = function(file)
        utils::write.csv(.dt_coerce_int64(get_table(tb)), file, row.names = FALSE)
    )
  }
  for (grp in names(CALIB_TABLE_ORDER)) for (tb in CALIB_TABLE_ORDER[[grp]]) render_table(tb)

  # ---- RData bundle: denormalized frames + manifest ---------------------
  output$download_rdata_bundle <- shiny::downloadHandler(
    filename = function() { s <- scope(); paste0(s$study, "_", s$experiment, ".RData") },
    content  = function(file) {
      snap <- full_snapshot(); s <- scope()
      snap <- snap[names(snap) != "xmap_antigen_family"]   # retired from bundle (annotations now in annotation_*)
      tables <- lapply(snap, .dt_coerce_int64)          # one frame per table, NK already prepended on calib_*
      manifest <- data.frame(
        table       = names(tables),
        label       = vapply(names(tables), function(t) table_doc(t)$label, character(1)),
        n_rows      = vapply(tables, function(d) if (is.null(d)) 0L else nrow(d), integer(1)),
        description = vapply(names(tables), function(t) table_doc(t)$what, character(1)),
        stringsAsFactors = FALSE)
      settings <- .settings_for_export(db_pool, s)      # cascade: metadata + values + provenance
      annotations <- .annotations_for_export(db_pool, s) # analyte/level/order for this experiment
      bundle <- list(
        study       = s$study,
        experiment  = s$experiment,
        project_id  = s$project_id,
        generated_at = as.character(Sys.time()),
        manifest    = manifest,
        settings    = settings,
        annotations = annotations,
        tables      = tables)
      save(bundle, file = file)
    }
  )

  # ---- JSON bundle: same content as the RData bundle in an open, cross-
  # language format (anything that reads JSON can consume it outside R). Plain
  # .json (no gzip binary-connection step to fail); every frame is coerced to
  # JSON-safe columns and toJSON runs with force=TRUE. The whole build is
  # guarded so a residual failure writes a readable {"error":...} INTO the file
  # rather than letting Shiny emit an HTML error page.
  output$download_json_bundle <- shiny::downloadHandler(
    filename = function() { s <- scope(); paste0(s$study, "_", s$experiment, "_bundle.json") },
    content  = function(file) {
      s <- scope()
      bundle <- tryCatch({
        snap     <- full_snapshot()
        snap     <- snap[names(snap) != "xmap_antigen_family"]   # retired from bundle
        tables   <- lapply(snap, .json_safe_df)
        settings <- .json_safe_df(.settings_for_export(db_pool, s))
        annotations <- lapply(.annotations_for_export(db_pool, s),
                              function(df) if (is.data.frame(df)) .json_safe_df(df) else df)
        manifest <- data.frame(
          table       = names(tables),
          label       = vapply(names(tables), function(t) table_doc(t)$label, character(1)),
          n_rows      = vapply(tables, function(d) if (is.null(d)) 0L else nrow(d), integer(1)),
          description = vapply(names(tables), function(t) table_doc(t)$what, character(1)),
          stringsAsFactors = FALSE)
        list(
          format       = "ispi-data-bundle/2",
          study        = s$study,
          experiment   = s$experiment,
          project_id   = s$project_id,
          generated_at = as.character(Sys.time()),
          manifest     = manifest,
          settings     = settings,
          annotations  = annotations,
          tables       = tables)
      }, error = function(e) list(error = paste("bundle build failed:", conditionMessage(e))))

      json <- tryCatch(
        jsonlite::toJSON(bundle, dataframe = "rows", na = "null", null = "null",
                         auto_unbox = TRUE, POSIXt = "ISO8601", force = TRUE),
        error = function(e)
          jsonlite::toJSON(list(error = paste("JSON serialization failed:", conditionMessage(e))),
                           auto_unbox = TRUE))
      writeLines(json, file)
    }
  )

  invisible(NULL)
}
