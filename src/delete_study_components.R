# =============================================================================
# delete_study_components.R  (server)
# -----------------------------------------------------------------------------
# Section 11.8: one universal, current-scoped mechanism to delete study data at
# either EXPERIMENT or WHOLE-STUDY granularity. NO plate-level deletion.
#
# Replaces the broken legacy paths (single-plate delete in plate_management.R /
# study_configuration.R::delete_plate; whole-study delete via the out-of-scope
# proc public.delete_study_accession_rows in import_lumifile.R).
#
# Deletes children-first inside ONE transaction on a single checked-out
# connection (a pool cannot run a multi-statement transaction). Reuses the table
# sets and .tbl() helper from calib_data_access.R so table names stay single-
# sourced -- this file MUST be sourced after calib_data_access.R.
#
# Scope of deletion (verified against the live schema):
#   experiment-scoped (experiment AND study delete):
#     xmap_header/standard/control/buffer/sample, xmap_dilution_analysis,
#     xmap_antigen_family                              -> project/study/experiment
#     annotation_analyte / annotation_level / annotation_order
#                                                      -> project/study/experiment
#     calib_* curve tables (CALIB_CURVE_TABLES)        -> by curve_id
#     calib_run                                        -> by job_id (NOT-EXISTS
#         guard so a shared batch job spanning other scopes is never orphaned)
#     curve_lookup (base table)                        -> project/study/experiment
#     calib_settings (experiment rows)                 -> project/study/experiment
#   study-level only (whole-study delete):
#     xmap_dilution_parameters, xmap_study_config      -> study/project
#     all remaining calib_settings for the study
#   NEVER touched (global/shared): calib_settings_meta,
#     calib_settings_migration_log, calib_settings_studycfg_log.
# =============================================================================

# ---- scope predicate shared by count + delete -------------------------------
# Experiment-scoped tables all carry project_id / study_accession /
# experiment_accession. project_id is IS NOT DISTINCT FROM to tolerate NULLs.
.dc_scope_where <- function(whole_study) {
  if (whole_study)
    "project_id IS NOT DISTINCT FROM $1 AND study_accession = $2"
  else
    "project_id IS NOT DISTINCT FROM $1 AND study_accession = $2 AND experiment_accession = $3"
}
.dc_scope_params <- function(whole_study, project, study, experiment) {
  if (whole_study) list(project, study) else list(project, study, experiment)
}
.dc_is_whole <- function(experiment) {
  is.null(experiment) || !nzchar(experiment) || identical(experiment, "__ALL__")
}

# Experiment-scoped tables other than the calib_* curve tables + curve_lookup.
# Includes the three annotation tables (annotation_analyte/level/order), which
# carry the same (project_id, study_accession, experiment_accession) key, so an
# experiment delete removes that experiment's annotations and a whole-study
# delete removes all of the study's annotations -- matching .ann_delete_study().
.DC_EXP_TABLES   <- c(unname(CALIB_RAW_TABLES), "xmap_dilution_analysis", "xmap_antigen_family",
                      "annotation_analyte", "annotation_level", "annotation_order")
# Study-level-only tables (no experiment_accession).
.DC_STUDY_TABLES <- c("xmap_dilution_parameters", "xmap_study_config")

# ---- dry-run: rows that WOULD be deleted, per table (read-only) --------------
count_study_components <- function(conn, project, study, experiment = NULL) {
  whole <- .dc_is_whole(experiment)
  sw <- .dc_scope_where(whole); sp <- .dc_scope_params(whole, project, study, experiment)
  cnt <- function(sql, params = list()) {
    res <- if (length(params)) DBI::dbGetQuery(conn, sql, params = params)
           else               DBI::dbGetQuery(conn, sql)
    as.integer(res[[1]])
  }
  out <- integer(0)

  cids <- DBI::dbGetQuery(conn, sprintf("SELECT curve_id FROM %s WHERE %s",
                                        .tbl("curve_lookup"), sw), params = sp)$curve_id
  idlist <- if (length(cids)) paste(cids, collapse = ",") else NULL

  if (!is.null(idlist)) {
    for (tb in CALIB_CURVE_TABLES)
      out[[tb]] <- cnt(sprintf("SELECT count(*) FROM %s WHERE curve_id IN (%s)", .tbl(tb), idlist))
    # runs whose fits are entirely within this scope (would be fully orphaned)
    out[["calib_run"]] <- cnt(sprintf(
      "SELECT count(*) FROM %s r
         WHERE EXISTS     (SELECT 1 FROM %s f  WHERE f.job_id  = r.job_id AND f.curve_id  IN (%s))
           AND NOT EXISTS (SELECT 1 FROM %s f2 WHERE f2.job_id = r.job_id AND f2.curve_id NOT IN (%s))",
      .tbl("calib_run"), .tbl("calib_fit"), idlist, .tbl("calib_fit"), idlist))
  }

  out[["curve_lookup"]] <- cnt(sprintf("SELECT count(*) FROM %s WHERE %s",
                                       .tbl("curve_lookup"), sw), sp)
  for (tb in .DC_EXP_TABLES)
    out[[tb]] <- cnt(sprintf("SELECT count(*) FROM %s WHERE %s", .tbl(tb), sw), sp)

  out[["calib_settings"]] <- if (whole)
    cnt(sprintf("SELECT count(*) FROM %s WHERE project_id IS NOT DISTINCT FROM $1 AND study_accession = $2",
                .tbl("calib_settings")), list(project, study))
  else
    cnt(sprintf("SELECT count(*) FROM %s WHERE project_id IS NOT DISTINCT FROM $1 AND study_accession = $2 AND experiment_accession = $3",
                .tbl("calib_settings")), list(project, study, experiment))

  if (whole) for (tb in .DC_STUDY_TABLES)
    out[[tb]] <- cnt(sprintf("SELECT count(*) FROM %s WHERE study_accession = $1 AND project_id IS NOT DISTINCT FROM $2",
                             .tbl(tb)), list(study, project))
  out[!is.na(out)]
}

# ---- the delete, atomic (children-first, one transaction) -------------------
delete_study_components <- function(conn, project, study, experiment = NULL) {
  stopifnot(!is.null(study), nzchar(study))
  whole <- .dc_is_whole(experiment)
  sw <- .dc_scope_where(whole); sp <- .dc_scope_params(whole, project, study, experiment)

  run_txn <- function(co) {
    ex <- function(sql, params = list())
      if (length(params)) DBI::dbExecute(co, sql, params = params)
      else                DBI::dbExecute(co, sql)
    deleted <- integer(0)

    cids <- DBI::dbGetQuery(co, sprintf("SELECT curve_id FROM %s WHERE %s",
                                        .tbl("curve_lookup"), sw), params = sp)$curve_id
    idlist <- if (length(cids)) paste(cids, collapse = ",") else NULL

    jobs <- character(0)
    if (!is.null(idlist)) {
      jobs <- DBI::dbGetQuery(co, sprintf("SELECT DISTINCT job_id FROM %s WHERE curve_id IN (%s)",
                                          .tbl("calib_fit"), idlist))$job_id
      for (tb in CALIB_CURVE_TABLES)
        deleted[[tb]] <- ex(sprintf("DELETE FROM %s WHERE curve_id IN (%s)", .tbl(tb), idlist))
    }
    # calib_run: only rows whose fits are now entirely gone (guard shared jobs)
    if (length(jobs)) {
      joblist <- paste(sprintf("'%s'", jobs), collapse = ",")
      deleted[["calib_run"]] <- ex(sprintf(
        "DELETE FROM %s r WHERE r.job_id::text IN (%s)
           AND NOT EXISTS (SELECT 1 FROM %s f WHERE f.job_id = r.job_id)",
        .tbl("calib_run"), joblist, .tbl("calib_fit")))
    }

    deleted[["curve_lookup"]] <- ex(sprintf("DELETE FROM %s WHERE %s", .tbl("curve_lookup"), sw), sp)

    for (tb in .DC_EXP_TABLES)
      deleted[[tb]] <- ex(sprintf("DELETE FROM %s WHERE %s", .tbl(tb), sw), sp)

    deleted[["calib_settings"]] <- if (whole)
      ex(sprintf("DELETE FROM %s WHERE project_id IS NOT DISTINCT FROM $1 AND study_accession = $2",
                 .tbl("calib_settings")), list(project, study))
    else
      ex(sprintf("DELETE FROM %s WHERE project_id IS NOT DISTINCT FROM $1 AND study_accession = $2 AND experiment_accession = $3",
                 .tbl("calib_settings")), list(project, study, experiment))

    if (whole) for (tb in .DC_STUDY_TABLES)
      deleted[[tb]] <- ex(sprintf("DELETE FROM %s WHERE study_accession = $1 AND project_id IS NOT DISTINCT FROM $2",
                                  .tbl(tb)), list(study, project))
    deleted
  }

  if (inherits(conn, "Pool")) {
    co <- pool::poolCheckout(conn); on.exit(pool::poolReturn(co), add = TRUE)
  } else co <- conn
  DBI::dbBegin(co)
  tryCatch({ r <- run_txn(co); DBI::dbCommit(co); r },
           error = function(e) {
             DBI::dbRollback(co)
             stop(sprintf("delete_study_components failed (rolled back): %s",
                          conditionMessage(e)), call. = FALSE)
           })
}

# =============================================================================
# Server wiring (top-level, sourced local=TRUE into the app server, matching the
# rest of the app). UI is defined in delete_study_components_ui.R.
# =============================================================================

# Preview result: named integer vector of per-table row counts, or NULL.
dc_preview_counts <- reactiveVal(NULL)

# clear the preview whenever the target study or scope changes
observeEvent(list(input$readxMap_study_accession, input$dc_scope), {
  dc_preview_counts(NULL)
}, ignoreInit = TRUE)

# Preview (dry-run): count what would be deleted for the chosen scope.
observeEvent(input$dc_preview, {
  study <- input$readxMap_study_accession
  proj  <- tryCatch(userWorkSpaceID(), error = function(e) NA)
  if (is.null(study) || identical(study, "Click here") || is.na(study)) {
    showNotification("Choose a study first.", type = "warning"); return()
  }
  exp <- if (identical(input$dc_scope, "__ALL__") || is.null(input$dc_scope)) NULL else input$dc_scope
  counts <- tryCatch(
    count_study_components(db_pool, project = proj, study = study, experiment = exp),
    error = function(e) { showNotification(paste("Preview failed:", conditionMessage(e)),
                                           type = "error", duration = NULL); NULL })
  dc_preview_counts(counts)
})

output$dc_blast <- DT::renderDataTable({
  counts <- dc_preview_counts()
  if (is.null(counts)) {
    df <- data.frame(Message = "Choose a scope and click Preview to see what would be deleted.")
    return(DT::datatable(df, rownames = FALSE, options = list(dom = "t")))
  }
  nz <- counts[counts > 0]
  df <- if (!length(nz)) data.frame(table = "(nothing to delete for this scope)", rows = 0L)
        else data.frame(table = names(nz), rows = as.integer(nz), row.names = NULL)
  if (length(nz)) df <- rbind(df, data.frame(table = "TOTAL", rows = as.integer(sum(nz))))
  DT::datatable(df, rownames = FALSE,
                options = list(dom = "t", pageLength = 100, ordering = FALSE))
})

# Confirm button appears only after a preview that found something to delete.
output$dc_confirm_ui <- renderUI({
  counts <- dc_preview_counts()
  if (is.null(counts) || !any(counts > 0)) return(NULL)
  study <- input$readxMap_study_accession
  scope_lbl <- if (identical(input$dc_scope, "__ALL__")) "the WHOLE study"
               else sprintf("experiment '%s'", input$dc_scope)
  tagList(
    tags$hr(),
    div(class = "alert alert-danger",
        sprintf("You are about to permanently delete %s of %s (%d rows).",
                scope_lbl, study, as.integer(sum(counts)))),
    actionButton("dc_delete", tagList(icon("trash"), "Delete permanently"),
                 class = "btn-danger")
  )
})

# Execute: double-confirm, then transactional delete.
observeEvent(input$dc_delete, {
  counts <- dc_preview_counts()
  if (is.null(counts) || !any(counts > 0)) return()
  study <- input$readxMap_study_accession
  scope_lbl <- if (identical(input$dc_scope, "__ALL__")) "the whole study (all experiments)"
               else sprintf("experiment '%s'", input$dc_scope)
  shinyalert::shinyalert(
    title = "Confirm permanent deletion",
    text = sprintf("Delete %s of %s?\n\n%d rows across %d tables. This cannot be undone.",
                   scope_lbl, study, as.integer(sum(counts)), length(counts[counts > 0])),
    type = "warning", showCancelButton = TRUE,
    confirmButtonText = "Yes, delete permanently", confirmButtonCol = "#d9534f",
    cancelButtonText = "Cancel", inputId = "dc_delete_confirm")
})

observeEvent(input$dc_delete_confirm, {
  req(isTRUE(input$dc_delete_confirm))
  study <- input$readxMap_study_accession
  proj  <- tryCatch(userWorkSpaceID(), error = function(e) NA)
  exp   <- if (identical(input$dc_scope, "__ALL__") || is.null(input$dc_scope)) NULL else input$dc_scope
  shinybusy::show_modal_spinner(spin = "fading-circle", color = "#d9534f",
                                text = "Deleting study components... please wait.")
  res <- tryCatch(
    delete_study_components(db_pool, project = proj, study = study, experiment = exp),
    error = function(e) e)
  shinybusy::remove_modal_spinner()
  if (inherits(res, "error")) {
    shinyalert::shinyalert("Delete failed", conditionMessage(res), type = "error")
    return()
  }
  dc_preview_counts(NULL)
  total <- as.integer(sum(unlist(res)))
  shinyalert::shinyalert(
    title = "Deleted",
    text = sprintf("Removed %d rows for %s.\n\nRefresh the study/experiment lists to see the change.",
                   total,
                   if (identical(input$dc_scope, "__ALL__")) study else paste0(study, " / ", input$dc_scope)),
    type = "success")
  # Whole-study delete: drop the study from the sidebar selector and fall back to
  # "Click here" -- the same effect a project switch has. study_choices_rv (app.R)
  # feeds output$main_study_selector; rebuilding it re-renders the selectize with
  # its selected="Click here" default. Experiment deletes leave the study in place.
  if (identical(input$dc_scope, "__ALL__")) {
    ch <- study_choices_rv()
    if (!is.null(ch)) study_choices_rv(ch[as.character(ch) != study])
    updateSelectizeInput(session, "readxMap_study_accession", selected = "Click here")
  }
})
