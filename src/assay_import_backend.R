# =============================================================================
# assay_import_backend.R  —  11.10 Assay Import Refactor, Phase 0
# -----------------------------------------------------------------------------
# The ONE landing routine that commits an assembled import. Replaces the three
# inline commit bodies (import_lumifile.R upload_batch_button observer,
# elisa_reader.R upload observer, flowjo_reader.R upload observer) and the dead
# upload_batch_to_database().
#
# Contract in: per-experiment "units" of already-assembled DB frames (from
# assemble_upload_frames() / a reader's assemble()), plus study-level scope and
# planned visits. Contract out: a counts/result list.
#
# Two deliberate changes from the legacy paths:
#   1. ATOMIC. The whole import runs inside pool::poolWithTransaction(); any
#      failure rolls the entire import back. No more partial writes.
#   2. HARD-FAIL curve_lookup. register_curve_lookup() failing now aborts the
#      transaction (was "non-fatal, never rolls back"). curve_lookup is a load-
#      bearing registry — a silent miss stranded settings and broke the worker.
#
# Ordering invariant (see REFACTOR_settings_cascade.md §8): within a unit,
# standards are inserted, THEN curve_lookup is registered, THEN the antigen
# family + cascade settings are written — the antigen tier needs the full ladder
# (project/study/experiment/feature/antigen) and feature comes from the just-
# registered curves.
#
# Conventions: pool first. Inside the transaction the checked-out `conn` is
# passed to the existing DBI-generic seams unchanged (insert_to_table,
# register_curve_lookup, upload_antigen_family, upload_planned_visits).
#
# Runtime seam dependencies (in scope once helper libraries are sourced):
#   insert_to_table()            [plate_validator_functions.R]
#   register_curve_lookup()      [curve_lookup_functions.R]
#   upload_antigen_family()      [plate_validator_functions.R]  (threads feature,
#                                  writes cascade via write_antigen_settings_to_cascade)
#   upload_planned_visits()      [plate_validator_functions.R]
#   check_existing_plates()      [db_functions.R]  (optional duplicate guard)
# =============================================================================


#' Orchestrate stage-2: assemble -> split -> commit for one reader.
#'
#' The generic glue the module calls once a layout has validated clean. Keeps the
#' module thin and keeps assay-specific behaviour (assemble opts, feature split)
#' in the reader/descriptor.
#'
#' @param pool   db_pool.
#' @param reader an assay_reader (from the registry).
#' @param sheets validated layout sheets (reader$parse_layout output).
#' @param scope  list(project_id, study, experiment, user).
#' @param opts   passed through to assemble/commit.
#' @return commit_assay_import() result list.
run_assay_commit <- function(pool, reader, sheets, scope, opts = list()) {
  cat("[assay_import] run_assay_commit: assembling frames\n")
  frames <- reader$assemble(sheets, scope, opts)
  cat("[assay_import] assembled; splitting experiments\n")
  units  <- reader$split_experiments(frames)
  cat(sprintf("[assay_import] %d unit(s); entering commit\n", length(units)))
  commit_assay_import(pool, units, scope,
                      timepoint_map = frames$timepoint_map, opts = opts)
}


#' Commit an assembled assay import atomically.
#'
#' @param pool          a pool::Pool (db_pool). First arg, per convention.
#' @param units         list of per-experiment units. Each unit is a list with
#'                      $experiment (chr or NULL -> scope$experiment) and the
#'                      frames $header/$samples/$standards/$blanks/$controls/
#'                      $antigen_list. Produced by a reader's split_experiments().
#' @param scope         list(project_id, study, experiment, user). project_id is
#'                      an integer >= 16; user tags the cascade audit column.
#' @param timepoint_map study-level planned-visit frame, inserted ONCE. NULL ok.
#' @param opts          list; opts$skip_existing (default TRUE) runs the
#'                      per-experiment duplicate guard via check_existing_plates.
#' @return list(success, counts, experiments, message). On failure the function
#'         stops inside the transaction (rolled back) and returns a failed result.
commit_assay_import <- function(pool, units, scope,
                                timepoint_map = NULL, opts = list()) {

  stopifnot(!is.null(pool))
  if (is.null(units) || !length(units))
    stop("commit_assay_import(): no units to commit", call. = FALSE)

  cat(sprintf("[assay_import] commit: project_id(len %d) study(len %d) exp(len %d)\n",
              length(scope$project_id), length(scope$study), length(scope$experiment)))

  project_id <- as.integer(scope$project_id)[1]
  study      <- as.character(scope$study)[1]
  user       <- if (is.null(scope$user)) "import" else as.character(scope$user)[1]
  skip_exist <- if (is.null(opts$skip_existing)) TRUE else isTRUE(opts$skip_existing)

  if (is.na(project_id) || project_id < AI_MIN_PROJECT_ID)
    stop(sprintf("commit_assay_import(): project_id must be an integer >= %d",
                 AI_MIN_PROJECT_ID), call. = FALSE)
  if (is.na(study) || !nzchar(study))
    stop("commit_assay_import(): scope$study is required", call. = FALSE)

  zero_counts <- list(header = 0L, samples = 0L, standards = 0L, blanks = 0L,
                      controls = 0L, antigens = 0L, visits = 0L, curves = 0L)

  result <- tryCatch({
    pool::poolWithTransaction(pool, function(conn) {

      counts   <- zero_counts
      exp_done <- character()

      # ── study-level planned visits, inserted once ───────────────────────
      if (!is.null(timepoint_map) && nrow(timepoint_map) > 0) {
        cat(sprintf("[commit] planned visits (%d rows)\n", nrow(timepoint_map)))
        counts$visits <- land_planned_visits(
          conn = conn, timepoint_map = timepoint_map, study_accession = study
        )
      }

      # ── per-experiment units ─────────────────────────────────────────────
      for (u in units) {
        exp_acc <- if (is.null(u$experiment)) scope$experiment else u$experiment
        exp_acc <- as.character(exp_acc)[1]
        if (is.na(exp_acc) || !nzchar(exp_acc))
          stop("commit_assay_import(): unit has no experiment_accession",
               call. = FALSE)

        # Force scope columns onto every frame so a reader can't leak a stale
        # experiment/project into a table. xmap_* frames use DB names; the
        # antigen frame keeps TEMPLATE names (study_name/experiment_name) because
        # prepare_batch_antigen_family() selects and renames those.
        stamp <- function(df) {
          if (is.null(df) || !nrow(df)) return(df)
          df$project_id           <- project_id
          df$study_accession      <- study
          df$experiment_accession <- exp_acc
          df
        }
        stamp_antigen <- function(df) {
          if (is.null(df) || !nrow(df)) return(df)
          df$project_id      <- project_id
          df$study_name      <- study
          df$experiment_name <- exp_acc
          df
        }
        header    <- stamp(u$header)
        samples   <- stamp(u$samples)
        standards <- stamp(u$standards)
        blanks    <- stamp(u$blanks)
        controls  <- stamp(u$controls)
        antigens  <- stamp_antigen(u$antigen_list)

        # ── duplicate guard ────────────────────────────────────────────────
        if (skip_exist && !is.null(header) && "plate_id" %in% names(header)) {
          cat(sprintf("[commit] %s: duplicate-plate check\n", exp_acc))
          plate_ids <- unique(header$plate_id)
          existing  <- tryCatch(
            check_existing_plates(conn = conn, project_id = project_id,
                                  study_accession = study,
                                  experiment_accession = exp_acc,
                                  plateids = plate_ids),
            error = function(e) NULL)
          if (!is.null(existing) && nrow(existing) > 0)
            stop(sprintf("plates already exist for %s / %s / %s — nothing committed",
                         project_id, study, exp_acc), call. = FALSE)
        }

        # ── header ─────────────────────────────────────────────────────────
        if (!is.null(header) && nrow(header) > 0) {
          cat(sprintf("[commit] %s: header insert (%d rows)\n", exp_acc, nrow(header)))
          if ("acquisition_date" %in% names(header))
            header$acquisition_date <-
              standardize_date_for_postgres(header$acquisition_date)
          .insert_or_die(conn, "xmap_header", header,
                         c("project_id", "study_accession", "plate_id"),
                         "header")
          counts$header <- counts$header + nrow(header)
        }

        # ── samples ──────────────────────────────────────────────────────────
        if (!is.null(samples) && nrow(samples) > 0) {
          cat(sprintf("[commit] %s: sample insert (%d rows)\n", exp_acc, nrow(samples)))
          .insert_or_die(conn, "xmap_sample", samples,
                         c("project_id", "study_accession", "plate_id", "well"),
                         "sample")
          counts$samples <- counts$samples + nrow(samples)
        }

        # ── standards -> curve_lookup (HARD FAIL) ────────────────────────────
        if (!is.null(standards) && nrow(standards) > 0) {
          cat(sprintf("[commit] %s: standard insert (%d rows)\n", exp_acc, nrow(standards)))
          .insert_or_die(conn, "xmap_standard", standards,
                         c("project_id", "study_accession", "plate_id", "well"),
                         "standard")
          counts$standards <- counts$standards + nrow(standards)

          cat(sprintf("[commit] %s: register_curve_lookup\n", exp_acc))
          cl <- register_curve_lookup(conn = conn, standards_df = standards,
                                      project_id = project_id)
          if (!isTRUE(cl$success))
            stop(sprintf("curve_lookup registration failed for %s: %s",
                         exp_acc, if (is.null(cl$message)) "unknown error"
                                  else paste(cl$message, collapse = "; ")),
                 call. = FALSE)
          counts$curves <- counts$curves + (cl$rows_inserted %||% 0L)
        }

        # ── blanks / controls ────────────────────────────────────────────────
        if (!is.null(blanks) && nrow(blanks) > 0) {
          cat(sprintf("[commit] %s: blank insert (%d rows)\n", exp_acc, nrow(blanks)))
          .insert_or_die(conn, "xmap_buffer", blanks,
                         c("project_id", "study_accession", "plate_id", "well"),
                         "blank")
          counts$blanks <- counts$blanks + nrow(blanks)
        }
        if (!is.null(controls) && nrow(controls) > 0) {
          cat(sprintf("[commit] %s: control insert (%d rows)\n", exp_acc, nrow(controls)))
          .insert_or_die(conn, "xmap_control", controls,
                         c("project_id", "study_accession", "plate_id", "well"),
                         "control")
          counts$controls <- counts$controls + nrow(controls)
        }

        # ── antigen family + cascade (AFTER curve_lookup) ────────────────────
        if (!is.null(antigens) && nrow(antigens) > 0) {
          cat(sprintf("[commit] %s: antigen family + cascade (%d rows)\n", exp_acc, nrow(antigens)))
          if (!"feature" %in% names(antigens))
            stop(sprintf("antigen_list for %s has no 'feature' column (11.10 requires per-feature antigens)",
                         exp_acc), call. = FALSE)
          a_n <- land_antigen_family(
            conn = conn, antigen_import_list = antigens,
            project_id = project_id, study_accession = study,
            experiment_accession = exp_acc, user = user
          )
          counts$antigens <- counts$antigens + (a_n %||% 0L)
        }

        cat(sprintf("[commit] %s: done\n", exp_acc))
        exp_done <- c(exp_done, exp_acc)
      }

      list(success = TRUE, counts = counts, experiments = exp_done,
           message = .summarise_counts(counts, exp_done))
    })
  },
  error = function(e) {
    list(success = FALSE, counts = zero_counts, experiments = character(),
         message = paste("Import rolled back:", conditionMessage(e)))
  })

  result
}


# ---- internal helpers -------------------------------------------------------

# Feature-inclusive antigen-family landing (11.10 Phase 1).
#
# Converges bead/flow onto the same landing as ELISA, but fixes the dedup key:
# upload_antigen_family()/get_existing_antigens() key on (study, experiment,
# antigen) WITHOUT feature, which would wrongly skip a second feature of an
# antigen already present. This reuses the kept primitives
# (prepare_batch_antigen_family, insert_new_rows, write_antigen_settings_to_cascade)
# with a (study, experiment, antigen, feature) key and a feature-aware existing
# read, touching no legacy file.
#
# Runtime seam dependencies: prepare_batch_antigen_family() [batch_layout_functions.R],
# insert_new_rows(), write_antigen_settings_to_cascade() [plate_validator_functions.R].
land_antigen_family <- function(conn, antigen_import_list, project_id,
                                study_accession, experiment_accession,
                                user = "import") {

  family_df <- prepare_batch_antigen_family(antigen_import_list)
  if (is.null(family_df) || !nrow(family_df)) return(0L)

  # feature-aware existing read (get_existing_antigens omits feature)
  existing <- tryCatch(
    DBI::dbGetQuery(conn,
      "SELECT study_accession, experiment_accession, antigen, feature
         FROM madi_results.xmap_antigen_family
        WHERE study_accession = $1 AND experiment_accession = $2",
      params = list(study_accession, experiment_accession)),
    error = function(e)
      data.frame(study_accession = character(), experiment_accession = character(),
                 antigen = character(), feature = character(),
                 stringsAsFactors = FALSE))

  join_keys <- c("study_accession", "experiment_accession", "antigen")
  if ("feature" %in% names(family_df) && "feature" %in% names(existing))
    join_keys <- c(join_keys, "feature")

  # guard against duplicate (antigen[, feature]) rows in the template — insert_new_rows
  # anti-joins against EXISTING but does not dedup WITHIN new_data.
  dk <- intersect(join_keys, names(family_df))
  family_df <- family_df[!duplicated(family_df[, dk, drop = FALSE]), , drop = FALSE]

  n <- insert_new_rows(
    conn = conn, schema = "madi_results", table = "xmap_antigen_family",
    new_data = family_df, existing_data = existing,
    join_keys = join_keys, label = "antigen family"
  )

  # 11.9a: route fit-settings to the cascade (curve_lookup already registered).
  write_antigen_settings_to_cascade(conn, family_df, project_id,
                                    study_accession, experiment_accession, user)
  n
}

# insert_to_table wrapper that turns a failed insert into a stop() so the
# surrounding transaction rolls back (insert_to_table itself returns a status
# list rather than throwing).
# Real column names of a table (for trimming insert frames so stray columns —
# e.g. ELISA's timepoint 'time_unit' that xmap_planned_visit lacks — don't make
# COPY fail). Cached per (schema,table) within a call is unnecessary; the query
# is cheap. Returns character(0) on failure (caller then skips trimming).
.table_columns <- function(conn, schema, table) {
  tryCatch(
    DBI::dbGetQuery(conn,
      "SELECT column_name FROM information_schema.columns
        WHERE table_schema = $1 AND table_name = $2",
      params = list(schema, table))$column_name,
    error = function(e) character())
}

.insert_or_die <- function(conn, table, data, required_cols, label) {
  # Flatten any list-columns to scalar character before insert. A list-column
  # (e.g. per-plate wavelengths holding c(450, 620)) cannot be sent by
  # dbAppendTable and makes RPostgres drop the backend connection ("server
  # closed the connection unexpectedly"). Collapse to a single string per row.
  if (!is.null(data) && ncol(data) > 0) {
    for (col in names(data)) {
      if (is.list(data[[col]])) {
        data[[col]] <- vapply(data[[col]], function(x) {
          if (is.null(x) || length(x) == 0) return(NA_character_)
          paste(as.character(x), collapse = ", ")
        }, character(1))
      }
    }
    # Trim to the table's real columns (required cols are validated inside
    # insert_to_table, so dropping extras here is safe and prevents
    # "column ... does not exist" COPY failures).
    tcols <- .table_columns(conn, "madi_results", table)
    if (length(tcols)) {
      keep <- intersect(names(data), tcols)
      if (length(keep)) data <- data[, keep, drop = FALSE]
    }
  }
  res <- insert_to_table(conn, "madi_results", table, data, label,
                         required_cols = required_cols)
  if (!isTRUE(res$success))
    stop(sprintf("%s insert failed: %s", label,
                 if (is.null(res$message)) "unknown error"
                 else paste(res$message, collapse = "; ")), call. = FALSE)
  invisible(res)
}

# Planned visits go through insert_new_rows (dedup), not .insert_or_die, so trim
# to the real table columns here too before the COPY.
land_planned_visits <- function(conn, timepoint_map, study_accession) {
  planned <- prepare_planned_visits(timepoint_map = timepoint_map)
  if (is.null(planned) || !nrow(planned)) return(0L)
  if (!"study_accession" %in% names(planned)) planned$study_accession <- study_accession

  tcols <- .table_columns(conn, "madi_results", "xmap_planned_visit")
  if (length(tcols)) {
    keep <- intersect(names(planned), tcols)
    if (length(keep)) planned <- planned[, keep, drop = FALSE]
  } else {
    planned$time_unit <- NULL   # fallback: drop the known offender
  }

  existing <- get_existing_visits(conn, study_accession)
  insert_new_rows(conn = conn, schema = "madi_results", table = "xmap_planned_visit",
                  new_data = planned, existing_data = existing,
                  join_keys = c("study_accession", "timepoint_name"),
                  label = "planned visit")
}

.summarise_counts <- function(counts, experiments) {
  sprintf(
    paste0("Imported %d experiment(s) [%s] — header:%d samples:%d standards:%d ",
           "blanks:%d controls:%d antigens:%d visits:%d curves:%d"),
    length(experiments), paste(experiments, collapse = ", "),
    counts$header, counts$samples, counts$standards, counts$blanks,
    counts$controls, counts$antigens, counts$visits, counts$curves
  )
}

# local %||% (backend is sourced independently of the contract file's copy)
`%||%` <- function(a, b) if (is.null(a)) b else a
