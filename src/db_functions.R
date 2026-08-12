# ---- Wavelength sentinel — used instead of NA/NULL so that SQL UNIQUE constraints ----
# and R joins work correctly for bead-array data (no wavelength).
# Must match the value used in the DB migration.
# 
WL_NONE <- "__none__"

#' Replace NA or empty wavelength values with the sentinel
normalize_wavelength <- function(x) {
 ifelse(is.na(x) | trimws(x) == "", WL_NONE, as.character(x))
}

fetch_study_parameters <- function(study_accession, param_user, param_group = "standard_curve_options", project_id = userWorkSpaceID(), conn) {
  query <- glue("
  SELECT study_accession, param_name, param_boolean_value, param_character_value
	FROM madi_results.xmap_study_config
  WHERE project_id = {project_id}
  AND study_accession = '{study_accession}'
  AND param_user = '{param_user}'
  AND param_group = '{param_group}';
")
  study_parameters <- dbGetQuery(conn, query)
  return(list(
    applyProzone = study_parameters[study_parameters$param_name=="applyProzone", "param_boolean_value"],
    blank_option = study_parameters[study_parameters$param_name=="blank_option", "param_character_value"],
    standard_source = study_parameters[study_parameters$param_name=="default_source", "param_character_value"],
    is_log_response = study_parameters[study_parameters$param_name=="is_log_mfi_axis", "param_boolean_value"],
    is_log_independent = TRUE,
    mean_mfi = study_parameters[study_parameters$param_name=="mean_mfi", "param_boolean_value"]
  ))
}

fetch_antigen_parameters <- function(study_accession, experiment_accession, project_id, conn) {
  query <- glue("
  SELECT
    xmap_antigen_family_id,
    study_accession,
    experiment_accession,
    antigen,
    l_asy_min_constraint,
    l_asy_max_constraint,
    l_asy_constraint_method,
    standard_curve_concentration,
    pcov_threshold
  FROM madi_results.xmap_antigen_family
  WHERE project_id = {project_id}
  AND study_accession = '{study_accession}'
  AND experiment_accession = '{experiment_accession}'
  AND l_asy_constraint_method IS NOT NULL;
")
  antigen_constraints <- dbGetQuery(conn, query)
  return(antigen_constraints=antigen_constraints)
}

fetch_db_header <- function(study_accession, experiment_accession, project_id, conn) {
  query <- glue("SELECT study_accession, experiment_accession, plateid, plate, nominal_sample_dilution,plate_id,
  assay_response_variable, assay_independent_variable, nominal_sample_dilution, project_id
  FROM madi_results.xmap_header
WHERE project_id = {project_id}
AND study_accession = '{study_accession}'
AND experiment_accession = '{experiment_accession}'
")
  header_data <- dbGetQuery(conn, query)
  header_data <- distinct(header_data)
  return(header_data)
}

fetch_db_header_experiments <- function(study_accession, conn, verbose = TRUE) {
  query <- glue("SELECT study_accession, experiment_accession, plateid, plate, nominal_sample_dilution, plate_id,
  assay_response_variable, assay_independent_variable
  FROM madi_results.xmap_header
WHERE study_accession = '{study_accession}'
")
  header_data <- dbGetQuery(conn, query)
  header_data <- distinct(header_data)
  return(header_data)
}

fetch_db_standards <- function(study_accession, experiment_accession, project_id, conn) {
  query <- glue("SELECT study_accession, experiment_accession, feature, plate_id, stype, source, wavelength, sampleid, well, dilution, antigen, antibody_mfi AS mfi, nominal_sample_dilution
  FROM madi_results.xmap_standard
WHERE project_id = {project_id}
AND study_accession = '{study_accession}'
AND experiment_accession = '{experiment_accession}'
")
  standard_df  <- dbGetQuery(conn, query)
  standard_df <- distinct(standard_df)
  return(standard_df)
}

fetch_db_buffer <- function(study_accession, experiment_accession, project_id, conn) {
  query <- glue("SELECT study_accession, experiment_accession, plate_id, stype, source, wavelength, well, antigen, dilution, 
  feature, antibody_mfi AS mfi, nominal_sample_dilution 
  FROM madi_results.xmap_buffer
WHERE project_id = {project_id}
AND study_accession = '{study_accession}'
AND experiment_accession = '{experiment_accession}'
")
  blank_data <- dbGetQuery(conn, query)
  blank_data <- distinct(blank_data)
  return(blank_data)
}

fetch_db_controls <- function(study_accession, experiment_accession, project_id, conn) {
  query <- glue("SELECT study_accession, experiment_accession, plate_id, well, stype, source, wavelength, dilution, pctaggbeads, samplingerrors, antigen, antibody_mfi as MFI, antibody_n
                    feature, project_id, plateid, nominal_sample_dilution, plate
                  	FROM madi_results.xmap_control
              WHERE project_id = {project_id}
              AND study_accession = '{study_accession}'
              AND experiment_accession = '{experiment_accession}';")

  control_data <- dbGetQuery(conn, query)
  control_data <- distinct(control_data)

  return(control_data)
}

fetch_db_samples <- function(study_accession, experiment_accession, project_id, conn) {
  query <- glue("SELECT study_accession,
experiment_accession, plate_id, timeperiod, patientid,
well, stype, source, wavelength, sampleid,  agroup, dilution, pctaggbeads, samplingerrors, antigen, antibody_mfi AS mfi,
antibody_n, nominal_sample_dilution, feature FROM madi_results.xmap_sample
WHERE project_id = {project_id}
AND study_accession = '{study_accession}'
AND experiment_accession = '{experiment_accession}'
")
  sample_data <- dbGetQuery(conn, query)
  sample_data <- distinct(sample_data)
  return(sample_data)
}

fix_source_nom <- function(df, std_prefix) {
  
  suffix <- sub("^[^|]*", "", df$source_nom)
  df$source_nom <- paste0(std_prefix, suffix)
  
  df
}

apply_source_nom <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  
  # Normalize wavelength
  if ("wavelength" %in% names(df)) {
    df$wavelength <- normalize_wavelength(df$wavelength)
  } else {
    df$wavelength <- WL_NONE
  }
  
  # Build source_nom (same as pull_data)
  if ("source" %in% names(df)) {
    df$source_nom <- build_source_nom(df$source, df$wavelength)
  }
  
  # Build plate_nom if plate and nominal_sample_dilution exist
  if (all(c("plate", "nominal_sample_dilution") %in% names(df))) {
    df$plate_nom <- paste0(df$plate, "-", df$nominal_sample_dilution)
  }
  
  df
}

#' Overwrite source with source|wavelength for ELISA data
#' For bead array (wavelength == WL_NONE or missing), source is left unchanged.
enrich_source_with_wavelength <- function(df) {
  if (!"wavelength" %in% names(df) || !"source" %in% names(df)) return(df)
  
  src <- as.character(df$source)
  src[is.na(src) | trimws(src) == ""] <- "unknown"
  wl  <- as.character(df$wavelength)
  
  df$source <- ifelse(
    is.na(wl) | trimws(wl) == "" | wl == WL_NONE,
    src,
    paste0(src, "|", wl, "_nm")
  )
  
  df
}

# align_source_prefixes <- function(standards_df, target_df) {
#   if (is.null(target_df) || nrow(target_df) == 0) return(target_df)
#   if (is.null(standards_df) || nrow(standards_df) == 0) return(target_df)
#   
#   std_prefixes <- unique(sub("\\|.*$", "", standards_df$source_nom))
#   tgt_prefixes <- unique(sub("\\|.*$", "", target_df$source_nom))
#   
#   if (length(std_prefixes) == 1 && length(tgt_prefixes) == 1 &&
#       std_prefixes != tgt_prefixes) {
#     message(sprintf("  Aligning source_nom prefix: '%s' -> '%s'", 
#                     tgt_prefixes, std_prefixes))
#     target_df$source_nom <- sub(
#       paste0("^", gsub("([.|()\\^{}+$*?])", "\\\\\\1", tgt_prefixes)),
#       std_prefixes,
#       target_df$source_nom
#     )
#   }
#   
#   target_df
# }

shiny_notify <- function(session = shiny::getDefaultReactiveDomain()) {
  function(msg) {
    shiny::showNotification(
      msg,
      type = "message",
      session = session
    )
  }
}

# ---- DIAGNOSTIC: Pre-upsert data inspector ----
#' ---- Diagnose potential duplicate-key issues before upserting ----
#' Call this immediately before upsert_best_curve() to see exactly what
#' the data looks like in the natural-key space.  The output goes to the
#' R console (message()) and is also returned invisibly as a list.
#'
#' @param df         data.frame to be upserted
#' @param table      target table name (used to look up natural keys)
#' @param conn       optional DBI connection – if supplied the function also
#'                   checks which rows already exist in the DB on the NK
#' @param schema     DB schema (default "madi_results")
#' @return invisibly, a list with elements: nk, n_rows, n_distinct_nk,
#'         dup_keys (data.frame of duplicated NK combos), wavelength_info
diagnose_upsert_data <- function(df, table, conn = NULL, schema = "madi_results") {

  nk <- get_natural_keys(table)
  if (is.null(nk)) {
    message("[diagnose] Unknown table: ", table)
    return(invisible(NULL))
  }

  hdr <- paste0(
    "\n",
    strrep("=", 70), "\n",
    "  UPSERT DIAGNOSTIC — table: ", table, "\n",
    strrep("=", 70)
  )
  message(hdr)

  # ----- basic shape -----
  message("  Incoming rows        : ", nrow(df))
  message("  Incoming columns     : ", paste(names(df), collapse = ", "))
  message("  Natural key columns  : ", paste(nk, collapse = ", "))

  missing_nk <- setdiff(nk, names(df))
  if (length(missing_nk) > 0) {
    message("  ** MISSING NK cols   : ", paste(missing_nk, collapse = ", "))
  }

  present_nk <- intersect(nk, names(df))
  
  message("\n  --- HEAD of incoming data ---")
  print(head(df))

  # ----- NA audit per NK column -----
  for (col in present_nk) {
    n_na <- sum(is.na(df[[col]]))
    n_empty <- if (is.character(df[[col]])) sum(trimws(df[[col]]) == "", na.rm = TRUE) else 0
    uniq <- length(unique(df[[col]]))
    message(sprintf("  %-30s  NA=%d  empty=%d  unique=%d  sample: %s",
                    col, n_na, n_empty, uniq,
                    paste(head(unique(df[[col]]), 4), collapse = ", ")))
  }

  # ----- wavelength-specific diagnostics -----
  wl_info <- NULL
  if ("wavelength" %in% names(df)) {
    wl_vals    <- unique(df$wavelength)
    wl_na      <- sum(is.na(df$wavelength))
    wl_sentinel <- sum(df$wavelength == WL_NONE, na.rm = TRUE)
    wl_info    <- list(values = wl_vals, n_na = wl_na, n_sentinel = wl_sentinel)
    message("  wavelength values    : ", paste(wl_vals, collapse = ", "),
            "  (NA: ", wl_na, ", sentinel '", WL_NONE, "': ", wl_sentinel, ")")
    if (wl_na > 0) {
      message("  ** WARNING: wavelength has NA values — these should be '", WL_NONE, "'")
    }
  } else {
    message("  wavelength column    : NOT PRESENT in data")
  }

  # ----- duplicate detection on NK -----
  if (length(present_nk) > 0) {
    nk_df   <- df[, present_nk, drop = FALSE]
    dup_idx <- duplicated(nk_df)
    n_dup   <- sum(dup_idx)
    message("  Distinct NK combos   : ", nrow(unique(nk_df)))
    message("  Duplicate NK rows    : ", n_dup)

    if (n_dup > 0) {
      dup_keys <- unique(nk_df[dup_idx, , drop = FALSE])
      message("  ** DUPLICATE NK combos (first 5):")
      print(head(dup_keys, 5))
    } else {
      dup_keys <- data.frame()
    }
  } else {
    dup_keys <- data.frame()
  }

  # ----- check for within-data duplicates on the DB constraint (6-col) -----
  # This catches the case where NK in R has more columns than DB constraint
  nk_no_wl <- setdiff(present_nk, "wavelength")
  if (length(nk_no_wl) > 0 && "wavelength" %in% present_nk) {
    nk6_df <- df[, nk_no_wl, drop = FALSE]
    dup6   <- sum(duplicated(nk6_df))
    message("  Distinct NK combos WITHOUT wavelength : ", nrow(unique(nk6_df)))
    message("  Duplicate rows WITHOUT wavelength     : ", dup6)
    if (dup6 > 0) {
      message("  ** WARNING: Data has rows that differ ONLY by wavelength.")
      message("     If the DB unique constraint does NOT include wavelength,")
      message("     the INSERT will fail with a duplicate-key violation.")
    }
  }

  # ----- optional: check DB for existing rows -----
  if (!is.null(conn) && DBI::dbIsValid(conn) && length(present_nk) > 0) {
    tryCatch({
      # Sample first NK combo to check
      sample_row <- df[1, present_nk, drop = FALSE]
      conditions <- vapply(present_nk, function(col) {
        val <- sample_row[[col]]
        if (is.na(val)) {
          paste0(col, " IS NULL")
        } else {
          paste0(col, " = '", gsub("'", "''", as.character(val)), "'")
        }
      }, character(1))
      check_sql <- sprintf(
        "SELECT COUNT(*) AS n FROM %s.%s WHERE %s",
        schema, table, paste(conditions, collapse = " AND ")
      )
      existing_n <- DBI::dbGetQuery(conn, check_sql)$n
      message("  Existing DB rows matching first NK combo: ", existing_n)
    }, error = function(e) {
      message("  DB check skipped: ", conditionMessage(e))
    })
  }

  message(strrep("=", 70), "\n")

  invisible(list(
    nk             = nk,
    n_rows         = nrow(df),
    n_distinct_nk  = if (length(present_nk) > 0) nrow(unique(df[, present_nk, drop = FALSE])) else NA,
    dup_keys       = dup_keys,
    wavelength_info = wl_info
  ))
}

get_primary_key <- function(table) {
  keys <- list(
    best_plate_all = "best_plate_all_id",
    best_glance_all = "best_glance_all_id",
    best_tidy_all = "best_tidy_all_id",
    best_sample_se_all = "best_sample_se_all_id",
    best_standard_all = "best_standard_all_id",
    best_pred_all = "best_pred_all_id"
  )
  keys[[table]]
}

## Build SQL components for primary key based upsert
build_sql_components_pk <- function(conn, schema, table, cols, nk, pk) {
  schema_id <- as.character(DBI::dbQuoteIdentifier(conn, schema))
  table_id <- as.character(DBI::dbQuoteIdentifier(conn, table))

  cols_quoted <- vapply(cols, function(x) {
    as.character(DBI::dbQuoteIdentifier(conn, x))
  }, character(1), USE.NAMES = FALSE)

  nk_quoted <- vapply(nk, function(x) {
    as.character(DBI::dbQuoteIdentifier(conn, x))
  }, character(1), USE.NAMES = FALSE)

  pk_quoted <- as.character(DBI::dbQuoteIdentifier(conn, pk))

  cols_list <- paste(cols_quoted, collapse = ", ")
  nk_list <- paste(nk_quoted, collapse = ", ")

  # Build WHERE clause for natural key matching
  # Use IS NOT DISTINCT FROM instead of = to handle NULL values correctly
  # (NULL = NULL evaluates to NULL/FALSE in SQL, but IS NOT DISTINCT FROM
  #  treats two NULLs as equal, which is the correct behavior for NK matching)
  nk_conditions <- vapply(nk, function(x) {
    col_quoted <- as.character(DBI::dbQuoteIdentifier(conn, x))
    paste0("t.", col_quoted, " IS NOT DISTINCT FROM tmp.", col_quoted)
  }, character(1), USE.NAMES = FALSE)

  nk_where_clause <- paste(nk_conditions, collapse = " AND ")

  # Build UPDATE SET clause for non-key columns
  update_cols <- setdiff(cols, c(nk, pk))
  if (length(update_cols) > 0) {
    update_quoted <- vapply(update_cols, function(x) {
      as.character(DBI::dbQuoteIdentifier(conn, x))
    }, character(1), USE.NAMES = FALSE)
    set_clause <- paste(
      vapply(update_quoted, function(col) paste0(col, " = tmp.", col), character(1)),
      collapse = ", "
    )
  } else {
    set_clause <- NULL
  }

  # Columns for INSERT (excluding primary key - let it auto-generate)
  insert_cols <- setdiff(cols, pk)
  insert_cols_quoted <- vapply(insert_cols, function(x) {
    as.character(DBI::dbQuoteIdentifier(conn, x))
  }, character(1), USE.NAMES = FALSE)
  insert_cols_list <- paste(insert_cols_quoted, collapse = ", ")

  list(
    schema_id = schema_id,
    table_id = table_id,
    pk = pk,
    pk_quoted = pk_quoted,
    cols = cols,
    cols_list = cols_list,
    insert_cols = insert_cols,
    insert_cols_list = insert_cols_list,
    nk = nk,
    nk_list = nk_list,
    nk_where_clause = nk_where_clause,
    set_clause = set_clause
  )
}

## ────────────────────────────────────────────────────────────────────
## PLATE SCOPE columns — the leading prefix of every UNIQUE constraint.
## Within a batch, ALL rows for each plate scope are regenerated,
## so we can safely DELETE by scope (fast index scan) rather than
## matching every row on the full 11-14 column NK (slow IS NOT DISTINCT FROM).
## ────────────────────────────────────────────────────────────────────
SCOPE_COLS <- c("project_id", "study_accession", "experiment_accession",
                "plateid", "plate", "nominal_sample_dilution")

## Execute single batch using SCOPED DELETE + bulk INSERT
## This replaces the old row-level IS NOT DISTINCT FROM join with a
## set-level DELETE by plate scope, which is orders of magnitude faster
## because it uses the leading columns of the UNIQUE constraint B-tree index.
upsert_batch_pk <- function(conn, df, table, sql_parts, use_copy, notify,
                            shiny_mode = TRUE) {
  
  tryCatch({
    DBI::dbWithTransaction(conn, {
      tmp_name <- paste0("tmp_", substr(digest::digest(Sys.time()), 1, 8))
      tmp_id   <- as.character(DBI::dbQuoteIdentifier(conn, tmp_name))
      temp_cols <- setdiff(sql_parts$cols, sql_parts$pk)
      df_temp <- df[, temp_cols, drop = FALSE]
      create_sql <- sprintf(
        "CREATE TEMP TABLE %s (%s) ON COMMIT DROP",
        tmp_id,
        paste(sprintf("%s %s",
                      vapply(temp_cols, function(x)
                        as.character(DBI::dbQuoteIdentifier(conn, x)), character(1)),
                      vapply(df_temp, pg_type_map, character(1))
        ), collapse = ", ")
      )
      DBI::dbExecute(conn, create_sql)

      # Load data into temp table using COPY (binary protocol)

      if (use_copy && requireNamespace("RPostgres", quietly = TRUE)) {
        RPostgres::dbWriteTable(
          conn, tmp_name, df_temp,
          append = TRUE, row.names = FALSE, copy = TRUE
        )
      } else {
        DBI::dbWriteTable(conn, tmp_name, df_temp,
                          append = TRUE, row.names = FALSE)
      }

      # Let PostgreSQL know the temp table's size for better query plans
      DBI::dbExecute(conn, sprintf("ANALYZE %s", tmp_id))

      # ── Step 1: Scoped DELETE ──────────────────────────────────────
      # Delete ALL existing rows whose plate-scope matches any incoming
      # plate-scope. This is safe because a batch always regenerates
      # complete results for each plate scope.
      #
      # Uses the leading columns of the UNIQUE index → index scan,
      # not the old 11-14 column IS NOT DISTINCT FROM → seq scan.
      scope_available <- intersect(SCOPE_COLS, temp_cols)
      scope_quoted <- vapply(scope_available, function(x)
        as.character(DBI::dbQuoteIdentifier(conn, x)), character(1), USE.NAMES = FALSE)

      delete_sql <- sprintf(
        "DELETE FROM %s.%s t
         WHERE EXISTS (
           SELECT 1 FROM (
             SELECT DISTINCT %s FROM %s
           ) scope
           WHERE %s
         )",
        sql_parts$schema_id, sql_parts$table_id,
        paste(scope_quoted, collapse = ", "),
        tmp_id,
        paste(sprintf("t.%s = scope.%s", scope_quoted, scope_quoted), collapse = " AND ")
      )
      n_deleted <- DBI::dbExecute(conn, delete_sql)
      message(sprintf("[upsert_batch_pk] %s: scoped DELETE removed %d existing rows", table, n_deleted))
      
      # delete_sql <- sprintf(
      #   "DELETE FROM %s.%s t USING %s tmp WHERE %s",
      #   sql_parts$schema_id, sql_parts$table_id,
      #   tmp_id, sql_parts$nk_where_clause
      # )
      # DBI::dbExecute(conn, delete_sql)

      # ── Step 2: Bulk INSERT ────────────────────────────────────────

      insert_sql <- sprintf(
        "INSERT INTO %s.%s (%s) SELECT %s FROM %s",
        sql_parts$schema_id, sql_parts$table_id,
        sql_parts$insert_cols_list, sql_parts$insert_cols_list, tmp_id
      )
      n_inserted <- DBI::dbExecute(conn, insert_sql)
      message(sprintf("[upsert_batch_pk] %s: INSERT added %d rows", table, n_inserted))
    })
    TRUE
  }, error = function(e) {
    msg <- paste0("Batch failed for ", table, ": ", conditionMessage(e))
    if (shiny_mode && !is.null(shiny::getDefaultReactiveDomain())) {
      showNotification(
        id = "error_batch", msg,
        duration = NULL, closeButton = TRUE, type = "error"
      )
    } else {
      message(msg)
    }
    FALSE
  })
}

## ────────────────────────────────────────────────────────────────────
## Feature sentinel — same pattern as wavelength.
## Use AFTER running migration_phase2_not_null_nk.sql.
## ────────────────────────────────────────────────────────────────────
FEAT_NONE <- "__none__"

#' Replace NA or empty feature values with the sentinel
normalize_feature <- function(x) {
  ifelse(is.na(x) | trimws(x) == "", FEAT_NONE, as.character(x))
}

## ────────────────────────────────────────────────────────────────────
## Phase 2: ON CONFLICT upsert (requires NOT NULL NK columns)
## ────────────────────────────────────────────────────────────────────
## Activate by passing use_on_conflict = TRUE to upsert_best_curve
## AFTER running migration_phase2_not_null_nk.sql.
##
## This is the fastest possible upsert strategy:
## - Single SQL statement: INSERT ... ON CONFLICT ... DO UPDATE SET
## - PostgreSQL uses the B-tree index on the UNIQUE constraint directly
## - No DELETE, no two-pass, no IS NOT DISTINCT FROM
## ────────────────────────────────────────────────────────────────────
upsert_batch_on_conflict <- function(conn, df, table, sql_parts, use_copy, notify) {
  tryCatch({
    DBI::dbWithTransaction(conn, {
      tmp_name <- paste0("tmp_", substr(digest::digest(Sys.time()), 1, 8))
      tmp_id <- as.character(DBI::dbQuoteIdentifier(conn, tmp_name))

      temp_cols <- setdiff(sql_parts$cols, sql_parts$pk)
      df_temp <- df[, temp_cols, drop = FALSE]

      create_sql <- sprintf(
        "CREATE TEMP TABLE %s (%s) ON COMMIT DROP",
        tmp_id,
        paste(sprintf("%s %s",
                      vapply(temp_cols, function(x) as.character(DBI::dbQuoteIdentifier(conn, x)), character(1)),
                      vapply(df_temp, pg_type_map, character(1))
        ), collapse = ", ")
      )
      DBI::dbExecute(conn, create_sql)

      if (use_copy && requireNamespace("RPostgres", quietly = TRUE)) {
        RPostgres::dbWriteTable(
          conn, tmp_name, df_temp,
          append = TRUE, row.names = FALSE, copy = TRUE
        )
      } else {
        DBI::dbWriteTable(conn, tmp_name, df_temp, append = TRUE, row.names = FALSE)
      }

      DBI::dbExecute(conn, sprintf("ANALYZE %s", tmp_id))

      # Build ON CONFLICT upsert using the UNIQUE constraint name
      constraint_name <- paste0(table, "_nk")

      # Non-key, non-PK columns to update on conflict
      update_cols <- setdiff(temp_cols, sql_parts$nk)
      insert_cols_quoted <- vapply(temp_cols, function(x)
        as.character(DBI::dbQuoteIdentifier(conn, x)), character(1), USE.NAMES = FALSE)
      insert_cols_list <- paste(insert_cols_quoted, collapse = ", ")

      if (length(update_cols) > 0) {
        update_quoted <- vapply(update_cols, function(x)
          as.character(DBI::dbQuoteIdentifier(conn, x)), character(1), USE.NAMES = FALSE)
        set_clause <- paste(
          vapply(update_quoted, function(col) paste0(col, " = EXCLUDED.", col), character(1)),
          collapse = ", "
        )
        conflict_action <- paste("DO UPDATE SET", set_clause)
      } else {
        conflict_action <- "DO NOTHING"
      }

      upsert_sql <- sprintf(
        "INSERT INTO %s.%s (%s)
         SELECT %s FROM %s
         ON CONFLICT ON CONSTRAINT %s
         %s",
        sql_parts$schema_id, sql_parts$table_id,
        insert_cols_list,
        insert_cols_list,
        tmp_id,
        DBI::dbQuoteIdentifier(conn, constraint_name),
        conflict_action
      )
      n_upserted <- DBI::dbExecute(conn, upsert_sql)
      message(sprintf("[upsert_batch_on_conflict] %s: ON CONFLICT upserted %d rows", table, n_upserted))
    })
    TRUE
  }, error = function(e) {
    showNotification(
      id = "error_batch",
      paste0("Batch failed for ", table, " (ON CONFLICT): ", conditionMessage(e)),
      duration = NULL, closeButton = TRUE, type = "error"
    )
    FALSE
  })
}

## Map R types to PostgreSQL types
pg_type_map <- function(col) {
  switch(class(col)[1],
         "integer" = "INTEGER",
         "numeric" = "DOUBLE PRECISION",
         "character" = "TEXT",
         "logical" = "BOOLEAN",
         "Date" = "DATE",
         "POSIXct" = "TIMESTAMPTZ",
         "POSIXlt" = "TIMESTAMPTZ",
         "factor" = "TEXT",
         "TEXT"
  )
}

## Helper: Get natural keys for table

get_natural_keys <- function(table) {
  keys <- list(
    best_plate_all = c("project_id",
      "study_accession", "experiment_accession",
      "plateid", "plate", "nominal_sample_dilution", "source", "wavelength"
    ),
    best_glance_all = c("project_id",
      "study_accession", "experiment_accession",
      "plateid", "plate", "nominal_sample_dilution", "source", "wavelength", "antigen", "feature"
    ),
    best_tidy_all = c("project_id",
      "study_accession", "experiment_accession",
      "plateid", "plate", "nominal_sample_dilution", "source", "wavelength", "antigen", "feature", "term"
    ),
    best_sample_se_all = c( "project_id",
      "study_accession", "experiment_accession",
      "plateid", "plate", "nominal_sample_dilution", "source", "wavelength", "antigen", "feature",
      "patientid", "timeperiod", "sampleid", "dilution"
    ),
    best_standard_all = c("project_id",
      "study_accession", "experiment_accession",
      "plateid", "plate", "nominal_sample_dilution", "source", "wavelength", "antigen", "feature", "well"
    ), # dilution not included as it can be NA when geometric mean is used
    best_pred_all = c("project_id",
      "study_accession", "experiment_accession",
      "plateid", "plate", "nominal_sample_dilution", "source", "wavelength", "antigen", "feature", "x"
    )
  )

  keys[[table]]
}

## Helper: Build UPSERT SQL using glue_sql

build_upsert_sql_glue <- function(conn, schema, table, tmp_name, cols, nk) {

  ## Pre-quote identifiers to avoid glue_sql conflicts
  schema_id <- DBI::dbQuoteIdentifier(conn, schema)
  table_id <- DBI::dbQuoteIdentifier(conn, table)
  tmp_id <- DBI::dbQuoteIdentifier(conn, tmp_name)

  ## Quote column names
  cols_quoted <- vapply(cols, function(x) {
    as.character(DBI::dbQuoteIdentifier(conn, x))
  }, character(1), USE.NAMES = FALSE)

  nk_quoted <- vapply(nk, function(x) {
    as.character(DBI::dbQuoteIdentifier(conn, x))
  }, character(1), USE.NAMES = FALSE)

  ## Build column list strings

  cols_list <- paste(cols_quoted, collapse = ", ")
  nk_list <- paste(nk_quoted, collapse = ", ")

  ## Build SET clause for non-key columns
  update_cols <- setdiff(cols, nk)

  if (length(update_cols) > 0) {
    update_quoted <- vapply(update_cols, function(x) {
      as.character(DBI::dbQuoteIdentifier(conn, x))
    }, character(1), USE.NAMES = FALSE)

    set_parts <- vapply(update_quoted, function(col) {
      paste0(col, " = EXCLUDED.", col)
    }, character(1), USE.NAMES = FALSE)

    set_clause <- paste(set_parts, collapse = ", ")
    conflict_action <- paste("DO UPDATE SET", set_clause)
  } else {
    conflict_action <- "DO NOTHING"
  }

  ## Build final SQL using glue_sql with DBI::SQL for pre-quoted parts
  glue::glue_sql(
    "INSERT INTO {DBI::SQL(schema_id)}.{DBI::SQL(table_id)} ({DBI::SQL(cols_list)})
     SELECT {DBI::SQL(cols_list)}
     FROM {DBI::SQL(tmp_id)}
     ON CONFLICT ({DBI::SQL(nk_list)})
     {DBI::SQL(conflict_action)}",
    .con = conn
  )
}

select_antigen_plate <- function(loaded_data,
                                 study_accession = study_accession,
                                 experiment_accession = experiment_accession,
                                 source = source,
                                 antigen = antigen,
                                 plate = plate,
                                 wavelength = WL_NONE,
                                 antigen_constraints = antigen_constraints) {
  print("select antigen plate in batch\n")
  print("plate in\n")
  print(plate)
  print(paste("wavelength in:", wavelength))
  print("standards structure\n")
  print(str(loaded_data$standards))
  print("antigens\n")
  print(unique(loaded_data$standards$antigen))
  print("source_nom\n")
  print(unique(loaded_data$standards$source_nom))
  print("plate_nom\n")
  print(unique(loaded_data$standards$plate_nom))
  print("plate\n")
  print(unique(loaded_data$standards$plate))
  
  # ── Filter standards ───────────────────────────────────────────────
  if ("source_nom" %in% names(loaded_data$standards)) {
    plate_standard <- loaded_data$standards[
      loaded_data$standards$source_nom == source &
        loaded_data$standards$antigen    == antigen &
        loaded_data$standards$plate_nom  == plate, ]
  } else {
    plate_standard <- loaded_data$standards[
      loaded_data$standards$source    == source &
        loaded_data$standards$antigen   == antigen &
        loaded_data$standards$plate_nom == plate, ]
  }
  
  # ── Filter by wavelength for standards ────────────────────────────
  if ("wavelength" %in% names(plate_standard) &&
      !is.null(wavelength) && wavelength != WL_NONE) {
    plate_standard$wavelength <- normalize_wavelength(plate_standard$wavelength)
    wl_filter <- plate_standard$wavelength == normalize_wavelength(wavelength)
    cat("wavelength filter:\n")
    print(wl_filter)
    if (any(wl_filter)) {
      plate_standard <- plate_standard[wl_filter, , drop = FALSE]
    } else {
      message(sprintf(
        "[select_antigen_plate] WARNING: wavelength '%s' matched 0 rows; keeping all %d rows. Wavelengths in data: %s",
        wavelength, nrow(plate_standard),
        paste(unique(plate_standard$wavelength), collapse = ", ")
      ))
    }
  }
  
  # ── Guard against empty plate_standard data ───────────────────────
  if (is.null(plate_standard) || nrow(plate_standard) == 0) {
    warning(paste("No standard curve data found for:",
                  "source =", source,
                  ", antigen =", antigen,
                  ", plate =", plate))
    return(NULL)
  }
  
  # ── Filter blanks ─────────────────────────────────────────────────
  plate_blanks <- loaded_data$blanks[
    loaded_data$blanks$antigen   == antigen &
      loaded_data$blanks$plate_nom == plate, ]
  
  # ── Filter samples ────────────────────────────────────────────────
  plate_samples <- loaded_data$samples[
    loaded_data$samples$antigen   == antigen &
      loaded_data$samples$plate_nom == plate, ]
  
  # # ── Filter mcmc_samples — same pattern as blanks and samples ──────
  # plate_mcmc_samples <- if (!is.null(loaded_data$mcmc_samples) &&
  #                           nrow(loaded_data$mcmc_samples) > 0) {
  #   loaded_data$mcmc_samples[
  #     loaded_data$mcmc_samples$antigen   == antigen &
  #       loaded_data$mcmc_samples$plate_nom == plate, , drop = FALSE]
  # } else {
  #   data.frame()
  # }
  # 
  # # ── Filter mcmc_pred — dense prediction grid with MCMC pCoV ──────
  # plate_mcmc_pred <- if (!is.null(loaded_data$mcmc_pred) &&
  #                        nrow(loaded_data$mcmc_pred) > 0) {
  #   loaded_data$mcmc_pred[
  #     loaded_data$mcmc_pred$antigen   == antigen &
  #       loaded_data$mcmc_pred$plate_nom == plate, , drop = FALSE]
  # } else {
  #   data.frame()
  # }
  
  # ── Filter mcmc_samples — match by antigen, plate_nom, AND source_nom ──
  plate_mcmc_samples <- if (!is.null(loaded_data$mcmc_samples) &&
                            nrow(loaded_data$mcmc_samples) > 0) {
    mcmc_df <- loaded_data$mcmc_samples
    filter_mask <- mcmc_df$antigen == antigen & mcmc_df$plate_nom == plate
    # Also filter by source_nom if available (critical for ELISA multi-wavelength)
    if ("source_nom" %in% names(mcmc_df)) {
      filter_mask <- filter_mask & mcmc_df$source_nom == source
    }
    mcmc_df[filter_mask, , drop = FALSE]
  } else {
    data.frame()
  }
  
  # ── Filter mcmc_pred — match by antigen, plate_nom, AND source_nom ──
  plate_mcmc_pred <- if (!is.null(loaded_data$mcmc_pred) &&
                         nrow(loaded_data$mcmc_pred) > 0) {
    pred_df <- loaded_data$mcmc_pred
    filter_mask <- pred_df$antigen == antigen & pred_df$plate_nom == plate
    # Also filter by source_nom if available (critical for ELISA multi-wavelength)
    if ("source_nom" %in% names(pred_df)) {
      filter_mask <- filter_mask & pred_df$source_nom == source
    }
    pred_df[filter_mask, , drop = FALSE]
  } else {
    data.frame()
  }
  
  # ── Filter blanks, samples, mcmc_samples, mcmc_pred by wavelength ─
  if (!is.null(wavelength) && wavelength != WL_NONE) {
    if ("wavelength" %in% names(plate_blanks) && nrow(plate_blanks) > 0) {
      wl_b <- plate_blanks$wavelength == normalize_wavelength(wavelength)
      if (any(wl_b)) plate_blanks <- plate_blanks[wl_b, , drop = FALSE]
    }
    if ("wavelength" %in% names(plate_samples) && nrow(plate_samples) > 0) {
      wl_s <- plate_samples$wavelength == normalize_wavelength(wavelength)
      if (any(wl_s)) plate_samples <- plate_samples[wl_s, , drop = FALSE]
    }
    if ("wavelength" %in% names(plate_mcmc_samples) && nrow(plate_mcmc_samples) > 0) {
      wl_m <- plate_mcmc_samples$wavelength == normalize_wavelength(wavelength)
      if (any(wl_m)) plate_mcmc_samples <- plate_mcmc_samples[wl_m, , drop = FALSE]
    }
    if ("wavelength" %in% names(plate_mcmc_pred) && nrow(plate_mcmc_pred) > 0) {
      wl_p <- plate_mcmc_pred$wavelength == normalize_wavelength(wavelength)
      if (any(wl_p)) plate_mcmc_pred <- plate_mcmc_pred[wl_p, , drop = FALSE]
    }
  }
  
  # anything after - is removed (nominal sample dilutions)
  plate_c <- sub("-.*$", "", plate)
  
  # ── Resolve response column ───────────────────────────────────────
  response_col <- resolve_response_col(plate_standard)
  
  # ── Antigen settings ──────────────────────────────────────────────
  antigen_settings <- obtain_lower_constraint(
    dat                  = plate_standard,
    antigen              = antigen,
    study_accession      = study_accession,
    experiment_accession = experiment_accession,
    plate                = plate_c,
    plateid              = unique(plate_standard$plateid),
    plate_blanks         = plate_blanks,
    antigen_constraints  = antigen_constraints,
    response_col         = response_col
  )
  
  # ── Fixed lower asymptote ─────────────────────────────────────────
  fixed_a_result <- resolve_fixed_lower_asymptote(antigen_settings)
  fixed_a_result <- validate_fixed_lower_asymptote(
    fixed_a_result_raw = fixed_a_result,
    verbose            = TRUE
  )
  
  # ── Blank standard error ──────────────────────────────────────────
  std_error_blank <- get_blank_se(antigen_settings = antigen_settings)
  
  # ── Sort mcmc_pred by x for smooth line drawing ───────────────────
  if (nrow(plate_mcmc_pred) > 0 && "x" %in% names(plate_mcmc_pred)) {
    plate_mcmc_pred <- plate_mcmc_pred[order(plate_mcmc_pred$x), , drop = FALSE]
  }
  
  # ── Return ────────────────────────────────────────────────────────
  return(list(
    plate_standard     = plate_standard,
    plate_blanks       = plate_blanks,
    plate_samples      = plate_samples,
    plate_mcmc_samples = plate_mcmc_samples,
    plate_mcmc_pred    = plate_mcmc_pred,
    antigen_settings   = antigen_settings,
    fixed_a_result     = fixed_a_result,
    std_error_blank    = std_error_blank
  ))
}
# select_antigen_plate <- function(loaded_data,
#                                  study_accession = study_accession,
#                                  experiment_accession = experiment_accession,
#                                  source = source,
#                                  antigen = antigen,
#                                  plate = plate,
#                                  wavelength = WL_NONE,
#                                  antigen_constraints = antigen_constraints) {
#   
#   print("select antigen plate in batch\n")
#   print("plate in\n")
#   print(plate)
#   print(paste("wavelength in:", wavelength))
#   print("standards structure\n")
#   print(str(loaded_data$standards))
#   print("antigens\n")
#   print(unique(loaded_data$standards$antigen))
#   print("source_nom\n")
#   print(unique(loaded_data$standards$source_nom))
#   print("plate_nom\n")
#   print(unique(loaded_data$standards$plate_nom))
#   print("plate\n")
#   print(unique(loaded_data$standards$plate))
#   
#   # ── Filter standards ───────────────────────────────────────────────
#   if ("source_nom" %in% names(loaded_data$standards)) {
#     plate_standard <- loaded_data$standards[
#       loaded_data$standards$source_nom == source &
#         loaded_data$standards$antigen    == antigen &
#         loaded_data$standards$plate_nom  == plate, ]
#   } else {
#     plate_standard <- loaded_data$standards[
#       loaded_data$standards$source    == source &
#         loaded_data$standards$antigen   == antigen &
#         loaded_data$standards$plate_nom == plate, ]
#   }
#   
#   # ── Filter by wavelength for standards ────────────────────────────
#   if ("wavelength" %in% names(plate_standard) &&
#       !is.null(wavelength) && wavelength != WL_NONE) {
#     plate_standard$wavelength <- normalize_wavelength(plate_standard$wavelength)
#     wl_filter <- plate_standard$wavelength == normalize_wavelength(wavelength)
#     if (any(wl_filter)) {
#       plate_standard <- plate_standard[wl_filter, , drop = FALSE]
#     } else {
#       message(sprintf(
#         "[select_antigen_plate] WARNING: wavelength '%s' matched 0 rows; keeping all %d rows. Wavelengths in data: %s",
#         wavelength, nrow(plate_standard),
#         paste(unique(plate_standard$wavelength), collapse = ", ")
#       ))
#     }
#   }
#   
#   # ── Guard against empty plate_standard data ───────────────────────
#   if (is.null(plate_standard) || nrow(plate_standard) == 0) {
#     warning(paste("No standard curve data found for:",
#                   "source =", source,
#                   ", antigen =", antigen,
#                   ", plate =", plate))
#     return(NULL)
#   }
#   
#   # ── Filter blanks ─────────────────────────────────────────────────
#   plate_blanks <- loaded_data$blanks[
#     loaded_data$blanks$antigen   == antigen &
#       loaded_data$blanks$plate_nom == plate, ]
#   
#   # ── Filter samples ────────────────────────────────────────────────
#   plate_samples <- loaded_data$samples[
#     loaded_data$samples$antigen   == antigen &
#       loaded_data$samples$plate_nom == plate, ]
#   
#   # ── Filter mcmc_samples — same pattern as blanks and samples ──────
#   plate_mcmc_samples <- loaded_data$mcmc_samples[
#     loaded_data$mcmc_samples$antigen   == antigen &
#       loaded_data$mcmc_samples$plate_nom == plate, ]
#   
#   # ── Filter blanks, samples, mcmc_samples by wavelength ────────────
#   if (!is.null(wavelength) && wavelength != WL_NONE) {
#     if ("wavelength" %in% names(plate_blanks) && nrow(plate_blanks) > 0) {
#       wl_b <- plate_blanks$wavelength == normalize_wavelength(wavelength)
#       if (any(wl_b)) plate_blanks <- plate_blanks[wl_b, , drop = FALSE]
#     }
#     if ("wavelength" %in% names(plate_samples) && nrow(plate_samples) > 0) {
#       wl_s <- plate_samples$wavelength == normalize_wavelength(wavelength)
#       if (any(wl_s)) plate_samples <- plate_samples[wl_s, , drop = FALSE]
#     }
#     if ("wavelength" %in% names(plate_mcmc_samples) && nrow(plate_mcmc_samples) > 0) {
#       wl_m <- plate_mcmc_samples$wavelength == normalize_wavelength(wavelength)
#       if (any(wl_m)) plate_mcmc_samples <- plate_mcmc_samples[wl_m, , drop = FALSE]
#     }
#   }
#   
#   # anything after - is removed (nominal sample dilutions)
#   plate_c <- sub("-.*$", "", plate)
#   
#   # ── Resolve response column ───────────────────────────────────────
#   response_col <- resolve_response_col(plate_standard)
#   
#   # ── Antigen settings ──────────────────────────────────────────────
#   antigen_settings <- obtain_lower_constraint(
#     dat                  = plate_standard,
#     antigen              = antigen,
#     study_accession      = study_accession,
#     experiment_accession = experiment_accession,
#     plate                = plate_c,
#     plateid              = unique(plate_standard$plateid),
#     plate_blanks         = plate_blanks,
#     antigen_constraints  = antigen_constraints,
#     response_col         = response_col
#   )
#   
#   # ── Fixed lower asymptote ─────────────────────────────────────────
#   fixed_a_result <- resolve_fixed_lower_asymptote(antigen_settings)
#   fixed_a_result <- validate_fixed_lower_asymptote(
#     fixed_a_result_raw = fixed_a_result,
#     verbose            = TRUE
#   )
#   
#   # ── Blank standard error ──────────────────────────────────────────
#   std_error_blank <- get_blank_se(antigen_settings = antigen_settings)
#   
#   # ── Return ────────────────────────────────────────────────────────
#   return(list(
#     plate_standard     = plate_standard,
#     plate_blanks       = plate_blanks,
#     plate_samples      = plate_samples,
#     plate_mcmc_samples = plate_mcmc_samples,
#     antigen_settings   = antigen_settings,
#     fixed_a_result     = fixed_a_result,
#     std_error_blank    = std_error_blank
#   ))
# }
# select_antigen_plate <- function(loaded_data,
#                                  study_accession = study_accession,
#                                  experiment_accession = experiment_accession,
#                                  source = source,
#                                  antigen = antigen,
#                                  plate = plate,
#                                  wavelength = WL_NONE,
#                                  antigen_constraints = antigen_constraints){
#   print("select antigen plate in batch\n")
#   print("plate in\n")
#   print(plate)
#   print(paste("wavelength in:", wavelength))
#   print("standards structure\n")
#   print(str(loaded_data$standards))
#   print("antigens\n")
#   print(unique(loaded_data$standards$antigen))
#   print("source_nom\n")
#   print(unique(loaded_data$standards$source_nom))
#   print("plate_nom\n")
#   print(unique(loaded_data$standards$plate_nom))
# 
#   print("plate\n")
#   print(unique(loaded_data$standards$plate))
# 
#   # Use source_nom for filtering if available, fall back to source
#   if ("source_nom" %in% names(loaded_data$standards)) {
#     plate_standard  <- loaded_data$standards[loaded_data$standards$source_nom == source &
#                                                loaded_data$standards$antigen == antigen &
#                                                loaded_data$standards$plate_nom == plate ,]
#   } else {
#     plate_standard  <- loaded_data$standards[loaded_data$standards$source == source &
#                                                loaded_data$standards$antigen == antigen &
#                                                loaded_data$standards$plate_nom == plate ,]
#   }
#   
#   # ── Filter by wavelength when data contains multiple wavelengths ──
#   if ("wavelength" %in% names(plate_standard) && 
#       !is.null(wavelength) && wavelength != WL_NONE) {
#     plate_standard$wavelength <- normalize_wavelength(plate_standard$wavelength)
#     wl_filter <- plate_standard$wavelength == normalize_wavelength(wavelength)
#     if (any(wl_filter)) {
#       plate_standard <- plate_standard[wl_filter, , drop = FALSE]
#     } else {
#       message(sprintf(
#         "[select_antigen_plate] WARNING: wavelength '%s' matched 0 rows; keeping all %d rows. Wavelengths in data: %s",
#         wavelength, nrow(plate_standard),
#         paste(unique(plate_standard$wavelength), collapse = ", ")
#       ))
#     }
#   }
#   # Guard against empty plate_standard data
#   if (is.null(plate_standard) || nrow(plate_standard) == 0) {
#     warning(paste("No standard curve data found for:",
#                   "source =", source,
#                   ", antigen =", antigen,
#                   ", plate =", plate))
#     return(NULL)
#   }
# 
#   plate_blanks <- loaded_data$blanks[loaded_data$blanks$antigen == antigen &
#                                        loaded_data$blanks$plate_nom == plate,]
# 
#   plate_samples <- loaded_data$samples[loaded_data$samples$antigen == antigen &
#                                          loaded_data$samples$plate_nom == plate,]
# 
#   # ── Filter blanks and samples by wavelength when relevant ──
#   if (!is.null(wavelength) && wavelength != WL_NONE) {
#     if ("wavelength" %in% names(plate_blanks) && nrow(plate_blanks) > 0) {
#       plate_blanks$wavelength <- normalize_wavelength(plate_blanks$wavelength)
#       wl_b <- plate_blanks$wavelength == normalize_wavelength(wavelength)
#       if (any(wl_b)) plate_blanks <- plate_blanks[wl_b, , drop = FALSE]
#     }
#     if ("wavelength" %in% names(plate_samples) && nrow(plate_samples) > 0) {
#       plate_samples$wavelength <- normalize_wavelength(plate_samples$wavelength)
#       wl_s <- plate_samples$wavelength == normalize_wavelength(wavelength)
#       if (any(wl_s)) plate_samples <- plate_samples[wl_s, , drop = FALSE]
#     }
#   }
# 
#   # anything after - is removed (nominal sample dilutions)
#   plate_c <- sub("-.*$", "", plate)
# 
#   # Resolve response column for this data (mfi for bead array, absorbance for ELISA)
#   response_col <- resolve_response_col(plate_standard)
# 
#   antigen_settings <- obtain_lower_constraint(dat = plate_standard,
#                                               antigen = antigen,
#                                               study_accession = study_accession,
#                                               experiment_accession = experiment_accession,
#                                               plate = plate_c,
# 
#                                               plateid = unique(plate_standard$plateid),
# 
#                                               plate_blanks = plate_blanks,
#                                               antigen_constraints = antigen_constraints,
#                                               response_col = response_col)
# 
# 
#   fixed_a_result <- resolve_fixed_lower_asymptote(antigen_settings)
#   fixed_a_result <- validate_fixed_lower_asymptote(
#     fixed_a_result_raw = fixed_a_result,
#     verbose = TRUE
#   )
# 
#   std_error_blank <- get_blank_se(antigen_settings = antigen_settings)
# 
#   return (list(plate_standard=plate_standard,
#                plate_blanks=plate_blanks,
#                plate_samples=plate_samples,
#                antigen_settings=antigen_settings,
#                fixed_a_result = fixed_a_result,
#                std_error_blank = std_error_blank))
# }

#### Fetch saved results from std_curver
fetch_current_sc_options_wide <- function(currentuser, study_accession, project_id, conn) {
  query <- glue_sql(
    "
SELECT
  study_accession,
  param_user,
  BOOL_OR(CASE WHEN param_name = 'is_log_mfi_axis' THEN param_boolean_value END) AS is_log_mfi_axis,
  MAX(CASE WHEN param_name = 'blank_option' THEN param_character_value END) AS blank_option,
  BOOL_OR(CASE WHEN param_name = 'applyProzone' THEN param_boolean_value END) AS apply_prozone
FROM madi_results.xmap_study_config
WHERE project_id = {project_id}
  AND study_accession = {study_accession}
  AND param_user = {currentuser}
  AND param_name IN ('is_log_mfi_axis', 'blank_option', 'applyProzone')
GROUP BY study_accession, param_user
",
    currentuser     = currentuser,
    study_accession = study_accession,
    .con = conn
  )

  print(query)
  dbGetQuery(conn, query)
}

attach_antigen_familes <- function(best_pred_all, antigen_families, default_family = "All Antigens") {
  # Handle case where antigen_families is NULL or empty
  if (is.null(antigen_families) || nrow(antigen_families) == 0) {
    # Create antigen_family column with default value for all antigens
    best_pred_all$antigen_family <- default_family
    return(best_pred_all)
  }

  # Ensure antigen_families has required columns
  required_cols <- c("study_accession", "antigen", "antigen_family")
  if (!all(required_cols %in% names(antigen_families))) {
    # If required columns are missing, use default family
    best_pred_all$antigen_family <- default_family
    return(best_pred_all)
  }

  # Perform the merge
  pred_with_antigen_familes <- merge(best_pred_all,
                                     antigen_families[, required_cols],
                                     by = c("study_accession", "antigen"),
                                     all.x = TRUE)

  # Replace NA/NULL antigen_family values with the default
  if ("antigen_family" %in% names(pred_with_antigen_familes)) {
    na_family <- is.na(pred_with_antigen_familes$antigen_family) |
      pred_with_antigen_familes$antigen_family == "" |
      is.null(pred_with_antigen_familes$antigen_family)
    pred_with_antigen_familes$antigen_family[na_family] <- default_family
  } else {
    # Column doesn't exist after merge, add it with default
    pred_with_antigen_familes$antigen_family <- default_family
  }

  return(pred_with_antigen_familes)
}
