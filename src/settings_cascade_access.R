# =============================================================================
# settings_cascade_access.R  --  the app's single read/write boundary for the
#                                unified settings cascade (calib_settings*)
# -----------------------------------------------------------------------------
# The app reads AND writes settings through the same resolver the worker uses.
# Replaces: fetch_antigen_feature_settings() (view read -> resolver shim),
# fetch_study_configuration() (xmap_study_config -> resolver), and every
# UPDATE/INSERT madi_results.xmap_study_config handler (-> set_setting /
# unset_setting against calib_settings).
#
# POOL: every function takes `pool` as its first argument. It accepts a
# pool::Pool OR a bare DBI connection -- DBI's generics (dbGetQuery/dbExecute)
# dispatch on Pool, checking a connection out and back per call. Pass the app's
# shared pool object here instead of the old global `conn`; that is the "use
# pool everywhere" win (connection reuse) that helped the standard-curve module.
#
# CONVENTIONS (shared with calib_data_access.R):
#   * values bound as $1,$2,... (RPostgres binds scalars; = ANY() array binds fail)
#   * text scope wildcard = '__none__'; project wildcard/system tier = -1
#   * project_id is the OUTERMOST tier and REQUIRED -- a missing project is a hard
#     error, never a silent resolve-to-system (studies without a project are
#     unsupported).
#
#  CONFIG IS NOW STUDY-SCOPED, NOT PER-USER.  The legacy xmap_study_config keyed
#  config by param_user; the cascade key is (project, study, experiment, feature,
#  antigen)+param_name, and param_user is AUDIT provenance only. Users sharing a
#  study now share its settings (last-write-wins, tagged with the writer). This
#  is intended: collaborators on the same data want the same fit settings.
# =============================================================================

stopifnot(requireNamespace("DBI", quietly = TRUE))

SETTINGS_SCHEMA         <- getOption("ispi.calib_schema", "madi_results")
SETTINGS_NONE           <- "__none__"
SETTINGS_SYSTEM_PROJECT <- -1L

.settings_tbl <- function(name) sprintf("%s.%s", SETTINGS_SCHEMA, name)

# ---- Legacy -> canonical param-name map (UI still uses a few old names) ------
UI_PARAM_NAME_MAP <- c(
  is_log_mfi_axis = "is_log_response",
  applyProzone    = "apply_prozone",
  mean_mfi        = NA_character_,   # deprecated: refuse
  default_source  = NA_character_    # deprecated: refuse
)
.canonical_param <- function(param_name) {
  if (param_name %in% names(UI_PARAM_NAME_MAP)) {
    mapped <- UI_PARAM_NAME_MAP[[param_name]]
    if (is.na(mapped))
      stop(sprintf("set_setting: '%s' is deprecated and is no longer stored", param_name))
    return(mapped)
  }
  param_name
}

# ---- Query helper (0-row frame on empty/error) -------------------------------
.settings_q <- function(pool, sql, params = list()) {
  out <- tryCatch(
    if (length(params)) DBI::dbGetQuery(pool, sql, params = params)
    else                DBI::dbGetQuery(pool, sql),
    error = function(e) {
      warning("settings_cascade_access query failed: ", conditionMessage(e), call. = FALSE)
      NULL
    })
  if (is.null(out)) data.frame() else out
}

# ---- Typed coercions (IDENTICAL semantics to the worker) ---------------------
.settings_as_bool <- function(x, default = NA) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(default)
  tolower(trimws(as.character(x))) %in% c("true", "t", "1", "yes")
}
.settings_as_num <- function(x, default = NA_real_) {
  v <- suppressWarnings(as.numeric(x)); if (is.na(v)) default else v
}
coerce_setting_value <- function(value_text, param_data_type) {
  if (is.null(value_text) || is.na(value_text)) return(NA)
  switch(param_data_type,
    boolean   = .settings_as_bool(value_text),
    integer   = as.integer(.settings_as_num(value_text)),
    numeric   = .settings_as_num(value_text),
    character = as.character(value_text),
    as.character(value_text))
}

# ---- Scope normalization + well-formed-scope validation ----------------------
.norm_txt <- function(x) {
  if (is.null(x) || length(x) == 0) return(SETTINGS_NONE)
  x <- as.character(x)[1]
  if (is.na(x) || !nzchar(trimws(x))) SETTINGS_NONE else trimws(x)
}
.normalize_scope <- function(project, study, experiment, feature, antigen) {
  if (is.null(project) || length(project) == 0 || is.na(project))
    stop("settings scope: project_id is required (studies without a project are unsupported)")
  proj <- suppressWarnings(as.integer(project))
  if (is.na(proj)) stop("settings scope: project_id must be an integer, got: ", project)
  list(project = proj, study = .norm_txt(study), experiment = .norm_txt(experiment),
       feature = .norm_txt(feature), antigen = .norm_txt(antigen))
}
.validate_scope_ladder <- function(s) {
  none <- SETTINGS_NONE
  if (s$antigen    != none && s$feature    == none) stop("scope: antigen set without feature")
  if (s$feature    != none && s$experiment == none) stop("scope: feature set without experiment")
  if (s$experiment != none && s$study      == none) stop("scope: experiment set without study")
  invisible(TRUE)
}

# =============================================================================
# 1. READ -- effective settings for a scope (general reader; worker analogue)
# =============================================================================
resolve_settings_scoped <- function(pool, project, study, experiment = NULL,
                                     feature = NULL, antigen = NULL, group = NULL) {
  s <- .normalize_scope(project, study, experiment, feature, antigen)
  sql <- sprintf(
    "SELECT r.param_name, m.param_group, m.param_label, m.param_control_type,
            m.param_choices_list, r.param_data_type, r.value_text, r.tier_rank
       FROM %s(($1)::int, $2, $3, $4, $5) r
       LEFT JOIN %s m USING (param_name)
      %s
      ORDER BY m.param_group, r.param_name",
    .settings_tbl("resolve_settings"), .settings_tbl("calib_settings_meta"),
    if (!is.null(group)) "WHERE m.param_group = $6" else "")
  params <- list(s$project, s$study, s$experiment, s$feature, s$antigen)
  if (!is.null(group)) params <- c(params, list(group))
  df <- .settings_q(pool, sql, params)
  if (nrow(df)) df$value <- mapply(coerce_setting_value, df$value_text,
                                   df$param_data_type, SIMPLIFY = FALSE)
  df
}

settings_as_list <- function(resolved) {
  if (is.null(resolved) || !nrow(resolved)) return(list())
  stats::setNames(resolved$value, resolved$param_name)
}

# =============================================================================
# 2. READ -- editor view (meta-driven: ALL editable params, set or not)
# =============================================================================
settings_editor_view <- function(pool, project, study, experiment = NULL,
                                  feature = NULL, antigen = NULL, group = NULL) {
  s <- .normalize_scope(project, study, experiment, feature, antigen)
  sql <- sprintf(
    "SELECT m.param_name, m.param_group, m.param_label, m.param_control_type,
            m.param_choices_list, m.param_data_type, m.param_description,
            r.value_text AS effective_text, r.tier_rank,
            (ovr.calib_settings_id IS NOT NULL) AS is_overridden_here
       FROM %s m
       LEFT JOIN %s(($1)::int, $2, $3, $4, $5) r USING (param_name)
       LEFT JOIN %s ovr
         ON ovr.param_name = m.param_name
        AND ovr.project_id = ($1)::int AND ovr.study_accession = $2
        AND ovr.experiment_accession = $3 AND ovr.feature = $4 AND ovr.antigen = $5
      %s
      ORDER BY m.param_group, m.param_name",
    .settings_tbl("calib_settings_meta"), .settings_tbl("resolve_settings"),
    .settings_tbl("calib_settings"),
    if (!is.null(group)) "WHERE m.param_group = $6" else "")
  params <- list(s$project, s$study, s$experiment, s$feature, s$antigen)
  if (!is.null(group)) params <- c(params, list(group))
  df <- .settings_q(pool, sql, params)
  if (nrow(df)) df$effective_value <- mapply(coerce_setting_value, df$effective_text,
                                             df$param_data_type, SIMPLIFY = FALSE)
  df
}

# =============================================================================
# 3a. READ -- legacy-shape shim for study-config consumers
# -----------------------------------------------------------------------------
# Drop-in for the old fetch_study_configuration(): returns the SAME long shape
# the ~15 render/reactive consumers expect (param_group, param_name, and the
# typed columns param_character_value / param_integer_value / param_boolean_value),
# reconstructed from the resolver. The `user` arg is GONE (config is study-scoped
# now); callers pass project_id instead. Consumer bodies are unchanged.
# Numeric + integer both land in param_integer_value, matching the legacy table
# (which had no numeric column) so existing consumers read them the same way.
fetch_study_configuration <- function(pool, study, project_id, group = NULL) {
  r <- resolve_settings_scoped(pool, project_id, study, group = group)
  base <- data.frame(param_group = character(), param_name = character(),
                     param_character_value = character(),
                     param_integer_value = integer(),
                     param_boolean_value = logical(),
                     tier_rank = integer(), stringsAsFactors = FALSE)
  if (!nrow(r)) return(base)
  out <- data.frame(
    param_group = r$param_group, param_name = r$param_name,
    param_character_value = NA_character_, param_integer_value = NA_integer_,
    param_boolean_value = NA, tier_rank = r$tier_rank, stringsAsFactors = FALSE)
  for (i in seq_len(nrow(r))) {
    dt <- r$param_data_type[i]; v <- r$value_text[i]
    if (is.null(v) || is.na(v)) next
    if (dt %in% c("integer", "numeric")) out$param_integer_value[i]   <- as.integer(.settings_as_num(v))
    else if (dt == "boolean")            out$param_boolean_value[i]   <- .settings_as_bool(v)
    else                                 out$param_character_value[i] <- as.character(v)
  }
  out
}

# =============================================================================
# 3b. READ -- backward-compatible WIDE shim for the antigen reader
# =============================================================================
fetch_antigen_feature_settings <- function(pool, project_id, study, antigen,
                                            experiment = NULL, feature = NULL) {
  resolved <- resolve_settings_scoped(pool, project_id, study, experiment,
                                      feature, antigen, group = "calibration")
  vals <- settings_as_list(resolved)
  if (!length(vals)) return(data.frame())
  as.data.frame(vals, stringsAsFactors = FALSE, check.names = FALSE)
}

# =============================================================================
# 4. WRITE -- UPSERT an override / DELETE to revert
# =============================================================================
.param_meta <- function(pool, param_name) {
  m <- .settings_q(pool, sprintf(
    "SELECT param_name, param_group, param_data_type FROM %s WHERE param_name = $1",
    .settings_tbl("calib_settings_meta")), params = list(param_name))
  if (!nrow(m)) stop(sprintf("set_setting: unknown param '%s' (not in calib_settings_meta)", param_name))
  as.list(m[1, ])
}

set_setting <- function(pool, project, study, param_name, value, user,
                        experiment = NULL, feature = NULL, antigen = NULL) {
  param_name <- .canonical_param(param_name)
  s   <- .normalize_scope(project, study, experiment, feature, antigen)
  .validate_scope_ladder(s)
  met <- .param_meta(pool, param_name)
  dt  <- met$param_data_type

  iv <- nv <- bv <- cv <- NA
  if      (dt == "integer") iv <- as.integer(.settings_as_num(value))
  else if (dt == "numeric") nv <- .settings_as_num(value)
  else if (dt == "boolean") bv <- .settings_as_bool(value)
  else                      cv <- as.character(value)

  sql <- sprintf(
    "INSERT INTO %s
        (project_id, study_accession, experiment_accession, feature, antigen,
         param_name, param_group, param_data_type,
         param_integer_value, param_numeric_value, param_boolean_value, param_character_value,
         param_user, updated_at)
      VALUES (($1)::int,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,now())
      ON CONFLICT ON CONSTRAINT calib_settings_scope_param_uniq DO UPDATE SET
         param_data_type=EXCLUDED.param_data_type, param_group=EXCLUDED.param_group,
         param_integer_value=EXCLUDED.param_integer_value,
         param_numeric_value=EXCLUDED.param_numeric_value,
         param_boolean_value=EXCLUDED.param_boolean_value,
         param_character_value=EXCLUDED.param_character_value,
         param_user=EXCLUDED.param_user, updated_at=now()",
    .settings_tbl("calib_settings"))
  DBI::dbExecute(pool, sql, params = list(
    s$project, s$study, s$experiment, s$feature, s$antigen,
    param_name, met$param_group, dt,
    if (is.na(iv)) NA_integer_ else iv, if (is.na(nv)) NA_real_ else nv,
    if (is.na(bv)) NA else bv, if (is.na(cv)) NA_character_ else cv, user))
  invisible(TRUE)
}

unset_setting <- function(pool, project, study, param_name,
                          experiment = NULL, feature = NULL, antigen = NULL) {
  param_name <- .canonical_param(param_name)
  s <- .normalize_scope(project, study, experiment, feature, antigen)
  DBI::dbExecute(pool, sprintf(
    "DELETE FROM %s WHERE project_id=($1)::int AND study_accession=$2
       AND experiment_accession=$3 AND feature=$4 AND antigen=$5 AND param_name=$6",
    .settings_tbl("calib_settings")),
    params = list(s$project, s$study, s$experiment, s$feature, s$antigen, param_name))
  invisible(TRUE)
}

# =============================================================================
# 5. SCOPE OPTIONS — live tier choices from curve_lookup (for the editor picker)
# -----------------------------------------------------------------------------
# The editor's breadcrumb derives available tiers from curve_lookup EVERY time,
# so a study that grows (new antigens/features loaded later) automatically gains
# the deeper tiers with no code change. Each call returns the distinct values at
# ONE tier under the currently-chosen parent scope, so the picker cascades and
# the ladder is enforced by construction (you can't pick an antigen until a
# feature narrows the set).
#
# Convention: pass '__none__' for tiers not yet chosen. Real (non-wildcard)
# values above the requested tier filter the options; the requested tier is the
# one whose DISTINCT values are returned, sorted.
# =============================================================================

#' Distinct values available at `tier` under the given parent scope, from
#' curve_lookup. tier in c("experiment","feature","antigen").
scope_options <- function(pool, project, study,
                          tier = c("experiment", "feature", "antigen"),
                          experiment = "__none__", feature = "__none__") {
  tier <- match.arg(tier)
  if (is.null(project) || is.na(project)) stop("scope_options: project_id required")
  col <- switch(tier, experiment = "experiment_accession", feature = "feature", antigen = "antigen")

  where <- c("project_id = ($1)::int", "study_accession = $2")
  params <- list(as.integer(project), as.character(study))
  # apply parent filters only for tiers above the requested one
  if (tier %in% c("feature", "antigen") && !identical(experiment, "__none__")) {
    where <- c(where, sprintf("experiment_accession = $%d", length(params) + 1)); params <- c(params, list(experiment))
  }
  if (tier == "antigen" && !identical(feature, "__none__")) {
    where <- c(where, sprintf("feature = $%d", length(params) + 1)); params <- c(params, list(feature))
  }
  sql <- sprintf("SELECT DISTINCT %s AS v FROM %s WHERE %s AND %s <> '__none__' ORDER BY 1",
                 col, .settings_tbl("curve_lookup"), paste(where, collapse = " AND "), col)
  df <- .settings_q(pool, sql, params)
  if (!nrow(df)) character(0) else as.character(df$v)
}

#' Human label for a resolver tier_rank, relative to the DEEPEST active tier in
#' the current breadcrumb. Returns "set here" when the value originates at the
#' current scope depth, else "inherited from {tier}".
#'   depth: 2=study,3=experiment,4=feature,5=antigen (the deepest chosen tier)
provenance_label <- function(tier_rank, depth) {
  tier_name <- c(`0` = "system default", `1` = "project", `2` = "study",
                 `3` = "experiment", `4` = "feature", `5` = "antigen")
  if (length(tier_rank) == 0 || is.null(tier_rank) || is.na(tier_rank)) return("unset")
  if (isTRUE(tier_rank >= depth)) "set here"
  else {
    nm <- tier_name[[as.character(tier_rank)]]
    if (is.null(nm) || is.na(nm)) "unset" else paste0("inherited from ", nm)
  }
}

# =============================================================================
# 6. EXPORT / IMPORT -- a study's OWN override rows (round-trippable)
# -----------------------------------------------------------------------------
# export_settings_scoped(): every calib_settings override row the study owns,
#   across all sub-scopes (study/experiment/feature/antigen). This is the sparse
#   DATA of the cascade -- NOT resolved values -- so a reload reproduces exactly
#   the overrides without materialising __system__ defaults.
# import_settings_scoped(): re-apply exported rows to a target project/study via
#   set_setting() (per-row sub-scope preserved; '__none__' sentinels -> NULL).
#   reset_first=TRUE clears the target study's existing overrides first, for a
#   clean restore rather than a merge.
# =============================================================================
export_settings_scoped <- function(pool, project, study) {
  .settings_q(pool, sprintf(
    "SELECT project_id, study_accession, experiment_accession, feature, antigen,
            param_name, param_group, param_data_type,
            param_integer_value, param_numeric_value,
            param_boolean_value, param_character_value,
            param_user, updated_at
       FROM %s
      WHERE project_id = ($1)::int AND study_accession = $2
      ORDER BY experiment_accession, feature, antigen, param_group, param_name",
    .settings_tbl("calib_settings")),
    params = list(project, study))
}

# '__none__' sentinel (unset sub-scope) -> NULL for the write helpers.
.unsentinel <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) ||
      identical(as.character(x), "__none__")) NULL else as.character(x)
}

# Value carried in whichever typed column matches param_data_type.
.settings_row_value <- function(row) {
  dt <- row$param_data_type
  if (identical(dt, "integer"))      row$param_integer_value
  else if (identical(dt, "numeric")) row$param_numeric_value
  else if (identical(dt, "boolean")) row$param_boolean_value
  else                               row$param_character_value
}

import_settings_scoped <- function(pool, rows, project, study, user,
                                   reset_first = FALSE) {
  stopifnot(is.data.frame(rows))
  if (!all(c("param_name", "param_data_type") %in% names(rows)))
    stop("import_settings_scoped: rows missing param_name / param_data_type")
  if (isTRUE(reset_first)) {
    cur <- export_settings_scoped(pool, project, study)
    for (i in seq_len(nrow(cur)))
      unset_setting(pool, project, study, cur$param_name[i],
                    experiment = .unsentinel(cur$experiment_accession[i]),
                    feature    = .unsentinel(cur$feature[i]),
                    antigen    = .unsentinel(cur$antigen[i]))
  }
  n <- 0L
  for (i in seq_len(nrow(rows))) {
    r <- rows[i, , drop = FALSE]
    set_setting(pool, project, study, r$param_name, .settings_row_value(r), user,
      experiment = .unsentinel(if ("experiment_accession" %in% names(r)) r$experiment_accession else NULL),
      feature    = .unsentinel(if ("feature" %in% names(r)) r$feature else NULL),
      antigen    = .unsentinel(if ("antigen" %in% names(r)) r$antigen else NULL))
    n <- n + 1L
  }
  invisible(n)
}
