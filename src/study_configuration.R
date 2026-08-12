# study configuration settings
# -----------------------------------------------------------------------------
# REFACTORED for the unified settings cascade. Settings reads/writes go through
# settings_cascade_access.R (resolve_settings_scoped / fetch_study_configuration
# / set_setting / unset_setting). This file keeps only:
#   * study-scoped list helpers (sources/arms/timeperiods) — moved to db_pool
#   * a NO-OP seeder stub (new studies inherit __system__; nothing is seeded)
#   * a resolver-backed download helper
#   * delete_plate (data-editing; unchanged, still takes an explicit conn)
# fetch_study_configuration() now lives in settings_cascade_access.R.
# =============================================================================

## Render the study parameters. Under the sparse cascade a study always resolves
## the __system__ defaults immediately (there is nothing to poll for), so this
## just flips the ready flag. Kept as a function so existing callers still work.
check_and_render_study_parameters <- function(study_accession, user,
                                               max_attempts = 5, delay = 1, attempt = 1) {
  # Defaults are guaranteed by the __system__ seed; no per-study rows to wait on.
  study_params_ready(TRUE)
  invisible(TRUE)
}


obtain_initial_source <- function(study_accession) {
  study_sources_query <- paste0("SELECT DISTINCT source
    FROM madi_results.xmap_standard
    WHERE study_accession = '", study_accession, "'")
  study_sources_df <- dbGetQuery(db_pool, study_sources_query)
  unique(study_sources_df$source)[1]
}

obtain_all_sc_source <- function(study_accession) {
  study_sources_query <- paste0("SELECT DISTINCT source
    FROM madi_results.xmap_standard
    WHERE study_accession = '", study_accession, "'")
  study_sources_df <- dbGetQuery(db_pool, study_sources_query)
  unique(study_sources_df$source)
}


# -----------------------------------------------------------------------------
# NO-OP seeder. Formerly wrote ~18 default rows per study into xmap_study_config.
# Under the sparse cascade a new study starts with ZERO rows and inherits the
# __system__ defaults via resolve_settings(); overrides are written lazily by
# set_setting() only when a user changes a value. Kept as a stub so the call site
# (study_configuration_ui.R reset handler) and any other reference keep working.
# -----------------------------------------------------------------------------
# intitialize_study_configurations <- function(study_accession, user, initial_source,
#                                               project_id = userWorkSpaceID()) {
#   invisible(NULL)
# }


## fetch_study_configuration() REMOVED — now defined in settings_cascade_access.R
## as a resolver-backed shim: fetch_study_configuration(db_pool, study, project_id).


## Download the resolved configuration for a study (was per-user xmap_study_config;
## now the effective cascade values). Signature drops `user`.
download_user_parameters <- function(study_accession, project_id = userWorkSpaceID()) {
  fetch_study_configuration(db_pool, study = study_accession, project_id = project_id)
}


fetch_study_sources <- function(study_accession) {
  query <- paste0("SELECT DISTINCT study_accession, source
    FROM madi_results.xmap_standard
    WHERE study_accession = '", study_accession, "';")
  dbGetQuery(db_pool, query)
}

fetch_study_arms <- function(study_accession) {
  query <- paste0("SELECT DISTINCT study_accession, agroup
    FROM madi_results.xmap_sample
    WHERE study_accession = '", study_accession, "';")
  dbGetQuery(db_pool, query)
}

fetch_study_timeperiods <- function(study_accession) {
  query <- paste0("SELECT DISTINCT study_accession, timeperiod
    FROM madi_results.xmap_sample
    WHERE study_accession = '", study_accession, "';")
  dbGetQuery(db_pool, query)
}

## (Removed: the large commented-out fetch_antigen_family_table blocks. The live
##  fetch_antigen_family_table is defined in antigen_family_ui.R.)


## Delete a plate and all associated data. Data-editing (separate concern from
## settings); still takes an EXPLICIT conn. Callers should pass a pooled
## connection via pool::poolWithTransaction(db_pool, function(conn) { ... }) so the
## seven deletes run atomically in one transaction on one connection.
# --- [11.8] removed -> consolidated in delete_study_components(.R/_ui.R) ---
