# =============================================================================
# compute_cluster_registry.R  --  label -> {base_url, api_key} lookup for
#                                  per-project/study i-spi-compute routing.
# -----------------------------------------------------------------------------
# Secrets (API keys) stay in env vars, same place every other secret in this
# app already lives (see auth_config.R, compute_api_client.R) -- NOT in the
# settings cascade, which is an admin-visible/exportable store
# (settings_export_import_ui.R). Only a free-text LABEL ("default", "clone-b",
# ...) travels through the cascade; the label->secret mapping is resolved
# here, from env vars read fresh on every call (cheap: a handful of vars).
#
# CONFIG (env, set alongside ISPI_COMPUTE_URL/ISPI_COMPUTE_API_KEY):
#   ISPI_COMPUTE_URL                 the "default" clone (today's only pair)
#   ISPI_COMPUTE_API_KEY
#   ISPI_COMPUTE_URL__<SUFFIX>       an additional clone, e.g. __CLONE_B
#   ISPI_COMPUTE_API_KEY__<SUFFIX>   matching key for that SAME <SUFFIX>
# <SUFFIX> is matched verbatim between the URL/key pair (case-sensitive, as
# env var names are); the cascade LABEL used to select it is
# lower-case(<SUFFIX>) with "-" normalized to "_", so a `compute_cluster`
# setting value of "clone-b", "CLONE_B", or "clone_b" all resolve to the same
# clone. Adding a clone requires a pod restart (env vars read at process
# startup by the platform; scanned fresh here per call -- no app code change).
#
# DEPENDS: settings_cascade_access.R (resolve_settings_scoped, settings_as_list)
# =============================================================================

#' Scan Sys.getenv() for ISPI_COMPUTE_URL[__SUFFIX] / ISPI_COMPUTE_API_KEY[__SUFFIX]
#' pairs and return a named list keyed by lower-cased, "-"->"_" normalized label.
#' @return list(label = list(base_url=, api_key=, label=), ...). Each entry
#'   carries its own `label` (identical to the list name) so callers that hand
#'   the {base_url, api_key} pair onward (e.g. compute_api_client()) can also
#'   surface which cluster it is -- used by the Compute-fits/Compute-weights
#'   status boxes to confirm which clone a job is actually running against.
#'   Always has "default" when ISPI_COMPUTE_URL is set (today's deployments
#'   always set it).
compute_cluster_registry <- function() {
  env_names <- names(Sys.getenv())
  url_vars  <- grep("^ISPI_COMPUTE_URL(__.+)?$", env_names, perl = TRUE, value = TRUE)
  reg <- list()
  for (vn in url_vars) {
    # "" for the bare pair, "CLONE_B" (case preserved) for a __CLONE_B pair.
    suffix   <- sub("^ISPI_COMPUTE_URL(?:__)?", "", vn, perl = TRUE)
    label    <- if (!nzchar(suffix)) "default" else tolower(gsub("-", "_", suffix, fixed = TRUE))
    key_var  <- if (!nzchar(suffix)) "ISPI_COMPUTE_API_KEY" else paste0("ISPI_COMPUTE_API_KEY__", suffix)
    base_url <- Sys.getenv(vn)
    if (!nzchar(base_url)) next                       # defensive: blank URL, skip
    if (label %in% names(reg)) {
      warning(sprintf(
        "compute_cluster_registry: duplicate cluster label '%s' (from env var %s); keeping the first one found",
        label, vn), call. = FALSE)
      next
    }
    reg[[label]] <- list(base_url = base_url, api_key = Sys.getenv(key_var), label = label)
  }
  reg
}

#' Resolve the {base_url, api_key} pair for one project/study/experiment scope.
#' Reads the `compute_cluster` cascade setting (free-text label); unset,
#' unreadable (e.g. project_id not yet chosen), or unknown-in-the-registry all
#' fall back to "default" -- the exact ISPI_COMPUTE_URL/ISPI_COMPUTE_API_KEY
#' pair every deployment already has. Never throws: every caller (the app's
#' shared compute_api reactive) must be able to call this unconditionally,
#' including before a project is selected.
#' @param pool a pool::Pool or DBI connection.
#' @param scope list(project_id=, study=, experiment=) -- same shape as
#'   app.R's calib_scope().
resolve_compute_cluster <- function(pool, scope) {
  registry <- compute_cluster_registry()
  default_cluster <- registry[["default"]] %||%
    list(base_url = Sys.getenv("ISPI_COMPUTE_URL", "https://localhost/i-spi-compute"),
         api_key  = Sys.getenv("ISPI_COMPUTE_API_KEY"),
         label    = "default")

  pid <- scope$project_id
  label <- if (is.null(pid) || length(pid) == 0 || is.na(pid)) {
    # No project selected yet (pre-login, or a queue-panel poll that fires
    # before the user has picked a project) -- NOT an error, just "too early".
    # Silently use the default; warning() here would spam the log every poll.
    NA_character_
  } else {
    tryCatch({
      resolved <- resolve_settings_scoped(pool, pid, scope$study, scope$experiment,
                                          group = "infrastructure")
      v <- settings_as_list(resolved)[["compute_cluster"]]
      if (is.null(v) || is.na(v) || !nzchar(trimws(as.character(v)))) NA_character_
      else trimws(as.character(v))
    }, error = function(e) {
      warning("resolve_compute_cluster: settings lookup failed, using default cluster: ",
              conditionMessage(e), call. = FALSE)
      NA_character_
    })
  }

  if (is.na(label)) return(default_cluster)

  key <- tolower(gsub("-", "_", label, fixed = TRUE))
  hit <- registry[[key]]
  if (is.null(hit)) {
    warning(sprintf(
      "resolve_compute_cluster: compute_cluster = '%s' has no matching ISPI_COMPUTE_URL__%s env var; falling back to 'default'",
      label, toupper(key)), call. = FALSE)
    return(default_cluster)
  }
  hit
}
