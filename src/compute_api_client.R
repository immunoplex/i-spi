# =============================================================================
# compute_api_client.R  --  thin client for the i-spi-compute job API
# -----------------------------------------------------------------------------
# Replaces the old BATCH_API_URL/BATCH_API_KEY batch client. Points at the
# i-spi-compute FastAPI service behind the Traefik strip-prefix ingress
# (ROOT_PATH=/i-spi-compute). The ONE behavioural change vs the legacy client:
# script_type is now a first-class argument, so the SAME client submits both
# 'bayesian' and 'frequentist' jobs -- the engine is chosen per submission, not
# baked into the endpoint.
#
# CONFIG (env, set alongside the other i-spi env vars):
#   ISPI_COMPUTE_URL      base URL incl. root path, e.g.
#                         https://<host>/i-spi-compute   (no trailing /jobs)
#   ISPI_COMPUTE_API_KEY  value of the sealed-secret `i-spi-compute` key API_KEY
#
# DEPENDS: httr2, jsonlite
#
# USAGE
#   api <- compute_api_client()                 # reads env
#   job <- api$submit_job(curve_ids = c(9057, 9058, 9101),
#                         multiplate_group_ids = c(...),   # optional, parallel
#                         script_type = "frequentist",
#                         params = list(models = "logistic4,gompertz4"))
#   st  <- api$get_job(job$job_id)              # poll
# =============================================================================

stopifnot(requireNamespace("httr2", quietly = TRUE))

VALID_SCRIPTS <- c("bayesian", "frequentist")

#' Construct an i-spi-compute API client.
#' @return a list of functions: submit_job, get_job, list_jobs, cancel_job, health.
compute_api_client <- function(
    base_url = Sys.getenv("ISPI_COMPUTE_URL", "https://localhost/i-spi-compute"),
    api_key  = Sys.getenv("ISPI_COMPUTE_API_KEY"),
    timeout  = 30,
    verbose  = isTRUE(as.logical(Sys.getenv("ISPI_COMPUTE_VERBOSE", "FALSE")))) {

  base_url <- sub("/+$", "", base_url)  # tolerate a trailing slash in config
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

  .log <- function(fmt, ...) if (isTRUE(verbose)) message(sprintf(paste0("[i-spi-compute] ", fmt), ...))

  # ---- internal: build an authed request to a sub-path --------------------
  .req <- function(..., auth = TRUE) {
    r <- httr2::request(base_url)
    r <- httr2::req_url_path_append(r, ...)
    r <- httr2::req_timeout(r, timeout)
    if (auth) {
      if (!nzchar(api_key))
        stop("compute_api_client: ISPI_COMPUTE_API_KEY is not set.")
      r <- httr2::req_headers(r, `X-API-Key` = api_key)
    }
    r
  }

  # Turn an error body into a readable string. FastAPI validation (422) returns
  # {"detail":[{loc,msg,type},...]}; other errors may use {error}/{message}.
  .fmt_detail <- function(body) {
    if (is.null(body)) return("")
    if (is.character(body)) return(body)
    d <- body$detail
    if (!is.null(d)) {
      if (is.list(d) && length(d) && is.list(d[[1]])) {
        return(paste(vapply(d, function(x)
          sprintf("%s: %s", paste(unlist(x$loc), collapse = "."),
                  x$msg %||% x$type %||% "invalid"), character(1)), collapse = " | "))
      }
      return(paste(unlist(d), collapse = " | "))
    }
    body$error %||% body$message %||% ""
  }

  # Perform a request, logging (when verbose) and surfacing the FULL error body.
  .perform <- function(req, what = "request", payload = NULL) {
    if (isTRUE(verbose) && !is.null(payload))
      .log("POST %s payload: %s", what,
           tryCatch(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"),
                    error = function(e) "<unserializable>"))
    resp <- httr2::req_perform(httr2::req_error(req, is_error = function(r) FALSE))
    status <- httr2::resp_status(resp)
    body <- tryCatch(httr2::resp_body_json(resp),
              error = function(e) tryCatch(httr2::resp_body_string(resp),
              error = function(e2) NULL))
    .log("%s -> HTTP %d  body: %s", what, status,
         if (is.character(body)) body
         else tryCatch(jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"),
                       error = function(e) "<no body>"))
    if (status >= 400) {
      det <- .fmt_detail(body)
      stop(sprintf("HTTP %d%s", status, if (nzchar(det)) paste0(" \u2014 ", det) else ""),
           call. = FALSE)
    }
    if (is.null(body) || (is.character(body) && !nzchar(body))) invisible(NULL) else body
  }
  .json <- function(req, ...) .perform(req, ...)

  list(

    #' Submit a calibration job for a BATCH OF CURVES. Scope resolution lives in
    #' the app (scope -> curve_ids via curve_lookup); the worker groups by
    #' multiplate_group_id and never sees natural keys or scope.
    #' @param curve_ids integer vector of curves to fit.
    #' @param multiplate_group_ids uuid vector, parallel to curve_ids (optional;
    #'   the fit-delivery views also carry it, so the worker can group without it).
    #' @param script_type one of bayesian|frequentist
    #' @param params named list -> passthrough CLI flags (e.g. list(models=...)).
    #' @return parsed response incl. job_id.
    submit_job = function(curve_ids, multiplate_group_ids = NULL, script_type,
                          params = list(), cdan_cv_threshold = NULL) {
      script_type <- match.arg(script_type, VALID_SCRIPTS)
      if (!length(curve_ids)) stop("submit_job: curve_ids is empty")
      payload <- list(
        curve_ids            = as.list(as.integer(curve_ids)),
        multiplate_group_ids = if (!is.null(multiplate_group_ids))
                                 as.list(as.character(multiplate_group_ids)) else NULL,
        script_type = script_type,
        # An EMPTY R list serializes to JSON `[]`, but the API types `params` as
        # an object -> 422 "Input should be a valid dictionary". Omit when empty
        # (API applies its default); a non-empty NAMED list serializes as `{}`.
        params = if (length(params)) as.list(params) else NULL,
        cdan_cv_threshold = cdan_cv_threshold)
      payload <- payload[!vapply(payload, is.null, logical(1))]
      .perform(httr2::req_body_json(httr2::req_method(.req("jobs"), "POST"), payload),
               what = "jobs", payload = payload)
    },

    #' Poll one job. Returns status, percentage, eta_display,
    #' current_experiment, error, output_path.
    get_job = function(job_id) .json(.req("jobs", job_id)),

    #' List jobs, optionally filtered by study and/or status.
    list_jobs = function(study = NULL, status = NULL) {
      r <- .req("jobs")
      q <- list(study = study, status = status)
      q <- q[!vapply(q, is.null, logical(1))]
      if (length(q)) r <- do.call(httr2::req_url_query, c(list(r), q))
      .json(r)
    },

    #' Cancel a queued/running job.
    cancel_job = function(job_id)
      .json(httr2::req_method(.req("jobs", job_id), "DELETE")),

    #' Liveness check (no auth).
    health = function() .json(.req("health", auth = FALSE))
  )
}

# Terminal poll states -- helper so the module doesn't hardcode these strings.
COMPUTE_TERMINAL_STATES <- c("completed", "failed", "cancelled")
is_terminal_status <- function(status) isTRUE(status %in% COMPUTE_TERMINAL_STATES)
