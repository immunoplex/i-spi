source(here::here("./src/compute_api_client.R"))

api <- compute_api_client(
  base_url = Sys.getenv("ISPI_BASE_URL"),
  api_key  = Sys.getenv("ISPI_API_KEY")
)

# --- refit a batch and watch it to completion ---------------------------------
watch_job <- function(api, job_id, every = 5) {
  repeat {
    j <- api$get_job(job_id)
    cat(sprintf("%s  %-9s  %s  %s%%  eta=%s\n",
                format(Sys.time(), "%H:%M:%S"),
                j$status, j$progress %||% "?", j$percentage %||% 0,
                j$eta_display %||% "?"))
    if (is_terminal_status(j$status)) {
      if (!is.null(j$error)) { cat("---- error ----\n"); cat(j$error, "\n") }
      return(invisible(j))
    }
    Sys.sleep(every)
  }
}
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# --- query the queue for what's currently running -----------------------------
running_jobs <- function(api) {
  res  <- api$list_jobs(status = "running")   # -> GET /jobs?status=running
  jobs <- res$jobs %||% list()
  if (!length(jobs)) {
    cat("No running jobs.\n")
    return(invisible(character(0)))
  }
  for (j in jobs) {
    cat(sprintf("%-36s  %-9s  %s  %s%%  group=%s\n",
                j$job_id,
                j$status,
                j$progress        %||% "?",
                j$percentage      %||% 0,
                j$current_group   %||% "-"))
  }
  invisible(vapply(jobs, function(j) j$job_id, character(1)))
}

# just the ids (e.g. to feed cancel_job / watch_job):
running_job_ids <- function(api) {
  jobs <- api$list_jobs(status = "running")$jobs %||% list()
  vapply(jobs, function(j) j$job_id, character(1))
}

# --- query the queue for what's currently queued -----------------------------
queue_jobs <- function(api) {
  res  <- api$list_jobs(status = "queued")   # -> GET /jobs?status=queued
  jobs <- res$jobs %||% list()
  if (!length(jobs)) {
    cat("No queued jobs.\n")
    return(invisible(character(0)))
  }
  for (j in jobs) {
    cat(sprintf("%-36s  %-9s  %s  %s%%  group=%s\n",
                j$job_id,
                j$status,
                j$progress        %||% "?",
                j$percentage      %||% 0,
                j$current_group   %||% "-"))
  }
  invisible(vapply(jobs, function(j) j$job_id, character(1)))
}

# just the ids (e.g. to feed cancel_job / watch_job):
queued_job_ids <- function(api) {
  jobs <- api$list_jobs(status = "queued")$jobs %||% list()
  vapply(jobs, function(j) j$job_id, character(1))
}

# --- query the queue for the whole list -----------------------------
all_jobs <- function(api) {
  res  <- api$list_jobs()   # -> GET /jobs
  jobs <- res$jobs %||% list()
  if (!length(jobs)) {
    cat("No jobs.\n")
    return(invisible(character(0)))
  }
  for (j in jobs) {
    cat(sprintf("%-36s  %-9s  %s  %s%%  group=%s\n",
                j$job_id,
                j$status,
                j$progress        %||% "?",
                j$percentage      %||% 0,
                j$current_group   %||% "-"))
  }
  invisible(vapply(jobs, function(j) j$job_id, character(1)))
}

# just the ids (e.g. to feed cancel_job / watch_job):
all_job_ids <- function(api) {
  jobs <- api$list_jobs()$jobs %||% list()
  vapply(jobs, function(j) j$job_id, character(1))
}

job_list <- all_jobs(api = api)
running_list <- running_jobs(api = api)
# watch_job(api, job$job_id)


