# =============================================================================
# help_utils.R  --  concept-keyed help/docs engine for the settings cascade
# -----------------------------------------------------------------------------
# Three display layers, one source of truth per layer:
#   Layer 1  setting label            <- calib_settings_meta.param_label   (DB)
#   Layer 2  one-line description      <- calib_settings_meta.param_description (DB)
#   Layer 3  drill-down explainer + refs <- help/settings/<concept>.md      (files)
#
# Content is CONCEPT-keyed, not param-keyed: one markdown note can document
# several params (e.g. the precision note explains both include_measurement_error
# and pcov_threshold). A note declares which params it covers via `params:` in its
# YAML frontmatter; load_help() builds a param -> concept index from that.
#
# Each note also carries an `audience:` tag (user | dev | both, default user).
# The settings UI renders drill-downs for user-facing notes only; a dev-only note
# is hidden from the user popover (it can still power a developer view later).
#
# Anatomy of a note (help/settings/<id>.md):
#   ---
#   id: precision_measurement_error          # optional; defaults to filename
#   title: Precision profile & measurement error
#   audience: both                           # user | dev | both  (default user)
#   params: [include_measurement_error, pcov_threshold]
#   references:
#     - text: "Author (Year). Title. Journal."
#       doi: "10.xxxx/xxxxx"                 # or url: for a direct link
#   ---
#   <markdown body = the 1-2 paragraph explainer>
#
# Dependencies: shiny, bslib (>= 0.5, for popover), bsicons, yaml.
# No global `%||%` is defined here (the settings UI defines its own); this file
# uses an internal .hv() helper instead, so nothing is clobbered.
# =============================================================================

# internal null/empty coalesce (NOT an operator, to avoid clobbering %||%)
.hv <- function(x, default) if (is.null(x) || length(x) == 0) default else x

# ---- parsing ----------------------------------------------------------------
.parse_help_file <- function(path) {
  raw  <- readLines(path, warn = FALSE, encoding = "UTF-8")
  meta <- list(); body_lines <- raw
  fences <- which(trimws(raw) == "---")
  if (length(fences) >= 2 && fences[1] == 1) {
    yaml_block <- raw[(fences[1] + 1):(fences[2] - 1)]
    parsed <- tryCatch(yaml::yaml.load(paste(yaml_block, collapse = "\n")),
                       error = function(e) {
                         warning(sprintf("help: bad YAML in %s: %s",
                                         basename(path), conditionMessage(e)))
                         list()
                       })
    if (is.list(parsed)) meta <- parsed
    body_lines <- if (fences[2] < length(raw)) raw[(fences[2] + 1):length(raw)] else character(0)
  }
  meta$body     <- trimws(paste(body_lines, collapse = "\n"))
  meta$id       <- .hv(meta$id, tools::file_path_sans_ext(basename(path)))
  meta$audience <- tolower(as.character(.hv(meta$audience, "user"))[1])
  # normalise `params` to a plain character vector
  meta$params   <- if (is.null(meta$params)) character(0) else as.character(unlist(meta$params))
  meta
}

# Load every note under `dir` into a registry keyed by concept id. Fail-soft:
# a missing directory or bad file warns and yields an (empty) registry rather
# than crashing the app. The param -> concept index is stored as attr "by_param".
load_help <- function(dir = "help/settings") {
  if (!dir.exists(dir)) {
    warning(sprintf("help: directory not found: %s (help drill-downs disabled)", dir))
    reg <- structure(list(), by_param = list()); return(reg)
  }
  files <- list.files(dir, pattern = "\\.md$", full.names = TRUE)
  entries <- lapply(files, function(f)
    tryCatch(.parse_help_file(f),
             error = function(e) { warning(sprintf("help: failed to parse %s: %s",
                                                   basename(f), conditionMessage(e))); NULL }))
  entries <- Filter(Negate(is.null), entries)
  ids <- vapply(entries, function(e) e$id, character(1))
  dup <- unique(ids[duplicated(ids)])
  if (length(dup)) warning(sprintf("help: duplicate concept id(s): %s",
                                    paste(dup, collapse = ", ")))
  entries <- stats::setNames(entries, ids)

  # param -> concept id index. A named LIST (not vector) so a missing key
  # returns NULL rather than throwing "subscript out of bounds".
  by_param <- list()
  for (e in entries) for (p in e$params) {
    if (!is.na(p) && nzchar(p)) {
      if (p %in% names(by_param) && !identical(by_param[[p]], e$id))
        warning(sprintf("help: param '%s' mapped to multiple concepts (%s, %s)",
                        p, by_param[[p]], e$id))
      by_param[[p]] <- e$id
    }
  }
  structure(entries, by_param = by_param)
}

# ---- lookup -----------------------------------------------------------------
# Resolve the registry: explicit arg, else the global HELP_SETTINGS. It is
# assigned into globalenv() at startup (see global.R), and the engine's own
# functions also live in globalenv(), so we search there explicitly -- an
# inherits-only walk from here would not descend into the app's child env.
.help_registry <- function(help = NULL) {
  if (!is.null(help)) return(help)
  get0("HELP_SETTINGS", envir = globalenv(), inherits = TRUE)
}

help_concept_for_param <- function(param_name, help = NULL) {
  reg <- .help_registry(help); if (is.null(reg)) return(NULL)
  idx <- attr(reg, "by_param"); if (is.null(idx) || !length(idx)) return(NULL)
  id  <- idx[[param_name]]           # named list -> NULL when the key is absent
  if (is.null(id)) return(NULL)
  reg[[id]]
}

.audience_ok <- function(entry, audience = "user") {
  a <- .hv(entry$audience, "user")
  if (identical(audience, "dev")) return(TRUE)          # dev view sees everything
  a %in% c("user", "both")                              # user view: user + both
}

# ---- rendering --------------------------------------------------------------
.render_references <- function(refs) {
  if (is.null(refs) || length(refs) == 0) return(NULL)
  items <- lapply(refs, function(r) {
    if (is.character(r)) return(shiny::tags$li(r))
    url <- .hv(r$url, if (!is.null(r$doi)) paste0("https://doi.org/", r$doi) else NULL)
    txt <- .hv(r$text, "")
    if (!is.null(url))
      shiny::tags$li(txt, " ",
        shiny::tags$a(bsicons::bs_icon("box-arrow-up-right"), href = url,
                      target = "_blank", rel = "noopener",
                      class = "help-ref-link", .noWS = "before"))
    else shiny::tags$li(txt)
  })
  shiny::tagList(
    shiny::tags$div(class = "help-ref-heading", "References"),
    shiny::tags$ul(class = "help-refs", items))
}

# Layer 3 drill-down for a settings param.
#
# The app is a shinydashboard (Bootstrap 3) page, so a bslib::popover (Bootstrap
# 5) would render but never activate. Instead the icon is a click that fires a
# namespaced Shiny input; the module opens a shiny::modalDialog (framework-
# agnostic) built from settings_help_content(). A modal also suits a 1-2
# paragraph + references note better than a cramped popover.
#
# settings_help_icon() returns a clickable info icon ONLY when a user-facing
# concept note exists for the param (else NULL -- the visible one-liner stands
# on its own). `ns` is the module namespace function.
settings_help_icon <- function(param_name, ns, help = NULL, audience = "user") {
  entry <- help_concept_for_param(param_name, help)
  ok <- !is.null(entry) && .audience_ok(entry, audience) &&
        is.character(entry$body) && nzchar(entry$body)
  if (!ok) return(NULL)
  title <- .hv(entry$title, param_name)
  shiny::tags$span(
    class = "help-icon", tabindex = "0", role = "button",
    `aria-label` = paste("More about", title), title = "Learn more",
    onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority:'event'})",
                      ns("help_show"), param_name),
    bsicons::bs_icon("info-circle"))
}

# Modal body for a param's concept note: title lead + markdown explainer +
# references, or NULL when there is nothing user-facing to show. The module
# server passes this to showModal().
settings_help_content <- function(param_name, help = NULL, audience = "user") {
  entry <- help_concept_for_param(param_name, help)
  if (is.null(entry) || !.audience_ok(entry, audience) ||
      !is.character(entry$body) || !nzchar(entry$body)) return(NULL)
  shiny::div(class = "help-pop",
    shiny::div(class = "help-pop-body", shiny::markdown(entry$body)),
    .render_references(entry$references))
}

# Title for a param's concept modal (falls back to the param name).
settings_help_title <- function(param_name, help = NULL) {
  entry <- help_concept_for_param(param_name, help)
  if (is.null(entry)) param_name else .hv(entry$title, param_name)
}

# Drop once into the UI (settingsCascadeUI already does this).
help_styles <- function() {
  shiny::tags$style(shiny::HTML("
    .param-label-wrap { display: inline-flex; align-items: center; gap: .35rem; }
    .param-desc { margin: .1rem 0 0; font-size: .8rem; color: var(--bs-secondary, #6c757d); }
    .help-icon  { color: var(--bs-secondary, #6c757d); cursor: pointer; line-height: 1; }
    .help-icon:hover, .help-icon:focus { color: var(--bs-primary, #0d6efd); outline: none; }
    .help-icon-plain { cursor: help; }
    .help-pop { max-width: 34rem; }
    .help-pop-short { font-weight: 600; margin-bottom: .4rem; }
    .help-pop-body p:last-child { margin-bottom: .25rem; }
    .help-ref-heading { font-size: .72rem; text-transform: uppercase; letter-spacing: .04em;
                        color: var(--bs-secondary, #6c757d); margin: .6rem 0 .25rem; }
    .help-refs { font-size: .78rem; padding-left: 1.1rem; margin-bottom: 0; }
    .help-refs li { margin-bottom: .35rem; }
    .help-ref-link { text-decoration: none; }
  "))
}
