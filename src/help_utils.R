# =============================================================================
# help_utils.R  --  concept-keyed help/docs engine, app-wide
# -----------------------------------------------------------------------------
# Originally settings-cascade only (Layer 1/2/3 below); generalized 2026-10-09
# to cover any help_id anywhere in the app, per
# docs/help-system/assessment/04-architecture-recommendation.md. Everything
# settings-specific is UNCHANGED (help_concept_for_param, settings_help_icon,
# settings_help_content, settings_help_title all still work exactly as before,
# param -> concept indirection and all) -- the generalization is purely
# additive: new id-direct functions (help_icon, help_modal_title,
# help_modal_body) alongside the old ones, both reading the same registry.
#
# For the settings cascade specifically, three display layers, one source of
# truth per layer:
#   Layer 1  setting label            <- calib_settings_meta.param_label   (DB)
#   Layer 2  one-line description      <- calib_settings_meta.param_description (DB)
#   Layer 3  drill-down explainer + refs <- help/settings/<concept>.md      (files)
# Content is CONCEPT-keyed, not param-keyed, there: one markdown note can
# document several params (e.g. the precision note explains both
# include_measurement_error and pcov_threshold) via `params:` in its
# frontmatter; load_help() builds a param -> concept index from that. Outside
# the settings cascade there is no param indirection -- a help_icon() call
# just names the help_id directly.
#
# Every note carries an `audience:` tag (user | dev | both, default user). A
# dev-only note is hidden from the user-facing icon/modal (it can still power
# a developer view later).
#
# DIRECTORY CONVENTION: one directory per the help_id's leading dotted
# segment -- compute.* -> help/compute/, data.* -> help/data/, qc.* ->
# help/qc/, settings.* -> help/settings/, study_overview.* ->
# help/study_overview/, glossary.* -> help/glossary/. Filename = the id with
# that leading segment dropped (compute.standard_curve.model_engine ->
# help/compute/standard_curve.model_engine.md). This is a human-navigability
# convention only -- the LOOKUP is always by id, from the merged registry
# (load_help_merged(), assigned to HELP_REGISTRY in global.R), so moving a
# file between directories is never a breaking change.
#
# Anatomy of a note (help/<dir>/<rest-of-id>.md):
#   ---
#   id: compute.standard_curve.model_engine  # optional; defaults to filename
#   title: Standard-curve model selection
#   audience: both                     # user | dev | both  (default user)
#   category: compute-decision         # procedural | conceptual | glossary | compute-decision
#   params: [include_measurement_error]  # SETTINGS-CASCADE NOTES ONLY -- see above
#   see_also: [glossary.model_forms, qc.precision_weights.method]
#   references:
#     - text: "Author (Year). Title. Journal."
#       doi: "10.xxxx/xxxxx"           # or url: for a direct link
#   ---
#   Neutral, audience-general explanation -- always shown.
#
#   ::: more
#   Optional deeper paragraph(s), shown behind a "More detail" toggle in the
#   same modal rather than as a second file/modal.
#   :::
#
# A note's body/more text may embed [[help_id]] or [[help_id|display text]] to
# auto-link another concept (e.g. a glossary term) -- resolved at modal-RENDER
# time (not at parse/load time), because the same stored note can be rendered
# from different modules with different ns(), and the click target has to be
# built with the CALLING module's namespace. An id that doesn't resolve falls
# back to plain text, never a broken link.
#
# Dependencies: shiny, bslib (>= 0.5, for popover), bsicons, yaml.
# No global `%||%` is defined here (the settings UI defines its own); this file
# uses an internal .hv() helper instead, so nothing is clobbered.
# =============================================================================

# The single source of truth for which category directories exist. global.R's
# HELP_REGISTRY assignment and test-help-registry.R's sync checks both read
# this constant rather than each keeping their own copy of the directory list
# -- two independent copies drifted out of sync the first time a category
# directory (help/project, help/import) was added after both were written, so
# this list now lives in exactly one place. Add a new category here when
# adding one, not in global.R or the test.
HELP_CONTENT_DIRS <- c(
  "help/settings", "help/compute", "help/data", "help/qc",
  "help/study_overview", "help/glossary", "help/project", "help/import",
  "help/schema"
)

# internal null/empty coalesce (NOT an operator, to avoid clobbering %||%)
.hv <- function(x, default) if (is.null(x) || length(x) == 0) default else x

# "schema" backs the thin schema.<table> wrapper notes in help/schema/*.md --
# category-only; their schema_table: key does the real work via
# render_schema_tables() below. load_help() globs *.md only, load_schema_registry()
# globs *.yaml only, so both live in help/schema/ without colliding.
HELP_VALID_CATEGORIES <- c("procedural", "conceptual", "glossary", "compute-decision", "schema")

# Pull an optional "::: more" ... ":::" fenced span out of a note's body
# lines, returning the remaining body lines and the more-block's lines
# separately. A "::: more" with no matching closing ":::" is malformed --
# treated as if there were no more-block at all (never an error; the whole
# file just reads as one block, which is always a safe degradation).
.split_more_block <- function(body_lines) {
  start <- which(trimws(body_lines) == "::: more")
  if (!length(start)) return(list(body = body_lines, more = character(0)))
  start <- start[1]
  after <- seq_along(body_lines) > start
  end_rel <- which(trimws(body_lines[after]) == ":::")
  if (!length(end_rel)) return(list(body = body_lines, more = character(0)))
  end <- start + end_rel[1]
  list(body = body_lines[-(start:end)], more = body_lines[(start + 1):(end - 1)])
}

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
  split <- .split_more_block(body_lines)
  meta$body     <- trimws(paste(split$body, collapse = "\n"))
  meta$more     <- if (length(split$more)) trimws(paste(split$more, collapse = "\n")) else NULL
  meta$id       <- .hv(meta$id, tools::file_path_sans_ext(basename(path)))
  meta$audience <- tolower(as.character(.hv(meta$audience, "user"))[1])
  meta$category <- if (is.null(meta$category)) NA_character_
                    else tolower(as.character(meta$category)[1])
  if (!is.na(meta$category) && !(meta$category %in% HELP_VALID_CATEGORIES))
    warning(sprintf("help: %s has unrecognized category '%s' (expected one of: %s)",
                    basename(path), meta$category, paste(HELP_VALID_CATEGORIES, collapse = ", ")))
  # normalise `params` / `see_also` to plain character vectors
  meta$params   <- if (is.null(meta$params))   character(0) else as.character(unlist(meta$params))
  meta$see_also <- if (is.null(meta$see_also)) character(0) else as.character(unlist(meta$see_also))
  # optional: a database table name whose column/index/FK documentation
  # (from the schema registry, see load_schema_registry() below) renders
  # appended to this note's modal -- for a data.* note explaining a table
  # shown on the Data tab.
  meta$schema_table <- if (is.null(meta$schema_table)) NA_character_
                        else as.character(meta$schema_table)[1]
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

# Merge load_help() across several directories into one registry -- the
# app-wide equivalent of load_help()'s single-directory call. A concept id
# repeated across directories warns and keeps whichever directory's copy was
# seen first (same warn-and-continue style as a duplicate id within one
# directory); same for a param -> id collision across directories.
load_help_merged <- function(dirs) {
  regs <- lapply(dirs, load_help)
  entries <- list(); by_param <- list()
  for (reg in regs) {
    for (id in names(reg)) {
      if (id %in% names(entries)) {
        warning(sprintf("help: duplicate concept id '%s' across help directories; keeping the first one found", id))
        next
      }
      entries[[id]] <- reg[[id]]
    }
    bp <- attr(reg, "by_param")
    for (p in names(bp)) {
      if (p %in% names(by_param) && !identical(by_param[[p]], bp[[p]])) {
        warning(sprintf("help: param '%s' mapped to multiple concepts across help directories (%s, %s)",
                        p, by_param[[p]], bp[[p]]))
        next
      }
      by_param[[p]] <- bp[[p]]
    }
  }
  structure(entries, by_param = by_param)
}

# ---- lookup -----------------------------------------------------------------
# Resolve the registry: explicit arg, else the global HELP_REGISTRY (the
# app-wide merged registry, assigned in global.R), falling back to the older
# HELP_SETTINGS name if only that is set (keeps a pre-generalization global.R
# working). Both are assigned into globalenv() at startup, and the engine's
# own functions also live in globalenv(), so we search there explicitly -- an
# inherits-only walk from here would not descend into the app's child env.
.help_registry <- function(help = NULL) {
  if (!is.null(help)) return(help)
  reg <- get0("HELP_REGISTRY", envir = globalenv(), inherits = TRUE)
  if (!is.null(reg)) return(reg)
  get0("HELP_SETTINGS", envir = globalenv(), inherits = TRUE)
}

# Direct id lookup -- the app-wide equivalent of help_concept_for_param()'s
# param -> id indirection, for a help_icon() call outside the settings
# cascade that already knows its own help_id.
#
# Falls back to the param-name indirection (help_concept_for_param) when the
# direct lookup misses: a single input$help_show event is fired by BOTH
# help_icon() (sends a help_id, e.g. in settings_cascade_ui.R's own
# "Scope:" bar) and settings_help_icon() (sends a calib_settings param_name,
# from a per-row drill icon in the SAME module) -- one shared observer has to
# resolve whichever kind of value it receives, so this is the one place that
# needs to try both rather than each caller guessing which lookup applies.
help_entry_for_id <- function(help_id, help = NULL) {
  reg <- .help_registry(help); if (is.null(reg)) return(NULL)
  entry <- reg[[help_id]]
  if (!is.null(entry)) return(entry)
  help_concept_for_param(help_id, reg)
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

# ---- app-wide (id-direct, no param indirection) ------------------------------
# help_icon()/help_modal_title()/help_modal_body() mirror
# settings_help_icon()/settings_help_title()/settings_help_content() exactly,
# except they take a help_id directly instead of resolving one from a
# settings-cascade param name. Use these anywhere outside the settings
# cascade; use the settings_* versions for a calib_settings_meta param (they
# keep working unchanged).

help_icon <- function(help_id, ns, help = NULL, audience = "user") {
  entry <- help_entry_for_id(help_id, help)
  ok <- !is.null(entry) && .audience_ok(entry, audience) &&
        is.character(entry$body) && nzchar(entry$body)
  if (!ok) return(NULL)
  title <- .hv(entry$title, help_id)
  shiny::tags$span(
    class = "help-icon", tabindex = "0", role = "button",
    `aria-label` = paste("More about", title), title = "Learn more",
    onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority:'event'})",
                      ns("help_show"), help_id),
    bsicons::bs_icon("info-circle"))
}

help_modal_title <- function(help_id, help = NULL) {
  entry <- help_entry_for_id(help_id, help)
  if (is.null(entry)) help_id else .hv(entry$title, help_id)
}

# Resolve [[help_id]] / [[help_id|display text]] inside a note's body/more
# text into a clickable span that fires the SAME ns("help_show") event this
# modal's own icon used -- built at render time because ns() is only known
# then (the stored note is shared across however many modules render it). An
# id that isn't in the registry falls back to plain text (the custom display
# text if given, else the bare id) rather than a broken link.
.resolve_glossary_links <- function(text, ns, help = NULL) {
  if (is.null(text) || !nzchar(text)) return(text)
  reg <- .help_registry(help)
  pattern <- "\\[\\[([A-Za-z0-9_.]+)(\\|([^\\]]+))?\\]\\]"
  matches <- unique(regmatches(text, gregexpr(pattern, text, perl = TRUE))[[1]])
  for (mt in matches) {
    pm   <- regmatches(mt, regexec(pattern, mt, perl = TRUE))[[1]]
    id   <- pm[2]; disp <- pm[4]
    entry <- if (!is.null(reg)) reg[[id]] else NULL
    repl <- if (is.null(entry)) {
      if (nzchar(disp)) disp else id
    } else {
      label <- if (nzchar(disp)) disp else .hv(entry$title, id)
      sprintf(paste0("<span class=\"help-xlink\" tabindex=\"0\" role=\"button\" ",
                     "onclick=\"Shiny.setInputValue('%s', '%s', {priority:'event'})\">%s</span>"),
             ns("help_show"), id, label)
    }
    text <- gsub(mt, repl, text, fixed = TRUE)
  }
  text
}

# "See also" list under a modal's body/more/references -- same visual
# treatment as .render_references() but linking to other help_ids rather
# than external URLs. Silently drops an id that isn't in the registry (never
# shows a dead link) rather than erroring.
.render_see_also <- function(ids, ns, help = NULL) {
  if (is.null(ids) || !length(ids)) return(NULL)
  reg <- .help_registry(help)
  items <- lapply(ids, function(id) {
    entry <- if (!is.null(reg)) reg[[id]] else NULL
    if (is.null(entry)) return(NULL)
    shiny::tags$li(shiny::tags$a(
      href = "#", class = "help-xlink",
      onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority:'event'}); return false;",
                        ns("help_show"), id),
      .hv(entry$title, id)))
  })
  items <- Filter(Negate(is.null), items)
  if (!length(items)) return(NULL)
  shiny::tagList(
    shiny::tags$div(class = "help-ref-heading", "See also"),
    shiny::tags$ul(class = "help-seealso", items))
}

# ---- database schema tables (Columns / Indexes / Referenced By) -------------
# A note's optional `schema_table: <db_table_name>` names an entry here,
# rendered appended to its modal. The registry itself is small, hand-authored
# (or regenerated from a live-DB query, see dev/data_tab_schema_report.sql)
# YAML files under help/schema/<db_table_name>.yaml -- NOT part of the
# help_id/HELP_REGISTRY system (a db table name isn't a help_id, and this
# content is tabular, not a concept explanation), loaded separately.
#
# One file per table:
#   columns:       [{name, type, description, fk}, ...]   # fk: "table.column" or ""/omitted
#   indexes:       [{name, column, type}, ...]
#   referenced_by: [{name, column, table_reference, column_reference}, ...]
# Fail-soft, same convention as load_help(): a missing directory or a file
# that doesn't parse warns and is skipped, never errors the app.
load_schema_registry <- function(dir = "help/schema") {
  if (!dir.exists(dir)) return(list())
  files <- list.files(dir, pattern = "\\.ya?ml$", full.names = TRUE)
  reg <- list()
  for (f in files) {
    parsed <- tryCatch(yaml::yaml.load_file(f),
                       error = function(e) {
                         warning(sprintf("schema: failed to parse %s: %s",
                                         basename(f), conditionMessage(e)))
                         NULL
                       })
    if (is.null(parsed)) next
    tbl <- tools::file_path_sans_ext(basename(f))
    reg[[tbl]] <- parsed
  }
  reg
}

.schema_table_html <- function(rows, cols, empty_msg) {
  if (is.null(rows) || !length(rows))
    return(shiny::tags$p(shiny::tags$em(empty_msg)))
  header <- shiny::tags$tr(lapply(names(cols), function(k) shiny::tags$th(cols[[k]])))
  body <- lapply(rows, function(r) {
    shiny::tags$tr(lapply(names(cols), function(k)
      shiny::tags$td(.hv(as.character(r[[k]]), ""))))
  })
  shiny::tags$table(class = "help-schema-table", header, body)
}

#' Render the Columns / Indexes / "Tables that reference this table" sections
#' for one database table, or NULL if it isn't in the registry (never an
#' error -- a note naming a table not yet documented just shows nothing extra,
#' same fail-soft spirit as a missing help_id).
render_schema_tables <- function(db_table, registry = NULL) {
  reg <- if (!is.null(registry)) registry
         else get0("SCHEMA_REGISTRY", envir = globalenv(), inherits = TRUE)
  tbl <- if (!is.null(reg)) reg[[db_table]] else NULL
  if (is.null(tbl)) return(NULL)
  shiny::tagList(
    shiny::tags$div(class = "help-schema-heading", sprintf("Table: %s", db_table)),
    shiny::tags$div(class = "help-schema-section-label", "Columns"),
    .schema_table_html(tbl$columns,
      list(name = "Name", type = "Type", description = "Description", fk = "Foreign Key"),
      "No column information available."),
    shiny::tags$div(class = "help-schema-section-label", "Indexes"),
    .schema_table_html(tbl$indexes,
      list(name = "Name", column = "Column", type = "Type"),
      "No index information available."),
    shiny::tags$div(class = "help-schema-section-label", "Tables that reference this table"),
    .schema_table_html(tbl$referenced_by,
      list(name = "Name", column = "Column", table_reference = "Table Reference",
          column_reference = "Column Reference"),
      "No other table references this one."))
}

# Modal body for a help_id: title lead + markdown explainer (glossary-linked)
# + an optional "More detail" disclosure for the note's ::: more block (also
# glossary-linked) + see_also + references + an optional schema_table section,
# or NULL when there is nothing user-facing to show. The caller's module
# passes this to showModal() from its own input$help_show observer (one per
# module, same pattern settingsCascadeServer already has).
help_modal_body <- function(help_id, ns, help = NULL, audience = "user") {
  entry <- help_entry_for_id(help_id, help)
  if (is.null(entry) || !.audience_ok(entry, audience) ||
      !is.character(entry$body) || !nzchar(entry$body)) return(NULL)
  reg <- .help_registry(help)
  body_html <- shiny::markdown(.resolve_glossary_links(entry$body, ns, reg))
  more_tag <- NULL
  if (!is.null(entry$more) && nzchar(entry$more)) {
    more_html <- shiny::markdown(.resolve_glossary_links(entry$more, ns, reg))
    more_tag <- shiny::tags$details(class = "help-more",
      shiny::tags$summary("More detail"), more_html)
  }
  schema_tag <- if (!is.na(entry$schema_table) && nzchar(entry$schema_table))
    render_schema_tables(entry$schema_table) else NULL
  shiny::div(class = "help-pop",
    shiny::div(class = "help-pop-body", body_html),
    more_tag,
    .render_see_also(entry$see_also, ns, reg),
    .render_references(entry$references),
    schema_tag)
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
    .help-xlink { color: var(--bs-primary, #337ab7); cursor: pointer; text-decoration: underline dotted; }
    .help-xlink:hover, .help-xlink:focus { text-decoration: underline; outline: none; }
    .help-more { margin-top: .5rem; }
    .help-more summary { cursor: pointer; color: var(--bs-primary, #337ab7); font-size: .85rem;
                         font-weight: 600; }
    .help-more summary:hover { text-decoration: underline; }
    .help-more[open] summary { margin-bottom: .35rem; }
    .help-seealso { font-size: .78rem; padding-left: 1.1rem; margin-bottom: 0; }
    .help-seealso li { margin-bottom: .2rem; }
    .help-schema-heading { font-weight: 700; margin-top: 1rem; margin-bottom: .3rem; }
    .help-schema-section-label { font-size: .72rem; text-transform: uppercase; letter-spacing: .04em;
                                 color: var(--bs-secondary, #6c757d); margin: .7rem 0 .2rem; }
    .help-schema-table { width: 100%; border-collapse: collapse; font-size: .76rem;
                         margin-bottom: .3rem; display: block; overflow-x: auto; white-space: nowrap; }
    .help-schema-table th, .help-schema-table td { border: 1px solid #dee2e6; padding: .3rem .5rem;
                                                   text-align: left; white-space: normal; }
    .help-schema-table th { background-color: #f6f8fa; font-weight: 600; }
    .help-schema-table tr:nth-child(even) td { background-color: #fafbfc; }
  "))
}
