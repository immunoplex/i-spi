# =============================================================================
# settings_cascade_ui.R  --  generic settings editor over the cascade
# -----------------------------------------------------------------------------
# ONE editor for ALL settings (every param_group), replacing the settings surface
# of study_configuration_ui.R. Annotation and data-editing are NOT here -- they
# stay behind the wall in the (shrunken) legacy file for a later refactor.
#
# Design:
#   * Scope breadcrumb: Project > Study > [+ Experiment] > [+ Feature] > [+ Antigen].
#     Deeper tiers are OPT-IN and LIVE-DERIVED from curve_lookup via scope_options():
#       - exactly one value at a tier  -> auto-selected, shown as a static crumb
#       - more than one                -> a selector; the next tier stays hidden
#                                         until a choice is made (ladder = physical)
#       - zero                          -> tier hidden entirely
#     Re-derived on every scope change, so a study that grows gains tiers with no
#     code change.
#   * Editor table: meta-driven, grouped by param_group. Every param shown with
#     its effective value + provenance chip (from tier_rank) + help (param_
#     description) + a revert action enabled only when overridden AT this scope.
#   * Reads settings_editor_view(); writes set_setting()/unset_setting() on db_pool.
#
# Requires settings_cascade_access.R sourced first.
# =============================================================================

# ---- UI ---------------------------------------------------------------------
settingsCascadeUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    if (exists("help_styles", mode = "function")) help_styles(),
    shiny::div(class = "settings-scope-bar",
      shiny::strong("Scope: "),
      shiny::uiOutput(ns("breadcrumb"), inline = TRUE)
    ),
    shiny::hr(),
    shiny::uiOutput(ns("editor"))
  )
}

# ---- Server -----------------------------------------------------------------
# `scope` is a reactive giving at least list(project_id=, study=) -- reuse the
# app's calib_scope. Project & Study come from there; deeper tiers are chosen here.
settingsCascadeServer <- function(id, pool, scope) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    NONE <- "__none__"

    # chosen deeper tiers (NULL = not chosen yet)
    sel <- shiny::reactiveValues(experiment = NULL, feature = NULL, antigen = NULL)

    proj  <- shiny::reactive(scope()$project_id)
    study <- shiny::reactive(scope()$study)

    # Reset deeper selections when the study changes.
    shiny::observeEvent(list(proj(), study()), {
      sel$experiment <- NULL; sel$feature <- NULL; sel$antigen <- NULL
    })

    # ---- live-derived tier state ------------------------------------------
    # For each tier returns list(options, chosen, mode) where mode is
    # "hidden" | "auto" (1 option) | "select" (>1, awaiting/!= chosen).
    exp_state <- shiny::reactive({
      shiny::req(proj(), study())
      opts <- scope_options(pool, proj(), study(), "experiment")
      .tier_state(opts, sel$experiment)
    })
    feat_state <- shiny::reactive({
      st <- exp_state(); if (st$chosen == NONE) return(list(options = character(0), chosen = NONE, mode = "hidden"))
      opts <- scope_options(pool, proj(), study(), "feature", experiment = st$chosen)
      .tier_state(opts, sel$feature)
    })
    ant_state <- shiny::reactive({
      fs <- feat_state(); es <- exp_state()
      if (fs$chosen == NONE) return(list(options = character(0), chosen = NONE, mode = "hidden"))
      opts <- scope_options(pool, proj(), study(), "antigen",
                            experiment = es$chosen, feature = fs$chosen)
      .tier_state(opts, sel$antigen)
    })

    # effective scope + depth (deepest active tier: 2 study .. 5 antigen)
    eff_scope <- shiny::reactive({
      es <- exp_state(); fs <- feat_state(); as <- ant_state()
      depth <- 2L
      if (es$chosen != NONE) depth <- 3L
      if (fs$chosen != NONE) depth <- 4L
      if (as$chosen != NONE) depth <- 5L
      list(experiment = es$chosen, feature = fs$chosen, antigen = as$chosen, depth = depth)
    })

    # ---- breadcrumb UI ----------------------------------------------------
    output$breadcrumb <- shiny::renderUI({
      shiny::req(proj(), study())
      crumbs <- list(
        shiny::tags$span(class = "crumb fixed", paste0("Project ", proj())),
        .sep(), shiny::tags$span(class = "crumb fixed", study())
      )
      crumbs <- c(crumbs, .tier_crumb(ns, "experiment", "Experiment", exp_state()))
      if (exp_state()$chosen != NONE)
        crumbs <- c(crumbs, .tier_crumb(ns, "feature", "Feature", feat_state()))
      if (feat_state()$chosen != NONE)
        crumbs <- c(crumbs, .tier_crumb(ns, "antigen", "Antigen", ant_state()))
      shiny::div(class = "crumbs", crumbs)
    })

    # selector inputs feed back into sel$*
    shiny::observeEvent(input$pick_experiment, {
      sel$experiment <- input$pick_experiment; sel$feature <- NULL; sel$antigen <- NULL
    })
    shiny::observeEvent(input$pick_feature, { sel$feature <- input$pick_feature; sel$antigen <- NULL })
    shiny::observeEvent(input$pick_antigen, { sel$antigen <- input$pick_antigen })

    # ---- editor table -----------------------------------------------------
    ev <- shiny::reactive({
      shiny::req(proj(), study())
      es <- eff_scope()
      settings_editor_view(pool, proj(), study(),
                           experiment = es$experiment, feature = es$feature, antigen = es$antigen)
    }) |> shiny::bindEvent(proj(), study(), eff_scope(), input$refresh, ignoreNULL = FALSE)

    output$editor <- shiny::renderUI({
      df <- ev(); es <- eff_scope()
      if (!nrow(df)) return(shiny::em("No settings defined."))
      groups <- split(df, df$param_group)
      shiny::tagList(lapply(names(groups), function(g) {
        rows <- groups[[g]]
        shiny::div(class = "settings-group",
          shiny::h4(.group_title(g)),
          lapply(seq_len(nrow(rows)), function(i) .param_row(ns, rows[i, ], es$depth)))
      }))
    })

    # ---- write / revert ----------------------------------------------------
    shiny::observeEvent(input$set_param, {
      p <- input$set_param                       # list(param=, value=)
      es <- eff_scope()
      tryCatch({
        set_setting(pool, project = proj(), study = study(),
                    experiment = es$experiment, feature = es$feature, antigen = es$antigen,
                    param_name = p$param, value = p$value, user = currentuser())
        shiny::showNotification(paste0(p$param, " set at this scope"), type = "message")
      }, error = function(e) shiny::showNotification(paste0("Could not set ", p$param, ": ", e$message),
                                                     type = "error"))
    })
    shiny::observeEvent(input$revert_param, {
      es <- eff_scope()
      tryCatch({
        unset_setting(pool, project = proj(), study = study(),
                      experiment = es$experiment, feature = es$feature, antigen = es$antigen,
                      param_name = input$revert_param)
        shiny::showNotification(paste0(input$revert_param, " reverted to inherited"), type = "message")
      }, error = function(e) shiny::showNotification(e$message, type = "error"))
    })

    # ---- help drill-down (Layer 3): open a modal with the concept note --------
    # BS3-safe: shiny modals work under shinydashboard, unlike a bslib popover.
    shiny::observeEvent(input$help_show, {
      pn <- input$help_show
      if (!exists("settings_help_content", mode = "function")) return()
      body <- settings_help_content(pn, audience = "user")
      if (is.null(body)) return()
      shiny::showModal(shiny::modalDialog(
        title     = settings_help_title(pn),
        body, easyClose = TRUE, size = "l",
        footer = shiny::modalButton("Close")))
    })
  })
}

# ---- internal helpers -------------------------------------------------------
.tier_state <- function(opts, chosen) {
  NONE <- "__none__"
  if (length(opts) == 0) list(options = opts, chosen = NONE,          mode = "hidden")
  # else if (length(opts) == 1) list(options = opts, chosen = opts[[1]], mode = "auto")
  else if (is.null(chosen))   list(options = opts, chosen = NONE,      mode = "select")
  else                        list(options = opts, chosen = chosen,    mode = "select")
}

.sep <- function() shiny::tags$span(class = "crumb-sep", " \u25B8 ")
.tier_crumb <- function(ns, tier, label, st) {
  if (st$mode == "hidden") return(NULL)
  if (st$mode == "auto")
    return(list(.sep(), shiny::tags$span(class = "crumb fixed",
                                         title = paste(label, "(only option)"), st$chosen)))
  # selectable
  list(.sep(), shiny::selectInput(ns(paste0("pick_", tier)), NULL,
        choices = c(setNames("__none__", paste0("All ", label, "s")), st$options),
        selected = st$chosen, width = "160px"))
}
.group_title <- function(g) tools::toTitleCase(gsub("_", " ", g))
.param_row <- function(ns, r, depth) {
  prov <- provenance_label(r$tier_rank, depth)
  can_revert <- isTRUE(r$is_overridden_here)
  desc <- if (length(r$param_description) == 1 && !is.na(r$param_description) &&
              nzchar(r$param_description)) r$param_description else NULL
  # Layer 3: concept drill-down icon (opens a modal via input$help_show). NULL
  # when no user-facing note covers this param.
  drill <- if (exists("settings_help_icon", mode = "function"))
             settings_help_icon(r$param_name, ns, audience = "user") else NULL
  shiny::fluidRow(class = "param-row",
                  shiny::column(4,
                                shiny::div(class = "param-label-wrap",
                                           shiny::tags$label(r$param_label %||% r$param_name),
                                           drill),
                                if (!is.null(desc)) shiny::helpText(class = "param-desc", desc)),
                  shiny::column(4, .value_control(ns, r)),
                  shiny::column(2, shiny::tags$span(
                    class = paste0("prov ", if (isTRUE(prov == "set here")) "set" else "inh"), prov)),
                  shiny::column(2, if (isTRUE(can_revert))
                    shiny::actionLink(ns(paste0("revertlink_", r$param_name)), "revert",
                                      onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority:'event'})",
                                                        ns("revert_param"), r$param_name))))
}

# Control chosen from meta; on change, push {param,value} to input$set_param.
# Adds a MULTI-SELECT (checkbox group) branch for model_form_list and
# any other param whose param_control_type contains "multiple".
.value_control <- function(ns, r) {
  set_evt <- ns("set_param")
  pname   <- r$param_name
  # single-value change handler (text/number/select/checkbox)
  onchg <- sprintf("Shiny.setInputValue('%s', {param:'%s', value: this.value}, {priority:'event'})",
                   set_evt, pname)

  val <- if (length(r$effective_text) == 1 && !is.na(r$effective_text)) r$effective_text else ""
  ctl <- if (length(r$param_control_type) == 1 && !is.na(r$param_control_type)) r$param_control_type else "textInput"
  choices <- if (length(r$param_choices_list) == 1 && !is.na(r$param_choices_list) && nzchar(r$param_choices_list))
    trimws(strsplit(r$param_choices_list, ",")[[1]]) else character(0)

  # ---- MULTI-SELECT (checkbox group): the authorized SET, e.g. model_form_list.
  # Every choice from param_choices_list is a checkbox; those in the resolved
  # comma-joined value are checked. On any toggle, gather all checked siblings
  # into one comma-joined string and push it back as this param's value.
  if (grepl("multiple", ctl) && length(choices)) {
    selected <- trimws(strsplit(val, ",")[[1]])
    grp <- paste0("mfset_", pname)   # class to scope the "gather checked" query
    # JS: collect checked boxes sharing this group class, join with commas, send.
    # gather_js <- sprintf(
    #   "var v=Array.from(document.querySelectorAll('.%s:checked')).map(function(e){return e.value;}).join(',');Shiny.setInputValue('%s',{param:'%s',value:v},{priority:'event'});",
    #   grp, set_evt, pname)
    gather_js <- sprintf(
      "var g=this.closest('.mf-checkgroup');var v=Array.from(g.querySelectorAll('input:checked')).map(function(e){return e.value;});v=Array.from(new Set(v)).join(',');Shiny.setInputValue('%s',{param:'%s',value:v},{priority:'event'});",
      set_evt, pname)
    return(shiny::div(class = "mf-checkgroup",
                      lapply(choices, function(c) shiny::tags$label(class = "mf-check",
                                                                    shiny::tags$input(type = "checkbox", class = grp, value = c,
                                                                                      checked = if (c %in% selected) "checked" else NULL,
                                                                                      onchange = gather_js),
                                                                    c))))
  }

  # ---- boolean toggle
  if (grepl("switchInput|checkbox", ctl))
    return(shiny::tags$input(type = "checkbox", checked = if (isTRUE(as.logical(val))) "checked" else NULL,
                             onchange = sub("this.value", "this.checked", onchg, fixed = TRUE)))

  # ---- single-select / radio
  if (grepl("select|radio", ctl) && length(choices))
    return(shiny::tags$select(onchange = onchg,
                              lapply(choices, function(c) shiny::tags$option(value = c,
                                                                             selected = if (identical(c, val)) "selected" else NULL, c))))

  # ---- text / number fallback
  shiny::tags$input(type = if (grepl("numeric", ctl)) "number" else "text",
                    value = val, onchange = onchg)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a
