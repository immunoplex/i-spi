# =============================================================================
# assay_description_rule_ui.R  —  per-type description parse-rule builder + preview
# -----------------------------------------------------------------------------
# A self-contained panel the generic import module mounts (bead / ELISA only;
# flow has no delimited Description). ONE rule per specimen type (X / S / B / C):
# delimiters, element order, dilution mode. For each type it shows a handful of
# STRUCTURALLY-DISTINCT example strings parsed live under the current rule, plus
# a verdict computed over EVERY distinct shape (not just the shown ones), and an
# "approve" checkbox that only enables when the rule is valid for all shapes.
#
# The module gates the template download on ALL present types being approved.
#
# Public surface:
#   ai_rule_ui(ns, de)                                  -> UI (tabset per type)
#   ai_rule_install(input, output, session, de, descriptions_r)
#                                                       -> list(rules, approved)
#
# `de` is the descriptor$description_elements list: list(base, optional, bcs).
# `descriptions_r` is a reactive returning a data.frame(Type, Description[, Well])
# for the parsed batch, or NULL before parsing.
#
# Depends on assay_description_parse.R (ai_default_ruleset, ai_parse_rule,
# ai_propagate_delimiters, ai_representative_descriptions, ai_preview_description)
# and shinyjs + shinyjqui (loaded by the app). Source AFTER assay_description_parse.R.
# =============================================================================

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

AI_RULE_TYPES <- c(X = "Samples (X)", S = "Standards (S)",
                   B = "Blanks (B)",  C = "Controls (C)")

# default description elements if a descriptor doesn't supply them
.ai_default_de <- list(
  base     = c("PatientID", "TimePeriod", "DilutionFactor"),
  optional = c("SampleGroupA", "SampleGroupB"),
  bcs      = c("Source", "DilutionFactor"))


# ---- UI ---------------------------------------------------------------------

ai_rule_ui <- function(ns, de = NULL) {
  de <- de %||% .ai_default_de

  one_type <- function(t) {
    delim_id  <- ns(paste0("rule_delim_",   t))
    dmode_id  <- ns(paste0("rule_dmode_",   t))
    prev_id   <- ns(paste0("rule_preview_", t))
    appr_id   <- ns(paste0("rule_approve_", t))

    order_ctrl <- if (t == "X")
      tagList(
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("rule_opt_X"), label = "Include optional elements",
          choices = de$optional, selected = de$optional,
          status = "outline-primary",
          checkIcon = list(yes = icon("check"), no = icon("times"))),
        uiOutput(ns("rule_order_ui_X")))
    else
      shinyjqui::orderInput(
        inputId = ns(paste0("rule_order_", t)),
        label   = "Element order (drag to reorder)",
        items   = de$bcs, width = "100%", item_class = "info")

    tabPanel(
      title = unname(AI_RULE_TYPES[t]), value = t,
      tags$div(style = "padding-top:10px;",
        textInput(delim_id, "Delimiters (each character separates a component)",
                  value = "_"),
        tags$small(style = "color:#666;",
          "Type every separator that can appear, e.g. \"_\", or \"_ ,;\" for ",
          "underscore/space/comma/semicolon. \":\" and \"/\" are reserved for a ",
          "1:100 or 1/100 dilution."),
        tags$br(), tags$br(),
        order_ctrl,
        radioButtons(dmode_id, "Dilution detection",
          choices = c("Automatic (find 1:N or integer)" = "auto",
                      "Positional (fixed slot)"          = "position"),
          selected = "auto", inline = TRUE),
        tags$hr(),
        tags$label(style = "font-weight:600;", "Live preview (representative examples)"),
        uiOutput(prev_id),
        tags$hr(),
        checkboxInput(appr_id, "This rule is correct — approve", value = FALSE)))
  }

  tagList(
    tags$h5("Description parse rules (one per specimen type)"),
    do.call(tabsetPanel, c(list(id = ns("rule_tabs")),
                           lapply(names(AI_RULE_TYPES), one_type))),
    tags$div(style = "margin-top:8px;", textOutput(ns("rule_overall"))))
}


# ---- Server -----------------------------------------------------------------

ai_rule_install <- function(input, output, session, de = NULL, descriptions_r) {
  de     <- de %||% .ai_default_de
  ns     <- session$ns
  types  <- names(AI_RULE_TYPES)

  rules_rv   <- reactiveVal(ai_default_ruleset(types))
  touched_rv <- reactiveVal(character())

  # descriptions for one type (empty if nothing parsed / none of that type)
  desc_for <- function(t) {
    dd <- descriptions_r()
    if (is.null(dd) || !all(c("Type", "Description") %in% names(dd))) return(character())
    keep <- !is.na(dd$Type) & substr(as.character(dd$Type), 1L, 1L) == t
    d <- as.character(dd$Description[keep])
    d[!is.na(d) & nzchar(trimws(d))]
  }
  present <- function(t) length(desc_for(t)) > 0L

  # X order UI depends on the optional-elements toggle
  output$rule_order_ui_X <- renderUI({
    opt   <- input$rule_opt_X
    items <- c(de$base, opt[opt %in% de$optional])
    shinyjqui::orderInput(
      inputId = ns("rule_order_X"),
      label   = "Element order (drag to reorder)",
      items   = items, width = "100%", item_class = "primary")
  })

  # current element order for a type, from its orderInput (fallback to defaults)
  order_for <- function(t) {
    v <- input[[paste0("rule_order_", t)]]
    if (is.null(v) || !length(v)) if (t == "X") de$base else de$bcs else as.character(v)
  }

  reset_approval <- function(t)
    updateCheckboxInput(session, paste0("rule_approve_", t), value = FALSE)

  # write one type's rule into the store from the live inputs
  refresh_rule <- function(t) {
    rs <- rules_rv()
    rs[[t]] <- ai_parse_rule(
      type          = t,
      delimiters    = input[[paste0("rule_delim_",  t)]] %||% "_",
      order         = order_for(t),
      dilution_mode = input[[paste0("rule_dmode_",  t)]] %||% "auto")
    rules_rv(rs)
  }

  # per-type observers: delimiter (with first-edit seeding), order, dilution mode
  lapply(types, function(t) local({
    tt <- t

    observeEvent(input[[paste0("rule_delim_", tt)]], {
      d  <- input[[paste0("rule_delim_", tt)]] %||% "_"
      rs <- rules_rv(); rs[[tt]]$delimiters <- d
      seen <- touched_rv()
      if (!length(seen)) {                         # first delimiter chosen anywhere
        rs <- ai_propagate_delimiters(rs, d, except = tt)
        for (u in setdiff(types, tt))
          updateTextInput(session, paste0("rule_delim_", u), value = d)
      }
      touched_rv(union(seen, tt)); rules_rv(rs); reset_approval(tt)
    }, ignoreInit = TRUE)

    observeEvent(input[[paste0("rule_order_", tt)]],
                 { refresh_rule(tt); reset_approval(tt) }, ignoreInit = TRUE)
    observeEvent(input[[paste0("rule_dmode_", tt)]],
                 { refresh_rule(tt); reset_approval(tt) }, ignoreInit = TRUE)
    if (tt == "X")
      observeEvent(input$rule_opt_X,
                   { refresh_rule("X"); reset_approval("X") }, ignoreInit = TRUE)
  }))

  # per-type representative sampling (shared by the preview + the approve gate)
  rep_r <- setNames(lapply(types, function(t) reactive({
    d <- desc_for(t); if (!length(d)) return(NULL)
    ai_representative_descriptions(d, t, rules_rv()[[t]], n = 4L)
  })), types)

  # render preview + toggle the approve checkbox enabled state
  lapply(types, function(t) local({
    tt <- t

    output[[paste0("rule_preview_", tt)]] <- renderUI({
      rep <- rep_r[[tt]]()
      if (is.null(rep))
        return(tags$em("Upload and parse instrument file(s) to preview examples."))
      rule <- rules_rv()[[tt]]

      chips <- lapply(rep$examples, function(s) {
        pv <- ai_preview_description(tt, s, rule)
        fld <- lapply(seq_len(nrow(pv$fields)), function(i) {
          f  <- pv$fields[i, ]
          bg <- if (f$ok) "#e6f4ea" else if (f$required) "#fdecea" else "#eef1f4"
          br <- if (f$ok) "#69b57b" else if (f$required) "#d9534f" else "#c9ced4"
          tags$span(style = sprintf(
            "display:inline-block;margin:2px 4px;padding:2px 8px;border-radius:10px;background:%s;border:1px solid %s;",
            bg, br),
            tags$b(f$field), ": ",
            if (nzchar(f$value)) f$value else tags$span(style = "color:#b02a37;", "(missing)"))
        })
        tags$div(style = "margin-bottom:8px;",
          tags$code(s),
          tags$div(style = "margin-top:3px;", fld))
      })

      ok  <- isTRUE(rep$all_valid)
      col <- if (ok) "#2e7d32" else "#b02a37"
      verdict <- tags$div(style = sprintf("margin-bottom:8px;font-weight:600;color:%s;", col),
        if (ok)
          sprintf("Rule valid for all %d distinct shape(s).", rep$n_distinct_shapes)
        else
          sprintf("Rule fails on %d of %d distinct shape(s) — e.g. %s",
                  rep$n_failing_shapes, rep$n_distinct_shapes,
                  paste(utils::head(rep$failing_examples, 3), collapse = " ; ")))

      shown_note <- if (rep$n_distinct_shapes > rep$n_shown)
        tags$div(style = "color:#666;font-size:90%;",
                 sprintf("Showing %d of %d distinct shapes.",
                         rep$n_shown, rep$n_distinct_shapes)) else NULL

      tagList(verdict, chips, shown_note)
    })

    observe({
      rep <- rep_r[[tt]]()
      enable <- !is.null(rep) && isTRUE(rep$all_valid)
      shinyjs::toggleState(id = paste0("rule_approve_", tt), condition = enable)
      if (!enable) updateCheckboxInput(session, paste0("rule_approve_", tt), value = FALSE)
    })
  }))

  # every PRESENT type must be approved AND valid for all its shapes
  approved <- reactive({
    dd <- descriptions_r()
    if (is.null(dd)) return(FALSE)
    for (t in types) if (present(t)) {
      rep <- rep_r[[t]]()
      if (!isTRUE(input[[paste0("rule_approve_", t)]]) || is.null(rep) || !isTRUE(rep$all_valid))
        return(FALSE)
    }
    TRUE
  })

  output$rule_overall <- renderText({
    dd <- descriptions_r()
    if (is.null(dd)) return("Parse instrument file(s) to configure parse rules.")
    if (isTRUE(approved()))
      return("All specimen-type rules approved — template download enabled.")
    "Approve the rule for each specimen type to enable template download."
  })

  list(rules = reactive(rules_rv()), approved = approved)
}
