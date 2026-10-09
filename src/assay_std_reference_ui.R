# =============================================================================
# assay_std_reference_ui.R  --  "Stage 1.5" of the description pre-processor
# -----------------------------------------------------------------------------
# Mounted between the plate grid (Stage 1) and the description/shape engine
# (Stages 2-3). For a specimen type whose dilution can't be recovered from the
# instrument file or the Description text (Standards are the first, and so
# far only, real case -- see RBX_DILUTION_AUTHORITATIVE_SOURCE_PLAN.md's
# Phase 0 Findings), this is where the user supplies it: an explicit
# label -> dilution mapping, entered once per experiment, remembered for every
# later batch in that same experiment via the settings cascade.
#
# Deliberately small. This is a stopgap entry point for the wells parsing
# genuinely could not resolve, not a general-purpose editor -- it shows only
# the distinct descriptions still needing a value and gets out of the way
# once they're filled in. Context-specific settings entered this way (at the
# point and moment the gap is discovered) are part of a study's documentation,
# not disposable config -- see the memory note this is built against; keep
# this screen easy and timely rather than featureful.
#
# Public surface:
#   ai_std_reference_ui(id)
#   ai_std_reference_server(id, inventory_rv, pool, scope,
#                           enabled = reactive(TRUE), specimen_type = "S",
#                           refresh = reactive(NULL))
#     scope: a reactive returning list(project_id, study, experiment, user) --
#       the SAME shape assay_import_module.R's own `scope` argument has.
#     refresh: optional reactive whose changing value (any change, any type)
#       forces a reload of the saved reference from the DB -- for a caller
#       that writes to it from outside this module (e.g. an auto-seed step).
#     returns list(ready = reactive(lgl), reference = reactive(data.frame or NULL))
#
# Depends on assay_std_reference_rules.R (pure logic), assay_well_inventory.R
# (AI_SPECIMEN_LABELS), settings_cascade_access.R (resolve_settings_scoped,
# set_setting), shiny, DT. Source AFTER all of those, BEFORE
# assay_import_module.R.
# =============================================================================

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

ai_std_reference_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("status")),
    conditionalPanel(
      condition = sprintf("output['%s']", ns("has_candidates")),
      DT::dataTableOutput(ns("table")),
      tags$div(style = "margin-top:8px;",
        actionButton(ns("save"), "Save dilution reference", class = "btn-primary")),
      tags$p(tags$small(style = "color:#5f6368;",
        "These wells' dilution could not be read from the instrument file or ",
        "the Description text. Enter the true dilution for each — it is ",
        "remembered for the rest of this experiment, so a later batch won't ",
        "ask again for a description already covered.")),
      bsCollapse(id = ns("paste_collapse"),
        bsCollapsePanel(
          title = "Or paste a two-column table from a spreadsheet",
          tags$p(tags$small(style = "color:#5f6368;",
            "Copy a Standard point / Dilution factor table out of Excel (a header row ",
            "is fine, it's detected and skipped) and paste it below. Rows are matched to ",
            "the table above by description text first, then by the trailing number if the ",
            "exact text differs (e.g. a pasted “STD_1” matches “S1”).")),
          textAreaInput(ns("paste_box"), NULL, rows = 4,
                       placeholder = "STD_1\t50\nSTD_2\t125\nSTD_3\t313\n..."),
          actionButton(ns("paste_apply"), "Fill dilutions from pasted table", class = "btn-sm"))
      )
    )
  )
}

ai_std_reference_server <- function(id, inventory_rv, pool, scope,
                                    enabled = reactive(TRUE), specimen_type = "S",
                                    refresh = reactive(NULL)) {
  force(inventory_rv); force(pool); force(scope); force(enabled); force(specimen_type)
  force(refresh)

  PARAM <- "standard_dilution_reference"

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── the currently-saved reference for this experiment ───────────────────
    # Re-read whenever the scope (project/study/experiment) changes -- a
    # different experiment has a different saved set.
    saved_rv  <- reactiveVal(AI_STD_REFERENCE_EMPTY)
    scope_key <- reactive({
      s <- scope() %||% list()
      paste(s$project_id %||% "", s$study %||% "", s$experiment %||% "", sep = "\r")
    })

    # Re-read whenever the scope changes OR `refresh` ticks -- the latter lets
    # a caller force a reload after writing to the saved reference from
    # elsewhere (e.g. assay_import_module.R auto-seeding it from a reader's
    # own authoritative source right after parsing), without which this
    # already-mounted module would keep showing a stale saved_rv.
    observeEvent(list(scope_key(), refresh()), {
      s <- scope() %||% list()
      if (is.null(s$project_id) || is.null(s$study) || !nzchar(s$experiment %||% "")) {
        saved_rv(AI_STD_REFERENCE_EMPTY)
        return()
      }
      df <- tryCatch({
        r   <- resolve_settings_scoped(pool, project = s$project_id, study = s$study,
                                       experiment = s$experiment)
        row <- r[r$param_name == PARAM, , drop = FALSE]
        ai_std_reference_parse(if (nrow(row)) row$value_text[1] else NULL)
      }, error = function(e) AI_STD_REFERENCE_EMPTY)
      saved_rv(df)
    }, ignoreNULL = FALSE)

    # ── candidates: distinct descriptions still needing a value ─────────────
    candidates <- reactive({
      if (!isTRUE(enabled())) return(AI_STD_REFERENCE_EMPTY[0L, c("description"), drop = FALSE])
      inv <- inventory_rv()
      tryCatch(ai_std_reference_candidates(inv, specimen_type, saved_rv()),
               error = function(e) {
                 warning(paste("std reference candidate scan failed:", conditionMessage(e)))
                 data.frame(description = character(), n_wells = integer(),
                           stringsAsFactors = FALSE)
               })
    })

    # staged edits: candidates + an empty Dilution column the user fills in.
    # Reset whenever the candidate set itself changes (new batch, or just
    # saved -- either way whatever was staged no longer applies).
    staged_rv <- reactiveVal(NULL)
    observeEvent(candidates(), {
      cd <- candidates()
      staged_rv(data.frame(
        description = cd$description, n_wells = cd$n_wells,
        dilution    = rep(NA_real_, nrow(cd)),
        stringsAsFactors = FALSE))
    })

    output$has_candidates <- reactive({ !is.null(staged_rv()) && nrow(staged_rv()) > 0L })
    outputOptions(output, "has_candidates", suspendWhenHidden = FALSE)

    output$table <- DT::renderDataTable({
      df <- staged_rv()
      req(df)
      DT::datatable(df, rownames = FALSE, selection = "none",
        colnames = c("Description", "Wells", "Dilution"),
        editable = list(target = "cell", disable = list(columns = c(0, 1))),
        options = list(dom = "t", paging = FALSE))
    })
    # The table lives inside a conditionalPanel gated on has_candidates (above),
    # which can go from hidden -> visible more than once in one session (e.g.
    # applying/confirming a plate-grid proposal can flip grid$ready() off then
    # back on). Without this, Shiny suspends the DT render while the panel is
    # hidden and the suspended output can come back stale/empty on re-show
    # instead of redrawing from the current staged_rv() -- same reasoning as
    # has_candidates's own suspendWhenHidden=FALSE two lines up.
    outputOptions(output, "table", suspendWhenHidden = FALSE)

    observeEvent(input$table_cell_edit, {
      info <- input$table_cell_edit
      df <- staged_rv(); if (is.null(df) || !nrow(df)) return()
      if (info$col == 2L)
        df$dilution[info$row] <- suppressWarnings(as.numeric(info$value))
      staged_rv(df)
    })

    observeEvent(input$paste_apply, {
      df <- staged_rv()
      if (is.null(df) || !nrow(df)) return()
      pasted <- ai_std_reference_parse_pasted(input$paste_box)
      if (!nrow(pasted)) {
        showNotification("Nothing parseable in the pasted text.", type = "warning")
        return()
      }
      matched <- ai_std_reference_match_pasted(df$description, pasted)
      hit <- !is.na(matched$dilution)
      df$dilution[hit] <- matched$dilution[hit]
      staged_rv(df)

      skipped <- attr(pasted, "skipped")
      unmatched_pasted <- setdiff(trimws(pasted$description), trimws(matched$matched_from[hit]))
      msg <- sprintf("Filled %d of %d row(s) from the paste.", sum(hit), nrow(df))
      if (length(skipped))
        msg <- paste0(msg, sprintf(" %d line(s) couldn't be parsed.", length(skipped)))
      if (length(unmatched_pasted))
        msg <- paste0(msg, sprintf(" %d pasted row(s) didn't match a description here: %s.",
                                   length(unmatched_pasted), paste(unmatched_pasted, collapse = ", ")))
      showNotification(msg, type = if (sum(hit) > 0) "message" else "warning", duration = 8)
    })

    observeEvent(input$save, {
      df <- staged_rv()
      if (is.null(df) || !nrow(df)) return()
      bad <- !is.finite(df$dilution) | df$dilution <= 0
      if (any(bad)) {
        showNotification(sprintf("%d row(s) still need a positive dilution value.",
                                 sum(bad)), type = "warning")
        return()
      }
      s <- scope() %||% list()
      merged <- ai_std_reference_merge(saved_rv(), df[, c("description", "dilution")])
      ok <- tryCatch({
        set_setting(pool, project = s$project_id, study = s$study,
                   param_name = PARAM, value = ai_std_reference_serialize(merged),
                   user = s$user, experiment = s$experiment)
        TRUE
      }, error = function(e) {
        showNotification(paste("Save failed:", conditionMessage(e)),
                         type = "error", duration = NULL)
        FALSE
      })
      if (ok) {
        saved_rv(merged)
        showNotification("Dilution reference saved.", type = "message")
      }
    })

    output$status <- renderUI({
      if (!isTRUE(enabled())) return(NULL)
      label <- tolower(AI_SPECIMEN_LABELS[[specimen_type]] %||% specimen_type)
      cd <- candidates()
      if (!nrow(cd))
        return(tags$div(style = "color:#2e7d32;font-weight:600;",
          sprintf("%s dilution: nothing needs manual entry.",
                  AI_SPECIMEN_LABELS[[specimen_type]] %||% specimen_type)))
      tags$div(style = "color:#b02a37;font-weight:600;",
        sprintf("%d %s description(s) need a dilution entered before continuing.",
                nrow(cd), label))
    })

    ready <- reactive({
      if (!isTRUE(enabled())) return(FALSE)
      nrow(candidates()) == 0L
    })

    reference <- reactive({
      df <- saved_rv()
      if (is.null(df) || !nrow(df)) return(NULL)
      cbind(specimen_type = specimen_type, df, stringsAsFactors = FALSE)
    })

    list(ready = ready, reference = reference)
  })
}
