# =============================================================================
# assay_shape_ui.R  --  Stages 2 & 3 UI
# -----------------------------------------------------------------------------
# One screen per specimen type, gated on the plate grid reporting ready:
#
#   Stage 2   pick the delimiter set (with a suggestion scored from the corpus),
#             pick the classification granularity (count / format / content),
#             and see the descriptions grouped live into SHAPES
#   Stage 3   select a shape, bind each identity component to a token slot, a
#             format pattern, a constant, or the type code, and watch the
#             resolved values update string by string
#
# -----------------------------------------------------------------------------
# WHAT CHANGED AND WHY (two reported faults, one root cause)
#
# FAULT A  "Apply to this group" threw the user back to the Samples (X) tab and
#          the first description group, losing their place.
# FAULT B  After ticking "approve", this section became unresponsive.
#
# Both came from output$type_tabs calling rule_of() inside renderUI, which made
# the whole tabset depend on rules_rv(). Every apply and every approval rebuilt
# the entire tabset, which (A) reset the selected tab to the first panel and
# re-rendered the shapes table with a hardcoded selected = 1, and (B) destroyed
# and recreated every DT and input in it -- while sixteen separate call sites
# each recomputed ai_shape_table() over the full corpus, and ai_ruleset_ready()
# re-resolved every distinct string of every shape of every type on top.
#
# The fixes:
#   * the tabset depends ONLY on types_present(). Rule-derived control values
#     are read with isolate() at build time and kept in step afterwards by an
#     observer, so changing a rule no longer rebuilds the UI that edits it.
#   * shape_state[[type]]() is ONE cached reactive per specimen type holding the
#     shape table, the per-well keys and the per-shape verdicts. Every panel
#     reads it; nothing recomputes a shape table on its own.
#   * the shapes table re-renders with the row matching the CURRENT shape key,
#     not row 1, and the tabset restores the selected tab.
#   * full-batch resolution is deferred until every group is approved. Before
#     that the summary reports progress from the cached verdicts, so approving a
#     group no longer triggers a resolve of the whole plate set.
#   * approving bumps refresh_rv, which forces a clean recompute of the cached
#     state for the section while preserving tab and group selection.
#   * every cached computation is wrapped in tryCatch, so a bad rule shows as a
#     failed verdict instead of killing the outputs and leaving a dead panel.
#
# Public surface (unchanged):
#   ai_shape_ui(id)
#   ai_shape_server(id, inventory_rv, enabled = reactive(TRUE), assay = reactive(NA))
#     returns list(ruleset, ready, resolved, issues)
#
# Depends on assay_well_inventory.R, assay_shape_rules.R, shiny, DT.
# =============================================================================

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

AI_SHAPE_BY_LABELS <- c(
  "Number of elements"          = "count",
  "Element formats (default)"   = "format",
  "Element formats + values"    = "content")

AI_HOW_LABELS <- c(
  "Element position"      = "slot",
  "Find by format"        = "pattern",
  "Same value every well" = "constant",
  "From the type code"    = "from_type",
  "Not used"              = "ignore")


# ---- UI ---------------------------------------------------------------------

ai_shape_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(gsub("@ID@", ns("wrap"), '
      #@ID@ .ai-chip { display:inline-block; margin:2px 5px 2px 0; padding:3px 9px;
                       border-radius:11px; border:1px solid #c9ced4; background:#f6f8fa;
                       font-family: ui-monospace, Menlo, Consolas, monospace;
                       font-size:12px; }
      #@ID@ .ai-chip b { color:#1a73e8; }
      #@ID@ .ai-chip i { color:#5f6368; font-style:normal; font-size:11px; }
      #@ID@ .ai-req  { color:#b02a37; font-weight:600; }
      #@ID@ .ai-ok   { color:#2e7d32; font-weight:600; }
    ', fixed = TRUE))),

    tags$div(id = ns("wrap"),
      uiOutput(ns("gate_notice")),
      uiOutput(ns("type_tabs")),

      wellPanel(
        tags$h4("Parse profile"),
        tags$p(tags$small(
          "Save these rules to a file and load it for the next batch from the ",
          "same source. Useful when a format does not suit the suggested defaults.")),
        fluidRow(
          column(5, textInput(ns("profile_name"), "Profile name",
                              placeholder = "e.g. Nijmegen bead panel")),
          column(3, tags$div(style = "margin-top:25px;",
                    downloadButton(ns("profile_dl"), "Save profile"))),
          column(4, fileInput(ns("profile_up"), "Load profile",
                              accept = c(".yaml", ".yml", ".json")))
        ),
        textInput(ns("profile_notes"), "Notes (optional)", width = "100%"),
        uiOutput(ns("profile_report"))
      ),

      wellPanel(
        tags$h4("Description rules check"),
        uiOutput(ns("overall"))
      )
    )
  )
}


# ---- Server -----------------------------------------------------------------

ai_shape_server <- function(id, inventory_rv, enabled = reactive(TRUE),
                            assay = reactive(NA_character_)) {
  force(inventory_rv); force(enabled)
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rules_rv    <- reactiveVal(list())
    approved_rv <- reactiveVal(list())     # key "T\rshape_key" -> TRUE
    sel_shape   <- reactiveValues()        # per type: selected shape_key
    report_rv   <- reactiveVal(NULL)       # profile-fit report
    refresh_rv  <- reactiveVal(0L)         # explicit invalidation for the section
    fingerprints_rv <- reactiveVal(list()) # per-type description corpus last seen

    bump <- function() refresh_rv(isolate(refresh_rv()) + 1L)

    # ── helpers ──────────────────────────────────────────────────────────────
    inv <- reactive(inventory_rv())

    types_present <- reactive({
      d <- inv()
      if (is.null(d) || !nrow(d)) return(character())
      intersect(AI_SPECIMEN_TYPES, unique(stats::na.omit(d$specimen_type)))
    })

    desc_of <- function(t) {
      d <- inv()
      if (is.null(d) || !nrow(d)) return(character())
      d$description[!is.na(d$specimen_type) & d$specimen_type == t]
    }
    plates_of <- function(t) {
      d <- inv()
      if (is.null(d) || !nrow(d)) return(character())
      d$plate_key[!is.na(d$specimen_type) & d$specimen_type == t]
    }

    rule_of <- function(t) {
      r <- rules_rv()[[t]]
      if (!is.null(r)) return(r)
      ai_shape_rule(t, "_", "format", list(), NULL, list())
    }

    approve_key  <- function(t, k) paste(t, k, sep = "\r")
    is_approved  <- function(t, k) isTRUE(approved_rv()[[approve_key(t, k)]])
    set_approved <- function(t, k, val) {
      a <- approved_rv(); a[[approve_key(t, k)]] <- isTRUE(val); approved_rv(a)
    }
    clear_type_approvals <- function(t) {
      a <- approved_rv()
      a[grepl(paste0("^", t, "\r"), names(a))] <- NULL
      approved_rv(a)
    }

    # ── ONE cached state per specimen type ───────────────────────────────────
    # Shape table + per-well keys + per-shape verdicts, computed once per change
    # to (inventory, that type's rule, refresh). Built statically for all four
    # types so no reactive is ever created inside an observer.
    shape_state <- local({
      out <- list()
      for (t in AI_SPECIMEN_TYPES) out[[t]] <- local({
        tt <- t
        reactive({
          refresh_rv()                          # explicit invalidation hook
          d <- inv()
          if (is.null(d) || !nrow(d)) return(NULL)
          rule <- rules_rv()[[tt]]
          if (is.null(rule)) return(NULL)

          desc   <- desc_of(tt)
          plates <- plates_of(tt)
          st <- tryCatch(
            ai_shape_table(desc, rule$delimiters, rule$shape_by, plates = plates),
            error = function(e) {
              warning(sprintf("shape table failed for type %s: %s", tt,
                              conditionMessage(e)))
              NULL
            })
          if (is.null(st)) return(NULL)

          verdicts <- lapply(st$shapes$shape_key, function(k) {
            strs <- unique(desc[!is.na(st$keys) & st$keys == k])
            tryCatch(ai_shape_verdict(tt, k, strs, rule),
                     error = function(e)
                       list(ok = FALSE, n_strings = length(strs), n_failing = length(strs),
                            failing_examples = utils::head(strs, 1L),
                            missing_components = "rule error"))
          })
          names(verdicts) <- st$shapes$shape_key

          list(shapes = st$shapes, keys = st$keys, desc = desc,
               verdicts = verdicts, rule = rule)
        })
      })
      out
    })

    state_of <- function(t) {
      f <- shape_state[[t]]
      if (is.null(f)) NULL else f()
    }

    # ── keep the ruleset in step with the inventory's CURRENT types ──────────
    # This used to seed ONCE ("if (!length(rules_rv()))"), so it only ever
    # covered the specimen types present when the files were first parsed.
    # Editing wells on the plate grid changes that set: retyping two wells to
    # Control introduced a type with NO RULE, which surfaced as a Controls tab
    # with no description groups -- nothing to select, nothing to apply, nothing
    # to approve, and a gate that could never open.
    #
    # Now it runs on every inventory change. A per-type fingerprint of the
    # description corpus means an untouched type is not disturbed at all, so
    # retyping a blank does not invalidate approved sample rules.
    observeEvent(inv(), {
      d <- inv()
      if (is.null(d) || !nrow(d)) {
        rules_rv(list()); approved_rv(list()); fingerprints_rv(list()); return()
      }

      sync <- tryCatch(
        ai_ruleset_sync(d, rules_rv(), fingerprints_rv()),
        error = function(e) {
          showNotification(paste("Could not propose description rules:",
                                 conditionMessage(e)),
                           type = "error", duration = NULL)
          NULL
        })
      if (is.null(sync)) return()

      changed <- length(sync$added) || length(sync$refreshed)
      rules_rv(sync$rules)
      fingerprints_rv(sync$fingerprints)

      # A type whose shapes were rebuilt may have lost or re-keyed groups. An
      # approval for a group that no longer exists would let that type read as
      # fully approved on the strength of nothing.
      stale <- ai_stale_approvals(sync$rules, approved_rv())
      if (length(stale)) {
        a <- approved_rv(); a[stale] <- NULL; approved_rv(a)
      }
      for (t in c(sync$added, sync$refreshed)) sel_shape[[t]] <- NA_character_

      if (length(sync$added))
        showNotification(
          sprintf("%s now present after your edits \u2014 configure the description group(s) below.",
                  paste(vapply(sync$added, function(t)
                    sprintf("%s (%s)", AI_SPECIMEN_LABELS[[t]], t), character(1)),
                    collapse = ", ")),
          type = "warning", duration = 12)

      if (changed) bump()
    }, ignoreNULL = FALSE)

    # default the selected group per type once its state exists, without
    # overriding a selection the user has already made
    observe({
      for (t in types_present()) {
        ss <- state_of(t)
        if (is.null(ss) || !nrow(ss$shapes)) next
        cur <- sel_shape[[t]]
        if (is.null(cur) || is.na(cur) || !(cur %in% ss$shapes$shape_key))
          sel_shape[[t]] <- ss$shapes$shape_key[1]
      }
    })

    output$gate_notice <- renderUI({
      if (isTRUE(enabled())) return(NULL)
      tags$div(class = "alert alert-warning",
        "Confirm the plate layout first. The description rules depend on which",
        " wells are samples, standards, blanks and controls.")
    })

    # ── the per-type tabset ──────────────────────────────────────────────────
    # DEPENDS ONLY ON types_present(). Everything rule-derived is isolate()d:
    # if this reacted to rules_rv() then applying a rule would rebuild the UI
    # that applied it, resetting the tab and the selected group (fault A).
    output$type_tabs <- renderUI({
      tp <- types_present()
      if (!length(tp))
        return(tags$em("Parse instrument file(s) and confirm the layout to configure description rules."))

      keep_tab <- isolate(input$type_tabs_inner)
      if (is.null(keep_tab) || !(keep_tab %in% tp)) keep_tab <- tp[1]

      panels <- lapply(tp, function(t) {
        init_delim <- isolate(rule_of(t)$delimiters)
        init_by    <- isolate(rule_of(t)$shape_by)
        tabPanel(
          title = sprintf("%s (%s)", AI_SPECIMEN_LABELS[[t]], t), value = t,
          tags$div(style = "padding-top:12px;",

            tags$h5("1. How is the description separated?"),
            fluidRow(
              column(5, textInput(ns(paste0("delim_", t)),
                                  "Separator characters", value = init_delim)),
              column(7, tags$div(style = "margin-top:25px;",
                        uiOutput(ns(paste0("delimsugg_", t)))))
            ),
            tags$small(style = "color:#5f6368;",
              "Every character you type separates elements. Use \"_ ,;\" for ",
              "underscore, space, comma and semicolon together. \":\" and \"/\" ",
              "are never separators \u2014 they belong to a 1:100 dilution."),
            tags$br(), tags$br(),
            radioButtons(ns(paste0("shapeby_", t)), "Group descriptions by",
                         choices = AI_SHAPE_BY_LABELS, selected = init_by,
                         inline = TRUE),

            tags$hr(),
            tags$h5("2. Description groups"),
            DT::dataTableOutput(ns(paste0("shapes_", t))),
            tags$small(style = "color:#5f6368;",
                       "Select a group to set what its elements mean."),

            tags$hr(),
            tags$h5("3. What does each element mean?"),
            uiOutput(ns(paste0("chips_", t))),
            uiOutput(ns(paste0("binds_", t))),
            tags$div(style = "margin-top:8px;",
              actionButton(ns(paste0("apply_", t)), "Apply to this group",
                           class = "btn-primary btn-sm"),
              actionButton(ns(paste0("applyall_", t)),
                           "Apply to all groups with the same element count",
                           class = "btn-sm")),

            tags$hr(),
            tags$h5("4. Result"),
            uiOutput(ns(paste0("verdict_", t))),
            DT::dataTableOutput(ns(paste0("preview_", t))),
            tags$div(style = "margin-top:8px;",
              checkboxInput(ns(paste0("approve_", t)),
                            "These rules are correct for this group \u2014 approve",
                            value = FALSE)),
            uiOutput(ns(paste0("rollup_", t)))
          ))
      })

      do.call(tabsetPanel,
              c(list(id = ns("type_tabs_inner"), selected = keep_tab), panels))
    })

    # Keep the delimiter / granularity controls in step when a rule changes from
    # OUTSIDE the panel (a loaded profile). Without this the tabset no longer
    # re-renders on rule changes, so the controls would show stale values.
    observeEvent(report_rv(), {
      rs <- rules_rv()
      for (t in names(rs)) {
        updateTextInput(session, paste0("delim_", t), value = rs[[t]]$delimiters)
        updateRadioButtons(session, paste0("shapeby_", t), selected = rs[[t]]$shape_by)
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # ── static per-type wiring (all four types, present or not) ──────────────
    lapply(AI_SPECIMEN_TYPES, function(t) local({
      tt <- t

      current_shape <- reactive({
        ss <- state_of(tt)
        if (is.null(ss) || !nrow(ss$shapes)) return(NULL)
        k <- sel_shape[[tt]]
        if (is.null(k) || is.na(k) || !(k %in% ss$shapes$shape_key))
          k <- ss$shapes$shape_key[1]
        k
      })

      shape_strings <- reactive({
        ss <- state_of(tt); k <- current_shape()
        if (is.null(ss) || is.null(k)) return(character())
        unique(ss$desc[!is.na(ss$keys) & ss$keys == k])
      })

      # -- delimiter suggestions
      output[[paste0("delimsugg_", tt)]] <- renderUI({
        d <- desc_of(tt); if (!length(d)) return(NULL)
        s <- tryCatch(ai_suggest_delimiters(d), error = function(e) NULL)
        if (is.null(s)) return(NULL)
        s <- s[s$score > 0, , drop = FALSE]
        if (!nrow(s))
          return(tags$small(style = "color:#8a6d3b;",
            "No separator splits these strings \u2014 each description looks like one element."))
        top <- utils::head(s, 3L)
        tagList(
          tags$small(style = "color:#5f6368;", "Suggested: "),
          lapply(seq_len(nrow(top)), function(i)
            actionButton(ns(paste0("usedelim_", tt, "_", i)),
                         sprintf("\"%s\"  (%.0f%% of strings, %d element count%s)",
                                 top$delimiter[i], 100 * top$coverage[i],
                                 top$n_token_counts[i],
                                 if (top$n_token_counts[i] == 1) "" else "s"),
                         class = "btn-xs btn-default",
                         style = "margin-right:6px;")))
      })

      lapply(1:3, function(i) local({
        ii <- i
        observeEvent(input[[paste0("usedelim_", tt, "_", ii)]], {
          d <- desc_of(tt); req(length(d))
          s <- ai_suggest_delimiters(d); s <- s[s$score > 0, , drop = FALSE]
          req(nrow(s) >= ii)
          updateTextInput(session, paste0("delim_", tt), value = s$delimiter[ii])
        }, ignoreInit = TRUE)
      }))

      # -- delimiter / granularity changes rebuild this type's shapes, keeping
      #    the bindings of any shape key that survives
      observeEvent(input[[paste0("delim_", tt)]], {
        rs <- rules_rv(); if (is.null(rs[[tt]])) return()
        dl <- input[[paste0("delim_", tt)]]
        if (is.null(dl) || !nzchar(dl)) return()
        if (identical(dl, rs[[tt]]$delimiters)) return()   # no-op guard
        rs[[tt]]$delimiters <- dl
        rs[[tt]] <- tryCatch(ai_rule_refresh_shapes(rs[[tt]], desc_of(tt)),
                             error = function(e) rs[[tt]])
        rules_rv(rs); clear_type_approvals(tt)
        sel_shape[[tt]] <- NA_character_
        bump()
      }, ignoreInit = TRUE)

      observeEvent(input[[paste0("shapeby_", tt)]], {
        rs <- rules_rv(); if (is.null(rs[[tt]])) return()
        sb <- input[[paste0("shapeby_", tt)]]
        if (is.null(sb) || identical(sb, rs[[tt]]$shape_by)) return()
        rs[[tt]]$shape_by <- sb
        rs[[tt]] <- tryCatch(ai_rule_refresh_shapes(rs[[tt]], desc_of(tt)),
                             error = function(e) rs[[tt]])
        rules_rv(rs); clear_type_approvals(tt)
        sel_shape[[tt]] <- NA_character_
        bump()
      }, ignoreInit = TRUE)

      # -- shape table. Re-renders when the rule changes (the Status column
      #    must update), so it MUST restore the current row rather than row 1.
      output[[paste0("shapes_", tt)]] <- DT::renderDataTable({
        ss <- state_of(tt)
        if (is.null(ss) || !nrow(ss$shapes))
          return(DT::datatable(data.frame(message = "No descriptions for this type."),
                               options = list(dom = "t"), rownames = FALSE))
        ks <- ss$shapes$shape_key
        show <- data.frame(
          Elements = ss$shapes$n_tokens,
          Formats  = ss$shapes$classes,
          Strings  = ss$shapes$n_strings,
          Wells    = ss$shapes$n_wells,
          Example  = ss$shapes$example,
          Longest  = ss$shapes$longest,
          Status   = vapply(ks, function(k) {
            if (is.null(ss$rule$shapes[[k]])) return("not set")
            if (!isTRUE(ss$verdicts[[k]]$ok)) return("incomplete")
            if (is_approved(tt, k)) "approved" else "ready"
          }, character(1)),
          stringsAsFactors = FALSE)

        keep <- isolate(sel_shape[[tt]])
        row  <- if (!is.null(keep) && !is.na(keep)) match(keep, ks) else 1L
        if (is.na(row)) row <- 1L

        DT::formatStyle(
          DT::datatable(show, rownames = FALSE,
                        selection = list(mode = "single", selected = row),
                        options = list(dom = "tp", pageLength = 8, scrollX = TRUE,
                                       # keep the page the user was on
                                       displayStart = ((row - 1L) %/% 8L) * 8L)),
          "Status",
          backgroundColor = DT::styleEqual(
            c("not set", "incomplete", "ready", "approved"),
            c("#fff3cd", "#f8d7da", "#e8f0fe", "#e6f4ea")))
      })

      observeEvent(input[[paste0("shapes_", tt, "_rows_selected")]], {
        i  <- input[[paste0("shapes_", tt, "_rows_selected")]]
        ss <- state_of(tt)
        if (!length(i) || is.null(ss) || nrow(ss$shapes) < i) return()
        k <- ss$shapes$shape_key[i]
        if (!identical(k, sel_shape[[tt]])) sel_shape[[tt]] <- k
      })

      # -- token chips for the widest string in the group
      output[[paste0("chips_", tt)]] <- renderUI({
        ss <- state_of(tt); k <- current_shape()
        if (is.null(ss) || is.null(k)) return(tags$em("No group selected."))
        row <- ss$shapes[ss$shapes$shape_key == k, , drop = FALSE]
        if (!nrow(row)) return(NULL)
        # the no-description group: no elements exist, so the only way to fill
        # the required components is a constant per component
        if (identical(k, AI_EMPTY_SHAPE_KEY) || row$n_tokens[1] == 0L)
          return(tags$div(style = "color:#8a6d3b;",
            tags$small(sprintf(
              "%d well(s) of this type carry no description text, so there are no elements to point at. Set the values directly below \u2014 choose \"Same value every well\" for each required component.",
              row$n_wells[1]))))
        toks <- .ai_sr_split(row$longest[1], ss$rule$delimiters)
        cls  <- ai_token_class(toks)
        tagList(
          tags$div(style = "margin-bottom:6px;",
            tags$small(style = "color:#5f6368;", "Elements of "),
            tags$code(row$longest[1])),
          lapply(seq_along(toks), function(i)
            tags$span(class = "ai-chip",
                      tags$b(sprintf("%d", i)), " ", toks[i], " ",
                      tags$i(sprintf("(%s)", cls[i])))))
      })

      # -- binding editor: one row per component, required first
      output[[paste0("binds_", tt)]] <- renderUI({
        ss <- state_of(tt); k <- current_shape()
        if (is.null(ss) || is.null(k)) return(NULL)
        b     <- ss$rule$shapes[[k]] %||% list()
        comps <- ai_type_components(tt)
        req_c <- AI_TYPE_REQUIRED[[tt]]

        lapply(comps, function(cm) {
          cur <- b[[cm]]
          how <- cur$how %||% (if (cm %in% req_c) "constant" else "ignore")
          fluidRow(
            column(3, tags$div(style = "margin-top:28px;",
              tags$span(class = if (cm %in% req_c) "ai-req" else "", cm),
              if (cm %in% req_c) tags$small(" (required)") else NULL)),
            column(3, selectInput(ns(paste0("bh_", tt, "_", cm)), NULL,
                                  choices = AI_HOW_LABELS, selected = how)),
            column(6, uiOutput(ns(paste0("bd_", tt, "_", cm)))))
        })
      })

      # -- verdict + live resolved preview (reads the cached verdict)
      output[[paste0("verdict_", tt)]] <- renderUI({
        ss <- state_of(tt); k <- current_shape()
        if (is.null(ss) || is.null(k)) return(NULL)
        v <- ss$verdicts[[k]]
        if (is.null(v)) return(NULL)
        if (isTRUE(v$ok))
          tags$div(class = "ai-ok",
                   sprintf("All %d string(s) in this group resolve completely.",
                           v$n_strings))
        else
          tags$div(class = "ai-req",
            sprintf("%d of %d string(s) incomplete%s. Examples: %s",
                    v$n_failing, v$n_strings,
                    if (length(v$missing_components))
                      sprintf(" \u2014 missing %s",
                              paste(v$missing_components, collapse = ", ")) else "",
                    paste(v$failing_examples, collapse = " ; ")))
      })

      output[[paste0("preview_", tt)]] <- DT::renderDataTable({
        ss <- state_of(tt); k <- current_shape()
        s <- shape_strings()
        if (is.null(ss) || is.null(k) || !length(s))
          return(DT::datatable(data.frame(message = "Nothing to preview"),
                               options = list(dom = "t"), rownames = FALSE))
        s <- utils::head(s[order(-nchar(s))], 12L)
        out <- do.call(rbind, lapply(s, function(x) {
          r <- tryCatch(ai_resolve_one(x, tt, ss$rule), error = function(e) NULL)
          if (is.null(r))
            return(data.frame(Description = x, PatientID = "", TimePeriod = "",
                              Dilution = "", Source = "", GroupA = "", GroupB = "",
                              OK = "no", stringsAsFactors = FALSE))
          data.frame(
            Description = x,
            PatientID   = r$values[["PatientID"]],
            TimePeriod  = r$values[["TimePeriod"]],
            Dilution    = if (is.na(r$dilution_value)) "" else as.character(r$dilution_value),
            Source      = r$values[["Source"]],
            GroupA      = r$values[["SampleGroupA"]],
            GroupB      = r$values[["SampleGroupB"]],
            OK          = if (is.null(r$issues) || !any(r$issues$severity == "error"))
                            "yes" else "no",
            stringsAsFactors = FALSE)
        }))
        DT::formatStyle(
          DT::datatable(out, rownames = FALSE,
                        options = list(dom = "tp", pageLength = 6, scrollX = TRUE)),
          "OK",
          backgroundColor = DT::styleEqual(c("yes", "no"), c("#e6f4ea", "#f8d7da")))
      })

      # -- commit the editor into the rule
      read_binding <- function(cm) {
        how <- input[[paste0("bh_", tt, "_", cm)]] %||% "ignore"
        if (identical(how, "ignore")) return(NULL)
        val <- input[[paste0("bv_", tt, "_", cm)]]
        switch(how,
          slot = {
            sl <- suppressWarnings(as.integer(val)); sl <- sl[!is.na(sl)]
            if (!length(sl)) NULL else ai_binding("slot", slot = sl)
          },
          pattern = {
            cl <- as.character(val)
            if (!length(cl) || !nzchar(cl[1])) NULL else ai_binding("pattern", class = cl)
          },
          constant  = ai_binding("constant", value = as.character(val %||% "")),
          from_type = ai_binding("from_type"),
          NULL)
      }

      # Commit does NOT touch the selected tab or group: sel_shape is left as
      # it is and the tabset no longer re-renders, so the user stays put and can
      # read the verdict for the group they just applied (fault A).
      commit <- function(keys) {
        rs <- rules_rv(); if (is.null(rs[[tt]])) return()
        for (k in keys) {
          b <- list()
          for (cm in ai_type_components(tt)) {
            bb <- read_binding(cm)
            if (!is.null(bb)) b[[cm]] <- bb
          }
          rs[[tt]]$shapes[[k]] <- b
          set_approved(tt, k, FALSE)
        }
        rules_rv(rs)
        updateCheckboxInput(session, paste0("approve_", tt), value = FALSE)
        bump()
        showNotification(sprintf("Updated %d description group(s) for %s. Reviewing %s.",
                                 length(keys), AI_SPECIMEN_LABELS[[tt]],
                                 if (length(keys) == 1) "this group" else "the first of them"),
                         type = "message", duration = 4)
      }

      observeEvent(input[[paste0("apply_", tt)]], {
        k <- current_shape(); req(!is.null(k)); commit(k)
      })

      observeEvent(input[[paste0("applyall_", tt)]], {
        ss <- state_of(tt); k <- current_shape()
        req(!is.null(ss), !is.null(k))
        n <- ss$shapes$n_tokens[ss$shapes$shape_key == k][1]
        commit(ss$shapes$shape_key[ss$shapes$n_tokens == n])
      })

      # -- approval. Bumps refresh_rv so the whole section recomputes cleanly
      #    from the current rules, which is what the "fully reload" fix needs,
      #    while sel_shape and the selected tab are left untouched (fault B).
      observeEvent(input[[paste0("approve_", tt)]], {
        k <- current_shape(); if (is.null(k)) return()
        want <- isTRUE(input[[paste0("approve_", tt)]])
        if (identical(want, is_approved(tt, k))) return()      # no-op guard
        if (want) {
          ss <- state_of(tt)
          v  <- if (is.null(ss)) NULL else ss$verdicts[[k]]
          if (!isTRUE(v$ok)) {
            showNotification("This group does not resolve completely yet.",
                             type = "warning")
            updateCheckboxInput(session, paste0("approve_", tt), value = FALSE)
            return()
          }
        }
        set_approved(tt, k, want)
        bump()
      }, ignoreInit = TRUE)

      # keep the checkbox in step when the selected group changes
      observeEvent(current_shape(), {
        k <- current_shape(); if (is.null(k)) return()
        updateCheckboxInput(session, paste0("approve_", tt),
                            value = is_approved(tt, k))
      })

      output[[paste0("rollup_", tt)]] <- renderUI({
        ss <- state_of(tt)
        if (is.null(ss) || !nrow(ss$shapes)) return(NULL)
        ks   <- ss$shapes$shape_key
        done <- vapply(ks, function(k) is_approved(tt, k), logical(1))
        ok   <- vapply(ks, function(k) isTRUE(ss$verdicts[[k]]$ok), logical(1))
        tags$p(style = "margin-top:6px;",
          tags$small(sprintf("%d of %d description group(s) approved for %s\u2003\u00b7\u2003%d resolve completely.",
                             sum(done), length(ks), AI_SPECIMEN_LABELS[[tt]], sum(ok))))
      })
    }))

    # ── binding detail controls (static: 4 types x every component) ──────────
    # These read the CACHED state; the previous version called ai_shape_table()
    # once per component, so opening a panel re-scanned the corpus seven times.
    lapply(AI_SPECIMEN_TYPES, function(t)
      lapply(AI_COMPONENTS, function(cm) local({
        tt <- t; cc <- cm
        output[[paste0("bd_", tt, "_", cc)]] <- renderUI({
          how <- input[[paste0("bh_", tt, "_", cc)]] %||% "ignore"
          ss  <- state_of(tt)
          k   <- sel_shape[[tt]]
          cur <- if (!is.null(ss) && !is.null(k) && !is.na(k))
                   ss$rule$shapes[[k]][[cc]] else NULL
          n_tok <- 0L
          if (!is.null(ss) && !is.null(k) && !is.na(k) && nrow(ss$shapes)) {
            m <- match(k, ss$shapes$shape_key)
            if (!is.na(m)) n_tok <- ss$shapes$n_tokens[m]
          }
          if (is.na(n_tok)) n_tok <- 0L
          id <- ns(paste0("bv_", tt, "_", cc))

          switch(how,
            slot = selectInput(id, NULL, multiple = TRUE,
                               choices = if (n_tok > 0) stats::setNames(
                                 seq_len(n_tok),
                                 sprintf("element %d", seq_len(n_tok))) else integer(0),
                               selected = if (!is.null(cur) && identical(cur$how, "slot"))
                                 cur$slot else NULL),
            pattern = selectInput(id, NULL, multiple = TRUE,
                                  choices = setdiff(AI_TOKEN_CLASSES, "empty"),
                                  selected = if (!is.null(cur) && identical(cur$how, "pattern"))
                                    cur$class else
                                    if (cc == "DilutionFactor") c("ratio", "integer") else NULL),
            constant = textInput(id, NULL,
                                 value = if (!is.null(cur) && identical(cur$how, "constant"))
                                   cur$value else "",
                                 placeholder = if (cc == "Source") "e.g. PBS"
                                               else if (cc == "DilutionFactor") "e.g. 1"
                                               else "value used for every well"),
            from_type = tags$div(style = "margin-top:30px;",
                                 tags$small(style = "color:#5f6368;",
                                   "Taken from the number after the type letter, e.g. S3 \u2192 3.")),
            tags$div())
        })
      })))

    # ── profile save / load ──────────────────────────────────────────────────
    output$profile_dl <- downloadHandler(
      filename = function() {
        nm <- gsub("[^A-Za-z0-9._-]+", "_", trimws(input$profile_name %||% ""))
        if (!nzchar(nm)) nm <- "parse_profile"
        paste0(nm, ".yaml")
      },
      content = function(file) {
        rs <- rules_rv()
        if (!length(rs)) {
          writeLines("# No rules configured yet.", file); return(invisible())
        }
        ai_write_profile(file, rs,
                         name = input$profile_name %||% "unnamed",
                         notes = input$profile_notes %||% "",
                         assay = assay() %||% NA_character_)
      })

    observeEvent(input$profile_up, {
      req(input$profile_up)
      d <- inv()
      if (is.null(d) || !nrow(d)) {
        showNotification("Parse instrument file(s) before loading a profile.",
                         type = "warning"); return()
      }
      res <- tryCatch({
        prof <- ai_read_profile(input$profile_up$datapath)
        ap   <- ai_profile_apply(prof$rules, d)
        rules_rv(ap$rules)
        approved_rv(list())
        for (t in names(ap$rules)) sel_shape[[t]] <- NA_character_
        if (nzchar(prof$name %||% ""))
          updateTextInput(session, "profile_name", value = prof$name)
        if (nzchar(prof$notes %||% ""))
          updateTextInput(session, "profile_notes", value = prof$notes)
        report_rv(ap$report)      # also syncs the delimiter / granularity inputs
        bump()
        ap$report
      }, error = function(e) e)

      if (inherits(res, "error")) {
        showNotification(paste("Could not load that profile:", conditionMessage(res)),
                         type = "error", duration = NULL)
        return()
      }
      n_new <- sum(res$status == "new_shape")
      showNotification(
        if (n_new)
          sprintf("Profile loaded. %d description group(s) in this batch are not in the profile \u2014 check them.", n_new)
        else "Profile loaded and it covers every description group in this batch.",
        type = if (n_new) "warning" else "message", duration = 10)
    })

    output$profile_report <- renderUI({
      r <- report_rv(); if (is.null(r) || !nrow(r)) return(NULL)
      lbl <- c(from_profile = "from the profile", new_shape = "new in this batch",
               unused_in_batch = "in the profile but absent here")
      cnt <- table(factor(r$status, levels = names(lbl)))
      tagList(
        tags$p(tags$small(style = "color:#5f6368;",
          paste(sprintf("%d %s", as.integer(cnt), lbl[names(cnt)]), collapse = " \u00b7 "))),
        if (any(r$status == "new_shape"))
          tags$ul(lapply(which(r$status == "new_shape"), function(i)
            tags$li(tags$small(sprintf("%s: %s (%d well(s)) \u2014 needs checking",
                                       r$specimen_type[i], r$example[i],
                                       r$n_wells[i]))))))
    })

    # ── progress, gate, resolution ───────────────────────────────────────────
    # Progress is read off the CACHED verdicts. ai_ruleset_ready() re-resolved
    # every string of every shape of every type on each call, which is what made
    # ticking a checkbox feel like a hang.
    progress <- reactive({
      tp <- types_present()
      if (!length(tp)) return(NULL)
      rows <- lapply(tp, function(t) {
        ss <- state_of(t)
        if (is.null(ss) || !nrow(ss$shapes))
          return(data.frame(specimen_type = t, groups = 0L, resolving = 0L,
                            approved = 0L, stringsAsFactors = FALSE))
        ks <- ss$shapes$shape_key
        data.frame(specimen_type = t, groups = length(ks),
                   resolving = sum(vapply(ks, function(k) isTRUE(ss$verdicts[[k]]$ok), logical(1))),
                   approved  = sum(vapply(ks, function(k) is_approved(t, k), logical(1))),
                   stringsAsFactors = FALSE)
      })
      do.call(rbind, rows)
    })

    ready <- reactive({
      if (!isTRUE(enabled())) return(FALSE)
      p <- progress()
      if (is.null(p) || !nrow(p)) return(FALSE)
      all(p$groups > 0) && all(p$approved == p$groups) &&
        all(p$resolving == p$groups)
    })

    # Full-batch resolution is deferred until the gate opens. Before that it is
    # thousands of ai_resolve_one() calls per reactive tick, for a number the
    # summary can get from the cached verdicts instead.
    resolution <- reactive({
      d <- inv()
      if (is.null(d) || !nrow(d) || !length(rules_rv())) return(NULL)
      if (!isTRUE(ready())) return(NULL)
      refresh_rv()
      tryCatch(ai_resolve_inventory(d, rules_rv()), error = function(e) {
        warning(paste("resolution failed:", conditionMessage(e)))
        NULL
      })
    })

    output$overall <- renderUI({
      d <- inv()
      if (is.null(d) || !nrow(d)) return(tags$em("Nothing loaded yet."))
      p <- progress()
      if (is.null(p)) return(tags$em("No specimen types to configure."))

      # Name a present type that has nothing to configure. The reported fault
      # showed up as an empty tab with no explanation of why the gate was shut.
      cov <- tryCatch(ai_ruleset_coverage(d, rules_rv()), error = function(e) NULL)
      if (!is.null(cov) && nrow(cov))
        return(tagList(
          tags$div(style = "color:#b02a37;font-weight:600;",
                   "A specimen type present on the plates has nothing to configure."),
          tags$ul(lapply(seq_len(nrow(cov)), function(i)
            tags$li(tags$small(cov$message[i])))),
          tags$p(tags$small(style = "color:#5f6368;", sprintf(
            "This normally follows retyping wells on the plate grid. If those wells carry no description text their group is listed as \"%s\" \u2014 select it and set constant values.",
            AI_EMPTY_SHAPE_KEY)))))

      if (isTRUE(ready())) {
        res  <- resolution()
        iss  <- if (is.null(res)) NULL else res$issues
        nerr <- if (is.null(iss)) 0L else sum(iss$severity == "error")
        return(tagList(
          tags$div(style = "color:#2e7d32;font-weight:600;",
                   "Every description group is bound and approved. The layout template can be built."),
          if (nerr > 0)
            tags$p(tags$small(sprintf("%d well(s) still do not resolve: %s", nerr,
                                      iss$message[iss$severity == "error"][1])))))
      }

      tagList(
        tags$div(style = "color:#b02a37;font-weight:600;",
                 sprintf("%d of %d description group(s) approved.",
                         sum(p$approved), sum(p$groups))),
        tags$ul(lapply(seq_len(nrow(p)), function(i)
          tags$li(tags$small(sprintf(
            "%s: %d group(s), %d resolve completely, %d approved",
            AI_SPECIMEN_LABELS[[p$specimen_type[i]]], p$groups[i],
            p$resolving[i], p$approved[i]))))))
    })

    list(ruleset  = reactive(rules_rv()),
         ready    = ready,
         resolved = reactive({ r <- resolution(); if (is.null(r)) NULL else r$resolved }),
         issues   = reactive({ r <- resolution(); if (is.null(r)) NULL else r$issues }))
  })
}
