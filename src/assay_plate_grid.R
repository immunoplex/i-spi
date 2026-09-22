# =============================================================================
# assay_plate_grid.R  --  Stage 1 of the description pre-processor
# -----------------------------------------------------------------------------
# The plate-layout confirmation step, mounted BEFORE any delimiter or element
# control exists. One plate at a time, every well drawn, coloured by specimen
# type, labelled with the leading characters of its description:
#
#   * hover a well              -> full description text
#   * click a well              -> add/remove it from the selection
#   * click a row or column head-> select that whole row/column
#   * edit the selection        -> set specimen type and/or description for
#                                  every selected well at once, optionally
#                                  across every plate in the batch, or across
#                                  every well in the batch sharing the same
#                                  description text
#   * plate switcher            -> dropdown + prev/next, works for 1 or 20+ plates
#
# Bulk editing is the point, not a bonus. The failure mode this step exists to
# fix -- a file that types all 96 wells "X" -- needs 8 blanks and 16 standards
# reassigned per plate. One well at a time is 24 modals per plate. Row/column
# selection plus "apply to all plates" makes it two clicks for a batch.
#
# When a plate has no standards or no blanks, ai_propose_specimen_types() offers
# candidates with a readable reason and a confidence. Proposals are never
# applied silently: they land marked "proposed" (dashed outline) and the step
# will not report ready until a human has confirmed them.
#
# Public surface:
#   ai_plate_grid_ui(id)
#   ai_plate_grid_server(id, inventory_rv, n_wells = reactive(96))
#     inventory_rv : a reactiveVal holding the inventory (read AND written here)
#     returns list(ready = reactive(lgl), issues = reactive(df), selection = reactive(chr))
#
# Depends on assay_well_inventory.R, shiny, DT. No shinyjqui/plotly needed --
# the grid is plain HTML so cells can carry text, a tooltip and a click target
# at once, which a plotly scatter cannot.
# =============================================================================

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

# Specimen palette, matching plot_plate_layout() in batch_layout_functions.R so
# the pre-processor and the existing plate plots agree on colour.
AI_GRID_FILL <- c(X = "#8DB600", S = "#A1CAF1", B = "#F3C300",
                  C = "#2B3D26", E = "#FFFFFF")
AI_GRID_INK  <- c(X = "#1c2b00", S = "#10314d", B = "#3d3100",
                  C = "#FFFFFF", E = "#9aa0a6")
AI_GRID_EDGE <- c(X = "#6d8f00", S = "#7aa9d4", B = "#c9a300",
                  C = "#1b2818", E = "#d4d7db")

# How many leading characters of the description fit in a well, by plate width.
# A 96-well plate (12 columns) has room for a genuinely useful prefix; 384
# (24 columns) does not, and a label too wide for its cell is worse than a short
# one the user hovers, because it clips mid-token without saying so.
AI_GRID_LABEL_DEFAULTS <- list(
  #                chars font_px cell_px
  `12` = c(chars = 8, font = 10, cell = 36),   # <= 12 cols (6/12/24/48/96-well)
  `24` = c(chars = 4, font =  9, cell = 26),   # <= 24 cols (384-well)
  `48` = c(chars = 2, font =  8, cell = 18)    #  > 24 cols (1536-well)
)

#' Label geometry for a column count, before any user override.
ai_grid_label_geometry <- function(cols) {
  if (cols <= 12) AI_GRID_LABEL_DEFAULTS[["12"]]
  else if (cols <= 24) AI_GRID_LABEL_DEFAULTS[["24"]]
  else AI_GRID_LABEL_DEFAULTS[["48"]]
}

# JS string literal escape for the onclick handlers.
.ai_js <- function(x) gsub("'", "\\\\'", gsub("\\\\", "\\\\\\\\", as.character(x)))


# ---- UI ---------------------------------------------------------------------

ai_plate_grid_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # CSS is emitted by substituting @ID@ rather than by sprintf(): the old form
    # needed exactly one ns() argument per "#%s" selector, so adding a rule
    # silently desynchronised the count and sprintf() failed at UI build time.
    tags$style(HTML(gsub("@ID@", ns("grid_wrap"), '
      #@ID@ .ai-plate { display:grid; gap:3px; align-items:stretch;
                        font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; }
      #@ID@ .ai-hdr   { font-size:11px; color:#5f6368; text-align:center;
                        padding:2px 0; cursor:pointer; user-select:none;
                        border-radius:3px; }
      #@ID@ .ai-hdr:hover { background:#eceff1; color:#202124; }
      #@ID@ .ai-well  { position:relative; min-height:34px; border-radius:4px;
                        border:1px solid; display:flex; align-items:center;
                        justify-content:center; cursor:pointer; overflow:visible;
                        line-height:1.1; letter-spacing:-0.3px; padding:0 2px; }
      #@ID@ .ai-well:hover { filter:brightness(1.06); }
      #@ID@ .ai-lbl   { overflow:hidden; white-space:nowrap; text-overflow:clip;
                        width:100%; text-align:center; }
      #@ID@ .ai-sel   { outline:3px solid #1a73e8; outline-offset:-1px; }
      #@ID@ .ai-user  { border-width:2px; border-color:#202124 !important; }
      #@ID@ .ai-prop  { border-style:dashed; border-width:2px; }
      #@ID@ .ai-well[data-desc]:hover::after {
              content: attr(data-desc); position:absolute; bottom:105%; left:50%;
              transform:translateX(-50%); white-space:pre; z-index:60;
              background:#202124; color:#fff; font-size:11px; padding:4px 7px;
              border-radius:4px; pointer-events:none;
              box-shadow:0 2px 8px rgba(0,0,0,.28); max-width:340px; }
      #@ID@ .ai-legend span { display:inline-block; margin-right:14px; font-size:12px; }
      #@ID@ .ai-swatch { display:inline-block; width:11px; height:11px;
                         border:1px solid #9aa0a6; border-radius:2px;
                         margin-right:4px; vertical-align:-1px; }
    ', fixed = TRUE))),

    wellPanel(
      tags$h4("Confirm the plate layout"),
      tags$p(tags$small(
        "Check that each well's specimen type is right before choosing how to ",
        "read the description field. Hover a well for its full description. ",
        "Click wells, or a row/column heading, to select them, then edit below.")),

      fluidRow(
        column(5, uiOutput(ns("plate_switcher"))),
        column(3, selectInput(ns("label_chars"), "Label characters",
                              choices = c("Auto" = "auto", "2" = "2", "3" = "3",
                                          "4" = "4", "5" = "5", "6" = "6",
                                          "8" = "8", "10" = "10", "12" = "12"),
                              selected = "auto")),
        column(4, tags$div(style = "margin-top:25px;",
          actionButton(ns("prev_plate"), "\u2039 Previous", class = "btn-sm"),
          actionButton(ns("next_plate"), "Next \u203a", class = "btn-sm"),
          tags$span(style = "margin-left:10px;", textOutput(ns("plate_counter"), inline = TRUE))))
      ),

      uiOutput(ns("plate_summary")),
      tags$div(id = ns("grid_wrap"), uiOutput(ns("grid"))),
      tags$div(class = "ai-legend", style = "margin-top:10px;", uiOutput(ns("legend")))
    ),

    wellPanel(
      tags$h4("Edit the selected wells"),
      textOutput(ns("selection_label")),
      fluidRow(
        column(4, selectInput(ns("edit_type"), "Specimen type",
                              choices = c("(leave unchanged)" = "",
                                          "X \u2014 test sample"        = "X",
                                          "S \u2014 standard point"     = "S",
                                          "B \u2014 blank"              = "B",
                                          "C \u2014 control"            = "C",
                                          "(clear \u2014 empty well)"   = "__empty__"))),
        column(3, textInput(ns("edit_suffix"), "Type index (optional)",
                            placeholder = "e.g. 3 for S3")),
        column(5, textInput(ns("edit_desc"), "Description (optional)",
                            placeholder = "leave blank to keep existing"))
      ),
      checkboxInput(ns("edit_all_plates"),
                    "Apply to the same wells on every plate in the batch", FALSE),
      checkboxInput(ns("edit_matching"),
                    "Apply to every well in the batch whose description matches the selected well",
                    FALSE),
      actionButton(ns("apply_edit"), "Apply to selection", class = "btn-primary"),
      actionButton(ns("clear_sel"), "Clear selection", class = "btn-sm"),
      actionButton(ns("undo_edit"), "Undo last edit", class = "btn-sm")
    ),

    conditionalPanel(
      condition = sprintf("output['%s']", ns("has_proposals")),
      wellPanel(
        tags$h4("Suggested specimen types"),
        tags$p(tags$small(
          "These wells are typed as samples but look like blanks, standards or ",
          "controls. Select the rows you agree with and apply them. Nothing is ",
          "changed until you do.")),
        DT::dataTableOutput(ns("proposals")),
        tags$div(style = "margin-top:8px;",
          actionButton(ns("apply_proposals"), "Apply selected suggestions",
                       class = "btn-primary"),
          actionButton(ns("apply_all_proposals"), "Apply all high-confidence",
                       class = "btn-sm"))
      )
    ),

    wellPanel(
      tags$h4("Layout check"),
      uiOutput(ns("gate"))
    )
  )
}


# ---- Server -----------------------------------------------------------------

ai_plate_grid_server <- function(id, inventory_rv, n_wells = reactive(96)) {
  force(inventory_rv)
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    sel_rv   <- reactiveVal(character())   # selected wells on the CURRENT plate
    undo_rv  <- reactiveVal(NULL)          # one-deep inventory snapshot

    inv_or_null <- reactive(inventory_rv())

    plates <- reactive({
      inv <- inv_or_null()
      if (is.null(inv) || !nrow(inv)) return(NULL)
      u <- !duplicated(inv$plate_key)
      data.frame(plate_key = inv$plate_key[u], plate_label = inv$plate_label[u],
                 plate_index = inv$plate_index[u], stringsAsFactors = FALSE)
    })

    # ── plate navigation ─────────────────────────────────────────────────────
    output$plate_switcher <- renderUI({
      p <- plates()
      if (is.null(p))
        return(tags$em("Upload and parse instrument file(s) to see the layout."))
      selectInput(ns("plate_sel"), "Plate",
                  choices = stats::setNames(p$plate_key, p$plate_label),
                  selected = isolate(input$plate_sel) %||% p$plate_key[1],
                  width = "100%")
    })

    current_plate <- reactive({
      p <- plates(); req(p)
      pk <- input$plate_sel
      if (is.null(pk) || !(pk %in% p$plate_key)) p$plate_key[1] else pk
    })

    step_plate <- function(delta) {
      p <- plates(); if (is.null(p)) return()
      i <- match(current_plate(), p$plate_key)
      j <- min(max(i + delta, 1L), nrow(p))
      if (j != i) updateSelectInput(session, "plate_sel", selected = p$plate_key[j])
    }
    observeEvent(input$prev_plate, step_plate(-1L))
    observeEvent(input$next_plate, step_plate(+1L))
    observeEvent(input$plate_sel, sel_rv(character()), ignoreInit = TRUE)

    output$plate_counter <- renderText({
      p <- plates(); if (is.null(p)) return("")
      sprintf("Plate %d of %d", match(current_plate(), p$plate_key), nrow(p))
    })

    plate_rows <- reactive({
      inv <- inv_or_null(); req(inv)
      inv[inv$plate_key == current_plate(), , drop = FALSE]
    })

    # ── the grid ─────────────────────────────────────────────────────────────
    output$grid <- renderUI({
      d <- plate_rows()
      if (is.null(d) || !nrow(d)) return(NULL)

      # geometry comes from the inventory, which inferred it from the wells
      # actually present (and from the reader's own report for .rbx). The UI
      # field is only a fallback: drawing a 96-well grid for a 384-well plate
      # would hide 288 wells behind a control the user may never have touched.
      iv    <- inv_or_null()
      nw    <- attr(iv, "n_wells") %||% n_wells() %||% 96
      nrw   <- attr(iv, "plate_rows") %||% ai_plate_dims(nw)[1]
      ncl   <- attr(iv, "plate_cols") %||% ai_plate_dims(nw)[2]
      rows  <- AI_ROW_LETTERS[seq_len(nrw)]
      cols  <- seq_len(ncl)

      # Label width scales with the plate: 8 characters at 12 columns, 4 at 24,
      # 2 beyond. The override handles a batch whose descriptions need more, or
      # are distinguishable in fewer.
      lg      <- ai_grid_label_geometry(ncl)
      n_chars <- as.integer(lg[["chars"]])
      font_px <- as.numeric(lg[["font"]])
      cell_px <- as.numeric(lg[["cell"]])
      ovr <- input$label_chars %||% "auto"
      if (!identical(ovr, "auto")) {
        o <- suppressWarnings(as.integer(ovr))
        if (!is.na(o) && o > 0) {
          n_chars <- o
          # shrink the type when more is asked for than the plate allows, so a
          # wide label still fits instead of clipping
          if (o > as.integer(lg[["chars"]]))
            font_px <- max(7, font_px - (o - as.integer(lg[["chars"]])) * 0.5)
        }
      }

      sel  <- sel_rv()
      look <- stats::setNames(seq_len(nrow(d)), d$well)

      cell <- function(w) {
        i  <- look[[w]]
        if (is.na(i)) return(tags$div(class = "ai-well", style = "background:#fff;"))
        st <- d$specimen_type[i]; key <- if (is.na(st)) "E" else st
        dsc <- d$description[i]
        lbl <- if (is.na(dsc)) "" else substr(dsc, 1L, n_chars)

        cls <- c("ai-well")
        if (w %in% sel) cls <- c(cls, "ai-sel")
        if (identical(d$type_origin[i], "user") || identical(d$desc_origin[i], "user"))
          cls <- c(cls, "ai-user")
        if (identical(d$type_origin[i], "proposed")) cls <- c(cls, "ai-prop")

        tip <- sprintf("%s  \u2022  %s\n%s", w,
                       if (is.na(d$type_code[i])) "empty" else d$type_code[i],
                       if (is.na(dsc)) "(no description)" else dsc)

        tags$div(
          class = paste(cls, collapse = " "),
          style = sprintf("background:%s;color:%s;border-color:%s;font-size:%dpx;min-height:%dpx;",
                          AI_GRID_FILL[[key]], AI_GRID_INK[[key]],
                          AI_GRID_EDGE[[key]], font_px, cell_px),
          `data-desc` = tip,
          title = tip,
          onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority:'event'})",
                            ns("well_click"), .ai_js(w)),
          tags$span(class = "ai-lbl", lbl))
      }

      hdr <- function(text, kind, value)
        tags$div(class = "ai-hdr",
                 onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority:'event'})",
                                   ns(kind), .ai_js(value)),
                 title = sprintf("Select %s %s", if (kind == "row_click") "row" else "column", text),
                 text)

      # header row, then one row per plate row: corner + col heads, then cells
      children <- c(
        list(tags$div()),
        lapply(cols, function(c) hdr(as.character(c), "col_click", as.character(c))),
        unlist(lapply(rows, function(r)
          c(list(hdr(r, "row_click", r)),
            lapply(cols, function(c) cell(paste0(r, c))))),
          recursive = FALSE))

      tags$div(
        class = "ai-plate",
        style = sprintf("grid-template-columns: 26px repeat(%d, minmax(0, 1fr));", ncl),
        children)
    })

    output$legend <- renderUI({
      d <- plate_rows(); req(d)
      cnt <- table(factor(ifelse(is.na(d$specimen_type), "E", d$specimen_type),
                          levels = c(AI_SPECIMEN_TYPES, "E")))
      tagList(
        lapply(names(cnt), function(k)
          tags$span(
            tags$span(class = "ai-swatch",
                      style = sprintf("background:%s;", AI_GRID_FILL[[k]])),
            sprintf("%s (%d)", AI_SPECIMEN_LABELS[[k]], cnt[[k]]))),
        tags$span(style = "color:#5f6368;",
                  "solid dark border = edited \u00b7 dashed = suggested, unconfirmed"))
    })

    output$plate_summary <- renderUI({
      d <- plate_rows(); req(d)
      iv  <- inv_or_null()
      occ <- sum(!is.na(d$type_code))
      nd  <- sum(!is.na(d$description))
      tagList(
        tags$p(style = "color:#5f6368;font-size:12px;margin:6px 0;",
               sprintf("%d occupied well(s), %d with a description, from %s \u00b7 %d-well plate (%d\u00d7%d)",
                       occ, nd, d$source_file[1] %||% "unknown file",
                       attr(iv, "n_wells") %||% 96L,
                       attr(iv, "plate_rows") %||% 8L,
                       attr(iv, "plate_cols") %||% 12L)),
        if (isTRUE(attr(iv, "plate_upgraded")))
          tags$p(style = "color:#8a6d3b;font-size:12px;margin:6px 0;",
                 sprintf("The file holds wells beyond a %d-well plate, so it is being read as %d wells.",
                         attr(iv, "plate_declared") %||% 96L,
                         attr(iv, "n_wells") %||% 96L)))
    })

    # ── selection ────────────────────────────────────────────────────────────
    observeEvent(input$well_click, {
      w <- input$well_click; req(w)
      s <- sel_rv()
      sel_rv(if (w %in% s) setdiff(s, w) else c(s, w))
    })

    observeEvent(input$row_click, {
      r <- input$row_click; req(r)
      d <- plate_rows(); req(d)
      w <- d$well[!is.na(d$row_letter) & d$row_letter == r]
      s <- sel_rv()
      sel_rv(if (all(w %in% s)) setdiff(s, w) else union(s, w))
    })

    observeEvent(input$col_click, {
      c0 <- suppressWarnings(as.integer(input$col_click)); req(!is.na(c0))
      d <- plate_rows(); req(d)
      w <- d$well[!is.na(d$col_number) & d$col_number == c0]
      s <- sel_rv()
      sel_rv(if (all(w %in% s)) setdiff(s, w) else union(s, w))
    })

    observeEvent(input$clear_sel, sel_rv(character()))

    output$selection_label <- renderText({
      s <- sel_rv()
      if (!length(s)) return("No wells selected. Click wells, or a row or column heading.")
      d <- plate_rows()
      shown <- paste(utils::head(s, 12), collapse = ", ")
      if (length(s) > 12) shown <- paste0(shown, sprintf(" ... (+%d more)", length(s) - 12))
      sprintf("%d well(s) selected on plate %s: %s", length(s), current_plate(), shown)
    })

    # ── apply an edit ────────────────────────────────────────────────────────
    observeEvent(input$apply_edit, {
      inv <- inv_or_null(); req(inv)
      s <- sel_rv()
      if (!length(s)) {
        showNotification("Select at least one well first.", type = "warning")
        return()
      }
      tsel <- input$edit_type %||% ""
      dsc  <- input$edit_desc %||% ""
      if (!nzchar(tsel) && !nzchar(trimws(dsc))) {
        showNotification("Choose a specimen type or enter a description.", type = "warning")
        return()
      }

      undo_rv(inv)

      new_type <- if (!nzchar(tsel)) NULL
                  else if (identical(tsel, "__empty__")) ""
                  else paste0(tsel, trimws(input$edit_suffix %||% ""))
      new_desc <- if (nzchar(trimws(dsc))) dsc else NULL

      pk <- if (isTRUE(input$edit_all_plates)) NULL else current_plate()
      inv <- ai_inventory_set(inv, pk, s, type_code = new_type, description = new_desc)

      # "same description anywhere in the batch" -- the fix for a file where
      # every blank says only "blank": retype all of them in one action.
      if (isTRUE(input$edit_matching)) {
        cur <- plate_rows()
        descs <- unique(stats::na.omit(cur$description[cur$well %in% s]))
        for (dd in descs) {
          m <- ai_inventory_matching(inv, dd)
          if (nrow(m))
            for (p in unique(m$plate_key))
              inv <- ai_inventory_set(inv, p, m$well[m$plate_key == p],
                                      type_code = new_type, description = new_desc)
        }
      }

      inventory_rv(inv)
      n_scope <- if (isTRUE(input$edit_all_plates)) "every plate" else
                 sprintf("plate %s", current_plate())
      showNotification(sprintf("Updated %d well(s) on %s.", length(s), n_scope),
                       type = "message")
      updateTextInput(session, "edit_desc", value = "")
      updateTextInput(session, "edit_suffix", value = "")
    })

    observeEvent(input$undo_edit, {
      prev <- undo_rv()
      if (is.null(prev)) {
        showNotification("Nothing to undo.", type = "warning"); return()
      }
      inventory_rv(prev); undo_rv(NULL)
      showNotification("Last edit undone.", type = "message")
    })

    # ── proposals ────────────────────────────────────────────────────────────
    proposals <- reactive({
      inv <- inv_or_null()
      if (is.null(inv) || !nrow(inv)) return(NULL)
      p <- ai_propose_specimen_types(inv)
      if (!nrow(p)) return(NULL)
      p
    })

    output$has_proposals <- reactive(!is.null(proposals()))
    outputOptions(output, "has_proposals", suspendWhenHidden = FALSE)

    output$proposals <- DT::renderDataTable({
      p <- proposals(); req(p)
      DT::datatable(
        p[, c("plate_key", "well", "current_type", "proposed_type",
              "confidence", "reason")],
        rownames = FALSE, selection = "multiple",
        colnames = c("Plate", "Well", "Now", "Suggested", "Confidence", "Why"),
        options = list(dom = "tp", pageLength = 10, scrollX = TRUE))
    })

    apply_props <- function(rows) {
      inv <- inv_or_null(); p <- proposals()
      if (is.null(inv) || is.null(p) || !length(rows)) {
        showNotification("No suggestions selected.", type = "warning"); return()
      }
      undo_rv(inv)
      inventory_rv(ai_apply_proposals(inv, p, rows))
      showNotification(sprintf("Applied %d suggestion(s). Confirm them in the grid.",
                               length(rows)), type = "message")
    }
    observeEvent(input$apply_proposals,
                 apply_props(input$proposals_rows_selected))
    observeEvent(input$apply_all_proposals, {
      p <- proposals(); req(p)
      apply_props(which(p$confidence == "high"))
    })

    # ── gate ─────────────────────────────────────────────────────────────────
    issues <- reactive({
      inv <- inv_or_null()
      if (is.null(inv)) return(NULL)
      ai_inventory_requirements(inv)
    })

    unconfirmed <- reactive({
      inv <- inv_or_null()
      if (is.null(inv)) return(0L)
      sum(!is.na(inv$type_origin) & inv$type_origin == "proposed")
    })

    ready <- reactive({
      iss <- issues()
      !is.null(iss) && !any(iss$severity == "error") && unconfirmed() == 0L
    })

    output$gate <- renderUI({
      inv <- inv_or_null()
      if (is.null(inv))
        return(tags$em("Nothing loaded yet."))
      iss <- issues()
      nerr <- sum(iss$severity == "error"); nwarn <- sum(iss$severity == "warning")
      unc <- unconfirmed()

      msgs <- if (nrow(iss)) lapply(seq_len(nrow(iss)), function(i)
        tags$li(style = sprintf("color:%s;",
                  if (iss$severity[i] == "error") "#b02a37" else "#8a6d3b"),
                iss$message[i])) else NULL

      tagList(
        if (isTRUE(ready()))
          tags$div(style = "color:#2e7d32;font-weight:600;",
                   "Every plate has samples, standards and blanks. Continue to the description step.")
        else
          tags$div(style = "color:#b02a37;font-weight:600;",
                   sprintf("%d problem(s) to resolve before continuing.", nerr + unc)),
        if (unc > 0)
          tagList(
            tags$p(sprintf("%d well(s) carry a suggested type that nobody has confirmed.", unc)),
            actionButton(ns("confirm_props"), "I have checked these \u2014 confirm them",
                         class = "btn-sm btn-primary")),
        if (!is.null(msgs)) tags$ul(msgs),
        if (nwarn > 0 && nerr == 0)
          tags$p(tags$small(style = "color:#8a6d3b;",
                            "Warnings do not block the next step.")))
    })

    observeEvent(input$confirm_props, {
      inv <- inv_or_null(); req(inv)
      inventory_rv(ai_confirm_proposals(inv))
      showNotification("Suggested types confirmed.", type = "message")
    })

    list(ready = ready, issues = issues, selection = reactive(sel_rv()))
  })
}
