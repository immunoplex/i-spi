# =============================================================================
# source_alias.R  --  global editor for madi_results.source_alias
# -----------------------------------------------------------------------------
# Ungated, global sidebar tab: normalise raw standard-curve `source` labels to a
# canonical name (used by the Compare-fits tab and anywhere else that shows a
# source). GLOBAL -- mappings apply to every study. Reads/writes ONLY
# madi_results.source_alias; the review list reads curve_lookup_unmasked.
#
# Follows the delete_study_components pattern: sourced local=TRUE into the app
# server as top-level output$/observers (plain sa_* ids), placed by ui_handler.R
# via uiOutput("source_alias_ui"). No explicit server mount needed. Uses db_pool.
# =============================================================================

local({
  or <- function(a, b) if (is.null(a) || length(a) == 0) b else a

  sa_reload <- reactiveVal(0L)                       # bump to re-read the table

  sa_aliases <- reactive({
    sa_reload()
    tryCatch(DBI::dbGetQuery(db_pool,
      "SELECT raw_source, canonical_source, COALESCE(note, '') AS note
         FROM madi_results.source_alias
        ORDER BY canonical_source, raw_source"),
      error = function(e) data.frame(raw_source = character(), canonical_source = character(),
                                     note = character(), stringsAsFactors = FALSE))
  })
  sa_staged <- reactiveVal(NULL)                     # in-progress edits
  observeEvent(sa_aliases(), { sa_staged(sa_aliases()) }, ignoreNULL = FALSE)

  sa_unmapped <- reactive({
    sa_reload()
    tryCatch(DBI::dbGetQuery(db_pool,
      "SELECT cl.source AS raw_source, count(*) AS n_curves
         FROM madi_results.curve_lookup_unmasked cl
         LEFT JOIN madi_results.source_alias a ON a.raw_source = cl.source
        WHERE a.raw_source IS NULL AND cl.source IS NOT NULL AND cl.source <> ''
        GROUP BY cl.source
        ORDER BY n_curves DESC, cl.source"),
      error = function(e) data.frame(raw_source = character(), n_curves = integer(),
                                     stringsAsFactors = FALSE))
  })

  # ---- UI (filled into ui_handler's uiOutput("source_alias_ui")) ------------
  output$source_alias_ui <- renderUI({
    tagList(
      h3("Standard-curve source names"),
      div(class = "alert alert-info",
          tags$strong("Global."),
          " These mappings normalise raw standard-source labels to a canonical name",
          " across every study (e.g. NIBSC / NIBSC06 / NIBSC06_140 \u2192 NIBSC06140).",
          " Unmapped sources display as-is."),
      fluidRow(
        column(7,
          h4("Current mappings"),
          helpText("Edit Canonical / Note in place, then Save. Select row(s) to delete."),
          DT::dataTableOutput("sa_alias_table"),
          div(style = "margin-top:8px;",
              actionButton("sa_save", "Save changes", class = "btn-primary"),
              actionButton("sa_delete", "Delete selected", class = "btn-danger"))
        ),
        column(5,
          h4("Unmapped raw sources"),
          helpText("Raw values in curve_lookup with no mapping yet (most-used first)."),
          DT::dataTableOutput("sa_unmapped_table"),
          tags$hr(),
          h4("Add mapping"),
          uiOutput("sa_add_raw_ui"),
          textInput("sa_add_canon", "Canonical name"),
          textInput("sa_add_note", "Note (optional)"),
          actionButton("sa_add", "Add mapping", class = "btn-success")
        )
      )
    )
  })

  # ---- current mappings: editable table -------------------------------------
  output$sa_alias_table <- DT::renderDataTable({
    df <- or(sa_staged(), sa_aliases())
    DT::datatable(df, rownames = FALSE, selection = "multiple",
      colnames = c("Raw source", "Canonical", "Note"),
      editable = list(target = "cell", disable = list(columns = 0)),  # raw_source read-only
      options = list(dom = "tp", pageLength = 15, scrollX = TRUE))
  })

  observeEvent(input$sa_alias_table_cell_edit, {
    info <- input$sa_alias_table_cell_edit
    df <- sa_staged(); if (is.null(df) || !nrow(df)) return()
    col <- c("raw_source", "canonical_source", "note")[info$col + 1L]
    df[info$row, col] <- info$value
    sa_staged(df)
  })

  observeEvent(input$sa_save, {
    df <- sa_staged(); if (is.null(df) || !nrow(df)) return()
    res <- tryCatch({
      for (i in seq_len(nrow(df))) {
        canon <- trimws(as.character(df$canonical_source[i]))
        if (is.na(canon) || !nzchar(canon)) next          # skip blank canonicals
        note  <- as.character(df$note[i])
        note  <- if (is.na(note) || !nzchar(trimws(note))) NA else trimws(note)
        DBI::dbExecute(db_pool,
          "INSERT INTO madi_results.source_alias (raw_source, canonical_source, note)
           VALUES ($1, $2, $3)
           ON CONFLICT (raw_source) DO UPDATE
             SET canonical_source = EXCLUDED.canonical_source,
                 note = EXCLUDED.note, updated_at = now()",
          params = list(df$raw_source[i], canon, note))
      }
      TRUE
    }, error = function(e) {
      showNotification(paste("Save failed:", conditionMessage(e)), type = "error", duration = NULL)
      FALSE
    })
    if (isTRUE(res)) { showNotification("Mappings saved.", type = "message"); sa_reload(sa_reload() + 1L) }
  })

  observeEvent(input$sa_delete, {
    sel <- input$sa_alias_table_rows_selected
    df  <- sa_staged()
    if (is.null(df) || !length(sel)) {
      showNotification("Select row(s) to delete.", type = "warning"); return()
    }
    raws <- df$raw_source[sel]
    res <- tryCatch({
      for (r in raws)
        DBI::dbExecute(db_pool, "DELETE FROM madi_results.source_alias WHERE raw_source = $1",
                       params = list(r))
      TRUE
    }, error = function(e) {
      showNotification(paste("Delete failed:", conditionMessage(e)), type = "error", duration = NULL)
      FALSE
    })
    if (isTRUE(res)) {
      showNotification(sprintf("Deleted %d mapping(s).", length(raws)), type = "message")
      sa_reload(sa_reload() + 1L)
    }
  })

  # ---- unmapped sources + add form ------------------------------------------
  output$sa_unmapped_table <- DT::renderDataTable(
    DT::datatable(sa_unmapped(), rownames = FALSE, selection = "single",
                  colnames = c("Raw source", "# curves"),
                  options = list(dom = "tp", pageLength = 10)))

  output$sa_add_raw_ui <- renderUI({
    um  <- sa_unmapped()
    pick <- input$sa_unmapped_table_rows_selected
    sel <- if (!is.null(pick) && length(pick)) um$raw_source[pick] else NULL
    selectInput("sa_add_raw", "Raw source", choices = um$raw_source, selected = sel)
  })

  observeEvent(input$sa_add, {
    raw   <- input$sa_add_raw
    canon <- trimws(or(input$sa_add_canon, ""))
    if (is.null(raw) || !nzchar(raw))  { showNotification("Pick a raw source.", type = "warning"); return() }
    if (!nzchar(canon))                { showNotification("Enter a canonical name.", type = "warning"); return() }
    note <- or(input$sa_add_note, ""); note <- if (nzchar(trimws(note))) trimws(note) else NA
    res <- tryCatch({
      DBI::dbExecute(db_pool,
        "INSERT INTO madi_results.source_alias (raw_source, canonical_source, note)
         VALUES ($1, $2, $3)
         ON CONFLICT (raw_source) DO UPDATE
           SET canonical_source = EXCLUDED.canonical_source,
               note = EXCLUDED.note, updated_at = now()",
        params = list(raw, canon, note))
      TRUE
    }, error = function(e) {
      showNotification(paste("Add failed:", conditionMessage(e)), type = "error", duration = NULL)
      FALSE
    })
    if (isTRUE(res)) {
      showNotification(sprintf("Mapped '%s' \u2192 '%s'.", raw, canon), type = "message")
      updateTextInput(session, "sa_add_canon", value = "")
      updateTextInput(session, "sa_add_note",  value = "")
      sa_reload(sa_reload() + 1L)
    }
  })
})
