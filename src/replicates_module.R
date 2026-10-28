# =============================================================================
# replicates_module.R  --  Data > Raw inputs > Replicates
# -----------------------------------------------------------------------------
# Finds wells that were run as duplicates / triplicates / ... and writes their
# mean (or median) into a NEW experiment under the same study, so downstream QC
# and curve fitting can work on one value per sample instead of one per well.
#
# A replicate group is the set of unmasked rows that share
#     plate (plateid + plate + nominal dilution) . stype . sample ID .
#     patient ID . timeperiod . dilution . wavelength . antigen . feature . source
# (only the columns a given table actually has -- e.g. xmap_control carries no
# timeperiod/patientid). The plate is part of the key ON PURPOSE: replicates are
# wells on the SAME plate. The same sample on two plates is a plate-to-plate
# comparison, not a replicate, and is left alone.
#
# Masked wells are excluded from the aggregate and the report counts them.
#
# ELISA: aggregate AFTER the 450-620 wavelength subtraction. Subtraction writes a
# "<exp>|D" experiment (wavelength "delta"); run this tool on THAT experiment.
# The tab warns when it sees an ELISA experiment that still carries two
# wavelengths and has no |D counterpart.
#
# Layout
#   pure functions (rep_*)      grouping, report, aggregation, naming  -- no DB
#   rep_build_new_experiment()  assembles the new experiment's frames  -- no DB
#   rep_write_experiment()      one transaction: insert + register curves
#   replicatesUI/Server         the tab
#
# Aggregated rows keep the identity columns (project, study, plate, stype, ID,
# patient, timeperiod, dilution, ...), take the LOWEST well label of the group,
# and carry the aggregated response; per-well QC columns (aggregated-bead %,
# sampling errors, gates, predictions, masks) are left empty -- the compute
# pipeline recalculates them for the new experiment. The count column
# (antibody_n) is the SUM of the replicates' counts.
# =============================================================================

REP_TYPES <- c(S = "xmap_standard", C = "xmap_control", B = "xmap_buffer", X = "xmap_sample")
REP_LABELS <- c(S = "Standards", C = "Controls", B = "Blanks", X = "Test samples")
REP_MAX_NAME <- 15L
REP_SUFFIX   <- "ag"

# Columns that identify a replicate group (intersected with what the table has).
REP_KEY_COLS <- c("plateid", "plate", "nominal_sample_dilution", "stype", "sampleid",
                  "patientid", "timeperiod", "dilution", "wavelength", "antigen",
                  "feature", "source")
# Identity columns an aggregated row keeps besides the key.
REP_CARRY_COLS <- c("study_accession", "experiment_accession", "plate_id", "well",
                    "id_imi", "agroup", "antibody_name", "project_id",
                    "sample_dilution_factor")

.rep_or <- function(a, b) if (is.null(a) || !length(a)) b else a

rep_resp_col <- function(df) intersect(c("assay_response", "antibody_mfi"), names(df))[1]
rep_key_cols <- function(df) intersect(REP_KEY_COLS, names(df))

# Unmasked rows, ordered by well (A1, A2 ... B1 ...) so "first well" is stable.
rep_active <- function(df) {
  if (is.null(df) || !nrow(df)) return(df)
  if ("masked" %in% names(df)) df <- df[!(df$masked %in% TRUE), , drop = FALSE]
  if (nrow(df) && "well" %in% names(df)) {
    w <- as.character(df$well)
    df <- df[order(substr(w, 1, 1), suppressWarnings(as.integer(sub("^[A-Za-z]+", "", w)))), ,
             drop = FALSE]
  }
  df
}

# Integer group id per row (1..G in order of first appearance); NA keys group.
rep_group_id <- function(df, keys) {
  if (!nrow(df)) return(integer(0))
  if (!length(keys)) return(rep(1L, nrow(df)))
  parts <- lapply(keys, function(k) {
    x <- as.character(df[[k]]); x[is.na(x)] <- "\001NA"; x })
  s <- do.call(paste, c(parts, sep = "\r"))
  match(s, unique(s))
}

# ---- report -----------------------------------------------------------------
# Per-group detail for ONE table: only groups with more than one unmasked row.
rep_detail <- function(df, type) {
  empty <- data.frame()
  if (is.null(df) || !nrow(df)) return(empty)
  act <- rep_active(df); if (!nrow(act)) return(empty)
  keys <- rep_key_cols(act); rc <- rep_resp_col(act)
  gid  <- rep_group_id(act, keys); n <- tabulate(gid)
  multi <- which(n > 1L); if (!length(multi)) return(empty)
  first <- match(multi, gid)
  out <- data.frame(Type = REP_LABELS[[type]], stringsAsFactors = FALSE)[rep(1, length(multi)), , drop = FALSE]
  for (k in keys) out[[k]] <- act[[k]][first]
  out$Replicates <- n[multi]
  out$Wells <- vapply(multi, function(g) paste(act$well[gid == g], collapse = ", "), "")
  if (!is.na(rc)) {
    v <- as.numeric(act[[rc]])
    st <- function(f) vapply(multi, function(g) {
      x <- v[gid == g]; x <- x[!is.na(x)]; if (length(x)) f(x) else NA_real_ }, 0)
    out$Mean   <- round(st(mean), 4)
    out$Median <- round(st(stats::median), 4)
    out$CV_pct <- round(vapply(multi, function(g) {
      x <- v[gid == g]; x <- x[!is.na(x)]
      if (length(x) > 1 && mean(x) != 0) 100 * stats::sd(x) / abs(mean(x)) else NA_real_ }, 0), 1)
  }
  rownames(out) <- NULL
  out
}

# One summary row per sample type: how many rows, groups, replicate groups.
rep_summary <- function(frames) {
  do.call(rbind, lapply(names(REP_TYPES), function(t) {
    df <- frames[[t]]
    total <- if (is.null(df)) 0L else nrow(df)
    act <- rep_active(df); n_act <- if (is.null(act)) 0L else nrow(act)
    if (n_act) { n <- tabulate(rep_group_id(act, rep_key_cols(act))) } else n <- integer(0)
    data.frame(type = t, label = REP_LABELS[[t]], rows = total,
               masked = total - n_act, groups = length(n),
               rep_groups = sum(n > 1L), rep_rows = sum(n[n > 1L]),
               duplicates = sum(n == 2L), triplicates = sum(n == 3L), more = sum(n > 3L),
               rows_after = length(n), stringsAsFactors = FALSE)
  }))
}

# ---- aggregation ------------------------------------------------------------
# All unmasked rows of one table collapsed to one row per group (singletons pass
# through with their own value).
rep_aggregate <- function(df, fun = c("mean", "median")) {
  fun <- match.arg(fun); f <- if (fun == "mean") mean else stats::median
  act <- rep_active(df)
  if (is.null(act) || !nrow(act)) return(act)
  keys <- rep_key_cols(act); rc <- rep_resp_col(act)
  gid <- rep_group_id(act, keys)
  out <- act[!duplicated(gid), , drop = FALSE]            # lowest well of each group
  keep <- unique(c(keys, REP_CARRY_COLS, rc, "antibody_n"))
  for (cn in setdiff(names(out), keep)) out[[cn]][] <- NA   # per-well QC / computed
  agg <- function(x, g, fn) {
    vapply(split(x, factor(g, levels = seq_len(max(g)))), function(v) {
      v <- v[!is.na(v)]; if (length(v)) fn(v) else NA_real_ }, 0)
  }
  if (!is.na(rc)) out[[rc]] <- agg(as.numeric(act[[rc]]), gid, f)
  if ("antibody_n" %in% names(out)) {
    nn <- agg(as.numeric(act$antibody_n), gid, sum)
    out$antibody_n <- suppressWarnings(as.integer(nn))
  }
  if ("masked" %in% names(out))      out$masked <- FALSE
  if ("mask_reason" %in% names(out)) out$mask_reason <- NA_character_
  out <- out[, !grepl("^xmap_.*_id$", names(out)), drop = FALSE]   # serial PKs regenerate
  rownames(out) <- NULL
  out
}

# ---- naming -----------------------------------------------------------------
# Default: the source name + "ag", inside the 15-character limit. The source's
# own characters are trimmed (never the suffix); "|" (the ELISA "|D" marker)
# is not allowed in a name, so "EXP1|D" becomes "EXP1Dag".
rep_default_name <- function(source, suffix = REP_SUFFIX, max = REP_MAX_NAME) {
  base <- gsub("[^A-Za-z0-9_.-]", "", as.character(.rep_or(source, "")))
  paste0(substr(base, 1, max - nchar(suffix)), suffix)
}

# NULL when acceptable, otherwise the reason it is not.
rep_validate_name <- function(name, source, existing = character(0)) {
  name <- trimws(as.character(.rep_or(name, "")))
  if (!nzchar(name))                       return("Enter a name for the new experiment.")
  if (nchar(name) > REP_MAX_NAME)          return(sprintf("Use at most %d characters (now %d).", REP_MAX_NAME, nchar(name)))
  if (!grepl("^[A-Za-z0-9_.-]+$", name))   return("Use letters, digits, '_', '-' or '.' only.")
  if (identical(name, source))             return("The new name must differ from the source experiment.")
  if (name %in% existing)                  return(sprintf("An experiment named '%s' already exists in this study.", name))
  NULL
}

# ---- assemble the new experiment (no DB) ------------------------------------
# frames: list(S=, C=, B=, X=, header=) of the SOURCE experiment.
# types : the sample types to aggregate. Others are copied unchanged when
#         carry_others (so the new experiment is complete), else left out.
rep_build_new_experiment <- function(frames, new_exp, types, fun = "mean", carry_others = TRUE) {
  tabs <- list(); stats <- list()
  for (t in names(REP_TYPES)) {
    df <- frames[[t]]
    if (is.null(df) || !nrow(df)) next
    if (t %in% types)        out <- rep_aggregate(df, fun)
    else if (carry_others) { out <- df; out <- out[, !grepl("^xmap_.*_id$", names(out)), drop = FALSE] }
    else next
    if (!is.null(out) && nrow(out)) {
      out$experiment_accession <- new_exp
      tabs[[REP_TYPES[[t]]]] <- out
      stats[[t]] <- list(before = nrow(df), after = nrow(out), aggregated = t %in% types)
    }
  }
  hdr <- frames$header
  if (!is.null(hdr) && nrow(hdr)) {
    hdr <- hdr[, !grepl("^xmap_.*_id$", names(hdr)), drop = FALSE]
    hdr$experiment_accession <- new_exp
  }
  list(header = hdr, tables = tabs, stats = stats)
}

# ---- write (DB; one transaction) --------------------------------------------
rep_write_experiment <- function(pool, built, project, study, source_exp, new_exp,
                                 schema = "madi_results") {
  pool::poolWithTransaction(pool, function(conn) {
    ins <- function(tbl, df) {
      if (is.null(df) || !nrow(df)) return(0L)
      DBI::dbAppendTable(conn, DBI::Id(schema = schema, table = tbl), df)
      nrow(df)
    }
    n <- list(xmap_header = ins("xmap_header", built$header))
    for (tb in names(built$tables)) n[[tb]] <- ins(tb, built$tables[[tb]])

    # settings that hang off the experiment name (skipped when the table is gone)
    if (DBI::dbExistsTable(conn, DBI::Id(schema = schema, table = "xmap_antigen_family"))) {
      af <- DBI::dbGetQuery(conn, sprintf(
        "SELECT * FROM %s.xmap_antigen_family WHERE study_accession = %s AND experiment_accession = %s AND project_id = %s",
        schema, DBI::dbQuoteString(conn, study), DBI::dbQuoteString(conn, source_exp),
        DBI::dbQuoteLiteral(conn, project)))
      if (nrow(af)) {
        af <- af[, names(af) != "xmap_antigen_family_id", drop = FALSE]
        af$experiment_accession <- new_exp
        n$xmap_antigen_family <- ins("xmap_antigen_family", af)
      }
    }
    if (exists("register_derived_curves", mode = "function"))
      register_derived_curves(conn, project = project, study = study, experiment = new_exp)
    n
  })
}

# ---- UI ---------------------------------------------------------------------
replicatesUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(class = "help-block", style = "margin:8px 0;",
      shiny::strong("Replicate wells (duplicates, triplicates, …) and the experiment made from their mean or median."),
      shiny::br(),
      shiny::tags$small("Replicates = unmasked rows on the same plate with the same sample ID, patient, time period, ",
        "dilution, wavelength, antigen, feature and source. Masked wells are left out.")),
    shiny::uiOutput(ns("elisa_note")),
    shiny::h4("1. Which wells have replicates"),
    shiny::uiOutput(ns("summary")),
    shiny::h4("2. Create an aggregated experiment"),
    shiny::fluidRow(
      shiny::column(4,
        shiny::checkboxGroupInput(ns("types"), "Sample types to aggregate",
          choiceNames = unname(REP_LABELS), choiceValues = names(REP_LABELS), selected = "X")),
      shiny::column(3,
        shiny::radioButtons(ns("fun"), "Aggregate by",
          choices = c("Mean" = "mean", "Median" = "median"), selected = "mean"),
        shiny::checkboxInput(ns("carry"), "Copy the other sample types unchanged", TRUE),
        shiny::tags$small(class = "text-muted",
          "Keeps the new experiment complete (e.g. standards for the curves) when you aggregate only some types.")),
      shiny::column(5,
        shiny::textInput(ns("new_name"), sprintf("New experiment name (max %d characters)", REP_MAX_NAME)),
        shiny::tags$script(shiny::HTML(sprintf(
          "$(function(){ $('#%s').attr('maxlength', %d); });", ns("new_name"), REP_MAX_NAME))),
        shiny::uiOutput(ns("name_msg")),
        shiny::actionButton(ns("create"), "Create aggregated experiment",
                            icon = shiny::icon("layer-group"), class = "btn-primary"))),
    shiny::h4("3. Replicate groups in this experiment"),
    shinycssloaders::withSpinner(DT::dataTableOutput(ns("detail")), type = 4, color = "#337ab7"),
    shiny::div(style = "margin-top:6px;",
      shiny::downloadButton(ns("dl_detail"), "Download report (CSV)", class = "btn-xs"))
  )
}

# ---- server -----------------------------------------------------------------
#' @param scope         reactive -> list(study, experiment, project_id)
#' @param get_table     function(tbl) -> the Data tab's frame for the selected experiment
#' @param refresh_experiments optional reactiveVal bumped so the experiment list reloads
replicatesServer <- function(id, scope, get_table, pool, reload_trigger,
                             refresh_experiments = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    frames <- shiny::reactive({
      s <- scope(); shiny::req(s$study, s$experiment); reload_trigger()
      list(S = get_table("xmap_standard"), C = get_table("xmap_control"),
           B = get_table("xmap_buffer"),   X = get_table("xmap_sample"),
           header = get_table("xmap_header"))
    })

    existing <- shiny::reactive({
      s <- scope(); shiny::req(s$study); reload_trigger()
      if (!is.null(refresh_experiments)) refresh_experiments()
      d <- .calib_q(pool, sprintf(
        "SELECT DISTINCT experiment_accession FROM %s WHERE project_id = $1 AND study_accession = $2",
        .tbl("xmap_header")), params = list(s$project_id, s$study))
      as.character(d$experiment_accession)
    })

    summ <- shiny::reactive(rep_summary(frames()))

    # default name follows the selected experiment
    shiny::observeEvent(scope()$experiment, {
      shiny::updateTextInput(session, "new_name", value = rep_default_name(scope()$experiment))
    }, ignoreNULL = TRUE)

    name_problem <- shiny::reactive(
      rep_validate_name(input$new_name, scope()$experiment, existing()))
    output$name_msg <- shiny::renderUI({
      p <- name_problem()
      if (!is.null(p)) shiny::div(class = "text-danger", style = "margin-bottom:6px;", p)
    })
    shiny::observe({
      ok <- is.null(name_problem()) && length(input$types) > 0
      shinyjs::toggleState("create", condition = ok)
    })

    # ELISA: this tool is meant to run after wavelength subtraction
    output$elisa_note <- shiny::renderUI({
      h <- frames()$header; s <- scope()
      if (is.null(h) || !nrow(h) || !"wavelengths" %in% names(h)) return(NULL)
      two <- any(grepl("\\|", .rep_or(h$wavelengths, "")), na.rm = TRUE)
      if (two)
        shiny::div(class = "alert alert-warning", style = "margin:8px 0;",
          shiny::strong("This experiment still has two wavelengths. "),
          "Subtract them first (the Subtract Wavelengths tab), then select the new ",
          "subtracted experiment and run the replicate aggregation there.")
      else if (all(grepl("^delta", stats::na.omit(h$wavelengths))) && any(!is.na(h$wavelengths)))
        shiny::div(class = "text-muted", style = "margin:8px 0;",
          "Wavelength-subtracted (delta) experiment — ready to aggregate replicates.")
    })

    output$summary <- shiny::renderUI({
      d <- summ(); shiny::req(d)
      sel <- input$types
      rows <- lapply(seq_len(nrow(d)), function(i) {
        r <- d[i, ]
        shiny::tags$tr(class = if (r$type %in% sel) "info" else NULL,
          shiny::tags$td(r$label), shiny::tags$td(r$rows), shiny::tags$td(r$masked),
          shiny::tags$td(r$rep_groups), shiny::tags$td(r$duplicates),
          shiny::tags$td(r$triplicates), shiny::tags$td(r$more),
          shiny::tags$td(r$rep_rows),
          shiny::tags$td(if (r$type %in% sel) sprintf("%d → %d", r$rows - r$masked, r$rows_after) else "—"))
      })
      shiny::tags$table(class = "table table-condensed table-striped", style = "max-width:900px;",
        shiny::tags$thead(shiny::tags$tr(lapply(
          c("Sample type", "Rows", "Masked (excluded)", "Replicate groups", "Duplicates",
            "Triplicates", "4 or more", "Rows in groups", "Rows if aggregated"), shiny::tags$th))),
        shiny::tags$tbody(rows))
    })

    detail <- shiny::reactive({
      f <- frames()
      parts <- Filter(function(x) nrow(x) > 0,
                      lapply(names(REP_TYPES), function(t) rep_detail(f[[t]], t)))
      if (!length(parts)) return(data.frame())
      cols <- unique(unlist(lapply(parts, names)))
      do.call(rbind, lapply(parts, function(p) { for (m in setdiff(cols, names(p))) p[[m]] <- NA; p[, cols, drop = FALSE] }))
    })
    output$detail <- DT::renderDataTable({
      d <- detail()
      if (!nrow(d)) d <- data.frame(Message = "No replicate groups found in this experiment.")
      DT::datatable(d, rownames = FALSE, filter = "top",
                    options = list(scrollX = TRUE, pageLength = 15))
    }, server = TRUE)
    output$dl_detail <- shiny::downloadHandler(
      filename = function() { s <- scope(); paste0(s$study, "_", s$experiment, "_replicates.csv") },
      content  = function(file) utils::write.csv(detail(), file, row.names = FALSE))

    # ---- create ----
    shiny::observeEvent(input$create, {
      if (!is.null(name_problem()) || !length(input$types)) return()
      d <- summ(); d <- d[d$type %in% input$types, , drop = FALSE]
      shiny::showModal(shiny::modalDialog(
        title = "Create aggregated experiment?", easyClose = TRUE,
        shiny::p(shiny::strong(trimws(input$new_name)), sprintf(
          " will be created in study %s from %s, using the %s of each replicate group.",
          scope()$study, scope()$experiment, input$fun)),
        shiny::tags$ul(lapply(seq_len(nrow(d)), function(i) shiny::tags$li(sprintf(
          "%s: %d rows → %d (%d replicate groups)", d$label[i], d$rows[i] - d$masked[i],
          d$rows_after[i], d$rep_groups[i])))),
        if (isTRUE(input$carry)) shiny::p(class = "text-muted",
          "Other sample types are copied unchanged."),
        shiny::p(class = "text-muted", "The source experiment is not changed."),
        footer = shiny::tagList(shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("confirm"), "Create", class = "btn-primary"))))
    })

    shiny::observeEvent(input$confirm, {
      shiny::removeModal()
      s <- scope(); nm <- trimws(input$new_name)
      if (!is.null(rep_validate_name(nm, s$experiment, existing()))) return()
      res <- tryCatch({
        built <- rep_build_new_experiment(frames(), nm, input$types, input$fun, isTRUE(input$carry))
        rep_write_experiment(pool, built, s$project_id, s$study, s$experiment, nm)
        TRUE
      }, error = function(e) {
        shiny::showNotification(paste("Aggregation failed (rolled back):", conditionMessage(e)),
                                type = "error", duration = NULL)
        FALSE })
      if (isTRUE(res)) {
        reload_trigger(reload_trigger() + 1)
        if (!is.null(refresh_experiments)) refresh_experiments(refresh_experiments() + 1)
        shiny::showNotification(sprintf(
          "Experiment '%s' created. Select it in the experiment list to work with it.", nm),
          type = "message", duration = 10)
      }
    })
  })
}
