# =============================================================================
# wavelength_subtraction_module.R  --  Data > Raw inputs > Subtract Wavelengths
# -----------------------------------------------------------------------------
# Makes a NEW experiment from an experiment that was read at more than one
# wavelength (ELISA: 450 nm signal, 620 nm reference): for every well,
#     response(new) = response(signal wavelength) - response(reference wavelength)
# The tab only exists while the selected experiment has two or more wavelengths
# (dataTabServer inserts / removes it). It replaces the old "Subtract Wavelengths"
# button on the Plates tab, which always produced "<exp>|D" for ONE plate.
#
# What the user controls
#   * the new experiment's name (<= 15 characters; default <source>D)
#   * the reference wavelength (subtracted) and the signal wavelength(s) it is
#     subtracted FROM (default: highest wavelength is the reference, every other
#     one is a signal)
#   * which plates go into the new experiment
# and sees, before committing, the wavelengths by sample type and by plate.
#
# A signal row is paired with the reference row of the SAME well on the same
# plate (plate, sample type, sample ID, patient, time period, dilution, antigen,
# feature, source, well). Signal rows with no reference partner are dropped and
# counted. A pair is masked when either well is masked. Per-well computed
# columns (gates, predictions) are blanked -- the compute pipeline recalculates
# them for the new experiment. The new rows carry wavelength "delta" (one signal)
# or "delta_<signal>" (several), and the header's `wavelengths` says the same.
#
# Writing is shared with the Replicates tool: rep_write_experiment() (one
# transaction: header + data rows + antigen-family copy + curve registration).
# Depends on replicates_module.R (REP_*, rep_* helpers).
# =============================================================================

WSUB_COMPUTED <- list(
  xmap_sample   = c("gate_class", "antibody_au", "antibody_au_se", "reference_dilution",
                    "gate_class_dil", "norm_mfi", "in_linear_region", "gate_class_loq",
                    "in_quantifiable_range", "gate_class_linear_region", "quality_score",
                    "predicted_concentration"),
  xmap_standard = "predicted_mfi")

# ---- pure helpers -----------------------------------------------------------
.wsub_is_wl <- function(x) { x <- as.character(x); !is.na(x) & nzchar(x) & x != "__none__" }

.wsub_str <- function(df, cols) {
  if (!length(cols)) return(rep("", nrow(df)))
  do.call(paste, c(lapply(cols, function(k) {
    x <- as.character(df[[k]]); x[is.na(x)] <- "\001NA"; x }), sep = "\r"))
}
# plate identity, the same string on the header and on every data table
.wsub_plate_key <- function(df) .wsub_str(df, intersect(c("plateid", "plate", "nominal_sample_dilution"), names(df)))
.wsub_plate_label <- function(df) {
  lab <- .wsub_str(df, intersect(c("plateid", "plate", "nominal_sample_dilution"), names(df)))
  p <- if ("plateid" %in% names(df)) as.character(df$plateid) else as.character(df$plate_id)
  nsd <- if ("nominal_sample_dilution" %in% names(df)) as.character(df$nominal_sample_dilution) else NA
  ifelse(is.na(nsd) | !nzchar(nsd), p, paste0(p, "  (dilution ", nsd, ")"))
}

# distinct real wavelengths across the four data tables, numerically ordered;
# already-subtracted ("delta...") values are not real wavelengths
wsub_wavelengths <- function(frames) {
  w <- unique(unlist(lapply(frames[names(REP_TYPES)], function(d)
    if (!is.null(d) && "wavelength" %in% names(d)) as.character(d$wavelength))))
  w <- w[.wsub_is_wl(w) & !grepl("^delta", w)]
  w[order(suppressWarnings(as.numeric(w)), w)]
}

# Wavelengths by sample type, and by plate.
wsub_report <- function(frames) {
  by_type <- do.call(rbind, lapply(names(REP_TYPES), function(t) {
    d <- frames[[t]]
    if (is.null(d) || !nrow(d) || !"wavelength" %in% names(d)) return(NULL)
    d <- d[.wsub_is_wl(d$wavelength), , drop = FALSE]; if (!nrow(d)) return(NULL)
    k <- .wsub_plate_key(d)
    do.call(rbind, lapply(split(seq_len(nrow(d)), as.character(d$wavelength)), function(i)
      data.frame(Type = REP_LABELS[[t]], Wavelength = as.character(d$wavelength[i[1]]),
                 Rows = length(i), Plates = length(unique(k[i])), stringsAsFactors = FALSE)))
  }))
  allw <- do.call(rbind, lapply(names(REP_TYPES), function(t) {
    d <- frames[[t]]; if (is.null(d) || !nrow(d) || !"wavelength" %in% names(d)) return(NULL)
    d <- d[.wsub_is_wl(d$wavelength), , drop = FALSE]
    if (nrow(d)) data.frame(key = .wsub_plate_key(d), wavelength = as.character(d$wavelength),
                            label = .wsub_plate_label(d), stringsAsFactors = FALSE)
  }))
  by_plate <- if (is.null(allw)) NULL else do.call(rbind, lapply(split(allw, allw$key), function(g)
    data.frame(key = g$key[1], Plate = g$label[1],
               Wavelengths = paste(sort(unique(g$wavelength)), collapse = " | "),
               Rows = nrow(g), stringsAsFactors = FALSE)))
  if (!is.null(by_plate)) rownames(by_plate) <- NULL
  list(by_type = by_type, by_plate = by_plate)
}

# Plates that hold the reference AND at least one signal wavelength.
wsub_eligible_plates <- function(report_plates_frames, signals, ref) {
  keys <- NULL
  for (t in names(REP_TYPES)) {
    d <- report_plates_frames[[t]]
    if (is.null(d) || !nrow(d) || !"wavelength" %in% names(d)) next
    k <- .wsub_plate_key(d); w <- as.character(d$wavelength)
    lab <- .wsub_plate_label(d)
    cur <- data.frame(key = k, wl = w, label = lab, stringsAsFactors = FALSE)
    keys <- rbind(keys, cur[cur$wl %in% c(signals, ref), , drop = FALSE])
  }
  if (is.null(keys)) return(data.frame(key = character(0), label = character(0)))
  ok <- vapply(split(keys, keys$key), function(g) ref %in% g$wl && any(g$wl %in% signals), NA)
  u <- keys[!duplicated(keys$key) & keys$key %in% names(ok)[ok], c("key", "label")]
  rownames(u) <- NULL; u
}

.wsub_label <- function(signal, n_signals) if (n_signals == 1L) "delta" else paste0("delta_", signal)

# signal - reference for ONE data table. Returns list(out, unmatched).
wsub_apply <- function(df, signals, ref, keep_plates, table_name = NULL) {
  if (is.null(df) || !nrow(df) || !"wavelength" %in% names(df))
    return(list(out = NULL, unmatched = 0L))
  rc <- rep_resp_col(df)
  df <- df[.wsub_plate_key(df) %in% keep_plates, , drop = FALSE]
  wl <- as.character(df$wavelength)
  keys <- setdiff(intersect(c(REP_KEY_COLS, "well"), names(df)), "wavelength")
  refd <- df[wl == ref, , drop = FALSE]
  refd <- refd[!duplicated(.wsub_str(refd, keys)), , drop = FALSE]
  rk <- .wsub_str(refd, keys)
  pieces <- list(); unmatched <- 0L
  for (s in signals) {
    sd <- df[wl == s, , drop = FALSE]; if (!nrow(sd)) next
    m <- match(.wsub_str(sd, keys), rk)
    ok <- !is.na(m); unmatched <- unmatched + sum(!ok)
    if (!any(ok)) next
    o <- sd[ok, , drop = FALSE]; r <- refd[m[ok], , drop = FALSE]
    if (!is.na(rc)) o[[rc]] <- as.numeric(o[[rc]]) - as.numeric(r[[rc]])
    o$wavelength <- .wsub_label(s, length(signals))
    if ("masked" %in% names(o)) {
      mm <- (o$masked %in% TRUE) | (r$masked %in% TRUE)
      o$masked <- mm
      if ("mask_reason" %in% names(o))
        o$mask_reason <- ifelse(mm, ifelse(is.na(o$mask_reason), r$mask_reason, o$mask_reason), NA_character_)
    }
    pieces[[length(pieces) + 1]] <- o
  }
  if (!length(pieces)) return(list(out = NULL, unmatched = unmatched))
  out <- do.call(rbind, pieces)
  for (cn in intersect(if (is.null(table_name)) NULL else WSUB_COMPUTED[[table_name]], names(out))) out[[cn]][] <- NA
  out <- out[, !grepl("^xmap_.*_id$", names(out)), drop = FALSE]
  rownames(out) <- NULL
  list(out = out, unmatched = unmatched)
}

# The whole new experiment (no DB): frames = list(S,C,B,X,header) of the source.
wsub_build <- function(frames, new_exp, signals, ref, keep_plates) {
  tabs <- list(); stats <- list()
  for (t in names(REP_TYPES)) {
    r <- wsub_apply(frames[[t]], signals, ref, keep_plates, REP_TYPES[[t]])
    n_in <- if (is.null(frames[[t]])) 0L else sum(.wsub_plate_key(frames[[t]]) %in% keep_plates &
                                                   as.character(frames[[t]]$wavelength) %in% signals)
    if (!is.null(r$out) && nrow(r$out)) {
      r$out$experiment_accession <- new_exp
      tabs[[REP_TYPES[[t]]]] <- r$out
    }
    stats[[t]] <- list(signal_rows = n_in, written = if (is.null(r$out)) 0L else nrow(r$out),
                       unmatched = r$unmatched)
  }
  hdr <- frames$header
  if (!is.null(hdr) && nrow(hdr)) {
    hdr <- hdr[.wsub_plate_key(hdr) %in% keep_plates, , drop = FALSE]
    hdr <- hdr[, !grepl("^xmap_.*_id$", names(hdr)), drop = FALSE]
    hdr$experiment_accession <- new_exp
    if ("wavelengths" %in% names(hdr))
      hdr$wavelengths <- paste(.wsub_label(signals, length(signals)), collapse = ",")
  }
  list(header = hdr, tables = tabs, stats = stats)
}

# ---- UI ---------------------------------------------------------------------
wavelengthSubtractUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(class = "help-block", style = "margin:8px 0;",
      shiny::strong("Subtract one wavelength from another and keep the result as a new experiment."),
      shiny::br(),
      shiny::tags$small("Run this before the Replicates tool: aggregate replicates on the subtracted experiment.")),
    shiny::h4("1. Wavelengths in this experiment"),
    shiny::uiOutput(ns("report_type")),
    DT::dataTableOutput(ns("report_plate")),
    shiny::h4("2. What to subtract"),
    shiny::fluidRow(
      shiny::column(3, shiny::uiOutput(ns("ref_ui"))),
      shiny::column(3, shiny::uiOutput(ns("signal_ui"))),
      shiny::column(6, shiny::uiOutput(ns("plates_ui")))),
    shiny::h4("3. New experiment"),
    shiny::fluidRow(shiny::column(5,
      shiny::textInput(ns("new_name"), sprintf("New experiment name (max %d characters)", REP_MAX_NAME)),
      shiny::tags$script(shiny::HTML(sprintf(
        "$(function(){ $('#%s').attr('maxlength', %d); });", ns("new_name"), REP_MAX_NAME))),
      shiny::uiOutput(ns("name_msg")),
      shiny::uiOutput(ns("equation")),
      shiny::actionButton(ns("create"), "Create subtracted experiment",
                          icon = shiny::icon("minus"), class = "btn-primary")))
  )
}

# ---- server -----------------------------------------------------------------
#' @return list(multi = reactive(TRUE when the experiment has >= 2 wavelengths))
wavelengthSubtractServer <- function(id, scope, get_table, pool, reload_trigger,
                                     refresh_experiments = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    frames <- shiny::reactive({
      s <- scope(); shiny::req(s$study, s$experiment); reload_trigger()
      list(S = get_table("xmap_standard"), C = get_table("xmap_control"),
           B = get_table("xmap_buffer"),   X = get_table("xmap_sample"),
           header = get_table("xmap_header"))
    })
    wls    <- shiny::reactive(wsub_wavelengths(frames()))
    multi  <- shiny::reactive(length(wls()) >= 2L)
    report <- shiny::reactive(wsub_report(frames()))

    existing <- shiny::reactive({
      s <- scope(); shiny::req(s$study); reload_trigger()
      if (!is.null(refresh_experiments)) refresh_experiments()
      d <- .calib_q(pool, sprintf(
        "SELECT DISTINCT experiment_accession FROM %s WHERE project_id = $1 AND study_accession = $2",
        .tbl("xmap_header")), params = list(s$project_id, s$study))
      as.character(d$experiment_accession)
    })

    shiny::observeEvent(scope()$experiment, {
      shiny::updateTextInput(session, "new_name", value = rep_default_name(scope()$experiment, "D"))
    }, ignoreNULL = TRUE)

    # -- report --
    output$report_type <- shiny::renderUI({
      r <- report()$by_type; shiny::req(r)
      w <- wls()
      cells <- function(type) vapply(w, function(x) {
        v <- r$Rows[r$Type == type & r$Wavelength == x]
        if (length(v)) format(v, big.mark = ",") else "—" }, "")
      types <- unique(r$Type)
      shiny::tags$table(class = "table table-condensed table-striped", style = "max-width:700px;",
        shiny::tags$thead(shiny::tags$tr(shiny::tags$th("Sample type"),
          lapply(w, function(x) shiny::tags$th(paste(x, "(rows)"))))),
        shiny::tags$tbody(lapply(types, function(t) shiny::tags$tr(
          shiny::tags$td(t), lapply(cells(t), shiny::tags$td)))))
    })
    output$report_plate <- DT::renderDataTable({
      p <- report()$by_plate; shiny::req(p)
      DT::datatable(p[, c("Plate", "Wavelengths", "Rows")], rownames = FALSE,
                    options = list(dom = "tp", pageLength = 8, scrollX = TRUE))
    })

    # -- choices --
    output$ref_ui <- shiny::renderUI({
      w <- wls(); shiny::req(length(w) >= 2L)
      shiny::radioButtons(session$ns("ref"), "Subtract this wavelength (reference)",
        choices = w, selected = w[length(w)])
    })
    output$signal_ui <- shiny::renderUI({
      ref <- input$ref; shiny::req(ref)
      o <- setdiff(wls(), ref)
      shiny::checkboxGroupInput(session$ns("signals"), "…from these wavelength(s)",
                                choices = o, selected = o)
    })
    eligible <- shiny::reactive({
      shiny::req(input$ref, length(input$signals) > 0)
      wsub_eligible_plates(frames(), input$signals, input$ref)
    })
    output$plates_ui <- shiny::renderUI({
      e <- eligible()
      prev <- shiny::isolate(input$plates)
      sel <- if (is.null(prev)) e$key else intersect(prev, e$key)
      if (!nrow(e)) return(shiny::div(class = "text-warning",
        "No plate holds both the reference and a signal wavelength."))
      shiny::tagList(
        shiny::checkboxGroupInput(session$ns("plates"), "Plates to include",
          choiceNames = e$label, choiceValues = e$key, selected = sel),
        shiny::tags$small(class = "text-muted",
          "Plates missing the reference or every signal wavelength are not listed."))
    })

    name_problem <- shiny::reactive(rep_validate_name(input$new_name, scope()$experiment, existing()))
    output$name_msg <- shiny::renderUI({
      p <- name_problem()
      if (!is.null(p)) shiny::div(class = "text-danger", style = "margin-bottom:6px;", p)
    })
    output$equation <- shiny::renderUI({
      shiny::req(input$ref, length(input$signals) > 0)
      shiny::div(style = "margin-bottom:8px;", shiny::tags$small(
        "New response = ", paste(sprintf("%s − %s", input$signals, input$ref), collapse = ", "),
        sprintf(" (written as wavelength '%s')", paste(.wsub_label(input$signals, length(input$signals)), collapse = "', '"))))
    })
    shiny::observe({
      shinyjs::toggleState("create", condition = is.null(name_problem()) &&
        length(input$signals) > 0 && length(input$plates) > 0)
    })

    # -- create --
    plan <- shiny::reactive({
      wsub_build(frames(), trimws(input$new_name), input$signals, input$ref, input$plates)
    })
    shiny::observeEvent(input$create, {
      if (!is.null(name_problem()) || !length(input$plates) || !length(input$signals)) return()
      b <- plan(); st <- b$stats
      shiny::showModal(shiny::modalDialog(
        title = "Create subtracted experiment?", easyClose = TRUE,
        shiny::p(shiny::strong(trimws(input$new_name)), sprintf(" will be created in study %s from %s: %s, for %d plate(s).",
          scope()$study, scope()$experiment,
          paste(sprintf("%s − %s", input$signals, input$ref), collapse = ", "), length(input$plates))),
        shiny::tags$ul(lapply(names(st), function(t) if (st[[t]]$signal_rows > 0) shiny::tags$li(sprintf(
          "%s: %d rows written%s", REP_LABELS[[t]], st[[t]]$written,
          if (st[[t]]$unmatched > 0) sprintf(" (%d signal rows had no reference well and were dropped)", st[[t]]$unmatched) else "")))),
        shiny::p(class = "text-muted", "The source experiment is not changed."),
        footer = shiny::tagList(shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("confirm"), "Create", class = "btn-primary"))))
    })
    shiny::observeEvent(input$confirm, {
      shiny::removeModal()
      s <- scope(); nm <- trimws(input$new_name)
      if (!is.null(rep_validate_name(nm, s$experiment, existing()))) return()
      ok <- tryCatch({
        b <- wsub_build(frames(), nm, input$signals, input$ref, input$plates)
        if (!length(b$tables)) stop("no wells could be paired with a reference well")
        rep_write_experiment(pool, b, s$project_id, s$study, s$experiment, nm)
        TRUE
      }, error = function(e) {
        shiny::showNotification(paste("Subtraction failed (rolled back):", conditionMessage(e)),
                                type = "error", duration = NULL); FALSE })
      if (isTRUE(ok)) {
        reload_trigger(reload_trigger() + 1)
        if (!is.null(refresh_experiments)) refresh_experiments(refresh_experiments() + 1)
        shiny::showNotification(sprintf(
          "Experiment '%s' created. Select it in the experiment list, then use Replicates if you have duplicates.", nm),
          type = "message", duration = 10)
      }
    })

    list(multi = multi)
  })
}
