# =============================================================================
# std_curve_compare_module.R  --  "Compare fits" sub-tab (11.4)
# -----------------------------------------------------------------------------
# A proper tab (not a modal) that compares standard-curve fits three ways:
#   (a) "forms"   -- multiple model forms of ONE curve+method
#   (b) "methods" -- frequentist vs bayesian for ONE curve (best model each)
#   (c) "plates"  -- all curves in a multiplate group under ONE approach
#
# All three reduce to a COMPARISON SET = list of fits (curve_id, method,
# model_name, label). Every plot iterates that set:
#   1. Overlaid standard curves (fitted lines + observed points)
#   2. Cross-fit CV% of FITTED response at each shared standard concentration
#      (serial-dilution points -- shared across the set; NOT back-calculated
#      concentration, which differs across fits)
#   3. Forest panels (2x5): a,b,c,d,g (estimate + CI) and LLOQ/ULOQ/shape LLOQ/
#      shape ULOQ/inflection (point only -- no CI in the diagnostics schema)
#
# Data: calib_data_access.R (fetch_curve_batch, fetch_calib_fit,
# fetch_calib_methods, fetch_calib_best_model, fetch_calib_params,
# fetch_calib_standards, fetch_calib_samples, fetch_calib_diagnostics,
# fetch_curve_lookup). All plots are ggplot (renderPlot) -- non-interactive.
# =============================================================================

# ---- parameter panels: order + display labels ------------------------------
.CMP_PARAM_LEVELS <- c("a", "b", "c", "d", "g",
                       "lloq", "uloq", "shape_lloq", "shape_uloq", "inflection")
.CMP_PARAM_LABELS <- c(a = "a", b = "b", c = "c", d = "d", g = "g",
                       lloq = "LLOQ", uloq = "ULOQ",
                       shape_lloq = "shape LLOQ", shape_uloq = "shape ULOQ",
                       inflection = "inflection")

# ---- comparison figure: colours + model-form line dashes -------------------
# Kelly maximum-contrast palette for per-plate/curve lines; dash by curveRcore
# model form. Ported from summarize_sc_fits_plotly styling.
.CMP_KELLY <- c("#f3c300","#875692","#f38400","#a1caf1","#be0032","#c2b280",
                "#848482","#008856","#e68fac","#0067a5","#f99379","#604e97",
                "#f6a600","#b3446c","#dcd300","#882d17","#8db600","#654522",
                "#e25822","#2b3d26")
.CMP_DASH <- c(logistic5 = "solid", loglogistic5 = "dash", logistic4 = "dot",
               loglogistic4 = "dashdot", gompertz4 = "longdash")

# estimate + CI for one model term: bayes (q_lo/q_hi) if present, else freq
# (estimate +/- 1.96*std_error), else point only.
.cmp_ci <- function(estimate, std_error, q_lo, q_med, q_hi) {
  fin <- function(x) length(x) && is.finite(x)
  if (fin(q_lo) && fin(q_hi))
    list(est = if (fin(q_med)) q_med else estimate, lo = q_lo, hi = q_hi)
  else if (fin(std_error) && fin(estimate))
    list(est = estimate, lo = estimate - 1.96 * std_error, hi = estimate + 1.96 * std_error)
  else
    list(est = if (fin(estimate)) estimate else NA_real_, lo = NA_real_, hi = NA_real_)
}

# Is the fit's response on a log10 scale? Detected from the standards the same
# way the Explore Curve plot does (response_model ~ log10(assay_response_raw)).
.cmp_y_is_log <- function(sp) {
  if (is.null(sp) || !nrow(sp)) return(TRUE)
  raw <- suppressWarnings(as.numeric(sp$assay_response_raw))
  mod <- suppressWarnings(as.numeric(sp$response_model))
  ok <- is.finite(raw) & raw > 0 & is.finite(mod)
  if (sum(ok) >= 3) isTRUE(mean(abs(mod[ok] - log10(raw[ok])), na.rm = TRUE) < 0.05) else TRUE
}

# curveRcore forward models (verbatim from models.R). x = log10(concentration)
# for every model EXCEPT loglogistic4, which is the Hill form on RAW x. Returns
# response on the model (fit) scale; the caller converts to natural if the fit
# logged the response. b > 0 (always increasing) in this parameterisation.
.cmp_predict <- function(model, conc, a, b, c, d, g = NA_real_) {
  x <- if (identical(model, "loglogistic4")) conc else log10(conc)
  switch(model,
    logistic4    = a + (d - a) / (1 + exp(-(x - c) / b)),
    logistic5    = a + (d - a) / (1 + exp(-(x - c) / b))^g,
    loglogistic4 = a + (d - a) / (1 + (c / x)^b),
    loglogistic5 = a + (d - a) * (1 + g * exp(-b * (x - c)))^(-1 / g),
    gompertz4    = a + (d - a) * exp(-exp(-b * (x - c))),
    rep(NA_real_, length(conc)))
}

# Point estimates (a,b,c,d,g) for one fit; prefers `estimate`, falls back to the
# bayes posterior median q_med. Missing terms (e.g. g on a 4-param fit) -> NA.
.cmp_get_params <- function(pool, cid, method, model) {
  pr <- tryCatch(fetch_calib_params(pool, cid, method, model), error = function(e) NULL)
  if (is.null(pr) || !nrow(pr)) return(NULL)
  g1 <- function(t) {
    est <- suppressWarnings(as.numeric(pr$estimate[tolower(pr$term) == t]))
    if (!length(est) || !is.finite(est[1])) {
      qm <- suppressWarnings(as.numeric(pr$q_med[tolower(pr$term) == t]))
      return(if (length(qm)) qm[1] else NA_real_)
    }
    est[1]
  }
  list(a = g1("a"), b = g1("b"), c = g1("c"), d = g1("d"), g = g1("g"))
}

# Forest rows for ONE fit -> data.frame(parameter, item, est, lo, hi).
.cmp_forest_rows <- function(pool, cid, method, model, item) {
  num <- function(x) suppressWarnings(as.numeric(x))
  out <- list()

  pr <- tryCatch(fetch_calib_params(pool, cid, method, model), error = function(e) NULL)
  for (p in c("a", "b", "c", "d", "g")) {
    row <- if (!is.null(pr) && nrow(pr)) pr[tolower(pr$term) == p, , drop = FALSE] else pr[0, ]
    if (!is.null(row) && nrow(row)) {
      ci <- .cmp_ci(num(row$estimate[1]), num(row$std_error[1]),
                    num(row$q_lo[1]), num(row$q_med[1]), num(row$q_hi[1]))
    } else ci <- list(est = NA_real_, lo = NA_real_, hi = NA_real_)
    out[[length(out) + 1]] <- data.frame(parameter = p, item = item,
      est = ci$est, lo = ci$lo, hi = ci$hi, stringsAsFactors = FALSE)
  }

  dg <- tryCatch(fetch_calib_diagnostics(pool, cid, method), error = function(e) NULL)
  dget <- function(col) if (!is.null(dg) && nrow(dg) && col %in% names(dg)) num(dg[[col]][1]) else NA_real_
  # LOQ *_conc names are exactly what calib_loq() constructs. The inflection
  # column name is not referenced anywhere in the data layer, so try the likely
  # candidates and use the first present (adjust once the real name is known).
  dget_any <- function(cands) {
    for (col in cands) if (!is.null(dg) && nrow(dg) && col %in% names(dg)) {
      v <- num(dg[[col]][1]); if (is.finite(v)) return(v) }
    NA_real_
  }
  pt_vals <- list(
    lloq       = dget("lloq_conc"),
    uloq       = dget("uloq_conc"),
    shape_lloq = dget("shape_lloq_conc"),
    shape_uloq = dget("shape_uloq_conc"),
    inflection = dget_any(c("inflect_x", "inflection_conc", "inflection_point",
                            "inflection", "inflection_x", "inflection_log10")))
  for (nm in names(pt_vals)) {
    out[[length(out) + 1]] <- data.frame(parameter = nm, item = item,
      est = pt_vals[[nm]], lo = NA_real_, hi = NA_real_, stringsAsFactors = FALSE)
  }
  do.call(rbind, out)
}

# raw source -> canonical, from madi_results.source_alias (NON-destructive; an
# unmapped source falls back to itself). Lets the pickers label/group/filter by
# the real standard rather than spelling variants (NIBSC/NIBSC06 -> NIBSC06140).
.cmp_source_map <- function(pool) {
  a <- tryCatch(DBI::dbGetQuery(pool,
    "SELECT raw_source, canonical_source FROM madi_results.source_alias"),
    error = function(e) NULL)
  if (is.null(a) || !nrow(a)) return(function(x) as.character(x))
  m <- stats::setNames(a$canonical_source, a$raw_source)
  function(x) { out <- unname(m[as.character(x)]); ifelse(is.na(out), as.character(x), out) }
}

# =============================================================================
stdCurveCompareUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(4,
        shiny::radioButtons(ns("mode"), "Comparison mode",
          choices = c("All plates (multiplate group)"       = "plates",
                      "Plates gating (sample QC)"           = "plates_gating",
                      "Model forms (one curve)"            = "forms",
                      "Frequentist vs Bayesian (one curve)" = "methods",
                      "Sources (compare standards)"         = "sources"),
          selected = "plates")),
      shiny::column(8, shiny::uiOutput(ns("picker")))
    ),

    # ---- fit-comparison body (all modes EXCEPT plates_gating) --------------
    shiny::conditionalPanel(
      condition = "input.mode != 'plates_gating'", ns = ns,
      shiny::uiOutput(ns("set_note")),
      shiny::hr(),
      shiny::h4("Overlaid standard curves"),
      shinycssloaders::withSpinner(
        plotly::plotlyOutput(ns("summary_plot"), height = "520px"), type = 4, color = "#337ab7"),
      shiny::div(style = "margin:6px 0 2px;",
        shiny::downloadButton(ns("dl_cv_rdata"), "Download CV% (RData)", class = "btn-xs"),
        shiny::downloadButton(ns("dl_cv_json"),  "Download CV% (JSON)",  class = "btn-xs")),
      shiny::uiOutput(ns("cv_note")),
      shiny::h4("Parameter comparison"),
      shinycssloaders::withSpinner(
        shiny::plotOutput(ns("forest_plot"), height = "640px"), type = 4, color = "#337ab7")
    ),

    # ---- plate-gating body (own module; sample QC by plate) ----------------
    shiny::conditionalPanel(
      condition = "input.mode == 'plates_gating'", ns = ns,
      plateGatingUI(ns("gating"))
    )
  )
}

# =============================================================================
stdCurveCompareServer <- function(id, pool, scope = NULL,
                                  selected_curve = shiny::reactiveVal(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
    num <- function(x) suppressWarnings(as.numeric(x))

    scope_val <- reactive(if (!is.null(scope)) scope()
                          else list(study = NULL, experiment = NULL, project_id = NA))

    # "Plates gating" mode is a self-contained module (sample QC by plate). It
    # reads the same scope; its UI is shown by the conditionalPanel above.
    plateGatingServer("gating", pool = pool, scope = scope_val)

    # Scoped curves + labels (batch has multiplate_group_id/antigen/feature;
    # curve_lookup adds plateid). One join drives every picker + label.
    curves <- reactive({
      s <- scope_val()
      shiny::req(s$study, s$experiment)
      b <- tryCatch(fetch_curve_batch(pool, s$study, s$experiment, s$project_id),
                    error = function(e) NULL)
      if (is.null(b) || !nrow(b)) return(NULL)
      # curve_lookup adds plateid + the rest of the position key + source.
      lk   <- tryCatch(fetch_curve_lookup(pool), error = function(e) NULL)
      keep <- intersect(c("curve_id", "plateid", "plate", "nominal_sample_dilution",
                          "wavelength", "source"), names(lk))
      if (!is.null(lk) && "curve_id" %in% keep)
        b <- merge(b, unique(lk[, keep]), by = "curve_id", all.x = TRUE)
      for (col in c("plateid", "plate", "nominal_sample_dilution", "wavelength", "source"))
        if (is.null(b[[col]])) b[[col]] <- NA
      b$csource <- .cmp_source_map(pool)(b$source)      # canonical standard source
      b$clabel  <- sprintf("%s / %s%s", b$antigen %||% "?", b$feature %||% "?",
                           ifelse(is.na(b$plateid), paste0(" (curve ", b$curve_id, ")"),
                                  paste0(" @ ", b$plateid)))
      b
    })

    src_choices <- reactive({
      cv <- curves(); if (is.null(cv)) return(NULL)
      sort(unique(as.character(cv$csource[!is.na(cv$csource)])))
    })

    # Mode D: curve "positions" (natural key minus source) carrying >1 source.
    positions <- reactive({
      cv <- curves(); if (is.null(cv)) return(list())
      cv$poskey <- paste(cv$plateid, cv$plate, cv$nominal_sample_dilution,
                         cv$wavelength, cv$antigen, cv$feature, sep = "\u001f")
      Filter(function(p) length(unique(p$csource[!is.na(p$csource)])) > 1,
             split(cv, cv$poskey))
    })

    # ---- picker (mode-dependent), seeded from the shared selection ----------
    # A/B/C get a Standard-source selector (hidden when the study has one source)
    # that narrows the curve/group list; Mode D uses a position picker instead.
    output$picker <- renderUI({
      # plates_gating owns its own source/antigen/feature filters -> no picker
      if (identical(input$mode, "plates_gating")) return(NULL)
      cv <- curves()
      if (is.null(cv)) return(shiny::helpText("No curves in scope yet."))

      if (identical(input$mode, "sources")) {
        pos <- positions()
        if (!length(pos)) return(shiny::helpText(
          "No curve position in this study has more than one standard source."))
        labs <- vapply(pos, function(p) sprintf(
          "%s / %s @ %s  (%d sources: %s)",
          p$antigen[1] %||% "?", p$feature[1] %||% "?", p$plateid[1] %||% "?",
          length(unique(p$csource)), paste(sort(unique(p$csource)), collapse = ", ")),
          character(1))
        return(shiny::tagList(
          shiny::selectInput(ns("position"),
            "Curve position (compared across its sources)",
            choices = stats::setNames(names(pos), labs)),
          shiny::radioButtons(ns("d_approach"), "Approach",
            choices = c("Frequentist" = "frequentist", "Bayesian" = "bayesian"),
            selected = "frequentist", inline = TRUE)
        ))
      }

      seed <- shiny::isolate(selected_curve())
      seed_src <- if (!is.null(seed))
        cv$csource[match(as.character(seed$curve_id), as.character(cv$curve_id))] else NA
      src <- src_choices()
      shiny::tagList(
        if (length(src) > 1)
          shiny::selectInput(ns("csource"), "Standard source", choices = src,
            selected = if (!is.na(seed_src) && seed_src %in% src) seed_src else src[1])
        else NULL,
        shiny::uiOutput(ns("scoped_picker"))
      )
    })

    # curve/group picker, filtered to the selected source. Re-renders on source
    # change; the Source selector above keeps its value.
    output$scoped_picker <- renderUI({
      cv <- curves(); shiny::req(cv)
      if (identical(input$mode, "sources")) return(NULL)
      src     <- src_choices()
      sel_src <- if (length(src) > 1) input$csource else (src %||% NA_character_)[1]
      cvf <- if (!is.null(sel_src) && !is.na(sel_src))
               cv[cv$csource == sel_src, , drop = FALSE] else cv
      if (!nrow(cvf)) return(shiny::helpText("No curves for this source."))

      seed     <- shiny::isolate(selected_curve())
      seed_cid <- if (!is.null(seed)) as.character(seed$curve_id) else NULL

      if (identical(input$mode, "plates")) {
        grp_ids <- unique(cvf$multiplate_group_id)
        grp_lab <- vapply(grp_ids, function(g) {
          r <- cvf[cvf$multiplate_group_id == g, ][1, ]
          sprintf("%s / %s (%d plates)", r$antigen %||% "?", r$feature %||% "?",
                  sum(cvf$multiplate_group_id == g))
        }, character(1))
        seed_grp <- cvf$multiplate_group_id[match(seed_cid, as.character(cvf$curve_id))]
        shiny::tagList(
          shiny::selectInput(ns("group"), "Multiplate group",
            choices = stats::setNames(as.character(grp_ids), grp_lab),
            selected = as.character(seed_grp %||% grp_ids[1])),
          shiny::radioButtons(ns("approach"), "Approach",
            choices = c("Frequentist" = "frequentist", "Bayesian" = "bayesian"),
            selected = "frequentist", inline = TRUE)
        )
      } else {
        choices <- stats::setNames(as.character(cvf$curve_id), cvf$clabel)
        sel_cid <- if (!is.null(seed_cid) && seed_cid %in% choices) seed_cid else choices[1]
        tl <- shiny::selectInput(ns("curve"), "Curve", choices = choices, selected = sel_cid)
        if (identical(input$mode, "forms"))
          tl <- shiny::tagList(tl, shiny::uiOutput(ns("method_picker")))
        tl
      }
    })

    # method picker for "forms" (which method's model forms to compare)
    output$method_picker <- renderUI({
      shiny::req(input$curve)
      meths <- tryCatch(fetch_calib_methods(pool, input$curve), error = function(e) character(0))
      if (!length(meths)) return(shiny::helpText("No computed fits for this curve."))
      shiny::selectInput(ns("method"), "Method", choices = meths, selected = meths[1])
    })

    # ---- the comparison set: data.frame(curve_id, method, model_name, label) -
    comp_set_full <- reactive({
      cv <- curves(); shiny::req(cv)
      mode <- input$mode %||% "forms"

      if (mode == "forms") {
        cid <- input$curve; meth <- input$method
        shiny::req(cid, meth)
        # ALL converged model forms (not just the best) present in calib_fit --
        # logistic4, gompertz4, ... whichever the Compute-fits run produced.
        fits <- tryCatch(fetch_calib_fit(pool, cid, meth), error = function(e) NULL)
        shiny::validate(shiny::need(!is.null(fits) && nrow(fits) > 0,
                                    "No fits found for this curve+method."))
        conv <- fits[!is.na(fits$converged) & fits$converged, , drop = FALSE]
        shiny::validate(shiny::need(nrow(conv) > 0,
                                    "No converged model forms for this curve+method."))
        data.frame(curve_id = cid, method = meth, model_name = conv$model_name,
                   label = conv$model_name, stringsAsFactors = FALSE)

      } else if (mode == "methods") {
        cid <- input$curve; shiny::req(cid)
        meths <- tryCatch(fetch_calib_methods(pool, cid), error = function(e) character(0))
        shiny::validate(shiny::need(length(meths) > 0, "No computed fits for this curve."))
        rows <- lapply(meths, function(m) {
          bm <- tryCatch(fetch_calib_best_model(pool, cid, m), error = function(e) NULL)
          if (is.null(bm) || !nrow(bm)) return(NULL)
          data.frame(curve_id = cid, method = m, model_name = bm$model_name[1],
                     label = m, stringsAsFactors = FALSE)
        })
        out <- do.call(rbind, rows)
        shiny::validate(shiny::need(!is.null(out) && nrow(out) > 0, "No best models found."))
        out

      } else if (mode == "plates") {
        shiny::req(input$group, input$approach)
        ids <- as.character(cv$curve_id[cv$multiplate_group_id == input$group])
        shiny::validate(shiny::need(length(ids) > 0, "No curves in this group."))
        rows <- lapply(ids, function(cid) {
          bm <- tryCatch(fetch_calib_best_model(pool, cid, input$approach), error = function(e) NULL)
          if (is.null(bm) || !nrow(bm)) return(NULL)
          lab <- cv$clabel[match(cid, as.character(cv$curve_id))]
          data.frame(curve_id = cid, method = input$approach,
                     model_name = bm$model_name[1], label = lab %||% cid,
                     stringsAsFactors = FALSE)
        })
        out <- do.call(rbind, rows)
        shiny::validate(shiny::need(!is.null(out) && nrow(out) > 0,
                                    "No best models under this approach for this group."))
        out

      } else {  # sources -- compare the source variants of one curve position
        shiny::req(input$position, input$d_approach)
        p <- positions()[[input$position]]
        shiny::validate(shiny::need(!is.null(p) && nrow(p) > 0, "Pick a position."))
        rows <- lapply(seq_len(nrow(p)), function(i) {
          bm <- tryCatch(fetch_calib_best_model(pool, p$curve_id[i], input$d_approach),
                         error = function(e) NULL)
          if (is.null(bm) || !nrow(bm)) return(NULL)
          lab <- if (identical(as.character(p$csource[i]), as.character(p$source[i])))
                   as.character(p$csource[i])
                 else sprintf("%s (%s)", p$csource[i], p$source[i])
          data.frame(curve_id = p$curve_id[i], method = input$d_approach,
                     model_name = bm$model_name[1], label = lab, stringsAsFactors = FALSE)
        })
        out <- do.call(rbind, rows)
        shiny::validate(shiny::need(!is.null(out) && nrow(out) > 0,
                                    "No best models under this approach for these sources."))
        out
      }
    })

    # The overlay now draws EVERY curve in the set -- the 12-plate cap and its
    # member picker were retired. comp_set is kept as an alias of comp_set_full
    # so the figure reads the same full set as CV% and the forest panels.
    comp_set <- reactive(comp_set_full())

    output$set_note <- renderUI({
      full <- tryCatch(comp_set_full(), error = function(e) NULL)
      if (is.null(full) || !nrow(full)) return(NULL)
      shiny::tags$p(class = "text-muted",
        sprintf("Comparing %d fit%s (overlay, CV%% and parameters use all).",
                nrow(full), if (nrow(full) == 1) "" else "s"))
    })

    # Shared x-range: the set's standard + sample concentrations (like the
    # Explore Curve plot, whose limits combine standards and test samples).
    conc_range <- reactive({
      cs <- comp_set_full(); key <- unique(cs[, c("curve_id", "method")])
      cc <- unlist(lapply(seq_len(nrow(key)), function(i) {
        sp <- tryCatch(fetch_calib_standards(pool, key$curve_id[i], key$method[i]), error = function(e) NULL)
        sm <- tryCatch(fetch_calib_samples(pool, key$curve_id[i], key$method[i]), error = function(e) NULL)
        c(if (!is.null(sp) && nrow(sp)) num(sp$concentration),
          if (!is.null(sm) && nrow(sm)) c(num(sm$predicted_concentration), num(sm$final_concentration)))
      }))
      cc <- cc[is.finite(cc) & cc > 0]
      if (length(cc)) range(cc) else NULL
    })

    # ---- CV% cross-plate summary (concentration level) ----------------------
    # Preserves the module's ORIGINAL cross-plate CV computation. Meaningful only
    # in "plates" mode (>= 2 plates per concentration). Reused by the figure AND
    # the RData/JSON downloads. Returns data.frame(grp, conc, dilution, n_plates,
    # cv) ordered by concentration, or NULL.
    cv_summary <- reactive({
      if (!identical(input$mode %||% "forms", "plates")) return(NULL)
      cs <- tryCatch(comp_set_full(), error = function(e) NULL)
      if (is.null(cs) || !nrow(cs)) return(NULL)
      key <- unique(cs[, c("curve_id", "method")])
      pts <- do.call(rbind, lapply(seq_len(nrow(key)), function(i) {
        sp <- tryCatch(fetch_calib_standards(pool, key$curve_id[i], key$method[i]), error = function(e) NULL)
        if (is.null(sp) || !nrow(sp)) return(NULL)
        inc <- sp[is.na(sp$included) | sp$included, , drop = FALSE]
        data.frame(curve_id = key$curve_id[i], log10c = num(inc$log10_concentration),
                   conc = num(inc$concentration), dilution = num(inc$dilution),
                   resp = num(inc$assay_response_raw), stringsAsFactors = FALSE)
      }))
      if (is.null(pts) || !nrow(pts)) return(NULL)
      pts <- pts[is.finite(pts$resp) & is.finite(pts$log10c), , drop = FALSE]
      if (!nrow(pts)) return(NULL)
      pts$grp <- round(pts$log10c, 4)
      pm <- do.call(rbind, lapply(split(pts, list(pts$curve_id, pts$grp), drop = TRUE), function(g)
        data.frame(curve_id = g$curve_id[1], grp = g$grp[1],
                   conc = stats::median(g$conc, na.rm = TRUE),
                   resp = mean(g$resp, na.rm = TRUE), stringsAsFactors = FALSE)))
      dil_by <- tapply(pts$dilution, pts$grp, function(v) stats::median(v, na.rm = TRUE))
      agg <- do.call(rbind, lapply(split(pm, pm$grp), function(g) {
        v <- g$resp[is.finite(g$resp)]; m <- mean(v); sdv <- stats::sd(v)
        data.frame(grp = g$grp[1], conc = stats::median(g$conc, na.rm = TRUE),
                   dilution = as.numeric(dil_by[[as.character(g$grp[1])]]),
                   n_plates = length(v),
                   cv = if (length(v) >= 2 && abs(m) > .Machine$double.eps) 100 * sdv / abs(m) else NA_real_,
                   stringsAsFactors = FALSE)
      }))
      agg <- agg[is.finite(agg$cv), , drop = FALSE]
      if (!nrow(agg)) return(NULL)
      agg[order(agg$conc), , drop = FALSE]
    })

    # Long CV table for download: full curve_lookup natural key + curve_id, then
    # concentration, dilution, n_plates, cv_percent. One row per (group curve) x
    # (concentration level); cross-plate n_plates/cv repeat across the curves.
    cv_download_table <- reactive({
      agg <- cv_summary(); if (is.null(agg) || !nrow(agg)) return(NULL)
      lk  <- tryCatch(curves(), error = function(e) NULL); if (is.null(lk) || !nrow(lk)) return(NULL)
      cs  <- comp_set_full(); ids <- unique(as.character(cs$curve_id))
      nk_cols <- setdiff(names(lk), "curve_id")
      do.call(rbind, lapply(ids, function(cid) {
        row <- lk[as.character(lk$curve_id) == cid, , drop = FALSE]
        if (!nrow(row)) return(NULL)
        base <- row[rep(1L, nrow(agg)), nk_cols, drop = FALSE]
        data.frame(base, curve_id = cid,
                   concentration = agg$conc, dilution = agg$dilution,
                   n_plates = agg$n_plates, cv_percent = round(agg$cv, 3),
                   row.names = NULL, stringsAsFactors = FALSE)
      }))
    })

    output$cv_note <- renderUI({
      if (!identical(input$mode %||% "forms", "plates"))
        return(shiny::tags$p(class = "text-muted",
          "CV% is shown only for the All-plates comparison."))
      if (is.null(cv_download_table()))
        return(shiny::tags$p(class = "text-muted",
          "No multiplate group for this plate \u2014 cross-plate CV% needs \u2265 2 plates in a group."))
      NULL
    })

    .cv_fname <- function(ext) {
      sc <- tryCatch(scope_val(), error = function(e) list())
      sprintf("cv_summary_%s_%s.%s", sc$study %||% "study", sc$experiment %||% "exp", ext)
    }
    output$dl_cv_rdata <- shiny::downloadHandler(
      filename = function() .cv_fname("RData"),
      content  = function(file) {
        cv_summary_table <- cv_download_table()
        if (is.null(cv_summary_table))
          cv_summary_table <- data.frame(
            message = "No multiplate group for this plate - cross-plate CV% needs >= 2 plates in a group.")
        save(cv_summary_table, file = file)
      })
    output$dl_cv_json <- shiny::downloadHandler(
      filename = function() .cv_fname("json"),
      content  = function(file) {
        tb <- cv_download_table()
        if (is.null(tb))
          tb <- list(message = "No multiplate group for this plate - cross-plate CV% needs >= 2 plates in a group.")
        jsonlite::write_json(tb, file, dataframe = "rows", pretty = TRUE, auto_unbox = TRUE)
      })

    # ---- overlaid standard curves + (plates only) cross-plate CV% ------------
    # Formatted plotly figure (ported from summarize_sc_fits_plotly): per-plate
    # fitted lines (Kelly palette, dashed by model form) + observed standard
    # points, on log10 axes with natural decade labels (no scientific/exponent).
    # PLATES MODE ONLY: cross-plate CV% overlaid on a right y2 axis (capped at 30%
    # so the low range stays readable; points above 30% labelled with their true
    # value) and a "Dilution Factor" x2 axis on top. No aggregated fit line.
    output$summary_plot <- plotly::renderPlotly({
      prog <- shiny::Progress$new(session = session); on.exit(prog$close())
      prog$set(message = "Building comparison figure\u2026", value = 0.2)
      cs <- comp_set(); mode <- input$mode %||% "forms"
      show_cv <- identical(mode, "plates")
      key <- unique(cs[, c("curve_id", "method")])
      sps <- lapply(seq_len(nrow(key)), function(i)
        tryCatch(fetch_calib_standards(pool, key$curve_id[i], key$method[i]), error = function(e) NULL))
      y_is_log <- .cmp_y_is_log(Find(function(z) !is.null(z) && nrow(z), sps))
      to_nat   <- function(v) if (isTRUE(y_is_log)) 10^v else v
      posok    <- function(d) d[is.finite(d$x) & is.finite(d$y) & d$x > 0 & d$y > 0, , drop = FALSE]

      pts <- do.call(rbind, lapply(sps, function(sp) {
        if (is.null(sp) || !nrow(sp)) return(NULL)
        inc <- sp[is.na(sp$included) | sp$included, , drop = FALSE]
        # points must be on the SAME scale the fit modelled (response_model via
        # to_nat), NOT raw response -- else they float above the fitted line.
        data.frame(x = num(inc$concentration), y = to_nat(num(inc$response_model)), stringsAsFactors = FALSE)
      }))
      rng  <- conc_range()
      grid <- if (!is.null(rng) && rng[1] > 0 && rng[2] > rng[1])
                10^seq(log10(rng[1]), log10(rng[2]), length.out = 250) else NULL

      p <- plotly::plot_ly()
      for (i in seq_len(nrow(cs))) {
        pr <- .cmp_get_params(pool, cs$curve_id[i], cs$method[i], cs$model_name[i])
        if (is.null(grid) || is.null(pr) || !is.finite(pr$a) || !is.finite(pr$b) ||
            !is.finite(pr$c) || !is.finite(pr$d)) next
        ln <- posok(data.frame(x = grid,
                y = to_nat(.cmp_predict(cs$model_name[i], grid, pr$a, pr$b, pr$c, pr$d, pr$g))))
        if (!nrow(ln)) next
        col <- .CMP_KELLY[((i - 1) %% length(.CMP_KELLY)) + 1]
        dsh <- .CMP_DASH[[cs$model_name[i]]]; if (is.null(dsh) || is.na(dsh)) dsh <- "solid"
        p <- plotly::add_lines(p, x = ln$x, y = ln$y, name = as.character(cs$label[i]),
               line = list(color = col, dash = dsh))
      }
      pts <- if (!is.null(pts)) posok(pts) else NULL
      if (!is.null(pts) && nrow(pts))
        p <- plotly::add_markers(p, x = pts$x, y = pts$y, name = "standards", showlegend = FALSE,
               marker = list(color = "grey35", size = 6, opacity = 0.55),
               hoverinfo = "text", text = sprintf("conc %.4g<br>resp %.4g", pts$x, pts$y))

      # log10 axes with natural decade labels (plain -- no scientific/exponent)
      declab <- function(v) formatC(v, format = "fg", big.mark = ",", drop0trailing = TRUE)
      xr <- if (!is.null(rng)) rng else if (!is.null(pts) && nrow(pts)) range(pts$x) else c(1, 10)
      lx <- log10(xr); if (!is.finite(diff(lx)) || diff(lx) <= 0) lx <- c(lx[1] - 0.5, lx[1] + 0.5)
      # trim the high end to the next log10 decade at/above the most concentrated
      # STANDARD point, so the axis never runs past it even if samples/fits do
      std_hi <- if (!is.null(pts) && nrow(pts)) suppressWarnings(max(pts$x, na.rm = TRUE)) else NA_real_
      hi_dec <- if (is.finite(std_hi) && std_hi > 0) ceiling(log10(std_hi)) else ceiling(lx[2])
      lo_pad <- max(0.03 * (hi_dec - lx[1]), 0.05)
      xlrange <- c(lx[1] - lo_pad, hi_dec)
      xdec <- 10^(seq(floor(lx[1]), hi_dec))
      xaxis <- list(title = "concentration", type = "log", range = xlrange, tickmode = "array",
                    tickvals = xdec, ticktext = vapply(xdec, declab, character(1)))
      lay <- list(xaxis = xaxis, yaxis = list(title = "response", type = "log"),
                  legend = list(orientation = "h", y = -0.2), margin = list(t = 40, r = 80))

      if (show_cv) {
        agg <- cv_summary()
        if (!is.null(agg) && nrow(agg)) {
          YCAP <- 30
          p <- plotly::add_trace(p, x = agg$conc, y = pmin(agg$cv, YCAP), yaxis = "y2",
                 type = "scatter", mode = "markers", name = "CV% (cross-plate)",
                 marker = list(color = "#8C70FF", size = 9), hoverinfo = "text",
                 text = sprintf("dilution %s<br>conc %.4g<br>CV%% %.1f<br>n=%d",
                                format(agg$dilution), agg$conc, agg$cv, agg$n_plates))
          # invisible trace so the dilution x2 axis shares the concentration domain
          p <- plotly::add_trace(p, x = agg$conc, y = rep(NA_real_, nrow(agg)), xaxis = "x2",
                 type = "scatter", mode = "lines", showlegend = FALSE, hoverinfo = "none",
                 line = list(color = "rgba(0,0,0,0)"))
          over <- agg[agg$cv > YCAP, , drop = FALSE]
          if (nrow(over))
            p <- plotly::add_text(p, x = over$conc, y = rep(YCAP, nrow(over)), yaxis = "y2",
                   text = sprintf("%.0f%%", over$cv), textposition = "top center",
                   showlegend = FALSE, hoverinfo = "none",
                   textfont = list(color = "#b2182b", size = 10))
          lay$yaxis2 <- list(title = "CV% (across plates)", overlaying = "y", side = "right",
                             range = c(0, YCAP * 1.10), showgrid = FALSE, zeroline = FALSE,
                             tickfont = list(color = "#8C70FF"))
          lay$xaxis2 <- list(overlaying = "x", side = "top", type = "log", range = xlrange,
                             title = "Dilution Factor", tickmode = "array",
                             tickvals = agg$conc, ticktext = as.character(agg$dilution),
                             tickfont = list(color = "#8db600"))
        }
      }
      do.call(function(...) plotly::layout(p, ...), lay)
    })


    # ---- 3. forest panels (2x5) ---------------------------------------------
    output$forest_plot <- renderPlot({
      cs <- comp_set_full()
      df <- do.call(rbind, lapply(seq_len(nrow(cs)), function(i)
        .cmp_forest_rows(pool, cs$curve_id[i], cs$method[i], cs$model_name[i], cs$label[i])))
      shiny::validate(shiny::need(!is.null(df) && nrow(df) > 0, "No parameters to plot."))
      df$parameter <- factor(df$parameter, levels = .CMP_PARAM_LEVELS,
                             labels = .CMP_PARAM_LABELS[.CMP_PARAM_LEVELS])
      df$item <- factor(df$item, levels = rev(unique(cs$label)))  # first fit on top
      ggplot2::ggplot(df, ggplot2::aes(x = est, y = item)) +
        ggplot2::geom_errorbarh(ggplot2::aes(xmin = lo, xmax = hi),
                                height = 0.25, na.rm = TRUE, colour = "grey40") +
        ggplot2::geom_point(na.rm = TRUE, size = 2, colour = "#1a7a40") +
        ggplot2::facet_wrap(~ parameter, nrow = 2, scales = "free_x") +
        ggplot2::labs(x = NULL, y = NULL) +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.y = ggplot2::element_text(size = 8),
                       strip.text = ggplot2::element_text(face = "bold"))
    })

    invisible(NULL)
  })
}
