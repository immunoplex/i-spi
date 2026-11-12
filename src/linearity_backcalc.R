# =============================================================================
# linearity_backcalc.R  --  concentrations for CONTROLS and STANDARDS, so they can
#                           be compared in QC > Replicate Linearity like test samples
#
# The worker back-calculates only test samples (calib_samples). Controls and the
# standard-curve calibrators carry no stored concentration, pcov or gate class, so
# they are computed here from what IS stored for each curve and method:
#
#   concentration  the best model's parameters (calib_param) inverted at the well's
#                  response, with the same inverses the worker uses (.fda_inv_*;
#                  Bayesian uses the posterior median of each parameter)
#   response scale measured from calib_standards (raw / log10 / ln), exactly as the
#                  fit-diagnostics report does; standards already carry the model
#                  scale (response_model), so only controls need it
#   pcov           the stored calib_grid pcov profile read at the estimated
#                  log10 concentration. The grid is the same delta-method pcov
#                  evaluated at y = f(x), so a response that inverts to x has the
#                  pcov the grid has at x (the parameter covariance itself is not
#                  stored). Off-curve responses get the cap (150), as the worker does
#   gate class     belowLLOQ / inDynamicRange / aboveULOQ from calib_diagnostics
#                  (LLOQ/ULOQ; the pcov-threshold fallback when they are undefined),
#                  the same rule as curveRcore::classify_pcov_gate
#   dilution       controls: final = estimate x the control's dilution, as for samples.
#                  Standards: the estimate itself (a standard's dilution is already in
#                  its nominal concentration)
#
# lin_backcalc_check() measures how closely this reproduces the worker: the model
# inverse against the stored grid, and the grid-read pcov against the pcov stored for
# test samples. Its result is shown when the points are loaded and goes in the report.
#
# Pure functions over plain frames, except lin_fetch_extra(), which reads the DB.
# Needs .fda_backcalc_conc / .fda_params (calib_data_access.R).
#
#   ex  <- lin_fetch_extra(pool, project, study, experiment, c("control", "standard"))
#   pts <- rbind(lin_prepare(raw_samples, "sample"),
#                lin_prepare(ex$control, "control"), lin_prepare(ex$standard, "standard"))
# =============================================================================

LIN_CV_CAP <- 150           # the worker's cv_x_max
LIN_LOG_FLOOR <- 1e-6       # the worker's floor before taking logs

# Response on the scale the model was fitted on.
lin_model_response <- function(raw, scale) {
  raw <- suppressWarnings(as.numeric(raw))
  switch(as.character(scale)[1],
         log10 = log10(pmax(raw, LIN_LOG_FLOOR)),
         ln    = log(pmax(raw, LIN_LOG_FLOOR)),
         raw   = raw,
         rep(NA_real_, length(raw)))
}

# Scale per curve and method from fetch_calib_standards_summary_scoped(). Where it
# cannot be measured (no standards, or something other than a plain transform such
# as a blank-subtracted fit), the experiment's most common measured scale is used,
# else log10, and `inferred` says so.
lin_scales <- function(summ) {
  out <- data.frame(.cid = character(), method = character(), scale = character(), inferred = logical(),
                    stringsAsFactors = FALSE)
  if (is.null(summ) || !is.data.frame(summ) || !nrow(summ)) return(out)
  num <- function(v) suppressWarnings(as.numeric(v))
  dr <- num(summ$dev_raw); d10 <- num(summ$dev_log10); dl <- num(summ$dev_ln)
  sc <- vapply(seq_along(dr), function(i) {
    v <- c(raw = dr[i], log10 = d10[i], ln = dl[i]); v <- v[is.finite(v)]
    if (!length(v)) return(NA_character_)
    k <- names(v)[which.min(v)]
    if (v[[k]] < 1e-6) k else NA_character_
  }, character(1))
  fb <- if (any(!is.na(sc))) names(sort(table(sc), decreasing = TRUE))[1] else "log10"
  data.frame(.cid = .lin_id(summ$curve_id), method = as.character(summ$method),
             scale = ifelse(is.na(sc), fb, sc), inferred = is.na(sc), stringsAsFactors = FALSE)
}

# Read a stored grid column at log10 concentrations. Outside the grid (or where the
# column is missing) the value is NA.
.lin_grid_at <- function(g, x_log10, col) {
  if (is.null(g) || !nrow(g) || !col %in% names(g)) return(rep(NA_real_, length(x_log10)))
  gx <- suppressWarnings(as.numeric(g$log10_concentration)); gv <- suppressWarnings(as.numeric(g[[col]]))
  ok <- is.finite(gx) & is.finite(gv)
  if (sum(ok) < 2L) return(rep(NA_real_, length(x_log10)))
  o <- order(gx[ok])
  stats::approx(gx[ok][o], gv[ok][o], xout = x_log10, rule = 1, ties = mean)$y
}

# Same rule as curveRcore::classify_pcov_gate: position against LLOQ/ULOQ when both
# exist, otherwise pcov against the threshold, with failures placed by the inflection.
lin_gate_class <- function(x_log10, pcov, pcov_threshold = 20, lloq = NA_real_, uloq = NA_real_, inflect = NA_real_) {
  n <- length(x_log10)
  gate <- rep(NA_character_, n); pass <- rep(NA, n)
  vx <- is.finite(x_log10)
  fin <- function(v) length(v) == 1L && is.finite(v)
  if (fin(lloq) && fin(uloq)) {
    below <- vx & x_log10 < lloq; above <- vx & x_log10 > uloq; inr <- vx & !below & !above
    gate[below] <- "belowLLOQ"; gate[inr] <- "inDynamicRange"; gate[above] <- "aboveULOQ"
    pass[vx] <- inr[vx]
  } else {
    thr <- if (fin(pcov_threshold)) pcov_threshold else 20
    ev <- vx & is.finite(pcov)
    ok <- ev & pcov < thr
    pass[ev] <- ok[ev]
    gate[ok] <- "inDynamicRange"
    fail <- ev & !ok
    if (fin(inflect)) { gate[fail & x_log10 < inflect] <- "belowLLOQ"; gate[fail & x_log10 >= inflect] <- "aboveULOQ" }
  }
  data.frame(pcov_gate_class = gate, pcov_pass = pass, stringsAsFactors = FALSE)
}

# Parameter list (a, b, c, d, g) of a best model: posterior median for Bayesian fits
# (the estimate when no median is stored), the estimate for frequentist fits.
.lin_params <- function(pr, method) {
  if (is.null(pr) || !nrow(pr)) return(NULL)
  est <- suppressWarnings(as.numeric(pr$estimate))
  if (identical(as.character(method), "bayesian") && "q_med" %in% names(pr)) {
    qm <- suppressWarnings(as.numeric(pr$q_med)); est <- ifelse(is.finite(qm), qm, est)
  }
  .fda_params(data.frame(term = pr$term, estimate = est, stringsAsFactors = FALSE))
}

.lin_invert <- function(model, y, p) {
  if (is.null(p)) return(rep(NA_real_, length(y)))
  vapply(y, function(v) .fda_backcalc_conc(model, v, p$a, p$b, p$c, p$d, p$g), numeric(1))
}

# Index a frame by curve and method (character keys, so integer64 ids are safe).
.lin_key <- function(df, ...) {
  parts <- list(...)
  do.call(paste, c(lapply(parts, as.character), sep = "\r"))
}

# ---- the back-calculation ----------------------------------------------------------
# resp:   one row per measurement. Needs curve_id and y (the response on the model
#         scale); method is optional (without it every method with a best fit is used);
#         any other column passes through unchanged.
# best:   curve_id, method, model_name (one row per curve and method)
# params: curve_id, method, model_name, term, estimate, q_med
# grid:   curve_id, method, log10_concentration, pcov (the stored profile)
# diag:   curve_id, method, lloq_log10, uloq_log10, inflect_x, pcov_threshold
# Returns resp's rows (per method) with model_name, x_log10, conc (natural scale, per
# unit of dilution), pcov, pcov_pass, pcov_gate_class.
lin_backcalc <- function(resp, best, params, grid, diag = NULL, cv_cap = LIN_CV_CAP) {
  stopifnot(is.data.frame(resp), all(c("curve_id", "y") %in% names(resp)))
  add <- function(df) {
    df$model_name <- character(nrow(df)); df$x_log10 <- numeric(nrow(df)); df$conc <- numeric(nrow(df))
    df$pcov <- numeric(nrow(df)); df$pcov_pass <- logical(nrow(df)); df$pcov_gate_class <- character(nrow(df)); df
  }
  if (!nrow(resp) || is.null(best) || !nrow(best)) return(add(resp[0, , drop = FALSE]))
  resp$.cid <- .lin_id(resp$curve_id)
  b <- data.frame(.cid = .lin_id(best$curve_id), method = as.character(best$method),
                  model_name = as.character(best$model_name), stringsAsFactors = FALSE)
  b <- b[!duplicated(paste(b$.cid, b$method)), , drop = FALSE]
  resp$.i <- seq_len(nrow(resp))
  m <- if ("method" %in% names(resp)) { resp$method <- as.character(resp$method); merge(resp, b, by = c(".cid", "method")) }
       else merge(resp, b, by = ".cid")
  if (!nrow(m)) return(add(resp[0, setdiff(names(resp), c(".cid", ".i")), drop = FALSE]))
  m <- m[order(m$.i), , drop = FALSE]
  m$x_log10 <- NA_real_; m$conc <- NA_real_; m$pcov <- NA_real_
  m$pcov_pass <- NA; m$pcov_gate_class <- NA_character_

  pk <- split(params, .lin_key(params, .lin_id(params$curve_id), params$method, params$model_name))
  gk <- split(grid,   .lin_key(grid,   .lin_id(grid$curve_id),   grid$method))
  dk <- if (is.null(diag) || !nrow(diag)) list() else split(diag, .lin_key(diag, .lin_id(diag$curve_id), diag$method))
  num1 <- function(d, col) if (is.null(d) || !nrow(d) || !col %in% names(d)) NA_real_ else suppressWarnings(as.numeric(d[[col]][1]))

  grp <- split(seq_len(nrow(m)), .lin_key(m, m$.cid, m$method))
  for (idx in grp) {
    cid <- m$.cid[idx[1]]; me <- m$method[idx[1]]; mod <- m$model_name[idx[1]]
    p <- .lin_params(pk[[.lin_key(NULL, cid, me, mod)]], me)
    conc <- .lin_invert(mod, suppressWarnings(as.numeric(m$y[idx])), p)
    x <- suppressWarnings(log10(conc))
    pc <- .lin_grid_at(gk[[.lin_key(NULL, cid, me)]], x, "pcov")
    pc[!is.finite(pc)] <- cv_cap; pc <- pmin(pc, cv_cap)
    d <- dk[[.lin_key(NULL, cid, me)]]
    g <- lin_gate_class(x, pc, num1(d, "pcov_threshold"), num1(d, "lloq_log10"), num1(d, "uloq_log10"), num1(d, "inflect_x"))
    m$conc[idx] <- conc; m$x_log10[idx] <- x; m$pcov[idx] <- pc
    m$pcov_pass[idx] <- g$pcov_pass; m$pcov_gate_class[idx] <- g$pcov_gate_class
  }
  m$curve_id <- m$.cid
  m[, setdiff(names(m), c(".cid", ".i", "y")), drop = FALSE]
}

# ---- shape into the point table's input (the lin_fetch columns) -----------------------
.lin_none_chr <- function(x) { x <- as.character(x); x[is.na(x) | !nzchar(x)] <- "__none__"; x }

# A control's label. xmap_control has no patient or description column; the importer puts
# the control's name ("QC1 (Low)", "Inhouse Ref") in `source` and the well in sampleid.
# So a sampleid that is only the well is replaced by the source, else by the type code and
# dilution, else it stays the well. The dilution is not part of the label: the same control
# is at 1:2500 on one plate and 1:5000 on another and is still one control.
lin_control_label <- function(sampleid, well, source = NULL, stype = NULL, dilution = NULL) {
  chr <- function(v) { v <- if (is.null(v)) rep("", length(sampleid)) else trimws(as.character(v)); v[is.na(v)] <- ""; v }
  sid <- chr(sampleid); w <- chr(well); src <- chr(source); st <- chr(stype)
  dl <- if (is.null(dilution)) rep(NA_real_, length(sid)) else suppressWarnings(as.numeric(dilution))
  dtxt <- ifelse(is.finite(dl), paste0("1:", format(dl, trim = TRUE, scientific = FALSE)), "")
  fallback <- !nzchar(sid) | toupper(sid) == toupper(w)
  alt <- ifelse(nzchar(src), src, ifelse(nzchar(dtxt), trimws(paste(st, dtxt)), ""))   # a bare "C" names nothing
  ifelse(fallback & nzchar(alt), alt, ifelse(nzchar(sid), sid, w))
}

# Controls: raw rows (curve_id, well, sampleid, dilution, mfi, plateid, plate, antigen,
# feature, source, wavelength) -> lin_fetch-shaped rows. One control well is read
# through every curve of its plate and antigen (every source) and every method.
lin_backcalc_controls <- function(ctl, best, params, grid, diag = NULL, scales = NULL) {
  if (is.null(ctl) || !nrow(ctl)) return(NULL)
  b <- data.frame(.cid = .lin_id(best$curve_id), method = as.character(best$method), stringsAsFactors = FALSE)
  ctl$.cid <- .lin_id(ctl$curve_id)
  ctl$sampleid <- lin_control_label(ctl$sampleid, ctl$well, ctl$control_source, ctl$stype, ctl$dilution)
  e <- merge(ctl, b, by = ".cid")
  if (!nrow(e)) return(NULL)
  sc <- if (is.null(scales) || !nrow(scales)) NULL else scales
  fb <- if (!is.null(sc)) names(sort(table(sc$scale), decreasing = TRUE))[1] else "log10"
  hit <- if (is.null(sc)) rep(NA_integer_, nrow(e)) else match(paste(e$.cid, e$method), paste(sc$.cid, sc$method))
  scale <- ifelse(is.na(hit), fb, sc$scale[hit])
  e$y <- NA_real_
  for (s in unique(scale)) { k <- scale == s; e$y[k] <- lin_model_response(e$mfi[k], s) }
  r <- lin_backcalc(e, best, params, grid, diag)
  if (!nrow(r)) return(NULL)
  dil <- suppressWarnings(as.numeric(r$dilution)); dil[!is.finite(dil)] <- 1
  data.frame(curve_id = r$curve_id, method = r$method, sampleid = .lin_none_chr(r$sampleid),
             patientid = "__none__", timeperiod = "__none__",
             dilution = ifelse(is.na(r$dilution), "1", format(r$dilution, trim = TRUE, scientific = FALSE)),
             final_concentration = r$conc * dil, pcov = r$pcov, pcov_pass = r$pcov_pass,
             pcov_gate_class = r$pcov_gate_class, plateid = r$plateid, plate = r$plate,
             antigen = r$antigen, feature = r$feature, source = r$source, wavelength = r$wavelength,
             well = r$well, stringsAsFactors = FALSE)
}

# Standards: calib_standards rows (curve_id, method, well, dilution, nominal, y_model,
# plateid, plate, antigen, feature, source, wavelength). The set is the standard LEVEL
# ("std <dilution>") on one antigen, so wells of one level on every plate are replicates.
lin_backcalc_standards <- function(std, best, params, grid, diag = NULL) {
  if (is.null(std) || !nrow(std)) return(NULL)
  std$y <- suppressWarnings(as.numeric(std$y_model))
  r <- lin_backcalc(std, best, params, grid, diag)
  if (!nrow(r)) return(NULL)
  lvl <- format(suppressWarnings(as.numeric(r$dilution)), trim = TRUE, scientific = FALSE)
  data.frame(curve_id = r$curve_id, method = r$method, sampleid = paste0("std ", lvl),
             patientid = "__none__", timeperiod = "__none__", dilution = lvl,
             final_concentration = r$conc, pcov = r$pcov, pcov_pass = r$pcov_pass,
             pcov_gate_class = r$pcov_gate_class, plateid = r$plateid, plate = r$plate,
             antigen = r$antigen, feature = r$feature, source = r$source, wavelength = r$wavelength,
             well = r$well, nominal = suppressWarnings(as.numeric(r$nominal)), stringsAsFactors = FALSE)
}

# ---- how close is this to the worker? ----------------------------------------------------
# (a) model inverse: invert the stored grid's predicted response and compare with the
#     grid's own log10 concentration (frequentist: ~0; Bayesian: the gap between the
#     posterior-median parameters and the median of the draw-wise inverses), over
#     grid points where pcov is informative (< 100).
# (b) pcov: the grid read at each stored test sample's concentration against the pcov
#     stored for that sample. `samples` is the lin_fetch frame (curve_id, method,
#     final_concentration, dilution, pcov).
lin_backcalc_check <- function(best, params, grid, samples = NULL) {
  q <- function(v, p) if (length(v)) as.numeric(stats::quantile(v, p, na.rm = TRUE, names = FALSE)) else NA_real_
  if (is.null(best) || !nrow(best) || is.null(grid) || !nrow(grid)) return(NULL)
  b <- data.frame(.cid = .lin_id(best$curve_id), method = as.character(best$method),
                  model_name = as.character(best$model_name), stringsAsFactors = FALSE)
  pk <- split(params, .lin_key(params, .lin_id(params$curve_id), params$method, params$model_name))
  gk <- split(grid, .lin_key(grid, .lin_id(grid$curve_id), grid$method))
  sk <- if (is.null(samples) || !nrow(samples)) list() else split(samples, .lin_key(samples, .lin_id(samples$curve_id), samples$method))
  rows <- lapply(unique(b$method), function(me) {
    bi <- b[b$method == me, , drop = FALSE]
    dg <- numeric(); dp <- numeric()
    for (i in seq_len(nrow(bi))) {
      g <- gk[[.lin_key(NULL, bi$.cid[i], me)]]
      if (is.null(g) || !nrow(g)) next
      p <- .lin_params(pk[[.lin_key(NULL, bi$.cid[i], me, bi$model_name[i])]], me)
      x <- suppressWarnings(as.numeric(g$log10_concentration)); y <- suppressWarnings(as.numeric(g$predicted_response))
      pc <- suppressWarnings(as.numeric(g$pcov))
      use <- is.finite(x) & is.finite(y) & is.finite(pc) & pc < 100
      if (any(use) && !is.null(p))
        dg <- c(dg, abs(suppressWarnings(log10(.lin_invert(bi$model_name[i], y[use], p))) - x[use]))
      s <- sk[[.lin_key(NULL, bi$.cid[i], me)]]
      if (!is.null(s) && nrow(s)) {
        dil <- suppressWarnings(as.numeric(s$dilution)); fc <- suppressWarnings(as.numeric(s$final_concentration))
        sx <- suppressWarnings(log10(fc / dil)); sp <- suppressWarnings(as.numeric(s$pcov))
        ok <- is.finite(sx) & is.finite(sp) & sp < LIN_CV_CAP
        if (any(ok)) { gp <- .lin_grid_at(g, sx[ok], "pcov"); dp <- c(dp, abs(gp - sp[ok])[is.finite(gp)]) }
      }
    }
    dg <- dg[is.finite(dg)]
    data.frame(method = me, n_grid = length(dg), inverse_med_dlog10 = q(dg, .5), inverse_p95_dlog10 = q(dg, .95),
               n_samples = length(dp), pcov_med_diff = q(dp, .5), pcov_p95_diff = q(dp, .95), stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

lin_backcalc_check_text <- function(chk) {
  if (is.null(chk) || !nrow(chk)) return("")
  f <- function(v, d = 3) if (is.na(v)) "n/a" else format(signif(v, d), scientific = FALSE, trim = TRUE)
  paste(vapply(seq_len(nrow(chk)), function(i) {
    z <- chk[i, ]
    sprintf("%s: inverse vs stored grid, median %s log10 (95th pct %s) over %d grid points%s", z$method,
            f(z$inverse_med_dlog10), f(z$inverse_p95_dlog10), z$n_grid,
            if (z$n_samples > 0) sprintf("; pcov vs stored test samples, median %s points (95th pct %s) over %d samples",
                                         f(z$pcov_med_diff), f(z$pcov_p95_diff), z$n_samples) else "")
  }, ""), collapse = " | ")
}

# Where did the control wells go? One line from lin_fetch_extra()$control_stages: wells
# stored, how many joined to a curve, were not masked, had a best fit, got a concentration.
lin_control_stages_text <- function(st) {
  if (is.null(st)) return("")
  n <- function(v) if (is.na(v)) "?" else format(v, big.mark = ",")
  paste0("Control wells: ", n(st$stored), " stored", if (!is.na(st$masked) && st$masked > 0) sprintf(" (%s masked)", n(st$masked)) else "",
         " > ", n(st$joined), " matched to a curve (plate, antigen, feature, dilution, wavelength) > ", n(st$kept),
         " not masked > ", n(st$with_fit), " read through a best fit > ", n(st$with_conc), " with a concentration",
         if (!is.na(st$names) && nzchar(st$names)) paste0(". Names stored: ", substr(st$names, 1, 700)) else "", ".")
}

# ---- fetch ---------------------------------------------------------------------------------------
# Reads only what is needed, scoped to the experiment. Returns
#   list(control = raw|NULL, standard = raw|NULL, check = data.frame|NULL, scales_inferred = n)
# `samples` (the lin_fetch frame) enables the pcov check.
lin_fetch_extra <- function(pool, project, study, experiment, kinds = c("control", "standard"),
                            samples = NULL, include_masked = FALSE) {
  kinds <- intersect(kinds, c("control", "standard"))
  out <- list(control = NULL, standard = NULL, check = NULL, scales_inferred = 0L)
  if (!length(kinds)) return(out)
  scope <- "cl.project_id = $1 AND cl.study_accession = $2 AND cl.experiment_accession = $3"
  par <- list(project, study, experiment)
  cl <- .tbl("curve_lookup"); f <- .tbl("calib_fit")
  best <- .calib_q(pool, sprintf(
    "SELECT f.curve_id, f.method, f.model_name FROM %s f JOIN %s cl ON cl.curve_id = f.curve_id
      WHERE f.is_best AND %s", f, cl, scope), params = par)
  if (!nrow(best)) return(out)
  params <- .calib_q(pool, sprintf(
    "SELECT p.curve_id, p.method, p.model_name, p.term, p.estimate::float8 AS estimate, p.q_med::float8 AS q_med
       FROM %s p JOIN %s f ON f.curve_id = p.curve_id AND f.method = p.method AND f.model_name = p.model_name AND f.is_best
       JOIN %s cl ON cl.curve_id = p.curve_id WHERE %s", .tbl("calib_param"), f, cl, scope), params = par)
  grid <- .calib_q(pool, sprintf(
    "SELECT g.curve_id, g.method, g.log10_concentration::float8 AS log10_concentration,
            g.predicted_response::float8 AS predicted_response, g.pcov::float8 AS pcov
       FROM %s g JOIN %s cl ON cl.curve_id = g.curve_id WHERE %s", .tbl("calib_grid"), cl, scope), params = par)
  diag <- .calib_q(pool, sprintf(
    "SELECT d.curve_id, d.method, d.lloq_log10::float8 AS lloq_log10, d.uloq_log10::float8 AS uloq_log10,
            d.inflect_x::float8 AS inflect_x, d.pcov_threshold::float8 AS pcov_threshold
       FROM %s d JOIN %s cl ON cl.curve_id = d.curve_id WHERE %s", .tbl("calib_diagnostics"), cl, scope), params = par)

  if ("control" %in% kinds) {
    # Masked wells are dropped here, not in SQL, so that the stage counts can say how many.
    ctl <- .calib_q(pool, sprintf(
      "SELECT cl.curve_id, c.well, c.sampleid, c.stype, c.source AS control_source,
              c.dilution::float8 AS dilution, c.antibody_mfi::float8 AS mfi,
              c.masked, cl.plateid, cl.plate, cl.antigen, cl.feature, cl.source, cl.wavelength
         FROM %s c JOIN %s cl ON %s
        WHERE %s", .tbl("xmap_control"), cl, .nk_join_on(BLK_NK_JOIN_COLS, cl = "cl", s = "c"), scope),
      params = par)
    tot <- tryCatch(.calib_q(pool, sprintf(
      "SELECT count(*)::int AS n, (count(*) FILTER (WHERE c.masked))::int AS n_masked,
              string_agg(DISTINCT concat_ws(' ', c.sampleid, '(stype', c.stype || ',', 'source', c.source || ',',
                                            'dilution', c.dilution::text || ')'), '; ') AS names
         FROM %s c WHERE c.project_id = $1 AND c.study_accession = $2 AND c.experiment_accession = $3",
      .tbl("xmap_control")), params = par), error = function(e) NULL)
    n_joined <- if (is.null(ctl)) 0L else nrow(ctl)
    if (n_joined && !include_masked && "masked" %in% names(ctl)) ctl <- ctl[!(ctl$masked %in% TRUE), , drop = FALSE]
    summ <- fetch_calib_standards_summary_scoped(pool, project, study, experiment)
    sc <- lin_scales(summ)
    out$scales_inferred <- sum(sc$inferred)
    out$control <- lin_backcalc_controls(ctl, best, params, grid, diag, sc)
    out$control_stages <- list(
      stored = if (is.data.frame(tot) && all(c("n", "n_masked") %in% names(tot)) && nrow(tot)) as.integer(tot$n[1]) else NA_integer_,
      masked = if (is.data.frame(tot) && "n_masked" %in% names(tot) && nrow(tot)) as.integer(tot$n_masked[1]) else NA_integer_,
      names = if (is.data.frame(tot) && "names" %in% names(tot) && nrow(tot)) as.character(tot$names[1]) else NA_character_,
      joined = n_joined, kept = if (is.null(ctl)) 0L else nrow(ctl),
      with_fit = if (is.null(out$control)) 0L else nrow(out$control),
      with_conc = if (is.null(out$control)) 0L else sum(is.finite(out$control$final_concentration)))
  }
  if ("standard" %in% kinds) {
    std <- .calib_q(pool, sprintf(
      "SELECT cs.curve_id, cs.method, cs.well, cs.dilution::float8 AS dilution, cs.concentration::float8 AS nominal,
              cs.response_model::float8 AS y_model, cl.plateid, cl.plate, cl.antigen, cl.feature, cl.source, cl.wavelength
         FROM %s cs JOIN %s cl ON cl.curve_id = cs.curve_id
        WHERE %s%s", .tbl("calib_standards"), cl, scope, if (include_masked) "" else " AND cs.included"), params = par)
    out$standard <- lin_backcalc_standards(std, best, params, grid, diag)
  }
  out$check <- tryCatch(lin_backcalc_check(best, params, grid, samples), error = function(e) NULL)
  out
}
