# =============================================================================
# linearity_report.R  --  replicate sets and linearity between estimates of the
#                         same sample, after concentration / pcov / gate class
#                         have been computed.  (QC > Replicate Linearity)
#
# A "point" is one back-calculated estimate: one row of calib_samples, joined to
# its curve (plate, antigen, feature, source, wavelength, method) and to the
# plate well it came from. A "set" is every point that belongs to the same
# biological sample:  antigen . feature . patientid . timeperiod  (the specimen
# id stands in when a sample has no patient id).
#
# Inside a set, points differ in some of: plate, well, dilution, method
# (frequentist / bayesian), standard-curve source, wavelength. How they differ
# says what a comparison means:
#     duplicate      same plate, same dilution, different wells
#     dilution       same plate, different dilutions
#     plate          different plates, same dilution
#     plate+dilution different plates AND dilutions
#     method         same well, frequentist vs bayesian curve
#     source         same well, different standard-curve source
#
# The user picks what goes on the x axis and the y axis (each a filter over
# method / source / plate / dilution / wavelength), which dimensions must match
# between the two, and gets one row per (x point, y point) pair. A regression of
# y on x (log10 by default) tests equivalence: slope 1 and intercept 0 is the
# 1:1 line. An optional transform maps y so the fitted line IS 1:1, and optional
# pcov thresholds on each axis choose which pairs contribute to the fit.
#
# Pure functions over plain data frames (testable without a DB); only
# lin_fetch() touches the pool.
#
#   raw  <- lin_fetch(pool, project, study, experiment)
#   pts  <- lin_prepare(raw)
#   sets <- lin_sets(pts)
#   pr   <- lin_pairs(pts, x = list(dilution = "100"), y = list(dilution = "400"),
#                     match = c("plate", "method", "source"))
#   reg  <- lin_regress(pr, scale = "log10", x_pcov_max = 20, y_pcov_max = 20,
#                       transform = TRUE)
#   rep  <- lin_report_build(list(scope = ..., pairs = pr, reg = reg, ...))
#   lin_report_text(rep) / lin_report_json(rep)
#
# PHASE 1 covers test samples (calib_samples). Standards, controls and blanks
# have no stored back-calculated concentration / pcov / gate class; phase 2 will
# back-calculate them and add rows with kind = "standard" | "control" | "blank".
# =============================================================================

LIN_GATES <- c("belowLLOQ", "inDynamicRange", "aboveULOQ", "unclassified")
LIN_GATE_LABEL <- c(belowLLOQ = "below LLOQ", inDynamicRange = "in range",
                    aboveULOQ = "above ULOQ", unclassified = "unclassified")
# Fill = the y-axis point's class, outline = the x-axis point's class.
LIN_GATE_COL <- c(belowLLOQ = "#3b6fb6", inDynamicRange = "#2f9e6e",
                  aboveULOQ = "#e08a2c", unclassified = "#9aa0a6")

LIN_THRESH <- list(
  min_pairs = 3L, few_pairs = 10L,
  r2_warn = 0.9,                 # R-squared below this is called weak
  bias_fold_warn = 1.25,         # |mean ratio| beyond this fold is a systematic bias
  gated_share_warn = 0.5,        # share of pairs removed by the pcov gate
  hetero_p = 0.01, cook_mult = 4, # Cook's distance > 4/n
  pcov_cap = 150,                # pcov is capped at this by the worker
  pcov_floor = 1,                # pcov below this (percent) is raised to it before weighting
  ccc_warn = 0.90, ccc_info = 0.95, ba_trend_p = 0.01
)

# The dimensions a pair can be selected on, or required to match.
LIN_DIMS <- c("method", "source", "plate", "dilution", "wavelength")
# "well" = same physical measurement (plate + well + dilution + specimen), used
# for method / source comparisons.
LIN_MATCH_CHOICES <- c(plate = "plate", dilution = "dilution", method = "method",
                       source = "source", wavelength = "wavelength",
                       well = "same well")

LIN_PRESETS <- list(
  duplicate = list(label = "Duplicates", detail = "Same plate, same dilution, different wells",
                   match = c("plate", "dilution", "method", "source", "wavelength"), differ = "well", vary = NULL),
  dilution  = list(label = "Dilution vs dilution", detail = "Same plate, two dilutions",
                   match = c("plate", "method", "source", "wavelength"), differ = "dilution", vary = "dilution"),
  plate     = list(label = "Plate vs plate", detail = "Two plates, same dilution",
                   match = c("dilution", "method", "source", "wavelength"), differ = "plate", vary = "plate"),
  platedil  = list(label = "Dilution across plates", detail = "Two dilutions on different plates",
                   match = c("method", "source", "wavelength"), differ = c("plate", "dilution"), vary = "dilution"),
  method    = list(label = "Frequentist vs Bayesian", detail = "Same wells, two fitting methods",
                   match = c("well", "source", "wavelength"), differ = "method", vary = "method"),
  source    = list(label = "Source vs source", detail = "Same wells, two standard-curve sources",
                   match = c("well", "method", "wavelength"), differ = "source", vary = "source"),
  custom    = list(label = "Custom", detail = "Choose each axis, what must match and what must differ",
                   match = character(), differ = character(), vary = NULL)
)

# Axis selections for a preset, from what the data holds: the dimension a preset
# varies gets its two most common values (lower dilution on x; frequentist on x).
lin_preset_sel <- function(pts, key) {
  p <- LIN_PRESETS[[key]]
  sel <- list(x = list(), y = list(), match = p$match, differ = p$differ)
  d <- p$vary
  if (!is.null(d) && nrow(pts)) {
    col <- pts[[d]]; col[is.na(col)] <- "(none)"
    u <- unique(data.frame(s = pts$set_id, v = col, stringsAsFactors = FALSE))
    top <- names(sort(table(u$v), decreasing = TRUE))
    top <- head(top, 2L)
    top <- if (d == "dilution") .lin_sort_dil(top) else if (d == "method") rev(sort(top)) else sort(top)
    if (length(top) >= 1L) sel$x[[d]] <- top[1]
    if (length(top) >= 2L) sel$y[[d]] <- top[2]
  }
  sel
}

# ---- small helpers ----------------------------------------------------------
.lin_or <- function(a, b) if (is.null(a) || !length(a) || all(is.na(a))) b else a
.lin_id <- function(x) {
  if (inherits(x, "integer64")) bit64::as.character.integer64(x) else as.character(x)
}
.lin_none <- function(x) { x <- as.character(x); x[!is.na(x) & x %in% c("__none__", "")] <- NA_character_; x }
.lin_fmt <- function(x, digits = 4) {
  if (is.null(x) || !length(x)) return("NA")
  vapply(x, function(v) if (is.na(v)) "NA" else format(signif(v, digits), scientific = FALSE, trim = TRUE), "")
}
.lin_p <- function(p) {
  if (is.na(p)) "NA" else if (p < 1e-4) "<0.0001" else format(round(p, 4), nsmall = 0, scientific = FALSE)
}
.lin_chr <- function(x) ifelse(is.na(x), "-", as.character(x))
# Specimen id for display: when it is just the well label, say so.
.lin_spec <- function(specimen, well) {
  s <- as.character(specimen); w <- as.character(well)
  same <- !is.na(s) & !is.na(w) & nzchar(s) & toupper(trimws(s)) == toupper(trimws(w))
  s[same] <- "(same as well)"
  s
}
.lin_sort_dil <- function(x) {
  x <- unique(x[!is.na(x)]); n <- suppressWarnings(as.numeric(x))
  x[order(is.na(n), n, x)]
}

# ---- fetch -------------------------------------------------------------------
# One row per calib_samples row, with its curve context and the plate well(s)
# it came from. xmap_sample is aggregated to one row per identity key BEFORE the
# join (a hash join; a per-row subquery is far slower on a large experiment).
# `source` is the standard-curve source from curve_lookup, not xmap_sample's.
lin_fetch <- function(pool, project, study, experiment) {
  sql <- sprintf(
    "SELECT cl.curve_id, cs.method, cs.sampleid, cs.patientid, cs.timeperiod, cs.dilution,
            cs.final_concentration, cs.predicted_concentration, cs.se_concentration,
            cs.pcov, cs.pcov_pass, cs.pcov_gate_class, cs.censor_class,
            cl.plateid, cl.plate, cl.antigen, cl.feature, cl.source, cl.wavelength,
            xs.well
       FROM %s cs
       JOIN %s cl ON cl.curve_id = cs.curve_id
       LEFT JOIN (SELECT plateid, antigen, feature, sampleid, patientid, timeperiod,
                         dilution::text AS dilution,
                         string_agg(DISTINCT well, ',' ORDER BY well) AS well
                    FROM %s
                   WHERE project_id = $1 AND study_accession = $2 AND experiment_accession = $3
                   GROUP BY 1, 2, 3, 4, 5, 6, 7) xs
              ON xs.plateid = cl.plateid AND xs.antigen = cl.antigen AND xs.feature = cl.feature
             AND xs.sampleid = cs.sampleid AND xs.patientid = cs.patientid
             AND xs.timeperiod = cs.timeperiod AND xs.dilution = cs.dilution
      WHERE cl.project_id = $1 AND cl.study_accession = $2 AND cl.experiment_accession = $3",
    .tbl("calib_samples"), .tbl("curve_lookup"), .tbl("xmap_sample"))
  .calib_q(pool, sql, params = list(project, study, experiment))
}

# ---- prepare -----------------------------------------------------------------
# Normalise a raw frame (lin_fetch() output, or a test frame) to the point table.
lin_prepare <- function(raw, kind = "sample") {
  if (is.null(raw) || !is.data.frame(raw) || !nrow(raw)) return(.lin_empty_points())
  g <- function(nm, default = NA_character_) if (nm %in% names(raw)) raw[[nm]] else rep(default, nrow(raw))
  num <- function(v) suppressWarnings(as.numeric(v))
  plateid <- .lin_none(g("plateid"))
  plate   <- .lin_none(g("plate")); plate <- ifelse(is.na(plate), plateid, plate)
  patient <- .lin_none(g("patientid"))
  spec    <- .lin_none(g("sampleid"))
  time    <- .lin_none(g("timeperiod"))
  dil     <- .lin_none(g("dilution"))
  # The worker leaves pcov_gate_class empty when LLOQ/ULOQ are undefined. Then a
  # point that passed the pcov threshold (pcov_pass) is taken as in range; one
  # that failed is "unclassified" (outside, but which side is unknown).
  gate <- as.character(g("pcov_gate_class"))
  missing_gate <- is.na(gate) | !(gate %in% LIN_GATES[1:3])
  pass <- g("pcov_pass", NA)
  gate[missing_gate] <- ifelse(!is.na(pass[missing_gate]) & as.logical(pass[missing_gate]),
                               "inDynamicRange", "unclassified")
  conc <- num(.lin_or(g("final_concentration", NA_real_), NA_real_))
  p <- data.frame(
    kind = kind,
    curve_id = .lin_id(g("curve_id")),
    method = as.character(g("method")),
    source = .lin_none(g("source")),
    plate = plate, plateid = plateid,
    well = .lin_none(g("well")),
    patientid = patient, timeperiod = time, specimen_id = spec,
    dilution = dil, dilution_num = num(dil),
    wavelength = .lin_none(g("wavelength")),
    antigen = as.character(g("antigen")), feature = .lin_none(g("feature")),
    conc = conc, pcov = num(g("pcov", NA_real_)),
    gate_class = gate, gate_inferred = missing_gate,
    censor_class = as.character(g("censor_class")),
    stringsAsFactors = FALSE)
  who <- ifelse(is.na(p$patientid), paste0("spec:", p$specimen_id), p$patientid)
  p$set_id  <- paste(p$antigen, .lin_chr(p$feature), who, .lin_chr(p$timeperiod), sep = " | ")
  p$meas_id <- paste(p$plate, .lin_chr(p$well), .lin_chr(p$dilution), .lin_chr(p$specimen_id),
                     .lin_chr(p$wavelength), sep = "/")
  p$pt_id   <- paste(p$meas_id, p$method, .lin_chr(p$source), p$curve_id, sep = "|")
  p
}

.lin_empty_points <- function() {
  data.frame(kind = character(), curve_id = character(), method = character(), source = character(),
             plate = character(), plateid = character(), well = character(), patientid = character(),
             timeperiod = character(), specimen_id = character(), dilution = character(),
             dilution_num = numeric(), wavelength = character(), antigen = character(),
             feature = character(), conc = numeric(), pcov = numeric(), gate_class = character(),
             gate_inferred = logical(), censor_class = character(), set_id = character(), meas_id = character(),
             pt_id = character(), stringsAsFactors = FALSE)
}

# ---- sets --------------------------------------------------------------------
LIN_SET_TYPES <- c(duplicate = "Duplicates (same plate, same dilution)",
                   dilution = "Dilutions on the same plate",
                   plate = "Plates at the same dilution",
                   platedil = "Plates and dilutions both differ",
                   method = "Frequentist and Bayesian estimates",
                   source = "More than one curve source")

# One row per set with a TRUE/FALSE per type. A set can have several types.
lin_sets <- function(pts) {
  empty <- data.frame(set_id = character(), antigen = character(), feature = character(),
                      patientid = character(), timeperiod = character(), n_points = integer(),
                      n_plates = integer(), n_dilutions = integer(),
                      duplicate = logical(), dilution = logical(), plate = logical(),
                      platedil = logical(), method = logical(), source = logical(),
                      stringsAsFactors = FALSE)
  if (is.null(pts) || !nrow(pts)) return(empty)
  ids <- unique(pts$set_id)
  first <- !duplicated(pts$set_id)
  out <- data.frame(set_id = pts$set_id[first], antigen = pts$antigen[first],
                    feature = pts$feature[first], patientid = pts$patientid[first],
                    timeperiod = pts$timeperiod[first], stringsAsFactors = FALSE)
  out$n_points <- as.integer(table(pts$set_id)[out$set_id])
  flag <- function(sets) out$set_id %in% sets
  m <- unique(pts[, c("set_id", "plate", "dilution", "meas_id")])
  cnt_pd <- ave(seq_len(nrow(m)), paste(m$set_id, m$plate, m$dilution, sep = "\r"), FUN = length)
  out$duplicate <- flag(m$set_id[cnt_pd >= 2L])
  u <- unique(m[, c("set_id", "plate", "dilution")])
  nd_by_plate <- ave(seq_len(nrow(u)), paste(u$set_id, u$plate, sep = "\r"), FUN = length)
  np_by_dil   <- ave(seq_len(nrow(u)), paste(u$set_id, u$dilution, sep = "\r"), FUN = length)
  out$dilution <- flag(u$set_id[nd_by_plate >= 2L])
  out$plate    <- flag(u$set_id[np_by_dil >= 2L])
  np <- tapply(u$plate, u$set_id, function(v) length(unique(v)))
  nd <- tapply(u$dilution, u$set_id, function(v) length(unique(v)))
  out$n_plates    <- as.integer(np[out$set_id])
  out$n_dilutions <- as.integer(nd[out$set_id])
  out$platedil <- out$n_plates >= 2L & out$n_dilutions >= 2L
  nm <- tapply(pts$method, paste(pts$set_id, pts$meas_id, sep = "\r"), function(v) length(unique(v)))
  ns <- tapply(pts$source, paste(pts$set_id, pts$meas_id, sep = "\r"), function(v) length(unique(v)))
  key_set <- sub("\r.*$", "", names(nm))
  out$method <- flag(key_set[nm >= 2L]); out$source <- flag(key_set[ns >= 2L])
  out
}

# Count sets per type, overall and per antigen: the "what is there to compare"
# report. n_pairs_max is not computed here (depends on the axis selection).
lin_set_summary <- function(sets) {
  types <- names(LIN_SET_TYPES)
  if (!nrow(sets))
    return(data.frame(type = types, label = unname(LIN_SET_TYPES), sets = 0L, stringsAsFactors = FALSE))
  data.frame(type = types, label = unname(LIN_SET_TYPES),
             sets = vapply(types, function(t) sum(sets[[t]]), integer(1)),
             stringsAsFactors = FALSE, row.names = NULL)
}
lin_set_summary_by_antigen <- function(sets) {
  if (!nrow(sets)) return(data.frame())
  types <- names(LIN_SET_TYPES)
  ag <- sort(unique(sets$antigen))
  m <- t(vapply(ag, function(a) vapply(types, function(t) sum(sets[[t]][sets$antigen == a]), integer(1)),
                integer(length(types))))
  colnames(m) <- types
  data.frame(antigen = ag, sets = as.integer(table(sets$antigen)[ag]), m,
             stringsAsFactors = FALSE, row.names = NULL, check.names = FALSE)
}

# ---- pairing -----------------------------------------------------------------
.lin_sel_empty <- function(sel) is.null(sel) || !length(sel) || all(vapply(sel, function(v) !length(v), logical(1)))

# sel: list(method=, source=, plate=, dilution=, wavelength=); an absent or
# empty entry means "any". Values compare as text; NA is matched by "(none)".
lin_filter <- function(pts, sel = list()) {
  keep <- rep(TRUE, nrow(pts))
  for (d in intersect(names(sel), LIN_DIMS)) {
    v <- sel[[d]]
    if (!length(v)) next
    col <- pts[[d]]; col[is.na(col)] <- "(none)"
    keep <- keep & col %in% v
  }
  pts[keep, , drop = FALSE]
}

LIN_PAIR_COLS <- c("pt_id", "meas_id", "curve_id", "method", "source", "plate", "plateid", "well",
                   "specimen_id", "dilution", "dilution_num", "wavelength", "conc", "pcov",
                   "gate_class", "censor_class")

# One row per (x point, y point) within a set. `match` lists dimensions that
# must be equal between the two, `differ` dimensions that must be unequal ("well"
# = same physical measurement, for either). When the
# x and y selections are identical each pair appears once (the ordering of such
# a pair is arbitrary). Capped at `max_pairs`; the attribute "truncated" says so.
lin_pairs <- function(pts, x = list(), y = list(), match = character(), differ = character(),
                      max_pairs = 200000L) {
  cols <- c("set_id", "antigen", "feature", "patientid", "timeperiod")
  empty <- function() {
    e <- data.frame(set_id = character(), antigen = character(), feature = character(),
                    patientid = character(), timeperiod = character(), stringsAsFactors = FALSE)
    for (s in c("x_", "y_")) for (cn in LIN_PAIR_COLS) e[[paste0(s, cn)]] <- if (cn %in% c("conc", "pcov", "dilution_num")) numeric() else character()
    e$pair_type <- character(); attr(e, "truncated") <- FALSE; e
  }
  if (is.null(pts) || !nrow(pts)) return(empty())
  X <- lin_filter(pts, x); Y <- lin_filter(pts, y)
  if (!nrow(X) || !nrow(Y)) return(empty())
  same_sel <- identical(lapply(x[sort(names(x))], sort), lapply(y[sort(names(y))], sort))
  xs <- X[, c(cols, LIN_PAIR_COLS)]
  ys <- Y[, c("set_id", LIN_PAIR_COLS)]
  names(ys)[-1] <- paste0("y_", names(ys)[-1]); names(xs)[-seq_along(cols)] <- paste0("x_", names(xs)[-seq_along(cols)])
  # Row counts per set first, so a huge cartesian product is refused before merge().
  est <- sum(as.numeric(table(xs$set_id)[intersect(names(table(xs$set_id)), names(table(ys$set_id)))]) *
             as.numeric(table(ys$set_id)[intersect(names(table(xs$set_id)), names(table(ys$set_id)))]))
  truncated <- FALSE
  if (is.finite(est) && est > 5 * max_pairs) { truncated <- TRUE }
  m <- merge(xs, ys, by = "set_id")
  if (!nrow(m)) return(empty())
  m <- m[m$x_pt_id != m$y_pt_id, , drop = FALSE]
  eq <- function(a, b) (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
  for (d in match) {
    m <- m[if (identical(d, "well")) eq(m$x_meas_id, m$y_meas_id) else eq(m[[paste0("x_", d)]], m[[paste0("y_", d)]]), , drop = FALSE]
    if (!nrow(m)) break
  }
  for (d in differ) {
    if (!nrow(m)) break
    m <- m[!(if (identical(d, "well")) eq(m$x_meas_id, m$y_meas_id) else eq(m[[paste0("x_", d)]], m[[paste0("y_", d)]])), , drop = FALSE]
  }
  if (same_sel && nrow(m)) m <- m[m$x_pt_id < m$y_pt_id, , drop = FALSE]
  if (nrow(m) > max_pairs) { m <- m[seq_len(max_pairs), , drop = FALSE]; truncated <- TRUE }
  if (!nrow(m)) return(empty())
  rownames(m) <- NULL
  m$pair_type <- lin_pair_type(m)
  attr(m, "truncated") <- truncated
  m
}

# What a pair compares, from how its two points differ.
lin_pair_type <- function(m) {
  eq <- function(a, b) (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
  same_meas <- eq(m$x_meas_id, m$y_meas_id)
  sp <- eq(m$x_plate, m$y_plate); sd <- eq(m$x_dilution, m$y_dilution)
  out <- rep("other", nrow(m))
  out[sp & sd] <- "duplicate"
  out[sp & !sd] <- "dilution"
  out[!sp & sd] <- "plate"
  out[!sp & !sd] <- "plate+dilution"
  out[same_meas & !eq(m$x_method, m$y_method)] <- "method"
  out[same_meas & eq(m$x_method, m$y_method) & !eq(m$x_source, m$y_source)] <- "source"
  out
}

# ---- error model, weighting, transform: the choices -----------------------------
LIN_ERROR_MODELS <- c(both = "Both estimates carry error (Deming / York)",
                      y = "Only y carries error (ordinary least squares)")
LIN_WEIGHTINGS <- c(full = "All pairs, equal weight",
                    gate = "Only pairs within the pcov gate",
                    weights = "All pairs, weighted by precision (1/pcov²)")
LIN_TRANSFORMS <- c(none = "None", y = "Map y onto x (1:1)",
                    harmonize = "Harmonize both to the latent concentration")

# SD of one estimate on the fitting scale, from its pcov. pcov = 100 ln(10)
# se(log10 conc), so on the log10 scale sd = pcov / (100 ln 10); on the natural
# scale it is a coefficient of variation: sd = conc * pcov / 100. pcov is floored
# (a pcov of 0 would give an infinite weight).
lin_sd <- function(conc, pcov, scale) {
  p <- pmax(pcov, LIN_THRESH$pcov_floor)
  if (identical(scale, "log10")) p / 100 / log(10) else abs(conc) * p / 100
}

# ---- fitting kernels: each returns c(a = intercept, b = slope) -------------------
.lin_ols_fast <- function(u, v) {
  mu <- mean(u); mv <- mean(v); sxx <- sum((u - mu)^2)
  b <- if (sxx > 0) sum((u - mu) * (v - mv)) / sxx else NA_real_
  c(a = mv - b * mu, b = b)
}
.lin_wls <- function(u, v, w) {
  sw <- sum(w); mu <- sum(w * u) / sw; mv <- sum(w * v) / sw
  sxx <- sum(w * (u - mu)^2)
  b <- if (sxx > 0) sum(w * (u - mu) * (v - mv)) / sxx else NA_real_
  c(a = mv - b * mu, b = b)
}
# Deming: delta = (error variance of y) / (error variance of x). delta = 1 is
# orthogonal regression. Swapping the axes (and inverting delta) inverts the line.
.lin_deming <- function(u, v, delta) {
  sxx <- stats::var(u); syy <- stats::var(v); sxy <- stats::cov(u, v)
  if (!is.finite(sxy) || sxy == 0) return(c(a = NA_real_, b = NA_real_))
  d <- syy - delta * sxx
  b <- (d + sqrt(d^2 + 4 * delta * sxy^2)) / (2 * sxy)
  c(a = mean(v) - b * mean(u), b = b)
}
# York (2004): straight line with an individual SD on every x and y (no
# correlation between the two errors). Iterates the weighted solution.
.lin_york <- function(u, v, sx, sy, tol = 1e-10, maxit = 100L) {
  b <- .lin_ols_fast(u, v)[["b"]]
  if (!is.finite(b)) return(c(a = NA_real_, b = NA_real_))
  for (i in seq_len(maxit)) {
    W <- 1 / (sy^2 + b^2 * sx^2)
    xb <- sum(W * u) / sum(W); yb <- sum(W * v) / sum(W)
    U <- u - xb; V <- v - yb
    beta <- W * (U * sy^2 + b * V * sx^2)
    bn <- sum(W * beta * V) / sum(W * beta * U)
    if (!is.finite(bn)) return(c(a = NA_real_, b = NA_real_))
    done <- abs(bn - b) < tol * max(1, abs(b)); b <- bn
    if (done) break
  }
  W <- 1 / (sy^2 + b^2 * sx^2)
  c(a = sum(W * v) / sum(W) - b * sum(W * u) / sum(W), b = b)
}
.lin_core <- function(u, v, error, weighting, delta, sx, sy) {
  if (identical(error, "y")) {
    if (identical(weighting, "weights")) return(.lin_wls(u, v, 1 / sy^2))
    return(.lin_ols_fast(u, v))
  }
  if (identical(weighting, "weights")) .lin_york(u, v, sx, sy) else .lin_deming(u, v, delta)
}

.lin_with_seed <- function(seed, expr) {
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had) old <- get(".Random.seed", envir = globalenv())
  on.exit(if (had) assign(".Random.seed", old, envir = globalenv())
          else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv()))
  set.seed(seed)
  expr
}

# ---- agreement metrics (CCC, Bland-Altman, correlation) ----------------------------
# u = x, v = y on the fitting scale. On the log10 scale the Bland-Altman
# difference v - u is log10(y/x).
.lin_ccc <- function(u, v) {
  n <- length(u); if (n < 2L) return(NA_real_)
  mu <- mean(u); mv <- mean(v)
  suu <- sum((u - mu)^2) / n; svv <- sum((v - mv)^2) / n; suv <- sum((u - mu) * (v - mv)) / n
  den <- suu + svv + (mu - mv)^2
  if (den > 0) 2 * suv / den else NA_real_
}
.lin_agree <- function(u, v, level = 0.95) {
  n <- length(u)
  r <- suppressWarnings(stats::cor(u, v))
  d <- v - u; m <- (u + v) / 2
  sd_d <- stats::sd(d); bias <- mean(d)
  tq <- stats::qt(1 - (1 - level) / 2, max(n - 1L, 1L))
  se_bias <- sd_d / sqrt(n)
  se_loa <- sqrt(sd_d^2 * (1 / n + 1.96^2 / (2 * max(n - 1L, 1L))))
  tr <- tryCatch(suppressWarnings(summary(stats::lm(d ~ m))$coefficients), error = function(e) NULL)
  sp <- tryCatch(suppressWarnings(stats::cor.test(u, v, method = "spearman", exact = FALSE)$estimate), error = function(e) NA_real_)
  list(n = n, pearson = r, r2 = r^2, spearman = unname(sp), ccc = .lin_ccc(u, v),
       bias = bias, sd_diff = sd_d, bias_lo = bias - tq * se_bias, bias_hi = bias + tq * se_bias,
       loa_lo = bias - 1.96 * sd_d, loa_hi = bias + 1.96 * sd_d,
       loa_lo_lo = bias - 1.96 * sd_d - tq * se_loa, loa_lo_hi = bias - 1.96 * sd_d + tq * se_loa,
       loa_hi_lo = bias + 1.96 * sd_d - tq * se_loa, loa_hi_hi = bias + 1.96 * sd_d + tq * se_loa,
       trend_slope = if (is.null(tr)) NA_real_ else unname(tr[2, 1]),
       trend_p = if (is.null(tr)) NA_real_ else unname(tr[2, 4]))
}

# ---- one fit with its uncertainty -------------------------------------------------
.lin_ols <- function(u, v, level, w = NULL) {
  fit <- if (is.null(w)) stats::lm(v ~ u) else stats::lm(v ~ u, weights = w)
  s <- summary(fit)
  co <- s$coefficients
  ci <- tryCatch(stats::confint(fit, level = level), error = function(e) matrix(NA_real_, 2, 2))
  df <- s$df[2]
  list(a = unname(co[1, 1]), b = unname(co[2, 1]),
       se_a = unname(co[1, 2]), se_b = unname(co[2, 2]),
       a_lo = ci[1, 1], a_hi = ci[1, 2], b_lo = ci[2, 1], b_hi = ci[2, 2],
       p_slope1 = if (df > 0 && is.finite(co[2, 2]) && co[2, 2] > 0) 2 * stats::pt(-abs((co[2, 1] - 1) / co[2, 2]), df) else NA_real_,
       p_int0   = if (df > 0 && is.finite(co[1, 2]) && co[1, 2] > 0) 2 * stats::pt(-abs(co[1, 1] / co[1, 2]), df) else NA_real_,
       r2 = s$r.squared, adj_r2 = s$adj.r.squared, rse = s$sigma, df = df, n = length(u), model = fit,
       ci_method = "t distribution")
}

# Bootstrap over pairs: draws of (a, b, ccc, bias). The error-in-variables
# fits (Deming, York) have no closed-form SE, so their CIs come from here; the
# CCC and Bland-Altman bias CIs always do. B = 0 skips it.
.lin_boot <- function(u, v, error, weighting, delta, sx, sy, B, seed, need_fit) {
  n <- length(u)
  if (B < 1L || n < 3L) return(NULL)
  .lin_with_seed(seed, {
    out <- matrix(NA_real_, B, 4L, dimnames = list(NULL, c("a", "b", "ccc", "bias")))
    for (i in seq_len(B)) {
      k <- sample.int(n, n, replace = TRUE)
      uu <- u[k]; vv <- v[k]
      if (need_fit) { ab <- .lin_core(uu, vv, error, weighting, delta, sx[k], sy[k]); out[i, 1:2] <- ab }
      out[i, 3] <- .lin_ccc(uu, vv); out[i, 4] <- mean(vv - uu)
    }
    out
  })
}

.lin_pct <- function(x, level) {
  x <- x[is.finite(x)]
  if (length(x) < 20L) return(c(NA_real_, NA_real_))
  unname(stats::quantile(x, c((1 - level) / 2, 1 - (1 - level) / 2), names = FALSE))
}

.lin_fit <- function(u, v, error, weighting, delta, sx, sy, level, B, seed) {
  n <- length(u)
  analytic <- identical(error, "y")
  draws <- .lin_boot(u, v, error, weighting, delta, sx, sy, B, seed, need_fit = !analytic)
  f <- if (analytic) {
    .lin_ols(u, v, level, w = if (identical(weighting, "weights")) 1 / sy^2 else NULL)
  } else {
    ab <- .lin_core(u, v, error, weighting, delta, sx, sy)
    a <- ab[["a"]]; b <- ab[["b"]]
    sea <- if (is.null(draws)) NA_real_ else stats::sd(draws[, "a"], na.rm = TRUE)
    seb <- if (is.null(draws)) NA_real_ else stats::sd(draws[, "b"], na.rm = TRUE)
    ca <- if (is.null(draws)) c(NA_real_, NA_real_) else .lin_pct(draws[, "a"], level)
    cb <- if (is.null(draws)) c(NA_real_, NA_real_) else .lin_pct(draws[, "b"], level)
    r <- suppressWarnings(stats::cor(u, v))
    list(a = a, b = b, se_a = sea, se_b = seb, a_lo = ca[1], a_hi = ca[2], b_lo = cb[1], b_hi = cb[2],
         p_slope1 = if (is.finite(seb) && seb > 0) 2 * stats::pnorm(-abs((b - 1) / seb)) else NA_real_,
         p_int0 = if (is.finite(sea) && sea > 0) 2 * stats::pnorm(-abs(a / sea)) else NA_real_,
         r2 = r^2, adj_r2 = NA_real_, rse = NA_real_, df = n - 2L, n = n, model = NULL, draws = draws[, c("a", "b"), drop = FALSE],
         ci_method = sprintf("bootstrap, %d resamples", B))
  }
  ag <- .lin_agree(u, v, level)
  if (!is.null(draws)) {
    cc <- .lin_pct(draws[, "ccc"], level)
    ag$ccc_lo <- cc[1]; ag$ccc_hi <- cc[2]
  } else { ag$ccc_lo <- NA_real_; ag$ccc_hi <- NA_real_ }
  f$agree <- ag
  f
}

.lin_band <- function(f, urange, level, scale) {
  g <- seq(urange[1], urange[2], length.out = 80)
  inv <- if (scale == "log10") function(z) 10^z else identity
  if (!is.null(f$model)) {
    pr <- stats::predict(f$model, newdata = data.frame(u = g), interval = "confidence", level = level)
    return(data.frame(x = inv(g), fit = inv(pr[, "fit"]), lwr = inv(pr[, "lwr"]), upr = inv(pr[, "upr"])))
  }
  fit <- f$a + f$b * g
  if (is.null(f$draws) || nrow(f$draws) < 20L) return(data.frame(x = inv(g), fit = inv(fit), lwr = inv(fit), upr = inv(fit)))
  M <- outer(f$draws[, "b"], g) + f$draws[, "a"]
  q <- apply(M, 2, stats::quantile, probs = c((1 - level) / 2, 1 - (1 - level) / 2), na.rm = TRUE, names = FALSE)
  data.frame(x = inv(g), fit = inv(fit), lwr = inv(pmin(q[1, ], fit)), upr = inv(pmax(q[2, ], fit)))
}

.lin_stats <- function(f, u, v) {
  n <- length(u)
  r <- v - (f$a + f$b * u)
  het <- tryCatch(suppressWarnings(stats::cor.test(abs(r), u, method = "spearman", exact = FALSE)), error = function(e) NULL)
  sh <- if (n >= 3L && n <= 5000L) tryCatch(stats::shapiro.test(r)$p.value, error = function(e) NA_real_) else NA_real_
  ck <- if (!is.null(f$model)) tryCatch(stats::cooks.distance(f$model), error = function(e) rep(NA_real_, n)) else rep(NA_real_, n)
  ag <- f$agree
  c(ag[c("pearson", "spearman", "ccc", "ccc_lo", "ccc_hi", "bias", "sd_diff", "bias_lo", "bias_hi", "loa_lo", "loa_hi",
         "loa_lo_lo", "loa_lo_hi", "loa_hi_lo", "loa_hi_hi", "trend_slope", "trend_p")],
    list(mean_diff = ag$bias, loa_lo_ = ag$loa_lo, hetero_rho = if (is.null(het)) NA_real_ else unname(het$estimate),
         hetero_p = if (is.null(het)) NA_real_ else het$p.value, shapiro_p = sh,
         n_influential = sum(ck > LIN_THRESH$cook_mult / n, na.rm = TRUE),
         max_cook = if (all(is.na(ck))) NA_real_ else suppressWarnings(max(ck, na.rm = TRUE))))
}

# ---- harmonization ------------------------------------------------------------------
# Both estimates are noisy readings of one latent concentration z:
#   x = alpha1 + beta1 z,  y = alpha2 + beta2 z,  beta1 beta2 = 1, alpha1 + alpha2 = 0
# (the correction is split equally between the two axes). With the fitted line
# y = a + b x on the fitting scale (b > 0):
#   z_x = sqrt(b) (x + a/(1+b)),   z_y = (y - a/(1+b)) / sqrt(b)
# so a pair on the line has z_x = z_y, and swapping the axes gives the same answer.
.lin_harm <- function(pr, scale, a, b) {
  tf <- if (identical(scale, "log10")) function(z) suppressWarnings(log10(ifelse(z > 0, z, NA_real_))) else identity
  c0 <- a / (1 + b); sb <- sqrt(b)
  ux <- tf(pr$x_conc); uy <- tf(pr$y_conc)
  sx <- lin_sd(pr$x_conc, pr$x_pcov, scale); sy <- lin_sd(pr$y_conc, pr$y_pcov, scale)
  list(zx = sb * (ux + c0), zy = (uy - c0) / sb, vx = b * sx^2, vy = sy^2 / b, c0 = c0, sb = sb)
}

# One consensus estimate per sample (set): the inverse-variance mean of every
# distinct point in the set after harmonization. Variance of a harmonized point
# is b sd_x^2 (x side) or sd_y^2 / b (y side). Uses every valid point with a
# known pcov (the weighting stands in for the pcov gate). NULL unless the fit is
# an errors-in-variables fit with b > 0.
lin_consensus <- function(pr, reg) {
  if (!isTRUE(reg$ok) || !identical(reg$error, "both") || !is.finite(reg$fit$b) || reg$fit$b <= 0 || !nrow(pr)) return(NULL)
  h <- .lin_harm(pr, reg$scale, reg$fit$a, reg$fit$b)
  pts <- rbind(data.frame(set_id = pr$set_id, pt = pr$x_pt_id, z = h$zx, v = h$vx, stringsAsFactors = FALSE),
               data.frame(set_id = pr$set_id, pt = pr$y_pt_id, z = h$zy, v = h$vy, stringsAsFactors = FALSE))
  pts <- pts[is.finite(pts$z) & is.finite(pts$v) & pts$v > 0, , drop = FALSE]
  pts <- pts[!duplicated(pts$pt), , drop = FALSE]
  if (!nrow(pts)) return(NULL)
  w <- 1 / pts$v
  sw <- rowsum(w, pts$set_id); swz <- rowsum(w * pts$z, pts$set_id); swz2 <- rowsum(w * pts$z^2, pts$set_id)
  k <- as.integer(table(pts$set_id)[rownames(sw)])
  zhat <- swz[, 1] / sw[, 1]; se <- 1 / sqrt(sw[, 1])
  q <- pmax(swz2[, 1] - swz[, 1]^2 / sw[, 1], 0)
  inv <- if (identical(reg$scale, "log10")) function(z) 10^z else identity
  meta <- pr[!duplicated(pr$set_id), c("set_id", "antigen", "feature", "patientid", "timeperiod"), drop = FALSE]
  out <- data.frame(set_id = rownames(sw), n_points = k, consensus = inv(zhat), z = zhat, se_z = se,
                    lo = inv(zhat - 1.96 * se), hi = inv(zhat + 1.96 * se),
                    q = q, q_df = k - 1L, q_ratio = ifelse(k > 1L, q / (k - 1L), NA_real_),
                    stringsAsFactors = FALSE, row.names = NULL)
  out <- merge(meta, out, by = "set_id", sort = FALSE)
  out[order(out$antigen, out$patientid, out$timeperiod), , drop = FALSE]
}

# ---- replicate precision (duplicates are exchangeable: no natural x) --------------------
# Within-pair SD, geometric CV and ICC(1) over the duplicate pairs' distinct
# points, on the fitting scale. NULL without duplicate pairs.
lin_replicate_precision <- function(pr, scale = "log10") {
  d <- pr[pr$pair_type == "duplicate" & is.finite(pr$x_conc) & is.finite(pr$y_conc), , drop = FALSE]
  if (identical(scale, "log10")) d <- d[d$x_conc > 0 & d$y_conc > 0, , drop = FALSE]
  if (nrow(d) < 3L) return(NULL)
  tf <- if (identical(scale, "log10")) log10 else identity
  u <- tf(d$x_conc); v <- tf(d$y_conc)
  sw <- stats::sd(v - u) / sqrt(2)
  cv <- if (identical(scale, "log10")) sqrt(exp((sw * log(10))^2) - 1) else NA_real_
  pts <- rbind(data.frame(s = d$set_id, p = d$x_pt_id, z = u, stringsAsFactors = FALSE),
               data.frame(s = d$set_id, p = d$y_pt_id, z = v, stringsAsFactors = FALSE))
  pts <- pts[!duplicated(pts$p), , drop = FALSE]
  k <- table(pts$s); pts <- pts[pts$s %in% names(k)[k >= 2L], , drop = FALSE]
  k <- table(pts$s); g <- length(k); N <- nrow(pts)
  icc <- NA_real_
  if (g >= 2L && N > g) {
    gm <- mean(pts$z); sm <- tapply(pts$z, pts$s, mean)
    ssb <- sum(k * (sm[names(k)] - gm)^2); ssw <- sum((pts$z - sm[pts$s])^2)
    msb <- ssb / (g - 1L); msw <- ssw / (N - g)
    k0 <- (N - sum(k^2) / N) / (g - 1L)
    icc <- (msb - msw) / (msb + (k0 - 1) * msw)
  }
  list(n_pairs = nrow(d), n_sets = g, sd_within = sw, cv = cv, icc = icc)
}

# ---- the regression -----------------------------------------------------------------------
# Fit y on x over the pairs that pass validity and the chosen weighting.
#   scale      "log10": log10(y) ~ log10(x), needs x,y > 0;  "natural": y ~ x
#   error      "y": only y carries error (ordinary / weighted least squares)
#              "both": both do (Deming; York when weighting = "weights")
#   weighting  "full" all valid pairs; "gate" only pairs with pcov within
#              x_pcov_max / y_pcov_max (NA = no limit on that axis; an unknown
#              pcov fails an active gate); "weights" every pair with a known pcov,
#              weighted by 1/sd^2; "auto" = "gate" when a threshold is given
#   delta      Deming error-variance ratio var(ey)/var(ex): "auto" (from pcov) or a number
#   transform  "none"; "y" maps y so the fit becomes 1:1; "harmonize" maps both x
#              and y to the latent concentration (needs error = "both", b > 0)
# Returns list(ok, ...): `inc` marks the pairs used; `x_plot`/`y_plot` are what to draw.
lin_regress <- function(pr, scale = c("log10", "natural"), error = c("y", "both"),
                        weighting = c("auto", "full", "gate", "weights"), delta = "auto",
                        x_pcov_max = NA_real_, y_pcov_max = NA_real_, level = 0.95,
                        transform = "none", B = 200L, seed = 1L) {
  scale <- match.arg(scale); error <- match.arg(error); weighting <- match.arg(weighting)
  if (isTRUE(transform)) transform <- "y" else if (isFALSE(transform)) transform <- "none"
  transform <- match.arg(transform, c("none", "y", "harmonize"))
  if (identical(weighting, "auto")) weighting <- if (!is.na(x_pcov_max) || !is.na(y_pcov_max)) "gate" else "full"
  n_all <- nrow(pr)
  res <- list(ok = FALSE, scale = scale, error = error, weighting = weighting, level = level, n_pairs = n_all,
              x_pcov_max = x_pcov_max, y_pcov_max = y_pcov_max, transform = transform, B = B,
              n_gated = 0L, n_nopcov = 0L, n_invalid = 0L, n = 0L)
  if (!n_all) { res$reason <- "No pairs to fit."; res$inc <- logical(); res$x_plot <- numeric(); res$y_plot <- numeric(); return(res) }
  x <- pr$x_conc; y <- pr$y_conc
  valid <- is.finite(x) & is.finite(y)
  if (scale == "log10") valid <- valid & x > 0 & y > 0
  res$n_invalid <- sum(!valid)
  sx_all <- lin_sd(x, pr$x_pcov, scale); sy_all <- lin_sd(y, pr$y_pcov, scale)
  gate_ok <- rep(TRUE, n_all)
  if (identical(weighting, "gate")) {
    if (!is.na(x_pcov_max)) gate_ok <- gate_ok & !is.na(pr$x_pcov) & pr$x_pcov <= x_pcov_max
    if (!is.na(y_pcov_max)) gate_ok <- gate_ok & !is.na(pr$y_pcov) & pr$y_pcov <= y_pcov_max
    res$n_gated <- sum(valid & !gate_ok)
  }
  sd_ok <- is.finite(sx_all) & is.finite(sy_all) & sx_all > 0 & sy_all > 0
  if (identical(weighting, "weights")) res$n_nopcov <- sum(valid & !sd_ok)
  inc <- valid & gate_ok & (!identical(weighting, "weights") | sd_ok)
  res$inc <- inc; res$n <- sum(inc)
  res$x_plot <- x; res$y_plot <- y
  if (res$n < LIN_THRESH$min_pairs) {
    res$reason <- sprintf("Only %d pair(s) are usable (need at least %d).", res$n, LIN_THRESH$min_pairs)
    return(res)
  }
  tf <- if (scale == "log10") log10 else identity
  inv <- if (scale == "log10") function(z) 10^z else identity
  u <- tf(x[inc]); v <- tf(y[inc]); sx <- sx_all[inc]; sy <- sy_all[inc]
  if (length(unique(u)) < 2L) { res$reason <- "All x values are identical; no line can be fitted."; return(res) }
  if (identical(error, "both") && !identical(weighting, "weights")) {
    d_auto <- if (any(is.finite(sx) & is.finite(sy))) mean(sy^2, na.rm = TRUE) / mean(sx^2, na.rm = TRUE) else 1
    if (is.numeric(delta) && is.finite(delta) && delta > 0) { res$delta <- delta; res$delta_source <- "set by the user" }
    else { res$delta <- if (is.finite(d_auto) && d_auto > 0) d_auto else 1
           res$delta_source <- if (is.finite(d_auto)) "from the pcov of each axis: mean(sd_y^2) / mean(sd_x^2)" else "pcov unknown; equal errors assumed" }
  } else res$delta <- NA_real_
  delta_use <- if (is.na(res$delta)) 1 else res$delta
  nb <- if (res$n > 20000L) min(B, 100L) else B
  fit <- .lin_fit(u, v, error, weighting, delta_use, sx, sy, level, nb, seed)
  if (!is.finite(fit$a) || !is.finite(fit$b)) { res$reason <- "The fit did not produce a finite line."; return(res) }
  res$fit <- fit; res$ok <- TRUE; res$fit_plot <- fit
  res$stats <- .lin_stats(fit, u, v)
  res$u_range <- range(u)
  a <- fit$a; b <- fit$b
  refit <- function(uu, vv, ssx, ssy, dl) .lin_fit(uu, vv, error, weighting, dl, ssx, ssy, level, nb, seed)
  if (identical(transform, "harmonize") && !(identical(error, "both") && b > 0)) {
    res$transform_note <- "Harmonization needs both estimates to carry error and a positive slope; no transform applied."
    transform <- res$transform <- "none"
  }
  if (identical(transform, "y") && abs(b) > 1e-12) {
    res$tf <- list(kind = "y", a = a, b = b, scale = scale,
                   formula = if (scale == "log10") "y' = 10^((log10(y) - a) / b)" else "y' = (y - a) / b")
    yt <- (tf(ifelse(valid, y, NA_real_)) - a) / b
    res$y_plot <- ifelse(valid, inv(yt), NA_real_)
    res$refit <- refit(u, (v - a) / b, sx, sy / b, delta_use / b^2)
    res$fit_plot <- res$refit
    res$u_range <- range(u)
  } else if (identical(transform, "harmonize")) {
    h <- .lin_harm(pr, scale, a, b)
    res$tf <- list(kind = "harmonize", a = a, b = b, scale = scale, alpha2 = h$c0, alpha1 = -h$c0, beta2 = h$sb, beta1 = 1 / h$sb,
                   formula = "z_x = sqrt(b) (x + a/(1+b));  z_y = (y - a/(1+b)) / sqrt(b)")
    zx <- h$zx; zy <- h$zy
    res$x_plot <- ifelse(valid, inv(zx), NA_real_); res$y_plot <- ifelse(valid, inv(zy), NA_real_)
    res$refit <- refit(zx[inc], zy[inc], sqrt(b) * sx, sy / sqrt(b), delta_use / b^2)
    res$fit_plot <- res$refit
    res$u_range <- range(zx[inc])
  }
  if (identical(error, "both") && b > 0) {
    h <- .lin_harm(pr, scale, a, b)
    sdz <- (h$zy - h$zx) / sqrt(h$vx + h$vy)
    sdz <- sdz[inc & is.finite(sdz)]
    res$harm <- list(c0 = h$c0, std_diff_sd = if (length(sdz) > 1L) stats::sd(sdz) else NA_real_, n = length(sdz))
  }
  res$band <- .lin_band(res$fit_plot, res$u_range, level, scale)
  res$replicate <- lin_replicate_precision(pr, scale)
  res
}

# The two weightings other than the primary, run with the same error model and
# scale; and the primary itself, so the table reads as one comparison.
lin_sensitivity <- function(pr, primary, x_pcov_max = NA_real_, y_pcov_max = NA_real_, B = 100L, seed = 1L) {
  xg <- if (is.na(x_pcov_max)) 20 else x_pcov_max; yg <- if (is.na(y_pcov_max)) 20 else y_pcov_max
  rows <- lapply(names(LIN_WEIGHTINGS), function(w) {
    r <- if (identical(w, primary$weighting)) primary else
      lin_regress(pr, scale = primary$scale, error = primary$error, weighting = w,
                  delta = if (is.null(primary$delta) || is.na(primary$delta)) "auto" else "auto",
                  x_pcov_max = xg, y_pcov_max = yg, level = primary$level, transform = "none", B = B, seed = seed)
    if (!isTRUE(r$ok)) return(data.frame(weighting = w, primary = identical(w, primary$weighting), n = r$n, a = NA, a_lo = NA, a_hi = NA,
                                         b = NA, b_lo = NA, b_hi = NA, ccc = NA, ccc_lo = NA, ccc_hi = NA, bias = NA, loa_lo = NA, loa_hi = NA,
                                         r = NA, r2 = NA, stringsAsFactors = FALSE))
    f <- r$fit; s <- r$stats
    data.frame(weighting = w, primary = identical(w, primary$weighting), n = r$n, a = f$a, a_lo = f$a_lo, a_hi = f$a_hi,
               b = f$b, b_lo = f$b_lo, b_hi = f$b_hi, ccc = s$ccc, ccc_lo = s$ccc_lo, ccc_hi = s$ccc_hi,
               bias = s$bias, loa_lo = s$loa_lo, loa_hi = s$loa_hi, r = s$pearson, r2 = f$r2, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

lin_transform_y <- function(y, a, b, scale = c("log10", "natural")) {
  scale <- match.arg(scale)
  if (scale == "log10") ifelse(y > 0, 10^((log10(pmax(y, .Machine$double.xmin)) - a) / b), NA_real_) else (y - a) / b
}

lin_equation <- function(f, scale, yname = "y", xname = "x") {
  sgn <- function(v) if (is.na(v)) "NA" else if (v < 0) paste0("- ", .lin_fmt(abs(v))) else paste0("+ ", .lin_fmt(v))
  if (scale == "log10")
    sprintf("log10(%s) = %s %s · log10(%s)   [%s = %s · %s^%s]", yname, .lin_fmt(f$a), sgn(f$b), xname,
            yname, .lin_fmt(10^f$a), xname, .lin_fmt(f$b))
  else
    sprintf("%s = %s %s · %s", yname, .lin_fmt(f$a), sgn(f$b), xname)
}

# CCC strength bands (McBride 2005).
lin_ccc_label <- function(ccc) {
  if (is.na(ccc)) "NA" else if (ccc > 0.99) "almost perfect" else if (ccc > 0.95) "substantial" else if (ccc > 0.90) "moderate" else "poor"
}

# ---- assessment ----------------------------------------------------------------------------
lin_assess <- function(reg, ctx = list()) {
  out <- list()
  add <- function(code, sev, msg, evidence = NULL, action = NULL)
    out[[length(out) + 1L]] <<- list(code = code, severity = sev, message = msg, evidence = evidence, action = action)
  if (!isTRUE(reg$ok)) {
    add("no-fit", "error", .lin_or(reg$reason, "No regression could be fitted."))
    return(out)
  }
  f <- reg$fit; s <- reg$stats; scale <- reg$scale; n <- reg$n
  lg <- identical(scale, "log10")
  if (n < LIN_THRESH$few_pairs)
    add("few-pairs", "warn", sprintf("Only %d pairs are in the fit; the line is poorly determined.", n))
  if (reg$n_gated > 0 && reg$n_gated / reg$n_pairs >= LIN_THRESH$gated_share_warn)
    add("pcov-gate-heavy", "warn", sprintf("The pcov gate removed %d of %d pairs.", reg$n_gated, reg$n_pairs),
        action = "The gate trims the curve extremes, where non-linearity and hook effects show. Compare with the full-set and precision-weighted fits.")
  if (reg$n_nopcov > 0)
    add("no-pcov", "info", sprintf("%d pair(s) have no pcov and were left out of the precision-weighted fit.", reg$n_nopcov))
  if (reg$n_invalid > 0)
    add("invalid-pairs", "info", sprintf("%d pair(s) have a non-finite%s concentration and were left out.", reg$n_invalid, if (lg) " or non-positive" else ""))
  ci_ok <- is.finite(f$b_lo) && is.finite(f$b_hi)
  slope_off <- ci_ok && (f$b_lo > 1 || f$b_hi < 1)
  if (slope_off) {
    add("slope-not-1", "warn",
        if (lg) sprintf("Slope differs from 1: the fold difference between the estimates changes with concentration (y is proportional to x^%s).", .lin_fmt(f$b, 3))
        else "Slope differs from 1: the estimates differ in proportion to concentration.",
        sprintf("slope %s, %d%% CI %s to %s, p(slope=1) %s", .lin_fmt(f$b), round(100 * reg$level), .lin_fmt(f$b_lo), .lin_fmt(f$b_hi), .lin_p(f$p_slope1)),
        "Across dilutions this is dilutional non-linearity (matrix or hook effect); between methods, a scale disagreement.")
  }
  if (is.finite(f$a_lo) && is.finite(f$a_hi) && (f$a_lo > 0 || f$a_hi < 0)) {
    if (lg && !slope_off)
      add("constant-fold", "warn", sprintf("A constant fold difference: y is %s-fold x at every concentration.", .lin_fmt(10^f$a, 3)),
          sprintf("intercept %s (CI %s to %s)", .lin_fmt(f$a), .lin_fmt(f$a_lo), .lin_fmt(f$a_hi)))
    else if (!lg)
      add("intercept-not-0", "info", "Intercept differs from 0 (a constant offset).",
          sprintf("intercept %s, CI %s to %s", .lin_fmt(f$a), .lin_fmt(f$a_lo), .lin_fmt(f$a_hi)))
  }
  if (is.finite(s$ccc)) {
    lab <- lin_ccc_label(s$ccc)
    sev <- if (s$ccc <= LIN_THRESH$ccc_warn) "warn" else if (s$ccc <= LIN_THRESH$ccc_info) "info" else NULL
    if (!is.null(sev))
      add("ccc-low", sev, sprintf("Agreement is %s (Lin's concordance coefficient %s).", lab, .lin_fmt(s$ccc, 3)),
          sprintf("CCC %s, %d%% CI %s to %s; Pearson r %s", .lin_fmt(s$ccc, 3), round(100 * reg$level), .lin_fmt(s$ccc_lo, 3), .lin_fmt(s$ccc_hi, 3), .lin_fmt(s$pearson, 3)),
          "CCC measures agreement, not just correlation: it falls with scatter, a shifted mean and a changed spread.")
  }
  if (lg) {
    fold <- 10^abs(s$bias)
    if (fold >= LIN_THRESH$bias_fold_warn)
      add("systematic-bias", "warn", sprintf("On average y is %s-fold %s x.", .lin_fmt(fold, 3), if (s$bias > 0) "higher than" else "lower than"),
          sprintf("Bland-Altman bias (mean log10 y/x) %s; limits of agreement %s-fold to %s-fold", .lin_fmt(s$bias), .lin_fmt(10^s$loa_lo, 3), .lin_fmt(10^s$loa_hi, 3)))
  }
  if (is.finite(s$trend_p) && s$trend_p < LIN_THRESH$ba_trend_p)
    add("ba-trend", "warn", "The difference between the estimates depends on the concentration (Bland-Altman trend).",
        sprintf("difference vs mean: slope %s, p %s", .lin_fmt(s$trend_slope), .lin_p(s$trend_p)),
        "A proportional bias, which the slope of the regression also reflects.")
  if (identical(reg$error, "y") && is.finite(s$pearson) && is.finite(f$b) && f$b < s$pearson - 0.05 && !isTRUE(slope_off))
    add("ols-attenuation", "info", "The ordinary-regression slope is below r, as noise in x alone would produce.",
        sprintf("OLS slope %s, correlation r %s", .lin_fmt(f$b), .lin_fmt(s$pearson)),
        "With equal noise on both axes and a true slope of 1 the expected OLS slope is r. Use the errors-in-variables fit for the slope.")
  if (identical(reg$error, "y") && is.finite(s$pearson) && is.finite(f$b) && f$b < s$pearson - 0.1 && slope_off)
    add("ols-below-r", "warn", "The slope is well below r: more than noise explains.", sprintf("OLS slope %s, r %s", .lin_fmt(f$b), .lin_fmt(s$pearson)))
  if (!is.na(s$hetero_p) && s$hetero_p < LIN_THRESH$hetero_p)
    add("heteroscedastic", "info", "Residual spread changes along the x axis.",
        sprintf("|residual| vs x: Spearman rho %s, p %s", .lin_fmt(s$hetero_rho), .lin_p(s$hetero_p)),
        "Precision weights suit this better than equal weights.")
  if (is.finite(s$n_influential) && s$n_influential > 0)
    add("influential", "info", sprintf("%d pair(s) have a large influence on the line (Cook's distance > 4/n).", s$n_influential),
        sprintf("max Cook's distance %s", .lin_fmt(s$max_cook)))
  if (!is.null(ctx$off_range_share) && is.finite(ctx$off_range_share) && ctx$off_range_share > 0.25)
    add("off-range", "info", sprintf("%s of the pairs have a point outside the dynamic range.", sprintf("%.0f%%", 100 * ctx$off_range_share)),
        action = "Colour shows which. Compare the three weightings in the sensitivity table.")
  if (!is.null(ctx$sens) && nrow(ctx$sens) == 3L && all(is.finite(ctx$sens$b))) {
    rng <- diff(range(ctx$sens$b))
    if (rng > 0.1)
      add("weighting-sensitive", "warn", "The slope depends on how pairs are weighted.",
          paste(sprintf("%s: %s", names(LIN_WEIGHTINGS)[match(ctx$sens$weighting, names(LIN_WEIGHTINGS))], .lin_fmt(ctx$sens$b, 3)), collapse = "; "),
          "Report the primary fit with this sensitivity range; the extremes of the curve probably drive the difference.")
  }
  if (!is.null(reg$harm) && is.finite(reg$harm$std_diff_sd) && reg$harm$std_diff_sd > 1.5)
    add("pcov-understates", "warn", "Pairs disagree by more than their pcov allows.",
        sprintf("SD of standardized differences %s (about 1 if pcov is the whole error)", .lin_fmt(reg$harm$std_diff_sd, 3)),
        "Another source of error (plate or dilution effects) is present; consensus intervals are too narrow.")
  if (identical(reg$transform, "harmonize") && !is.null(ctx$pair_types) && any(c("dilution", "plate+dilution") %in% ctx$pair_types))
    add("harmonize-dilutions", "warn", "Harmonizing dilutions maps a possible non-linearity onto one scale instead of exposing it.",
        action = "Report the slope as dilutional non-linearity, and prefer estimates from dilutions in the linear range.")
  if (!is.null(reg$transform_note)) add("transform-skipped", "info", reg$transform_note)
  if (!identical(reg$transform, "none") && !is.null(reg$refit))
    add("transform-applied", "info",
        if (identical(reg$transform, "harmonize")) "Both sets are harmonized to the latent concentration; the plotted line is the refit."
        else "A 1:1 transform of y is applied; the plotted line is the refit.",
        sprintf("original: a %s, b %s;  refit: a %s, b %s", .lin_fmt(f$a), .lin_fmt(f$b), .lin_fmt(reg$refit$a), .lin_fmt(reg$refit$b)))
  out
}

lin_severity <- function(findings) {
  r <- vapply(findings, function(f) match(f$severity, c("info", "warn", "error"), nomatch = 0L), integer(1))
  if (!length(r)) "ok" else c("ok", "info", "warn", "error")[max(r) + 1L]
}

# ---- report --------------------------------------------------------------------------------------
# ctx: list(scope, preset, x, y, match, differ, filters, sets, pairs, reg,
#           sens (lin_sensitivity), consensus (lin_consensus), n_gate_inferred)
lin_report_build <- function(ctx) {
  pr <- ctx$pairs; reg <- ctx$reg
  off <- if (nrow(pr)) mean(pr$x_gate_class != "inDynamicRange" | pr$y_gate_class != "inDynamicRange") else NA_real_
  findings <- lin_assess(reg, list(off_range_share = off, sens = ctx$sens,
                                   pair_types = if (nrow(pr)) unique(pr$pair_type) else character()))
  combos <- NULL
  if (nrow(pr)) {
    tb <- as.data.frame(table(x = pr$x_gate_class, y = pr$y_gate_class), stringsAsFactors = FALSE)
    combos <- tb[tb$Freq > 0, , drop = FALSE]
  }
  cons <- ctx$consensus
  list(generated = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
       scope = ctx$scope, preset = ctx$preset, x = ctx$x, y = ctx$y, match = ctx$match, differ = ctx$differ,
       n_gate_inferred = ctx$n_gate_inferred,
       filters = ctx$filters,
       sets = lin_set_summary(ctx$sets), n_sets = nrow(ctx$sets),
       n_pairs = nrow(pr),
       pair_types = if (nrow(pr)) as.list(table(pr$pair_type)) else list(),
       gate_combos = combos, reg = reg, sens = ctx$sens,
       consensus = if (is.null(cons)) NULL else list(
         n_samples = nrow(cons), n_multi = sum(cons$n_points > 1L),
         median_se_z = stats::median(cons$se_z), median_q_ratio = stats::median(cons$q_ratio, na.rm = TRUE)),
       findings = findings, severity = lin_severity(findings))
}

.lin_sel_text <- function(sel) {
  if (.lin_sel_empty(sel)) return("any")
  paste(vapply(names(sel), function(n) if (length(sel[[n]])) sprintf("%s = %s", n, paste(sel[[n]], collapse = ", ")) else NA_character_, ""),
        collapse = "; ") |> (\(z) gsub("(; )?NA", "", z))()
}

lin_report_text <- function(rep) {
  L <- character()
  w <- function(...) L <<- c(L, sprintf(...))
  reg <- rep$reg; sc <- rep$scope
  w("# Replicate linearity report")
  w("")
  w("Generated: %s", rep$generated)
  w("")
  w("## Scope")
  w("- project_id: %s", .lin_or(sc$project_id, "NA"))
  w("- study: %s", .lin_or(sc$study, "NA"))
  w("- experiment: %s", .lin_or(sc$experiment, "NA"))
  f <- rep$filters
  w("- antigen: %s; feature: %s", .lin_or(f$antigen, "all"), .lin_or(f$feature, "all"))
  w("- timeperiod: %s; patient filter: %s", if (length(f$timeperiod)) paste(f$timeperiod, collapse = ", ") else "all",
    if (nzchar(.lin_or(f$patient, ""))) f$patient else "none")
  w("")
  w("## Set definition")
  w("A set is every estimate of the same sample (antigen, feature, patientid, timeperiod).")
  w("- comparison: %s", .lin_or(rep$preset, "custom"))
  w("- x axis points: %s", .lin_sel_text(rep$x))
  w("- y axis points: %s", .lin_sel_text(rep$y))
  w("- a pair must match on: %s", if (length(rep$match)) paste(rep$match, collapse = ", ") else "nothing (every x point is paired with every y point in its set)")
  w("- a pair must differ in: %s", if (length(rep$differ)) paste(rep$differ, collapse = ", ") else "nothing")
  if (isTRUE(rep$n_gate_inferred > 0)) w("- gate class: %d point(s) had no stored class and were set from pcov_pass (in range if it passed, otherwise unclassified)", rep$n_gate_inferred)
  w("- sets in scope: %d; pairs formed: %d", rep$n_sets, rep$n_pairs)
  if (length(rep$pair_types)) w("- pair types: %s", paste(sprintf("%s %d", names(rep$pair_types), unlist(rep$pair_types)), collapse = "; "))
  if (!is.null(rep$gate_combos) && nrow(rep$gate_combos)) {
    w("")
    w("Gate class of the two points in each pair (x / y):")
    for (i in seq_len(nrow(rep$gate_combos)))
      w("- x %s / y %s: %d", LIN_GATE_LABEL[[rep$gate_combos$x[i]]], LIN_GATE_LABEL[[rep$gate_combos$y[i]]], rep$gate_combos$Freq[i])
  }
  w("")
  w("## Regression model")
  w("- fitted on: %s", if (reg$scale == "log10") "log10 of both axes" else "natural concentration")
  w("- error model: %s", LIN_ERROR_MODELS[[reg$error]])
  w("- weighting (primary): %s", LIN_WEIGHTINGS[[reg$weighting]])
  if (identical(reg$weighting, "gate")) w("- pcov gate: x <= %s, y <= %s (NA = no limit)", .lin_fmt(reg$x_pcov_max), .lin_fmt(reg$y_pcov_max))
  if (identical(reg$error, "both") && !identical(reg$weighting, "weights") && !is.null(reg$delta) && !is.na(reg$delta))
    w("- Deming error-variance ratio delta = var(ey)/var(ex) = %s (%s)", .lin_fmt(reg$delta, 4), reg$delta_source)
  if (identical(reg$error, "both") && identical(reg$weighting, "weights")) w("- York regression with each point's own x and y SD (from its pcov)")
  w("- pairs: %d total, %d used, %d removed by the pcov gate, %d without pcov (weighted fit), %d non-finite",
    reg$n_pairs, .lin_or(reg$n, 0L), .lin_or(reg$n_gated, 0L), .lin_or(reg$n_nopcov, 0L), .lin_or(reg$n_invalid, 0L))
  if (!isTRUE(reg$ok)) {
    w("- result: no fit. %s", .lin_or(reg$reason, ""))
  } else {
    ft <- reg$fit; st <- reg$stats
    w("- equation: %s", lin_equation(ft, reg$scale))
    w("- intercept a = %s (SE %s, %d%% CI %s to %s, p(a=0) %s)", .lin_fmt(ft$a), .lin_fmt(ft$se_a), round(100 * reg$level), .lin_fmt(ft$a_lo), .lin_fmt(ft$a_hi), .lin_p(ft$p_int0))
    w("- slope b = %s (SE %s, %d%% CI %s to %s, p(b=1) %s)", .lin_fmt(ft$b), .lin_fmt(ft$se_b), round(100 * reg$level), .lin_fmt(ft$b_lo), .lin_fmt(ft$b_hi), .lin_p(ft$p_slope1))
    w("- confidence intervals: %s", ft$ci_method)
    if (reg$scale == "log10") w("- reading: intercept %s means a constant fold difference of %s; slope %s means the fold difference changes with concentration (y ~ x^b).",
                                 .lin_fmt(ft$a), .lin_fmt(10^ft$a, 3), .lin_fmt(ft$b))
    w("")
    w("## Agreement (primary metrics)")
    w("- Lin's concordance correlation coefficient: %s (%d%% CI %s to %s): %s", .lin_fmt(st$ccc, 4), round(100 * reg$level), .lin_fmt(st$ccc_lo, 4), .lin_fmt(st$ccc_hi, 4), lin_ccc_label(st$ccc))
    if (reg$scale == "log10") {
      w("- Bland-Altman bias (mean log10 y/x): %s = %s-fold (CI %s-fold to %s-fold)", .lin_fmt(st$bias), .lin_fmt(10^st$bias, 3), .lin_fmt(10^st$bias_lo, 3), .lin_fmt(10^st$bias_hi, 3))
      w("- Bland-Altman limits of agreement: %s-fold to %s-fold (SD of log10 ratio %s)", .lin_fmt(10^st$loa_lo, 3), .lin_fmt(10^st$loa_hi, 3), .lin_fmt(st$sd_diff))
    } else {
      w("- Bland-Altman bias (mean y - x): %s (CI %s to %s)", .lin_fmt(st$bias), .lin_fmt(st$bias_lo), .lin_fmt(st$bias_hi))
      w("- Bland-Altman limits of agreement: %s to %s (SD of differences %s)", .lin_fmt(st$loa_lo), .lin_fmt(st$loa_hi), .lin_fmt(st$sd_diff))
    }
    w("- Bland-Altman trend (difference vs mean): slope %s, p %s", .lin_fmt(st$trend_slope), .lin_p(st$trend_p))
    w("- Pearson r %s; R-squared %s; Spearman rho %s (R-squared and r describe tightness, not agreement)", .lin_fmt(st$pearson), .lin_fmt(ft$r2), .lin_fmt(st$spearman))
    if (identical(reg$error, "y")) w("- with equal noise on both axes and a true slope of 1 an OLS slope near r is expected: slope %s, r %s", .lin_fmt(ft$b), .lin_fmt(st$pearson))
    w("- residuals: Shapiro-Wilk p %s; |residual| vs x Spearman rho %s (p %s); %s influential pair(s)", .lin_p(st$shapiro_p), .lin_fmt(st$hetero_rho), .lin_p(st$hetero_p),
      if (is.na(st$max_cook)) "n/a" else as.character(st$n_influential))
    if (!is.null(reg$replicate)) {
      rp <- reg$replicate
      w("- replicate precision over %d duplicate pair(s) in %d sets: within-pair SD %s%s; ICC %s", rp$n_pairs, rp$n_sets, .lin_fmt(rp$sd_within),
        if (is.na(rp$cv)) "" else sprintf(" (geometric CV %.1f%%)", 100 * rp$cv), .lin_fmt(rp$icc))
    }
  }
  if (!is.null(rep$sens) && nrow(rep$sens)) {
    w("")
    w("## Sensitivity to weighting (same error model and scale)")
    w("| weighting | primary | n | intercept a (CI) | slope b (CI) | CCC (CI) | BA bias | limits of agreement | r | R-squared |")
    w("|---|---|---|---|---|---|---|---|---|---|")
    for (i in seq_len(nrow(rep$sens))) {
      s <- rep$sens[i, ]
      w("| %s | %s | %d | %s (%s to %s) | %s (%s to %s) | %s (%s to %s) | %s | %s to %s | %s | %s |", LIN_WEIGHTINGS[[s$weighting]], if (s$primary) "yes" else "no", s$n,
        .lin_fmt(s$a), .lin_fmt(s$a_lo), .lin_fmt(s$a_hi), .lin_fmt(s$b), .lin_fmt(s$b_lo), .lin_fmt(s$b_hi),
        .lin_fmt(s$ccc), .lin_fmt(s$ccc_lo), .lin_fmt(s$ccc_hi), .lin_fmt(s$bias), .lin_fmt(s$loa_lo), .lin_fmt(s$loa_hi), .lin_fmt(s$r), .lin_fmt(s$r2))
    }
    w("(The gate row uses x/y pcov limits of the gate setting, or 20 where unset.)")
  }
  w("")
  w("## Transformation")
  tr <- reg$transform
  if (isTRUE(reg$ok) && !identical(tr, "none") && !is.null(reg$tf)) {
    tf <- reg$tf; rf <- reg$refit
    w("- applied: %s", if (identical(tr, "harmonize")) "harmonize both sets to the latent concentration" else "map y onto x (1:1)")
    w("- parameters from the primary fit: a = %s, b = %s", .lin_fmt(tf$a, 6), .lin_fmt(tf$b, 6))
    if (identical(tr, "y")) {
      w("- algorithm: fit %s on %s over the pairs used; then %s, applied to every pair's y.",
        if (tf$scale == "log10") "log10(y)" else "y", if (tf$scale == "log10") "log10(x)" else "x", tf$formula)
      if (tf$scale == "log10") w("- equivalent: y' = %s · y^%s", .lin_fmt(10^(-tf$a / tf$b), 6), .lin_fmt(1 / tf$b, 6))
      w("- note: this inverts the fitted line, so it is only as valid as the slope; with the errors-in-variables fit the slope is not attenuated.")
    } else {
      w("- model: x = alpha1 + beta1 z, y = alpha2 + beta2 z with beta1 beta2 = 1 and alpha1 + alpha2 = 0 (the correction is split equally)")
      w("- algorithm: %s (on the %s scale)", tf$formula, if (tf$scale == "log10") "log10" else "natural")
      w("- parameters: alpha1 = %s, alpha2 = %s, beta1 = %s, beta2 = %s", .lin_fmt(tf$alpha1, 6), .lin_fmt(tf$alpha2, 6), .lin_fmt(tf$beta1, 6), .lin_fmt(tf$beta2, 6))
      w("- note: this corrects systematic differences between the two sets; it does not remove scatter, and it should not be used to hide dilutional non-linearity.")
    }
    w("- check: refit after the transform gives intercept %s, slope %s (should be 0 and 1)", .lin_fmt(rf$a), .lin_fmt(rf$b))
  } else {
    w("- applied: no%s", if (!is.null(reg$transform_note)) paste0(" (", reg$transform_note, ")") else "")
    w("- available: map y onto x, y' = 10^((log10(y) - a) / b) (natural scale y' = (y - a) / b); or harmonize both sets to the latent concentration, z_x = sqrt(b)(x + a/(1+b)), z_y = (y - a/(1+b)) / sqrt(b).")
  }
  if (!is.null(reg$harm) && is.finite(reg$harm$std_diff_sd))
    w("- standardized differences (z_y - z_x)/sqrt(var_x + var_y): SD %s over %d pairs (about 1 when pcov is the whole error)", .lin_fmt(reg$harm$std_diff_sd, 3), reg$harm$n)
  if (!is.null(rep$consensus)) {
    cs <- rep$consensus
    w("")
    w("## Consensus per sample")
    w("- each sample's harmonized estimates are combined by inverse-variance weighting (variance b sd_x^2 on the x side, sd_y^2 / b on the y side), giving one concentration with an interval")
    w("- %d samples; %d with more than one estimate; median SE of the consensus %s (%s)", cs$n_samples, cs$n_multi, .lin_fmt(cs$median_se_z),
      if (identical(reg$scale, "log10")) "log10 units" else "natural units")
    w("- median heterogeneity (Cochran Q per degree of freedom): %s (about 1 when pcov is the whole error)", .lin_fmt(cs$median_q_ratio))
  }
  w("")
  w("## Findings")
  if (!length(rep$findings)) w("- none: no linearity problems were detected.")
  for (fd in rep$findings) {
    w("- [%s] %s: %s", toupper(fd$severity), fd$code, fd$message)
    if (!is.null(fd$evidence)) w("    evidence: %s", fd$evidence)
    if (!is.null(fd$action)) w("    action: %s", fd$action)
  }
  w("")
  w("## Sets available in this experiment")
  for (i in seq_len(nrow(rep$sets))) w("- %s: %d sets", rep$sets$label[i], rep$sets$sets[i])
  paste(L, collapse = "\n")
}

lin_report_json <- function(rep) {
  reg <- rep$reg
  slim <- function(f) if (is.null(f)) NULL else f[setdiff(names(f), c("model", "draws"))]
  obj <- list(generated = rep$generated, scope = rep$scope, comparison = rep$preset,
              filters = rep$filters, x = rep$x, y = rep$y, match = rep$match, differ = rep$differ,
              n_sets = rep$n_sets, n_pairs = rep$n_pairs, pair_types = rep$pair_types,
              gate_combos = rep$gate_combos,
              regression = list(ok = reg$ok, scale = reg$scale, error_model = reg$error, weighting = reg$weighting,
                                delta = reg$delta, delta_source = reg$delta_source, level = reg$level,
                                x_pcov_max = reg$x_pcov_max, y_pcov_max = reg$y_pcov_max,
                                n_pairs = reg$n_pairs, n_used = reg$n, n_gated = reg$n_gated, n_nopcov = reg$n_nopcov, n_invalid = reg$n_invalid,
                                reason = reg$reason, fit = slim(reg$fit), stats = reg$stats, replicate = reg$replicate,
                                equation = if (isTRUE(reg$ok)) lin_equation(reg$fit, reg$scale) else NULL),
              sensitivity = rep$sens,
              transform = if (!identical(reg$transform, "none") && !is.null(reg$tf)) list(applied = TRUE, kind = reg$tf$kind, scale = reg$tf$scale,
                                                                                         formula = reg$tf$formula, a = reg$tf$a, b = reg$tf$b,
                                                                                         alpha1 = reg$tf$alpha1, alpha2 = reg$tf$alpha2, beta1 = reg$tf$beta1, beta2 = reg$tf$beta2,
                                                                                         refit = slim(reg$refit)) else list(applied = FALSE),
              harmonization = reg$harm, consensus = rep$consensus,
              findings = rep$findings, severity = rep$severity)
  jsonlite::toJSON(obj, auto_unbox = TRUE, null = "null", na = "null", digits = NA, pretty = TRUE, force = TRUE)
}
