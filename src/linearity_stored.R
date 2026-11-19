# =============================================================================
# linearity_stored.R  --  CONTROLS and STANDARDS for QC > Replicate Linearity,
#                         read from the estimates the compute job stores
#
# Since i-spi-compute 1.1 (calib schema 0.7.0, migration 011) the worker stores,
# for every curve and method, the same estimates for control wells
# (calib_controls) and standard points (calib_standards estimate columns) that it
# stores for test samples (calib_samples): concentration, pcov, gate class and
# censoring, from one calculation in curveRcore. Curves fitted earlier were filled
# by a one-off backfill using the same calculation. The app computes nothing here.
#
#   controls   calib_controls: final_concentration (estimate x the control's
#              dilution, as for samples). Masked control wells are stored too
#              (included = false) and left out unless asked for.
#   standards  calib_standards: the estimate at the standard's own level,
#              10^predicted_log10_concentration (a standard's dilution is already in
#              its nominal concentration), plus nominal and recovery_pct.
#
# Pure shaping functions over plain frames, plus lin_fetch_extra(), which reads the DB.
#
#   ex  <- lin_fetch_extra(pool, project, study, experiment, c("control", "standard"))
#   pts <- rbind(lin_prepare(raw_samples, "sample"),
#                lin_prepare(ex$control, "control"), lin_prepare(ex$standard, "standard"))
# =============================================================================

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

.lin_num <- function(v) suppressWarnings(as.numeric(v))
.lin_dil_txt <- function(d) { d <- .lin_num(d); ifelse(is.na(d), "1", format(d, trim = TRUE, scientific = FALSE)) }

# Stored control rows (calib_controls joined to curve_lookup) -> lin_fetch-shaped rows.
# One control well appears once per curve (every standard source of its plate) and method.
lin_stored_controls <- function(ctl) {
  if (is.null(ctl) || !is.data.frame(ctl) || !nrow(ctl)) return(NULL)
  data.frame(curve_id = .lin_id(ctl$curve_id), method = as.character(ctl$method),
             sampleid = .lin_none_chr(lin_control_label(ctl$sampleid, ctl$well, ctl$control_source,
                                                        ctl$stype, ctl$dilution)),
             patientid = "__none__", timeperiod = "__none__", dilution = .lin_dil_txt(ctl$dilution),
             final_concentration = .lin_num(ctl$final_concentration), pcov = .lin_num(ctl$pcov),
             pcov_pass = as.logical(ctl$pcov_pass), pcov_gate_class = as.character(ctl$pcov_gate_class),
             censor_class = as.character(ctl$censor_class),
             plateid = ctl$plateid, plate = ctl$plate, antigen = ctl$antigen, feature = ctl$feature,
             source = ctl$source, wavelength = ctl$wavelength, well = ctl$well, stringsAsFactors = FALSE)
}

# Stored standard rows (calib_standards joined to curve_lookup) -> lin_fetch-shaped rows.
# The set is the standard LEVEL ("std <dilution>") on one antigen, so wells of one level
# on every plate are replicates.
lin_stored_standards <- function(std) {
  if (is.null(std) || !is.data.frame(std) || !nrow(std)) return(NULL)
  lvl <- format(.lin_num(std$dilution), trim = TRUE, scientific = FALSE)
  data.frame(curve_id = .lin_id(std$curve_id), method = as.character(std$method),
             sampleid = paste0("std ", lvl), patientid = "__none__", timeperiod = "__none__", dilution = lvl,
             final_concentration = 10^.lin_num(std$predicted_log10_concentration), pcov = .lin_num(std$pcov),
             pcov_pass = as.logical(std$pcov_pass), pcov_gate_class = as.character(std$pcov_gate_class),
             censor_class = as.character(std$censor_class),
             plateid = std$plateid, plate = std$plate, antigen = std$antigen, feature = std$feature,
             source = std$source, wavelength = std$wavelength, well = std$well,
             nominal = .lin_num(std$nominal), recovery_pct = .lin_num(std$recovery_pct), stringsAsFactors = FALSE)
}

# How many (curve, method) pairs carry stored estimates. `have`/`need` per type; a
# pair is missing when it was fitted before i-spi-compute 1.1 and not yet backfilled.
lin_coverage_text <- function(cov) {
  if (is.null(cov) || !length(cov)) return("")
  one <- function(k, lab) {
    z <- cov[[k]]; if (is.null(z) || is.na(z$need) || z$need == 0L) return(NULL)
    sprintf("%s: stored estimates for %s of %s fitted curve(s)%s", lab, format(z$have, big.mark = ","),
            format(z$need, big.mark = ","),
            if (z$have < z$need) " (the rest were fitted before i-spi-compute 1.1; a refit or the backfill adds them)" else "")
  }
  paste(unlist(list(one("control", "Controls"), one("standard", "Standards"))), collapse = " | ")
}

# Where did the control wells go? One line from lin_fetch_extra()$control_stages: wells
# stored, how many have stored estimates, were not masked, have a concentration.
lin_control_stages_text <- function(st) {
  if (is.null(st)) return("")
  n <- function(v) if (is.null(v) || is.na(v)) "?" else format(v, big.mark = ",")
  paste0("Control wells: ", n(st$stored), " stored", if (!is.null(st$masked) && !is.na(st$masked) && st$masked > 0) sprintf(" (%s masked)", n(st$masked)) else "",
         " > ", n(st$estimated), " estimates stored (one per curve source and method) > ", n(st$kept),
         " not masked > ", n(st$with_conc), " with a concentration",
         if (!is.null(st$names) && !is.na(st$names) && nzchar(st$names)) paste0(". Names stored: ", substr(st$names, 1, 700)) else "", ".")
}

# Does the database hold the stored estimates (migration 011)?
lin_has_stored <- function(pool) {
  r <- .calib_q(pool, "SELECT to_regclass($1) IS NOT NULL AS ok", params = list(.tbl("calib_controls")))
  is.data.frame(r) && nrow(r) && isTRUE(r$ok[1])
}

# ---- fetch ---------------------------------------------------------------------------------------
# Reads only what is needed, scoped to the experiment. Returns
#   list(control = raw|NULL, standard = raw|NULL, coverage = list, control_stages = list,
#        stored = TRUE|FALSE)
# `samples` is accepted for the module's interface and not used.
lin_fetch_extra <- function(pool, project, study, experiment, kinds = c("control", "standard"),
                            samples = NULL, include_masked = FALSE) {
  kinds <- intersect(kinds, c("control", "standard"))
  out <- list(control = NULL, standard = NULL, coverage = list(), control_stages = NULL, stored = TRUE)
  if (!length(kinds)) return(out)
  if (!lin_has_stored(pool)) { out$stored <- FALSE; return(out) }
  scope <- "cl.project_id = $1 AND cl.study_accession = $2 AND cl.experiment_accession = $3"
  par <- list(project, study, experiment)
  cl <- .tbl("curve_lookup")
  int1 <- function(d, col) if (is.data.frame(d) && nrow(d) && col %in% names(d)) as.integer(d[[col]][1]) else NA_integer_

  if ("control" %in% kinds) {
    # Masked wells are dropped here, not in SQL, so that the stage counts can say how many.
    ctl <- .calib_q(pool, sprintf(
      "SELECT cc.curve_id, cc.method, cc.well, cc.sampleid, cc.source AS control_source,
              cc.dilution::float8 AS dilution, cc.included, cc.final_concentration::float8 AS final_concentration,
              cc.pcov::float8 AS pcov, cc.pcov_pass, cc.pcov_gate_class, cc.censor_class,
              cl.plateid, cl.plate, cl.antigen, cl.feature, cl.source, cl.wavelength
         FROM %s cc JOIN %s cl ON cl.curve_id = cc.curve_id
        WHERE %s", .tbl("calib_controls"), cl, scope), params = par)
    tot <- .calib_q(pool, sprintf(
      "SELECT count(*)::int AS n, (count(*) FILTER (WHERE c.masked))::int AS n_masked,
              string_agg(DISTINCT concat_ws(' ', c.sampleid, '(stype', c.stype || ',', 'source', c.source || ',',
                                            'dilution', c.dilution::text || ')'), '; ') AS names
         FROM %s c WHERE c.project_id = $1 AND c.study_accession = $2 AND c.experiment_accession = $3",
      .tbl("xmap_control")), params = par)
    # fitted (curve, method) pairs that have control wells, and how many have stored estimates
    cov <- .calib_q(pool, sprintf(
      "SELECT count(*)::int AS need,
              (count(*) FILTER (WHERE EXISTS (SELECT 1 FROM %s cc WHERE cc.curve_id = p.curve_id AND cc.method = p.method)))::int AS have
         FROM (SELECT DISTINCT f.curve_id, f.method FROM %s f JOIN %s cl ON cl.curve_id = f.curve_id
                WHERE f.is_best AND %s
                  AND EXISTS (SELECT 1 FROM %s c WHERE c.curve_id = f.curve_id)) p",
      .tbl("calib_controls"), .tbl("calib_fit"), cl, scope, .tbl("control_for_fit")), params = par)
    n_est <- if (is.data.frame(ctl)) nrow(ctl) else 0L
    if (n_est && !include_masked) ctl <- ctl[!(ctl$included %in% FALSE), , drop = FALSE]
    out$control <- lin_stored_controls(ctl)
    out$coverage$control <- list(need = int1(cov, "need"), have = int1(cov, "have"))
    out$control_stages <- list(
      stored = int1(tot, "n"), masked = int1(tot, "n_masked"),
      names = if (is.data.frame(tot) && nrow(tot) && "names" %in% names(tot)) as.character(tot$names[1]) else NA_character_,
      estimated = n_est, kept = if (is.data.frame(ctl)) nrow(ctl) else 0L,
      with_conc = if (is.null(out$control)) 0L else sum(is.finite(out$control$final_concentration)))
  }
  if ("standard" %in% kinds) {
    std <- .calib_q(pool, sprintf(
      "SELECT cs.curve_id, cs.method, cs.well, cs.dilution::float8 AS dilution, cs.concentration::float8 AS nominal,
              cs.predicted_log10_concentration::float8 AS predicted_log10_concentration,
              cs.pcov::float8 AS pcov, cs.pcov_pass, cs.pcov_gate_class, cs.censor_class,
              cs.recovery_pct::float8 AS recovery_pct,
              cl.plateid, cl.plate, cl.antigen, cl.feature, cl.source, cl.wavelength
         FROM %s cs JOIN %s cl ON cl.curve_id = cs.curve_id
        WHERE %s%s", .tbl("calib_standards"), cl, scope, if (include_masked) "" else " AND cs.included"), params = par)
    if (is.data.frame(std) && nrow(std)) {
      # a (curve, method) pair is estimated when any of its points carries an estimate
      est <- !is.na(std$predicted_log10_concentration) | !is.na(std$pcov) | !is.na(std$censor_class)
      key <- paste(.lin_id(std$curve_id), std$method)
      done <- unique(key[est])
      out$coverage$standard <- list(need = length(unique(key)), have = length(done))
      std <- std[key %in% done, , drop = FALSE]
    } else out$coverage$standard <- list(need = 0L, have = 0L)
    out$standard <- lin_stored_standards(std)
  }
  out
}
