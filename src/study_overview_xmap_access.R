# =============================================================================
# study_overview_xmap_access.R  --  xmap_* specimen reads for Study Overview [P2]
# -----------------------------------------------------------------------------
# The original pull_standard / pull_blank / pull_control read the bead-count QC
# thresholds (lower_bc_threshold, pct_agg_threshold) via an INNER JOIN to
# madi_results.xmap_study_config keyed by (study_accession, param_user). That
# table is STALE: those params now live in calib_settings (the cascade,
# bead_count group), and the INNER JOIN can silently return zero rows for a study
# that was never written there -- which would make the whole pane come back
# empty. So here the thresholds are resolved ONCE from calib_settings and bound
# into the queries as parameters; the stale join is gone.
#
# Semantics note: xmap_study_config thresholds were PER-USER; the cascade is
# PER-STUDY/scope. This deliberately moves threshold selection from per-user to
# per-study -- the correct post-refactor behavior.
#
# Each so_pull_* preserves its original SELECT contract exactly (same columns,
# same lowbeadn/highbeadagg CASE flags, same header join), so downstream
# summarise_data() / make_summspec() are unchanged. std_source is kept on the
# standard pull (blank/control do not have it), matching summarise_data()'s
# conditional grouping.
#
# Depends on: DBI, glue; resolve_settings_scoped() + settings_as_list()
# (settings_cascade_access.R).
# =============================================================================

# schema resolver (self-contained: do not depend on other access files' helpers)
.so_xmap_schema <- function() getOption("ispi.calib_schema", "madi_results")

# ---- resolve the two bead thresholds from the cascade (study scope) ----------
# Returns list(lower_bc = <num>, pct_agg = <num>). Falls back to wide-open
# sentinels if a value is missing so the pull still returns rows (everything
# flagged "Acceptable") rather than an empty pane.
so_bead_thresholds <- function(pool, project, study) {
  vals <- tryCatch(
    settings_as_list(resolve_settings_scoped(pool, project, study, group = "bead_count")),
    error = function(e) { warning("so_bead_thresholds: ", conditionMessage(e)); list() })
  num <- function(x, default) {
    v <- suppressWarnings(as.numeric(x)); if (length(v) != 1 || is.na(v)) default else v
  }
  list(
    lower_bc = num(vals[["lower_bc_threshold"]], -Inf),  # -Inf -> nothing is "LowBeadN"
    pct_agg  = num(vals[["pct_agg_threshold"]],  Inf)    #  Inf -> nothing is "PctAggBeads"
  )
}

# internal: run a specimen pull with thresholds bound as literals (no config join)
.so_specimen_query <- function(pool, schema, table, alias, selected_study,
                               lower_bc, pct_agg, extra_cols = "") {
  # $1 study, $2 lower_bc, $3 pct_agg
  glue::glue_sql(
    "SELECT DISTINCT
         {`a`}.study_accession,
         {`a`}.experiment_accession,
         ({`a`}.experiment_accession || '_' || h.nominal_sample_dilution) AS Analyte,
         h.plateid,
         h.plate,
         h.nominal_sample_dilution,
         {`a`}.feature,
         {`a`}.well,
         {`a`}.antigen,
         {`a`}.antibody_mfi  AS mfi,
         {`a`}.antibody_n    AS bead_count,
         {`a`}.pctaggbeads,
         {DBI::SQL(extra_cols)}
         CASE WHEN {`a`}.antibody_n   < {lower_bc} THEN 'LowBeadN'    ELSE 'Acceptable' END AS lowbeadn,
         CASE WHEN {`a`}.pctaggbeads  > {pct_agg}  THEN 'PctAggBeads' ELSE 'Acceptable' END AS highbeadagg
     FROM {`schema`}.{`table`} AS {`a`}
     INNER JOIN {`schema`}.xmap_header AS h
             ON h.study_accession      = {`a`}.study_accession
            AND h.experiment_accession = {`a`}.experiment_accession
            AND TRIM(h.plate_id)       = TRIM({`a`}.plate_id)
     WHERE {`a`}.study_accession = {selected_study}
     ORDER BY {`a`}.experiment_accession, {`a`}.antigen",
    a = alias, schema = schema, table = table, .con = pool)
}

so_pull_standard <- function(pool, selected_study, lower_bc, pct_agg) {
  q <- .so_specimen_query(pool, .so_xmap_schema(), "xmap_standard", "s", selected_study,
                          lower_bc, pct_agg, extra_cols = "s.source AS std_source,")
  DBI::dbGetQuery(pool, q)
}
so_pull_blank <- function(pool, selected_study, lower_bc, pct_agg) {
  q <- .so_specimen_query(pool, .so_xmap_schema(), "xmap_buffer", "b", selected_study,
                          lower_bc, pct_agg)
  DBI::dbGetQuery(pool, q)
}
so_pull_control <- function(pool, selected_study, lower_bc, pct_agg) {
  q <- .so_specimen_query(pool, .so_xmap_schema(), "xmap_control", "c", selected_study,
                          lower_bc, pct_agg)
  DBI::dbGetQuery(pool, q)
}

# ---- raw samples (xmap_sample) -- reusable by the timepoint / arm / bead panes.
# xmap_sample already carries plate/plateid/nominal_sample_dilution, so no header
# join is needed. Masked rows excluded. Thresholds bound as literals (no config).
so_pull_raw_samples <- function(pool, selected_study, lower_bc, pct_agg) {
  q <- glue::glue_sql(
    "SELECT
        s.study_accession, s.experiment_accession,
        (s.experiment_accession || '_' || s.nominal_sample_dilution) AS analyte,
        s.plateid, s.plate, s.nominal_sample_dilution, s.feature, s.antigen,
        s.sampleid, s.stype, s.patientid, s.agroup, s.timeperiod, s.dilution, s.source,
        s.antibody_mfi AS mfi, s.antibody_n AS bead_count, s.pctaggbeads,
        CASE WHEN s.antibody_n  < {lower_bc} THEN 'LowBeadN'    ELSE 'Acceptable' END AS lowbeadn,
        CASE WHEN s.pctaggbeads > {pct_agg}  THEN 'PctAggBeads' ELSE 'Acceptable' END AS highbeadagg
       FROM {`schema`}.xmap_sample AS s
      WHERE s.study_accession = {selected_study} AND NOT s.masked
      ORDER BY s.experiment_accession, s.antigen, s.plate",
    schema = .so_xmap_schema(), .con = pool)
  DBI::dbGetQuery(pool, q)
}

# convenience: resolve thresholds from the cascade, then pull raw samples.
so_raw_samples <- function(pool, project, study) {
  th <- so_bead_thresholds(pool, project, study)
  so_pull_raw_samples(pool, study, th$lower_bc, th$pct_agg)
}

# ---- one call for the non-sample summary the BCS / bead panes need -----------
# Pulls standard/blank/control (xmap_*, thresholds from cascade), then feeds them
# to make_summspec() with EMPTY tibbles for raw/samples/low_bead/high_agg/plates
# -- so NO best_* path (pull_samples/pull_fits/check_plate) is touched. Returns
# summ_spec restricted to specimen_type in {standard, blank, control}.
so_nonsample_summspec <- function(pool, project, study) {
  th <- so_bead_thresholds(pool, project, study)
  empty <- tibble::tibble()
  std <- so_pull_standard(pool, study, th$lower_bc, th$pct_agg)
  blk <- so_pull_blank(pool,    study, th$lower_bc, th$pct_agg)
  ctl <- so_pull_control(pool,  study, th$lower_bc, th$pct_agg)
  ss <- make_summspec(standard = std, blank = blk, control = ctl,
                      raw = empty, low_bead = empty, high_agg = empty,
                      plates = empty, active_samples = empty)
  if (is.null(ss) || !nrow(ss)) return(ss)
  ss[ss$specimen_type %in% c("standard", "blank", "control"), , drop = FALSE]
}

# ---- full summary INCLUDING raw_sample rows (for the bead-count pane) ---------
# Same as so_nonsample_summspec but feeds raw_samples too, so make_summspec emits
# per-specimen nlowbead / nhighbeadagg for standard/blank/control AND raw_sample.
# Still no best_* path (active_samples/plates/low_bead/high_agg left empty).
so_full_summspec <- function(pool, project, study) {
  th <- so_bead_thresholds(pool, project, study)
  empty <- tibble::tibble()
  std <- so_pull_standard(pool,     study, th$lower_bc, th$pct_agg)
  blk <- so_pull_blank(pool,        study, th$lower_bc, th$pct_agg)
  ctl <- so_pull_control(pool,      study, th$lower_bc, th$pct_agg)
  raw <- so_pull_raw_samples(pool,  study, th$lower_bc, th$pct_agg)
  make_summspec(standard = std, blank = blk, control = ctl, raw = raw,
                low_bead = empty, high_agg = empty,
                plates = empty, active_samples = empty)
}
