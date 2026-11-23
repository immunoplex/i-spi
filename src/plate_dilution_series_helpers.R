# =============================================================================
# plate_dilution_series_helpers.R  --  pure helpers for QC > Plate Dilution
# Series (plate_dilution_series_module.R): shape raw xmap_* rows into points and
# into the test-sample and control series overlaid on the Sources sub-tab.
#
# No Shiny, no database: tested by test-plate-dilution-series.R.
#
# Dilution scale: every xmap_* table stores the dilution FACTOR (standards
# 50..2,952,450; samples and controls 50..5000 on the RPE plates), so all
# series share the x axis as stored.
# =============================================================================

PDS_NONE_SRC  <- "(no source)"
PDS_NONE_FEAT <- "(no feature)"

pds_feat_label <- function(x) {
  x <- as.character(x)
  ifelse(is.na(x) | x %in% c("__none__", ""), PDS_NONE_FEAT, x)
}

# Any raw xmap_* frame (standards, samples, controls) -> numeric dilution/MFI,
# antigen/feature/plate helper columns; rows that can't sit on log-log axes
# dropped. Masked rows are KEPT (this tab shows contamination in context).
pds_prep_points <- function(df) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(NULL)
  if (!"feature"      %in% names(df)) df$feature <- NA_character_
  if (!"antigen"      %in% names(df)) df$antigen <- NA_character_
  if (!"source"       %in% names(df)) df$source  <- NA_character_
  if (!"antibody_mfi" %in% names(df)) return(NULL)
  if (!"dilution"     %in% names(df)) return(NULL)
  plate_raw <- if ("plate" %in% names(df) && !all(is.na(df$plate)))
                 as.character(df$plate)
               else if ("plateid" %in% names(df)) as.character(df$plateid)
               else "plate"
  num <- function(x) suppressWarnings(as.numeric(as.character(x)))
  df$dil_num     <- num(df$dilution)
  df$mfi_num     <- num(df$antibody_mfi)
  df$feature_lbl <- pds_feat_label(df$feature)
  df$antigen_lbl <- as.character(df$antigen)
  df$plate_lbl   <- plate_raw
  df <- df[is.finite(df$dil_num) & df$dil_num > 0 &
           is.finite(df$mfi_num) & df$mfi_num > 0, , drop = FALSE]
  if (!nrow(df)) return(NULL)
  df
}

# Test samples for one analyte: only patientid/timeperiod series measured at more
# than one dilution on a plate (the series worth joining). Labelled
# "<patientid> | <timeperiod>" in $sample_lbl.
pds_sample_series <- function(raw, antigen, feature) {
  smp <- pds_prep_points(raw)
  if (is.null(smp)) return(NULL)
  if (!all(c("patientid", "timeperiod") %in% names(smp))) return(NULL)
  smp <- smp[smp$antigen_lbl == antigen & smp$feature_lbl == feature, , drop = FALSE]
  if (!nrow(smp)) return(NULL)
  smp$sample_lbl <- paste0(as.character(smp$patientid), " | ", as.character(smp$timeperiod))
  key  <- paste(smp$sample_lbl, smp$plate_lbl, sep = "\r")
  ndil <- tapply(smp$dil_num, key, function(v) length(unique(v)))
  keep <- names(ndil)[ndil >= 2]
  smp  <- smp[key %in% keep, , drop = FALSE]
  if (!nrow(smp)) return(NULL)
  smp
}

# Controls for one analyte, all of them (a control at a single dilution is shown
# as points). $control_name is the control's name (lin_control_label: sampleid,
# or the Source on plates loaded before sampleids came from descriptions);
# $control_source its own Source, NA when that Source is only the name;
# $series "<name> | <control source>" (just the name without a source).
# `sources`: optional character vector to keep (NA source matched by PDS_NONE_SRC).
pds_control_series <- function(raw, antigen, feature, sources = NULL) {
  ctl <- pds_prep_points(raw)
  if (is.null(ctl)) return(NULL)
  ctl <- ctl[ctl$antigen_lbl == antigen & ctl$feature_lbl == feature, , drop = FALSE]
  if (!nrow(ctl)) return(NULL)
  g <- function(nm) if (nm %in% names(ctl)) ctl[[nm]] else rep(NA_character_, nrow(ctl))
  nm  <- lin_control_label(g("sampleid"), g("well"), g("source"), g("stype"), g("dilution"))
  src <- trimws(as.character(g("source")))
  src[is.na(src) | src %in% c("", "__none__") | toupper(src) == toupper(trimws(nm))] <- NA_character_
  ctl$control_name   <- nm
  ctl$control_source <- src
  ctl$series <- ifelse(is.na(src), nm, paste0(nm, " | ", src))
  if (length(sources)) {
    lab <- ifelse(is.na(src), PDS_NONE_SRC, src)
    ctl <- ctl[lab %in% sources, , drop = FALSE]
    if (!nrow(ctl)) return(NULL)
  }
  ctl
}

# Distinct control sources of an analyte's controls (for the selector), with
# PDS_NONE_SRC standing in for controls without one.
pds_control_sources <- function(raw, antigen, feature) {
  ctl <- pds_control_series(raw, antigen, feature)
  if (is.null(ctl)) return(character())
  sort(unique(ifelse(is.na(ctl$control_source), PDS_NONE_SRC, ctl$control_source)))
}
