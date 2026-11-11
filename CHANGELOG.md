# Changelog

Releases are tagged `vYYYY.MM.DD` (the date the tag was cut); a second release on the same day takes a `.N` suffix.

## v2026.11.11.1 — Replicate Linearity

### Added
- **Quality Control > Replicate Linearity** (between Standard Curve and Precision
  Weights): finds samples measured more than once (duplicates, several dilutions,
  several plates, frequentist vs Bayesian, several curve sources), lets you choose
  what goes on each axis, and shows how well the estimates agree. Test samples
  only; standards, controls and blanks are planned.
- Scatter with a 1:1 line, regression line with confidence band, fill colour by
  the y point's gate class and outline by the x point's (9 combinations), point
  size by combined precision, and hover with plate, well, patientid, timeperiod,
  specimen_id, sample_dilution, wavelength, method and source for both axes.
- **Error model:** both estimates carry error (Deming; York when weighted) or only
  y (ordinary / weighted least squares), on the log10 or natural scale, with the
  error-variance ratio taken from pcov, set to 1, or entered.
- **Regression uses** all pairs, pairs within a pcov gate for each axis, or all
  pairs weighted by precision; the two not chosen are reported as sensitivity
  analyses.
- **Agreement:** Lin's concordance coefficient and Bland-Altman bias, limits of
  agreement and trend lead; Pearson r, R-squared and Spearman are kept. Duplicates
  also report within-pair CV and ICC.
- **Transforms:** map y onto x (1:1), or harmonize both sets to the latent
  concentration; plus a consensus concentration with an interval per sample.
- Report (Markdown and JSON) with project, study, experiment, set definition,
  model, regression and agreement, sensitivity analyses, transformation parameters
  and consensus; downloads for the pairs and the consensus table.
- Help: an information icon beside every control opens a plain-language note
  (`qc.linearity.*`), and three Glossary entries under a new topic, "Agreement and
  linearity". Design note: `docs/design/replicate_linearity.md`.

### Notes
- Gate class is missing for about 40% of stored samples; it is then inferred from
  the pcov pass flag and the report says how many were.
- No schema changes and no worker changes.

## v2026.11.11 — Fit diagnostics

### Added
- **Data > Results > Fit diagnostics**: builds a report for the selected
  experiment from the stored fit, sampler, parameter, gate and LOO results. It
  gives a verdict, ranked findings with evidence, and ordered next steps, and
  downloads as Markdown or JSON written for an LLM to read.
- Report content: response scale as actually fitted, settings in force with
  tier and who/when for overrides, pooled-fit size, sampler statistics
  (R-hat, ESS, divergences, tree depth, E-BFMI, Student-t `nu`), per-plate
  parameter heterogeneity and outliers, gate failures, LOO reliability.
- Assay-aware warning when the response scale does not match the assay
  (for example raw absorbance fitted without a log transform).
- Warning when a stored fit no longer matches the current settings.
- A notice after each completed compute job listing its warnings and errors.
- Help: glossary entry "Fit diagnostics report", how-to note for the tab, and
  a design/growth document, `docs/design/fit_diagnostics.md`.

### Also in this release
- Data > Raw inputs: Replicates and Subtract Wavelengths tabs.
- Import: optional-step roll-ups, approve-loop fix, rbx run dates and plate progress.

### Notes
- Motivating case: ELISA plates 1–10 (project 92) fitted poorly because an
  experiment-level override set `is_log_response = false` for absorbance data.
- No schema changes and no worker changes.
