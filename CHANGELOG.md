# Changelog

Releases are tagged `vYYYY.MM.DD` (the date the tag was cut).

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
