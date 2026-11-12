# Changelog

Releases are tagged `vYYYY.MM.DD` (the date the tag was cut); a second release on the same day takes a `.N` suffix.

## v2026.11.11.3 — Replicate Linearity: controls and standards

### Added
- **Quality Control > Replicate Linearity: controls and standards.** Tick **Include**
  (test samples, controls, standards) before *Find replicate sets*, then choose which
  type to compare. A control's set is its name on one antigen and feature; a standard's
  is its dilution level. The worker stores no concentration for either, so the app
  computes it from each curve's stored best model (Bayesian: posterior-median
  parameters), reads pcov from the stored precision profile and takes the gate class
  from LLOQ/ULOQ. A control well is estimated through every source and method of its
  plate and antigen. Masked controls and excluded standards are left out.
  A control is named by its `source` (for example `QC1 (Low)`, `Inhouse Ref`) when its
  `sampleid` is only the well, so one control is one set across plates and dilutions.
  Loading controls reports how many wells survive each stage (stored, matched to a curve,
  not masked, fitted, with a concentration).
  On load the app checks itself against the worker (the model inverse against the
  stored grid; the pcov it reads against the pcov stored for test samples) and the
  result goes in the report. Help note `qc.linearity.kinds`; design in
  `docs/design/replicate_linearity.md`. No schema or worker changes.

### Changed
- **Importer, "Configure the description field" > 4. Result**: the table now shows
  where each description is found: the number of wells and a "Where (plate: wells)"
  column such as `1. p1: A1, A2, B4 (+3 more); 2. p2: C5`, with wells in plate order.
  It lists every distinct string in the group (up to 200, paged, with column filters)
  instead of the 12 longest, and the "incomplete" message names the plate and wells of
  each example. In a big batch the sparse, oddly formatted label can be found without
  scanning plate after plate.
- **Importer, plate layout**: **Label characters** now defaults to 10 and offers 2 to 25
  (plus Auto). Wells widen to fit the label, so a long label is not clipped; a plate
  wider than the panel extends past it.

### Fixed
- **A long red error (for example after a failed batch fit) could not be closed.** The
  notification grew taller than the screen, so its close button was off-screen. Every
  notification is now capped to 60% of the window height and scrolls, with the close
  button always visible.
- **Bio-Plex .rbx / .srbx import: % aggregate beads and mask flags were never read.**
  - The mask flag is the "outlier" flag Bio-Plex Manager stores for every well and
    analyte (byte 12 of the 24-byte quantification record). It matches the vendor
    export's Outlier sheet cell for cell on every plate exported from the same file.
  - % aggregate beads is derived per well from the stored event counts:
    100 x (total - gated) / total. The binary stores no percentage. The first-read
    well (A1, occasionally B1) has no event counts, so it has no value.
  - The reader puts `pctaggbeads`, `masked` and `mask_reason` on the long response
    table, the layout template keeps them on its `assay_response_long` sheet, and the
    assembler writes them to `xmap_standard`, `xmap_sample`, `xmap_control` and
    `xmap_buffer`. Flagged points are therefore **masked from the start**, per analyte,
    with the reason "Bio-Plex Manager outlier flag (.rbx)" (or "masked at import" when a
    row has none). Edit the `masked` column of the template sheet before uploading to
    change this; other readers are unchanged.
- **Description pre-processor: two real defects found by the long-failing
  `test-assay-shape-rules.R`.**
  - A sample description like `PT01_T0_100` (PatientID_TimePeriod_Dilution) no longer
    loses its dilution: one bare integer left over after both the subject and the
    timepoint is proposed as the dilution.
  - A dilution ladder among ordinary samples was never proposed as standards: the
    "a curve is not a plate" size check divided by the wells that carry a numeric
    token instead of by the plate's candidate wells, so it was always 100%.

## v2026.11.11.2 — Quality Control: beads, sample identity, curve downloads

### Changed
- The **Quality Control - Basic** tab is now **Quality Control**.
- The **Bead Count** choice is now **Beads**, and appears only for bead-array
  experiments (it is hidden for absorbance/ELISA). A switch at the top selects
  **Bead Count** (the existing view) or **Aggregate Beads**.

### Added
- **Beads > Aggregate Beads**: percent aggregate beads (`pctaggbeads`, stored from
  the instrument file for samples, standards, controls and blanks) on the y axis
  against well position on the x axis, coloured by well type, with the study's
  `% aggregate beads` threshold, a list of wells above it, hover detail, and a
  CSV download. Help note `qc.bead_aggregate`.
- **Plate Dilution Series > Sources**: the details of a test-sample point now also
  show its well, patientid and timeperiod.
- **Standard Curve > Curve and Precision**: the test-sample popup shows patientid and
  timeperiod.
- **Standard Curve > Back-calculated samples**: a real **well** column (from the
  raw sample table) and **specimen_id** replace `sampleid`; where the importer stored
  the well as the sample id, specimen_id reads "(same as well)". The sample popup
  leads with the well, and the Replicate Linearity hover uses the same rule.
- **Standard Curve > Back-calculated samples**: experiment, plate (short name),
  antigen, feature and method are added to the table, with downloads of the table
  (.csv) and of everything stored for the selected plate, antigen and method (.xlsx:
  samples, standards, blanks, model selection, parameters, gates, LOQ/LOD limits,
  fitted grid).

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
