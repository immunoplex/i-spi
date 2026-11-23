# Changelog

From v1.0.0, releases follow [semantic versioning](https://semver.org): `vMAJOR.MINOR.PATCH`.
A release tag builds the image `ghcr.io/immunoplex/i-spi:MAJOR.MINOR.PATCH` (and `:MAJOR.MINOR`);
pushes to `main` keep building `:main`. Earlier releases were tagged by date (`vYYYY.MM.DD`, with
a `.N` suffix for a second release on the same day); those tags remain as history.

## Unreleased — Plates without test samples; sample identifiers from descriptions

### Added
- **Description pattern in the plate editor.** "Apply to matching wells" with a
  pattern such as `QC1*, QC2*` retypes every well whose description matches,
  on every plate and at every dilution, without selecting wells; the number of
  matching wells is shown first.
- **Source in the plate editor**, typed in or taken from the plate (file)
  names: the parts that differ between plates are proposed (e.g.
  `jackson_ON_JHM_high`, `southern_low`), editable, with a per-plate preview. It
  overrides the Source from the description rule (25 characters at most).
- **Experiments without test samples** (reagent or standards bridging, assay
  optimisation): *This experiment has no test samples* turns the per-plate
  "no test samples" error into a warning, and such plates get
  `nominal_sample_dilution` "1" instead of an empty value. No placeholder
  samples are added.

### Changed
- **`sampleid` comes from the well's description**, for every specimen type:
  the dilution ratio removed, letters and digits only, at most 15 characters
  ("QC1 (Orange) 1:50" -> `QC1Orange`, "Inhouse Ref 1:2952450" -> `InhouseRef`).
  It used to hold the number from the type code or a copy of the well. A well
  with no description keeps the type-code number. Different descriptions that
  end up with the same identifier are listed as a validation warning. Applies
  to new uploads; stored data is unchanged. Grouping by `sampleid` (replicates,
  linearity control sets) now follows the description.
- **"Same wells on every plate"** leaves wells that have no type and no reading
  alone, on every plate, instead of turning them into samples with no
  description (batches whose plates use different columns).

### Fixed
- **Description groups after a re-type.** A specimen-type tab drawn before its
  rule existed showed the separator "_" and, when the browser reported it,
  replaced the separator proposed from the data (a space for "QC1 (Orange)
  1:50"): every description became one element and the groups could not be
  configured. The box now takes the rule's separator instead.

## v1.2.0 — Choosing the precision-weights model; the same diagnostics for both models

Needs i-spi-compute 1.2.0 (curveRcore 0.7.1, curveRweights 0.3.1) and migration
012, both in place since 2026-11-22. With the additive model switched off (the
default) the model choice stays hidden, but two things are visible to everyone:
the run-time estimate above Submit, and the weights-fit table's diagnostics and
*check* column. Fits computed before 1.2.0 show "no diagnostics stored" there
until they are rerun.

### Added
- **Switch per deployment.** The additive weights model can be enabled with the
  environment variable `ISPI_ENABLE_ADDITIVE_WEIGHTS=true` (e.g. in the k8s
  manifest). The R option `ispi.enable_additive_weights` still works and wins
  when set.
- **Compute weights, in three steps** (additive model enabled):
  1. *Check the data*: for the selected method and scope, the number of samples
     and curve groups, people and time points, out-of-range samples (or that
     censoring was not recorded), readings per sample, the spread of the
     samples' SE, and the weights a run would replace, with a one-line
     recommendation. Nothing is switched automatically.
  2. *Choose the model*: Multiplicative stays preselected. The additive model's
     options (censor out-of-range samples, person term, kappa estimated or
     fixed at 1, and under Advanced beta1) take their
     defaults from the data check; options the data cannot support are greyed
     out with the reason.
  3. *Review and submit*: the job described in words and what it replaces.
- **Help**: new pages *Which precision-weights model?*, *Check the data* and
  *Additive model options*; glossary entries *tau*, *kappa* and
  *identifiability*. Each links to the section of the curveRweights guide that
  goes deeper.
- **Run-time estimate above Submit, for both models** (also with the additive
  model switched off): curve groups x seconds per group, scaled by the
  iterations x chains of the Advanced MCMC settings and by censoring. Calibrated
  on the 2026-11 sim runs at 4000 x 4 (about 40 s per group multiplicative,
  48 s additive, 145 s additive with censoring). The iteration inputs now accept
  what the compute API accepts (1000 to 20000; warm-up at least 100).
- **Results: the same diagnostics for both models.** With calib schema 0.7.1
  the weights-fit table shows `rhat_max`, `n_divergent`, `ess_min`,
  `ident_sd_ratio`, `resid_sd` and `resid_sd_ratio` for every fit, and the
  *check* column covers multiplicative rows too: sampling did not converge,
  kappa or beta1 weakly identified or wider than its prior, kappa fixed or
  outside 0.5 to 2, a variance model that gives low-SE (or high-SE) samples too
  much weight, and fits made before the diagnostics were stored. A
  multiplicative-only table without these columns (older schema) is unchanged.
  New glossary entry *Precision-weights fit diagnostics*. The download bundles'
  `weights_meta` carries the fit options.

### Changed
- What **tau** and **reliability** mean is stated correctly: with the person term
  (the default when people have several samples) tau is the variation *within*
  a person and reliability is a within-person reliability. Both, and kappa, are
  specific to the calibration method; the caption and help say not to compare
  Bayesian with frequentist values.

## v1.1.1 — Safe project queries; local sign-in bypass by environment

### Fixed
- **Manage Project: user input no longer becomes SQL.** Creating, joining and
  loading a project built its queries by pasting the project name, project
  number and user email into the SQL text. A name or email with an apostrophe
  failed, and the same gap allowed SQL injection. Every value is now passed as a
  bound parameter, project numbers must be whole numbers, and creating a project
  reads the new id with `RETURNING` instead of `currval()`. The same applies to
  loading a project from the project tables (`ui_handler.R`).
- **Local development without sign-in** (`LOCAL_DEV=1`) works from the
  environment alone: the app acts as `LOCAL_DEV_EMAIL` (default
  `local-dev@example.org`). Previously it needed an edit to `app.R`, and the
  sign-in and sign-out buttons switched to a different hard-coded account.
  `Renviron.example` documents both variables. The commented personal email
  address in `app.R` is removed.

## v1.1.0 — Stored estimates for controls and standards; About page

Needs i-spi-compute 1.1 (migration 011, curveRcore 0.7.0). On a database without
migration 011 the app still runs; the new tables are skipped and the views below
say why they are empty.

### Changed
- **QC > Replicate Linearity: controls and standards are read, not computed.** The
  compute job now stores, for every curve and method, the same estimates for control
  wells (`calib_controls`) and standard points (`calib_standards`) that it stores for
  test samples, from one calculation in curveRcore; earlier curves were filled once
  by a backfill using the same code. The app's own back-calculation
  (`linearity_backcalc.R`: posterior-median inverses, pcov read from the grid, scale
  guessed from the standards, and its self-check) is removed. The load message and
  the report state how many curves carry stored estimates.
- **Standard Curve > FDA 2018 classification** takes each standard's accuracy from
  the stored `recovery_pct` (median over a level's wells per plate) instead of
  inverting the curve in the app; the in-app copies of the curveRcore inverses are
  removed. A curve without stored estimates says so.

### Added
- **Data tab: Back-calculated controls and Control reference values** tabs, also in
  the RData, JSON and Excel bundles and the per-table CSVs. Control reference values
  show the study's rows and the project-wide rows.
- **Data tab: Back-calculated standards and Blanks** tabs (`calib_standards`,
  `calib_blanks`): the standard points and blank wells each fit used, the standards
  with their stored estimates and `recovery_pct`. Also in the bundles and CSVs.
- **About** (sidebar): the I-SPI version and commit, the database's calib schema
  version, and the compute releases (worker version, curveR packages, CmdStan) behind
  the stored results, overall and for the selected study, read from
  `calib_run.manifest`.
- Study **clone** copies the study's `control_reference` rows; study **delete**
  removes them and the curves' `calib_controls` rows; masking deletes clear
  `calib_controls` with the other curve results.
- Help: notes and schema entries for `calib_controls` and `control_reference`.

## v1.0.0 — Additive precision-weights model (feature-flagged); semantic versioning

### Changed
- Versioning: releases are now tagged with semantic versions (see above), and a release tag
  builds a versioned image. The image records its version and commit in `BUILD_STAMP`.

### Added
- **Precision Weights: additive measurement-error model (feature-flagged, off by default).**
  With `options(ispi.enable_additive_weights = TRUE)` the *Compute weights* panel offers a
  *Variance model* choice. *Additive* writes each sample's variance as real between-sample
  variation plus the measurement error the curve reports, so weights are bounded by
  1/tau^2, each sample gets a reliability, and out-of-range samples are treated as censored
  instead of weighted. The Summary figure, fit table and per-sample table are model aware
  (tau, kappa, reliability, censoring). Guides and the curveRfreq / curveRbayes pages are
  linked from the panel and the help pages. Needs i-spi-compute with migration 010 and
  curveRweights >= 0.3.0 / curveRcore >= 0.6.0. With the flag off nothing changes.
- Help: new glossary entry *Reliability*; schema and data-dictionary entries for the new
  `calib_weights` / `calib_weights_fit` columns.
- `AI_USE.md`: how generative AI is used in this project.

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
