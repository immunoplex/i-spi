# Replicate linearity (QC) — design

Status: phase 1 (test samples). Code: `src/linearity_report.R` (pure logic),
`src/linearity_module.R` (UI, plot, wiring), `src/test-linearity.R`. Placement:
Quality Control, between Standard Curve and Precision Weights.

## Purpose
Replace the old dilutional-linearity tool (`dev/dilutional_linearity_ui.R`, built
on retired `best_*` tables) with one that reads the `calib_*` results. Show how
well repeated estimates of the same sample agree after concentration, pcov and
gate class exist, and give a consensus estimate per sample.

## Data
`lin_fetch` reads `calib_samples` joined to `curve_lookup` (plate, antigen,
feature, source, wavelength) and, through a pre-aggregated hash join, to
`xmap_sample` for the well. `source` is the standard-curve source, not
`xmap_sample.source`. `specimen_id` is the sample id. `pcov_gate_class` is empty
for about 40% of stored samples (LLOQ/ULOQ undefined, all of project 90); then
`pcov_pass` TRUE is taken as in range, otherwise "unclassified", and the report
states how many were inferred.

## Concepts
- **Point**: one back-calculated estimate. **Set**: points sharing antigen,
  feature, patientid, timeperiod (specimen id when no patient).
- **Set kinds** (non-exclusive): duplicate, dilution (same plate), plate (same
  dilution), plate+dilution, method, source.
- **Pair**: an x point and a y point in one set. The user filters each axis by
  method/source/wavelength/plate/dilution, and says which dimensions must match
  and which must differ (`well` = same physical measurement). Presets fill these.
  With identical axis selections each pair appears once.

## Error model and weighting
Choices, all on the fitting scale (log10 default, or natural):

| error model | weighting | fit | intervals |
|---|---|---|---|
| y only | all / gate | OLS | t |
| y only | precision | WLS, weights 1/sd_y² | t |
| both | all / gate | Deming, delta = var(ey)/var(ex) | bootstrap over pairs |
| both | precision | York (individual sd_x, sd_y) | bootstrap |

- **SD from pcov.** pcov = 100·ln(10)·se(log10 conc), so on the log10 scale
  sd = pcov/(100 ln 10); on the natural scale sd = conc·pcov/100. pcov is floored
  at 1% before weighting.
- **delta** (Deming): `auto` = mean(sd_y²)/mean(sd_x²) over the pairs used;
  `equal` = 1 (orthogonal; exchangeable repeats); or custom. Swapping the axes
  gives slope 1/b and intercept −a/b exactly (tested).
- **Weighting**: the primary choice drives the plot and report; the other two are
  computed as sensitivity analyses (`lin_sensitivity`), with the gate using the
  entered limits (20 where unset). Precision weighting excludes pairs without a pcov.
- **Why not gate alone.** pcov peaks at the asymptotes, so a gate trims the
  concentration range where non-linearity and hook effects show, and lowers R² and
  worsens attenuation. It is offered, but weighting is the better default;
  the gate is a sensitivity analysis.

## Agreement metrics (primary)
Lin's CCC with a bootstrap CI; Bland–Altman bias with a t CI, limits of agreement
with their CIs, and a trend test of the difference against the mean (on the log
scale these are log10 ratios shown as fold). Pearson r, R² and Spearman are kept.
For duplicate pairs, within-pair SD, geometric CV and ICC(1).
With equal noise and true slope 1, the OLS slope is expected near r (not R²);
the report states this and flags a slope well below r.

## Transformations
- **Map y onto x**: log10 y' = (log10 y − a)/b (natural: y' = (y − a)/b), applied to
  every pair. The refit uses delta/b² (Deming) or sd_y/b (York), so it is exactly
  slope 1, intercept 0.
- **Harmonize**: x = α₁ + β₁z, y = α₂ + β₂z, β₁β₂ = 1, α₁ + α₂ = 0, giving
  z_x = √b (x + a/(1+b)), z_y = (y − a/(1+b))/√b. The refit is exactly 1:1, and the
  result does not depend on which estimate is called x. Needs error = both and b > 0.
  Appropriate for method or plate bridging; not for dilutions (a slope other than 1
  is the finding, and the report warns). A reference method should be anchored
  instead (not built).

## Consensus per sample
Each distinct point in a set is harmonized and combined by inverse variance
(var z_x = b·sd_x², var z_y = sd_y²/b): consensus 10^ẑ with interval ẑ ± 1.96 SE,
and Cochran Q/df (about 1 when pcov is the whole error). Uses every valid point
with a pcov. The SD of the standardized pair differences (z_y − z_x)/√(var_x+var_y)
is reported and flagged when well above 1.

## Plot
Fill = y gate class, outline = x gate class (9 combinations, one legend entry
each); size = sqrt(pcov_x² + pcov_y²), 16 px down to 5 px at 100; faded = not in
the regression; dashed 1:1; regression with a band (analytic for OLS, bootstrap
percentile for Deming/York); hover lists plate, well, patientid, timeperiod,
specimen_id, sample_dilution, wavelength, method, source for both axes plus
transformed values.

## Report
Markdown and JSON: project_id, study, experiment, filters, set definition
(including match/differ), pair counts and types, gate combinations, model and
weighting and delta, equation, agreement metrics, sensitivity table, transform
algorithm and parameters with a refit check, consensus summary, findings.

## Help
One information icon beside every control and section opens a plain-language note
(`src/help/qc/linearity.*.md`): overview, sets, error_model, delta, weighting,
scale, transform, intervals, agreement, consensus. Three Glossary concept entries
(topic "Agreement and linearity") carry the ideas behind them:
`glossary.errors_in_both_axes`, `glossary.agreement_ccc_ba`,
`glossary.latent_consensus`. The module handles its own `help_show` event, so the
icons work inside the QC tab. A test checks that every icon id and every
`[[link]]` resolves. When an option is added, add its note and icon in the same change.

## Phase 2 (not built)
Standards, controls and blanks have no stored back-calculated concentration,
pcov or gate class. Back-calculate them in the app from `calib_param` and the
LLOQ/ULOQ (validate against stored sample values) and add rows with
`kind = "standard" | "control" | "blank"`.

## Not built
- Anchoring one axis to a reference (alpha1 = 0, beta1 = 1) for harmonization.
- More than two sets harmonized jointly (alternate consensus and per-set Deming).
- A hierarchical Stan model giving uncertainty on z, alpha and beta directly.

## Known limits
- Bootstrap intervals cost B refits (B = 200 is quick for thousands of pairs).
- Natural-scale SD is conc·pcov/100, a first-order approximation.
- Fetching a very large experiment is slow (187k rows took about 30 s).
- Not exercised in a live browser during development.
