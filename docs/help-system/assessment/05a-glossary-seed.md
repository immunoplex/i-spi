# Phase 5a — Glossary Seed

Status: Draft for review
Date: 2026-10-09
Scope: §8.2 of `dev/ISPI_HELP_SYSTEM_ASSESSMENT_PLAN.md`

Starts from the plan's §8.2 seed list, corrected against actual terminology found in Phase 2 (`02-curveR-topic-index.md`) rather than the plan's placeholder names, then expanded with every additional term `01-ui-inventory.md`/`01a-decision-points.md` flagged as jargon. For each term: **card** (a 1-2 sentence glossary card is enough) or **entry** (substantial enough to warrant its own `category: conceptual` help entry, with the glossary card linking out to it) — every term gets one or the other, none left open.

A term already assigned its own row in `05-help-content-mapping.csv` (e.g. `glossary.blank_wells`) is marked **entry → see mapping table** rather than duplicated here with new prose guidance.

---

## Specimen & plate vocabulary

| Term | Card / Entry | Notes |
|---|---|---|
| Plate | card | Physical assay plate; one `plateid` per plate, wells addressed `row+col` (e.g. `A1`). |
| Well | card | One addressable position on a plate; carries a `specimen_type` and `description`. |
| Standard / Standard wells | entry → see mapping table | `glossary.standard_wells` — `data_dictionary.R table_doc('xmap_standard')`. |
| Control / Control wells | entry → see mapping table | `glossary.control_wells` — `table_doc('xmap_control')`. |
| Blank / Blank wells | entry → see mapping table | `glossary.blank_wells` — `table_doc('xmap_buffer')`. |
| Sample / Sample wells | entry → see mapping table | `glossary.sample_wells` — `table_doc('xmap_sample')`. |
| Specimen type codes (X / S / C / B) | **entry** | Worth its own consolidated entry beyond the four individual well-type cards above: explains the full code set together, the numeric suffix convention (`S1`..`S11`), and `type_origin` (`file` / `proposed` / `user` — whether a type was read from the instrument file, suggested by the app, or confirmed/typed by a person). Ties into `import.plate_grid.confirm` and `compute.import.dilution_source_precedence`. |
| Bead count | card | Ties to `qc.bead_count.thresholds` (mapping table) for the fuller explanation of the threshold controls. |
| Outlier | card | **Not currently user-facing** — `dilution_analysis_ui.R`/`outliers.R` are excluded per the 2026-10-09 scoping decision (dead code, pending a major refactor). Seed the term now so it's ready when that feature returns; don't build it out further until then. |
| Dilutional linearity | card | Same exclusion as Outlier — app-side concept with no curveR vignette topic either (confirmed in `02-curveR-topic-index.md`). Minimal stub only; revisit when the feature is refactored back in. |

## Dilution & calibration vocabulary

| Term | Card / Entry | Notes |
|---|---|---|
| Dilution factor | card | The numeric dilution applied to a specimen before measurement; what `compute.import.dilution_source_precedence` resolves per well. |
| Specimen dilution factor | — | **Merged into "Dilution factor" above** — not a materially distinct term in this app's vocabulary; don't seed it separately. |
| Standard curve | card | "The reference dilution series used to fit each curve" (`table_doc('xmap_standard')`); links out to `compute.standard_curve.model_engine` for how the curve is actually fitted. |
| Calibration | card | General term for the standard-curve-fitting + concentration-back-calculation process as a whole; mostly covered by the Standard curve and model-forms entries, doesn't need separate depth. |
| Dilution-source precedence / `dilution_src` | card | Short card pointing to the full `compute.import.dilution_source_precedence` entry (mapping table) rather than duplicating its content — the card's job is just "there's a priority order; click through for it." |

## Model & statistics vocabulary

| Term | Card / Entry | Notes |
|---|---|---|
| Calibration curve model forms (4PL / 5PL / Gompertz) | **entry** — `glossary.model_forms` | The plan's placeholder "4PL/5PL" isn't the actual terminology (per `02-curveR-topic-index.md`): the real forms are `logistic4`, `logistic5`, `loglogistic4`, `loglogistic5`, `gompertz4` (curveRcore). Genuinely needs real explanation (5 distinct shapes), not a 2-sentence card — curveRcore's `model-forms.Rmd` §"Model Selection Considerations" is **directly linkable** practical guidance to bridge from. `compute.standard_curve.model_engine` (mapping table) should `see_also` this entry rather than re-explain the forms itself. |
| Richards (model alias) | card | Resolves a real ambiguity Phase 2 found: `curveRbayes`'s own DESCRIPTION calls one of its models "Richards," but no function or object is ever literally named that in code — it's `loglogistic5`. A one-line clarifying card prevents confusion for anyone cross-referencing package docs. Link into `glossary.model_forms`. |
| Fit engine (Frequentist vs. Bayesian) | card | Which worker path runs (`curveRfreq` vs. `curveRbayes`) — links to `compute.standard_curve.model_engine` for the full decision. |
| AIC (Akaike Information Criterion) | card | Frequentist model-selection criterion; "lower is better among eligible models." |
| LOO-CV (leave-one-out cross-validation) | card | Bayesian model-selection analogue to AIC — bridge it exactly that way ("the Bayesian equivalent of AIC") rather than explaining LOO-CV from first principles, per `02-curveR-topic-index.md`'s own framing. |
| Eligibility gating / eligibility gates | **entry** | The same four gates are explained three times across curveRcore/curveRfreq/curveRbayes vignettes with framework-specific detail (per `02-curveR-topic-index.md`) — worth one consolidated app-side entry bridging from curveRcore's canonical version rather than picking one package's. `data.results.eligibility_gates` (mapping table) should link here. |
| Measurement error | **entry** | Already has substantial existing content at `help/settings/precision_measurement_error.md` — the glossary card here should **link out to that existing note**, not duplicate it. Also the resolution for the plan's seeded "harmonization" below. |
| Harmonization | card | Per `02-curveR-topic-index.md`: not a discrete vignette topic anywhere in the ecosystem — it's the *purpose* the precision-grid + precision-weighting machinery serves, not a documented concept on its own. Define as a short card pointing at `glossary.measurement_error` and `qc.precision_weights.method`, don't expect a dedicated source section to exist. |
| Concentration | card | The back-calculated value read off the fitted curve for a sample. |
| se_concentration | card | Standard error of the back-calculated concentration; the input to the precision profile / `pcov`. |
| pcov | card | Precision coefficient-of-variation series; links to `help/settings/precision_measurement_error.md` (existing) and `data.results.fitted_grid`. |

## Precision & QC metric vocabulary

| Term | Card / Entry | Notes |
|---|---|---|
| Precision weight | **entry** | Statistically substantial on its own; ties directly to `qc.precision_weights.method` (mapping table). |
| Blank / background subtraction | **entry** | More substance than a card: curveRcore documents distinct blank-handling **options** (ignored / included / subtracted / 3x / 10x) that are very likely a settings-cascade param (per `02-curveR-topic-index.md`) — this is a real decision, not just a definition. The Appendix C.2 table in `curveR-methods-comparison.Rmd` is a better bridging source than the main vignette's prose. |
| LLOQ / ULOQ | card | Lower/upper limits of quantification; part of the LOD/RDL/MDC/LOQ family at `data.results.diagnostics_loq`. |
| LOD (limit of detection) | card | **Amended 2026-10-09:** `curveRmetrics` removed from scope. Source is now `curveRcore::compute_detection_limits()` / `getting-started.Rmd` (needs bridging — not the directly-linkable definition the old `curveRmetrics` source had; write a short card rather than reusing vignette prose verbatim). |
| RDL (reliable detection limit) | card | Same source pattern as LOD (amended). |
| MDC (minimum detectable concentration) | card | Same source pattern as LOD/RDL (amended). |
| LOQ / Shape-LOQ vs. pcov-LOQ | **entry** | `02-curveR-topic-index.md` explicitly flags this as "worth its own glossary/concept entry rather than burying it inside the LOQ topic" — two genuinely different LOQ families (geometric/shape-based vs. precision-based). **Amended 2026-10-09:** now documented in curveRcore and curveRfreq only (`curveRmetrics` removed from scope). `data.results.diagnostics_loq` should `see_also` this entry. |
| Inflection point | card | **Amended 2026-10-09:** the directly-linkable `curveRmetrics` definition this card was going to reuse is no longer in scope, and no in-scope curveR package defines it either — confirmed it IS surfaced in the app (`data_dictionary.R`'s `calib_diagnostics` table description names it directly, and `01-ui-inventory.md`'s Diagnostics/LOQ row already lists it as a glossary term), so don't drop it — write the card from scratch: the point on the fitted curve of maximum slope / steepest response change. |
| Frequentist vs. Bayesian agreement | card | Only needed if/when the app surfaces a side-by-side comparison view (`study_overview.sample_estimate_quality` is the closest candidate today); `curveR-methods-comparison.Rmd`'s Discussion subsection is ready-to-link prose for this if/when it's built. |

## Infrastructure & scope vocabulary

| Term | Card / Entry | Notes |
|---|---|---|
| Experiment / study / project scope hierarchy | **entry** | Substantial: the cascade's inheritance model (project → study → experiment → feature → antigen), how a setting at one tier is inherited or overridden below it, and the `.validate_scope_ladder()` mechanics the original plan named. Underlies nearly every row in `settings.calibration.overview` and deserves to be understood before any individual setting is. |
| Project access key | entry → see mapping table | `glossary.access_key` — already a full mapping-table row. |
| Curve ID / curve registry / natural key | **entry** | `data.registry.curve_lookup` (mapping table) is this entry's attachment point; substantial enough (the 10-column natural key composition) to warrant real explanation, not a card. |
| Compute cluster | card | Infrastructure-flavored, not statistical, per `01a-decision-points.md` §5 — keep it short: "which deployed i-spi-compute stack your jobs run on; usually you don't need to touch this." |
| `multiplate_group_id` | card | Technical column term appearing directly in the Data tab (`calib_weights_fit`); short definitional card only. |

---

## Acceptance check

Every term from the plan's §8.2 seed list is accounted for above — either seeded directly, corrected to actual terminology (4PL/5PL → the five real model-form names), or explicitly merged/resolved ("specimen dilution factor" merged into "dilution factor"; "harmonization" resolved as a card pointing at existing/adjacent entries rather than a dedicated source). Every additional jargon term found while re-reading `01-ui-inventory.md` and `01a-decision-points.md` is included. Every term is marked card or entry — none left open. Every "entry" term either matches a `help_id` already present in `05-help-content-mapping.csv` or is flagged here as a new, mapping-table-worthy entry Phase 5's content-authoring pass should add a row for (`glossary.model_forms`, `glossary.measurement_error`, `glossary.scope_hierarchy`, `glossary.eligibility_gating`, `glossary.blank_background_subtraction`, `glossary.loq_families`, `glossary.precision_weight`, `glossary.curve_registry`) — eight new glossary-category rows beyond the 40 in the mapping table, not yet added there since they weren't anchored to a specific UI control in Phase 1's inventory (they're referenced *from* multiple UI rows via `see_also`, not owned by any one of them).
