# Phase 1a — Compute-Decision Deep Dives

Status: Draft for review
Date: 2026-10-09
Scope: §4.2 of `dev/ISPI_HELP_SYSTEM_ASSESSMENT_PLAN.md`

Each section answers the plan's four questions: (1) what changes downstream, (2) existing rationale doc, (3) authoritative curveR source, (4) precedence/resolution order if any.

---

## 1. Per-type Description parsing & dilution-source precedence

**Controls:** Import wizard step 3 (Standards dilution reference, `assay_std_reference_ui.R`) and step 4 (Configure the description field, `assay_shape_ui.R`), for all three assay types (Bead, ELISA, Flow).

**(1) What changes downstream:** Determines, per well, which `dilution` value (and which other identity fields — specimen type, timepoint, arm/group, etc.) gets written into the committed layout and ultimately `assay_response_long`/`plates_map`. This is upstream of all curve fitting — a wrong dilution here corrupts the standard curve itself, not just one sample's readout.

**(2) Existing rationale doc — read in full, do not re-derive:** `RBX_DILUTION_AUTHORITATIVE_SOURCE_PLAN.md` (repo root). Status line says "shipped," verified against two real files, committed to `main` (`a252b29`). Treat this file as the authoritative design doc for this decision point; a help entry here should largely summarize/link it rather than re-explain from scratch.

**Critical correction this assessment must carry forward** (the plan's own §4.2 "known candidates" list names the wrong files): `assay_description_parse.R` (`ai_desc_assign()`) and `assay_description_rule_ui.R` are **dead code for the live bead/.rbx import path** — confirmed by the RBX doc's own "Phase 0 discovery" and independently by `grep -rn "assay_description_rule_ui" src/` turning up zero mount points. The live engine is `assay_shape_rules.R` + `assay_shape_ui.R` (per-shape token **bindings**: `slot | pattern | constant | from_type | ignore`), a different, newer model than the file names the plan expected. Phase 2/5 should index `assay_shape_rules.R`/`assay_shape_ui.R` as the decision-point source, not `assay_description_parse.R`.

**Second correction:** `descriptor_flow.R` currently has `preprocess = TRUE` (confirmed in `01-ui-inventory.md`), meaning Flow imports *do* go through this same pre-processor today. The RBX doc's §7 "out of scope... Flow format has no preprocess flag" is now stale — verify with the user whether this was changed after the RBX doc was written and smoke-tested, since the doc's verification claims (zero manual entries needed for PIH, etc.) were run against Bead only.

**(3) Authoritative curveR source:** None directly — this is app-side import logic upstream of curveR. The downstream consumer is `curveRcore`'s data-transformation/preprocessing utilities (`curveRcore` DESCRIPTION: "data transformation utilities") once a well's dilution is resolved and committed.

**(4) Precedence/resolution order — confirmed shipped, this IS the help content:**
1. **Binary `dilution_map`** (from the instrument file itself) — **Sample (X) and Control (C) wells only, never Standards (S)**. (Standards are excluded because the Bio-Plex binary format's numeric dilution field is always `1` for Standards — a format limitation, not a parsing bug; the XML export, not the raw binary, carries a standard point's true value.)
2. **Text ratio** parsed via the shape-engine's per-shape bindings — any specimen type, unchanged from the general description-parsing behavior.
3. **Experiment-scoped reference table** (`standard_dilution_reference` cascade setting, entered via the Stage 1.5 `assay_std_reference_ui.R` screen) — fills whatever's still unresolved, any type. Keyed by **raw Description text**, not a type-code suffix or position (a lab whose labels have no digits would collapse every Standard to the bare code "S" if position/suffix were used as the key instead).
4. Otherwise: the well is blocked (same validation gate as before this feature existed).

`dilution_src` (`"instrument"` / `"reference"` / `"text"`) is shown inline in the shape-editor's live preview so a user can see which rule resolved each well — this provenance display is itself worth a short help note, since the RBX doc records a real bug (now fixed) where the preview silently showed blank even when the underlying resolution was correct, which "made the feature look completely unhooked."

**Scope note carried from the RBX doc:** bead (`.rbx`/`.srbx`) only was the original target; confirm whether xPONENT and ELISA readers ever populate an `instrument_dilution` column (the RBX doc says every non-bead adapter defaults it to `NA` via a generic fallback) — if so, step 1 of the precedence is bead-only in practice even though the code path is generic.

---

## 2. Standard-curve model selection & fit engine

**Controls:** `std_curve_calc_module.R` — `model_form` (multi-select, "Models to fit (feature settings)" — sourced from the `model_form_list` cascade setting), `fit_engine` (radio: Bayesian / Frequentist, default Frequentist), `bayes_precision` (select, Bayesian-only: draws/chain presets 1000/1500/3000/6000), `include_meas_err` (checkbox, Bayesian-only, default TRUE).

**(1) What changes downstream:**
- `model_form` / `model_form_list`: which of the five `curveRcore` forward models (`logistic4`, `logistic5`, `loglogistic4`, `loglogistic5`, `gompertz4`) are candidates for AIC (frequentist) or LOO-CV (Bayesian) selection. Stored per antigen/feature in settings, consumed by both `curveRfreq` and `curveRbayes` (model-name notation confirmed identical across both — `calib_data_access.R` comment: "the exact strings curveRfreq/curveRbayes... expect").
- `fit_engine`: which worker path runs (`worker_curveR.R` dispatches to `curveRfreq` or `curveRbayes`), fundamentally different statistical machinery (multi-start NLS ensemble vs. Stan hierarchical sampling).
- `bayes_precision`: `DEFAULT_BAYES_SAMPLING`/`chains`/`warmup` passed to the Bayesian worker job — more draws smooth the precision profile at a runtime cost; does not change the model, only estimation precision.
- `include_meas_err`: whether the Bayesian precision profile includes assay measurement noise or curve-uncertainty only — changes LLOQ/ULOQ since those are read off the precision profile.

**(2) Existing rationale doc:** `std_curve_calc_module.R` itself carries an unusually complete rationale as inline comments next to `bayes_help`/`meas_err_help` (see finding below) — these ARE a help entry already, just not using the `help_utils.R` mechanism. `dev/UNDERSTANDING_precision_and_measurement_error.md` likely covers the same ground at more length (not yet read in full for this pass — Phase 2 should cross-check it against the inline modal text for duplication/drift). `calib_data_access.R` (around the `FDA_MODEL_SPECS`/`.fda_inv_*` functions) documents the five models' parameterization and inverse-prediction math directly in code comments — a good source for the model-selection glossary entries.

**Flag — duplicated help content, a live example of the drift problem this whole project exists to prevent:** `std_curve_calc_module.R` has its own hand-rolled help mechanism — `actionLink(ns("bayes_help"), "?")` / `actionLink(ns("meas_err_help"), "?")`, each wired to an `observeEvent` that calls `shiny::showModal(shiny::modalDialog(...))` with hardcoded prose — **not** `help_utils.R`'s `settings_help_icon()`/`settings_help_content()`. The `meas_err_help` modal's text is near-identical in substance to `src/help/settings/precision_measurement_error.md` (same claims: U-shaped profile, delta-method combination, "ON (recommended)... OFF... honest lower bound," thin-standards caveat). **Open question for the user:** is the `include_meas_err` checkbox here the same underlying setting as the cascade's `include_measurement_error` param, or an independent per-job override? Either way, Phase 4's recommendation should fold this UI location into the unified `help_utils.R` mechanism (the note already exists — `params:` just needs to include whatever this checkbox's key is) rather than leaving two prose copies to drift apart. The `bayes_help` modal (sampling/chains explanation) has no existing cascade-settings equivalent and is a good candidate for a *new* concept note.

**(3) Authoritative curveR source:** `curveRfreq` (`frequentist-quickstart.Rmd`) for the NLS ensemble + AIC selection; `curveRbayes` (`bayesian-quickstart.Rmd`) for Stan hierarchical fitting + LOO-CV; `curveRcore` (`model-forms.Rmd`) for the five forward models' math (shared by both engines) — this is the right link target for "what is 4PL/5PL/Gompertz."

**(4) Precedence/order:** Not applicable in the RBX sense — these are independent choices, not a fallback chain. One ordering note worth capturing: `fit_engine` gates which other controls render (`bayes_precision`/`include_meas_err` only appear when `fit_engine == "bayesian"`), which a help entry should mention so a user isn't confused about "missing" controls under Frequentist.

---

## 3. Precision weighting scheme

**Controls:** `std_curve_weights_module.R` — `weight_method` (radio), `fit_scope` (radio, "Scope"), `scale_predictor` (radio), `chains`/`warmup`/`iter`/`adapt_delta`/`seed` (numeric, Stan tuning), `design_approved` (checkbox gate), plus `precision_weight_panel.R`'s `panel_source`/`panel_method` (display filters on the Summary tab, not compute-decisions).

**(1) What changes downstream:** All feed a `curveRweights` job (`worker_weights.R` on the compute tier — confirmed as a second, dedicated worker script separate from `worker_curveR.R`). `weight_method` and `scale_predictor` determine the joint Bayesian location-scale model curveRweights fits (`sigma_i = phi * se_i^beta1`, per the package DESCRIPTION) to relate calibration-curve precision (`se_concentration`) to residual variance. Output lands in the Data tab's `calib_weights`/`calib_weights_fit` tables.

**(2) Existing rationale doc:** `dev/HANDOFF_precision_weighting.md` and `dev/HANDOFF_precision_weighting.html`, `dev/REPORT_precision_gap.md` — all present in `dev/`, not yet read in full for this pass. These are prime candidates to be the authoritative source Phase 2 links to, or to mine for Phase 5 gap-filling if `curveRweights`'s own vignette (`precision-weighting.Rmd`) turns out to be written for package-developer rather than end-user audience (per the plan's own instruction to check that gap).

**(3) Authoritative curveR source:** `curveRweights` (`precision-weighting.Rmd`), consuming `calibration_result`/`calibration_result_multiplate` S3 objects produced by `curveRfreq`/`curveRbayes` — the DESCRIPTION is explicit about this dependency chain, worth stating in the help entry so a user understands precision weighting always runs *after* a standard curve fit, not instead of one.

**(4) Precedence/order:** Not applicable — independent statistical-method choices, not a fallback chain. Note the `design_approved` gate: a procedural precondition (the fit won't submit until design is approved), not itself a statistical decision — classify as procedural, not compute-decision, in Phase 5's mapping table, with a cross-reference to the statistical controls above it.

---

## 4. Bead Count thresholds — resolved as NOT compute-decision

**Controls:** `numericInput(lower_threshold_val)`, `numericInput(upper_threshold_val)`, `radioButtons(thresholdCriteria_val)` in `bead_count_controls_ui.R`.

Per the plan's own instruction to confirm rather than assume: **confirmed not a compute-decision today.** `grep -rln "bead_count_gc|is_low_bead_count"` across `src/` returns only `bead_count_functions.R` (definition) and `bead_count_analysis_ui.R` (display) — no fit-submission file, no well-exclusion gate, no `assay_shape_rules.R`/`std_curve_calc_module.R` reference reads this flag. It is a QC-display computation the user interprets visually; it does not currently feed curve fitting or sample exclusion. Classify as **conceptual** in Phase 5, not compute-decision — but flag to the user that this may be intentional friction (manual review before acting) rather than a missing feature, and that if a future refactor wires it into fit exclusion, this classification needs to be revisited.

---

## 5. Compute cluster routing

**Control:** `compute_cluster` cascade setting (`param_group = "infrastructure"`, `param_control_type = "textInput"`, added in `i-spi-compute/worker/migrations/006_compute_cluster.sql`), editable via the generic Calibration Settings editor (no dedicated UI screen).

**(1) What changes downstream:** Which deployed i-spi-compute clone (full API+Redis+worker stack, not just a worker replica) a project/study's fit jobs are submitted to, via an `ISPI_COMPUTE_URL__<LABEL>` env var on the app pod. An unrecognized label silently falls back to the default clone with a server-side `warning()` (per the migration's own comment) — this silent-fallback behavior is itself worth a help note, since a typo here produces no user-visible error.

**(2) Existing rationale doc:** The migration file's header comment (`006_compute_cluster.sql`, lines ~30–44) is the only documentation — there is no separate ADR. It directly and authoritatively answers the plan's Phase 3 §6 question about "the cluster of i-spi-compute clones": this confirms the **multiple full i-spi-compute stacks for routing/isolation** interpretation (option b in the plan's framing), not just worker-replica scaling — see `03-deployment-doc-reconciliation.md` for the full write-up. A user-facing help note for this setting should be short and procedural/infrastructure-flavored, not statistical.

**(3) Authoritative curveR source:** None — infrastructure routing, not statistics. No curveR cross-reference needed.

**(4) Precedence/order:** Single value, no precedence chain. One resolution behavior worth documenting: unset → default clone; set-but-unrecognized → default clone + silent server-side warning (not shown to the user in the app).

---

## Summary

Nine compute-decision rows from `01-ui-inventory.md` collapse into **five** deep-dive topics (the Data-tab Results sub-tabs "Fit/model selection," "Diagnostics/LOQ," "Precision weights," "Precision weights fit," and "Sample Estimate Quality" are downstream *displays* of topics 2/3 above, not separate decisions — Phase 5's mapping table should point each of those rows at the relevant topic here rather than getting its own deep-dive).

**Discrepancies flagged against the plan / prior docs, per the plan's "trust the repo" instruction:**
1. Plan's §4.2 names `assay_description_parse.R`/`reader_bead_rbx.R` as the live decision-point source; the actual live engine is `assay_shape_rules.R`/`assay_shape_ui.R` (confirmed dead code, independently verified beyond the RBX doc's own claim).
2. `RBX_DILUTION_AUTHORITATIVE_SOURCE_PLAN.md` §7 says Flow has no `preprocess` flag; `descriptor_flow.R` currently has `preprocess = TRUE` — stale claim, needs re-verification/re-test against Flow specifically.
3. Two independent, textually-overlapping help-content surfaces found for the measurement-error toggle (`help/settings/precision_measurement_error.md` vs. `std_curve_calc_module.R`'s hardcoded `meas_err_help` modal) — a live instance of the exact content-drift risk this whole assessment is trying to get ahead of.
4. Bead Count thresholds, which the plan flagged as "confirm whether user-adjustable and feeds downstream fitting," are confirmed user-adjustable but **not** wired to downstream fitting — resolved as conceptual, not compute-decision.
