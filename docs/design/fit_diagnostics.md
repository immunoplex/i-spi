# Fit diagnostics report — design and growth plan

Status: released 2026-11-11 (tag `v2026.11.11`). Code: `src/fit_diagnostics_report.R`
(pure logic), `src/fit_diagnostics_module.R` (Data > Results > Fit diagnostics tab),
`src/test-fit-diagnostics.R` (94 checks). User-facing text: glossary note
`glossary.fit_diagnostics_report` and how-to note `data.results.fit_diagnostics`
under `src/help/`.

## 1. Why it exists

A colleague reported that Bayesian ELISA fits were fine for up to 5 plates but
"odd" for plates 1–10 (Bionet_new, project 92, study ELISA). The cause was not
plate count. An experiment-level override of `is_log_response = false` had made
the worker fit raw optical density instead of log10, which pins the Student-t
degrees of freedom `nu` at its floor (about 2.1) and drops E-BFMI to about 0.29.
Finding that took reading seven result tables by hand, so the diagnostic
numbers now go into one document, assessed by rules, readable by a person or an
LLM.

Outcome of the case: on log10, `nu` 2.19 → 23.25, E-BFMI 0.29 → 0.69,
ESS 603 → 2585, fit time 179 s → 34 s, no plate flagged as an outlier.

## 2. What it does

1. **Collect** (`fit_diag_fetch`): lookup, groups, resolved settings per
   feature/antigen, overrides with who/when, assay type, standards summary, fit,
   sampler diagnostics, hyperparameters, per-plate parameters, gates, LOO, run.
   Every query is wrapped so a missing table degrades the report instead of
   failing it.
2. **Assess** (`fit_diag_build`): per *fit unit*, run rule checks and produce
   findings plus a strategy.
3. **Present**: Markdown for LLMs (`fit_diag_report_text`), JSON
   (`fit_diag_report_json`), a one-line verdict for the UI
   (`fit_diag_notice`), and a notification when a compute job completes.

Pure functions over data frames; only `fit_diag_fetch` touches the pool. That is
what makes the rules testable without a database and makes the report easy to
extend.

## 3. Key design decisions

- **Fit unit = (multiplate_group_id, job_id).** The worker persists per curve and
  later jobs overwrite curves, so one group can hold curves from several jobs.
  Each unit is assessed separately and keyed `method/<group8>/<job8>`.
- **Response scale is measured, not assumed.** `calib_run.params` does not record
  `is_log_response`, so the scale is inferred by comparing `calib_standards`
  `response_model` against raw / log10 / ln of `assay_response_raw`. The report
  then compares it with the currently resolved setting (`settings-stale`) and
  with the assay type (`scale-vs-assay`, from `xmap_header.assay_response_variable`).
- **Setting provenance.** For each fit-defining setting the report shows the
  resolved value, its tier (system, project, study, experiment, feature,
  antigen), and who/when for overrides. This answers "I changed the study
  setting, why didn't it apply?" (a more specific tier wins).
- **Frequentist is per plate.** Bayesian fits one pooled joint model per group;
  frequentist fits each plate independently. Pooling findings and strategy only
  apply to pooled fits.
- **Diagnose and recommend, never act.** The report changes no settings.
- **Graceful degradation.** Missing tables or columns produce an `info`
  finding or a blank section, never an error.
- **Ids are normalised** (`bit64::integer64` to character) before any joining.

## 4. Finding catalog (version 1)

Severity: `error` > `warn` > `info`. Thresholds live in `FD_THRESH` at the top of
`fit_diagnostics_report.R`; change them there, not in the rules.

| Code | Sev | Fires when | Typical action |
|---|---|---|---|
| `sampler-not-converged`, `freq-not-converged` | error | fit reports no convergence | review data, rerun |
| `sampler-rhat` | warn/err | R-hat > 1.01 / 1.05 | more iterations, check scale |
| `sampler-ess` | warn/err | min ESS < 400 / 100 | more sampling draws |
| `sampler-divergences` | warn/err | any / > 1% of draws | raise `adapt_delta` |
| `sampler-treedepth` | warn | max tree depth hit | reparameterise, check scale |
| `sampler-ebfmi` | warn | E-BFMI < 0.3 | check response scale, heavy tails |
| `freq-hessian` | warn | condition number > 1e8 | SEs unreliable; check standards |
| `no-sampler-diagnostics` | info | diagnostics absent | older run; recompute |
| `eligibility` / `fallback` | warn/err | plates fail gates; fallback model used | review failing plates |
| `mixed-models` | info | plates chose different models | usually fine |
| `heterogeneity-outlier` | warn | a plate's parameter is a robust-z outlier (> 3.5, 5+ plates) | review or exclude the plate |
| `heterogeneity-signal-level` | warn | upper-asymptote spread (max-min)/median > 0.5 | batch by signal level |
| `heterogeneity-inflection` | warn | inflection `c` range > 1 | check dilution/day |
| `weak-plate` | warn | SE/estimate > 0.5 on a plate parameter | review standards |
| `population-wide` | warn | sigma_d / mu_d > 0.5 | plates not exchangeable |
| `noise-heavy-tails` | warn/info | nu < 3 / nu < 6 | check scale first |
| `scale-vs-assay` | warn/info | log scale for absorbance or raw for MFI (or reverse) | align `is_log_response` |
| `scale-mixed` | warn | experiment fitted on mixed scales across methods | recompute |
| `setting-overridden` | warn/info | a more specific tier differs from the study value | confirm intent |
| `settings-stale` | warn | stored fit scale differs from current setting | recompute |
| `loo-pareto-k` | info | high Pareto-k observations | LOO unreliable for them |
| `pooled-size` | info | 8+ plates pooled | one plate affects all |

The strategy (`.fd_strategy`) orders actions: scale/override/stale first, then
systematic gate failures, plate review, batching or `bayes_single_plate` for
pooled groups, `adapt_delta`, more iterations.

## 5. How to grow it (the point of this document)

The aim is a report that gets smarter each time a new trouble pattern is
understood. Working rule: **every confirmed failure becomes a rule, a test and a
line in the case log below.**

Adding a rule:
1. Reproduce the pattern in `mk_data()` in `src/test-fit-diagnostics.R` (a
   synthetic unit with the signature numbers).
2. Add a check inside `.fd_unit()` using `add(code, severity, message, evidence,
   action)`. Put any new threshold in `FD_THRESH`.
3. If the finding implies an order of actions, extend `.fd_strategy()`.
4. Add the code to the table above and a case-log entry below.
5. Run `src/test-fit-diagnostics.R`, `test-help-registry.R`.

Where the next rules will come from:
- **Record fit-defining settings in `calib_run.params`** (worker change in
  `i-spi-compute`): `is_log_response`, `is_log_independent`, model list,
  `adapt_delta`, `bayes_*`. This removes the need to infer the scale and makes
  `settings-stale` exact.
- **Masked/excluded standards**: a finding for a high fraction of masked or
  excluded standard wells with their `mask_reason`/`exclusion_reason`
  (case 1 had 22 of 90 wells masked, 19 as "test"). Not built yet.
- **Standards with no upper plateau** (`at_bound` on `d`, `dynamic_range` zero):
  seen on PTgen_FC3A_1-38 (27 of 38 plates); the report counts gate failures by
  type but does not yet explain the standards cause.
- **Expose `bayes_single_plate`** (exists in the worker, not in app settings) and
  tie it to the pooled-group strategy.
- **Cross-experiment patterns**: compare the same antigen/feature across
  experiments to say "this plate's d is unlike every other run".
- **Thresholds from data**: once enough reports exist, calibrate `FD_THRESH`
  values against outcomes rather than convention.
- **Self-report to the LLM**: keep the Markdown stable and sectioned so prompts
  and downstream tools can rely on it.

## 6. Case log

| Date | Experiment | Signature | Cause | Fix | Rule |
|---|---|---|---|---|---|
| 2026-11-11 | ELISA FHA_FcR2A_1-10 (project 92) | `nu` ≈ 2.1–2.2 at floor, E-BFMI ≈ 0.29, spread 0.515 in raw units; all raw-scale Bayesian ELISA fits looked alike | experiment-level `is_log_response=false` on absorbance data | set true, recompute | `noise-heavy-tails`, `scale-vs-assay`, `setting-overridden`, `settings-stale` |
| 2026-11-11 | PTgen_FC3A_1-38 (frequentist) | 27 of 38 plates fail gates (`at_bound` d, `dynamic_range` 0) | standards lack an upper plateau | standards review (open) | systematic gate summary |

Open cases: FHA_FcR2A_11-38 and FHA_FcR3A_1-38 (stored fits raw, now inherit
true: recompute); PTgen_Fc2A_1-38 (experiment-level false override remains).

## 7. Known limits

- It can only assess what was persisted; a setting changed after the fit is
  detectable only through the inferred scale.
- Heuristic thresholds, not statistical tests; a clean report is not proof of a
  good fit.
- Reads seven tables per build, so it is built on demand, not on tab open.
- Not exercised in a live browser during development beyond the deployed
  reports users pasted back.
