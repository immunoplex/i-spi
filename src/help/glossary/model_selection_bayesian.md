---
id: glossary.model_selection_bayesian
title: Model selection (Bayesian)
audience: both
category: glossary
see_also: [glossary.loo, glossary.stacking_weights, glossary.bayesian_approach, glossary.model_selection_frequentist, data.results.loo_comparison, schema.calib_loo, compute.standard_curve.model_engine, data.results.fit_model_selection, schema.calib_fit]
references:
  - text: "curveRbayes::compare_models_loo(), compute_loo() — PSIS-LOO per candidate model plus Bayesian stacking weights; curveRcore::select_best_eligible() — the shared eligibility-gate step both engines run before ranking."
---
The [[glossary.bayesian_approach|Bayesian]] engine ranks its candidate
models by **[[glossary.loo|LOO-CV]]** (leave-one-out cross-validated
predictive accuracy) rather than AIC — the same eligibility-gate step the
frequentist engine uses runs first, so an ineligible candidate is excluded
from ranking regardless of how it scores. Alongside the single best model,
the engine also computes **[[glossary.stacking_weights|stacking weights]]**
across all eligible candidates — a way of combining predictions from
several plausible model shapes rather than committing entirely to one.

::: more
LOO and AIC are not on a comparable numeric scale and don't always agree on
a winner for the same data — this is expected, not a bug, since they
approximate different quantities (AIC approximates out-of-sample
prediction error under asymptotic/normality assumptions; LOO estimates it
directly via cross-validation). I-SPI never compares an AIC score against a
LOO score; see [[data.results.loo_comparison|the LOO comparison table]],
which is populated for Bayesian fits only.
:::
