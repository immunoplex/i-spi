---
id: data.results.fit_model_selection
title: Fit / model selection
audience: user
category: compute-decision
see_also: [compute.standard_curve.model_engine, schema.calib_fit, glossary.aic, glossary.model_selection_frequentist, glossary.model_selection_bayesian]
references:
  - text: "curveRcore getting-started vignette, \"Eligibility gating\" — assess_model_eligibility(), select_best_eligible()"
    url: "https://immunoplex.github.io/curveRcore/articles/getting-started.html"
---
This table lists **every** model candidate that was fitted for a curve, not just the one that was kept — one row per curve / method / model name. Which engine ([[compute.standard_curve.model_engine|frequentist or Bayesian]]) tried which model shapes is set elsewhere; this table is the record of what actually happened when it ran.

`is_best` marks the single row that was selected as the curve's result. `converged` and `eligible` show whether a candidate fit cleanly and passed the eligibility checks before being considered at all — a model can converge but still be ineligible (for example, a parameter estimate sitting on its constraint boundary). `score_type` and `selection_score` record what the winner was ranked on (AIC for frequentist, LOO for Bayesian).

::: more
Selection isn't a plain "lowest score wins" — only eligible candidates are ranked at all. If none pass every gate, the app falls back to the widest-dynamic-range candidate and flags it rather than silently returning nothing; treat a fallback selection with more caution than a normal one.
:::
