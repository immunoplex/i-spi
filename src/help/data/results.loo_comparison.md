---
id: data.results.loo_comparison
title: LOO comparison
audience: user
category: conceptual
see_also: [data.results.fit_model_selection, schema.calib_loo, glossary.loo, glossary.stacking_weights, glossary.model_selection_bayesian]
references:
  - text: "curveRbayes bayesian-quickstart vignette, \"LOO-CV model selection and stacking\" — compute_loo(), compare_models_loo()"
    url: "https://immunoplex.github.io/curveRbayes/articles/bayesian-quickstart.html"
---
Leave-one-out cross-validation (LOO-CV) results, one row per curve / model name — **Bayesian fits only**. `elpd_loo` and its standard error summarize each candidate model's predictive accuracy; `looic` is the LOO information criterion; `elpd_diff` shows how far a candidate trails the best one; `pareto_k_bad` flags observations where the LOO estimate itself may be unreliable.

This table is empty for frequentist fits because LOO-CV doesn't apply there — [[data.results.fit_model_selection|frequentist model selection]] uses AIC instead. The two aren't interchangeable scores on the same scale, which is why I-SPI doesn't try to show one table covering both engines.
