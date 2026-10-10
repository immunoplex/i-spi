---
id: glossary.loo
title: LOO-CV (leave-one-out cross-validation)
audience: both
category: glossary
see_also: [glossary.model_selection_bayesian, glossary.stacking_weights, glossary.aic, data.results.loo_comparison, schema.calib_loo, schema.calib_fit]
references:
  - text: "Vehtari A, Gelman A, Gabry J (2017). Practical Bayesian model evaluation using leave-one-out cross-validation and WAIC. Statistics and Computing 27:1413-1432."
    doi: "10.1007/s11222-016-9696-4"
  - text: "curveRbayes::compute_loo() — extracts the log_lik generated quantity and computes PSIS-LOO via the loo package."
---
**LOO-CV** estimates how well a Bayesian model would predict each
observation if that observation had been left out of fitting — true
leave-one-out cross-validation would mean literally refitting the model
once per data point, which is prohibitively slow for an MCMC fit, so
I-SPI's Bayesian engine uses **PSIS-LOO** (Pareto-smoothed importance
sampling), a fast, accurate approximation computed from a single fit's
posterior draws.

The headline numbers (see also
[[data.results.loo_comparison|the LOO comparison table]]): `elpd_loo` is
the expected log predictive density (higher is better — the opposite
direction from AIC); `looic` is the same quantity on an information-
criterion scale (`-2 * elpd_loo`, so lower is better there, like AIC);
`elpd_diff` shows how far a candidate trails the best model;
`pareto_k_bad` flags individual observations where the importance-sampling
approximation itself may be unreliable, which calls the LOO estimate's
trustworthiness into question for that observation rather than the model.

::: more
A handful of bad `pareto_k` values usually means a few wells are highly
influential on the fit — worth checking those wells directly rather than
assuming the whole LOO comparison is compromised; many bad values across a
model is a stronger signal that the model doesn't fit that curve well.
:::
