---
id: data.results.precision_weights_fit
title: Precision-weighting model fit (phi/beta1)
audience: user
category: compute-decision
see_also: [glossary.weights_fit_diagnostics, glossary.reliability, glossary.precision_weight, qc.precision_weights.method, data.results.precision_weights, schema.calib_weights_fit]
references:
  - text: "curveRweights precision-weighting.Rmd §\"Interpreting phi and beta1\""
    url: "https://immunoplex.github.io/curveRweights/articles/precision-weighting.html"
  - text: "curveRweights vignette \"Measurement error in assay data: what the weights mean\" (tau, kappa, reliability, identifiability)"
    url: "https://immunoplex.github.io/curveRweights/articles/measurement-error.html"
---

One row per multiplate group × method: the fitted scale-model parameters
behind every sample's [[glossary.precision_weight|precision weight]] in
that group. **phi** is the baseline scaling factor — phi = 1 means the
precision index is already a well-calibrated residual SD; phi > 1 means
there's excess variance beyond what the curve predicts (plate effects,
biological scatter); phi < 1 is unusual. **beta1** is the precision
exponent — beta1 = 1 is the textbook relationship; beta1 > 1 means
imprecise samples get down-weighted more aggressively than theory
predicts; beta1 near 0 means the precision index carries little
information and weights end up close to uniform. `interpretation` gives a
plain-language label for this combination; `n_fit`/`n_eff` and
`weight_ratio` describe how much data the fit had and how much the weights
actually vary.

When the **additive** model was used (`model` = additive), the row instead
reports [[glossary.tau|tau]] (SD of the real variation the design cell does not
explain, log10; within a person when the person term was used),
[[glossary.kappa|kappa]] (measurement-error scale; 1 means the curve's own `se`
is the true measurement SD), the group's mean
[[glossary.reliability|reliability]], the number of censored samples, an
`identifiability` note, and `fit_options` (the options the fit used). tau,
kappa and reliability are specific to the calibration method: do not compare
Bayesian with frequentist values.

Both models also store the same six
[[glossary.weights_fit_diagnostics|fit diagnostics]]: `rhat_max`,
`n_divergent`, `ess_min` (did the sampler converge), `ident_sd_ratio` (did the
data rather than the prior set kappa or beta1) and `resid_sd`,
`resid_sd_ratio` (does the variance model describe the scatter across the
range of `se`). The **check** column puts them into words, for every row:

* *sampling did not converge*: do not rely on the weights; rerun with more
  iterations (for the additive model, or with kappa fixed at 1);
* *posterior wider than its prior*: a sampling problem is likely;
* *kappa* or *beta1 weakly identified*: set largely by the prior (for the
  additive model, tau and reliability are then provisional);
* *kappa fixed at 1*, or *kappa outside 0.5 to 2* (the curve's `se` is not a
  calibrated measurement SD and the reliabilities rest on kappa);
* *low-SE* (or *high-SE*) *samples get too much weight*, or *residual scatter
  N times what the model expects*: the variance model does not fit this group;
* *no diagnostics stored*: the fit predates them.

::: more
`phi`/`beta1` are estimated jointly with a saturated cell-means location
model over the design columns chosen in [[qc.precision_weights.method|the
Compute weights step]], so they reflect genuine scale structure rather
than a misspecified mean. The analogy to classical meta-analysis: phi²
plays the role of between-study heterogeneity (tau²), and
`se_i^(2*beta1)` plays the role of per-observation measurement error —
except phi scales multiplicatively and beta1 is estimated from the data
rather than fixed.
:::
