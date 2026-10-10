---
id: data.results.precision_weights_fit
title: Precision-weighting model fit (phi/beta1)
audience: user
category: compute-decision
see_also: [glossary.precision_weight, qc.precision_weights.method, data.results.precision_weights, schema.calib_weights_fit]
references:
  - text: "curveRweights precision-weighting.Rmd §\"Interpreting phi and beta1\""
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
