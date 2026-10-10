---
id: data.results.precision_weights
title: Per-sample precision weights
audience: user
category: compute-decision
see_also: [glossary.precision_weight, qc.precision_weights.method, data.results.precision_weights_fit, schema.calib_weights]
references:
  - text: "curveRweights precision-weighting.Rmd §\"The model\""
---

Each row is one sample's [[glossary.precision_weight|precision weight]] from
a completed weighting job: `sigma` is that sample's predicted residual
scale (how imprecise its reading is expected to be, from the fitted
`phi`/`beta1` scale model — see [[data.results.precision_weights_fit|the
weights-fit table]]), `w` is the inverse-variance weight `1/sigma^2`, and
`w_norm` is `w` rescaled for use directly in a downstream weighted analysis.
Higher `w`/`w_norm` means a more reliable sample.
