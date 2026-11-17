---
id: data.results.precision_weights
title: Per-sample precision weights
audience: user
category: compute-decision
see_also: [glossary.reliability, glossary.precision_weight, qc.precision_weights.method, data.results.precision_weights_fit, schema.calib_weights]
references:
  - text: "curveRweights precision-weighting.Rmd §\"The model\""
    url: "https://immunoplex.github.io/curveRweights/articles/precision-weighting.html"
  - text: "curveRweights vignette \"Using the weights downstream: which analysis for which question\" (which column to pass to which analysis)"
    url: "https://immunoplex.github.io/curveRweights/articles/downstream-analysis.html"
---

Each row is one sample's [[glossary.precision_weight|precision weight]] from
a completed weighting job: `sigma` is that sample's predicted residual
scale (how imprecise its reading is expected to be, from the fitted
`phi`/`beta1` scale model — see [[data.results.precision_weights_fit|the
weights-fit table]]), `w` is the inverse-variance weight `1/sigma^2`, and
`w_norm` is `w` rescaled for use directly in a downstream weighted analysis.
Higher `w`/`w_norm` means a more reliable sample.

For rows from the **additive** model there are extra columns: `v_meas` (the
measurement variance; this, not `1/w`, is what a random-effects meta-regression
such as `metafor::rma.mv` expects), `tau2` (the between-sample variance),
`reliability` with its interval, and `censor_type`/`bound`. Samples outside the
standard curve are marked censored and have **no `w_norm`**: represent them by
their bound, not by a weight. After you filter to one arm or time point,
re-normalise `w` to mean 1 on the rows you analyse. The
[downstream analysis guide](https://immunoplex.github.io/curveRweights/articles/downstream-analysis.html)
shows how.
