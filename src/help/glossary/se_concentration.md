---
id: glossary.se_concentration
title: se_concentration
audience: both
category: glossary
see_also: [glossary.pcov, glossary.frequentist_approach, glossary.bayesian_approach, glossary.posterior_predictive, settings.precision_measurement_error, glossary.precision_weight, qc.precision_weights.method, schema.calib_samples, schema.calib_weights]
references:
  - text: "curveRfreq::predict_samples_freq() — propagates fit uncertainty to a back-calculated concentration via the delta method."
  - text: "curveRbayes::predict_samples_bayes() — summarizes the posterior-predictive concentration distribution."
---
**se_concentration** is the standard error of a sample's back-calculated
concentration — how uncertain the reading is, on the concentration scale,
once the response has been read off the fitted curve and inverted. It is
the basis for [[glossary.pcov|pcov]] (the same uncertainty expressed as a
%CV) and for [[glossary.loq_gating|LOQ gating]].

**How it's computed differs by engine.** Under the
[[glossary.frequentist_approach|frequentist]] approach, `se_concentration`
comes from the **delta method**: a first-order Taylor approximation that
propagates the fitted curve's parameter uncertainty (and, if
[[settings.precision_measurement_error|measurement error is included]], the
estimated assay noise) through the nonlinear inverse-prediction step
analytically — one formula, evaluated once. Under the
[[glossary.bayesian_approach|Bayesian]] approach there is no such formula:
every posterior draw of the curve (and, if measurement error is included, a
simulated noisy reading) is inverted separately, producing a full
[[glossary.posterior_predictive|posterior-predictive distribution]] of
concentration for that sample — `se_concentration` there is simply the
standard deviation of that distribution.

::: more
The two methods are not guaranteed to agree exactly even on the same data:
the delta method is a local linear approximation (good when the curve is
fairly linear near the sample's response, less reliable near a steep part
of the curve or a concentration far from the standards), while the
Bayesian posterior-predictive approach makes no such linearity assumption.
Both feed the same downstream columns (`se_concentration`, `pcov`,
`pcov_pass`) so the rest of the app treats them identically regardless of
which engine produced them.
:::
