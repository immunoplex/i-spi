---
id: glossary.posterior_predictive
title: Posterior (predictive) distribution for a sample
audience: both
category: glossary
see_also: [glossary.bayesian_approach, glossary.se_concentration, glossary.pcov, glossary.hierarchical_priors, compute.standard_curve.bayes_draws, schema.calib_samples]
references:
  - text: "curveRbayes::predict_samples_bayes(), extract_curve_draws(), tidy_samples() — back-calculate each sample's concentration from every retained posterior draw of the curve."
---
Under the [[glossary.bayesian_approach|Bayesian]] engine, a test sample's
concentration isn't produced as a single number with a plus-or-minus — it's
produced as a full **posterior-predictive distribution**: the fitted curve
is sampled many times (once per retained MCMC draw, each draw itself a
full, internally-consistent version of the curve), and the sample's
response is inverted through every one of those curve draws separately.
The result is a distribution of plausible concentrations for that sample,
not a single estimate with an assumed error shape around it.

Everything reported elsewhere — [[glossary.se_concentration|
se_concentration]], [[glossary.pcov|pcov]], the credible interval shown on
a sample's result — is a **summary** of this distribution (its standard
deviation, its quantiles), not a separately-derived quantity.

::: more
Because the distribution is empirical (built from actual draws, not a
formula), it can be skewed or asymmetric where a curve's response-to-
concentration mapping is itself nonlinear near that sample's reading —
something a single symmetric standard error, as produced by the
[[glossary.frequentist_approach|frequentist]] delta method, cannot
represent.
:::
