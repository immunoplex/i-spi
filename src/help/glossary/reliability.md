---
id: glossary.reliability
title: Reliability (of a weighted sample)
audience: user
category: glossary
topic: Precision and uncertainty
see_also: [glossary.tau, glossary.kappa, glossary.precision_weight, glossary.measurement_error, glossary.censoring, qc.precision_weights.model_choice, data.results.precision_weights_fit, data.results.precision_weights]
references:
  - text: "curveRweights vignette \"Measurement error in assay data: what the weights mean\""
    url: "https://immunoplex.github.io/curveRweights/articles/measurement-error.html"
  - text: "curveRweights vignette \"Using the weights downstream: which analysis for which question\" (regression calibration and correcting correlations for attenuation)"
    url: "https://immunoplex.github.io/curveRweights/articles/downstream-analysis.html"
---

**Reliability** is the share of a sample's variability that is real biological
signal rather than assay measurement error, a number from 0 (all noise) to 1
(all signal). It is produced by the additive precision-weight model as
`tau² / (tau² + measurement variance)` and reads like an intraclass
correlation: 0.9 means a result that mostly reflects the sample, 0.3 means one
dominated by assay noise.

::: more
Reliability matters most when a measured concentration is used as a
**predictor** (for example antibody level against infection, or one antigen
against another). Noise in a predictor flattens the fitted slope toward zero by
roughly the reliability, so weights alone do not fix it; the curveRweights
functions `reliability_correct()` and `attenuation_correct()` do. See the
[downstream analysis guide](https://immunoplex.github.io/curveRweights/articles/downstream-analysis.html).
Two cautions. With the person term in the model (the default when people
have several samples), real differences *between* people are absorbed by that
term, so the reliability is a **within-person** reliability: how much of the
change from one sample to the next is real (see [[glossary.tau|tau]]). And
reliability follows the `se` of the calibration it was computed from, so
Bayesian and frequentist reliabilities of the same samples can differ widely
(see [[glossary.kappa|kappa]]); compare reliabilities within one method only.

A group-level mean reliability and a per-sample value (with an interval) are
stored in the [[data.results.precision_weights_fit|weights fit]] and
[[data.results.precision_weights|per-sample weights]] tables. Out-of-range
samples are [[glossary.censoring|censored]] and have no weight.
:::
