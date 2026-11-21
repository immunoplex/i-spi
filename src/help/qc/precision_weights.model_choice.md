---
id: qc.precision_weights.model_choice
title: Which precision-weights model?
audience: user
category: compute-decision
see_also: [qc.precision_weights.data_check, qc.precision_weights.additive_options, glossary.weights_fit_diagnostics, qc.precision_weights.method, glossary.tau, glossary.reliability, glossary.precision_weight]
references:
  - text: "curveRweights vignette \"Measurement error in assay data\", section \"Additive versus multiplicative\""
    url: "https://immunoplex.github.io/curveRweights/articles/measurement-error.html#vs"
  - text: "curveRweights vignette \"Using the weights downstream: which analysis for which question\""
    url: "https://immunoplex.github.io/curveRweights/articles/downstream-analysis.html"
---

Both models give every sample a weight, `w = 1/sigma²`, from the precision the
calibration curve reports for it (`se`). They differ in what they assume makes
up a sample's variance.

* **Multiplicative** (the original model): all of a sample's variance is
  measurement, `sigma = phi * se^beta1`. A very precisely measured sample can
  get a very large weight, with no ceiling.
* **Additive measurement-error**: variance is real variation plus measurement,
  `sigma² = tau² + kappa² * se²`. No weight can exceed `1/tau²`, each sample
  gets a [[glossary.reliability|reliability]], and out-of-range samples can be
  [[glossary.censoring|censored]] instead of weighted.

| Situation | Choose |
|---|---|
| Comparing groups or time points, and people differ | Additive |
| Samples outside the standard curve (censoring recorded) | Additive |
| Assay noise dominates and people barely differ | Either; they agree |
| Reproducing an analysis made with earlier weights | The model used then (Multiplicative for weights made before 2026-11) |

The **Check the data** step above the choice lists the facts that matter and
gives a one-line recommendation. It never switches the model for you.

**After a run**, both models are judged the same way: the fit table's check
column reports [[glossary.weights_fit_diagnostics|sampling problems and a
residual check]] for every group. If the multiplicative fit says *low-SE
samples get too much weight*, the additive model is the better choice for that
experiment.

::: more
**What a run does.** Each method (Bayesian, frequentist) keeps one set of
weights per sample. Running either model replaces the stored set for that
method and selection; the `model` column of the results records which model
made it, and the download bundles say so too. To go back, run the other model
again: the multiplicative model with the same settings **and the same scope**
reproduces its earlier weights exactly. A different scope (one antigen instead
of the whole experiment) gives the same answer up to sampling noise (about 0.1%
in phi and beta1), not identical numbers, because the sampler takes a
different path.

**Why the additive model is preferred for group comparisons.** If real
person-to-person variation exists, a perfectly measured sample still tells you
only about one person. The multiplicative model ignores that and lets the most
precise samples dominate a weighted mean; the additive model puts a ceiling
on their weight. The vignette's simulation shows a weight range of hundreds of
thousands to one under the multiplicative model against about 90 to one under
the additive model, for the same `se` values.

**Bayesian or frequentist calibration?** Run the weights on the calibration
you will analyse. The two methods' `se` values can differ in shape, and the
additive model's split of the variance follows the `se`, so `tau`, `kappa` and
reliability are specific to the calibration method. Do not compare them across
methods.
:::
