---
id: qc.precision_weights.additive_options
title: Additive model options
audience: user
category: compute-decision
see_also: [qc.precision_weights.data_check, qc.precision_weights.model_choice, glossary.weights_fit_diagnostics, glossary.tau, glossary.kappa, glossary.identifiability, glossary.censoring, glossary.reliability]
references:
  - text: "curveRweights vignette \"Measurement error in assay data\", sections on censoring, repeated measures and identifiability"
    url: "https://immunoplex.github.io/curveRweights/articles/measurement-error.html#repeated"
  - text: "curveRweights reference: fit_precision_weights() and fit_additive_weight()"
    url: "https://immunoplex.github.io/curveRweights/reference/fit_additive_weight.html"
---

The defaults come from **Check the data**; change them only for a reason.

* **Treat out-of-range samples as censored** (on when censoring was recorded
  and some samples are out of range). A sample below the standard curve is
  known only to be below its limit; censoring uses exactly that, instead of
  the extrapolated value and its large SE. Censored samples get no weight
  (`w_norm` is empty) and are counted in `n_censored`.
* **Account for repeated samples from the same person** (on when people have
  several time points). Adds a person term, so one person's samples are not
  counted as independent people. With it, [[glossary.tau|tau]] is the
  variation *within* a person, and reliability is a within-person reliability.
  Turn it off only if each person really contributes one sample.
* **Measurement-error scale kappa**: *Estimate* (default) lets the data say how
  the curve's `se` relates to the real measurement error. *Fix at 1* takes the
  `se` as the measurement SD and estimates only tau; choose it when the
  [[glossary.identifiability|SE spread is narrow]] or when a previous fit
  reported kappa as weakly identified.
* **Advanced: estimate beta1** (off): beta1 = 1 is the theoretical value.
  Estimating it as well needs many samples over a wide range of `se`, and is
  disabled when the spread is narrow.
* **Iterations** (Advanced MCMC settings, both models): 4000 per chain
  including 1000 of warm-up, as every weights job has used so far. The run-time
  estimate above Submit follows the setting: run time grows in proportion to
  iterations x chains, and censoring makes each additive fit about three times
  slower. Raise the iterations when a fit's check reports a sampling problem
  (see [[glossary.weights_fit_diagnostics|fit diagnostics]]); fixing kappa at 1
  is the other remedy, but it moves any extra measurement error into tau.

The options used are stored with each fit (`fit_options`) and included in the
download bundles.

::: more
**Several readings of one sample** (a person at a time point read at two
dilutions or on two plates) are counted as separate samples, which overstates
the information they carry. A sample term is not offered: in the current model
it would absorb the real variation and push tau toward zero, removing the
ceiling on the weights. Check the data flags such selections.
:::
