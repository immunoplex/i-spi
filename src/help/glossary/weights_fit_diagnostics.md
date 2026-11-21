---
id: glossary.weights_fit_diagnostics
title: Precision-weights fit diagnostics (both models)
audience: user
category: glossary
topic: Precision and uncertainty
see_also: [data.results.precision_weights_fit, qc.precision_weights.model_choice, qc.precision_weights.additive_options, glossary.identifiability, glossary.convergence]
references:
  - text: "curveRweights reference: fit_saturated_weight() and fit_additive_weight(), value $diagnostics"
    url: "https://immunoplex.github.io/curveRweights/reference/fit_additive_weight.html"
  - text: "Vehtari A, Gelman A, Simpson D, Carpenter B, Buerkner PC (2021). Rank-normalization, folding, and localization: An improved Rhat for assessing convergence of MCMC. Bayesian Analysis 16(2):667-718."
    doi: "10.1214/20-BA1221"
---

Both weights models are fitted the same way: a Bayesian model of the
calibrated log10 concentrations (one mean per design cell, a plate term, and a
model for the variance), sampled with Stan. "Bayesian" and "frequentist" in the
method choice refer to how the standard curves were calibrated, not to the
weights fit. So both models get the same six diagnostics, stored with each fit:

| Column | Question it answers | Flagged when |
|---|---|---|
| `rhat_max` | Did the chains agree? | above 1.05 |
| `n_divergent` | Could the sampler explore the whole posterior? | any |
| `ess_min` | Were there enough effective draws for stable intervals? | below 400 |
| `ident_sd_ratio` | Did the data, not the prior, set the key scale parameter (kappa for additive, beta1 for multiplicative)? | above 0.8 (weak); above 1 (sampling problem) |
| `resid_sd` | Does the variance model describe the overall scatter? | outside 0.75 to 1.33 |
| `resid_sd_ratio` | Does it describe the scatter equally well at low and high `se`? | outside 0.67 to 1.5 |

The **check** column of the fit table turns these into words. A
**sampling problem** means the fit's numbers cannot be relied on: rerun with
more iterations (Advanced MCMC settings), and for the additive model consider
fixing kappa at 1.

**The residual check** compares each quantified sample's distance from its
fitted mean with the SD the model gave it. If the variance model is right,
these scaled residuals have SD about 1 everywhere. `resid_sd_ratio` divides
their SD in the fifth of samples with the smallest `se` by that in the fifth
with the largest. Above 1, the most precisely measured samples scatter more
than the model allows: they get **too much weight**. That is the typical
failure of the multiplicative model when people really differ, because it has
no floor for real variation. Below 1, the least precise samples get too much
weight. This is the most direct way to see, on your own data, which model
suits it.

::: more
The scaled residuals use a robust SD (1.4826 x the median absolute
deviation), so a few outliers do not set them. Censored samples are left out:
they carry their limit, not a measured value. A blank check for a row means
nothing was flagged; rows fitted before these diagnostics were stored say
"no diagnostics stored". The diagnostics judge the weights fit only, not the
calibration behind the `se` values (see [[glossary.convergence|convergence]]
for the curve fits).
:::
