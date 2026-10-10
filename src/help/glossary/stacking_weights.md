---
id: glossary.stacking_weights
title: Stacking weights
audience: both
category: glossary
see_also: [glossary.loo, glossary.model_selection_bayesian, schema.calib_loo, data.results.loo_comparison]
references:
  - text: "Yao Y, Vehtari A, Simpson D, Gelman A (2018). Using Stacking to Average Bayesian Predictive Distributions. Bayesian Analysis 13(3):917-1007. (general model-averaging interpretation below is standard Bayesian-stacking theory, not drawn from this app's own documentation)."
    doi: "10.1214/17-BA1091"
  - text: "curveRbayes::compare_models_loo() — computes LOO for each eligible candidate model and Bayesian stacking weights across them."
---
Rather than simply keeping the single best-[[glossary.loo|LOO]] model and
discarding the rest, **stacking** finds a weighted combination of all
eligible candidate models' predictive distributions that together predicts
held-out data better than any one model alone — the weight on each model
(`selection_weight`) reflects how much it contributes to that pooled
prediction, not just its individual rank.

This matters when two or more model shapes fit a curve nearly equally well:
picking one and discarding the other throws away genuine model-form
uncertainty, while the stacked combination reflects that the "true" curve
shape isn't fully resolved by this plate's data.

::: more
Stacking weights are **not** a probability that each model is "the correct
one" (that would be Bayesian model averaging under a different, stronger
set of assumptions) — they are weights chosen specifically to minimize the
stacked combination's predictive error, which is a more robust target when
none of the candidate models is assumed to be exactly true.
:::
