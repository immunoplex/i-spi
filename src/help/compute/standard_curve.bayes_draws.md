---
id: compute.standard_curve.bayes_draws
title: Bayesian sampling resolution & the precision profile
audience: user
category: compute-decision
see_also: [compute.standard_curve.model_engine, glossary.posterior_predictive, glossary.hierarchical_priors, glossary.robust_nonlinear_estimation, glossary.convergence, glossary.bayesian_approach]
---

The Bayesian precision profile is estimated from the model's posterior draws, and
its smoothness is governed by how many draws are kept: the total is chains ×
sampling, where sampling is the number of post-warmup draws per chain. Because the
plotted %CV is a ratio of posterior quantities, too few draws make it wobble from
point to point — much of that jaggedness is Monte Carlo noise, not real assay
behavior. Raising sampling reduces the noise roughly with the square root of the
draw count, so quadrupling draws roughly halves the wobble.

::: more
Increase draws through sampling rather than chains: chains beyond the worker's
core count run sequentially and cost wall time without adding parallelism. One
thing more draws will **not** fix is the blow-up at the very low and very high
ends of the curve — there the response is nearly flat, so back-calculated
concentration is genuinely ill-conditioned, and the high, unstable %CV at the
extremes is real. It reflects the assay's detection limits, not a sampling
artifact.
:::
