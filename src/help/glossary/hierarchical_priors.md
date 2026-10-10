---
id: glossary.hierarchical_priors
title: Hierarchical priors
audience: both
category: glossary
see_also: [glossary.bayesian_approach, glossary.robust_nonlinear_estimation, glossary.convergence, compute.standard_curve.bayes_draws, glossary.posterior_predictive]
references:
  - text: "curveRbayes Stan source, hierarchical_logistic4.stan — non-centered parameterization of curve-level parameters around population-level mu/sigma; curveRbayes::compute_dynamic_priors() sets the population-level priors from the observed response range rather than fixed defaults."
---
In the [[glossary.bayesian_approach|Bayesian]] engine, each curve's
parameters (e.g. the four parameters of a logistic curve) aren't fit in
isolation — they're drawn from a shared population-level distribution whose
own mean and spread (`mu`, `sigma`) are estimated from all the curves in the
fit at once. This is the **hierarchy**: individual curves inform the
population distribution, and the population distribution in turn regularizes
individual curves, pulling an under-determined curve's estimate toward what
similar curves in the same fit look like (partial pooling) rather than
letting it float freely on too little data.

The priors themselves are **data-adaptive**: the population-level spread is
computed from the observed response range rather than hard-coded, so the
amount of regularization scales with how much the data can support.

::: more
Implemented as a **non-centered parameterization** — each curve's parameter
is written as `mu + sigma * raw`, with `raw` given a simple standard-normal
prior, rather than sampling the parameter directly from `Normal(mu, sigma)`.
This re-parameterization is a standard Stan technique to avoid the sampler
getting stuck in the narrow "funnel" geometry that a directly-hierarchical
parameterization can produce when `sigma` is small — it's a sampling-
efficiency device, not a change to what's being modeled.
:::
