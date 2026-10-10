---
id: glossary.bayesian_approach
title: Bayesian fitting approach
audience: both
category: glossary
see_also: [glossary.frequentist_approach, glossary.hierarchical_priors, glossary.robust_nonlinear_estimation, glossary.model_selection_bayesian, glossary.posterior_predictive, glossary.convergence, compute.standard_curve.model_engine, compute.standard_curve.bayes_draws, glossary.model_forms, glossary.se_concentration]
references:
  - text: "curveRbayes package (fit_calibration_bayes()) — Bayesian hierarchical standard-curve fitting in Stan with data-adaptive priors, a robust Student-t likelihood, LOO-CV model selection, and posterior predictive concentration estimation."
---
The **Bayesian** engine fits the same family of nonlinear models (logistic,
Gompertz, Richards) but as a single hierarchical model spanning multiple
curves at once, using Stan (Hamiltonian Monte Carlo). Curve-level parameters
are drawn from shared population-level distributions
([[glossary.hierarchical_priors|hierarchical priors]]) that are themselves
estimated from the data, and the observation model uses a heavy-tailed
Student-t likelihood ([[glossary.robust_nonlinear_estimation|robust
estimation]]) rather than assuming every well is equally trustworthy. Model
choice is by [[glossary.loo|LOO-CV]] — see
[[glossary.model_selection_bayesian|Bayesian model selection]].

Choose Bayesian when you want full uncertainty propagation (a
[[glossary.posterior_predictive|posterior distribution]] for every sample,
not just a point and a standard error), or when plates are sparse enough
that pooling information across curves meaningfully stabilizes the fit.
The cost is runtime (MCMC sampling takes much longer than a least-squares
fit) and the need to check sampler [[glossary.convergence|convergence
diagnostics]] before trusting a result.

::: more
Because the hierarchical model shares information across curves, a
well-identified curve can help stabilize a poorly-identified sibling curve
in the same fit (partial pooling) — this is the main practical advantage
over fitting every curve in isolation, at the cost of a fit that is no
longer just "this one plate's data in, this one plate's answer out."
:::
