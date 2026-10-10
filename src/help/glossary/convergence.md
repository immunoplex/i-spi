---
id: glossary.convergence
title: Convergence (fit diagnostics)
audience: both
category: glossary
see_also: [glossary.frequentist_approach, glossary.bayesian_approach, glossary.hierarchical_priors, glossary.robust_nonlinear_estimation, compute.standard_curve.bayes_draws]
references:
  - text: "curveRcore::tidy_fit_diag() — the per-fit diagnostic columns stored for every candidate model (frequentist and Bayesian)."
  - text: "Vehtari A, Gelman A, Simpson D, Carpenter B, Bürkner PC (2021). Rank-normalization, folding, and localization: An improved Rhat for assessing convergence of MCMC. Bayesian Analysis 16(2):667-718. (general interpretation of Rhat/ESS/divergences below is standard MCMC-diagnostics practice, not drawn from this app's own documentation)."
    doi: "10.1214/20-BA1221"
---
**Convergence** means the fit's optimizer (frequentist) or sampler
(Bayesian) actually found a trustworthy answer, as opposed to stopping
early, getting stuck, or exploring the wrong region of parameter space. A
model can run without crashing and still have failed to converge — that's
exactly what these diagnostics are for catching.

**Frequentist** fits report `optimizer_code` (did the optimizer report
success), `gradient_norm` (how close to flat the objective's gradient is at
the reported optimum — large means it isn't really a local minimum yet),
`hessian_condition_number` (how well-determined the parameters are; a very
large value means some parameter combinations are nearly unidentifiable
from this data), and `rel_tol_achieved`.

**Bayesian** fits report `rhat_max` (compares between-chain and
within-chain variance for every parameter; values should be very close to
1 — appreciably above ~1.01 means the chains haven't mixed to the same
distribution and the posterior summary isn't trustworthy yet), `ess_bulk_min`/
`ess_tail_min` (effective sample size — how many genuinely independent
draws the correlated MCMC chain is worth; low ESS means wide uncertainty on
the posterior summaries themselves, even with many raw draws), `n_divergent`/
`pct_divergent` (transitions the sampler flagged as numerically unreliable,
often concentrated in a hard-to-sample region of the posterior — any
nonzero count deserves a closer look, not just a global low percentage),
`max_treedepth_hit` (the sampler was cut off exploring, a sign it's
struggling with scale or geometry), and `ebfmi_min` (energy-based
diagnostic for how well Hamiltonian Monte Carlo is exploring the full
posterior).

::: more
None of these diagnostics being "bad" proves the scientific answer is
wrong — they only say the numerical machinery struggled, which should lower
confidence in that fit and motivate checking the eligibility gates and
comparing against the alternative engine, not override a pass/fail decision
on its own.
:::
