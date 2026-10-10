---
id: glossary.robust_nonlinear_estimation
title: Robust nonlinear model estimation
audience: both
category: glossary
see_also: [glossary.bayesian_approach, glossary.frequentist_approach, glossary.hierarchical_priors, glossary.convergence, compute.standard_curve.bayes_draws]
references:
  - text: "curveRbayes Stan source, hierarchical_logistic4.stan — observation likelihood y ~ student_t(nu, mu, sigma) with nu ~ gamma(2, 0.1)."
---
"Robust" estimation means the fit is built to tolerate a handful of bad
wells (contamination, a pipetting error, a stray bead-count artifact)
without either (a) being dragged off course by them, or (b) requiring a
separate manual flag-and-exclude step first.

In the [[glossary.bayesian_approach|Bayesian]] engine this is literal: the
observation model is a **Student-t likelihood** (`y ~ student_t(nu, mu,
sigma)`) rather than a normal one, with its own degrees-of-freedom parameter
`nu` estimated from the data. A Student-t distribution has heavier tails
than a normal, so a well whose reading is far from what the curve predicts
contributes less to pulling the fit than it would under a normal
likelihood — it's automatically down-weighted rather than silently treated
as equally informative.

The [[glossary.frequentist_approach|frequentist]] engine's robustness lever
is different in kind: its multi-start Levenberg-Marquardt optimization
guards against a *numerically* bad outcome (a poor local minimum from an
unlucky starting point), not against a statistically influential outlier
well — it has no equivalent heavy-tailed down-weighting step.

::: more
A small estimated `nu` (few degrees of freedom) means the model found the
data genuinely heavy-tailed and is actively down-weighting some
observations; a large `nu` means the data looked essentially normal and the
Student-t likelihood is behaving almost exactly like a normal one — `nu`
being estimated rather than fixed lets the degree of robustness adapt to
each fit rather than being a one-size-fits-all setting.
:::
