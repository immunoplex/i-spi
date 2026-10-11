---
id: settings.decision_cutoff
title: Decision cutoff & interval draws
audience: both
category: compute-decision
params: [decision_cutoff, n_interval_draws]
see_also: [glossary.exceedance_probability, glossary.concentration_interval, glossary.censoring]
---
**Decision cutoff** is an optional concentration threshold — for example a
protective antibody level — in the same dilution-corrected units as
`final_concentration`. When it is set, every sample gets `p_below_cutoff`, the
probability its concentration is below the cutoff
([[glossary.exceedance_probability|threshold probabilities]]). Leave it unset
for none. It is usually set per antigen, since cutoffs are antigen-specific,
and takes effect the next time the curve is computed.

**Interval draws** sets how many Monte Carlo draws each sample gets for
[[glossary.frequentist_approach|frequentist]] predictive intervals and
probabilities (default 1000; 0 turns them off). Bayesian fits use their
posterior draws instead and ignore this setting.

::: more
The cutoff is read only from the settings cascade (project, study, antigen
overrides), never from the job submission, so a per-antigen value always
applies. The value used for each curve is stored with its diagnostics
(`calib_diagnostics.decision_cutoff`). More interval draws make the 2.5% /
97.5% quantiles and the probabilities more stable at the cost of fit time;
1000 is enough for probabilities to about ±0.02.
:::
