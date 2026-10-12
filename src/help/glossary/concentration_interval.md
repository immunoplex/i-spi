---
id: glossary.concentration_interval
title: Predictive concentration interval
audience: both
category: glossary
topic: Out-of-range samples
see_also: [glossary.censoring, glossary.exceedance_probability, glossary.posterior_predictive, glossary.se_concentration, glossary.pcov, schema.calib_samples, compute.standard_curve.out_of_range, settings.decision_cutoff, data.analysis.downstream_censoring, data.results.samples]
references:
  - text: "curveRcore::summarize_conc_draws() -- per-sample quantiles and probabilities, shared by both engines."
---
Every sample has a 95% interval for its concentration: `conc_q_lo`,
`conc_q_med` and `conc_q_hi` (2.5%, 50% and 97.5%), on the natural,
dilution-corrected scale — the same scale as `final_concentration`.

Unlike `se_concentration` and [[glossary.pcov|pcov]], the interval is
**asymmetric** and keeps the direction of the uncertainty. A sample below the
LLOQ typically gets a lower bound of 0 and a tight upper bound — useful
information even when its pcov is very large. A sample above the curve's top
gets an upper bound of "unbounded" (`conc_q_hi` is empty).

::: more
The interval comes from many back-calculations of the same sample: one per
posterior draw for [[glossary.bayesian_approach|Bayesian]] fits, and one per
Monte Carlo draw from the fitted parameter distribution plus residual noise
for [[glossary.frequentist_approach|frequentist]] fits. A draw whose response
falls beyond that draw's asymptote has no finite concentration; it is kept as
censored (concentration 0, or unbounded) rather than dropped.
`frac_draws_below_a` / `frac_draws_above_d` report how many draws that
affected. `se_concentration` and pcov are computed the older way, from the
invertible draws only, so near the ends of the curve they understate the
spread — the interval does not.
:::
