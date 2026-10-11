---
id: glossary.exceedance_probability
title: Threshold probabilities
audience: both
category: glossary
see_also: [glossary.censoring, glossary.concentration_interval, settings.decision_cutoff, data.analysis.downstream_censoring, schema.calib_samples, compute.standard_curve.out_of_range, data.results.samples]
---
Each sample carries the probability that its concentration lies on one side
of a threshold, computed from the same draws as its
[[glossary.concentration_interval|predictive interval]]:

- `p_below_lloq` — below the curve's LLOQ.
- `p_above_uloq` — above the curve's ULOQ.
- `p_below_cutoff` — below a [[settings.decision_cutoff|decision cutoff]] you set (for example a protective level), in dilution-corrected units. Empty when no cutoff is set.

These answer threshold questions directly instead of classifying a point
estimate. A low sample with a huge pcov can still have `p_below_cutoff` close
to 1 — it is almost certainly below the cutoff — which is exactly the
information pcov hides.

::: more
For group summaries, the expected number of samples below the cutoff is the
sum of their probabilities, and its uncertainty follows a Poisson-binomial
distribution. A practical three-way rule: call a sample *below* when
`p_below_cutoff` > 0.975, *above* when it is < 0.025, and report the rest as
*indeterminate* rather than forcing them into a category.
:::
