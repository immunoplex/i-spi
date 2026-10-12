---
id: glossary.censoring
title: Censoring (out-of-range samples)
audience: both
category: glossary
topic: Out-of-range samples
see_also: [glossary.concentration_interval, glossary.exceedance_probability, glossary.pcov, glossary.loq_gating, glossary.lod, compute.standard_curve.out_of_range, data.analysis.downstream_censoring, schema.calib_samples, settings.decision_cutoff, glossary.precision_weight, data.results.samples, study_overview.sample_estimate_quality]
references:
  - text: "curveRcore::classify_censoring() / classify_censoring_multiplate() -- censoring class and bounds for back-calculated samples."
  - text: "Helsel DR (2012). Statistics for Censored Environmental Data Using Minitab and R, 2nd ed. Wiley."
---
A sample outside the quantifiable range is **censored**: its concentration is
not measured imprecisely, it is known only to lie in an interval. A sample
below the LLOQ lies somewhere between 0 and about the LLOQ; a sample above the
ULOQ is somewhere above it, with no upper bound. I-SPI records this as a
`censor_class` and natural-scale, dilution-corrected bounds (`censor_lower`,
`censor_upper`) for every sample:

- **quantified** — inside [LLOQ, ULOQ]; no censoring.
- **lod_to_lloq** — detected but not quantified: between the LOD and the LLOQ (interval-censored).
- **below_lod** — indistinguishable from background: between 0 and the LOD (left-censored).
- **below_lloq** — below the LLOQ when no LOD is available (left-censored).
- **above_uloq** / **saturated** — above the ULOQ / at or above the upper LOD (right-censored). Usually best re-assayed at a higher dilution.

Treat these as intervals, not as points with a large [[glossary.pcov|pcov]].
`pcov` is symmetric and cannot tell the bounded low side from the unbounded
high side, so the same `pcov` can describe a tightly bounded low sample and an
open-ended high one.

::: more
Censored observations should be analysed with censored-data methods —
censored (Tobit) regression, survival-style methods, or a Bayesian censored
likelihood — rather than substituting LLOQ/2 or dropping them, both of which
bias group means. Down-weighting them with [[glossary.precision_weight|precision
weights]] is not a substitute either: their weights are small *because* their
values are extreme, which pulls weighted means away from the censored side.
See [[data.analysis.downstream_censoring|the downstream analysis guide]].
Classification uses the response scale first, so a sample whose response lies
beyond the curve's asymptote (no finite point estimate) is still placed on the
correct side.
:::
