---
id: glossary.pcov
title: pcov
audience: both
category: glossary
topic: Precision and uncertainty
see_also: [glossary.se_concentration, glossary.precision_profile, glossary.loq_gating, settings.precision_measurement_error, glossary.loq_shape_vs_pcov, glossary.posterior_predictive, qc.precision_weights.method, schema.calib_grid, schema.calib_samples, glossary.censoring, glossary.concentration_interval, compute.standard_curve.out_of_range]
references:
  - text: "curveRcore::pcov_from_se() -- pcov = se_concentration x ln(10) x 100, capped at cv_x_max (default 150)."
---
**pcov** is the percent coefficient of variation of a back-calculated
concentration. It is computed from [[glossary.se_concentration|se_concentration]],
the standard deviation of the **log₁₀** concentration:
`pcov = 100 × ln(10) × se_concentration` — for small errors, the %CV of the
concentration itself — capped at 150%. It is the number I-SPI's precision QC is
built around: plotted against concentration it traces the
[[glossary.precision_profile|precision profile]], and the LLOQ and ULOQ are
where that profile crosses the `pcov_threshold` (default 20%), which in turn
drives [[glossary.loq_gating|LOQ gating]].

The precision profile is worst near both ends of the curve because the curve
flattens there: a small change in response corresponds to a large change in
concentration, so `se_concentration` itself grows.

::: more
**What pcov does not tell you.** pcov is one symmetric number on the log
scale, where both ends of the curve are open-ended. It cannot distinguish a
sample below the LLOQ — whose concentration is bounded between 0 and about the
LLOQ — from one above the ULOQ, which has no upper bound, and its cap hides how
large the uncertainty really is at the extremes. For out-of-range samples use
the [[glossary.censoring|censoring class]] and the asymmetric
[[glossary.concentration_interval|predictive interval]] instead.

Computed identically from `se_concentration` regardless of engine, so a
[[glossary.frequentist_approach|frequentist]] and a
[[glossary.bayesian_approach|Bayesian]] pcov are on the same scale; only the
uncertainty estimate feeding into it differs.
:::
