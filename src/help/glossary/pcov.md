---
id: glossary.pcov
title: pcov
audience: both
category: glossary
see_also: [glossary.se_concentration, glossary.precision_profile, glossary.loq_gating, settings.precision_measurement_error, glossary.loq_shape_vs_pcov, glossary.posterior_predictive, qc.precision_weights.method, schema.calib_grid, schema.calib_samples]
---
**pcov** is the percent coefficient of variation of a back-calculated
concentration — [[glossary.se_concentration|se_concentration]] expressed as
a percentage of the concentration itself (`100 * se_concentration /
concentration`). It is the single number I-SPI's QC gates are built around:
plotted against concentration it traces the
[[glossary.precision_profile|precision profile]], and compared against the
`pcov_threshold` (default 20%) it drives [[glossary.loq_gating|LOQ
gating]] — `pcov_pass` is simply "is this point's pcov at or below the
threshold."

Because pcov is a ratio, the same absolute `se_concentration` means a
tighter pcov at a high concentration than at a low one — this is why the
precision profile is typically worst (highest pcov) near both ends of a
curve's range, not just near the floor.

::: more
Computed identically from `se_concentration` regardless of engine, so a
[[glossary.frequentist_approach|frequentist]]-derived pcov and a
[[glossary.bayesian_approach|Bayesian]]-derived pcov are on the same scale
and directly comparable — only the uncertainty estimate feeding into it
differs between the two.
:::
