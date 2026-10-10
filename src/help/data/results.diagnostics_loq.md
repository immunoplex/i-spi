---
id: data.results.diagnostics_loq
title: Diagnostics / LOQ
audience: user
category: compute-decision
see_also: [glossary.loq_shape_vs_pcov, settings.precision_measurement_error, schema.calib_diagnostics, glossary.rdl, glossary.lod, glossary.inflection_point, glossary.assay_sensitivity, glossary.assay_dynamic_range]
references:
  - text: "curveRcore getting-started vignette, \"$detection_limits — LODs, MDC, and RDL\" and \"Per-model precision grids and LOQs\" — compute_detection_limits()"
    url: "https://immunoplex.github.io/curveRcore/articles/getting-started.html"
---
One row per curve per method, holding the quality limits that say how much of the curve's range is actually trustworthy: **LLOQ/ULOQ** (the lower and upper limits of quantification — see [[glossary.loq_shape_vs_pcov|the two LOQ families]] this app computes), **limits of detection**, **RDL** (reliable detection limit), the curve's **inflection point**, and the thresholds those limits were computed against. Concentrations are given both on the natural scale and log₁₀.

A result outside LLOQ–ULOQ isn't necessarily wrong, but it means the assay's own precision can't vouch for it the way it can for a result inside that range — treat values near or outside these limits with more caution.

::: more
Limits of detection are derived from the confidence interval on the curve's lower/upper asymptotes (how far a response has to be from background before it's distinguishable from noise), while MDC and RDL come from inverting confidence-interval-adjusted versions of the fitted curve itself — two different conservatism choices for "can we trust a detection this close to the floor/ceiling."
:::
