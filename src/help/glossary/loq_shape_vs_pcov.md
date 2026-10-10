---
id: glossary.loq_shape_vs_pcov
title: Shape-LOQ vs. pcov-LOQ
audience: user
category: glossary
see_also: [data.results.diagnostics_loq, settings.precision_measurement_error, glossary.loq_gating, glossary.pcov]
references:
  - text: "curveRcore getting-started vignette, \"Comparing shape-LOQ with pcov-LOQ\""
    url: "https://immunoplex.github.io/curveRcore/articles/getting-started.html"
---
I-SPI computes two different limit-of-quantification families, and they usually don't agree exactly:

- **pcov-LOQ** is precision-based: the range where the %CV of a back-calculated concentration stays under the precision budget (the same [[settings.precision_measurement_error|%CV gate]] described for the precision profile). It reflects parameter uncertainty and measurement noise together.
- **Shape-LOQ** is purely geometric: it marks where the curve's own curvature changes fastest, with no reference to measurement uncertainty at all. It's typically the narrower, tighter range of the two.

::: more
Because the two measure different things, don't expect them to bracket the same interval — a wide gap between them is itself informative: it usually means precision (noise), not curve shape, is what's actually limiting the usable range.
:::
