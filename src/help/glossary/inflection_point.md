---
id: glossary.inflection_point
title: Inflection point
audience: both
category: glossary
see_also: [glossary.assay_sensitivity, data.results.diagnostics_loq, schema.calib_diagnostics, glossary.model_forms]
references:
  - text: "curveRcore::compute_inflection() — closed-form inflection point (log10 concentration and response) for the fitted model."
---
The **inflection point** of a fitted calibration curve is the concentration
(on the log10 scale) at which the curve's steepness is at its maximum —
where the response changes fastest per unit change in concentration, and
for a symmetric sigmoid like the standard 4-parameter logistic it sits at
the curve's midpoint between the lower and upper asymptotes. I-SPI computes
it analytically (a closed form from the fitted parameters), not by
numerically searching the curve.

Its main practical use is as the basis for
[[glossary.assay_sensitivity|assay sensitivity]] — the slope of the curve
evaluated at exactly this point.
