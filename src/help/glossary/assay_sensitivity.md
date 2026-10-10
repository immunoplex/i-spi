---
id: glossary.assay_sensitivity
title: Assay sensitivity (slope at inflection)
audience: both
category: glossary
see_also: [glossary.inflection_point, glossary.assay_dynamic_range, schema.calib_diagnostics, data.results.diagnostics_loq]
references:
  - text: "curveRcore::dydx_logistic4() — closed-form derivative of the fitted logistic curve; plotted in I-SPI as \"Sensitivity (slope at inflection)\" (src/plot_functions.R)."
---
**Assay sensitivity**, as I-SPI reports it, is the slope of the fitted
calibration curve evaluated exactly at its
[[glossary.inflection_point|inflection point]] — computed from a closed-form
derivative of the fitted model, not estimated numerically. A steeper slope
there means a bigger change in measured response for a given change in
concentration near the curve's center, which translates into finer
discrimination between two samples with similar concentrations in that
region.

A shallow slope at the inflection point is a sign the assay has limited
power to distinguish nearby concentrations even where the curve is at its
steepest — worth noting alongside, not instead of, the
[[glossary.assay_dynamic_range|dynamic range]] and
[[glossary.precision_profile|precision profile]], which describe range and
precision rather than this local discrimination power.
