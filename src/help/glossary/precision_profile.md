---
id: glossary.precision_profile
title: Precision profile
audience: both
category: glossary
see_also: [glossary.pcov, settings.precision_measurement_error, glossary.loq_gating, glossary.assay_dynamic_range]
references:
  - text: "curveRcore::assess_model_eligibility() — formalizes the precision profile as pcov evaluated over a concentration grid."
---
The **precision profile** is [[glossary.pcov|pcov]] plotted as a function
of concentration across a curve's range — typically U-shaped, worst (highest
pcov) near both the low and high ends of the curve and best somewhere in the
middle. It's the basis for where I-SPI draws the LLOQ/ULOQ limits: the
points where the profile crosses the precision budget (the pcov threshold,
default 20%).

See [[settings.precision_measurement_error|the measurement-error setting]]
for the fuller technical explanation of what shapes the profile and how the
include/exclude-measurement-error toggle changes what it represents.
