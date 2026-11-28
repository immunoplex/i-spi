---
id: glossary.response_offset
title: Response offset (c)
audience: user
category: glossary
topic: Curve models and fitting
see_also: [settings.noise_model, glossary.blank_wells, glossary.standard_wells]
---

A log response cannot be taken of a value ≤ 0, which happens with absorbance
after wavelength correction. If any included standard, control or test sample
is ≤ 0, I-SPI adds one small constant **c** to every well of the fit group:
c = 0.001 − (smallest response), so the smallest value sits 0.001 response
units above zero and every well is kept and estimated. Blanks never decide c.
When nothing is ≤ 0, c = 0 and nothing changes. c is recorded with the fit and
shown on the curve's Parameters tab; the curve plot's axis labels are in the
original units (c removed).
