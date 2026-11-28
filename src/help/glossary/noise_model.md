---
id: glossary.noise_model
title: Noise model (constant; additive + proportional)
audience: user
category: glossary
topic: Precision and uncertainty
see_also: [settings.noise_model, glossary.measurement_error, glossary.response_offset]
---

A **noise model** describes how the scatter of a single reading changes with the
response. **Constant** noise has one spread at every level of the fitting scale.
**Additive + proportional** noise has an absolute floor (sigma_add, in response
units) plus a part proportional to the response (cv):
SD = sqrt(sigma_add² + (cv × response)²). The second suits absorbance, whose
noise is nearly constant near the blank and grows with the reading. See
[[settings.noise_model|the noise model setting]].
