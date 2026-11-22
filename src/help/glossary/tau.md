---
id: glossary.tau
title: tau (real variation in the additive weights model)
audience: user
category: glossary
topic: Precision and uncertainty
see_also: [glossary.kappa, glossary.reliability, glossary.identifiability, qc.precision_weights.additive_options]
references:
  - text: "curveRweights vignette \"Measurement error in assay data\": every reading is signal plus noise; repeated measures"
    url: "https://immunoplex.github.io/curveRweights/articles/measurement-error.html#idea"
---

**tau** is the standard deviation (log10 concentration) of the real variation
between samples that the design cell (time point by group) does not explain,
as opposed to measurement error. Its square, `tau²`, is the floor of every
sample's variance, so no weight can exceed `1/tau²`.

::: more
What counts as "real variation" depends on the model. With the person term on
(the default when people have several samples), stable differences *between*
people go into that term, and tau is the variation *within* a person from one
sample to the next. Without it, tau is the whole spread between samples in a
cell. tau depends on the calibration method's `se` (see
[[glossary.kappa|kappa]]), so compare it only within one method.
:::
