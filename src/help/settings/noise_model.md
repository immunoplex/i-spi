---
id: settings.noise_model
title: Response noise model
audience: both
category: compute-decision
params: [noise_model]
see_also: [glossary.noise_model, glossary.response_offset, glossary.measurement_error, settings.precision_measurement_error, glossary.precision_profile]
references:
  - text: "Rocke DM, Lorenzato S (1995). A two-component model for measurement error in analytical chemistry. Technometrics 37(2):176-184."
  - text: "Belanger BA, Davidian M, Giltinan DM (1996). The effect of variance function estimation on nonlinear calibration inference in immunoassay data. Biometrics 52(1):158-175."
    url: "https://pubmed.ncbi.nlm.nih.gov/8934590/"
---

The **noise model** says how much the standards are expected to scatter around
the fitted curve at each response level. It decides how much each standard
counts in the fit and how wide the precision profile is, so it moves the LLOQ
and ULOQ.

- **Constant** (default): one spread on the fitting scale. On a log response
  that means the same *relative* error everywhere; on a linear response the same
  *absolute* error everywhere. Existing fits used this model.
- **Additive + proportional**: an absolute floor plus a part that grows with the
  response, SD = sqrt(sigma_add² + (cv × response)²), on the raw response. The
  two terms are estimated once per fit group (all plates fitted together) and
  shown on the curve's Parameters tab.

Use **additive + proportional for absorbance** (ELISA OD). Near the blank, OD
noise is a roughly constant few thousandths; higher up it is a few percent of
the reading. A constant model on log OD then over-trusts the lowest standards,
while a constant model on linear OD lets the top standards dominate and loses
the low end. With the additive + proportional model the log and linear fits
give about the same usable range. Bead-assay MFI is usually well described by
the constant model on log MFI.

In **Compute fits**, *Use settings* applies this setting per antigen; choosing
a model there overrides it for that job only.

::: more
The additive term (sigma_add) is learned mainly from the lowest one or two
standards of each plate, so it is less well determined than cv. Fitting all
plates of a group together, as I-SPI does, helps. On a log response the model
is applied through the delta method (SD on the log10 scale = raw SD /
(response × ln 10)), so both engines and both response scales use the same
noise description. When some responses are ≤ 0 a small
[[glossary.response_offset|response offset]] is added first.
:::
