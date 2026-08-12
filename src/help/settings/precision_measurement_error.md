---
id: precision_measurement_error
title: Precision profile & measurement error
audience: both
params: [include_measurement_error, pcov_threshold]
references:
  - text: "O'Connell MA, Belanger BA, Haaland PD (1993). Calibration and assay development using the four-parameter logistic model. Chemometrics and Intelligent Laboratory Systems 20(2):97-114."
    doi: "10.1016/0169-7439(93)80008-6"
  - text: "Belanger BA, Davidian M, Giltinan DM (1996). The effect of variance function estimation on nonlinear calibration inference in immunoassay data. Biometrics 52(1):158-175."
    url: "https://pubmed.ncbi.nlm.nih.gov/8934590/"
  - text: "Azadeh M, et al. (2018). Calibration Curves in Quantitative Ligand Binding Assays: Recommendations and Best Practices. The AAPS Journal 20:22."
    doi: "10.1208/s12248-017-0159-4"
---

The **precision profile** is the coefficient of variation (%CV) of a
back-calculated concentration, plotted against concentration. It defines the
usable range of a curve: the LLOQ and ULOQ are the concentrations where the
profile crosses your precision budget (the **%CV gate**, default 20%). Two
sources of error combine to shape it — uncertainty in the fitted curve itself,
and the **measurement (assay) noise** of a single reading. Because assay noise
usually grows with signal, the profile is U-shaped rather than flat.

**Including measurement error** (the default) gives the classical immunoassay
precision profile: how precisely a real, noisy reading pins down a concentration.
Turning it *off* reports only the curve's own uncertainty — an honest lower bound
that is appropriate when a plate has too few standards to estimate the noise
model well, since a poorly estimated variance function can distort the profile
and the limits derived from it. The grid and the per-sample precision always use
the **same** definition, so the sample cloud lies on the profile either way. The
switch chooses an honest presentation for the data you have; it does not
manufacture precision — more standards and replicated controls do.
