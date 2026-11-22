---
id: glossary.kappa
title: kappa (scale of the measurement error)
audience: user
category: glossary
topic: Precision and uncertainty
see_also: [glossary.tau, glossary.identifiability, glossary.se_concentration, qc.precision_weights.additive_options]
references:
  - text: "curveRweights vignette \"Measurement error in assay data\": how good is tau? (identifiability; reading kappa)"
    url: "https://immunoplex.github.io/curveRweights/articles/measurement-error.html#identifiability"
---

**kappa** relates the calibration curve's reported precision to the real
measurement error: a sample's measurement SD is `kappa * se`. kappa = 1 means
the curve's [[glossary.se_concentration|se]] is exactly the measurement SD.

::: more
Values between about 0.5 and 2 are common: the `se` has the right shape across
the curve but not exactly the right size (plate handling and bead counts add
noise the curve fit does not see). A kappa well above 2 means the readings
scatter much more than their `se` suggests, or the `se` barely changes between
samples whose real precision differs. Reliabilities then rest heavily on kappa;
the weights-fit table flags such groups. In one real data set the Bayesian
calibration's `se` needed kappa ≈ 3 while the frequentist `se` needed 1.5 for
the same samples. When kappa cannot be estimated well, fix it at 1 (see
[[qc.precision_weights.additive_options|additive options]]).
:::
