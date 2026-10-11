---
id: data.analysis.downstream_censoring
title: Analysing exported concentrations downstream
audience: both
category: procedural
see_also: [glossary.censoring, glossary.concentration_interval, glossary.exceedance_probability, glossary.precision_weight, qc.precision_weights.method, data.actions.excel_bundle, schema.calib_samples, compute.standard_curve.out_of_range]
references:
  - text: "curveR vignette \"Analysing I-SPI exports: censoring, predictive intervals and precision\" (worked code for every method below)"
    url: "https://immunoplex.github.io/curveR/articles/analysing-ispi-exports.html"
  - text: "Helsel DR (2012). Statistics for Censored Environmental Data Using Minitab and R, 2nd ed. Wiley."
---
Analyse concentrations on the **log₁₀ scale**, and treat samples outside the
quantifiable range as [[glossary.censoring|censored]] using the `censor_type`,
`censor_lower` and `censor_upper` columns in every export:

- **Censored (Tobit) regression** — the recommended default for group means and effects, e.g. `survival::survreg(Surv(lo, hi, type = "interval2") ~ ...)` with patient-clustered standard errors.
- **Bayesian censored likelihood** — the same model with a patient random effect, e.g. `brms` with `cens()`; best for longitudinal designs.
- **Survival-style nonparametric methods** (Kaplan–Meier on flipped data, `NADA2`) — for medians, percentiles and group comparisons without a normality assumption.
- **[[glossary.exceedance_probability|Threshold probabilities]]** — for "is it below the protective level?" questions, using `p_below_cutoff` directly.

Avoid substituting LLOQ/2 or dropping out-of-range samples as a primary
analysis; both bias the result.

::: more
**About precision weights.** In an immunoassay the measurement error depends
on the true concentration (the U-shaped precision profile), so weighting each
sample by its own precision pulls group means toward the best-measured middle
of the curve — for out-of-range samples strongly, and mildly even inside the
range. A simulation in the curveR vignette shows this bias persists even when
the SEs are known exactly. For group means and effects, prefer the unweighted
censored models above; use [[glossary.precision_weight|precision weights]]
where the aim is down-weighting genuinely noisy measurements, and report a
weighted-versus-unweighted sensitivity analysis. A censoring-aware option for
precision weights (weights for in-range samples, censoring indicators for
out-of-range ones) is planned.
:::
