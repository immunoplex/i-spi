---
id: data.results.eligibility_gates
title: Eligibility gates
audience: user
category: conceptual
see_also: [data.results.fit_model_selection, schema.calib_gate]
references:
  - text: "curveRcore getting-started vignette, \"Eligibility gating\" — assess_model_eligibility()"
    url: "https://immunoplex.github.io/curveRcore/articles/getting-started.html"
---
One row per curve / method / model name / gate — the individual pass/fail checks a fitted candidate has to clear before it's even allowed to compete for [[data.results.fit_model_selection|selection]]. `passed` is the check's outcome; `detail` explains why it failed, when it did.

A model can converge numerically and still fail a gate — common reasons are a parameter estimate sitting right at the edge of its allowed search range, a poorly conditioned uncertainty estimate, or predictions that don't cover a sensible dynamic range. The gates differ slightly between the two fit engines (a couple apply only to frequentist fits), but the purpose is the same either way: keep a technically-fitted but untrustworthy candidate from winning on score alone.

::: more
Gates run before AIC (frequentist) or LOO (Bayesian) ever rank anything — an ineligible candidate is excluded from ranking entirely, not just penalized. If every candidate for a curve fails, [[data.results.fit_model_selection|selection]] falls back to the widest-dynamic-range candidate and flags it, rather than returning nothing.
:::
