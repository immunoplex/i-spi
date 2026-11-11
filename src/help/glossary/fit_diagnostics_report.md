---
id: glossary.fit_diagnostics_report
title: Fit diagnostics report
audience: both
category: glossary
topic: Curve models and fitting
see_also: [data.results.fit_diagnostics, glossary.convergence, glossary.hierarchical_priors, glossary.bayesian_approach, glossary.frequentist_approach, data.results.eligibility_gates, glossary.loo]
references:
  - text: "I-SPI design note: docs/design/fit_diagnostics.md — how the report is built, every finding and threshold, and how to extend it."
---
The **fit diagnostics report** is a single document, built on demand for one
experiment, that gathers every number I-SPI stored about how its curves were
fitted and says what they imply. Use it when a set of plates does not fit well,
a fit looks "odd", or the same plates behave differently when analysed together
and apart.

It answers three questions in order:

1. **What was actually fitted?** The response scale (raw, log₁₀ or ln), the
   settings in force and who set them, how many plates were pooled, and how
   long the fit took.
2. **How healthy is the fit?** Sampler statistics for Bayesian fits (R-hat,
   effective sample size, divergences, E-BFMI, the Student-t degrees of freedom
   `nu`), optimizer and parameter-uncertainty checks for frequentist fits, the
   quantification gates, and LOO reliability.
3. **What should I do?** Findings are ranked error, warning or note, each with
   the evidence behind it, followed by a short ordered list of next steps.

The Markdown and JSON downloads are written so they can be pasted straight into
an LLM or sent to a colleague: they carry the settings and their provenance, so
a reader does not have to ask "which scale was this?".

::: more
**Typical causes it separates.** A pooled Bayesian fit can look odd for quite
different reasons: the response was fitted on the wrong scale for the assay
(for example raw optical density instead of log₁₀, which shows up as `nu`
pinned at its floor of 2 and a low E-BFMI); one plate fails the quantification
gates, and since a pooled model is only eligible when *every* plate passes, the
whole group is lost; or the plates are not really exchangeable because they
differ in signal level or day. The numbers that tell these apart are all stored
but live in separate result tables; the report reads them side by side.

**Fit unit.** The report assesses each *(multiplate group, job)* on its own.
Submitting plates 1–5, then 6–10, then 1–10 leaves one group whose curves come
from different jobs, and only the latest job's fit of each curve survives, so
mixing them would mislead.

**Also shown after every compute.** When a fit job finishes, a notice lists
any warnings or errors in that job and points to the report.

**Limits.** The report reads stored results; it cannot see a setting that was
changed after the fit unless it was also recorded (it flags such fits as
*stale*). It diagnoses and recommends; it never changes settings or refits.
:::
