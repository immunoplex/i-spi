---
id: data.results.fit_diagnostics
title: Fit diagnostics
audience: user
category: procedural
see_also: [glossary.fit_diagnostics_report, glossary.convergence, data.results.eligibility_gates, data.results.run]
---
Press **Build diagnostics report** to assess the stored fits for the selected
experiment. It reads the fit, sampler, parameter, gate and LOO results, then
shows a verdict, the findings with their evidence, and suggested next steps.
See [[glossary.fit_diagnostics_report|Fit diagnostics report]] for what it checks.

**When to use it**
- Plates fit well separately but badly together.
- A Bayesian fit took very long, or its curves look wrong.
- A fit job finished with a warning notice.
- You changed settings or masked standards and want to confirm the fit is now healthy.

**How to use it**
1. Select the study and experiment, then press **Build diagnostics report**.
2. Read the verdict and the first suggested step. Findings marked *error* come first.
3. Make the change it suggests (often a setting or a standard mask), recompute,
   and build the report again to confirm.
4. To get help from a colleague or an LLM, download the Markdown (or JSON) and
   paste it in; it already includes the settings and who set them.

The report is built on demand and clears when you change experiment or reload
data, so rebuild it after each recompute.
