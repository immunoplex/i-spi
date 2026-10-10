---
id: study_overview.sample_estimate_quality
title: Sample estimate quality (by fit method)
audience: user
category: compute-decision
see_also: [data.results.diagnostics_loq]
---
This view takes the same LOD/LOQ gating described in [[data.results.diagnostics_loq|Diagnostics / LOQ]] and rolls it up to the sample level: for every plate and antigen, what proportion of samples actually fell inside the quantifiable range versus below detection, above it, or without a usable fit at all. Filter by Source and Analyte, and switch the Method toggle to compare the frequentist and Bayesian fits' gating side by side.

Use it as a plate/antigen-level QC check — a plate with an unusually high proportion of below-LOD or no-fit samples is worth investigating before trusting its results, independent of any single curve's own diagnostics.
