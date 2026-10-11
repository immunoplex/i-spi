---
id: study_overview.sample_estimate_quality
title: Sample estimate quality (by fit method)
audience: user
category: compute-decision
see_also: [data.results.diagnostics_loq, glossary.censoring]
---
This view takes the same LOD/LOQ gating described in [[data.results.diagnostics_loq|Diagnostics / LOQ]] and rolls it up to the sample level: for every plate and antigen, what proportion of samples actually fell inside the quantifiable range versus below detection, above it, or without a usable fit at all. Filter by Source and Analyte, and switch the Method toggle to compare the frequentist and Bayesian fits' gating side by side.

Use it as a plate/antigen-level QC check — a plate with an unusually high proportion of below-LOD or no-fit samples is worth investigating before trusting its results, independent of any single curve's own diagnostics.

Classes come from each sample's [[glossary.censoring|censoring class]], computed when the curve was fitted — which also places samples whose response was beyond the curve's asymptotes. For curves fitted before that existed, the sample's concentration is compared with the curve's limits on the same (undiluted) scale; such a sample with no usable concentration is not counted as in range.
