---
id: qc.linearity.consensus
title: Consensus per sample
audience: user
category: procedural
see_also: [glossary.latent_consensus, qc.linearity.transform, qc.linearity.error_model, qc.linearity.delta]
---
**What it gives you.** One concentration for each sample, combined from all its
estimates (the duplicates, the dilutions, the plates, the two methods), together
with an interval. It is a better estimate than any single one because it uses all
of them and trusts the precise ones more.

**How it is made.**

1. The fitted line says how the two sets differ. Both sets are moved half way to
   a common scale (the *Harmonize* transform).
2. For each sample, its harmonized estimates are averaged, each weighted by how
   precise it is (the inverse of its variance). Precise estimates count for
   more.
3. The result is shown as a concentration with a 95% interval. A sample with
   only one estimate returns that estimate unchanged.

**Columns in the table.**

| Column | Meaning |
|---|---|
| n_points | How many distinct estimates were combined |
| consensus | The combined concentration (back on the concentration scale) |
| lo, hi | Its 95% interval |
| z, se_z | The same estimate on the fitting scale, and its standard error |
| q, q_df, q_ratio | A heterogeneity check: Q is how much the estimates disagree beyond what their pcov predicts, and q_ratio is Q per degree of freedom |

**Reading the heterogeneity check.** q_ratio near 1 means the estimates disagree
about as much as their pcov says they should. A value well above 1 means they
disagree more than pcov explains (a plate effect, a dilution effect, a bad
well); the consensus interval for that sample is then too narrow and the sample
deserves a look.

**When it is available.** It needs the *Both* error model (and a positive
slope), because it uses the harmonization. Use it for estimates that are
interchangeable readings of the same quantity. For dilution series, check the
non-linearity first (see [[qc.linearity.transform|Transform]]); a consensus
across dilutions that are not linear averages over a real difference.

**Which estimates are used.** Every valid estimate that has a pcov, regardless
of the *Regression uses* choice. Imprecise estimates are not removed; they simply
get a small weight.
