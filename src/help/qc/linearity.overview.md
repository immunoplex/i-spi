---
id: qc.linearity.overview
title: Replicate Linearity — what this tab is for
audience: user
category: procedural
see_also: [qc.linearity.sets, qc.linearity.error_model, qc.linearity.weighting, qc.linearity.transform, qc.linearity.agreement, qc.linearity.consensus]
---
Some samples are measured more than once: run in duplicate, at two dilutions, on
two plates, or analysed with two fitting methods (frequentist and Bayesian). If
the assay and the curve fits are behaving, all of those estimates should tell
the same story. This tab lines them up, plots one against the other, and tells
you how closely they agree and whether one is systematically higher or lower.

**Use it after concentrations are computed**, because it works on the
back-calculated concentrations, their precision (pcov) and their gate class.

**The four steps, top to bottom**

1. **Replicate sets.** Press *Find replicate sets*. The cards count how many
   samples were measured more than once, and in what way. See
   [[qc.linearity.sets|Replicate sets and pairs]].
2. **Choose what to compare.** Pick a comparison (for example *Dilution vs
   dilution*), then adjust what goes on the x axis and the y axis.
3. **Plot and regression.** Choose how the line is fitted. The options are
   explained one by one, each with an information icon beside it:
   [[qc.linearity.error_model|which estimates carry error]],
   [[qc.linearity.delta|the error-variance ratio]],
   [[qc.linearity.weighting|what the regression uses]],
   [[qc.linearity.scale|log or natural scale]], and
   [[qc.linearity.transform|the transform]].
4. **Report, consensus and pairs.** Download the report, or the single best
   concentration for each sample
   ([[qc.linearity.consensus|consensus]]).

**Reading the plot.** The dashed line is where two identical estimates would
fall (1:1). The solid line and its shaded band are the fitted relationship. If
the solid line lies on the dashed one, the two estimates agree. Fill colour is
the gate class of the y estimate and the outline is the gate class of the x
estimate, so a point with a green fill and an orange outline has a y in range and
an x above the upper limit of quantification. Bigger points are more precise.
Faded points were not used for the line. Hovering shows where each estimate came
from (plate, well, patient, timeperiod, specimen, dilution, wavelength, method,
source).

**First time?** Leave the options at their defaults, choose *Duplicates* or
*Frequentist vs Bayesian*, and read the
[[qc.linearity.agreement|agreement numbers]]. Change one option at a time after
that.

For the ideas behind the statistics, see the Glossary entries
[[glossary.errors_in_both_axes|Regression when both estimates are noisy]],
[[glossary.agreement_ccc_ba|Agreement: concordance and Bland–Altman]] and
[[glossary.latent_consensus|Latent concentration and consensus]].
