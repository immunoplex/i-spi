---
id: qc.linearity.kinds
title: Test samples, controls and standards
audience: user
category: procedural
see_also: [qc.linearity.sets, qc.linearity.overview]
---
**Include** chooses which wells are collected when you press *Find replicate sets*.
When more than one type is loaded, **Compare estimates of** picks one at a time:
the estimate of a control and the estimate of a patient sample are never paired
with each other.

| Type | One set is | Where the concentration comes from |
|---|---|---|
| Test samples | One patient and timeperiod on one antigen and feature | Stored by the compute job |
| Controls | One control (its name) on one antigen and feature | Computed here (below) |
| Standards | One standard level on one antigen and feature | Computed here (below) |

**Controls and standards are not stored with a concentration**, so the app works
it out from the curve that was stored for each plate, antigen and method:

1. The well's response is put on the scale the curve was fitted on.
2. The stored best model is inverted at that response to give the concentration.
   A Bayesian curve uses the posterior median of each parameter.
3. A control's concentration is multiplied by its dilution, as for samples. A
   standard's is not: its dilution is already part of its nominal concentration.
4. **pcov** is read from the curve's stored precision profile at that
   concentration. A response beyond the curve's asymptotes cannot be inverted: it
   has no concentration and pcov 150.
5. The **gate class** (below LLOQ, in range, above ULOQ) comes from the curve's
   LLOQ and ULOQ.

**How close is this to the compute job?** The app tests itself when the points
load. It re-inverts the stored curve and compares with the stored grid, and it
compares the pcov it reads with the pcov stored for test samples. The result is
shown in the report. The inverse is exact for frequentist fits. For Bayesian fits
it can differ a little, because the job takes the median of the concentration over
every posterior draw, and the app uses the median parameters.

**Standards.** A standard level is the same position in the dilution series (for
example "std 9"). Wells of one level on different plates are replicates. Masked or
excluded standards are left out. The estimate is the back-calculated concentration,
not the nominal one.

**Controls.** Masked control wells are left out. A control read through several
standard-curve sources gives one estimate per source, so *Source vs source* works.
Blanks have no concentration and are not included.
