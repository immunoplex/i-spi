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
| Controls | One control (its name) on one antigen and feature | Stored by the compute job |
| Standards | One standard level on one antigen and feature | Stored by the compute job |

**All three are estimated the same way, by the compute job, when the curve is
fitted.** The app only reads what was stored. For each curve and method the job
stores, per control well and per standard point, the concentration, its pcov, the
gate class (below LLOQ, in range, above ULOQ) and the censoring class, exactly as
for test samples.

- A control's concentration is multiplied by its dilution, as for samples. A
  standard's is not: its dilution is already part of its nominal concentration.
- A response beyond the curve's asymptotes has no concentration.
- Curves fitted before i-spi-compute 1.1 got these values from a one-off backfill
  that uses the same calculation. If a curve has none yet, the message after
  *Find replicate sets* says how many curves are missing; a refit adds them.

**Standards.** A standard level is the same position in the dilution series (for
example "std 9"). Wells of one level on different plates are replicates. Masked or
excluded standards are left out. The estimate is the back-calculated concentration,
not the nominal one.

**Controls.** Masked control wells are left out. A control read through several
standard-curve sources gives one estimate per source, so *Source vs source* works.
Blanks have no concentration and are not included.
