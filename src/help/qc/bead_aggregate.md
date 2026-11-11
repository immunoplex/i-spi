---
id: qc.bead_aggregate
title: Aggregate Beads
audience: user
category: procedural
see_also: [qc.bead_count.methods, qc.bead_count.thresholds]
---
Beads can stick together (aggregate) in a well, and aggregated beads read
unreliably. The instrument reports the **percent of beads that aggregated** for
every well. I-SPI stores it with the raw data (the `% Agg Beads` column of the
instrument file, kept as `pctaggbeads`) for samples, standards, controls and
blanks.

Choose **Aggregate Beads** under the Beads tab to see it. This view is available
only for bead-array experiments.

**Reading the plot**
- The y axis is the percent aggregate beads; the x axis is the well position,
  in column order (A1, B1 … H1, then A2 …), so neighbouring wells of a column
  sit side by side.
- Colour shows the type of well: samples, standards, controls or blanks.
- A dashed red line is the study's *% aggregate beads* threshold (set under Study
  Overview > Bead Count Options). Wells above it have a red ring and are listed in
  the table below the plot.
- Hover a point for the well, the value and, for samples, the sample, patient,
  timeperiod and dilution.
- Choose the plate (with its sample dilution) at the top. The value belongs to the
  well, so it is the same for every antigen and each well is shown once.

**What to look for.** A whole row or column elevated suggests a pipetting or
washing problem. Isolated high wells are candidates for review before their
results are trusted. Standards with high aggregation can distort the fitted
curve.

**No data?** If the view says no % aggregate beads were found, the value was not
in the file when the plate was imported. Re-import the plate with the
`% Agg Beads` column present.

**Download.** *Download % Aggregate Beads Data* saves every well of the
experiment (all plates and well types) as a CSV.
