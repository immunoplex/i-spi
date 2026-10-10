---
id: qc.bead_count.thresholds
title: Bead count thresholds
audience: user
category: conceptual
references:
  - text: "i-spi-refactor docs/help-system/assessment/01a-decision-points.md §4"
---

In a multiplex bead-based assay, each well's result for an antigen is a median
read across many individual beads. A well where too few beads were actually
read back is a less reliable median — fewer beads means more noise in that
estimate, independent of anything about the sample itself.

The **Lower Threshold** and **Upper Threshold** fields (defaults 35 and 50)
set the bead counts the plot flags against, and **Failed Well Criteria**
chooses which one governs the Low/Sufficient label a well gets. Wells below
the active threshold are marked "Low Bead Count" in the plot — a visual flag
for you to review, not an automatic exclusion. Bead count here does not
currently feed curve fitting or sample exclusion: a well flagged "Low Bead
Count" is still fit and reported like any other unless you act on it
separately.
