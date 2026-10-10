---
id: study_overview.high_aggregate_low_bead
title: High-aggregate and low-bead-count overview
audience: user
category: conceptual
see_also: [qc.bead_count.thresholds]
---
For a chosen analyte and specimen type, this shows which plates have wells
failing either of two bead-assay read-quality checks: **low bead count** (too
few beads read in that well — see [[qc.bead_count.thresholds|the bead count
threshold]]) and **high aggregates** (too many beads clumped together during the
read, which the instrument can't resolve as individual beads). The two are
different problems — one is "not enough signal," the other is "beads stuck
together distorting the count" — shown side by side so you can tell which is
actually driving a plate's quality issue.

No plot appears when nothing fails the thresholds for the selected
analyte/specimen type combination — that's the expected, healthy result, not a
sign the view is broken.

::: more
For Standard wells specifically, a **Standard Curve Source** selector also
appears, since standards can come from more than one source within a study and
the two sources may read differently.
:::
