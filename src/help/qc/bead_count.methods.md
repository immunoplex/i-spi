---
id: qc.bead_count.methods
title: Bead Count Analysis Methods
audience: user
category: procedural
see_also: [qc.bead_count.thresholds]
references:
  - text: "Matson, Zachary et al. \"shinyMBA: a novel R shiny application for quality control of the multiplex bead assay for serosurveillance studies.\" Scientific reports vol. 14,1 7442. 28 Mar. 2024."
    doi: "10.1038/s41598-024-57652-4"
---
Use the dropdown menu labeled "Plate in Sample Data" to select a plate from the
sample data in the currently selected experiment. Then, choose an antigen from
the selected plate to analyze bead counts for that antigen.

The figure below the dropdown menus displays bead counts on the y-axis for
each of the 96 wells in the multiplex bead assay on the x-axis. In the
figure:

- The red dotted horizontal line represents the lower threshold value.
- The blue dotted horizontal line represents the upper threshold value.
- The default lower and upper thresholds are 35 and 50, respectively.
- The black solid horizontal line represents the average bead count across
  all wells.
- Red-colored wells indicate a low bead count, while blue-colored wells have
  sufficient bead counts based on the failed well criterion.

The failed well criterion is either wells with bead counts below the upper
threshold or wells with bead counts below the lower threshold. To adjust the
lower and upper thresholds and modify the failed well criterion, see
[[qc.bead_count.thresholds|Bead Count Options]] on the Study Overview tab.

::: more
Below the figure, a table titled "Sample Values with Low Bead Counts" lists
samples that meet the low bead count criteria. The table includes:

- `bead_count_gc`: the gate class of the bead count, indicating whether it is
  sufficient or low.
- `is_low_bead_count`: a Boolean column where true indicates a low bead count
  and false indicates a sufficient bead count.

To download the bead count gate class for all samples in the currently
selected experiment within the selected study, click the download button
below the table.
:::
