---
id: data.results.controls
title: Back-calculated controls
audience: user
category: conceptual
see_also: [data.results.samples, data.results.control_reference, schema.calib_controls, glossary.control_wells, glossary.censoring]
references:
  - text: "data_dictionary.R table_doc('calib_controls')"
---
Each control well's concentration, read off the fitted curve exactly as test samples are: one row per curve / method / control well. `final_concentration` is `predicted_concentration` times the control's [[glossary.dilution_factor|dilution factor]]; `pcov`, the gate class, the [[glossary.censoring|censoring class]], the 95% interval and the threshold probabilities mean what they mean in [[data.results.samples|Back-calculated samples]].

A control well read on a plate with several standard sources has one row per source. Masked control wells are estimated too, with `included = false`, so you can see what a masked control would have read. `source` holds the control's name (for example `QC1 (Low)`).

The compute job stores these from i-spi-compute 1.1 on. Curves fitted earlier were filled once by a backfill that uses the same calculation; a curve with no rows here has not been fitted or backfilled since.
