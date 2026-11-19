---
id: data.results.standards
title: Back-calculated standards
audience: user
category: conceptual
see_also: [data.results.samples, data.results.controls, data.results.blanks, schema.calib_standards, glossary.standard_wells, glossary.censoring]
references:
  - text: "data_dictionary.R table_doc('calib_standards')"
---
The standard points each fit used, one row per curve / method / standard well / dilution, with the nominal `concentration`, the response on the fitted scale (`response_model`) and as measured (`assay_response_raw`). Masked points are listed with `included = false`. The table shows what the last fit used; a mask changed since then shows here only after the next fit.

From i-spi-compute 1.1 on, each point also carries its concentration read back off the fitted curve, exactly as test samples are: `predicted_concentration` at the point's own dilution, `final_concentration` (times the [[glossary.dilution_factor|dilution factor]], so the point's estimate of the stock concentration), `pcov`, the gate class, the [[glossary.censoring|censoring class]] and the 95% interval, as in [[data.results.samples|Back-calculated samples]]. `recovery_pct` is 100 × estimated / nominal concentration. Recovery of included points is optimistic, since the curve was fitted to them.

A point beyond the curve's asymptotes has no estimate and no gate class. Curves fitted before i-spi-compute 1.1 were filled once by a backfill that uses the same calculation.
