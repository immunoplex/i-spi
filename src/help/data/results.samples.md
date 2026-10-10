---
id: data.results.samples
title: Back-calculated samples
audience: user
category: conceptual
see_also: [data.results.diagnostics_loq, data.results.precision_weights, schema.calib_samples]
references:
  - text: "data_dictionary.R table_doc('calib_samples')"
---
Each test (patient) sample's concentration, read off the fitted curve — one row per curve / method / sample identity. `predicted_concentration` is read straight off the curve; `final_concentration` multiplies that by the sample's [[glossary.dilution_factor|dilution factor]] to give the concentration in the original, undiluted specimen. `pcov_pass` flags whether that sample's precision cleared the [[data.results.diagnostics_loq|quantification gate]] at the point it was read.

This table replaces an older, separate Sample QC tab — the same concentrations are produced here, alongside the curve and diagnostics they came from, rather than in a standalone view.
