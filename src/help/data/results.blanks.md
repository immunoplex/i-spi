---
id: data.results.blanks
title: Blanks
audience: user
category: conceptual
see_also: [data.results.standards, schema.calib_blanks, glossary.blank_wells]
references:
  - text: "data_dictionary.R table_doc('calib_blanks')"
---
The [[glossary.blank_wells|blank (buffer) wells]] each fit used, one row per curve / method / well, with the response on the fitted scale (`response_model`) and as measured (`assay_response_raw`). Masked wells are listed with `included = false` and their `mask_reason`. The table shows what the last fit used; a mask changed since then shows here only after the next fit.
