---
id: data.results.control_reference
title: Control reference values
audience: user
category: conceptual
see_also: [data.results.controls, schema.control_reference]
references:
  - text: "data_dictionary.R table_doc('control_reference')"
---
The value each control is expected to read: an expected concentration, optionally an SD and lower/upper limits, the units, and how it was set (`basis`: assigned by a supplier, or established from this lab's runs). Each value applies from `valid_from` until `valid_to` (open-ended when empty), so a new control lot or a re-assignment gets a new row instead of overwriting the old one.

This tab shows the rows for the selected study and the project-wide rows (`study_accession` = `__none__`); a study-specific value takes precedence. Compare them with [[data.results.controls|Back-calculated controls]]: the database view `calib_controls_qc` does that matching by run date and adds recovery, z-score and whether each control is within its limits.

I-SPI 1.1.0 shows and exports these values; editing them in the app comes in a later release.
