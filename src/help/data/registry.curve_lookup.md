---
id: data.registry.curve_lookup
title: The curve registry and the 10-column natural key
audience: user
category: conceptual
see_also: [schema.curve_lookup]
references:
  - text: "calib_data_access.R — CALIB_NK_COLS, the curve_lookup_nk unique index"
---
Every calibration curve I-SPI fits is identified by a stable `curve_id`, registered
once in this table and reused every time that same curve is re-fit. What makes a
curve "the same curve" is its **natural key** — ten columns that together uniquely
identify it: project, study, and experiment; plate ID and plate number; the nominal
sample dilution; source; wavelength; antigen; and feature.

::: more
Re-submitting a fit for the same natural key resolves back to the existing
`curve_id` rather than creating a duplicate — every result in the Results group
(fits, parameters, grids, samples) joins back to `curve_lookup` through this one
stable id, so re-running a curve updates its history in place instead of forking it.
:::
