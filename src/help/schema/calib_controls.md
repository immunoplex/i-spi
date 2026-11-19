---
id: schema.calib_controls
title: "Back-calculated controls (calib_controls)"
audience: both
category: schema
schema_table: calib_controls
see_also: [data.results.controls, schema.calib_samples, schema.control_reference]
---

Each control well's estimate on each fitted curve, with the same columns and
calculation as `calib_samples` (curveRcore 0.7.0 contract, i-spi-compute
migration 011). Grain: one row per curve / `method` / `well`; a control well
appears once per standard source of its plate. Masked wells are stored too, with
`included = false`. `source` is the control's name. Part of the **Results** group.
