---
id: schema.calib_standards
title: "Back-calculated standards (calib_standards)"
audience: both
category: schema
schema_table: calib_standards
see_also: [data.results.standards, schema.calib_samples, schema.calib_controls]
---

The standard points each fit used, written by the compute job with every fit
(the last fit's snapshot, including `included` and `mask_reason` at that time).
Grain: one row per curve / `method` / `well` / `dilution`. From calib schema
0.7.0 (i-spi-compute migration 011, curveRcore 0.7.0 contract) each point also
carries its back-calculated estimate, with the same columns and calculation as
`calib_samples`, plus `recovery_pct`. Part of the **Results** group.
