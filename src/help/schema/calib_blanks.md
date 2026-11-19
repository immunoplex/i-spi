---
id: schema.calib_blanks
title: "Blanks (calib_blanks)"
audience: both
category: schema
schema_table: calib_blanks
see_also: [data.results.blanks, schema.calib_standards, schema.xmap_buffer]
---

The blank (buffer) wells each fit used, written by the compute job with every
fit (the last fit's snapshot, including `included` and `mask_reason` at that
time). Grain: one row per curve / `method` / `well`. Part of the **Results**
group.
