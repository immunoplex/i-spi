---
id: schema.control_reference
title: "Control reference values (control_reference)"
audience: both
category: schema
schema_table: control_reference
see_also: [data.results.control_reference, schema.calib_controls]
---

Expected values for controls, owned by I-SPI (curveRcore 0.7.0 contract,
i-spi-compute migration 011). Grain: one row per project / study / control
source / lot / antigen / feature / `valid_from`. `study_accession = '__none__'`
is a project-wide value; a study-specific row wins over it. The view
`calib_controls_qc` joins these to `calib_controls` by run date and adds recovery,
z-score and whether the estimate is within the limits.
