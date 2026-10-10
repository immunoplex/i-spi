---
id: schema.calib_fit
title: "Fit / model selection (calib_fit)"
audience: both
category: schema
schema_table: calib_fit
see_also: [glossary.model_selection_frequentist, glossary.model_selection_bayesian, glossary.aic, glossary.loo]
---

Every candidate model fitted for each curve, with the selection outcome. The
winning model is the `is_best` row. Grain: one row per curve / method /
`model_name`. Part of the **Results** group.
