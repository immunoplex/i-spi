---
id: schema.calib_weights_fit
title: "Precision weights fit (calib_weights_fit)"
audience: both
category: schema
schema_table: calib_weights_fit
see_also: [glossary.precision_weight, glossary.reliability, data.results.precision_weights_fit]
---

One curveRweights fit per multiplate group x method: phi/beta1 scale-model
estimates (multiplicative model) or tau/kappa/reliability (additive model),
diagnostics, and interpretation. The `model` column says which; `fit_options`
records the options of an additive fit. Grain: one row
per `multiplate_group_id` / method. Part of the **Results** group.
