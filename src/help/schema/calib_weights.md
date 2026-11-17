---
id: schema.calib_weights
title: "Precision weights (calib_weights)"
audience: both
category: schema
schema_table: calib_weights
see_also: [glossary.precision_weight, glossary.reliability, glossary.se_concentration, data.results.precision_weights]
---

curveRweights per-sample precision weights (sigma/w/w_norm), from the
saturated location-scale model sigma_i = phi &times; se_i^beta1
(multiplicative model), or from the additive measurement-error model, which
also stores `v_meas`, `tau2`, a per-sample `reliability` and the censoring
flags. The `model` column says which produced a row; the most recent run for a
sample and method is the one kept. Grain: one row per curve / method / sample
identity. Part of the **Results** group.
