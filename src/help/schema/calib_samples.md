---
id: schema.calib_samples
title: "Back-calculated samples (calib_samples)"
audience: both
category: schema
schema_table: calib_samples
see_also: [glossary.se_concentration, glossary.pcov, glossary.posterior_predictive, glossary.censoring, glossary.concentration_interval, glossary.exceedance_probability, data.analysis.downstream_censoring]
---

Each test sample's concentration read off the fitted curve (predicted, and
final = x dilution) with precision. Samples outside the quantifiable range
also carry a censoring class and bounds, and every sample has a 95%
predictive interval and threshold probabilities. This replaces the old Sample
QC tab. Grain: one row per curve / method / sample identity. Part of the
**Results** group.
