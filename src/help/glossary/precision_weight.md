---
id: glossary.precision_weight
title: Precision weight
audience: user
category: glossary
see_also: [qc.precision_weights.method, data.results.precision_weights, data.results.precision_weights_fit, glossary.se_concentration, schema.calib_weights, schema.calib_weights_fit]
references:
  - text: "curveRweights precision-weighting.Rmd §\"The problem: unequal precision across the calibration curve\""
---

A **precision weight** is a continuous, per-sample weight (`w`, or its
normalized form `w_norm`) reflecting how reliably a sample's
back-calculated concentration was measured — a reading taken near the
flat ends of the standard curve gets a lower weight than one taken on its
steep, precise midrange. It replaces a binary in-range/out-of-range
quantification gate with smooth down-weighting, so downstream analyses
can use every observation while still accounting for how trustworthy each
one is.
