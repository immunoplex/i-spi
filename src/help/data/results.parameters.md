---
id: data.results.parameters
title: Fitted parameter estimates
audience: user
category: conceptual
see_also: [compute.standard_curve.model_engine, schema.calib_param]
---
The fitted coefficients of the winning model for each curve — one row per model
term (e.g. the four or five parameters of a [[compute.standard_curve.model_engine|
logistic or Gompertz]] curve), each with its estimate and uncertainty (standard
error and a credible/confidence interval). These are the actual numbers that define
the fitted curve shape, not just whether a fit succeeded.
