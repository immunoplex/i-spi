---
id: data.results.fitted_grid
title: Fitted grid
audience: user
category: conceptual
see_also: [settings.precision_measurement_error, schema.calib_grid]
references:
  - text: "curveRcore getting-started vignette, \"$grid\" — generate_prediction_grid(), predict_grid_response(), pcov_from_se()/se_from_pcov()"
    url: "https://immunoplex.github.io/curveRcore/articles/getting-started.html"
---
A dense, evenly spaced curve — about 200 points per curve/method — built for plotting and for reading precision off the fit directly, rather than only at the handful of concentrations that were actually measured. Each row carries the predicted response at that point, a confidence band around it, the inverse-predicted concentration, and the [[settings.precision_measurement_error|pcov]] QC series used to judge how precise a reading at that point would be.

This is the same curve and the same precision definition the standard-curve plot draws from — nothing here is a separate approximation of what you see on screen.
