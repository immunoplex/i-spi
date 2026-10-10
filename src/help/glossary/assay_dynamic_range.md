---
id: glossary.assay_dynamic_range
title: Assay dynamic range
audience: both
category: glossary
see_also: [glossary.loq_gating, glossary.precision_profile, data.results.diagnostics_loq, schema.calib_diagnostics, glossary.assay_sensitivity, schema.calib_gate]
references:
  - text: "min_dynamic_range_log10 argument, curveRfreq::fit_calibration_freq() / curveRbayes::fit_calibration_bayes() / curveRcore::assess_model_eligibility() — an eligibility gate, default 0.5 log10 decades (~3-fold)."
---
A curve's **dynamic range** is the span, in log10 concentration decades,
between its [[glossary.loq_gating|lower and upper quantification limits]] —
how wide a range of concentrations it can actually distinguish, not just
the range of standards that happened to be run. I-SPI displays it both as
decades and as an equivalent fold-change (e.g. "1.2 decades (16-fold)").

Dynamic range isn't only a reporting number: a minimum dynamic range
(default 0.5 log10 decades, about 3-fold) is one of the eligibility gates a
candidate model has to clear before it can be selected at all — a curve
that technically converges but spans too narrow a range to be useful is
excluded from [[glossary.model_selection_frequentist|selection]] on that
basis, under both engines.
