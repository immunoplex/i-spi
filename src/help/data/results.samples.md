---
id: data.results.samples
title: Back-calculated samples
audience: user
category: conceptual
see_also: [data.results.diagnostics_loq, data.results.precision_weights, schema.calib_samples, glossary.censoring, glossary.concentration_interval, glossary.exceedance_probability]
references:
  - text: "data_dictionary.R table_doc('calib_samples')"
---
Each test (patient) sample's concentration, read off the fitted curve — one row per curve / method / sample identity. `predicted_concentration` is read straight off the curve; `final_concentration` multiplies that by the sample's [[glossary.dilution_factor|dilution factor]] to give the concentration in the original, undiluted specimen. `pcov_pass` flags whether that sample's precision cleared the [[data.results.diagnostics_loq|quantification gate]] at the point it was read.

Samples outside the quantifiable range also carry a [[glossary.censoring|censoring class]] and bounds (`censor_class`, `censor_type`, `censor_lower`, `censor_upper`) — use those, not the point estimate, for such samples. Every sample also has a [[glossary.concentration_interval|95% predictive interval]] (`conc_q_lo` / `conc_q_med` / `conc_q_hi`) and [[glossary.exceedance_probability|threshold probabilities]] (`p_below_lloq`, `p_above_uloq`, `p_below_cutoff`). These columns are empty for curves computed before they existed; recompute to fill them.

This table replaces an older, separate Sample QC tab — the same concentrations are produced here, alongside the curve and diagnostics they came from, rather than in a standalone view.
