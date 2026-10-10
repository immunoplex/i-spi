---
id: glossary.rdl
title: RDL (reliable detection limit)
audience: both
category: glossary
see_also: [glossary.lod, data.results.diagnostics_loq, schema.calib_diagnostics]
references:
  - text: "curveRcore::compute_detection_limits() — computes LODs, MDC, and RDL for the best eligible model."
---
The **RDL (reliable detection limit)** is the lowest concentration at which
a result can be distinguished from background **after accounting for the
fitted curve's own uncertainty**, not just its nominal shape — it's derived
by inverting a confidence-interval-adjusted version of the fitted curve,
rather than reading the raw curve itself. This makes it a more conservative
(and generally higher) threshold than [[glossary.lod|LOD]], since it asks
"how low can a result go before the *uncertainty* in the curve itself makes
detection unreliable," not just "how far above background is the curve's
best estimate."

::: more
RDL and MDC (minimum detectable concentration) are computed the same way —
inverting a CI-adjusted curve — and are closely related; see
[[data.results.diagnostics_loq|the Diagnostics / LOQ table]] for how both
sit alongside LOD and the LLOQ/ULOQ pair in I-SPI's reported limits.
:::
