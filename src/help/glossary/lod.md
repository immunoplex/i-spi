---
id: glossary.lod
title: LOD (limit of detection)
audience: both
category: glossary
see_also: [glossary.rdl, data.results.diagnostics_loq, schema.calib_diagnostics]
references:
  - text: "curveRcore::compute_detection_limits() — LODs derived from the confidence interval on the curve's lower/upper asymptotes."
---
The **LOD (limit of detection)** is the response level a reading has to
clear before it's distinguishable from background — derived from the
confidence interval on the fitted curve's lower (and, where relevant,
upper) asymptote. A response within that CI of the asymptote is consistent
with "no real signal," even if its point estimate looks nonzero.

LOD answers "how far from background is far enough," while
[[glossary.rdl|RDL]] asks the stricter question of how low a *concentration*
reading can go before the curve's own uncertainty undermines trusting it —
the two represent different, complementary conservatism choices, and
I-SPI reports both rather than collapsing them into one number.
