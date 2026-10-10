---
id: glossary.loq_gating
title: Gating by LOQ
audience: both
category: glossary
see_also: [glossary.pcov, glossary.loq_shape_vs_pcov, glossary.assay_dynamic_range, schema.calib_grid, glossary.precision_profile]
references:
  - text: "curveRcore::classify_pcov_gate() — classifies each grid point / sample as belowLLOQ, inDynamicRange, or aboveULOQ."
---
**LOQ gating** is the pass/fail classification applied to each point on a
curve (and to each sample read off it): whether its
[[glossary.pcov|pcov]] clears the precision threshold and falls inside the
quantifiable range. Each point is classified as `belowLLOQ`,
`inDynamicRange`, or `aboveULOQ`; `pcov_pass` is the simple pass/fail (does
pcov clear the threshold at all), while `pcov_gate_class` additionally says
*which side* of the dynamic range a failing point is on.

::: more
One edge case worth knowing: a point can fail (`pcov_pass = FALSE`) while
its side of the range is ambiguous, in which case `pcov_gate_class` is
reported as missing (`NA`) rather than guessed — `pcov_pass` itself is
never `NA`, only the finer-grained side classification can be.
[[glossary.loq_shape_vs_pcov|Shape-LOQ vs. pcov-LOQ]] is a related but
different comparison — the *two definitions* of where quantification
breaks down, rather than this gate's pass/fail mechanics on one of them.
:::
