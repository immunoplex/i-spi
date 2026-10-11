---
id: compute.standard_curve.out_of_range
title: Out-of-range samples on the standard curve
audience: user
category: conceptual
see_also: [glossary.censoring, glossary.concentration_interval, glossary.exceedance_probability, glossary.pcov, compute.standard_curve.bayes_draws, data.analysis.downstream_censoring]
---
Test samples on the Curve and Precision tabs are coloured by their
[[glossary.censoring|censoring class]]: green inside the quantifiable range
(LLOQ–ULOQ), blue below the LLOQ (dark blue below the LOD), orange above the
ULOQ (dark orange when saturated). Samples from fits computed before this
feature existed stay grey until the curve is recomputed.

Hover a sample to see its concentration, its
[[glossary.concentration_interval|95% interval]] and its
[[glossary.exceedance_probability|threshold probabilities]]. Samples whose
response lies beyond the curve's asymptotes have no finite estimate; they are
placed at the left or right edge of the curve plot, on the side their
censoring class says they belong.

::: more
**Why a below-range sample can be more useful than its %CV suggests.** pcov is
one symmetric number on the log scale, where both ends of the curve are
open-ended. On the natural scale they are not: a below-LLOQ sample is bounded
between 0 and about the LLOQ, while an above-ULOQ sample has no upper bound.
The interval and probabilities keep that difference — a low sample often
answers "is it below the cutoff?" with near certainty. High samples are better
re-assayed at a higher dilution than modelled.
:::
