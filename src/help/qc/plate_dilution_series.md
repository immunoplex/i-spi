---
id: qc.plate_dilution_series
title: Plate dilution series view
audience: user
category: conceptual
see_also: [compute.standard_curve.model_engine]
---

This view reproduces the standard-curve-by-plate comparison from the analysis
notebooks: one facet per plate, dilution on the x-axis and response on the
y-axis, both log-scaled — a quick way to see whether the standard curve looks
consistent from plate to plate within a batch, before trusting any fitted
result from it.

Two sub-tabs cover it differently. **Analytes** fixes one standard-curve
source and facets by plate, with one trace per antigen/feature — this is
where you can click a point to stage it for masking or unmasking, the same
masking workflow the Explore-fits tab uses. **Sources** fixes one antigen
instead, facets by plate, and shows one trace per source; it also overlays
any test sample run at more than one dilution on a plate as its own dashed
trace, which is particularly useful for reading optimization plates.

::: more
Masking here still operates one curve (one plate + one antigen/feature) at a
time: the plot spans every plate so contamination is visible in context, but
a staged batch of points stays pinned to whichever curve the first staged
point belongs to. To mask the same well across several plates, stage and save
on one plate, then repeat on the next — that's expected, not a limitation to
work around.
:::
