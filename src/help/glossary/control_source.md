---
id: glossary.control_source
title: Control source and curve source
audience: user
category: glossary
topic: Plate layout and wells
see_also: [glossary.control_wells, import.plate_grid.confirm, qc.linearity.sets, qc.plate_dilution_series]
---

Two different things are called "source":

* The **curve source** is the source of the *standard* — the reference material the
  standard curve is built from (for example "Inhouse Ref"). It is part of what
  identifies a curve, so a plate with two standards has two curves.
* The **control source** is the Source of a *control* well — where that batch of
  the control came from, or how it was run (for example `jackson_high` or
  `southern_low`). It is set when the plate is loaded, typed in or taken from the
  plate names in *Confirm Plate Layout*.

The same control (say QC1 Orange) from two control sources is one control measured
two ways: comparing them is what **Control source vs control source** in Replicate
Linearity does. Every other comparison there keeps the control source the same, so
one supplier's QC is never counted as a plate or dilution replicate of another's.

::: more
Plates loaded before sample identifiers came from the description stored the
control's *name* in its Source ("QC1 (Low)"). For those plates the name is shown
and there is no control source, so control-source comparisons find nothing — which
is correct: the batches were not recorded.
:::
