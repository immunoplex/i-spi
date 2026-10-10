---
id: import.plate_grid.confirm
title: Confirming the plate layout
audience: user
category: procedural
see_also: [compute.import.description_shape_binding, compute.import.dilution_source_precedence]
---
Each well is typed (Standard, Control, Blank, or Sample) and described
automatically from the uploaded file. This step is where you check that work:
every plate needs at least one Sample, one Standard, and (usually) one Blank
before you can continue — a plate missing any of these is flagged as an error
here, not later. Click a well, row, or column heading to select it, then set its
type and/or description directly; "apply to all plates" and "apply the same fix
everywhere this description appears" save you from repeating an edit per plate.

::: more
Some wells can only be typed with a *suggested* match (e.g. a low-response well
proposed as a Blank from its signal alone, not from its label). A suggestion is
shown with its confidence and reason and must be explicitly reviewed — "I have
checked these — confirm them" — before the layout counts as resolved, even if you
don't change anything. This is a deliberate checkpoint: an automatic guess should
never silently become the record without a human looking at it.
:::
