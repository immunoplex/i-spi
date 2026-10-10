---
id: import.flow.upload
title: Importing post-gating flow cytometry data
audience: user
category: procedural
see_also: [import.plate_grid.confirm, compute.import.description_shape_binding]
---
Select one or more post-gating FlowJo export files. Unlike Bio-Plex and ELISA,
flow files don't report their own plate size — set **Wells per plate** yourself.
Enter the **Feature** (the measurement, e.g. `MFI`), and leave **Also create a
combined experiment** checked if you want one additional experiment that merges
every analyte under a single antigen, alongside the per-antigen experiments.
Flow data goes through the same layout-confirmation and description-binding steps
as every other format — see [[import.plate_grid.confirm|confirming the plate
layout]] and [[compute.import.description_shape_binding|configuring the
description field]].
