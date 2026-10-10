---
id: data.raw.plates
title: The Plates table and plate-level actions
audience: user
category: procedural
see_also: [schema.xmap_header]
---
One row per plate read: the assay run metadata for each plate (plate ID, nominal
sample dilution, wavelengths read, and whether the plate is masked out of
downstream fitting, with its reason).

Two plate-level actions are available from here when applicable: **Subtract
Wavelengths** (ELISA plates read at two wavelengths) creates a new derived
experiment holding the subtracted result, and **Split Optimization Plates**
splits a plate run across multiple nominal sample dilutions into separate,
single-dilution curve sets. Both register their output with the curve registry
automatically, so the derived data flows into fitting the same way an
originally-imported plate would.
