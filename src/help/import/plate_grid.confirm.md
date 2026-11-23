---
id: import.plate_grid.confirm
title: Confirming the plate layout
audience: user
category: procedural
see_also: [compute.import.description_shape_binding, compute.import.dilution_source_precedence, import.design_map]
---
Each well is typed (Standard, Control, Blank, or Sample) and described
automatically from the uploaded file. This step is where you check that work:
every plate needs at least one Sample, one Standard, and (usually) one Blank
before you can continue — a plate missing any of these is flagged as an error
here, not later. Click a well, row, or column heading to select it, then set its
type, description and/or Source directly. Three ways to reach many wells at once:

* **Same wells on every plate** repeats the edit at the same positions. Wells
  that are empty on another plate (no type, no reading) are left alone, so a
  batch whose plates use different columns is safe.
* **Description matches the selected well** repeats it wherever that exact
  description appears.
* **Description pattern** needs no selection: `QC1*, QC2*` reaches every well
  whose description starts with QC1 or QC2, on every plate and at every
  dilution. `*` matches anything, `?` one character, case is ignored. The count
  of matching wells is shown before you apply.

**Source.** Type a Source, or tick *Set Source from the plate name*: the parts
of the plate (file) names that differ between plates are proposed — supplier,
reader voltage, a run label — and you can untick parts you do not want. A
one-letter misspelling of a part that is otherwise the same on every plate
("supplier" / "supplie") and plain numbers such as dates are not proposed. The
Source you set overrides whatever the description rule would give, and is cut
to 25 characters.

**No test samples.** Reagent or standards bridging and assay optimisation runs
can have no test samples at all. When a plate has none, tick *This experiment
has no test samples*: the plates then load with standards, blanks and controls
only, and the validation lists a warning per plate instead of an error. Nothing
artificial is added.

::: more
Some wells can only be typed with a *suggested* match (e.g. a low-response well
proposed as a Blank from its signal alone, not from its label). A suggestion is
shown with its confidence and reason and must be explicitly reviewed — "I have
checked these — confirm them" — before the layout counts as resolved, even if you
don't change anything. This is a deliberate checkpoint: an automatic guess should
never silently become the record without a human looking at it.

**Sample identifier.** Each well's `sampleid` is taken from its description: the
dilution ratio is removed (it has its own column), then everything but letters
and digits, and the result is cut to 15 characters — "QC1 (Orange) 1:50"
becomes `QC1Orange`, a barcode stays a barcode. A well with no description keeps
the number from its type code. If two different descriptions end up with the
same identifier, the validation says so.
:::
