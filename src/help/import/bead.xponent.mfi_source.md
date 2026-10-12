---
id: import.bead.xponent.mfi_source
title: Choosing the MFI source in an xPONENT file
audience: user
category: procedural
see_also: [import.bead.xponent.upload]
---
An xPONENT export holds **more than one set of readings** for the same wells, each in
its own block of the file. **MFI source** chooses which block becomes the response
that I-SPI imports, validates and fits:

- **Net MFI** — the median fluorescence with the protocol's background already
  subtracted by xPONENT (the same as Median when the protocol defines no
  background). This is the default, and what this importer always used.
- **Median** — the raw median fluorescence per well, with no background removed.
- **Average Net MFI** — the average of Net MFI over wells that share a sample name.
  It has one value per *sample name*, not per well, so every replicate well of a
  sample receives the same number.

The blocks are found by their **label**, not their position, so it doesn't matter
which one a lab's export lists first. The control shows the blocks your selected
file(s) contain, in the order they appear, and offers only the sources that
**every** selected file has — a batch is always read from one source, never mixed.

If you change the source after parsing, click **Parse uploaded file(s)** again; the
layout template can't be downloaded until you do, so it can't silently use the old
choice.

::: more
**Background is subtracted once, not twice.** If you pick Net MFI, xPONENT has
already removed its protocol background, so don't also choose a blank-subtraction
option based on your [[glossary.blank_wells|blank wells]] when fitting. If you want
I-SPI to apply blank handling itself, import **Median**.

**Average Net MFI and replicates.** Because replicate wells receive an identical
value, the well-to-well variation between them is gone: they no longer carry
independent information about [[glossary.measurement_error|assay error]]. Prefer Net
MFI or Median unless the averaged value is specifically what you need.

**Not recorded in the database.** The chosen source is shown in the parse status but
isn't stored with the plate, so use the same source for every plate in an experiment.
:::
