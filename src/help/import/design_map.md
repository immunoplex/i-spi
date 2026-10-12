---
id: import.design_map
title: Assigning the study design from a spreadsheet
audience: user
category: procedural
see_also: [import.plate_grid.confirm]
---
Use this when the plate's descriptions don't carry the study design, or you want
to correct it in bulk: **sample type**, **subject**, **groups**, **timepoint**,
**source** and **dilution** for each well. Instead of fixing wells one at a time
or writing description rules, you fill in a table and I-SPI applies it.

1. **Download the study design template.** It is built for the plates you just
   parsed, with every well already listed and its current type filled in.
2. **Fill in the `current_plates_map` tab**, one row per well. The
   `example_template` tab shows a finished plate. Leave a value out by leaving its
   cell completely empty.
3. **Upload the edited workbook.** It is checked straight away, and every problem
   is listed with its spreadsheet row.
4. Click **Apply the study design to the loaded data** (it stays disabled while
   there are errors).

What each type needs: a **sample** (X) needs a subject and a timepoint, and should
have a dilution; a **standard** (S) needs a source and a dilution (an index such as
`S3` marks the standard point); a **blank** (B) needs a source and a dilution
(usually 1); a **control** (C) needs a dilution. A dilution is the denominator,
`100` or `1:100`.

You can upload and apply a table that covers only some wells: the rest keep what the
description field says. If you leave `sample_type` empty, the well keeps its
current type.

::: more
**What applying does.** The sample types are written to the plate layout above (so
the grid and the layout check update), and the other columns become the identity
of those wells in the layout template. Wells covered by the table no longer need
description rules, and standards covered by it no longer need an entry in the
standards dilution reference. If every occupied well is covered, the description
and dilution steps are satisfied by the table alone.

**Changing your mind.** Retyping a well on the grid afterwards hands that well
back to the description rules. **Remove the applied design** restores the layout to
how it was before you applied it. Parsing the instrument files again also clears
it.

**Checks.** The table is validated the way the description field is: plate and well
must exist on the loaded plates, the type must be X, S, C or B, dilutions must be a
whole number or a 1:N ratio, and each type must carry what it needs (above). Rows
for wells with no readings are ignored with a warning.

**Step 4 with a design applied.** Wells the design covers are not parsed from the
description field, so step 4 only asks for description rules for the wells that are
left (none, if the design covers every occupied well). In place of the rule
approvals, step 4 runs a **design integrity check** on the applied design and shows
the wells per plate and type. It reports as errors (these keep the step closed) any
covered well still missing what its type needs. It reports as warnings: a subject
assigned to more than one group, standard dilutions that do not increase with the
point number or differ between replicates of a point, and a dilution or source that
disagrees with the one in the instrument file (the design wins). Warnings are
carried into the validation table when you check the completed layout.
:::
