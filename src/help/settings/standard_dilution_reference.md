---
id: standard_dilution_reference
title: Standards dilution reference
audience: user
params: [standard_dilution_reference]
---

Some Standards wells can't have their dilution read from the instrument file
or from the Description text (the instrument reports no usable value for
Standards, and the lab's own label for a standard point — e.g. `S1`, `STD_1`,
`Std 1 (Hi)` — doesn't always spell out a ratio). This setting is where the
true dilution for each of those labels is recorded, once, so every later
import in this study or experiment resolves them automatically instead of
asking again.

**Format.** A JSON array, one object per label, each with `description` (the
**exact** text as it appears in that well's Description field — matching is
case-sensitive after trimming whitespace, not a pattern) and `dilution` (a
positive number):

```json
[
  {"description": "STD_1", "dilution": 50},
  {"description": "STD_2", "dilution": 125},
  {"description": "STD_3", "dilution": 313},
  {"description": "STD_4", "dilution": 781},
  {"description": "STD_5", "dilution": 1953},
  {"description": "STD_6", "dilution": 4883},
  {"description": "STD_7", "dilution": 12207},
  {"description": "STD_8", "dilution": 30518},
  {"description": "STD_9", "dilution": 76294},
  {"description": "STD_10", "dilution": 190735},
  {"description": "STD_11", "dilution": 476837}
]
```

A Bio-Plex `.rbx`/`.srbx` file that labels its standards `S1`..`S11` rather
than `STD_1`..`STD_11` needs `description` to say `S1`, `S2`, … instead —
whatever text is actually sitting in the Description column for that well,
not a label of your choosing.

**Don't hand-type this.** Both the place that prompts for it during import
("3. Standards dilution reference") and this field itself have a "paste from
a spreadsheet" box: copy a two-column *Standard point / Dilution factor*
table straight out of Excel and it's converted and merged in automatically,
matched to the right wells by description text (or by the trailing number
when the exact text differs, e.g. a pasted `STD_1` against a well labeled
`S1`). Editing the JSON above directly is only for a correction or a one-off
tweak you already know the shape of.

**Scope.** Like every other cascade setting, this value is remembered at
whatever scope it was set at (project, study, or narrower) and inherited
below that — set it once at the study level and every experiment in that
study skips re-asking for the same standard-point labels.
