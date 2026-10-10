---
id: import.wizard.commit
title: Committing the import
audience: user
category: procedural
see_also: [import.wizard.validation, settings.delete_components]
---
Writes the uploaded data to the database. The button stays disabled until
[[import.wizard.validation|validation]] reports zero errors and a study and
experiment are selected; clicking it is the one irreversible-feeling step in
this whole flow, but it isn't truly final — if you need to undo an import,
[[settings.delete_components|deleting study components]] removes it again.
Once committed, re-uploading the same workbook doesn't create a duplicate import
in this session; parse and upload a fresh batch for a new commit.
