---
id: compute.import.dilution_source_precedence
title: Where a well's dilution actually comes from
audience: user
category: compute-decision
see_also: [compute.import.description_shape_binding, settings.standard_dilution_reference]
references:
  - text: "RBX_DILUTION_AUTHORITATIVE_SOURCE_PLAN.md (i-spi-refactor repo)"
---

A well's [[glossary.dilution_factor|dilution]] can come from more than one place, and
when a file offers more than one, I-SPI resolves them in a fixed order rather than
picking arbitrarily:

1. **The instrument file's own numeric value** — used for Sample and Control wells
   whenever the uploaded file carries one. Never overridden by anything else for
   those wells.
2. **A ratio written in the Description text** (e.g. `1:100`) — used whenever the
   instrument didn't supply a value, for any well type.
3. **This experiment's saved dilution reference** — fills in whatever is still
   unresolved after the first two checks, for any well type.
4. **Otherwise the well is blocked** until one of the above supplies a value — you'll
   be prompted to enter it directly (see [[settings.standard_dilution_reference]]).

::: more
Standard wells are a deliberate exception to step 1: the Bio-Plex binary format has
no field for a standard point's *true* dilution — the number it reports there is
always a fixed placeholder, never a real reading — so instrument values are only
ever trusted for Sample and Control wells, never Standards. A Standard's dilution
always comes from its Description text (step 2) or the saved reference (step 3).
This is why Standards are the specimen type you're most likely to be asked for a
manual dilution reference, even on a file where Samples and Controls resolve
automatically.
:::
