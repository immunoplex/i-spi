---
id: settings.source_names
title: Canonicalizing raw instrument source names
audience: user
category: conceptual
---

The same physical standard or control can end up logged under different raw
names across different runs, instruments, or operators — "Inhouse Ref" on one
plate, "In-house Reference" on another, a typo variant on a third. Left alone,
the app would treat those as different sources, which breaks any comparison
that's supposed to track one source over time (the Compare-fits tab in
particular).

This screen maps each raw name to one canonical name, so everything
downstream treats the aliased names as the same source. It also lists
raw-source names that appear in your data but aren't mapped to a canonical
name yet, so you can catch a new variant as it shows up rather than
discovering it later as a silent gap in a comparison.

::: more
This mapping is **global, not per-study** — it applies to every study, not
just the one you're currently viewing. It's intentionally sidebar-level
rather than tucked inside one study's settings.
:::
