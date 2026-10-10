---
id: settings.clone_study
title: Cloning study components
audience: user
category: procedural
see_also: [settings.delete_components]
---

Cloning is a **shallow copy**: it copies the raw plate data, the curve registry,
this study's settings overrides, and its descriptive annotations (analyte / level /
order) into a new project and study you specify. **Fitted results are not copied** —
run Compute-fits again on the clone to generate them. New `curve_id`s and
multiplate-group ids are generated automatically, so the clone's fits are entirely
independent of the source study's.

Like [[settings.delete_components|deleting study components]], this is a two-step
flow: **Preview** shows exactly what would be cloned before anything happens.
