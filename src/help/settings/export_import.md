---
id: settings.export_import
title: Exporting and importing settings
audience: user
category: procedural
---

Export writes only the settings this study **overrides** — not the full resolved
cascade — so re-importing that file reproduces exactly those overrides without
baking in whatever the system defaults happened to be at export time. Import always
applies to the study you currently have open; check **Replace: clear this study's
existing overrides first** if you want the import to fully replace the current
overrides rather than merge with them.
