---
id: data.actions.rdata_bundle
title: RData Bundle
audience: user
category: procedural
see_also: [data.actions.json_bundle, data.actions.excel_bundle]
---
Downloads every table on this tab for the current study/experiment as one
`.RData` file — a manifest describing each table, the study's resolved
settings, its annotations, and the full (uncapped) data for every table,
ready to `load()` directly in R. Use [[data.actions.json_bundle|the JSON
export]] or [[data.actions.excel_bundle|the Excel workbook]] instead if you
need the same bundle outside R.
