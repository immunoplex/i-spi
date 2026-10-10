---
id: data.actions.excel_bundle
title: Excel Bundle
audience: user
category: procedural
see_also: [data.actions.rdata_bundle, data.actions.json_bundle]
---
The same content as [[data.actions.rdata_bundle|the RData Bundle]] and
[[data.actions.json_bundle|the JSON export]] — every table, the manifest,
resolved settings, and annotations — written as a single `.xlsx` workbook,
one sheet per table, for opening directly in Excel. A table with no rows
yet shows a one-row "Not computed yet" placeholder sheet rather than being
left out, so the sheet list always matches the manifest.
