---
id: data.actions.json_bundle
title: Export (JSON + settings)
audience: user
category: procedural
see_also: [data.actions.rdata_bundle, data.actions.excel_bundle]
---
The same content as [[data.actions.rdata_bundle|the RData Bundle]] — every
table, the manifest, resolved settings, and annotations — written as plain
JSON instead of an R-specific format, so it's readable by any language or
tool, not just R. Use [[data.actions.excel_bundle|the Excel workbook]]
instead if you want it as a spreadsheet rather than a text format.

**Precision-weights metadata.** The bundle also carries `weights_meta`: which weighting model produced the stored weights (multiplicative or additive), the curveRweights version, how many samples, curve groups and censored samples, and when they were computed. The per-table CSVs for the weights tables carry `model` and `weights_pkg_version` on every row. See [[glossary.precision_weight|precision weights]] and [[glossary.reliability|reliability]].
