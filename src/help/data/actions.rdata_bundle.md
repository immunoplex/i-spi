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

**Precision-weights metadata.** The bundle also carries `weights_meta`: which weighting model produced the stored weights (multiplicative or additive), the curveRweights version, how many samples, curve groups and censored samples, and when they were computed. The per-table CSVs for the weights tables carry `model` and `weights_pkg_version` on every row. See [[glossary.precision_weight|precision weights]] and [[glossary.reliability|reliability]].
