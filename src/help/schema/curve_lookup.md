---
id: schema.curve_lookup
title: "Curve lookup (curve_lookup)"
audience: both
category: schema
schema_table: curve_lookup
---

The stable registry of calibration curves. Each `curve_id` is defined by the
10-column natural key; everything in the **Results** group joins back here.
Grain: one row per curve (natural key). This is the **Registry** group.
