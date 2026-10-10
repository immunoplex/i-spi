---
id: compute.import.description_shape_binding
title: Configuring the description field
audience: user
category: compute-decision
see_also: [compute.import.dilution_source_precedence]
references:
  - text: "assay_shape_rules.R, assay_shape_ui.R (i-spi-refactor repo) -- inline design docs"
---

Every well's Description text needs to be broken down into what it actually means —
which part is the patient ID, which part is the timepoint, which part (if any) is a
dilution. Wells with the same **shape** (the same pattern of tokens once split on
your chosen separator characters) are grouped together automatically, and you tell
I-SPI once, per shape, what each part means: a specific token position, a value
matched by its format (a number, a ratio, a date-like pattern), a fixed constant for
every well in the group, or a value taken from the well's specimen type itself. That
one binding then applies to every well sharing that shape — you don't repeat the
work per well.

A **dilution ratio** (`1:100`, `1/50`, and similar) is a special case: once a token
position is bound to dilution by *format* rather than by fixed position, I-SPI finds
it wherever it falls in the text, even if other labs' descriptions put it in a
different spot. That's what feeds step 2 of
[[compute.import.dilution_source_precedence|the dilution precedence order]] — a ratio
recognized here is used before I-SPI ever asks you for a manual reference value.

::: more
This replaced an older, simpler approach that assumed one fixed token order per
specimen type — workable for files where every Standard, Control, and Sample
description had the same shape, but not for a batch mixing `"QC 1"`, `"SPIKE A"`,
and `"LtyUp"` under the same Control type (one, two, and one token respectively).
Binding by shape rather than by type handles that mix without forcing every lab's
export into one template.
:::
