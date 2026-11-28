---
id: glossary.blank_wells
title: Blank wells
audience: user
category: glossary
topic: Plate layout and wells
see_also: [glossary.standard_wells, glossary.control_wells, glossary.sample_wells, glossary.response_offset, schema.xmap_buffer]
---
**Blank** (buffer) wells carry no analyte and are used to estimate background
signal. In I-SPI they are used for quality checks and are shown on the curve
plot; they enter the curve fit only if the blank handling setting is
*included* (their mean becomes one extra calibrator point). Their mean is
subtracted from standards, controls and samples only when blank subtraction is
chosen (never by default), and blanks never decide the
[[glossary.response_offset|response offset]].
