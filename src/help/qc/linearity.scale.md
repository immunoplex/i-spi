---
id: qc.linearity.scale
title: Fit and axes (log10 or natural)
audience: user
category: procedural
see_also: [qc.linearity.agreement, qc.linearity.delta, glossary.pcov]
---
**What it decides.** Whether the line is fitted, and the plot drawn, on the
log10 of each concentration or on the concentrations themselves.

**log10 (default).** Laboratory concentrations usually span several orders of
magnitude and the error is proportional (an estimate of 10,000 is off by a
larger amount than an estimate of 10, but by a similar *percentage*). On a log
scale that proportional error becomes constant, points spread evenly along the
line, and low concentrations are not squashed against the axis. Differences
become fold changes:

- an **intercept** of 0.08 means one estimate is 10^0.08 = 1.2-fold the other at
  every concentration;
- a **slope** of 0.9 means the fold difference changes with concentration (the
  estimates drift apart as concentration rises).

Concentrations that are zero or negative cannot be logged, so those pairs are
left out (the number is shown).

**Natural.** The line is fitted to the concentrations as they are. Choose it when
the range is narrow, when absolute differences matter more than fold changes, or
when you want a result in the same units as the data. The intercept is then an
offset in concentration units and the slope is a proportional change. Large
concentrations dominate such a fit, so it is less informative for assays spanning
decades.

**How pcov fits in.** pcov is a percentage. On the log scale it converts exactly
to an error on log10 concentration. On the natural scale it converts to an error
of (concentration × pcov ÷ 100), which is an approximation that is best when pcov
is modest.

If you are unsure, use log10.
