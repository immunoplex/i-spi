---
id: qc.linearity.intervals
title: Interval and Bootstrap
audience: user
category: procedural
see_also: [qc.linearity.agreement, qc.linearity.error_model]
---
**Interval.** Every estimate on this tab (slope, intercept, CCC, the shaded band
around the line) comes with a range showing how well it is pinned down. The
*Interval* box sets how confident that range is: 90%, 95% (default) or 99%. If
the study were repeated many times, about 95 of every 100 intervals built this
way would contain the true value. If the interval for the slope contains 1, the
data do not show a difference from a 1:1 relationship.

**Bootstrap.** For ordinary regression the interval comes from a textbook
formula. The *Both* models (Deming and York) have no simple formula, so the
ranges are found by resampling: the pairs are drawn with replacement many times,
the line is refitted each time, and the spread of the refitted lines gives the
interval. The *Bootstrap* number is how many resamples are used.

- **200 (default)** is a good balance of speed and stability for thousands of pairs.
- **Larger values** (500–2000) give smoother, more repeatable intervals but take
  longer. Use them for the numbers you will publish.
- **0** skips resampling. Intervals for the *Both* models and for CCC are then not
  shown.

The resampling uses a fixed starting point, so the same data and settings always
give the same numbers.
