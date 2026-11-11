---
id: qc.linearity.agreement
title: Reading the agreement numbers
audience: user
category: procedural
see_also: [glossary.agreement_ccc_ba, qc.linearity.weighting, qc.linearity.error_model, qc.linearity.scale]
---
Four things are reported. Read them in this order.

**1. Concordance (CCC).** One number for how well the points follow the 1:1 line.
It is like a correlation, but it also drops when one estimate is shifted up or
down or has a different spread, which a correlation ignores. 1 is perfect
agreement.

| CCC | Strength |
|---|---|
| above 0.99 | almost perfect |
| 0.95 – 0.99 | substantial |
| 0.90 – 0.95 | moderate |
| below 0.90 | poor |

**2. Bland–Altman bias and limits of agreement.** For each pair, take the
difference between the two estimates. The *bias* is the average difference. The
*limits of agreement* are the range containing about 95% of the differences. On the
log scale these are fold changes: a bias of 1.10-fold with limits 0.7- to
1.7-fold means y is on average 10% higher, and for individual samples it can be
anywhere from 30% lower to 70% higher. Whether that is acceptable is a question
about your assay, not about statistics. The *trend* line tells you whether the
difference changes with concentration; a significant trend means a proportional
bias.

**3. The fitted line (intercept and slope).** On the log scale:

- the **intercept** is a constant fold difference (10 raised to the intercept) at
  every concentration;
- the **slope** tells you whether the fold difference changes with concentration.
  A slope of 1 means it does not. A slope of 0.9 means the estimates drift apart
  as concentration rises (for dilutions this is dilutional non-linearity).

Each comes with an interval. If the interval for the slope includes 1 and the
interval for the intercept includes 0, the data show no difference from 1:1.

**4. Correlation (r) and R².** These say how tightly the points hug *a* line, not
whether it is the 1:1 line. Two assays can have R² of 0.99 and differ two-fold.
They are kept for familiarity; rely on the CCC and Bland–Altman for agreement.

**Findings.** The coloured box lists plain-language findings (for example
"agreement is poor" or "the slope depends on how pairs are weighted") with the
numbers behind them and a suggested action.

More: [[glossary.agreement_ccc_ba|Agreement: concordance and Bland–Altman]].
