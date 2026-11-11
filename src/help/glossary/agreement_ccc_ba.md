---
id: glossary.agreement_ccc_ba
title: "Agreement: concordance and Bland–Altman"
audience: both
category: glossary
topic: Agreement and linearity
see_also: [qc.linearity.agreement, qc.linearity.weighting, glossary.errors_in_both_axes, glossary.pcov]
references:
  - text: "Lin LI (1989). A concordance correlation coefficient to evaluate reproducibility. Biometrics 45:255-268."
  - text: "Bland JM, Altman DG (1986). Statistical methods for assessing agreement between two methods of clinical measurement. Lancet 327:307-310."
  - text: "McBride GB (2005). A proposal for strength-of-agreement criteria for Lin's concordance correlation coefficient. NIWA Client Report HAM2005-062."
---
**Correlation is not agreement.** A correlation coefficient (r, or R²) says how
tightly points follow *some* straight line. Two assays can have R² of 0.99 and
yet one reads twice what the other does. For the question "do these two
estimates agree?", use measures that compare the points with the 1:1 line.

**Lin's concordance correlation coefficient (CCC).** A single number between −1
and 1 that is 1 only when every point lies on the 1:1 line. It falls when the
points scatter, when one estimate is shifted relative to the other, and when
their spreads differ. It treats the two estimates symmetrically. Rough
guide for the strength of agreement: above 0.99 almost perfect, 0.95–0.99
substantial, 0.90–0.95 moderate, below 0.90 poor.

**Bland–Altman analysis.** For each pair compute the difference between the two
estimates and their average. Then report:

- the **bias**: the mean difference;
- the **limits of agreement**: the mean difference ± 1.96 standard deviations of
  the differences, a range expected to contain about 95% of pairs;
- a **trend** test: whether the difference changes with the average
  concentration, which signals a proportional bias.

On the log scale a difference of logs is the log of a ratio, so I-SPI reports bias and
limits as fold changes (for example 1.10-fold, with limits 0.7- to 1.7-fold).
Whether a bias or a width of limits is acceptable depends on the purpose of the
assay and has to be decided by the lab.

I-SPI leads with the CCC and the Bland–Altman numbers and keeps r, R² and Spearman's
rank correlation alongside, because they are familiar and useful for spotting
scatter.

::: more
**CCC formula.** CCC = 2·s_xy ÷ (s_x² + s_y² + (x̄ − ȳ)²), using population
(divide by n) variances. It equals r times a "bias correction" factor between 0
and 1 that accounts for the shift and the difference in scale. Its interval in
I-SPI comes from a bootstrap.

**Limits of agreement have uncertainty too.** The report gives intervals for the
bias and for each limit; with few pairs they are wide.

**Duplicates.** For repeated wells of the same sample, the within-pair standard
deviation, geometric coefficient of variation (CV) and the intraclass correlation
(ICC) are also reported. They describe repeatability and do not depend on which
well is called x.
:::
