---
id: qc.linearity.delta
title: Error-variance ratio (how noisy is y compared with x?)
audience: user
category: procedural
see_also: [qc.linearity.error_model, qc.linearity.weighting, glossary.errors_in_both_axes, glossary.pcov]
---
**What it asks.** If both estimates are noisy, the fitted line needs to know how
the noise is shared between them. Is y about as noisy as x? Much noisier? Much
quieter? The error-variance ratio is that answer as one number:

> ratio = (noise in y)² ÷ (noise in x)²

"Noise" here means the typical size of the error of a single estimate (its
standard deviation). It is squared because statistics work with variances.

**How to read a value**

| Ratio | Meaning |
|---|---|
| 1 | y and x are equally noisy. |
| 4 | y's noise is twice x's (2 × 2 = 4). |
| 9 | y's noise is three times x's. |
| 0.25 | y's noise is half of x's. |
| 0.01 | y is ten times quieter than x. |

**What it does to the line.** A larger ratio says y is the noisy one, so the line
behaves more like ordinary regression of y on x. A smaller ratio says x is the
noisy one, so it behaves more like regressing x on y. A ratio of 1 splits the
difference: the line is the one that minimises the perpendicular (shortest)
distance from the points to the line, called orthogonal regression.

**The three choices**

- **From each axis's pcov (default).** I-SPI already knows how precise every
  estimate is: pcov is the percent coefficient of variation of the
  concentration. It converts each pcov to a noise size on the scale you are
  fitting, averages the squared noise for the x estimates and for the y
  estimates, and divides. You do not have to know anything; the data says how
  noisy each side is. Use this unless you have a reason not to.
- **Equal: exchangeable repeats (1).** Use for duplicates, or whenever x and y
  are the same kind of measurement and it would be arbitrary which is called x
  (for example two wells of the same sample). It also protects the answer from
  being steered by uneven pcov values you do not trust.
- **Custom.** Type your own ratio when you know it from validation work. For
  example, if method A has a coefficient of variation of 5% and method B 10%, and
  A is on x, the ratio is (10 ÷ 5)² = 4.

**When the box is not shown.** It only applies to *Both* with *all pairs* or
*within the pcov gate*. With *weighted by precision*, every point brings its own
noise on both axes, so no single ratio is needed.

**Checking your choice.** Look at *Standardized differences* in the report
(under the transformation). If pcov really describes the whole error, they have
a spread of about 1. A clearly larger spread means there is extra disagreement
that pcov does not explain (for example a plate effect), and the ratio from pcov
is understating the noise.
