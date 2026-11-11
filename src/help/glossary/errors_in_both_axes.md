---
id: glossary.errors_in_both_axes
title: Regression when both estimates are noisy
audience: both
category: glossary
topic: Agreement and linearity
see_also: [qc.linearity.error_model, qc.linearity.delta, qc.linearity.weighting, glossary.agreement_ccc_ba, glossary.latent_consensus, glossary.pcov, glossary.measurement_error]
references:
  - text: "Deming WE (1943). Statistical Adjustment of Data. Wiley."
  - text: "York D, Evensen NM, Martinez ML, Delgado JDB (2004). Unified equations for the slope, intercept, and standard errors of the best straight line. American Journal of Physics 72:367-375."
  - text: "Cornbleet PJ, Gochman N (1979). Incorrect least-squares regression coefficients in method-comparison analysis. Clinical Chemistry 25:432-438."
---
Comparing two estimates of the same samples usually means drawing a line through
points. The line most people learned, **ordinary least squares**, treats the
x values as exact and the y values as noisy. When *both* are noisy, as when
two assays or two curve fits each read the same sample, that assumption biases
the result.

**The problem: attenuation.** Each x carries a little random error, so the points
scatter sideways as well as up and down. Ordinary regression treats all of that
as vertical scatter and draws a line that is too flat. With equal noise on both
axes and a true slope of 1, the expected ordinary slope is the correlation r, so a
fitted slope of 0.85 can arise from noise alone. The effect grows when the noise is
large relative to the spread of true concentrations. A second symptom: regressing
y on x and x on y gives two different lines.

**The remedy.** Errors-in-variables regression allows for noise in both
directions.

- **Deming regression** needs one number, the ratio of the error variances
  (how much noisier y is than x; see
  [[qc.linearity.delta|Error-variance ratio]]). A ratio of 1 gives orthogonal
  regression, which minimises the perpendicular distance from each point to the
  line. Swapping the axes gives exactly the inverse line, so it does not matter
  which estimate is x.
- **York regression** lets every point have its own x and y error. I-SPI uses it
  when pairs are weighted by precision, with each estimate's error taken from its
  pcov.

I-SPI takes the size of each estimate's error from its
[[glossary.pcov|pcov]]: pcov = 100·ln(10) times the standard error of log10
concentration, so on the log scale the error is pcov ÷ (100·ln 10).

::: more
**Formula (Deming).** With sample variances s_xx, s_yy, covariance s_xy and
δ = (error variance of y) ÷ (error variance of x):

b = [ s_yy − δ·s_xx + √((s_yy − δ·s_xx)² + 4δ·s_xy²) ] ÷ (2·s_xy), a = ȳ − b·x̄.

As δ becomes large the line approaches ordinary regression of y on x; as δ
approaches 0 it approaches the regression of x on y.

**Intervals.** The Deming and York fits have no simple standard-error formula, so
I-SPI bootstraps (resamples the pairs). Ordinary regression uses the usual
t-based intervals.

**When ordinary regression is fine.** When x is known almost exactly (a
certified reference value, a prepared dilution factor) or when you only need to
predict y from x. Showing both fits side by side tells you how large the
attenuation was.

**Limits.** All these methods assume the error sizes you give are about right.
If real disagreement (plate effects, matrix effects) is larger than pcov
suggests, the line is still unbiased but its interval is too narrow; the report
flags this through the standardized differences.
:::
