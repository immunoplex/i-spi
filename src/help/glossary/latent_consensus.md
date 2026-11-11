---
id: glossary.latent_consensus
title: Latent concentration and consensus
audience: both
category: glossary
topic: Agreement and linearity
see_also: [qc.linearity.transform, qc.linearity.consensus, glossary.errors_in_both_axes, glossary.agreement_ccc_ba]
references:
  - text: "General method-comparison and measurement-error modelling (see Carroll RJ, Ruppert D, Stefanski LA, Crainiceanu CM (2006). Measurement Error in Nonlinear Models, 2nd ed., Chapman & Hall/CRC)."
---
**Latent concentration.** When two assays (or two curve fits, plates or labs)
measure the same sample, we can think of each reading as a noisy, possibly
rescaled view of one underlying true concentration that we never see directly.
That underlying value is the *latent* concentration. Each estimate is it, scaled
and shifted a little differently, plus noise.

**Harmonizing.** If the two sets differ in a consistent way (one is 20% higher, or
the gap widens with concentration), we can undo the difference. There are two
ways:

- **Map y onto x** treats x as the reference and rewrites y in x's units.
- **Harmonize** treats neither as the reference. The correction is split equally,
  so each set moves half way to a common scale, and the result is the same
  whichever one you call x. That makes sense when two methods are equally
  entitled to be the truth.

Harmonizing removes consistent bias. It does not remove random scatter. And it
should not be used to "fix" dilutional non-linearity: a sample that reads
differently at different dilutions has a real problem that harmonizing would
conceal.

**Consensus.** After harmonizing, each sample has several estimates on one scale.
The consensus is their weighted average, with weights proportional to their
precision (inverse variance), so precise estimates count for more. It comes with
an interval that shrinks as more estimates agree. A heterogeneity check (Q per
degree of freedom) shows whether the estimates disagree more than their pcov
predicts, which would mean the interval is too optimistic.

::: more
**Model.** Let x = α₁ + β₁·z and y = α₂ + β₂·z for latent z, with the constraints
β₁·β₂ = 1 and α₁ + α₂ = 0 so the correction is shared equally. From the fitted
line y = a + b·x:

- z_x = √b · (x + a ÷ (1 + b))
- z_y = (y − a ÷ (1 + b)) ÷ √b

A pair that lies exactly on the line has z_x = z_y. The variances are
b·sd_x² for z_x and sd_y² ÷ b for z_y, and the consensus is the inverse-variance
mean ẑ = Σ(z ÷ var) ÷ Σ(1 ÷ var) with standard error 1 ÷ √Σ(1 ÷ var). On the log
scale, 10^ẑ is the consensus concentration.

**If one method is a true reference** (a calibrated standard), anchor to it
instead (α = 0, β = 1 for that axis). That option is not yet built.

**More than two sets** (several dilutions or plates) generalise the model with one
α and β per set; that is not yet built either.
:::
