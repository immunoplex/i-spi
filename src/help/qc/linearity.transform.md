---
id: qc.linearity.transform
title: Transform (None, Map y onto x, or Harmonize)
audience: user
category: procedural
see_also: [glossary.latent_consensus, qc.linearity.error_model, qc.linearity.consensus, qc.linearity.agreement]
---
**What a transform is for.** If two sets of estimates differ in a consistent way
(one is 20% higher, or drifts upward at high concentration), you may want to
re-express them so they can be used together. The transform rewrites the values
using the line that was just fitted. It never changes the stored results; it only
changes what is plotted and reported here.

**None (default).** Show the estimates as they are. Use this to look at the
relationship, especially for dilutions, where a slope other than 1 is the
finding itself.

**Map y onto x (1:1).** Every y value is rewritten so that the line of best fit
lies exactly on the 1:1 line. x is left alone. Think of it as *converting y into
x's units*.

- Use when x is the one you trust or define as the standard: a validated method,
  the plate you treat as the reference, the neat or lowest dilution. You get a
  recipe to put the other estimates on the reference scale.
- Example: y reads 20% higher than x at every concentration. Mapping y onto x
  divides every y by 1.2.
- Care: it works by undoing the fitted slope, so it is only as good as that
  slope. Use it with the *Both* error model; the ordinary-regression slope is too
  shallow (see [[qc.linearity.error_model|Which estimates carry error?]]) and
  would over-correct.

**Harmonize both to the latent concentration.** Neither estimate is treated as
the truth. Each is seen as a noisy reading of one underlying (latent)
concentration, and both sets are moved *half way* to it: the correction is shared
equally between x and y, so it does not matter which one you called x. Both axes
change, and the points then sit on the 1:1 line.

- Use for two ways of measuring the same thing where neither is a reference:
  frequentist and Bayesian estimates, two plates, two kits, two labs.
- It is also the step that makes a single consensus concentration per sample
  possible (see [[qc.linearity.consensus|Consensus per sample]]).
- Needs the *Both* error model and a positive slope. If that is not the case the
  transform is skipped and a note says why.
- **Do not use it for dilutions.** If a sample read at two dilutions gives
  different concentrations, that is dilutional non-linearity (matrix effects or
  hook effect), a property of the sample. Harmonizing would hide it by moving
  the numbers together. Report the non-linearity and prefer estimates from
  dilutions that lie in the linear range.

**What you see.** Axis labels say that values were mapped or harmonized, hover
shows both the original and the transformed values, and the line drawn is a
refit of the transformed data (intercept 0 and slope 1 by construction). The
report records the formula and its parameters so the correction can be repeated.

**Neither transform reduces scatter.** They remove a consistent bias between the
two sets. Pairs that scatter widely around the line still do.

| If you want to… | Choose |
|---|---|
| Look at, or test, the relationship | None |
| Express y on the scale of a reference x | Map y onto x |
| Put two equally good methods on one scale, or get consensus concentrations | Harmonize |
| Check dilutional linearity | None |
