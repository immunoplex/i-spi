---
id: qc.linearity.error_model
title: Which estimates carry error?
audience: user
category: procedural
see_also: [glossary.errors_in_both_axes, qc.linearity.delta, qc.linearity.weighting, qc.linearity.transform, qc.linearity.consensus]
---
**The short answer: leave this on *Both* unless you have a specific reason.**
Every concentration on this tab was calculated from a response and a fitted
curve, so every one of them has some measurement error. That includes the
estimate on the x axis.

**What the choice means**

- **Both (Deming / York).** Treats the x estimates and the y estimates as both
  imperfect. The fitted line comes out the same whichever estimate you put on the
  x axis. This is the right model for comparing two noisy estimates of the same
  sample.
- **Only y (ordinary regression).** The regression most people learned in basic
  statistics. It assumes the x values are known exactly and all the scatter is in
  y. That is true when x is a certified reference concentration or a dilution
  factor you prepared, and it is rarely true for two estimates read from curves.

**Why it matters: the slope comes out too shallow**

Imagine two assays that both measure the same samples with some noise, and
they agree perfectly (the true slope is 1). Plot one against the other. Because
each x value is a little too high or too low by chance, the points spread
sideways as well as up and down. Ordinary regression blames all of that spread on
y, and the line it draws is flatter than the truth. A fitted slope of 0.85 can
then appear even though the two assays agree completely. This is called
*attenuation* (or regression dilution). It is larger when the noise is large
compared with how widely the true concentrations are spread.

A handy check: with equal noise on both axes and a true slope of 1, ordinary
regression gives a slope close to the correlation coefficient r (not r²). So a
slope below 1 that is about equal to r is what noise alone produces, not
evidence of a real difference between the estimates.

Another sign that ordinary regression is the wrong tool: regress y on x, then
regress x on y, and the two lines are not the same line. Swap the axes in the
*Both* model and you get exactly the same line.

**Choose *Both* when**

- you compare frequentist with Bayesian, one plate with another, one dilution
  with another, or duplicates with each other (all the usual cases);
- you want the answer not to depend on which estimate is called x.

**Choose *Only y* when**

- x really is a reference with negligible error, and you want to predict y from
  it; or
- you want to reproduce an ordinary-regression result for comparison. Seeing the
  two side by side is a good way to see how much attenuation there was.

**How this interacts with the other options**

- With *Both*, the [[qc.linearity.delta|error-variance ratio]] says how much
  noisier y is than x. With *Regression uses → all pairs weighted by precision*,
  each point's own pcov is used instead.
- The *Harmonize* transform and the per-sample
  [[qc.linearity.consensus|consensus]] need *Both*. They are switched off for
  *Only y*.

More background: [[glossary.errors_in_both_axes|Regression when both estimates
are noisy]].
