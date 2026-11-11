---
id: qc.linearity.weighting
title: Regression uses (all pairs, the pcov gate, or precision weights)
audience: user
category: procedural
see_also: [qc.linearity.error_model, qc.linearity.agreement, glossary.pcov, glossary.loq_gating, glossary.errors_in_both_axes]
---
**The question.** Not every estimate is equally trustworthy. An estimate near the
top or bottom of the standard curve has a large pcov (it is imprecise), because
the curve is flat there. "Regression uses" decides how much those shaky
estimates are allowed to influence the line.

**The three choices**

- **All pairs.** Every valid pair counts equally, however precise. Simple and
  transparent. A handful of very noisy pairs can pull the line, so the fitted slope
  may be off even if most of the data agree.
- **Pairs within the pcov gate.** Only pairs where *both* estimates have a pcov
  at or below a limit you set are used (limits for x and for y; 20% is the usual
  precision limit). Pairs outside the gate stay on the plot but are shown
  faded, and the line ignores them. The number of pairs used, and how many the
  gate removed, are shown under the plot.
- **All pairs, weighted by precision.** Nobody is thrown away. Each pair counts in
  proportion to how precise it is (its weight is 1 ÷ pcov²), so a pair with
  pcov 10% counts four times as much as one with pcov 20%. Pairs with no pcov
  cannot be weighted and are left out (the count is shown). With *Both* carrying
  error, each point's own pcov is used on each axis (the York method).

**Gate limits.** The two boxes (*gate: x pcov ≤* and *y pcov ≤*) set the gate. They
apply when "Pairs within the pcov gate" is chosen, and they are also used for the
sensitivity comparison below even when you did not choose the gate.

**A word of caution about the gate.** pcov is highest at the ends of the curve, so
the gate cuts away the lowest and highest concentrations. Those ends are exactly
where non-linearity and hook effects (very high concentrations reading low)
show up, so a gated fit can look better than the data deserve. The gate also
narrows the range of concentrations that remain, and a narrow range makes the
correlation look weaker and the slope more attenuated even when nothing is
wrong. It is a useful robustness check, but precision weighting is usually the
better primary choice, because it reduces the influence of poor estimates
without deleting them.

**You get the other two for free.** Whatever you choose, the other two are
fitted as well and listed in *Sensitivity to weighting* under the plot and in the
report (the primary one is marked). Read it like this:

- If all three give about the same slope and agreement, the conclusion does not
  depend on how the data were treated. Report it with confidence.
- If they differ, the answer depends on the noisy or extreme pairs. Report the
  range, and look at which gate classes the unusual points have (their outline
  and fill colours).

**What to pick**

| Situation | Suggested choice |
|---|---|
| First look, or a small number of pairs | All pairs |
| A mix of precise and imprecise estimates, many near the curve ends | Weighted by precision |
| You must show that the in-range data agree (for example a validation report) | Within the pcov gate, and also quote the other two |
