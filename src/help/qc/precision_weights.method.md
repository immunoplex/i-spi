---
id: qc.precision_weights.method
title: Precision weighting method & scope
audience: user
category: compute-decision
see_also: [glossary.precision_weight, data.results.precision_weights, data.results.precision_weights_fit, glossary.se_concentration, glossary.pcov]
references:
  - text: "curveRweights precision-weighting.Rmd, §\"The problem: unequal precision across the calibration curve\", §\"The model\""
  - text: "i-spi-refactor dev/HANDOFF_precision_weighting.md (architecture: separate job family from calibration, writes calib_weights/calib_weights_fit)"
  - text: "curveRweights::fit_saturated_weight() — brms Gaussian location-scale model, saturated cell-means location (yi ~ 0 + cell [+ (1|plate)]), scale log(sigma) = gamma_0 + gamma_1*log(cv); curveRweights::interpret_beta1() — the named beta1 regimes cited below."
---

A standard-curve fit tells you how precisely each sample's concentration was
back-calculated — samples read near the flat ends of the curve are less
reliable than ones read on its steep midrange. The traditional fix is a
binary **quantification gate** (LLOQ/ULOQ): keep everything inside it with
equal weight, discard everything outside it. **Precision weighting**
replaces that gate with a continuous, data-estimated weight per sample, so
every observation is kept and down-weighted smoothly by how reliable it
actually was.

**Method to weight** chooses which calibration results to weight — the
Bayesian-fitted or the Frequentist-fitted `calib_samples` for this
experiment — not a different weighting algorithm; the weighting model
itself is the same joint Bayesian scale model either way. **Scope** limits
the job to one feature/antigen instead of the whole experiment. **Design
columns** (timeperiod, agroup) define the experimental cells the weighting
model estimates within — at least one must actually vary for a given
scope, or the model has nothing to estimate against.

::: more
The scale model is a power law relating a sample's precision index
(`se_concentration`, the recommended choice, or `pcov`) to its residual
variability: `sigma_i = phi * se_i^beta1`, giving a weight
`w_i = 1/sigma_i^2`. It's fit as **one joint Bayesian model** together with
a saturated cell-means location model over the chosen design columns — the
two halves share a single fit precisely so the scale parameters (`phi`,
`beta1` — see [[data.results.precision_weights_fit|the weights-fit
results]]) can only be read correctly in light of what the location half
has already explained away.

**phi** is the baseline scale factor — the value of `sigma_i` when the
precision index equals 1, i.e. `phi = exp(gamma_0)` from the fitted
log-scale intercept. `phi = 1` means [[glossary.se_concentration|
se_concentration]] is already a calibrated residual SD: the curve's own
reported uncertainty, taken as-is, correctly predicts how far a reading
actually lands from its cell's true mean. `phi > 1` means real residual
scatter exceeds what curve-fit uncertainty alone predicts — some other
noise source is present (plate-to-plate drift beyond what the model's own
optional plate random intercept already absorbs, specimen handling, other
biological assay noise). `phi < 1` is unusual and points the other way:
the curve's reported SE is itself conservative, wider than the data
actually need.

**beta1** is the power-law exponent — it asks whether `se_concentration`
scales *proportionally* with true residual SD (`beta1 = 1`, the delta
method's theoretical prediction for a 4PL curve) or whether the mapping is
distorted. curveRweights names the regimes explicitly: `beta1` between 0.8
and 1.2 is **calibrated** (pcov/SE ~ residual SD, close to the theoretical
prediction); above 1.2 is **amplified** — a sample with a larger reported
SE is *even less* reliable than its SE alone suggests, so down-weighting
ends up steeper than the raw SE differences imply; between 0.2 and 0.8 is
**compressed** — SE differences overstate how much reliability actually
varies, so weights end up gentler, closer to uniform, than the raw SE
differences would suggest; below 0.2 is **near-uniform**. If every sample
in scope happens to have nearly identical SE (e.g. everything falls in the
curve's well-determined midrange), there's no gradient to estimate a slope
from at all, and `beta1` is simply **not identified** — the model falls
back to an intercept-only, uniform weighting rather than guessing.

**Why timeperiod and group/cohort arm have to be supplied, and why this is
a partition of *error* variance, not *total* variance:** the scale model's
job is to describe how *measurement noise* — not biology — scales with the
curve's own reported uncertainty. A sample's raw deviation from some
overall average mixes two very different things: genuine, systematic
differences in true concentration across cohort arms and timepoints (real
signal — e.g. an arm trending up over time after an intervention), and the
sample-to-sample scatter left once those systematic differences are
accounted for (the actual measurement imprecision the weights are meant to
capture). Fitting the scale model to raw, unpartitioned deviations would
let genuine study-design differences masquerade as assay imprecision — a
cohort arm or timepoint whose *true* concentrations happen to be more
spread out would look "imprecisely measured" even if every individual
reading in it was taken with excellent accuracy, which is exactly backward
for what precision weighting is supposed to capture.

This is exactly why the location half of the model is **fully
saturated**: it gives every distinct timeperiod x agroup combination (the
`cell_col` interaction, e.g. `interaction(Arm, Timeperiod)`) its own free,
unconstrained mean — `yi ~ 0 + cell [+ (1|plate)]`, one coefficient per
cell, with no assumed trend over time, no assumed additivity between arm
and timepoint, nothing smoothed or shared across cells (plate-to-plate
variation is handled separately, by its own optional random-intercept
term, not folded into the design cells). A saturated model is the *least
restrictive* location possible given the chosen design columns — because
it makes no assumption whatsoever about the shape of group/time
differences, it cannot itself introduce residual contamination the way a
simpler model could if its assumed shape (say, a linear time trend) turned
out to be wrong. Only what's left after subtracting each cell's own mean —
true within-cell residual, as close to pure measurement noise as the
available design can isolate — is handed to `phi`/`beta1`. Leave out a
design column that genuinely varies, and distinct cells collapse together:
real between-cell differences leak back into what the scale model treats
as noise, and the fitted weights stop purely reflecting measurement
reliability.
:::
