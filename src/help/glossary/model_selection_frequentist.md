---
id: glossary.model_selection_frequentist
title: Model selection (frequentist)
audience: both
category: glossary
see_also: [glossary.aic, glossary.frequentist_approach, glossary.model_selection_bayesian, data.results.fit_model_selection, schema.calib_fit, compute.standard_curve.model_engine]
references:
  - text: "curveRfreq::select_best_aic() — picks the lowest-AIC model among converged, eligible candidates; curveRcore::select_best_eligible() — the shared eligibility-gate step both engines run before ranking."
---
The [[glossary.frequentist_approach|frequentist]] engine fits every
candidate model shape (4PL, 5PL, Gompertz) independently, then picks a
winner by **lowest [[glossary.aic|AIC]]** among the candidates that both
converged and passed the eligibility gates (identifiability, dynamic
range, etc. — see [[data.results.fit_model_selection|the Fit / model
selection table]]). A model can converge numerically and still be excluded
from ranking if it fails a gate.

The output includes each candidate's AIC and its `delta_aic` — how far
behind the winner it trailed — so a close second place (small `delta_aic`)
is visible, not just hidden by the fact that a single model was ultimately
kept.

::: more
If every candidate for a curve fails eligibility, selection falls back to
the widest-dynamic-range candidate and flags it rather than returning
nothing — treat a fallback-selected curve with more caution than a normal
one.
:::
