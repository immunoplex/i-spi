---
id: compute.standard_curve.model_engine
title: Standard-curve model selection & fit engine
audience: user
category: compute-decision
see_also: [compute.standard_curve.bayes_draws, settings.precision_measurement_error, glossary.model_forms, glossary.frequentist_approach, glossary.bayesian_approach, glossary.model_selection_frequentist, glossary.model_selection_bayesian]
references:
  - text: "curveRfreq frequentist-quickstart.Rmd — multi-start NLS ensemble and AIC selection"
    url: "https://immunoplex.github.io/curveRfreq/articles/frequentist-quickstart.html"
  - text: "curveRbayes bayesian-quickstart.Rmd — hierarchical Stan model and LOO-CV selection"
    url: "https://immunoplex.github.io/curveRbayes/articles/bayesian-quickstart.html"
  - text: "curveRcore model-forms.Rmd — the five forward-model shapes shared by both engines"
    url: "https://immunoplex.github.io/curveRcore/articles/model-forms.html"
---

Two things are chosen here: which [[glossary.model_forms|curve shapes]] are candidates
("Models to fit"), and which fitting approach evaluates them ("Fit engine"). The two
engines are genuinely different statistical methods, not just different settings of
the same algorithm — **Frequentist** (the default) fits each candidate shape with
classical nonlinear least squares, trying several starting points and keeping the
best-converged fit; **Bayesian** fits a hierarchical model in Stan, sampling the full
posterior distribution rather than a single best-fit curve.

Within whichever engine is chosen, the "best" model among your selected shapes is
picked automatically: candidates first pass an eligibility check, then the winner is
chosen by AIC (Frequentist) or leave-one-out cross-validation, LOO-CV (Bayesian) — the
Bayesian equivalent of AIC. Switching to Bayesian also reveals two extra controls
further down (sampling resolution and whether to include assay measurement error in
the precision profile) that only apply to that engine and stay hidden under
Frequentist.

::: more
**The eligibility check** rejects a fitted candidate before it's allowed to compete on
AIC/LOO — checks such as whether a parameter hit the edge of its allowed search range,
whether the model's uncertainty estimate is well-conditioned, and whether its
predictions stay within a sensible dynamic range. This keeps a technically-converged
but nonsensical fit from winning just because it reports a good score.

**Choosing between the two engines** is mostly a speed-vs-depth trade-off: Frequentist
is fast and gives a single best curve with delta-method uncertainty; Bayesian is slower
(it runs MCMC sampling — see [[compute.standard_curve.bayes_draws|sampling
resolution]]) but returns a full posterior, which is what lets the precision profile
reflect uncertainty more flexibly, including the assay-measurement-error toggle
available only there.
:::
