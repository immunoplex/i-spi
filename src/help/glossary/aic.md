---
id: glossary.aic
title: AIC (Akaike Information Criterion)
audience: both
category: glossary
see_also: [glossary.model_selection_frequentist, glossary.loo, schema.calib_fit, data.results.fit_model_selection]
references:
  - text: "Akaike H (1974). A new look at the statistical model identification. IEEE Transactions on Automatic Control 19(6):716-723."
    doi: "10.1109/TAC.1974.1100705"
---
**AIC** scores how well a fitted model balances explaining the data against
its own complexity: it penalizes a model's log-likelihood by twice its
number of estimated parameters, so a model that fits only marginally better
by virtue of having more free parameters doesn't automatically win.
**Lower AIC is better.** It's the ranking criterion
[[glossary.model_selection_frequentist|frequentist model selection]] uses
to pick among candidate curve shapes.

`delta_aic` (a candidate's AIC minus the winner's) shows how close a
runner-up was — a small gap means the choice between two model shapes was
not decisive on this plate's data, even though only one was kept.

::: more
AIC approximates a model's expected out-of-sample predictive accuracy under
large-sample/regularity assumptions, which is why it's a fast, closed-form
alternative to the explicit cross-validation [[glossary.loo|LOO]] performs —
the Bayesian engine uses LOO instead largely because those asymptotic
assumptions are less reliably met for a hierarchical model's effective
parameter count.
:::
