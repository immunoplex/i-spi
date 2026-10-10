---
id: glossary.frequentist_approach
title: Frequentist fitting approach
audience: both
category: glossary
see_also: [glossary.bayesian_approach, glossary.model_selection_frequentist, glossary.se_concentration, glossary.convergence, compute.standard_curve.model_engine, glossary.model_forms, glossary.robust_nonlinear_estimation]
references:
  - text: "curveRfreq package (fit_calibration_freq()) — nonlinear least-squares calibration ensemble with multi-start Levenberg-Marquardt optimization and AIC-based model selection."
  - text: "O'Connell MA, Belanger BA, Haaland PD (1993). Calibration and assay development using the four-parameter logistic model. Chemometrics and Intelligent Laboratory Systems 20(2):97-114."
    doi: "10.1016/0169-7439(93)80008-6"
---
The **frequentist** engine fits an ensemble of candidate nonlinear models
(4PL, 5PL, Gompertz) to a plate's standard curve by nonlinear least squares,
using several random starting points per model (multi-start
Levenberg-Marquardt) so the optimizer isn't stuck on one local minimum. For
each curve it produces **one best-fit set of parameters**, chosen by
[[glossary.aic|AIC]] among the converged, eligible candidates — see
[[glossary.model_selection_frequentist|frequentist model selection]].

Choose frequentist when you want a fast, single answer per curve: it runs in
seconds, needs no MCMC tuning, and its uncertainty ([[glossary.se_concentration|
se_concentration]]/[[glossary.pcov|pcov]]) comes from a closed-form
approximation around that one best fit rather than full sampling.

::: more
Because frequentist fitting treats every curve independently, it can't pool
information across plates/antigens the way the [[glossary.bayesian_approach|
Bayesian]] hierarchical model does — a sparse or noisy plate gets no help
from sibling plates. Its convergence diagnostics (optimizer status, Hessian
condition number, gradient norm) describe whether the numerical optimizer
found a stable optimum, not whether the model itself is a good description
of the data — see [[glossary.convergence|convergence]].
:::
