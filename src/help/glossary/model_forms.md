---
id: glossary.model_forms
title: Model forms (4PL, 5PL, log-logistic, Gompertz)
audience: user
category: glossary
see_also: [compute.standard_curve.model_engine, glossary.frequentist_approach, glossary.bayesian_approach, glossary.inflection_point]
references:
  - text: "curveRcore model-forms.Rmd"
    url: "https://immunoplex.github.io/curveRcore/articles/model-forms.html"
---

The curve-fitting packages offer five sigmoidal (S-shaped) dose-response shapes a
standard curve can be fit to: two symmetric **logistic** forms (4- and 5-parameter,
often called 4PL/5PL), two **log-logistic** forms (4- and 5-parameter — the
5-parameter version is also called "Richards" in some literature), and a
**Gompertz** form, which is asymmetric and often a good fit for bead-based multiplex
assays. All describe the same basic rise-then-plateau response; they differ in how
much flexibility they allow for asymmetry around the curve's inflection point.
