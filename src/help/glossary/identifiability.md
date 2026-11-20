---
id: glossary.identifiability
title: Identifiability (of tau and kappa)
audience: user
category: glossary
topic: Precision and uncertainty
see_also: [glossary.tau, glossary.kappa, qc.precision_weights.data_check]
references:
  - text: "curveRweights vignette \"Measurement error in assay data\": how good is tau?"
    url: "https://immunoplex.github.io/curveRweights/articles/measurement-error.html#identifiability"
---

A quantity is **identifiable** when the data contain enough information to
estimate it. The additive weights model separates real variation
([[glossary.tau|tau]]) from measurement error ([[glossary.kappa|kappa]]) only
because samples differ in their `se`: where `se` is large the spread is mostly
measurement, where it is small mostly real. If all samples have nearly the same
`se`, the two cannot be told apart.

::: more
Each additive fit reports either "kappa identified from the data" or "kappa is
weakly identified", when the estimate of kappa barely moves away from its
prior. Treat tau and the reliabilities of a weakly identified group as
provisional, or rerun with kappa fixed at 1. The **SE spread** in Check the
data predicts this before you run: below about 2, consider fixing kappa.
:::
