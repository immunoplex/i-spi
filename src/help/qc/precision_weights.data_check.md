---
id: qc.precision_weights.data_check
title: Check the data (before computing weights)
audience: user
category: compute-decision
see_also: [qc.precision_weights.model_choice, qc.precision_weights.additive_options, glossary.identifiability, glossary.censoring, glossary.tau]
references:
  - text: "curveRweights vignette \"Measurement error in assay data\": censoring, repeated measures, identifiability"
    url: "https://immunoplex.github.io/curveRweights/articles/measurement-error.html#censoring"
---

Before anything is submitted, I-SPI reads the calibrated samples of your
selection (method, experiment, and antigen if you chose one) and lists the
facts that decide which model and options make sense. A tick means the data
support the corresponding option; `!` means something limits it.

* **Samples**: how many calibrated samples, in how many curve groups. Each curve
  group is fitted separately. The run-time estimate above Submit uses this
  count: at the default 4000 iterations x 4 chains, about 40 seconds per group
  for the multiplicative model, 50 seconds for the additive model, and 2.5
  minutes when out-of-range samples are censored.
* **People**: how many people, and how many time points each has. Several
  samples per person turn on the person term (see
  [[qc.precision_weights.additive_options|additive options]]).
* **Out of range**: samples below or above the standard curve. When censoring
  was recorded at calibration they can be censored in the additive model.
  *Not recorded* means the experiment was calibrated before I-SPI stored
  censoring: those samples then count as measured values with a large SE.
  Recalibrating the experiment records it.
* **Readings per sample**: more than one when a sample (a person at a time
  point) was read at several dilutions or on several plates. Those readings
  are counted as separate samples, which overstates the information they carry
  (see [[qc.precision_weights.additive_options|additive options]]).
* **SE spread**: for each curve group, the ratio of the 90th to the 10th
  percentile of the quantified samples' `se`, with the number of quantified
  samples. The additive model can tell real variation from measurement error
  only because the `se` differs between samples (see
  [[glossary.identifiability|identifiability]]). Narrow groups (below about 2)
  and groups with few quantified samples (under about 100) are where kappa is
  most often weakly identified; when most groups are narrow, fixing kappa at 1
  is proposed. This is a rough guide: in one experiment kappa was weakly
  identified in half the groups with a spread below 3 and in a quarter of
  those above it. Each fit says whether kappa was identified, in the *check*
  column of the fit table, and that is the test to trust.
* **Stored weights**: what a new run would replace.

::: more
The recommendation is a sentence, never an automatic switch: the model stays
on Multiplicative until you choose otherwise. The defaults of the additive
options follow these facts each time you change the selection; an option the
data cannot support is greyed out, with the reason shown under the options.
:::
