---
id: qc.linearity.sets
title: Replicate sets and pairs
audience: user
category: procedural
see_also: [qc.linearity.overview, qc.linearity.error_model, glossary.errors_in_both_axes, glossary.control_source]
---
**A set** is every estimate of the same sample: the same antigen, feature,
patient and timeperiod. (If a sample has no patient id, its specimen id stands
in. A control's set is its name, e.g. QC1Orange, whatever its control source.)
A single set can hold many estimates: two wells, two dilutions, two plates,
each estimated by two methods.

**The cards** count sets by how their estimates differ. A set can fall in more
than one card.

| Card | What differs inside the set |
|---|---|
| Duplicates | Same plate, same dilution, different wells |
| Dilutions on the same plate | Same plate, different dilutions |
| Plates at the same dilution | Different plates, same dilution |
| Plates and dilutions both differ | Different plates and different dilutions |
| Frequentist and Bayesian | The same well, estimated by both fitting methods |
| More than one curve source | The same well, estimated from more than one standard-curve source |
| More than one control source | The same control from more than one control source (e.g. two suppliers) |

Duplicates, dilutions and plates are counted within one control source: the same
QC from two suppliers is a control-source comparison, not a replicate.

Clicking a card fills in a sensible comparison for that kind.

**A pair** is one estimate on the x axis matched with one estimate on the y
axis, both from the same set. Each pair is one point on the plot.

**Choosing the two axes.** Each axis has its own filters (method, curve source,
wavelength, plate, dilution, and *control source* when the controls have one).
The **curve source** is the standard's source; the **control source** is the
control's own Source — see [[glossary.control_source|control source]]. Leave a filter empty to allow anything. For
example, for *Dilution vs dilution*, x is the lower dilution and y the higher.

**Same / different rules.** Two small boxes decide which estimates may pair up.

- *Pair two points only when these are the same.* Ticking **plate** means an x
  and a y are paired only if they come from the same plate. Ticking **same
  well** means they must be the same physical well (used to compare methods,
  because both methods estimate the same wells).
- *…and these are different.* Ticking **dilution** means the two must be at
  different dilutions; ticking **well** (used for duplicates) means they must be
  different wells, so a well is never paired with itself.

Every comparison except *Control source vs control source* ticks **control
source** under "the same", so one supplier's control is never paired with
another's as a plate or dilution replicate. *Control source vs control source*
pairs the same control at the same dilution (same method, curve source and
wavelength) from two control sources; pick the two sources on the axes.

**CV% in the pairs table.** `pair_cv_pct` is the CV of the pair's two
concentrations. `x_rep_n` / `x_rep_cv_pct` (and the y ones) count the replicate
wells of that point — same set, plate, dilution, method, curve source,
wavelength and control source — and give their CV. Both are the observed
spread, 100 × SD / mean, not [[glossary.pcov|pcov]] (the curve's own estimate).

The comparison buttons set these boxes for you. Change them only for an unusual
question. The counter under the boxes says how many pairs and sets result; if it
says no pairs match, loosen a rule.

**Antigen, feature, timeperiod, patient.** These filters narrow which samples
are included. Concentrations of different antigens are not comparable with each
other, so compare one antigen at a time unless you have a reason not to.
