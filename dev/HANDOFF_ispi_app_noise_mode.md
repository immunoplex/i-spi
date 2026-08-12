# HANDOFF — i-spi app: expose the "measurement error" toggle for Bayesian fits

**Audience:** the thread holding the **i-spi Shiny app** (the coverage panel /
job-submission path that POSTs to the i-spi-compute API).

**What changed upstream:** the Bayesian engine now computes its precision profile
and its per-sample pcov under **one shared variance definition**, selected by a new
job parameter. This fixes the long-standing issue where Bayesian sample points
floated below the precision profile instead of lying on it. The engine defaults to
the same (measurement-inclusive) definition it always used, so **doing nothing is
safe** — but you should expose the choice, because for sparse-standard plates the
measurement-error term is only crudely estimated and some analysts will want to
turn it off. Background and help-text source:
`UNDERSTANDING_precision_and_measurement_error.md`.

This is **client-side only** and needs **no server rebuild** — the API already
accepts the parameter through its `params` passthrough.

---

## 1. The parameter

Send inside the existing `params` object on `POST /jobs`, **Bayesian jobs only**:

```json
{
  "curve_ids": [9057, 9058],
  "script_type": "bayesian",
  "params": {
    "include_measurement_error": "true",
    "sampling": "1500"
  }
}
```

- **`include_measurement_error`** — string `"true"` or `"false"` (the worker parses
  the string). Omit it and the engine uses `"true"`.
  - `"true"` (default) → **measurement precision** (a.k.a. CDAN): the profile
    reflects how precisely a concentration can be recovered from a single noisy
    reading. This is the classical immunoassay precision profile and what LLOQ/ULOQ
    are meant to be read against.
  - `"false"` → **curve-only precision**: calibration-curve (posterior) uncertainty
    only, with no assay-noise term. A more conservative/honest choice when a plate
    has too few standards/controls to estimate measurement error well.
- Ignore it for `frequentist` jobs (the frequentist engine doesn't take it).

It lives alongside the `sampling`/precision-resolution control from the earlier
handoff — both are just entries in `params`, so the same request-builder handles
them.

> Once the optional API field ships (see `PLAN_noise_mode_api_worker.md` §4) you may
> instead send a real boolean top-level field `include_measurement_error`. Until
> then, use the `params` string form above.

## 2. UI

Add a control to the Bayesian fit-submission panel (show it only when method =
Bayesian):

- **Toggle:** "Include assay measurement error" — **default ON**.
- **Help / tooltip:** "On (recommended): the precision profile reflects the assay's
  measurement noise, so LLOQ/ULOQ describe real single-reading precision. Off: shows
  calibration-curve uncertainty only — useful when a plate has few standards or
  controls, where the measurement-error estimate is unreliable. See the precision
  documentation."
- Map the toggle to `params.include_measurement_error = "true"|"false"` in the
  request-builder.

**Optional nicety (recommended):** when the resolved batch has few standard points
or no replication, surface a non-blocking hint next to the toggle — e.g. "This
plate has few standards; the measurement-error estimate may be crude. Consider
comparing both settings." This operationalizes the guidance in the understanding
doc without forcing a choice.

## 3. What the analyst will see

- **ON:** the Bayesian precision profile passes through the sample cloud (as the
  frequentist plot already does). Previously the samples sat ~9.6 %CV below the
  curve; that gap is now closed by definition.
- **OFF:** samples still lie on the profile, but the whole profile sits lower
  (no measurement-noise term) and the usable range will look wider — read with the
  caveat that this omits assay noise.
- **Either way:** the inflection marker (Curve tab) now sits mid-curve. A separate
  server-side fix corrected a bug that placed the Bayesian inflection off the left
  edge (~log10 conc −4); no app change is needed for that, but you'll see the marker
  move into range on re-fit.

## 4. Acceptance check

1. Toggling the control changes `params.include_measurement_error` in the outgoing
   request (inspect the JSON / API logs).
2. A Bayesian re-fit with the toggle ON shows sample points on the precision
   profile; with it OFF, samples lie on a lower profile.
3. The persisted mode is visible in `calib_grid.noise_mode`
   (`measurement_*` vs `curve_only`) if you surface it anywhere.
4. Default (toggle ON / param absent) reproduces the prior profile shape but with
   the sample cloud now sitting on it.
