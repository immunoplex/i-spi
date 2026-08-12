# PLAN — pass `include_measurement_error` through curveRbayes → worker → API

One boolean flows end to end and lands identically on the grid and the sample
paths. Name it once and keep it the same at every layer:

| Layer | Form | Default |
|---|---|---|
| curveRbayes R arg | `include_measurement_error` (logical) | `TRUE` |
| worker CLI | `--include_measurement_error true|false` | `true` |
| Redis job param | `params.include_measurement_error` = `"true"|"false"` | absent ⇒ `true` |
| API request | `params: { "include_measurement_error": "true"|"false" }` | absent ⇒ `true` |
| App UI | toggle "Include assay measurement error" | on |

`TRUE` = measurement/CDAN profile (default). `FALSE` = curve-only. Rationale and
guidance: `UNDERSTANDING_precision_and_measurement_error.md`.

Layers 3 and 4 already work with **zero API/queue changes** — `app.py` passes any
`params` entry through and `supervisor.py` expands it to `--key value`. The real
edits are in the package and the worker. Everything below is additive and
backward-compatible (absent ⇒ prior default behavior for the grid; samples now
match the grid instead of floating below it).

---

## 1. curveRbayes — companion edits to `fit_calibration_bayes.R` (REQUIRED)

The refactored `predict_bayes.R` already accepts `include_measurement_error`. Wire
the entry point to accept it and pass it to **both** predict calls.

**a) Signature** — add the argument (default `TRUE`):

```r
fit_calibration_bayes <- function(standards, samples = NULL, blanks = NULL,
                                  response_var, model_names = c("logistic4","gompertz4"),
                                  ...,
                                  use_heteroscedastic_noise = FALSE,
                                  include_measurement_error = TRUE,   # <-- NEW
                                  run_loo = NULL, verbose = FALSE) {
```

**b) Grid call** — in the Section 6a per-model/per-curve loop:

```r
      ensemble_grids[[fam]][[idx]] <- predict_grid_bayes(
        base_g, bf, curve_idx = idx,
        n_draws  = n_draws_use,
        cv_x_max = cv_x_max,
        pcov_threshold = pcov_threshold,
        is_log_x = is_log_independent,
        is_log_response = is_log_response,
        include_measurement_error = include_measurement_error   # <-- NEW
      )
```

**c) Sample call** — in Section 7:

```r
        samples_out <- predict_samples_bayes(
          this_samp, best_fit, curve_idx = idx,
          response_variable = response_var,
          is_log_response   = is_log_response,
          n_draws  = n_draws_predict,
          cv_x_max = cv_x_max,
          is_log_x = is_log_independent,
          include_measurement_error = include_measurement_error   # <-- NEW
        )
```

**d) Meta** — record it for auditing (both per-plate `meta` and `multi_meta`):

```r
    include_measurement_error = include_measurement_error,
```

Rebuild/redeploy curveRbayes (the user is already doing this for the
`predict_bayes.R` refactor).

## 2. Worker — `worker_curveR.R` (REQUIRED)

**a) Default** in `parse_args()` `p <- list(...)`:

```r
            include_measurement_error = "true",
```

**b) Parse + pass** in `main()`, next to the other bayes settings. Default TRUE
unless the string is explicitly "false" (any casing):

```r
  inc_me <- !identical(tolower(trimws(P$include_measurement_error %||% "true")), "false")
```

and add to the `fit_calibration_bayes(...)` call in the `is_bayes` branch:

```r
        include_measurement_error = inc_me,
```

Frequentist ignores it (the freq path has no such argument) — parse it
unconditionally but only forward it on the bayes branch.

## 3. Supervisor — `supervisor.py` (NO CHANGE)

The `for key, value in params.items(): cmd.extend([f"--{key}", str(value)])` loop
already forwards `include_measurement_error`. A boolean sent from the app should be
serialized to the string `"true"`/`"false"` before it reaches Redis (do this in the
app or API, not the worker).

## 4. API — `app.py` (RECOMMENDED, not required)

It works today via the free-form `params` passthrough. To make it first-class,
validated, and self-documenting, add an optional typed field that normalizes into
`params` (mirroring how `cdan_cv_threshold` is merged):

```python
class JobSubmission(BaseModel):
    ...
    include_measurement_error: Optional[bool] = Field(
        None,
        description=("Bayesian only. If true (default), the precision profile and "
                     "per-sample pcov both include assay measurement error "
                     "(CDAN). If false, curve/parameter uncertainty only. "
                     "See UNDERSTANDING_precision_and_measurement_error.md."),
    )

    def model_post_init(self, __context):
        ...
        if self.include_measurement_error is not None and \
                "include_measurement_error" not in self.params:
            self.params["include_measurement_error"] = \
                "true" if self.include_measurement_error else "false"
```

This keeps the wire contract identical (worker still reads a string from
`params`) while giving clients a real boolean and Swagger documentation. Clients
that prefer may still send it inside `params` directly.

## 5. Persistence / audit

`calib_grid.noise_mode` is already written by the worker and now takes the values
`measurement_homoscedastic` | `measurement_heteroscedastic` | `curve_only`, so any
stored profile is self-describing. **Optional:** to record the mode on samples too,
add a `noise_mode text` column to `calib_samples` and one line in
`flatten_and_save.R`'s sample row (`noise_mode = .col(sm$noise_mode, n, NA_character_)`);
`predict_samples_bayes()` already emits the column. Not required — the grid's
`noise_mode` is sufficient to interpret a fit.

## 6. Rollout order

1. Merge + rebuild **curveRcore** (inflection fix, `compute_inflection.R`) and
   **curveRbayes** (`predict_bayes.R` + `fit_calibration_bayes.R` edits).
2. Rebuild the **worker** image (picks up the new packages + `worker_curveR.R`
   flag + the inflection worker patch).
3. (Optional) deploy the **API** field.
4. Ship the **app** toggle (see `HANDOFF_ispi_app_noise_mode.md`).

## 7. Smoke test

- Submit a Bayesian job with `params.include_measurement_error = "true"` and one
  with `"false"` on curve 88979.
- `true`: `diagnose_precision_gap.R` shows the bayesian `pcov - grid.pcov` mean ≈ 0
  with a non-significant x-slope (samples on the profile), matching frequentist.
- `false`: samples still lie on the (lower) profile; `calib_grid.noise_mode =
  curve_only`.
- Both: `calib_diagnostics.inflect_x` is in the standards' range, not ≈ −3.95.
- Absent flag: behaves like `true` (default), and samples now sit on the profile
  where before they floated ~9.6 %CV below it.
