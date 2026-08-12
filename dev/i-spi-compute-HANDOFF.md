# i-spi-compute — Handoff Summary (for the i-spi Shiny refactor thread)

Paste this whole file into a new thread. It is the context for refactoring the
**i-spi R Shiny app** to consume the newly-built `i-spi-compute` compute tier.
Everything below is already built, deployed-in-progress, and validated end to
end; the Shiny refactor is the remaining work.

---

## 1. What exists now (the compute tier — DONE)

`i-spi-compute` is a queue-worker system that fits immunoassay **calibration /
standard curves** (both **Bayesian** and **frequentist**) outside the Shiny app
and writes results to PostgreSQL. It replaced a legacy `stanassay`-based worker.

Data flow:
```
i-spi (Shiny)  →  i-spi-compute-api (FastAPI)  →  Redis queue  →  i-spi-compute-worker
                                                                      │ (Python supervisor
                                                                      │  spawns Rscript)
                                                                      ▼
                                                          worker_curveR.R  (curveR engine)
                                                                      │
                                                                      ▼
                                                     PostgreSQL  madi_results.calib_*
```

- **Engine:** the **curveR** R packages — `curveRcore` (contract/math/preprocess/
  eligibility/detection limits), `curveRfreq` (NLS, AIC selection), `curveRbayes`
  (Stan via **cmdstanr/CmdStan**, LOO selection). Both engines return the same S3
  `calibration_result_multiplate` object, so downstream code is method-agnostic.
- **Results:** written to new method-agnostic **`madi_results.calib_*`** tables. A
  `method` column ('bayesian' | 'frequentist') distinguishes engines. Writes are
  idempotent: delete-by-`(curve_id, method)` then insert.
- **Status:** builds, runs in Docker locally, and a real **frequentist** job
  completed end-to-end (`6/6 curves saved` to the live DB). Bayesian path builds
  with precompiled Stan models; cluster deploy is in progress.

## 2. The database contract (this is what the Shiny app must read)

**The refactor is fundamentally: change i-spi's read side from the legacy
`bayes_*` tables to the new `calib_*` tables.**

### Legacy tables (what i-spi reads today — TO BE REPLACED)
`madi_results.bayes_*`, notably:
- `bayes_curves` — one row per curve; best model in `plate_best_family`
  (values like `4pl`/`5pl`/`gompertz`); `lloq`,`uloq` (**RAW concentration
  scale**), `lod`,`uod`,`lrdl`,`urdl`, `inflect_x`/`inflect_y`(+CI), per-family
  `plate_elpd_*`.
- `bayes_curve_grid` — plotting grid: `concentration`, `log10_conc`, `mfi_median`.
- `bayes_samples` — back-calc per sample: `sampleid`,`patientid`,`timeperiod`,
  `well`,`dilution`, `raw_predicted_concentration`, `se_concentration`, `pcov`,
  `conc_lower`/`conc_upper`.

### New tables (what i-spi must read AFTER refactor)
`madi_results.calib_*` (all keyed on `curve_id` + `method`):
- `calib_run` — one row per job (`job_id` PK, run metadata).
- `calib_fit` — one row per (`curve_id`,`method`,`model_name`); `is_best` flag,
  `selection_score`, `score_type` ('loo_elpd' for bayes, 'aic' for freq),
  `selection_weight`, `converged`, `criterion`. **Best model = the `is_best` row's
  `model_name`** (values `logistic4`/`logistic5`/`gompertz4`, NOT `4pl`/`5pl`).
- `calib_param` — per-term parameters (`term`,`estimate`,`std_error`,`q_lo`,`q_med`,`q_hi`).
- `calib_gate` — eligibility gates per model.
- `calib_grid` — plotting grid: `log10_concentration`, `concentration`,
  `predicted_response`, `ci_lower`/`ci_upper`, `predicted_concentration`,
  `se_concentration`, `pcov`, `pcov_rmse`, `pcov_pass`, `d2y_dx2` (~200 pts).
- `calib_samples` — back-calc per sample (identity = `sampleid`,`patientid`,
  `timeperiod`,`dilution`; missing identity stored as sentinel `'__none__'`);
  `predicted_concentration`, `final_concentration` (× dilution), `se_concentration`,
  `pcov`, `pcov_rmse`, `pcov_pass`.
- `calib_diagnostics` — one row per (`curve_id`,`method`): `lloq_log10`/`uloq_log10`
  **AND** `lloq_conc`/`uloq_conc` (both scales stored), `shape_*`, `inflect_*`,
  detection limits.
- `calib_loo` — **bayesian only**: per-model LOO comparison (`elpd_loo`,
  `se_elpd_loo`,`p_loo`,`looic`,`elpd_diff`,`se_diff`,`weight`; `pareto_k_*` may be NULL).

### CRITICAL differences the Shiny read-side must handle (found via a parity check)
1. **Model-family names differ:** old `4pl/5pl/gompertz` ↔ new
   `logistic4/logistic5/gompertz4`. Any UI label/logic keyed on family must map.
2. **LLOQ/ULOQ scale:** old `bayes_curves.lloq/uloq` are **raw concentration**;
   new `calib_diagnostics` has BOTH `*_log10` and `*_conc` — read `*_conc` for a
   like-for-like value, `*_log10` if the plot axis is log10.
3. **Model selection may differ between engines** (old picked 5pl, new frequentist
   picked gompertz on the same data) — expected, not a bug; don't hard-code family.
4. **`curve_id` join:** both old and new reference `madi_results.curve_lookup`
   (load-time, read-only, one row per curve keyed on a 10-col natural key). Prefer
   joining old↔new on the NK, not on `curve_id` (legacy ids can drift).
5. **`calib_loo` is empty for frequentist** by design (freq uses AIC in
   `calib_fit`, not LOO).

## 3. How i-spi talks to the compute tier (the API)

i-spi currently points at the **old** batch API; the refactor repoints it at
`i-spi-compute`.

- **Base URL:** `https://<host>/i-spi-compute/` (Traefik ingress + strip-prefix
  middleware; API `ROOT_PATH=/i-spi-compute`). Old app used `/batch-api`.
- **Auth:** `X-API-Key` header. New key lives in sealed secret `i-spi-compute`
  key `API_KEY` (separate from the old `immunoplex-batch-cal/API_KEY`).
- **Submit:** `POST /jobs` with JSON:
  `{project_id, study, experiment, antigen, source, scope ('study'|'experiment'|
  'antigen'), script_type ('bayesian'|'frequentist'), params {}, cdan_cv_threshold}`.
  `script_type` selects the engine. `params` is a passthrough → CLI flags
  (e.g. `{"models":"logistic4,gompertz4","chains":"4"}`).
- **Poll:** `GET /jobs/{job_id}` → `status` (queued→running→completed/failed/
  cancelled), `percentage`, `eta_display`, `current_experiment`, `error`,
  `output_path` (`madi_results.calib_* (job_id=…)`).
- **List / cancel:** `GET /jobs?study=&status=`, `DELETE /jobs/{job_id}`.
- **Health:** `GET /health` (no auth).

So in the Shiny app: the **submit + poll** logic changes only in URL + key +
`script_type` (frequentist is now a first-class option); the **results display**
logic is the big change — it must read `calib_*` instead of `bayes_*`.

## 4. Repo / deployment shape (for reference)

Repo `i-spi-compute` (namespace `madi-preprod`, registry `ghcr.io/immunoplex`):
```
api/    app.py, api.Dockerfile, requirements.txt
worker/ worker_curveR.R, flatten_and_save.R, verify_saved.R,   # 3 co-located siblings
        supervisor.py, entrypoint.sh, worker.Dockerfile, requirements.txt
db/     calib_schema_v1.sql            # the calib_* migration (already applied)
docker-compose.yml, i-spi-compute.k8s.yaml
DEPLOYMENT.md, SECRETS.md, README.md, .env.example
```
- Secrets via **Sealed Secrets** (`kubeseal`): new secret `i-spi-compute`
  (`API_KEY`,`REDIS_AUTH`); DB password reused from existing
  `madi-lumi-reader/db_pwd_x` (DB user `d78039e`, host `mlr-c3d7-db.c.dartmouth.edu`,
  db `postgres`, `sslmode=require`).
- Worker CPU cap via `WORKER_CORES` (== container CPU limit; also caps Stan
  `parallel_chains`); worker pinned to `amd64` (Stan binaries).
- Runs **parallel** to the still-live production `immunoplex-batch-cal-*` stack;
  they must not collide (separate Redis, separate secret).

## 5. A NEW feature already spec'd but NOT yet built (relevant to Shiny)

**Masking (point/plate exclusion).** Lab needs to exclude ("mask") individual
calibration points or whole plates while **keeping** the data (flagged, not
deleted). Agreed design: add mask flag column(s) to the input schema; implement
exclusion as **PostgreSQL views** (single source of truth); the worker reads the
views (one-line `fetch_*` change) plus a viability guard (skip curves with too
few points after masking) and an audit record. The **Shiny app will need UI** to
set/clear masks and to show "fit on N of M points; k masked (reason)". This is
upcoming work that touches both the DB and the Shiny app.

## 6. What the new thread should focus on

Refactoring the **i-spi R Shiny app**:
- Repoint API calls: base URL `/i-spi-compute/`, new `API_KEY`, expose
  `script_type` (bayesian **and** frequentist) in the submit UI.
- Rewrite the results/plots read layer from `bayes_*` → `calib_*`, handling the
  five critical differences in §2 (family-name mapping, LLOQ/ULOQ scale, engine
  selection variance, NK-based joins, freq-has-no-LOO).
- (Later) masking UI per §5.

**To start, share:** the i-spi Shiny code that (a) submits/polls batch jobs and
(b) queries `bayes_*` and renders curves/samples/LOQs — that's the surface area
of the refactor.

---
*Everything in §1–§4 is built and validated. §5 is designed but unbuilt. §6 is
the new thread's job.*
