# calib_* parity report

- batch: 9057,9089,9105,9137,9153,9185
- n_curves: 6  method: bayesian  tol: 0
- reference: rds (old_snap)
- current: db (madi_results)
- result: **PASS**

| table | ref rows | cur rows | ref-only | cur-only | mismatch | ok | note |
|---|---|---|---|---|---|---|---|
| calib_fit | 24 | 24 | 0 | 0 | 0 | TRUE | identical |
| calib_param | 108 | 108 | 0 | 0 | 0 | TRUE | identical |
| calib_gate | 48 | 48 | 0 | 0 | 0 | TRUE | identical |
| calib_loo | 24 | 24 | 0 | 0 | 0 | TRUE | identical |
| calib_grid | 1200 | 1200 | 0 | 0 | 0 | TRUE | identical |
| calib_samples | 231 | 231 | 0 | 0 | 0 | TRUE | identical |
| calib_diagnostics | 6 | 6 | 0 | 0 | 0 | TRUE | identical |
| calib_standards | 60 | 60 | 0 | 0 | 0 | TRUE | identical |
| calib_blanks | 22 | 22 | 0 | 0 | 0 | TRUE | identical |

## Notes

- `job_id` is excluded from comparison (differs per run); `calib_run` is skipped (job-level provenance).
- Numeric columns compared within `tol`; set `PARITY_TOL` > 0 for bayesian runs if RNG-level jitter is expected.
- `Inf`/`-Inf`/`NaN` are matched exactly (e.g. `-Inf == -Inf` is equal); the tolerance applies only to finite pairs.
- `ref-only`/`cur-only` = rows present on one side only (keyed by the table's natural key).
- On any divergence, per-cell detail is written to `REPORT_calib_parity_diffs.csv` (columns: table, curve_id, locator, column, ref, cur, kind).
