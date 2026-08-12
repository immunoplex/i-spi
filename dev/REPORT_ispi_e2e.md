# i-spi-compute end-to-end report

- batch: 88976,88977,88978,88979,88980,88981,88982,88983,88984,88985
- n_curves: 10  n_groups(expected): 2  script/method: frequentist
- covers: study=ELISA_atr exp=FcgR2a_exp antigen=FHA feature=FcgR2a source=sandoglobuline
- job_id: 49f97fc9-c5cd-43c4-84d1-010c8dfa356b  final status: completed

| step | status | detail |
|---|---|---|
| resolve_batch | INFO | n_curves=10 n_groups=2 (sending multiplate_group_ids) |
| health | PASS | http 200 |
| submit | PASS | job_id=49f97fc9-c5cd-43c4-84d1-010c8dfa356b n_curves=10 script=frequentist |
| job_terminal | PASS | status=completed |
| registered_curves | INFO | 10/10 submitted curve_ids present in curve_lookup |
| calib_fit | PASS | 10/10 curves fit |
| calib_grid | PASS | 10/10 curves gridded |
| calib_standards | PASS | 10 curves, 200 rows |
| calib_blanks | PASS | 10 curves, 40 rows |
| diagnosis | INFO | OK: fit + standards + blanks all persisted for the batch. Compare calib_* against the last green run for a byte-identical check, then wire the app overlay. |

## Diagnosis

OK: fit + standards + blanks all persisted for the batch. Compare calib_* against the last green run for a byte-identical check, then wire the app overlay.
