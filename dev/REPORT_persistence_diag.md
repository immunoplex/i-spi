# calib_standards persistence diagnostic

- job_id: c5367cac-91f3-48a0-a724-2efe41d76c3c
- worker role: d78039e

| check | status | detail |
|---|---|---|
| worker_role | INFO | d78039e |
| insert_grant_standards | PASS | INSERT on calib_standards |
| insert_grant_blanks | PASS | INSERT on calib_blanks |
| insert_grant_fit | PASS | INSERT on calib_fit (baseline that DID persist) |
| insert_probe_standards | PASS | a well-formed row inserts (rolled back) |
| standards_notnull_cols | INFO | NOT NULL w/o default: curve_id, method, well, dilution, included |
| fit_rows_this_job | PASS | 24 calib_fit rows tagged with this job_id |
| standards_rows_this_job | FAIL | 0 calib_standards rows for this job |
| blanks_rows_this_job | FAIL | 0 calib_blanks rows for this job |
