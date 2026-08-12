# curve_id-batch DB confirmation

| check | status | detail |
|---|---|---|
| mgid_no_nulls | PASS | 26810/26810 populated |
| group_count | PASS | 8605 distinct multiplate groups over 26810 curves |
| group_key_pure | PASS | 0 groups span >1 non-plate NK (must be 0) |
| mgid_matches_function | PASS | 0 rows where stored id != function(cols) |
| unmasked_has_mgid | PASS | fetch_curve_batch selects it from this view |
| unmasked_has_curve_id | PASS |  |
| standard_for_fit_exists | PASS |  |
| standard_for_fit_columns | PASS | missing:  |
| blank_for_fit_exists | PASS |  |
| blank_for_fit_columns | PASS | missing:  |
| sample_for_fit_exists | PASS |  |
| sample_for_fit_columns | PASS | missing:  |
| standards_1to1 | PASS | std rows=60 distinct(curve,well,dil)=60 (must be equal) |
| blanks_present | PASS | 22 blank_for_fit rows for the batch |
| multisource_groups_exist | PASS | 848 antigen/feature/nominal contexts have >1 standard source (info) |
| app_vs_view_curveset | PASS | app(unmasked)=96 curves, standard_for_fit=96 curves, symdiff=0 |
