# standards/blanks save-path trace

- curve_id: 9057

| link | status | detail |
|---|---|---|
| worker_fetch_standards | PASS | 20 rows from standard_unmasked |
| worker_fetch_blanks | PASS | 6 rows from blank_unmasked |
| L1_preprocess_returns_standards | PASS | data.frame 20x27 [xmap_standard_id,study_accession,experiment_accession,plate_id,well,stype,sampleid,source,dilution,pctaggbeads,samplingerrors,antigen] |
| L1_preprocess_returns_blanks | PASS | data.frame 6x25 [xmap_buffer_id,study_accession,experiment_accession,plate_id,well,stype,pctaggbeads,samplingerrors,antigen,antibody_mfi,antibody_n,antibody_name] |
| L2_flatten_carries_frames | MANUAL | flatten_result not loaded. Load the worker package (library(<worker_pkg>)) and re-run, or CHECK its body: does it add calib_standards/calib_blanks (from pp$data/$blanks) to the flattened bundle? |
| L3_save_inserts | MANUAL | save_calib not loaded. Load the worker package and re-run, or CHECK its body for INSERT branches on calib_standards/calib_blanks, reached unconditionally and not inside an error-swallowing tryCatch. |
