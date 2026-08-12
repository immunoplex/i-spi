# Deployment Checklist — Issue 11.10 Assay-Import Refactor

Unified assay-import subsystem (ELISA, bead [raw / xPONENT / `.rbx`], flow) behind
one contract + one atomic backend + one generic module + per-assay descriptors.
Use this list to stage the change (commit / PR / release).

---

## 1. NEW files to add (15)

Core stack:
| File | Purpose |
|---|---|
| `assay_import_contract.R` | Constants, reader registry, validators, shared assembler (`assemble_upload_frames`), antigen-name canonicaliser (`.ai_clean_antigen`). |
| `assay_import_backend.R` | Atomic commit (`commit_assay_import` via `pool::poolWithTransaction`), `land_planned_visits`, `.insert_or_die` (list-column flatten + table-column trim). |
| `assay_import_module.R` | Generic 5-step Shiny module (upload → parse → download template → upload completed → validate → preview → commit). |
| `assay_import_mount.R` | `mount_assay_import()` — experiment selectize + 3-tab panel, starts the three module servers. |

Readers + descriptors:
| File | Purpose |
|---|---|
| `reader_bead.R` | Bead reader: registers `bead/raw` + `bead/xponent`; shared `.bead_*` fns (parse/template/validate/assemble); `.rbx_dil_from` / `.rbx_strip_ratio` (single home). |
| `reader_bead_rbx.R` | Registers `bead/rbx` (`.rbx`/`.srbx`); `process_rbx_files`, order-aware `.rbx_build_description`. Reuses bead's shared fns. |
| `rbx_binary_parser.R` | Pure-R Bio-Plex `.rbx` binary parser (`parse_rbx`, `long_dataframe`, `extract_tdap_*`). **User-provided.** |
| `reader_elisa.R` | Registers `elisa/xlsx`. |
| `reader_flow.R` | Registers `flow/flowjo`; per-feature experiment split + real-antigen threading. |
| `reader_bead_xponent_parsers.R` | xPONENT parsers (lifted from retired `xPonentReader.R`). |
| `reader_elisa_parsers.R` | ELISA parsers/validators (lifted from retired `elisa_reader.R`). |
| `import_helpers.R` | `extract_plate_number` (lifted from retired `import_lumifile.R`). |
| `descriptor_bead.R` | Bead descriptor + restored controls (wells, optional elements, drag-to-order sample & B/S/C elements, feature). |
| `descriptor_elisa.R` | ELISA descriptor (same controls, no feature). |
| `descriptor_flow.R` | Flow descriptor (wells + feature). |

## 2. EDITED files (2)

- **`app.R`** — inside the post-auth core-logic `observe`, add the source block
  (see §5) and the `mount_assay_import(...)` call. Ensure the two reactiveVals
  it needs exist before the call: `reactive_df_study_exp` and
  `experiment_choices_rv`.
- **`ui_handler.R`** — remove the `source("import_lumifile.R")` call from the
  tail of `load_project()` (it re-sourced the old UI on project load and
  clobbered `readxMapData`). The study selector `readxMap_study_accession` is
  untouched.

## 3. DELETE (retire) — 6 files

`import_lumifile.R`, `xPonentReader.R`, `elisa_reader.R`, `elisa_diagnostic.R`,
`flowjo_reader.R`, `segment_reader.R`.

Confirm none are `source()`d anywhere before deleting:
```
grep -rn 'source("\(import_lumifile\|xPonentReader\|elisa_reader\|elisa_diagnostic\|flowjo_reader\|segment_reader\).R"' .
```
(Expected: no matches.)

## 4. KEPT dependencies (sourced, NOT retired)

`plate_validator_functions.R`, `generate_layout_template_ref.R`,
`batch_layout_functions.R`, `curve_lookup_functions.R`, `derived_experiments.R`,
`flowjo_read_functions.R`, `generate_flowjo_layout_template.R`, plus the
already-present `db_functions.R`, `data_dictionary.R`, `assay_response.R`.
`db_pool` (global.R) is the shared connection pool used by the commit.

## 5. Source order (app.R, inside the authenticated core-logic observe)

Order matters: dependencies and lifted parsers first, then contract → backend →
module, then readers (bead before its `.rbx` extension), then descriptors, then
mount. **`reader_bead.R` MUST precede `reader_bead_rbx.R`** (the `.rbx` reader
reuses bead's shared functions and ratio helpers).

```r
source("plate_validator_functions.R",     local = TRUE)
source("generate_layout_template_ref.R",  local = TRUE)
source("batch_layout_functions.R",        local = TRUE)
source("curve_lookup_functions.R",        local = TRUE)
source("derived_experiments.R",           local = TRUE)
source("import_helpers.R",                local = TRUE)
source("flowjo_read_functions.R",         local = TRUE)
source("generate_flowjo_layout_template.R", local = TRUE)
source("reader_bead_xponent_parsers.R",   local = TRUE)
source("reader_elisa_parsers.R",          local = TRUE)
source("assay_import_contract.R",         local = TRUE)
source("assay_import_backend.R",          local = TRUE)
source("assay_import_module.R",           local = TRUE)
source("reader_bead.R",                   local = TRUE)
source("rbx_binary_parser.R",             local = TRUE)   # before reader_bead_rbx.R
source("reader_bead_rbx.R",               local = TRUE)
source("reader_elisa.R",                  local = TRUE)
source("reader_flow.R",                   local = TRUE)
source("descriptor_bead.R",               local = TRUE)
source("descriptor_elisa.R",              local = TRUE)
source("descriptor_flow.R",               local = TRUE)
source("assay_import_mount.R",            local = TRUE)

mount_assay_import(
  input = input, output = output, session = session,
  pool  = db_pool,
  scope = reactive(list(
    project_id = userWorkSpaceID(),
    study      = input$readxMap_study_accession,
    experiment = input$readxMap_experiment_accession_import,
    user       = currentuser())),
  study_exp             = reactive_df_study_exp,
  experiment_choices_rv = experiment_choices_rv)
```

## 6. Required packages (already in global.R)

`pool`, `RPostgres`, `DBI`, `readxl`, `openxlsx`, `dplyr`/`tidyr`,
`shinyjqui` (drag-to-order), `shinyWidgets` (checkbox buttons). No new package
dependencies were introduced.

## 7. Post-deploy smoke test (per assay)

For each of ELISA, Bead (raw/xPONENT/`.rbx`), Flow: pick a study + experiment,
upload instrument file(s), **Parse**, **Download** the template, re-upload it,
confirm **Validation** is clean (or only expected warnings), **Preview** row
counts look right, then **Commit** and confirm the success toast with counts.
Watch the console for the `[commit]` breadcrumbs (planned visits → header →
samples → standards → register_curve_lookup → blanks/controls → antigen family →
done). For `.rbx`, confirm `xmap_standard.dilution` shows the real serial
dilutions and the console shows the antigen/dilution parse lines.

## 8. Rollback

The subsystem is additive and self-contained: reverting the `app.R` source block
+ `mount_assay_import` call, restoring the `ui_handler.R` line, and restoring the
six retired files returns to the prior behavior. No schema or data migration is
involved (commits use existing `madi_results.xmap_*` tables).
