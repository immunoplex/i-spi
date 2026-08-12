# 11.10 — Assay Import Refactor: Scope & Strategy

Companion to `REFACTOR_settings_cascade.md`. This is the assessment and plan for
issue **11.10** — collapsing the three assay-import pathways (ELISA, bead array,
flow cytometry) onto one UI standard, one reader contract, and one landing
backend, then slotting the new bead `*.rbx` format in behind that contract.

Read §4 (conventions) of `REFACTOR_settings_cascade.md` first — every new
data-access function here obeys **pool-first, project-second, named-args**.

---

## 0. Ground rules for this refactor (from the request)

- **`pool` (`db_pool`) throughout.** New code takes `pool` first. Multi-table
  commits run inside `pool::poolWithTransaction(db_pool, function(conn) { ... })`
  so they are atomic *and* pool-compliant; the checked-out `conn` is handed to
  the existing DBI-generic seam functions unchanged.
- **New files, retire the old — do not edit the import set in place.** The
  replacement is a fresh set of files; the retired files are removed from the
  `app.R` source list in the final phase. (Shared libraries that non-import code
  also depends on are the exception — see §5.)
- **`*.rbx` comes last.** It is a new bead format added *after* the other three
  assays are refactored onto the contract. It must require zero UI/backend change
  — just a new reader registered in the bead format registry.

---

## 1. What the import subsystem is today (as-built map)

One monolithic import tab, three assays, five formats, no module boundary. All
import state lives as ~30 `reactiveVal`s in the **one** `app.R` server namespace
(`xponent_plate_data`, `batch_plate_data`, `layout_template_sheets`,
`flowjo_*_rv`, `batch_validation_state`, …). Every reader file is
`source(..., local = TRUE)` into that namespace and shares those globals by
lexical scope. That shared namespace *is* the coupling — there is no interface,
so a change to any reader can reach any other.

### Entry point
`ui_handler.R` → `import_tab` → `uiOutput("readxMapData")`.
`output$readxMapData` (a single `renderUI` in **`import_lumifile.R:173`**) draws
the whole tab: an assay selector (`assay_type_selector`: Bead Array / ELISA /
Post-gating Flow Cytometry), then a `conditionalPanel` per assay. Bead has a
second selector (`xPonentFile`: Raw File / xPONENT). Flow delegates to
`uiOutput("flowjo_import_ui")` (`flowjo_reader.R:49`). All input ids are global
and ad-hoc (`upload_experiment_files`, `feature_value`, `upload_layout_file`,
`upload_xponent_experiment_files`, `upload_elisa_experiment_files`, …).

### The five formats and where each lives

| assay | format | raw parser(s) | template builder | commit (LIVE) |
|-------|--------|---------------|------------------|---------------|
| Bead  | Raw .xlsx | `import_lumifile.R` raw observers + `generate_layout_template_ref.R` | `generate_layout_template` (`generate_layout_template_ref.R:343`) | inline in `observeEvent(input$upload_batch_button)` (`import_lumifile.R:2536`) |
| Bead  | xPONENT .csv | `xPonentReader.R` (`xponent_to_wide_plate_data`, `extract_xponent_header`, `extract_xponent_assay_response_long`, `process_xponent_files`) | shares bead template | shares bead commit |
| ELISA | .xlsx | `elisa_reader.R` (`process_elisa_files`, `combine_elisa_data`, plate-block parsers) | `generate_elisa_layout_template` (`elisa_reader.R:711`) | inline in ELISA upload observer (`elisa_reader.R` ~2130–2407) |
| Flow  | FlowJo .xlsx | `flowjo_reader.R` + `flowjo_read_functions.R` (`load_flowjo_file`, `pivot_flowjo_long`) | `generate_flowjo_layout_template.R` (`build_flowjo_*`) | inline in flow upload observer (`flowjo_reader.R` ~886–1090), **splits by feature** |
| Bead  | **`*.rbx`** | *not integrated yet* | — | — |

`segment_reader.R` is the legacy RAW segment UI builder and is **not sourced**
(`# source("segment_reader.R", …)` in `app.R:1112`) — effectively dead. It is in
the retire set. (One straggler call to `create_list_of_dataframes` remains in a
bead observer at `import_lumifile.R:1679`; it dies with that observer.)

### The one contract that already exists: the layout-template sheet set
All three assays already converge on an identical **layout template sheet set**
before commit: `plate_id`, `plates_map`, `antigen_list`, `subject_groups`,
`timepoint`, and a derived `assay_response_long`. The per-format code differs
only in getting *to* those sheets (parse raw → seed template) and in reading the
*completed* template back. **This sheet set is the natural contract** — the
backend should commit from it uniformly, and a "reader" is just the two functions
that produce and re-parse it.

---

## 2. The shared backend today (what to reuse, not rebuild)

Named in the issue as "shared seams already in place." All are DBI-generic
(dispatch on `conn` or a pooled handle) and already used by ≥1 assay:

| seam | file | role |
|------|------|------|
| `insert_to_table(conn, schema, table, data, label, required_cols)` | `plate_validator_functions.R:1022` | append one xmap_* table with a required-column guard |
| `insert_new_rows(conn, schema, table, new, existing, join_keys, label)` | `plate_validator_functions.R:997` | dedup-insert (antigens, visits) |
| `register_curve_lookup(conn, standards_df, project_id)` | `curve_lookup_functions.R:331` | register curves from committed standards |
| `upload_antigen_family(conn, antigen_list, project, study, experiment, user)` | `plate_validator_functions.R:1271` | `prepare_batch_antigen_family` → `insert_new_rows` → `write_antigen_settings_to_cascade` |
| `upload_planned_visits(conn, timepoint_map, study)` | `plate_validator_functions.R:1301` | dedup-insert visits |
| `write_antigen_settings_to_cascade(conn, antigen_df, project, study, experiment, user)` | `plate_validator_functions.R:1214` | 11.9a — route fit-settings to `calib_settings` |
| `prepare_batch_antigen_family(antigen_list, default_family)` | `batch_layout_functions.R:2175` | shape antigen sheet for insert — **requires `feature`** |
| `prepare_batch_header` / `prepare_planned_visits` | `batch_layout_functions.R` | shape header/visits |
| `get_existing_antigens` / `get_existing_visits` / `check_existing_plates` / `apply_column_mapping` / `standardize_date_for_postgres` | `db_functions.R` | commit helpers |

**There is already a clean, unified bead commit — `upload_batch_to_database`
(`plate_validator_functions.R:1328`) — but it is DEAD CODE.** It routes antigens
through `upload_antigen_family` (the correct path) and has the right shape. The
LIVE bead observer reimplements it inline and diverges. This dead function is the
blueprint for the shared backend; it and the three inline observers all collapse
into one routine.

---

## 3. The divergences to converge (the actual work)

The three commit paths do the *same* thing four different ways:

1. **Antigen family + cascade write.**
   - ELISA → `upload_antigen_family` (threads `feature` via
     `prepare_batch_antigen_family`; cascade write is inside it). **Correct.**
   - Bead → inline `insert_new_rows` with an `antigen_cols_needed` that **omits
     `feature`** (`import_lumifile.R:2835`), then a separate
     `write_antigen_settings_to_cascade`.
   - Flow → same inline pattern (`flowjo_reader.R:859`), per feature-experiment.

   **Pinch point:** `prepare_batch_antigen_family` hard-selects a `feature`
   column (`batch_layout_functions.R:2176`); bead's antigen list has none, which
   is *exactly why* bead inserts inline. Converging bead/flow onto
   `upload_antigen_family` means **threading `feature` into the antigen list**.
   Bead antigens span features (isotypes), so bead's `xmap_antigen_family` rows
   go from **per-antigen → per-(antigen, feature)** — a deliberate semantic/data
   change (called out in the issue). Flow already splits by feature, so it is one
   step away. We thread `feature` *in the reader* (upstream) so
   `prepare_batch_antigen_family` is used unmodified.

2. **Row insertion.** Bead + flow use `insert_to_table`; ELISA uses raw
   `DBI::dbAppendTable`. Unify on `insert_to_table` (it carries the
   required-column guard).

3. **Visits.** ELISA → `upload_planned_visits`; bead + flow → inline
   `insert_new_rows`. Unify on `upload_planned_visits`.

4. **Commit shape.** Three inline observer bodies + one dead function → one
   `commit_assay_import`.

**Non-obvious invariant kept throughout:** `register_curve_lookup` must run
**before** any cascade settings write — the antigen tier needs the full ladder
`(project/study/experiment/feature/antigen)` and `feature` comes from the
just-registered curves. Order in the backend is fixed: standards → curve_lookup →
antigens (→ cascade inside `upload_antigen_family`).

---

## 4. Target architecture

### 4.1 The reader contract (per format)
A format reader is a small object with a stable interface. Because the
layout-template sheet set is the real contract (§1), a reader is two functions
plus metadata:

```r
# registered in a format registry, keyed by (assay, format_id)
list(
  assay        = "bead",              # "bead" | "elisa" | "flow"
  format_id    = "xponent",           # unique within assay
  label        = "xPONENT (.csv)",
  accept       = c(".csv"),
  # 1) raw upload -> preview + everything needed to SEED the layout template
  parse_raw    = function(files, opts) list(
                    preview        = <wide/long for on-screen preview>,
                    plate_metadata = <header/plate_id seed>,
                    template_seed  = <named list of sheet seeds>
                 ),
  # 2) completed template -> the canonical sheet set the backend commits
  parse_layout = function(layout_file, opts) list(
                    plate_id, plates_map, antigen_list,
                    subject_groups, timepoint, assay_response_long
                 ),
  make_template = function(template_seed, opts) <xlsx path to download>
)
```

`antigen_list` from `parse_layout` **must include `feature`** (per §3.1) so the
backend can call `upload_antigen_family` unmodified.

### 4.2 The shared landing backend
One routine replaces the three inline commits and the dead
`upload_batch_to_database`:

```r
commit_assay_import <- function(pool, sheets, scope, opts = list()) {
  # scope = list(project_id, study, experiment, user)
  pool::poolWithTransaction(pool, function(conn) {
    # header -> sample -> standard -> curve_lookup -> blank -> control
    #        -> antigen_family (+cascade) -> planned_visits
    # via insert_to_table / register_curve_lookup / upload_antigen_family /
    #     upload_planned_visits — the §2 seams, unchanged.
    # Flow passes sheets already split per feature-experiment; the backend
    # loops the caller-supplied experiment set so bead(1)/elisa(1)/flow(N)
    # share one loop body.
  })
}
```

Atomicity is a real upgrade: today curve_lookup is "non-fatal, never rolls back"
and a mid-commit failure leaves partial data. Inside one transaction the whole
import lands or nothing does. (curve_lookup can stay soft-failing *within* the
txn if we want to preserve today's tolerance — decide in Phase 2.)

### 4.3 The assay-import module (one UI standard)
The issue asks for "three assay modules … each a proper Shiny module with ONE UI
standard (upload → template → validate → preview → commit)." The maintainable
realization is **one generic module** driven by an *assay descriptor*, mounted
three times (`elisa`, `bead`, `flow`). "Three modules" = three descriptors + three
mounts; the 5-step flow is written once.

```r
assay_import_ui(id)                       # the 5-step shell, namespaced
assay_import_server(id, pool, descriptor, scope_reactive)
# descriptor = list(
#   assay, label,
#   formats        = <registry entries from §4.1>,     # bead has raw/xponent/rbx
#   assay_controls = function(ns) <extra inputs, e.g. bead wells / feature>,
#   validate       = function(sheets, opts) <messages>,
#   split_experiments = function(sheets) <list of per-experiment sheet sets>  # flow=by feature; else identity
# )
```

The ~30 `app.R` import `reactiveVal`s become module-internal reactives. Nothing
import-related stays in the `app.R` server namespace except the three mounts.

### 4.4 Proposed new files (all LF, pool-first)

| new file | replaces / absorbs |
|----------|--------------------|
| `assay_import_contract.R` | the (implicit) reader contract + a format **registry** |
| `assay_import_backend.R` | the three inline commits + dead `upload_batch_to_database` |
| `assay_import_module.R` | `readxMapData` renderUI + `flowjo_import_ui` + all import observers |
| `reader_bead_raw.R` | bead-raw parsers in `import_lumifile.R` + `generate_layout_template_ref.R` |
| `reader_bead_xponent.R` | `xPonentReader.R` |
| `reader_elisa_xlsx.R` | `elisa_reader.R` parsers/template/validators (+ `elisa_diagnostic.R`) |
| `reader_flow_flowjo.R` | `flowjo_reader.R` + `flowjo_read_functions.R` + `generate_flowjo_layout_template.R` |
| `reader_bead_rbx.R` | **NEW** — added in the final phase |

---

## 5. Retire / keep / edit classification

**Retire fully (import-only; removed from `app.R` source list at the end):**
`import_lumifile.R`, `xPonentReader.R`, `elisa_reader.R`, `elisa_diagnostic.R`,
`flowjo_reader.R`, `flowjo_read_functions.R`, `generate_flowjo_layout_template.R`,
`generate_layout_template_ref.R`, `segment_reader.R`.

**KEEP as shared library — do NOT retire (used beyond import):**
`plate_validator_functions.R`, `batch_layout_functions.R`,
`curve_lookup_functions.R`, `db_functions.R`, `assay_response.R`,
`data_dictionary.R`. The new backend *calls* these seams. `derived_experiments.R`
(11.7), clone/delete modules, and the worker also depend on them — wholesale
retirement would break those. If a genuinely import-only helper is trapped inside
one of these, lift *that function* into a new reader file; leave the rest.

**Edit (wiring only, minimal):** `app.R` — swap the source list, drop the ~30
import reactiveVals, mount the three module instances. `ui_handler.R` — no change
needed (`import_tab` still renders one `uiOutput`, now the module's).

> Caution flagged in `REFACTOR_settings_cascade.md` §8: `app.R`/`ui_handler.R`
> have been silently reverted twice by re-basing on stale `uploads/` copies.
> `ui_handler.R` is **LF**; most import files are **CRLF** (`db_functions.R`,
> `flowjo_read_functions.R`, `assay_response.R`, `data_dictionary.R`, `global.R`
> are LF). Re-detect newline per file on every edit; verify the menuItem/tabItem
> wiring before and after.

---

## 6. Phased plan (each phase leaves the app runnable)

**Phase 0 — Contract + backend scaffolding (no behavior change).**
Create `assay_import_contract.R` (registry) and `assay_import_backend.R`
(`commit_assay_import` on `poolWithTransaction`, calling §2 seams). Wrap the three
existing readers as thin contract adapters so the registry is populated but the
live UI/observers are untouched. Structural validation only (`parse()` + balance).

**Phase 1 — Converge the antigen write (the issue's "first concrete step").**
Thread `feature` into bead + flow antigen lists (bead → per-(antigen,feature));
route all three through `upload_antigen_family`; delete the two inline antigen
insert blocks. Smallest, highest-value change; independently testable (settings
reach the cascade for every assay, worker stops erroring). Semantic/data change to
bead's `xmap_antigen_family` documented here.

**Phase 2 — Unify the commit.** Replace the three inline commit observers with
`commit_assay_import`; retire the dead `upload_batch_to_database`. Decide
curve_lookup soft-fail-within-txn vs hard-fail. Row insertion unified on
`insert_to_table`; visits on `upload_planned_visits`.

**Phase 3 — Generic module + UI standard.** Build `assay_import_module.R`; move
import reactiveVals into module state; mount `elisa`/`bead`/`flow`; rewire
`readxMapData` to the module. Retire the old renderUI/observers.

**Phase 4 — Retire old files.** Drop the nine import-only files from `app.R`;
each format is now a clean reader behind the contract. Re-verify wiring per §5
caution.

**Phase 5 — Add `*.rbx`.** Implement `reader_bead_rbx.R` to the §4.1 contract;
register it in the bead format registry. No UI or backend change — proves the
contract.

---

## 7. Invariants & risks (do not violate)

- **Never edit or rename a `curve_lookup` natural-key value, incl. `source`.**
  `source` is identity *and* a join key (`curve_lookup` ↔ `xmap_standard`).
- **`register_curve_lookup` before any cascade settings write** (§3, ordering).
- **Keep the 11.9a dual-write bridge:** `xmap_antigen_family` write stays (for
  annotations / editor / legacy view) alongside the cascade write, until
  `xmap_antigen_family` is formally retired (needs an annotation home first —
  11.6). The cascade write is the one the worker reads.
- **project_id is never NULL; real projects are `>= 16`.** Import must carry a
  real `project_id` (`workspace_id`) into every table + curve_lookup.
- **pool + transactions:** the import commit does *not* use temp tables today, so
  `poolWithTransaction` is a clean fit. Do **not** blind-swap `conn→pool` on the
  temp-table/transaction functions elsewhere in `db_functions.R` (§6 of the
  cascade doc) — those are out of scope here.
- **Silent-failure risks:** named args at every call site; `$N` bind order must
  match `params` order; positional call sites die quietly on signature change.
- **Bead "multiple" feature case:** bead Raw/xPONENT allow a single feature value
  or "multiple". Per-(antigen,feature) fan-out for the multi-feature case is the
  one genuinely new bit of logic in Phase 1 — scope it explicitly there.
- **Data-change verification (Phase 1):** confirm on a real import that bead
  `xmap_antigen_family` now has one row per (antigen, feature) and that
  `calib_settings` receives the four params at full ladder depth.

---

## 8. Open questions for you before Phase 0  (ANSWERED — see §9)

1. One generic module vs three hand-written modules.
2. curve_lookup failure policy under the new transaction.
3. Bead multi-feature imports — real path or defer?
4. Template step in the contract — completed template as source of truth?

---

## 9. DECISIONS (locked)

1. **One generic module + three descriptor files.** The 5-step UI is written
   once; ELISA/bead/flow are three descriptors mounted three times.
2. **Hard-fail `register_curve_lookup`.** A curve_lookup failure aborts the
   atomic commit (reverses the 11.9a "non-fatal, never rolls back"). Anything
   that relied on an import succeeding through a curve_lookup miss now sees the
   rollback instead — intended.
3. **Multi-feature bead is first-class, not deferred.** Import is explicit in
   features and antigens: the template expresses antigen × feature, and the
   antigen family / curve_lookup land one row per measured (antigen, feature).
   Bead stops being one-feature-per-import.
4. **Two-stage reader confirmed.** parse_raw/make_template →
   review/edit/highlight → re-upload → parse_layout/validate → (if clean) commit.
   The completed layout template is the source of truth and the cross-batch
   consistency check.

---

## 10. Phase 0 + Phase 1 — AS BUILT

**New files:** `assay_import_contract.R`, `assay_import_backend.R`. Both LF,
pool-first, structurally balanced, no edits to any legacy file. Every DB seam
they call is a kept-library function with a verified signature.

**Phase 0 — contract + backend.**
- `assay_import_contract.R`: canonical constants (`CURVE_LOOKUP_NK`,
  `AI_SHEET_NAMES`, `AI_FRAME_NAMES`, `AI_ANTIGEN_LIST_COLS` incl. feature,
  `AI_ANTIGEN_TEMPLATE_COLS`, `AI_COL_LIMITS` from the DDL); the reader spec
  (`new_assay_reader`) + format registry (`register_assay_format`,
  `get_assay_reader`, `list_assay_formats`); the shared stage-2 validator
  (`validate_layout_sheets` + `layout_sheets_ok`); the shared assembler
  (`assemble_upload_frames`); `default_split_experiments`.
- `assay_import_backend.R`: `commit_assay_import(pool, units, scope,
  timepoint_map, opts)` — atomic over `poolWithTransaction`, hard-fail
  curve_lookup, ordering standards → curve_lookup → antigen family; internal
  `.insert_or_die` (turns insert_to_table status into a rollback) and
  `.summarise_counts`.

**Phase 1 — converge the antigen write, make features explicit.**
- The assembler now emits a **feature-explicit antigen_list**: one row per
  measured (antigen, feature). `antigen_feature_pairs()` reads the distinct
  (antigen, feature) pairs from `assay_response_long` (the measured reality);
  `attach_feature_to_antigen_list()` fans the per-antigen metadata onto those
  pairs. A template that already has a fully-populated `feature` column is
  trusted unchanged.
- New `land_antigen_family()` in the backend replaces `upload_antigen_family`
  in the commit path. It reuses the SAME primitives
  (`prepare_batch_antigen_family`, `insert_new_rows`,
  `write_antigen_settings_to_cascade`) but fixes the dedup key to include
  `feature` (feature-aware existing read + `(study,exp,antigen,feature)`
  anti-join + within-batch de-dup). This is the fix for the multi-feature case
  that `upload_antigen_family`/`get_existing_antigens` couldn't express, done
  without editing either kept function.
- Validator additions surface, at the review step: missing antigen metadata
  columns (fatal — `prepare_batch_antigen_family` would error), the "cannot
  determine feature" case, and antigen ↔ measurement mismatches (warnings).

**Achieved:** all three assays now reach an identical, feature-explicit
antigen-family + cascade landing through one routine; the two inline antigen
paths become unreachable once the readers/module move onto this backend
(Phases 2–3); the worker gets its settings for every assay.

**SCHEMA PREREQUISITE — RESOLVED (DDL reviewed).**
`madi_results.xmap_antigen_family` has `feature character varying(15)` (nullable,
no default), and its ONLY constraint is `PRIMARY KEY (xmap_antigen_family_id)`
(a surrogate sequence key). There is NO unique constraint on
`(study, experiment, antigen)`, so multi-feature rows cannot collide and no
schema change is required. Two consequences: (a) there is no DB-level dedup, so
`land_antigen_family`'s app-side `(study,exp,antigen,feature)` anti-join + within-
batch de-dup is load-bearing; (b) `model_form_list` / `pcov_threshold` /
`concentration_unit_reported` are NOT NULL but have defaults, so the import
omitting them is correct (those settings live in the cascade per 11.9a).

**Source order (wiring, later phase):** `assay_import_contract.R` before
`assay_import_backend.R`, both after the seam libraries
(`plate_validator_functions.R`, `batch_layout_functions.R`,
`curve_lookup_functions.R`, `db_functions.R`, `settings_cascade_access.R`).

---

## 11. Phase 2 — bead reader (Option A: wrap-then-lift) — AS BUILT

**New file:** `reader_bead.R` (LF), plus small additions to the contract and
backend. Registers the two bead formats onto the contract; no legacy edits.

- Contract additions: `ai_validate_assay_response()` (ported from the legacy
  `validate_assay_response_data` so the reader doesn't depend on the retire-set
  `import_lumifile.R`) and `ai_bridge_result()` (folds a legacy
  `list(is_valid, messages, warnings)` into the issues frame).
- Backend addition: `run_assay_commit(pool, reader, sheets, scope, opts)` —
  generic assemble → split → commit glue so the Phase-3 module stays thin.
- `reader_bead.R` registers **bead/raw** (`.xlsx`) and **bead/xponent** (`.csv`).
  Both share ONE `parse_layout` (`import_layout_file` + ported normalization:
  project_id inject, nominal_sample_dilution compute, assay_response_long from
  sheet or raw-preview fallback), ONE `assemble` (shared assembler with
  `subject_merge = TRUE`), and ONE `validate_sheets` (shared validator +
  bridged `validate_batch_plate_metadata` + `ai_validate_assay_response`). They
  differ only in `parse_raw` (`process_experiment_files` vs
  `process_xponent_files`) and the xPONENT `assay_response_long_override` fed to
  the shared `generate_layout_template`. Default `split_experiments` → one unit,
  features carried as columns (multi-feature bead).

**Transition delegations (call in place now, LIFT into `reader_bead.R` in Phase
4, then retire the source):** `process_xponent_files()` (`xPonentReader.R`),
`generate_layout_template()` (`generate_layout_template_ref.R`). Everything else
the reader calls is a kept library.

**Not yet wired to the live UI.** Phase 2 defines + registers the bead reader and
the commit orchestrator; validated structurally (balance + every called
signature resolved). End-to-end execution arrives with the Phase 3 module, which
supplies `opts` from its inputs and calls `parse_raw`/`make_template`/
`parse_layout`/`validate_sheets`/`run_assay_commit`.

**Next:** Phase 2 continues with the ELISA and flow readers (same adapter
pattern), then Phase 3 builds the one generic module + three descriptors.

### 11.1 Phase 2 COMPLETE — all three readers

`reader_bead.R`, `reader_elisa.R`, `reader_flow.R` now register all four formats
(bead/raw, bead/xponent, elisa/xlsx, flow/flowjo). Contract/backend gained the
support they needed:

- **Assembler generalised** (`assemble_upload_frames`): a `natural_key` opt
  (bead/flow use the 4-col key; ELISA uses `(plateid, well)`); header trimmed to
  `AI_HEADER_COLS` (insert_to_table does not trim, so an unknown column would
  fail the append); `feature` sourced from whichever side has it (plates_map for
  bead/ELISA, response for flow) to avoid a `.x/.y` collision; quality metrics
  (`pctaggbeads`/`samplingerrors`) carried from the response side too.
- **ELISA** (`reader_elisa.R`): shared assembler with `extra_cols = "wavelength"`,
  `natural_key = (plateid, well)`, `response_variable = "absorbance"`, no subject
  merge. Its legacy `merge(antigen_list, features, by = NULL)` cross-join is
  DROPPED — the assembler derives a feature-explicit antigen list from measured
  pairs (Phase 1), which is stricter and uniform. Delegates to
  `process_elisa_files`, `generate_elisa_layout_template`,
  `validate_elisa_plate_metadata`, `validate_elisa_layout_data` (all
  `elisa_reader.R`, lift in Phase 4).
- **Flow** (`reader_flow.R`): analyte is the isotype (`feature` from the response;
  no `antigen`, so curves land `antigen = '__none__'`); `pct_agg → pctaggbeads`;
  ported pipe-dilution resolution (`.flow_resolve_dilution`); and an override
  `split_experiments` that fans the assembled frames into one commit unit per
  feature (`experiment_<feature>`), tagging each unit's antigen_list with that
  feature. This makes flow's feature explicit in the antigen_list column (not
  just the experiment name as before) so the feature-required antigen landing is
  satisfied. Delegates to `load_flowjo_file`, `pivot_flowjo_long`,
  `generate_flowjo_layout_template` (RETIRE set, lift in Phase 4).

**Flow open item (not a regression):** flow standards carry no `antigen`, so
curve_lookup rows get `antigen = '__none__'` while the antigen-family rows carry
the antigen_list's `antigen_abbreviation`. The cascade settings write keys on
(feature, antigen) from curve_lookup, so per-antigen flow settings may not match
— same ambiguity as the legacy flow path. Revisit when flow's antigen/feature
semantics are formalised (relates to 11.6).

All five new files balance structurally; every function the readers call resolves
to the contract, a kept library, or a documented Phase-4 lift target. Still not
wired to the live UI — that is Phase 3.

### 11.2 Flow antigen policy (corrected)

Flow binding is always to a real target (a whole virus/bacterium, e.g. "PT"),
so `__none__` is NOT acceptable for a flow antigen. `build_flowjo_antigen_list`
already carries `antigen_abbreviation` (the target) and `feature` (isotype), one
row per isotype. `reader_flow.R` now: (a) `split_experiments` threads that
antigen onto the standards/samples of each feature-unit so curve_lookup records
the real antigen instead of defaulting to `__none__`; and (b) `validate_sheets`
REFUSES a blank / `__none__` antigen (error) so it must be specified before
commit. Bead/ELISA already supply real antigens, so this is flow-only.

---

## 12. Phase 3 — generic module + descriptors + wiring — AS BUILT

**New files:** `assay_import_module.R` (generic UI + server),
`descriptor_bead.R`, `descriptor_elisa.R`, `descriptor_flow.R`,
`assay_import_mount.R`. All LF, structurally balanced.

- **`assay_import_module.R`** — ONE module implementing the 5-step standard:
  upload raw → download template → upload completed template → validate (issues
  shown in a DT table, errors red / warnings amber) → preview (per-sheet row
  counts) → commit (a `conditionalPanel` gated on `output$ready`, which is
  `layout_sheets_ok(issues) && sheets present && not already committed`). The
  ~30 app.R import reactiveVals collapse into the module's `reactiveValues(raw,
  sheets, issues, status, committed)`. It dispatches to registered readers via
  `current_reader()` and commits via `run_assay_commit`. `build_opts()` maps the
  descriptor's controls + scope into the reader `opts` (incl. `raw_preview` and
  flow's `dilutions_ref`).
- **Descriptors** — each declares `assay`, `label`, `default_format`, and
  `assay_controls(ns)` (bead: n_wells/feature/delimiter/element orders; ELISA:
  same minus feature; flow: n_wells + feature). Formats come from the registry.
- **`assay_import_mount.R`** — `mount_assay_import(output, pool, scope)` sets
  `output$readxMapData` to a 3-tab `tabsetPanel` of the assay UIs and starts one
  module server per assay. Single integration seam.

### app.R WIRING PATCH (apply deliberately — see §8 revert caution)

Two edits inside `server <- function(input, output, session)` (starts app.R:475):

**(a) Source the new stack.** After the existing import sources
(app.R:1102–1112), add — keeping the old sources for now so the delegated
parsers remain available (they are retired in Phase 4):

```r
      # 11.10 new assay-import stack (Phase 3). Order matters:
      source("assay_import_contract.R", local = TRUE)
      source("assay_import_backend.R",  local = TRUE)
      source("assay_import_module.R",   local = TRUE)
      source("reader_bead.R",           local = TRUE)
      source("reader_elisa.R",          local = TRUE)
      source("reader_flow.R",           local = TRUE)
      source("descriptor_bead.R",       local = TRUE)
      source("descriptor_elisa.R",      local = TRUE)
      source("descriptor_flow.R",       local = TRUE)
      source("assay_import_mount.R",    local = TRUE)
```

Sourcing these AFTER `import_lumifile.R` means the new `output$readxMapData`
(via the mount call below) overrides the old renderUI at import_lumifile.R:173.
The old import observers stay defined but bind to input ids no longer present in
the UI, so they never fire (harmless) until Phase 4 removes them.

**(b) Mount once.** Anywhere in the server body after the sources (e.g. right
after the source block), add:

```r
      mount_assay_import(
        output = output,
        pool   = db_pool,
        scope  = reactive(list(
          project_id = userWorkSpaceID(),
          study      = input$readxMap_study_accession,
          experiment = input$readxMap_experiment_accession_import,
          user       = currentuser()
        ))
      )
```

`db_pool` is the global pool (global.R:221); `userWorkSpaceID()` (app.R:861) and
`currentuser()` (app.R:866) are the existing scope reactives. No other app.R
change is needed for Phase 3; `ui_handler.R` is untouched (it already renders
`uiOutput("readxMapData")`).

### Verify after wiring
- Import tab shows three sub-tabs (Bead Array / ELISA / Post-gating Flow).
- Bead xPONENT still offers the format sub-selector (two bead formats).
- A completed template with an over-length antigen or (flow) a blank antigen is
  blocked with a red issue row; a clean file enables Commit.
- A committed import is atomic (kill the DB mid-commit → nothing lands) and a
  curve_lookup failure aborts the whole import.

### Still open
- **Phase 4:** remove the old import sources (app.R:1103,1105,1106,1107,1108,
  1111 and the commented 1112) and lift the delegated parsers into the reader
  files; retire `import_lumifile.R`, `xPonentReader.R`, `elisa_reader.R`,
  `elisa_diagnostic.R`, `flowjo_reader.R`, `flowjo_read_functions.R`,
  `generate_flowjo_layout_template.R`, `generate_layout_template_ref.R`,
  `segment_reader.R`.
- **Phase 5:** add `reader_bead_rbx.R` (`.rbx`) — register only, no UI/backend
  change.
- The module's preview is intentionally minimal (per-sheet row counts); richer
  plate/plot previews can be added to `assay_import_module.R` without touching
  readers or backend.

---

## 13. Phase 4 — retire the old, wire in the new — AS BUILT

The new stack is now self-contained and wired live. Verified: `app.R` and
`ui_handler.R` balance; no surviving file references a retired-only function
(one landmine found and fixed — see below); no `source()` of a retired file
remains; the mount call is present once; all 15 new sources are wired.

**Parser extraction (lift, then retire).** Two retire-set files mixed pure
parsers with observers; the parsers were extracted verbatim (brace-aware, then
balance-checked) into new libraries, and the observer/UI halves dropped:
- `reader_bead_xponent_parsers.R` — the 4 xPONENT parsers from `xPonentReader.R`.
- `reader_elisa_parsers.R` — the 21 ELISA parser/template/validator functions
  from `elisa_reader.R`.

**Landmine fixed:** `extract_plate_number()` was defined in `import_lumifile.R`
but is called by kept libraries (`batch_layout_functions.R`,
`generate_layout_template_ref.R`). Lifted into new `import_helpers.R` so it
survives the retirement.

**Kept as parser libraries (pure functions, no observers), now sourced directly
by `app.R`:** `generate_layout_template_ref.R`, `flowjo_read_functions.R`,
`generate_flowjo_layout_template.R` (the last two were previously sourced *by*
`flowjo_reader.R`).

**`app.R` edits (inside the post-auth `observe` at ~line 837):** removed the five
retire-set `source()` lines; added `import_helpers.R`, the two flow parser libs,
the two extracted parser libs, the 10 new-stack files, and one
`mount_assay_import(output, db_pool, scope)` call where `scope` reads
`userWorkSpaceID()` / `input$readxMap_study_accession` /
`input$readxMap_experiment_accession_import` / `currentuser()`.

**`ui_handler.R` edit:** removed `source("import_lumifile.R", local=TRUE)` from
the tail of `load_project()` (it re-sourced the old UI on every project load,
which would have clobbered the module's `output$readxMapData`). The module is
mounted once and is reactive to the workspace, so no per-load re-source is needed.

**DELETE these files from the repo (fully retired, no longer sourced, nothing
references them):**
`import_lumifile.R`, `xPonentReader.R`, `elisa_reader.R`, `elisa_diagnostic.R`,
`flowjo_reader.R`, `segment_reader.R`.
(`elisa_diagnostic.R`'s only function `run_elisa_diagnostic` has no callers.)

**Full source order now in `app.R`:** kept libs (plate_validator_functions,
generate_layout_template_ref, batch_layout_functions, curve_lookup_functions,
derived_experiments) → import_helpers → flowjo_read_functions →
generate_flowjo_layout_template → reader_bead_xponent_parsers →
reader_elisa_parsers → assay_import_contract → assay_import_backend →
assay_import_module → reader_bead → reader_elisa → reader_flow →
descriptor_bead → descriptor_elisa → descriptor_flow → assay_import_mount →
`mount_assay_import(...)`. (Function resolution is at call time, so this order is
safe; it is also read-top-to-bottom sensible.)

### Test checklist (Phase 4)
- App boots; Import tab shows three sub-tabs (Bead Array / ELISA / Post-gating
  Flow); no "could not find function" errors on load or project switch.
- Bead Array shows the Raw/xPONENT format selector; each parses raw files,
  downloads a template, re-uploads it, validates, previews, commits.
- ELISA and flow: same 5-step loop end-to-end.
- Over-length antigen / (flow) blank antigen is blocked with a red issue row;
  clean file enables Commit.
- Commit is atomic (interrupt mid-commit → nothing lands); a curve_lookup
  failure aborts the whole import (hard-fail).
- Multi-feature bead: `xmap_antigen_family` gets one row per (antigen, feature);
  `curve_lookup` one curve per (antigen, feature); cascade settings written at
  full ladder depth.
- Switch projects, reload — import tab still renders (no import_lumifile re-source).

### Files delivered in Phase 4
Edited: `app.R`, `ui_handler.R`. New: `import_helpers.R`,
`reader_bead_xponent_parsers.R`, `reader_elisa_parsers.R`. (Plus all Phase 0–3
new files.) Retire/delete: the six files listed above.

---

## 14. Live-testing fixes (ELISA + bead brought up end-to-end)

Interactive testing surfaced a series of real bugs, all now fixed. Recorded here
because several are traps the flow and `.rbx` paths would otherwise re-hit.

**Module / mount (`assay_import_module.R`, `assay_import_mount.R`, `app.R`)**
- **Loop-capture:** mounting the three module servers with a `for` loop passed
  `descriptors[[k]]` as an unforced promise; all three resolved to the last
  (flow) descriptor, so the ELISA tab ran the flow reader. Fixed with `lapply`
  + `force(descriptor)` in `assay_import_server`.
- **Explicit parse:** parsing was auto-triggered by `observeEvent(input$raw_files)`,
  which didn't fire reliably in the rendered module and left the user with no
  control. Replaced with an explicit "Parse uploaded file(s)" button + status
  line + `[assay_import]` console breadcrumbs.
- **Download gate:** the template download is gated behind a successful parse
  (`output$has_raw`) so it can't emit a broken `.htm` before data exists.
- **Restored experiment control:** the "Experiment name" selectize
  (`readxMap_experiment_accession_import`, create-enabled, 15-char cap) and its
  study-driven choice-population observer lived in the retired `import_lumifile.R`;
  rebuilt in `mount_assay_import` (needs `input/session/study_exp/
  experiment_choices_rv`, now passed from `app.R`). Without it, experiment scope
  was always NULL.

**The `%||%`-on-vectors trap (bit 3×: element_order, natural_key, functions)**
The app's global `%||%` is comparison-based (`x == ""`), so `%||%` on any
non-scalar throws `'length = N' in coercion to logical(1)`. RULE: never use
`%||%` on a non-scalar. All vector/object/function-valued defaults now use
explicit `if (is.null(x)) default else x` — in the reader `make_template`s
(element/bcs orders), `assemble_upload_frames` (extra_cols, col_mapping,
natural_key), and `new_assay_reader` (validate/assemble/split fns). Scalar `%||%`
is still fine.

**Commit backend (`assay_import_backend.R`) — robustness added while debugging**
- **List-column flatten:** `.insert_or_die` collapses any list-column (e.g.
  per-plate `wavelengths = c(450,620)`) to a scalar string; a list-column makes
  `dbAppendTable` drop the backend connection ("server closed unexpectedly").
- **Table-column trim:** `.insert_or_die` (and `land_planned_visits`) trim frames
  to the table's real columns (from `information_schema`) so a stray column
  (e.g. ELISA timepoint's `time_unit`, absent from `xmap_planned_visit`) can't
  fail the `COPY`. Required columns are still validated first.
- **`land_planned_visits`** replaced `upload_planned_visits` to apply that trim
  before the dedup-insert.
- **Scope scalarized** in the pre-transaction checks; step-level `[commit]`
  breadcrumbs added.

**Assembler (`assay_import_contract.R`)**
- **NA specimen_type:** the specimen filter `plates_map[substr(specimen_type,1,1)
  == prefix, ]` injected an all-NA row per empty well (NA index in `[`),
  producing NULL `plate_id`/`well` at insert. Now `!is.na(st) & st == prefix`.
  Same guard added to both `nominal_sample_dilution` filters.
- **ELISA nsd:** `calculate_elisa_nominal_sample_dilution` backfills NA grouping
  columns and wraps `aggregate` in tryCatch (blank scope emptied the frame →
  "no rows to aggregate").
- **Antigen reconciliation** matches `assay_response_long$antigen` against
  `antigen_abbreviation` (not the raw plate label), so bead antigens stop
  showing as "not in antigen_list".

**Bead reader (`reader_bead.R`)**
- **Feature injection:** bead's `assay_response_long` carries no feature (a
  per-batch isotype the user types), so `.bead_parse_layout` injects
  `opts$feature_value` into `assay_response_long` + `plates_map`. The user MUST
  set the Feature box (single feature per batch for now; multi-feature bead will
  need per-well features in the template — future).
- **file_name shim:** `validate_batch_plate_metadata` wants DB-name `file_name`;
  the template plate_id sheet has `plate_filename`, so validation derives one.

**Status:** ELISA, bead, and flow all import end-to-end (parse → template → edit
→ re-upload → validate → preview → atomic commit with counts). Flow's per-feature
split and real-antigen threading verified (3 experiments PT_exp_IgA/IgG/IgM,
antigen = PT in curve_lookup, cascade settings written). Problem 2 (constrained/
ordered description-field controls) is DONE: bead + ELISA descriptors restore the
96/384 well selector, the SampleGroupA/B optional-elements toggle, and the two
drag-to-order orderInputs (sample elements; B/S/C elements); the module renders
the dynamic sample-element orderInput from descriptor$description_elements, and
build_opts feeds the ordered vectors to the template generators.

Remaining: Phase 5 (`.rbx` bead format), and the physical deletion of the six
retired files once the owner is satisfied with testing.

---

## 15. Phase 5 — `.rbx` bead format — DONE

`reader_bead_rbx.R` registers a third bead format (`bead/rbx`, `.rbx`/`.srbx`)
that parses the Bio-Plex binary via the provided `rbx_binary_parser.R`
(`parse_rbx` + `long_dataframe`) and mirrors `process_xponent_files`'
output — `combined_plates` (wide) + `assay_response_long_override` (long, with
bead counts) + `header_list` — so it flows through the SAME bead pipeline
(`.bead_make_template` xPONENT/override path, `.bead_parse_layout`,
`.bead_validate_sheets`, `.bead_assemble`). No contract/backend/module change was
needed — the pluggable-reader design held. `app.R` sources `rbx_binary_parser.R`
then `reader_bead_rbx.R` after `reader_bead.R`.

**Live-tested end-to-end** (Tdap.rbx → committed):
`header:1 samples:444 standards:120 blanks:12 controls:0 antigens:6 visits:1
curves:6` — 96 wells × 6 analytes = 576 measurements, reconciled.

`.rbx`-specific handling added during bring-up:
- **Dilution:** the binary carries an authoritative per-well `dilution`
  (standard-curve dilutions for S, sample dilutions for X). `process_rbx_files`
  emits a `dilution_map`; the module threads it via `opts$dilution_map`;
  `.bead_parse_layout` injects it into `specimen_dilution_factor` by
  (plateid, well) and force-recomputes `nominal_sample_dilution` from it.
  (Confirmed: "injected .rbx dilution into 96/96 wells".)
- **Header:** `file_name` = the binary's real `source_path` (needs directory
  separators for the metadata validator); `acquisition_date` falls back to the
  file timestamp (not in the binary); `reader_serial` from the binary.
- **Antigen names:** analyte names are canonicalised at the source with
  `clean_antigen_label` so the wide columns and the response agree and no field
  carries a slash.

## 16. Global antigen-name policy (no slashes, consistent vocabulary)

`clean_antigen_label` ('/' -> '_', spaces -> '.', other punctuation removed) is
now applied to antigen names for EVERY assay, via a shared `.ai_clean_antigen`
helper in the contract:
- `assemble_upload_frames` canonicalises `assay_response_long$antigen`,
  `plates_map$antigen`, and `antigen_list$antigen_abbreviation` at commit;
- both validator reconciliations (`validate_layout_sheets`,
  `ai_validate_assay_response`) compare cleaned names, so a slash-only
  difference no longer raises a false warning.
It is idempotent (clean names unchanged) and NA-safe (flow's pre-threading
antigen passes through), so the working assays are undisturbed.

## 17. STATUS — 11.10 COMPLETE

All import paths verified end-to-end against real data through the unified
stack (one contract, one atomic backend, one generic module + descriptors):
- **ELISA** (.xlsx) — committed.
- **Bead** — raw (.csv-family), xPONENT, and **.rbx** — all committed.
- **Flow** (FlowJo .xlsx) — committed, per-feature split + real-antigen threading.
- **Description controls** — constrained/ordered orderInputs restored (bead/ELISA).
- **Legacy files retired** (six deleted by the owner); parsers lifted into
  `reader_bead_xponent_parsers.R` / `reader_elisa_parsers.R` / `import_helpers.R`.
- **Slash-free antigen vocabulary** enforced across all assays.

Known small follow-ups (non-blocking): (1) over-length identifiers (e.g.
`patientid` > 15) are now flagged at validation for the user to shorten;
(2) `.rbx` `acquisition_date` is a file-timestamp fallback (binary lacks it) and
can be corrected in the template; (3) if desired, inject the `.rbx` dilution at
template GENERATION (openxlsx) so the downloaded template shows real dilutions
during review (currently injected at parse/commit).

## 18. `.rbx` dilution + subject_id auto-fill (final) — DONE

The `.rbx` embeds the real dilution as a `1:N` ratio in the description
(standards: `Inhouse Ref 1:2952450`; diluted samples: `QC1 1:2500`). At the
source, `process_rbx_files` now splits each description into
`<name><delimiter><N>`, where `<name>` is the ratio-stripped label truncated to
the `patientid` limit (15) and `<N>` is the dilution. `build_plates_map` then
fills `specimen_dilution_factor` at GENERATION (name -> PatientID/Source slot,
`N` -> DilutionFactor slot), so the DOWNLOADED template already shows the correct
standard-curve and sample dilutions — no manual editing — and `subject_id` never
exceeds 15. `calculate_nominal_sample_dilution` filters `specimen_type == "X"`,
so `nominal_sample_dilution` reflects only test-sample dilutions even though
standards now carry their real dilutions. `.rbx_dil_from` / `.rbx_strip_ratio`
are defined in `reader_bead_rbx.R` (self-contained) so the path does not depend
on load order. (Assumes the default element order: name in slot 1, DilutionFactor
in slot 2 — which the descriptors use.) Supersedes follow-up (3) above.

**11.10 fully delivered and verified end-to-end for all formats, including the
`.rbx` standard-curve dilutions landing in `xmap_standard.dilution`.**

### 18.1 Refinements (post-verification)
- **Helper consolidation:** `.rbx_dil_from()` / `.rbx_strip_ratio()` now live in a
  single home (`reader_bead.R`, sourced before `reader_bead_rbx.R`); the
  duplicate copies were removed from `reader_bead_rbx.R`.
- **Order-aware `.rbx` split:** the Description builder
  (`.rbx_build_description`) now places the name in the PatientID (X) / Source
  (B/S/C) slot and the dilution in the DilutionFactor slot according to the
  user's actual `element_order` / `bcs_element_order` (threaded from opts), not a
  fixed position — so reordering the description elements no longer misaligns the
  generation-time dilution fill.
- **`.rbx` acquisition_date (ACKNOWLEDGED):** the Bio-Plex binary does not carry
  an acquisition timestamp, so the reader uses the uploaded file's modification
  time as a fallback (in an accepted date format) to satisfy validation. This is
  a known, accepted limitation; correct it in the plate_id sheet during template
  review if the true acquisition date is needed downstream.
