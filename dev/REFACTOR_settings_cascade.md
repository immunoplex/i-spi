# Settings Cascade Refactor — Architecture & Conventions

Orientation for anyone inheriting this codebase. It describes the unified
settings cascade: what it is, how the app and worker consume it, the naming and
scope conventions to follow, the database invariants that are now enforced, and
— importantly — **what is deliberately unfinished** so you don't either break the
finished parts or "fix" something that was left undone on purpose.

Read the "Deferred / not done" and "Conventions" sections before writing any new
data-access code. Most of the bugs during this refactor came from violating a
convention that wasn't written down yet. Now it is.

---

## 1. What the cascade is (the one-paragraph model)

There is **one settings store** (`madi_results.calib_settings`), **one resolver**
(`madi_results.resolve_settings(...)`), and **one metadata catalog**
(`madi_results.calib_settings_meta`). Both the fitting **worker** and the **app**
read settings through the resolver — neither reads settings from the old
`xmap_study_config` / `xmap_antigen_family` tables anymore. Settings are stored
sparsely: a row exists only where a value **overrides** a coarser default. A
scope with no row inherits from the tier above it, down to the seeded
`__system__` defaults. Editing a setting writes an override; "reset" deletes it.

This replaced a design where the same settings were copied per-study (and
per-user) across multiple tables, which drifted out of sync. The cascade's whole
point is that there is exactly one place each setting lives, resolved by scope.

---

## 2. The scope model and the ladder

A setting resolves against a **scope**: `(project, study, experiment, feature,
antigen)`. Tiers, most general to most specific:

| tier_rank | scope columns set                                      |
|-----------|--------------------------------------------------------|
| 0         | system (`project_id = -1`, everything else `__none__`) |
| 1         | project                                                |
| 2         | project + study                                        |
| 3         | + experiment                                           |
| 4         | + feature                                              |
| 5         | + feature + antigen                                    |

`resolve_settings` returns, per param, the value from the **most specific tier**
that has a row (highest `tier_rank`), else the system default.

**The ladder may not skip levels** — this is enforced by CHECK constraints:
- antigen requires feature (`antigen = '__none__' OR feature <> '__none__'`)
- feature requires experiment
- experiment requires study
- `project_id = -1` (system) requires everything else be `__none__`

Consequence worth internalizing: a per-antigen setting **requires a feature** in
its scope. "One antigen across all features" is *not expressible*, because
feature is a coarser tier than antigen. If a UI needs that, it must fan out to
one row per (feature, antigen).

**Sentinels:** text wildcard = `'__none__'`; project wildcard/system tier = `-1`.
Real project ids are `>= 16` (a CHECK on `curve_lookup` enforces this; ids
`0/1/4` were dev/test and were removed). **project_id is never NULL** going
forward — a missing project is a hard error, not a "match all".

---

## 3. The consumption paths

- **Worker** (`worker_curveR.R`): resolves per-`curve_id` via
  `resolve_settings_batch`, reads value-only, fits.
- **App fit path** (`std_curve_calc_module.R`, `std_curve_view_module.R`):
  resolves at the scope of the fit (whole-experiment → `feature/antigen =
  __none__`; single feature/antigen → that scope).
- **App settings editor** (`settings_cascade_ui.R`): reads
  `settings_editor_view()` (meta LEFT JOIN resolver, so *every* catalogued param
  shows, set or not) and writes with `set_setting()` / `unset_setting()`.
- **App study-config reads** (`study_configuration.R` + `_ui.R`):
  `fetch_study_configuration()` is now a resolver-backed shim returning the
  legacy long shape so the ~15 render consumers were unchanged except their
  call signature.

**`model_form_list` contract (load-bearing):** the cascade stores the
*authorized* model set; the Compute-fits control caps its options to whatever the
cascade resolved for that scope and lets the user only *shorten* it. The worker
fits exactly the submitted list. System default is all five
(`logistic4,loglogistic4,gompertz4,logistic5,loglogistic5`).

---

## 4. CONVENTIONS — follow these in all new data-access code

These are not style preferences; violating them caused most of the refactor's
bugs, several of which fail **silently** (wrong rows, no error).

### 4.1 `pool` first, always
Every access-layer function takes `pool` as its first argument (a `pool::Pool`;
DBI generics dispatch on it). Do **not** use the legacy global `conn` in new
code. The global `conn` still exists (see Deferred §6) but is being retired.

### 4.2 Scoped fetches: `(pool, project, study, experiment[, tbl])`, project-SECOND
Every scoped fetch has the **identical** signature order — project first among
the scope args:

```r
fetch_x_scoped <- function(pool, project, study, experiment) { ... }
```

and its SQL binds `WHERE project_id = $1 AND study_accession = $2 AND
experiment_accession = $3` with `params = list(project, study, experiment)`.
The `$N` order and the `params` order **must match**; a mismatch binds a study
string into an integer column (the classic `invalid input syntax for type
integer: "MADI_..."` error) or, worse, swaps two string columns and returns
wrong rows with no error.

### 4.3 Name the arguments at call sites
Always call scoped fetches with **named** args:

```r
fetch_x_scoped(db_pool, project = s$project_id, study = s$study, experiment = s$experiment)
```

Named binding survives a future signature reorder; positional binding fails
silently when a signature changes. Positional call sites are where signature
changes go to die quietly.

### 4.4 Strict project equality — except the resolver
Scope filters use plain `project_id = $1`. Do **not** use `IS NOT DISTINCT FROM`
for the project scope filter (that reintroduces NULL-matching we removed). The
**only** legitimate null-safe matching is (a) inside `resolve_settings` itself
(tier matching, where a wildcard must match) and (b) the natural-key joins that
match a curve to its standard/blank rows across nullable NK columns. Those stay.

### 4.5 Writing settings
Write via `set_setting(pool, project, study, param_name, value, user, experiment=,
feature=, antigen=)`. It UPSERTs (sparse store has no pre-seeded row to UPDATE),
routes the value into the correct typed column from `calib_settings_meta`, tags
`param_user` as **audit only** (not part of the key), and validates the ladder.
"Revert to default" is `unset_setting()` (a DELETE), never writing the default
value. Config is **study-scoped, not per-user** — collaborators share settings.

### 4.6 Adding a new setting = data, not code
Insert a `calib_settings_meta` row (with `param_group`, `param_control_type`,
`param_choices_list`, `param_description`) and a `__system__` default in
`calib_settings`. The editor renders it generically. No UI code per param.

---

## 5. Invariants enforced in the database

- `calib_settings` CHECK constraints: the scope ladder (§2) and system-tier-is-
  fully-wildcarded.
- `curve_lookup.project_id >= 16` (no dev/test/NULL projects).
- `calib_settings_scope_param_uniq` — one row per (scope, param); UPSERTs conflict
  on it.
- Every param in `calib_settings` has a row in `calib_settings_meta` (the editor
  depends on it).

---

## 6. Deferred / NOT done — do not assume these are finished

- **`conn` → `db_pool` migration is incomplete.** Only the calib/settings access
  layer and the study-config files use `pool`. **~26 other files still use the
  global `conn`** (`conn <- get_db_connection()` in `global.R`). That global must
  stay until they're migrated. Several of those files (`db_functions.R`,
  `import_lumifile.R`, `dilution_linearity_functions.R`) use **temp tables and
  multi-statement transactions**, which are pool-incompatible — they need
  `pool::poolWithTransaction`, not a blind swap. This is its own careful project.
- **Annotation and data-editing are parked behind the wall.** Descriptive antigen
  columns (`antigen_family`, `antigen_name`, strain, source, catalog) still live
  in `xmap_antigen_family` and are edited by the legacy (shrunken)
  `study_configuration_ui.R` paths. They are intentionally NOT in the cascade —
  the worker must never read annotation. An annotation store is a future step.
- **Spelling is reconciled, not normalized.** Antigen/feature case & punctuation
  were fixed only where a setting was stranded; broad NK fragmentation remains and
  recurs on ingest (nothing normalizes on write). See `NORMALIZATION_NOTES.md`.
- **Perf follow-up:** `lookup()`'s scoped fetch is done (indexed, ~10 rows). The
  Explore-fits view can show multiple `method` rows per curve — scope it to
  latest run / add a method filter (this masqueraded as a "fit ignored my models"
  bug once).

---

## 7. Diagnostics

Verbose tracing is gated behind one option. `vmsg(...)` (defined in `global.R`)
prints only when `options(ispi.verbose = TRUE)`; default is `FALSE`. Flip it at
runtime in the console — no restart. Diagnostics live permanently in the code
(mostly `std_curve_calc_module.R`); leave them, they're free when off.

---

## 8. Operational lessons (hard-won; save the next person the hours)

- **Verify an edit reached disk before restarting.** Several "the fix doesn't
  work" episodes were the edit never landing on the file the app sources. After
  editing, `sed -n 'N,Mp' file` and read it back *before* relaunching. Don't
  restart into unverified code.
- **Grep by body pattern, not function name, to find stragglers.** A signature
  change ripples to call sites and same-family functions. Name-based greps miss
  functions that don't share the naming (e.g. `standards_support` has no
  `_scoped` suffix). Grep the *body* pattern (`params = list(study`,
  `IS NOT DISTINCT FROM $N`) to find every non-conforming query.
- **Positional call sites fail silently on signature change.** When you change a
  signature, grep every call site; convert them to named args so it can't happen
  again.
- **Look directly at the table when a value seems wrong.** The "fits ran 5 models
  not 2" bug was stale `method` rows in `calib_fit`, not a submit bug — a
  `SELECT ... FROM calib_fit` settled it in seconds vs. hours of code reading.
- **A stale-in-memory function survives app reloads.** Fully restart the R
  process (not just the browser/app) after editing a sourced file, or an old
  function definition keeps running.
- **Edit the file uploaded THIS turn, not the copy in `uploads/`.** That folder
  accumulates pre- and post-refactor copies; `ui_handler.R` was refactored against
  a stale copy twice, silently reverting the 11.8 delete-tab wiring. For any
  cross-cutting file, re-verify the critical wiring (menuItem/tabItem, source
  order) BEFORE and AFTER every edit.
- **Detect newline style per file.** Most files are CRLF; `ui_handler.R` is LF.
  Multi-line search/replace fails silently against the wrong newline — read bytes,
  detect `\r\n` vs `\n`, build match strings with the file's newline.
- **Balance-check against the ORIGINAL, not zero.** Some files carry a pre-existing
  paren/brace imbalance from braces/parens inside SQL/JS string literals (e.g.
  `import_lumifile.R` is -3/1). A safe edit leaves the balance UNCHANGED vs the
  original; it is not necessarily zero.
- **No R runtime in the edit loop -> validate structurally.** Every edit: exact
  `str_replace` with a count==1 assert, then paren/brace/bracket balance vs the
  original; the human runs `parse()` + exercises the path. Claim "balances +
  expected to parse", not "works".
- **`register_curve_lookup` before any cascade settings write.** The antigen tier
  needs the full ladder `(project/study/experiment/feature/antigen)`, and `feature`
  comes from the registered curves — so the curves must exist first (normal imports
  and derived experiments alike).
- **`source` is part of the curve natural key AND a join key** (`curve_lookup` <->
  `xmap_standard`). Never `UPDATE` it to normalise spelling — that mutates identity
  and breaks joins. Use a non-destructive alias (`source_alias`) resolved to a
  canonical name at read time.
- **`calib_grid` stores only the best model per curve+method.** To overlay every
  converged model form, reconstruct each curve from its stored params
  (`fetch_calib_params`) via the curveRcore forward functions (`models.R`).
- **The worker reads settings ONLY from the cascade** (`resolve_settings_batch`).
  The `antigen_feature_settings` view / `.settings_map` readers in `worker_curveR.R`
  are dead code; imported settings must reach `calib_settings`.
- **Newline style is per file — re-detect every time.** In the std_curve family
  `std_curve_view_module.R`, `calib_data_access.R`, and `data_tab_module.R` are
  CRLF; `std_curve_compare_module.R` is LF. Build match strings with the file’s
  own newline or a multi-line replace fails silently.
- **The stale-`uploads/` trap recurred — treat the LAST-PRODUCED file as truth.**
  A `calib_data_access.R` edit re-based on a stale `uploads/` copy silently dropped
  two earlier functions (`fetch_scoped_table_counts`, scoped `fetch_curve_lookup`)
  while the deployed `data_tab_module.R` still called one of them. `diff` the
  deployed file against your latest and re-confirm earlier additions survive
  BEFORE editing.
- **A fast query can still be a slow read.** `calib_grid` `EXPLAIN ANALYZE`d at
  0.3s but took ~40s in the app: the wall is result WIDTH × rows over the VPN +
  RPostgres parsing, not the plan. Compare server exec time to the app’s measured
  read; fix by shipping fewer rows/bytes to the VIEW (cap the interactive display,
  keep exports full) — not query tuning. A `numeric->float8` cast does NOT cut
  transfer volume.
- **plotly fails at click-time, not parse-time.** Dual-axis / log-axis behaviour —
  `tickvals` units on a log axis, `overlaying`-axis range alignment, annotation
  coordinates on log axes — are runtime-only checks. Prefer text TRACES over
  annotations on log axes (traces take data coords; annotations need log10 x).

---

## 9. Key files

| file | role |
|------|------|
| `calib_data_access.R` | read boundary: scoped fetches, resolver-backed reads (`pool`, project-first) |
| `settings_cascade_access.R` | resolver reads + `set_setting`/`unset_setting` writes; scope options |
| `settings_cascade_ui.R` | the generic meta-driven editor + scope-picking breadcrumb |
| `study_configuration.R` / `_ui.R` | study-config reads/writes now via the cascade; seeder is a no-op stub |
| `worker_curveR.R` | worker consumption of `resolve_settings_batch` |
| SQL migrations | `create_settings_cascade.sql`, `migrate_*`, `seed_*`, `add_settings_scope_checks.sql`, `add_param_description.sql` |
| `std_curve_view_module.R` / `_calc_module.R` / `_compare_module.R` | Explore / Compute / Compare-fits sub-tabs, mounted as sibling `tabPanel`s under `uiOutput("std_curver_ui")` |
| `std_curve_compare_module.R` | 11.4 Compare-fits: fit-set abstraction, 4 modes (forms/methods/plates/sources), curves reconstructed from params, cross-plate CV, forest panels |
| `source_alias.R` / `source_alias.sql` | global (ungated) source-name alias editor + table/view/resolver; canonicalises `source` for the Compare tab |
| `delete_study_components.R` / `_ui.R` | 11.8 transactional experiment/whole-study deletion |
| `derived_experiments.R` | 11.7 ELISA wavelength subtraction + nominal-dilution split; each registers `curve_lookup` |
| `diag_multiplate_sources.R` / `.sql` | diagnostic: do any multiplate groups span >1 `source` |
| `write_antigen_settings_to_cascade()` in `plate_validator_functions.R` | 11.9a shared import->cascade antigen-settings write (ELISA/bead/flow) |

---

## 10. Update — module conn→pool standardization (done since §6 was written)

The `conn → pool` migration is now **complete for the calib access layer and the
two std_curve modules** (`std_curve_calc_module.R`, `std_curve_view_module.R`):
every scoped fetch is `(pool, project, study, experiment[, tbl])`, project-first,
named at call sites; `app.R` mounts both modules with `pool = db_pool`. So §6's
"~26 files still use conn" count is now lower — the two modules and the access
layer are off the list. The remaining `conn` users (import, dilution, study
overview, plate ops, etc.) are still deferred, still need `poolWithTransaction`
for the temp-table/transaction files.

Known-safe straggler: **`fetch_curve_batch`** (`calib_data_access.R:342`) is still
study-first (`pool, study, experiment, project_id, feature, antigen`) with
`IS NOT DISTINCT FROM $3`. Its one call site (`std_curve_calc_module.R:261`) is
study-first positional and *matches*, so it works — but it violates the
project-first convention. Unify it when convenient; low risk, not urgent.

`lookup()` final shape: SQL-filtered via `fetch_curve_lookup_scoped` (no R
re-filter), returns a column-shaped empty frame (`empty_lk()`, built from
`CALIB_NK_COLS`, no DB call) on the not-ready / error paths so the `fit_target`
consumer never hits `undefined columns selected`, and never calls the scoped
fetch on the empty path (which would trip its NA-project hard-stop).

**Hardened lesson (the expensive one this round):** a signature rename fixes the
*name* but NOT the *argument order*. `conn→pool` turned `f(conn, study, ...)` into
`f(pool, study, ...)` — pool-correct, order-still-broken — and it failed
**silently** (wrong column into an integer `$1`, or wrong rows with no error)
until the code path actually ran. After any signature change: grep every call
site by BODY pattern, convert to named args, and exercise the tab, because these
fail at click-time, not load-time.

---

## 11. View module — FORWARD WORK (new features, not refactor debt)

These are new analysis/feature work on `std_curve_view_module.R`, deferred
deliberately. Each is scoped from a real source pointer, with open questions
called out. NOT started.

### 11.1 Frequentist precision-profile grid laterally offset from sample points
**Symptom:** on the frequentist precision plot, the grid is x-translated from the
test-sample points; y-axis is correct.
**Diagnosis:** `fetch_calib_grid` (`calib_data_access.R:321`) selects
`predicted_concentration` (NOT dilution-scaled). `calib_samples` carries
`final_concentration` (dilution-scaled) and an unscaled `predicted_concentration`.
The two layers are plotted on different concentration scalings → lateral offset.
**Fix:** add a dilution-scaled concentration to the grid fetch/compute and plot
that; use the same scaled column for the curve plot x-axis too, so grid, points,
and curve share one scale. This is a fetch+compute change, not a pure plot tweak.
**COMPLETED - DONE**

### 11.2 Add LOQ shapes (lower + upper) to curve plot and the panel below it
**Data:** LOQ / shape-based LOQ come from the diagnostics row via
`calib_loq(diag, scale=)` (`calib_data_access.R:405-426`) — already fetched on the
view path (`fetch_calib_diagnostics`). LLOQ/ULOQ available on both `conc` and
`log10` scales; pick the scale matching the plot axis.
**Fix:** pure rendering add (overlay the LOQ bounds on the curve plot and the
sub-panel). No new fetch.
**COMPLETED - DONE**

### 11.3 Restore FDA 2018 standard-curve-point classification. **COMPLETED - DONE**
**Built method-agnostic (accuracy back-calc pinned to the frequentist fit),
displayed as colour-on-points on the Explore Curve plot — see §12 for the
as-built summary.** The original spec below is kept for history.

**Status:** the algorithm still EXISTS — `.extract_fda_loqs_from_dil_series`
(`std_curve_functions.R:1977`, called ~2641) populates `lloq_fda2018_*` /
`uloq_fda2018_*` glance columns from `compute_dil_series_accuracy()` (CV +
recovery on standards across plates). The older internal back-calc version
(`.compute_fda2018_scalars`, ~1081) is commented out — do NOT revive that one.
The old`*_fda2018_*` columns are not POPULATED in the new `calib_*` pipeline. 
So we need to create the dil-series accuracy step from the direct analysis of the standard curve points at known 
locations on the dilution series/concentration series that are comparable at each concentration level across the plates. 
This is all independent of the api/worker. Plot overlay (legacy) lives in `plot_functions.R`.
**COMPLETED - DONE**

### 11.4 Model-comparison sub-tab (new view in std_curve_view_module)
**Old source:** the previous model-comparison rendered in a MODAL — see
`std_curver_ui.R` (attached in-session). Port the comparison logic, drop the modal
for a proper sub-tab.
**Three comparison modes:** (a) multiple model forms, one plate; (b) frequentist
vs bayesian, one plate; (c) all plates together under one approach (freq or bayes).
**Each shows:** overlaid fits + precision curves, PLUS a forest plot comparing
parameters, one row per compared curve.
**Design specifications:** The top figure should contain overlapping standard curves.
The set of curves compared should be bounded by the scope of the comparison modes. 
The next plot should contain a CV% yaxis and a concentration x-axis where the cv% is based on the standard curve points compared at each concentration for the curve set defined by the scope of the comparison modes.
The third plot should contain a panel of small figures each with forest plots in rows for each of the comparison set.each panel contains forest plots for one parameter.
The set of parameters is the five model form parameters (a,b,c,d,g; always keep all five even if there are only four parameters for some comparison set), the upper and lower LOQs and the upper and lower shapeLOQs, the inflection point.
Maybe this could show up in two rows of five panels.
Some sets might be large and if > 12 curves compared, let the user choose which 12 curves to plot from the full set.
Parameter data comes from `fetch_calib_params` (per curve+method+
model); the forest plot needs estimate + CI per parameter (`q_lo/q_med/q_hi` for
bayes; std_error-derived for freq).
**COMPLETED - DONE**
**Built (`std_curve_compare_module.R`, mounted beside Explore/Compute):** the
unifying abstraction is "comparison set = list of fits `(curve_id, method, model,
label)`". A FOURTH mode was added — (d) **sources**: compare the source variants
of one curve position. Fitted curves are RECONSTRUCTED from stored params
(`calib_grid` holds only the best model). CV% is the cross-PLATE CV of observed
`assay_response_raw`, gated to the all-plates mode (blank elsewhere), Y broken at
30% with n-plate labels. Forest = 2x5 ggplot facets. Mode C caps the OVERLAY at 12
plates but CV%/forest always use ALL plates. A/B/C get a canonical Source selector
and Mode D resolves source variants, both via the new `source_alias` table; its
editor is a separate global sidebar tab (§9).

### 11.5 Tiered job-status polling + resume-after-login (Compute-fits)
**Need:** jobs can run a long time. The Compute-fits status should poll at
escalating intervals — every 10s for the first 2 min, then every 30s to 10 min,
then every 60s — and on login/return, report whether a prior job finished or is
still running.
**Buildable on existing API:** `compute_api_client` already exposes `get_job`,
`list_jobs`, `cancel_job`, `health` (`compute_api_client.R:32`). No API change —
this is app-side: a reactive timer with escalating `invalidateLater`/`reactivePoll`
intervals keyed off elapsed time, plus reading persisted job state (job_id +
status) on session start so a returning user sees in-flight/finished jobs. Needs a
place to persist the active job_id across sessions (calib_run already records jobs
by job_id/status — query it on load for this study/experiment).
**COMPLETED - DONE**

### 11.6 Refactor the Plate Label Editor tab and its components.
** Need: ** redesign this to annotate antigens, features, time periods, 
groups/arms with referent levels and store in a replacement for the 
xmap_antigen_families table.Move the Antigen family order and the 
antigen order and the time period and arm values out of the settings_cascade.
** Create a new UI not recycle the old ones (plate_management.R and antigen_family_ui.R) as it is mostly not plate based.**
Must not edit or rename any of the values of the curve_lookup natural key.

### 11.7 Fix the processes that split plates or subtract plates into different new experiments.
** Need ** ELISA experiments arrive with data in two wavelengths (450 and 620 nm) and we need to calculate the difference 450-620. 
A function does this and creates a new experiment with a |d suffix. 
But the curve_lookup table does not yet receive new rows/curve_ids corresponding to the new experiment. 
this is all in app.R, elisa_wavelength_subtraction.R, data_tab_module.R, plate_ops.R, and ui_handler.R
And then we sometimes have experiments with more than one Sample dilution. 
Identified by nominal sample dilution haveing more than one integer dilution 
factor listed or by a query of dilution in the xmap_samples_table.
There is a function that splits the experiment into two (or more ) experiments 
with all the resulting experiments having a single sample_dilution (split_plate_nominal_UI, . 
app.R, data_tab_module.R, import_lumifile.R, plate_ops.R, split_plates_nominal_sample_dilution.R, ui_handler.R .
Just like new ELISA experiments, each of these needs to fully populate the xmap_* tables and the curve_lookUp.
I'm discussing this here because there is much overlap between these functions and the shars of the process is spread out across too many files. . 
And we need to refactor and make them avaliable on the Data tab in the same location as they are currently, in more esily maintainable form.
**COMPLETED - DONE**

### 11.8 Create a universal mechanism to delete all study data. 
Right now there's a legacy and out of data method in the 
Plate Label Editor for deleting single plate data, and a link in the 
main app sidebar for deleting whole studies. These are not working 
and never had the scope of the current table structure 
(xmap_*, curve_lookup, calib_*). We need to rename the button on the main 
sidebar to Delete Study Components, and generalize to the deleting of single experiments and whole studies. no plate deletion.
Current methods in plate_management.R and import_lumifile.R and ui_handler.R
**COMPLETED - DONE**

### 11.9 correct the storage of the antigen feature settings.
The legacy import wrote antigen/feature settings into the mixed-use
`xmap_antigen_family` table, but the fitting worker reads settings ONLY from the
cascade (`resolve_settings_batch` -> `calib_settings`). `antigen_feature_settings`
is merely `SELECT ... FROM xmap_antigen_family`, and the worker's view-readers
(`.settings_map`/`fetch_sc_conc`) are dead code — so imported settings never
reached the fitter (`worker_curveR.R` errored "no standard_curve_concentration").
This split into a coordinated fix (11.9a) and a strategic reader refactor (11.10).
**COMPLETED - DONE**

**11.9a — coordinated import->cascade fix. COMPLETED - DONE.**
Shared helper `write_antigen_settings_to_cascade(conn, antigen_df, project, study,
experiment, user)` in `plate_validator_functions.R`: for each antigen it resolves
the `(feature, antigen)` pairs from the already-registered `curve_lookup` and
`set_setting()`s the four captured params — `standard_curve_concentration`,
`l_asy_min_constraint`, `l_asy_max_constraint`, `l_asy_constraint_method` — at full
ladder depth. Wired identically into ELISA (via `upload_antigen_family`), bead
(`import_lumifile.R`) and flow (`flowjo_reader.R`), each right after its existing
`xmap_antigen_family` write (which already runs after `register_curve_lookup`).
DUAL-WRITE bridge: the `xmap_antigen_family` write is kept for annotations / the
editor / the legacy view; the cascade write is additive and is the one the worker
reads. Runs on the import transaction conn (atomic with the import); no worker,
view, or `app.R` change. `set_setting` requires the param exist in
`calib_settings_meta`; each write is `tryCatch`-guarded so one bad param logs
rather than aborting the import.
**COMPLETED - DONE**

### 11.10 Assay import architecture — three assay modules over a shared reader/format contract
(Formerly the strategic half of 11.9.) The import/upload code is spread across many
files with no shared UI or interface standard: `elisa_reader.R` is pure parsers (UI
lives elsewhere), `flowjo_reader.R` is parsers + its own top-level `renderUI` import
UI, `xPonentReader.R` is parsers + some UI, `segment_reader.R` is a UI builder; and
the antigen-family write is done three ways (ELISA via the shared
`upload_antigen_family`, bead + flow via their own inline `insert_new_rows`). New
assays and new per-assay formats are coming (bead arrays also have a `*.rbx` format
whose reader is not yet integrated).

**Target architecture — THREE assay modules over a shared contract + shared backend:**
- **Three assay modules** — ELISA, bead array, flow cytometry — each a proper Shiny
  module with ONE UI standard (upload -> template -> validate -> preview -> commit).
- **Pluggable format readers behind a common contract.** Each file format implements
  `read(file) -> { long_data, plate_metadata, antigen_settings }`. The bead module
  then carries the xPonent-CSV and `.rbx` readers behind the same contract; new
  formats slot in without touching the assay UI.
- **One shared landing backend.** After parsing, a single routine (a) writes the
  `xmap_*` raw tables, (b) registers `curve_lookup` (`register_curve_lookup`), and
  (c) writes antigen settings to the cascade (reuse 11.9a's
  `write_antigen_settings_to_cascade`). 11.9a already carved this seam; 11.10
  generalises it.

**First concrete step (already scoped):** converge bead + flow's *xmap* antigen
write onto the shared `upload_antigen_family` (ELISA already uses it). This needs
`feature` threaded into the bead antigen list — `prepare_batch_antigen_family`
(`batch_layout_functions.R`) requires `feature`, but bead's `antigen_cols_needed`
omits it, which is exactly why bead inserts inline today. Bead antigens span
features (isotypes), so this turns bead's `xmap_antigen_family` rows from
per-antigen into per-(antigen,feature) — a deliberate semantic/data change.

**Constraints / invariants:**
- Never edit or rename any `curve_lookup` natural-key value (incl. `source`).
- Keep the 11.9a dual-write bridge until `xmap_antigen_family` is formally retired
  (settings already live in the cascade; annotations need a home first — see 11.6).
- `register_curve_lookup` before any cascade settings write, always.

**Source list:** `app.R`, `batch_layout_functions.R`, `db_functions.R`,
`elisa_diagnostic.R`, `elisa_reader.R`, `flowjo_read_functions.R`, `flowjo_reader.R`,
`generate_flowjo_layout_template.R`, `generate_layout_template_ref.R`,
`import_lumifile.R`, `plate_validator_functions.R`, `segment_reader.R`,
`xPonentReader.R`. Shared seams already in place: `upload_antigen_family`,
`prepare_batch_antigen_family`, `register_curve_lookup`,
`write_antigen_settings_to_cascade`, `insert_new_rows` / `insert_to_table`.

### 11.11 Clone a study (new project_id + study name). **COMPLETED - DONE**
Files: `clone_study_components.R` / `clone_study_components_ui.R` (LF), mirroring the
Delete-Study module; wired in `app.R` (sourced right after the delete module) and
`ui_handler.R` (`menuItem` + `tabItem` “Clone Study”). Reuses `.tbl()` /
`CALIB_RAW_TABLES` from `calib_data_access.R`.

**Shallow clone** (decided): copies the raw `xmap_*` tables, the `curve_lookup`
registry, and the study’s `calib_settings` into a NEW `(project_id, study)`; fitted
`calib_*` results are NOT copied — the clone is re-fit via Compute-fits. So there is
NO `curve_id` remap: every table is re-scoped by `(project_id, study_accession)`,
never by `curve_id`.

**Identity regenerated by the DATABASE, not the app** (no old->new maps needed):
- `curve_lookup.curve_id` is `GENERATED ALWAYS AS IDENTITY` → omitted on insert.
- `curve_lookup.multiplate_group_id` is set by the BEFORE-INSERT trigger
  `trg_set_multiplate_group_id` from the new scope columns → omitted on insert, so
  the clone gets fresh group ids grouped exactly as a fresh import would.
Columns are introspected from `information_schema`, dropping IDENTITY and serial
(`nextval`) auto-PKs so they reassign; `project_id`/`study_accession` are rewritten.

**`workspace_id` (legacy synonym of `project_id`)** is re-scoped to the new project
wherever the column exists (e.g. `xmap_header`) — and force-kept in the column list
even though its default is `nextval`, so it is SET, not sequence-assigned. Required
because the study list joins `xmap_header.workspace_id → xmap_users.workspace_id
WHERE workspace_id = userWorkSpaceID()`.

**Visibility (`xmap_users`).** The target project must ALREADY exist in
`madi_lumi_users.projects` (validated in Preview + the txn; its `project_name` is
fetched). The clone then inserts an idempotent `xmap_users (auth0_user =
currentuser(), project_name, workspace_id = newproj)` row so the study appears when
that project is loaded. Membership (`madi_lumi_users.project_users`) is established
at project CREATION (the creator becomes a member), so the clone does NOT grant it.
The `xmap_users` pointer may be rewritten by the next project-load `UPDATE`
(accepted: immediate visibility now; durable access via membership + load).

**Guards / validation:** target `project_id` a whole number in 16..32767 (the
`curve_lookup` CHECK is `>= 16`); `study_accession` non-empty and `<= 15` chars
(`varchar(15)`); collision refusal if the target `(project, study)` already holds
`curve_lookup`/`xmap_header` rows; everything in ONE transaction (rollback on error).

**Open item:** assumes `xmap_*` rows are self-contained by scope/natural key (no
surrogate-FK link between `xmap_*` tables) — validate with a test clone. If such a
link exists, that pair alone needs an explicit old->new id map.

---

## 12. Update — FDA 2018, Data-tab & Compare performance, Compare figure (done since §11)

Work completed after §11 was written. The module files below are the source of
truth; an earlier round LOST edits by re-basing on stale `uploads/` copies (see §8).

**FDA 2018 (11.3) — DONE.** New in `calib_data_access.R`: `.fda_inv_*` (curveRcore
analytic inverses, copied verbatim, `.fda_`-prefixed so they can’t mask the
package originals), `.fda_backcalc_conc`, `.fda_params`, `FDA2018_*` thresholds,
and `fda2018_classify_group(pool, curve_id)` (a fixed FOUR batched set-based reads,
was `3N` per-plate). It computes, per concentration level across the curve’s
multiplate group: cross-plate CV% (`SD/mean` of raw response) and median per-plate
recovery back-calculated through each plate’s FREQUENTIST fit; verdict against FDA
2018 LBA criteria (interior CV≤20% & recovery [80,120]; the two extreme levels
relaxed to 25% & [75,125]). A non-invertible response is FAIL_ACC, never NA; NA is
reserved for a level with no measured response. DISPLAY (in
`std_curve_view_module.R`): the verdict is painted onto the Explore Curve plot’s
standard points as COLOUR (Okabe-Ito, 4 categories + grey NA), SHAPE = mask state
(filled = in fit, hollow = masked), staged-to-mask ring unchanged; a one-line
caption carries LLOQ/ULOQ + n-pass. Frequentist-pinned so it’s identical on both
method tabs; falls back to the plain fit/masked scheme when no frequentist fit
exists. No ribbon (a ggplot strip can’t be pixel-aligned under a plotly plot).

**Masking crash fix (`std_curve_view_module.R`).** The Curve plot’s `plotly_click`
observer had a stray `event_register("plotly_click")` — a string where plotly wants
the plot object — that crashed every mask click (`$ operator is invalid for atomic
vectors`). Registration already happens once on the plot in the render; the observer
now just reads `event_data`, `tryCatch`-guarded.

**Performance.**
- Progress feedback on Explore & Compare: `shinycssloaders::withSpinner` on every
  plot + staged `shiny::Progress` messages in the heavy renders.
- **SC selector**: `fetch_curve_lookup` gained optional `(project, study,
  experiment)` scope (backward-compatible: no args = whole registry). It used to
  pull the ENTIRE `curve_lookup` and filter in R.
- **Data tab (`data_tab_module.R`)**: the eager “load all 14 scoped tables” snapshot
  is now LAZY — only the raw frames + `curve_lookup` load up front (the plate-ops
  contract); the `calib_*` Results tables load on first view (Shiny suspends hidden
  outputs). Per-table load timing prints to the console. The status “empty tables”
  hint uses one server-side count probe (`fetch_scoped_table_counts`, a single
  `UNION ALL`) instead of materialising every Results table.
- **`calib_grid` (132k × 28, ~40s):** `EXPLAIN ANALYZE` = 0.3s — the cost is column
  WIDTH over the VPN + `numeric` parsing, not the query. The interactive preview is
  capped (`fetch_calib_grid_scoped(..., display_limit = 5000)`) with a “first N of
  M” caption; CSV / RData / JSON exports still pull the full table. Dropped the
  redundant `ix_calib_grid_curve` (PK already leads with `curve_id`); added
  `ix_curve_lookup_scope (project_id, study_accession, experiment_accession)`.

**Compare figure overhaul (`std_curve_compare_module.R`; this file is LF).** The two
ggplot panels (overlay + CV%) were replaced by ONE plotly figure ported from
`summarize_sc_fits_plotly` (`std_curver_summary_functions.R`): per-plate curves
(Kelly palette, dashed by model form) on log10 axes with plain natural decade
labels (no scientific/exponent); in “plates” mode ONLY, cross-plate CV% on a right
`y2` axis (capped at 30%, points above labelled with their true value via a text
trace) and a “Dilution Factor” `x2` axis on top. The module’s ORIGINAL CV%
computation is preserved as a `cv_summary()` reactive feeding both the figure and
the downloads. Modes reordered so “All plates” is first + default. RData/JSON
download of the CV table (full `curve_lookup` NK + `curve_id` +
concentration/dilution/n_plates/cv_percent). The 12-plate overlay cap + its member
picker were RETIRED — the overlay now draws every curve in the group. The x-axis is
trimmed to the next log10 decade at/above the most concentrated standard point.
