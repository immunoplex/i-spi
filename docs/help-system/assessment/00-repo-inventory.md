Title: Phase 0 — Repository & Environment Inventory
Status: Draft for review
Date: 2026-10-09
Scope: §3 of `dev/ISPI_HELP_SYSTEM_ASSESSMENT_PLAN.md`

---

## 0. Key findings — read this first

These are discrepancies between the assessment plan's assumptions and what the repositories actually contain. Per the plan's own instruction (§0), these are flagged rather than silently reconciled.

### 0.1 A help-content engine already exists (not mentioned in the plan)

`i-spi-refactor/src/help_utils.R` is a working, in-use **concept-keyed help/docs engine**, currently scoped to the settings cascade only:

- **Storage:** one markdown file per concept under `src/help/settings/<id>.md`, with YAML frontmatter — `id`, `title`, `audience` (`user`/`dev`/`both`), `params` (list of settings params the concept covers), `references` (list of `{text, doi|url}`).
- **Content is concept-keyed, not param-keyed** — one note can cover several related params (e.g. `precision_measurement_error.md` covers both `include_measurement_error` and `pcov_threshold`).
- **Mechanism:** a clickable info icon (`settings_help_icon()`) next to a labeled control fires a namespaced Shiny input, which opens a `shiny::modalDialog` (`settings_help_content()`) rendering the markdown body + a formatted references list.
- **Why a modal, not a popover** — this is stated directly in a code comment: *"The app is a shinydashboard (Bootstrap 3) page, so a `bslib::popover` (Bootstrap 5) would render but never activate."* This independently confirms the modal-popup decision already made for Phase 4 §7.2 (§10.1 of the plan) — it isn't just a preference, it's a hard technical constraint of this app's Bootstrap version.
- **Two existing content notes:** `apply_prozone.md`, `precision_measurement_error.md`. The latter is genuinely well-written, citation-backed, Lancet-adjacent prose already — see it as the style bar for Phase 2 content authoring and for filling vignette gaps per §10.5's resolution.
- **What it does *not* yet have:** `see_also` cross-links, a `category` classification (procedural/conceptual/glossary/compute-decision), and the audience tag is binary per-note rather than the "one neutral explanation + expandable more detail" model just decided for §10.2 — i.e., today a note is either shown or not shown per audience, it doesn't progressively disclose within one note.

**Implication for Phase 4:** the architecture recommendation should be framed as *generalizing `help_utils.R` from settings-only to app-wide*, not designing a new mechanism from scratch. This also means Phase 4's §7.2 "evaluate trade-offs" framing is partly moot — the modal-via-click-icon approach is already proven correct for this app's Bootstrap constraint; the open design work is schema extension (`see_also`, `category`, expandable detail) and generalizing `load_help()` beyond `help/settings/`.

There is also an `src/references/` directory containing existing source material: two `.docx` guides (`2026_01_14_I-SPI_Batch_Workflow.docx`, `2026_01_14_I-SPI_Guide.docx`), two assay-method PDFs, and an `FDA2018_protocol.Rmd`. These are separate from the Zotero-based reference workflow resolved in §10.3, but should be checked for overlap/supersession before Phase 2 treats Zotero as the sole external-reference source.

### 0.2 "The i-spi repo" is ambiguous — ~18 candidate directories, 3 different git remotes

The plan assumed a single `i-spi` repo. The actual `R-git` directory contains many: `i-spi`, `i-spi-add-fcuploader`, `i-spi-bug-upload`, `i-spi-calc-MDC-RDL`, `i-spi-elisa-curves`, `i-spi-fix_bayes`, `i-spi-fix_study`, `i-spi-inspect`, `i-spi-loader2026` (a–f), `i-spi-nominal-a`, `i-spi-refactor`, `i-spi-upload`, `i-spi_add_flowjo`, `i-spi_CDAN`, `i-spi_modify_curveR`.

**Resolved (high confidence, proceeding on this basis):** `i-spi-refactor` is the canonical/active app repo.
- Remote `https://github.com/immunoplex/i-spi.git` — shared by the majority of the other clones.
- Last commit **2026-10-08** — by far the most recent (next-closest is `i-spi-add-fcuploader` at 2026-04-22).
- Clean working tree.
- Contains the assessment plan itself (`dev/ISPI_HELP_SYSTEM_ASSESSMENT_PLAN.md`).

Everything else in this assessment proceeds against `i-spi-refactor`. Flagged for your awareness, not blocking:
- Plain `i-spi/` points to a **different** remote entirely (`dartmouth/madi-shiny-group-madi-lumi-reader.git`), last commit 2025-07-17, dirty (24 uncommitted changes). Looks like a stale/legacy clone under an older remote name, not the current repo.
- `i-spi-bug-upload`, `i-spi-calc-MDC-RDL`, `i-spi-elisa-curves`, `i-spi-fix_study`, `i-spi-loader2026*`, `i-spi-nominal-a`, `i-spi-upload` all point to `immunoodle/i-spi.git` (yet another remote/org), last commits ranging 2025-08 to 2026-03. These read as old per-feature working clones (one directory per branch, rather than git branches in one clone) — likely safe to ignore for this assessment, but **confirm before I treat them as irrelevant**, in case any contains merged-nowhere work still needed.
- `i-spi_CDAN` is not a git repo at all (no `.git`).

### 0.3 The curveR ecosystem is six packages, not four — and `curveR` itself is a meta-package

The plan named four packages (`curveRcore`, `curveRfreq`, `curveRbayes`, `curveRweights`). Actual inventory:

| Name | Role | In original plan? |
|---|---|---|
| `curveRcore` | Foundation: forward models (4PL/5PL/loglogistic/Gompertz), inverses, derivatives, shared `calibration_result` S3 class | Yes |
| `curveRfreq` | Frequentist NLS ensemble fitting (AIC model selection) | Yes |
| `curveRbayes` | Bayesian hierarchical fitting via Stan (logistic/Gompertz/Richards, LOO-CV) | Yes |
| `curveRweights` | Bayesian precision weighting from calibration-curve uncertainty | Yes |
| `curveRmetrics` | **New** — analytical QC metrics for fitted curves, consuming output from curveRfreq **and `stanassay`** (see below) | No — must add to Phase 1/2 scope |
| `curveR` | **Meta-package only** — installs/attaches curveRcore + curveRfreq as hard deps, curveRbayes + curveRweights as optional Stan-dependent extras. No independent statistical content. | No — not separate content; exclude from the topic index as a source, but its startup message/vignette may be a reasonable entry point to link from the app's own "about the stats" page |

**Flag:** `curveRmetrics`'s `DESCRIPTION` says it computes metrics "from the frequentist package curveRfreq and the Bayesian package **stanassay**" — not `curveRbayes`. `stanassay` is a separate repo in `R-git/` (remote `hardikguptadartmouth/stanassay.git`, last commit 2026-03-27, different GitHub org than the `immunoplex` packages). This looks like `curveRbayes` superseded `stanassay` as the actively maintained Bayesian package, with `curveRmetrics`'s doc text not yet updated — the same class of documentation drift the plan already anticipated for the deployment docs. **Needs verification**, ideally from you: is `stanassay` fully retired in favor of `curveRbayes`, or still live for some purpose `curveRmetrics` actually depends on? This affects whether Phase 1/2 need to index `stanassay`'s docs at all.

Also note `curveRfreq_clone2` (remote = `curveRfreq`, dirty, last commit 2026-05-15) and `curveRweights_set_docs` (remote = `curveRweights`, dirty, last commit 2026-05-08) are duplicate scratch clones of packages already covered above — excluded from the inventory as redundant, but flagged in case either holds uncommitted work the canonical clone doesn't.

| Package | Vignettes | Man pages | Bib/reference files | Working tree |
|---|---|---|---|---|
| curveRcore | `getting-started.Rmd`, `model-forms.Rmd` | 80 | `vignettes/core_references.bib` | dirty (4 changes) |
| curveRfreq | `frequentist-quickstart.Rmd` | 14 | none found | dirty (1 change) |
| curveRbayes | `bayesian-quickstart.Rmd` | 22 | `vignettes/references.bib` | clean |
| curveRweights | `precision-weighting.Rmd` | 14 | none found | clean |
| curveRmetrics | `Workflow.Rmd` | 28 | none found | clean |

All five are actively developed (`main` branch, last commits 2026-06-09 through 2026-10-04). `curveRcore` and `curveRfreq` currently have uncommitted local changes — not a problem for reading/indexing, but Phase 2's topic index should note it's indexing a point-in-time snapshot that may shift.

### 0.4 The deployment repo is `deploy_ispi` (remote: `i-spi-deployment`)

Confirmed by content match: `ARCHITECTURE.md`, `architecture.svg`, `deployment-order.svg`, `K3S.md`, `OFFLINE-IMAGES.md`, `README-STANDALONE-ISPI.md`, `TEST-DEPLOYMENT-CIVO.md`, `TEST-DEPLOYMENT-LOCAL.md` all present at top level, exactly matching the plan's description. Also present: `auth-sequence.svg` (not mentioned in the plan — a third diagram to check for staleness in Phase 3), `batch-calculator.yml`, and a `k8s-manifests/` directory (contents not yet enumerated — Phase 3 should look here for the actual `i-spi.yml` state the plan wants reconciled against `ISPI_COMPUTE_URL`/`ISPI_COMPUTE_API_KEY`).

Note: a working copy of `i-spi.yml` also exists at `i-spi-refactor/dev/i-spi.yml`, alongside copies of `i-spi-compute.k8s.yaml` and `i-spi-compute-sim.k8s.yaml` — Phase 3 needs to determine which copy (here vs. `deploy_ispi/k8s-manifests/`) is authoritative before editing.

### 0.5 `std_curve_calc_module.R` lives in the app repo, not `i-spi-compute`

The plan's §3.3 lists `std_curve_calc_module.R` under the i-spi-compute inventory ("the VERBOSE liveness diagnostic box"). It actually lives in `i-spi-refactor/src/std_curve_calc_module.R` — it's the app-side module that submits fits and polls status ("the CALCULATE side"), not compute-tier code. Compute tier confirmed to contain `worker_curveR.R`, `supervisor.py`, `app.py`, plus `worker_weights.R` (a **second, dedicated worker script for curveRweights** — not mentioned in the plan, confirms precision weighting has its own compute path separate from the main curve-fitting worker).

Import confirmation (`grep library(curveR`):
- `worker_curveR.R` → `library(curveRcore); library(curveRfreq); library(curveRbayes)`
- `worker_weights.R` → `library(curveRweights)` (plus DBI/RPostgres/jsonlite)

### 0.6 Dilution-analysis and outlier-detection UI files are in `dev/`, not `src/`

The plan's §3.4 expects `dilution_analysis_ui.R`, `dilutional_linearity_ui.R`, and `outliers.R` in the app's live source tree. They are **not** in `i-spi-refactor/src/`; files with these and related names (`dilution_analysis_ui.R`, `dilution_analysis_parameters_ui.R`, `dilution_standards_controls_ui.R`, `dilution_linearity_functions.R`, `revised_dilution_analysis_functions.R`, `outlier_ui1.R`, `outliers.R`, plus subgroup-detection files) exist only in `i-spi-refactor/dev/` — the scratch/working-notes folder, alongside handoff docs and shell scripts, not the live Shiny module tree.

`src/` does contain `plate_dilution_series_module.R`, which a code comment elsewhere (`std_curve_weights_module.R`) describes as covering both "Explore fits" and "Compute fits" dilution views — this may be the current, shipped dilution module that superseded the `dev/`-resident files, or the `dev/` files may be in-progress work staged before merge. **Needs your confirmation** before Phase 1 enumerates the Data tab's dilution/outlier sub-tabs, since the answer determines whether `plate_dilution_series_module.R` or the `dev/` files are the ones to document.

### 0.7 `assay_std_reference_ui.R` has landed

The plan's §4.1 asks to confirm whether the `assay_std_reference_ui.R` / std-reference stage (from `RBX_DILUTION_AUTHORITATIVE_SOURCE_PLAN.md`) has shipped or is still planned. Confirmed: `assay_std_reference_ui.R` and `assay_std_reference_rules.R` both exist in `src/`, alongside a passing test file `test-assay-std-reference-rules.R`. Treat it as landed for Phase 1.

---

## 1. Repo inventory table

| Repo (canonical) | Path | Remote | Branch | Last commit | Working tree | Top-level layout |
|---|---|---|---|---|---|---|
| i-spi (app) | `i-spi-refactor/` | immunoplex/i-spi.git | main | 2026-10-08 | clean | `.github/`, `dead/`, `dev/`, `src/`, Dockerfile, docker-compose.yml, CONTRIBUTING.md, README.md, RBX_DILUTION_AUTHORITATIVE_SOURCE_PLAN.md, WORKER_PARALLELISM_NOTES.md |
| i-spi-compute | `i-spi-compute/` | immunoplex/i-spi-compute.git | main | 2026-10-05 | clean | `api/`, `db/`, `dev/`, `worker/`, docker-compose.yml, DEPLOYMENT.md, SECRETS.md, `.k8s.yaml` x2 |
| curveRcore | `curveRcore/` | immunoplex/curveRcore.git | main | 2026-10-04 | **dirty (4)** | `R/`, `data/`, `data-raw/`, `man/`, `vignettes/`, `tests/`, `doc/`, `docs/`, `pkgdown/`, DESCRIPTION, NEWS.md |
| curveRfreq | `curveRfreq/` | immunoplex/curveRfreq.git | main | 2026-09-26 | **dirty (1)** | same shape as curveRcore |
| curveRbayes | `curveRbayes/` | immunoplex/curveRbayes.git | main | 2026-09-26 | clean | `inst/` instead of `doc/`; otherwise same shape |
| curveRweights | `curveRweights/` | immunoplex/curveRweights.git | main | 2026-10-04 | clean | adds `.claude/`; otherwise same shape |
| curveRmetrics | `curveRmetrics/` | immunoplex/curveRmetrics.git | main | 2026-06-09 | clean | `R/`, `inst/`, `man/`, `vignettes/`, `docs/`, no `tests/` |
| curveR (meta) | `curveR/` | immunoplex/curveR.git | main | 2026-06-07 | clean | meta-package only, see §0.3 |
| deployment | `deploy_ispi/` | immunoplex/i-spi-deployment.git | main | 2026-07-06 | **dirty (7)** | `db-dumps/`, `k8s-manifests/`, `templates/`, ARCHITECTURE.md + 3 `.svg`, 5 deployment `.md`s |

## 2. curveR package inventory

| Package | Title | Vignettes | Man pages (exported docs) | Bib/reference files | Apparent scope |
|---|---|---|---|---|---|
| curveRcore | Shared Model Mathematics for Immunoassay Calibration Curves | getting-started, model-forms | 80 | `core_references.bib` | Foundation: 5 forward models + inverses/derivatives, `calibration_result` S3 class, eligibility gating, LOD/LLOQ/ULOQ |
| curveRfreq | Frequentist Calibration Curves for the curveR Suite | frequentist-quickstart | 14 | none | Multi-start NLS ensemble (4PL/5PL/Gompertz), AIC selection, delta-method uncertainty |
| curveRbayes | Bayesian Immunoassay Standard Curve Fitting | bayesian-quickstart | 22 | `references.bib` | Stan hierarchical fitting (logistic/Gompertz/Richards), LOO-CV, posterior predictive concentration |
| curveRweights | Bayesian Precision Weighting from Calibration Curve Uncertainty | precision-weighting | 14 | none | Joint Bayesian location-scale model → per-observation precision weights |
| curveRmetrics | Quality Metrics for Standard Curves from curveRfreq and stanassay | Workflow | 28 | none | Analytical QC metrics for fitted curves (see §0.3 stanassay flag) |

`grep -rl "@references" R/` / `@examples` per package not yet run — deferred to Phase 2's topic index, where it's directly actionable (it determines which functions already carry citable statistical references vs. need one added).

## 3. i-spi-compute inventory

Confirmed present, matching `WORKER_PARALLELISM_NOTES.md`:
- `worker/worker_curveR.R` — imports curveRcore, curveRfreq, curveRbayes
- `worker/supervisor.py`
- `api/app.py`
- `worker/worker_weights.R` — **not in original plan**; dedicated worker importing curveRweights + DBI/RPostgres/jsonlite
- `db/calib_schema_v1.sql`

Not found here (see §0.5): `std_curve_calc_module.R` — lives in the app repo instead.

## 4. i-spi app inventory (file existence check only — full tab/control enumeration is Phase 1)

All of the following, named in the plan or in prior docs reviewed, **confirmed present in `src/`**: `import_lumifile.R` *(not found — see note)*, `combined_plates.R` *(not found)*, `bead_count_functions.R`, `standard_curve_ui.R` *(not found — see note)*, `assay_description_parse.R`, `assay_description_rule_ui.R`, `assay_plate_grid.R`, `assay_shape_ui.R`, `assay_shape_rules.R`, `assay_import_module.R`, `descriptor_flow.R`, `calib_data_access.R`, `reader_bead_rbx.R`, `compute_api_client.R`.

**Three names from the plan were not found verbatim in `src/`** — flag for Phase 1 to resolve by function rather than filename:
- `import_lumifile.R` — not present; `reader_bead.R`, `reader_bead_rbx.R`, `reader_bead_xponent_parsers.R`, `reader_elisa.R`, `reader_elisa_parsers.R`, `reader_flow.R` exist and likely cover this role, split by assay type rather than one dispatcher file.
- `combined_plates.R` — not present under that name; candidate equivalents not yet identified, needs a Phase 1 grep pass.
- `standard_curve_ui.R` — not present under that name; `std_curve_calc_module.R`, `std_curve_compare_module.R`, `std_curve_view_module.R`, `std_curve_weights_module.R` exist and almost certainly supersede a single monolithic file.

**Precision-weights UI file** (flagged in the plan as unconfirmed): **confirmed** — `precision_weight_panel.R`.

**Additional file not named anywhere in prior docs, clearly part of the help-candidate family:** `settings_cascade_access.R` / `settings_cascade_ui.R` (the settings cascade `help_utils.R` is built for), `study_configuration.R` / `study_configuration_ui.R`, `clone_study_components.R`, `delete_study_components.R`, `compute_cluster_registry.R` (relevant to Phase 3's cluster/replica question), `blank_control_ui.R`, `curve_lookup_functions.R`.

**Internal docs already in `dev/` directly useful as authoritative source material for Phase 1/2** (not content to write from scratch, content to *link*): `UNDERSTANDING_precision_and_measurement_error.md`, `HANDOFF_precision_weighting.md`, `REPORT_precision_gap.md`, `ASSAY_IMPORT_POLICIES.md`, `REFACTOR_settings_cascade.md`, `REFACTOR_wiring_std_curve.md`, `REFACTOR_11.10_assay_import.md`, `calib_tables_data_dictionary.md`.

---

## Acceptance criterion check

Per the plan's §3: every repo has a filled-in inventory table (✅ above), and every file name used in the plan has been confirmed to exist or flagged as renamed/missing (✅ — three app files flagged in §4, `std_curve_calc_module.R` relocated in §0.5, curveR package count corrected in §0.3).

## Open items — resolved 2026-10-09

1. **Canonical repo:** confirmed `i-spi-refactor`. All other `i-spi-*` directories are out of scope for this assessment.
2. **`stanassay` vs. `curveRbayes`:** `stanassay` is **out of scope — ignore it**. `curveRmetrics`'s DESCRIPTION text naming it is stale/drift, as suspected; `curveRbayes` is the active Bayesian package.

   **Amended 2026-10-09: `curveRmetrics` is now also out of scope.** The curveR ecosystem for this assessment is **`curveR`, `curveRcore`, `curveRfreq`, `curveRbayes`, `curveRweights` only** — the original four packages plus the `curveR` meta-package, matching the plan's original assumption. `curveRmetrics` was added to scope earlier in this assessment (§0.3 below, Phase 2's topic index, and Phase 5's mapping table) and has since been removed by your direction; those sections are left as a factual record of what was found rather than rewritten, but `curveRmetrics` content should not be carried forward into Phase 2 (content authoring). Any help entry that would have cited a `curveRmetrics` function (e.g. the LOQ/MDC/RDL detection-limit family) should cite `curveRcore::compute_detection_limits()` instead — see Phase 2's §0.6 for the overlap this resolves.
3. **`curveR` meta-package:** **include it** — it's the ecosystem overview that lays out how the other five packages relate. Useful as the entry point / "about the statistics" link for the help system, not as its own source of statistical content.
4. **`plate_dilution_series_module.R` vs. `dev/` dilution & outlier files:** does **not** supersede them — they're genuinely separate. However, **dilution analysis and outlier detection (`dilution_analysis_ui.R`, `dilutional_linearity_ui.R`, `dilution_analysis_parameters_ui.R`, `dilution_standards_controls_ui.R`, `dilution_linearity_functions.R`, `revised_dilution_analysis_functions.R`, `outlier_ui1.R`, `outliers.R`, and the subgroup-detection files) are currently **not user-facing** and need a major refactor before they're used again. **Phase 1 should exclude these from the live UI inventory** — do not plan help content for them yet; they are not part of the running app today. Revisit once/if they're refactored back in.
5. **`i-spi.yml` / k8s manifest authority:** the copies under **`i-spi-refactor/dev/`** (`i-spi.yml`, `i-spi-compute.k8s.yaml`, `i-spi-compute-sim.k8s.yaml`) are **authoritative**. The copies in `deploy_ispi/k8s-manifests/` are the stale ones Phase 3 needs to reconcile/replace, not the source of truth.
6. **`std_curve_calc_module.R`:** confirmed app-side (§0.5 stands as written).
7. **i-spi-compute's two workers, confirmed:** `worker_curveR.R` fits standard/calibration curves (curveRcore + curveRfreq + curveRbayes); `worker_weights.R` fits precision weights (curveRweights). Both are therefore in scope as the compute-decision targets for Phase 1's deep-dive.
8. **Phase 4 framing:** proceeding on the basis that Phase 4 generalizes/extends `help_utils.R` rather than designing a new mechanism from scratch (§0.1) — not separately contested, treated as confirmed by proceeding.

Proceeding to Phase 1 on this basis.
