# Phase 1 — UI Inventory

Status: Draft for review
Date: 2026-10-09
Scope: §4.1 of `dev/ISPI_HELP_SYSTEM_ASSESSMENT_PLAN.md`

Walked from `src/ui_handler.R` (main sidebar) down through every module it mounts. "Help type" is one of **procedural** / **conceptual** / **glossary** / **compute-decision** per the plan's definitions. Compute-decision rows get a full deep-dive in `01a-decision-points.md`.

**Excluded from this inventory, per user direction (2026-10-09):** dilution analysis and outlier detection (`dilution_analysis_ui.R`, `dilutional_linearity_ui.R`, `dilution_analysis_parameters_ui.R`, `dilution_standards_controls_ui.R`, `dilution_linearity_functions.R`, `revised_dilution_analysis_functions.R`, `outlier_ui1.R`, `outliers.R`, `subgroup_detection_ui.R`, `subgroup_function.R`, `subgroup_summary_functions.R`, all in `dev/`). Confirmed in `ui_handler.R` (~lines 510–542): their UI entry point, the "Advanced Diagnostics" `tabPanel` under Quality Control, is entirely commented out — `radioGroupButtons` choices `Dilution Analysis`, `Dilutional Linearity`, `Outliers`, `Subgroup Detection`, `Subgroup Detection Summary` and all five `conditionalPanel`s are dead in the live UI. Not user-facing; needs a major refactor before reuse; excluded from help planning until that happens.

---

## Main navigation

| Tab | Sub-tab | Source file(s) | Controls/inputs present | Help candidate? | Help type | Notes |
|---|---|---|---|---|---|---|
| (home) | Home page | `ui_handler.R` (`landing_page_ui`) | none (static) | N | — | Static welcome/quick-start text already present; not a help-system target. |
| Manage Project | Create New Project | `ui_handler.R` (`manage_project_ui`) | `textInput(project_name)`, `actionButton(create_project)` | Y | procedural | |
| Manage Project | Add New Project | `ui_handler.R`, `user_management.R` | `textInput(project_id)`, `textInput(access_id)`, `actionButton(add_project)` | Y | procedural + glossary | `access_id` must be a UUID (regex-validated against `project_access_keys`); "access key" / "project sharing" is a glossary term worth defining here. |
| Manage Project | Load Existing Project | `ui_handler.R` (`load_ui`), `user_management.R` | DT tables (owned / accessible projects), 2× `actionButton(execute_project_button*)` | N | — | Self-explanatory table-select-and-load pattern. |

## Import Plate Data (`import_tab`)

Mounted via `assay_import_mount.R` → one `tabPanel` per descriptor, each running the generic 9-step wizard in `assay_import_module.R`.

| Tab | Sub-tab | Source file(s) | Controls/inputs present | Help candidate? | Help type | Notes |
|---|---|---|---|---|---|---|
| Import | Experiment name (shared, above the tabset) | `assay_import_mount.R` | `selectizeInput(readxMap_experiment_accession_import)` | N | — | |
| Import | Bead Array | `descriptor_bead.R`, `assay_import_module.R` | `fileInput(raw_files)`, `textInput(feature_value)`, `actionButton(parse_btn)` | Y | procedural | |
| Import | ELISA | `descriptor_elisa.R`, `assay_import_module.R` | `fileInput(raw_files)`, `actionButton(parse_btn)` | Y | procedural | No assay-specific controls — feature is read from the plate_map sheet. |
| Import | Post-gating Flow Cytometry | `descriptor_flow.R`, `assay_import_module.R` | `fileInput(raw_files)`, `numericInput(n_wells)`, `textInput(feature_value)`, `checkboxInput(combine_experiment)`, `actionButton(parse_btn)` | Y | procedural | **Flag:** `descriptor_flow.R` currently sets `preprocess = TRUE`, so Flow now goes through the same pre-processor stages (steps 2–4 below) as Bead/ELISA. `RBX_DILUTION_AUTHORITATIVE_SOURCE_PLAN.md` §7 says "Flow format has no `preprocess` flag... out of scope" — that statement is **stale**; verify with the user whether Flow's pre-processor path has been smoke-tested the way Bead/ELISA have. |
| Import (per assay, step 2) | Confirm the plate layout | `assay_plate_grid.R` (`ai_plate_grid_ui`) | plate-grid editor (specimen type + description per well) | Y | procedural | |
| Import (per assay, step 3) | Standards dilution reference | `assay_std_reference_ui.R`, `assay_std_reference_rules.R` | editable DT (description → dilution), Save gate | Y | **compute-decision** | See deep-dive §1 in `01a-decision-points.md`. Landed per plan's open question — confirmed shipped. |
| Import (per assay, step 4) | Configure the description field | `assay_shape_ui.R`, `assay_shape_rules.R` | per-shape token binding editor (slot / pattern / constant / from_type / ignore), coverage banner | Y | **compute-decision** | This, not `assay_description_parse.R`, is the live description-parsing engine. See deep-dive §1. |
| Import (per assay, step 5–9) | Template download / upload / validation / preview / commit | `assay_import_module.R`, `assay_import_backend.R`, `assay_import_contract.R` | `downloadButton`, `fileInput(layout_file)`, DT issues table, `actionButton(commit)` | Y | procedural | Standard wizard mechanics; one glossary-ish note on what "validation errors vs. warnings" means would help. |

## View, Process, and Export Data (`view_files_tab`) → Experiments → Data

Driven entirely by `data_dictionary.R`'s `CALIB_TABLE_ORDER`; every label/description below is the literal `table_doc()` text, not paraphrased.

| Tab | Sub-tab | Source file(s) | Controls/inputs present | Help candidate? | Help type | Notes |
|---|---|---|---|---|---|---|
| Data → Raw inputs | Plates | `data_tab_module.R`, `data_dictionary.R` | DT view, CSV download, plate-ops hooks (`header_actions`, split, wavelength subtraction) | Y | procedural + glossary | "One row per study/experiment/plate" — plate-ops actions (split optimization plates, wavelength subtraction) are themselves decision points worth a procedural note. |
| Data → Raw inputs | Standard | `data_tab_module.R` | DT view, CSV download | Y | glossary | "Calibration/standard-curve points: the reference dilution series used to fit each curve." |
| Data → Raw inputs | Control | `data_tab_module.R` | DT view, CSV download | Y | glossary | "Positive/negative control wells run alongside the samples." |
| Data → Raw inputs | Blank | `data_tab_module.R` | DT view, CSV download | Y | glossary | "Blank/buffer wells used for background estimation." |
| Data → Raw inputs | Sample | `data_tab_module.R` | DT view, CSV download | Y | glossary | "The test (patient) samples measured against the standard curve." |
| Data → Registry | Curve lookup | `data_tab_module.R` | DT view, CSV download | Y | conceptual | "The stable registry of calibration curves... everything in Results joins back here." Good anchor point for explaining the 10-column natural key / `curve_id` concept. |
| Data → Results | Run | `data_tab_module.R` | DT view, CSV download | Y | procedural | "One row per compute job: which engine/version ran, parameters, status, timing." |
| Data → Results | Fit / model selection | `data_tab_module.R` | DT view, CSV download | Y | **compute-decision** | "Every candidate model fitted... the winning model is the `is_best` row." Ties directly to model-form selection, deep-dive §2. |
| Data → Results | Parameters | `data_tab_module.R` | DT view, CSV download | Y | conceptual | Fitted parameter estimates with uncertainty per model term. |
| Data → Results | Eligibility gates | `data_tab_module.R` | DT view, CSV download | Y | conceptual | "Pass/fail checks that decide whether a fitted model is eligible for selection" — directly explains the quantification-eligibility gating curveRfreq's DESCRIPTION mentions. |
| Data → Results | Fitted grid | `data_tab_module.R` | DT view, CSV download | Y | conceptual | The dense fitted-curve plotting series incl. the pcov QC series — links to the precision-profile concept already documented in `help/settings/precision_measurement_error.md`. |
| Data → Results | Back-calculated samples | `data_tab_module.R` | DT view, CSV download | Y | conceptual | "Replaces the old Sample QC tab" — worth a note on why/what changed for users who remember the old tab. |
| Data → Results | Diagnostics / LOQ | `data_tab_module.R` | DT view, CSV download | Y | **compute-decision / glossary** | LLOQ/ULOQ, LOD, RDL, inflection point — core glossary terms, each tied to a specific curveRcore function (`assess_model_eligibility()` family). |
| Data → Results | LOO comparison | `data_tab_module.R` | DT view, CSV download | Y | conceptual | "Bayesian leave-one-out model comparison... empty for frequentist, which selects by AIC." Explains why this table is sometimes empty — a likely support question. |
| Data → Results | Precision weights | `data_tab_module.R` | DT view, CSV download | Y | **compute-decision** | curveRweights per-sample weights (sigma/w/w_norm). Deep-dive §4. |
| Data → Results | Precision weights fit | `data_tab_module.R` | DT view, CSV download | Y | **compute-decision** | One curveRweights fit per multiplate group × method (phi/beta1). Deep-dive §4. |

## View, Process, and Export Data → Experiments → Quality Control - Basic

`radioGroupButtons(qc_component)`: Bead Count / Plate Dilution Series / Standard Curve / Precision Weights.

| Tab | Sub-tab | Source file(s) | Controls/inputs present | Help candidate? | Help type | Notes |
|---|---|---|---|---|---|---|
| QC - Basic | Bead Count | `bead_count_analysis_ui.R`, `bead_count_controls_ui.R`, `bead_count_functions.R` | `numericInput(lower_threshold_val)`, `numericInput(upper_threshold_val)`, `radioButtons(thresholdCriteria_val)` | Y | conceptual | **Resolved, not compute-decision**: `bead_count_gc`/`is_low_bead_count` (the pass/fail flag these thresholds drive) is referenced only in `bead_count_analysis_ui.R` — nowhere in the fit-submission path or any well-exclusion gate. It's a QC *display* flag the user interprets, not an input to curve fitting today. Still ELISA-gated (`output.exp_assay_type_js == 'bead_assay'`; ELISA shows `bead_not_available_ui` instead) — a procedural note explaining that gating is warranted. |
| QC - Basic | Plate Dilution Series | `plate_dilution_series_module.R` | plate/source/analyte faceted views | Y | conceptual | **Confirmed by user: separate feature from the excluded `dilution_analysis_ui.R`**, not a superset/replacement. Covers both "Explore fits" and "Compute fits" dilution views per a comment in `std_curve_weights_module.R`. |
| QC - Basic | Standard Curve | `std_curve_calc_module.R`, `std_curve_compare_module.R`, `std_curve_view_module.R` | see deep-dive §2 | Y | **compute-decision** | Model selection, fit engine, Bayesian precision resolution, measurement-error toggle. |
| QC - Basic | Precision Weights | `precision_weight_panel.R`, `std_curve_weights_module.R` | see deep-dive §4 | Y | **compute-decision** | curveRweights job submission + summary panel. |

## Change Study Settings (`study_settings`)

| Tab | Sub-tab | Source file(s) | Controls/inputs present | Help candidate? | Help type | Notes |
|---|---|---|---|---|---|---|
| Study Settings | Calibration Settings | `settings_cascade_ui.R`, `settings_cascade_access.R`, `help_utils.R` | generic per-param editor (text/select/multi-select/checkbox depending on `param_control_type`), revert-to-tier | Y | **mixed — see deep-dives** | **Already has a working help mechanism** (`src/help/settings/*.md`). Confirmed params in code/migrations: `apply_prozone` (has note), `include_measurement_error` + `pcov_threshold` (share one note), `model_form_list` (compute-decision, no note — gap), `standard_dilution_reference` (compute-decision, no note — gap, see deep-dive §1), `compute_cluster` (infrastructure routing, no note — gap, see deep-dive §5). **The full param catalog lives in the `calib_settings_meta` DB table, not fully enumerable from version control** (migrations only show incremental additions) — Phase 5 should pull the live table (`SELECT param_name, param_group, param_label, param_description FROM madi_results.calib_settings_meta`) for a complete list rather than relying on this grep-based partial inventory. |
| Study Settings | Annotations | `annotation_ui.R` | Antigen/Feature text fields + save; `selectInput(ref_timeperiod)`, `selectInput(ref_agroup)` + save; display-order editor + save | Y | procedural | Three sub-sections: Antigen/Feature annotations, Timepoints & Arms/Groups (referent selection), Display order. |
| Study Settings | Source Names | `source_alias.R` | DT (current mappings), DT (unmapped raw sources), add-mapping control | Y | conceptual | Maps raw instrument source strings to a canonical standard-curve source name — worth explaining *why* sources need canonicalizing (plates from different runs/instruments naming the same physical standard differently). |
| Study Settings | Export/Import | `settings_export_import_ui.R` | `downloadButton(dl_rdata)`, `downloadButton(dl_json)`, `fileInput(upload)`, `actionButton(apply)` | Y | procedural | |
| Study Settings | Delete Components | `delete_study_components_ui.R`, `delete_study_components.R` | `selectInput(dc_scope)`, `actionButton(dc_preview)`, delete confirm | Y | procedural (cautionary) | Destructive, preview-then-commit pattern — a help note should emphasize irreversibility, not just mechanics. |
| Study Settings | Clone Study | `clone_study_components_ui.R`, `clone_study_components.R` | scope selector, `actionButton(clone_preview)`, clone confirm | Y | procedural | |

## Study Overview (`study_overview`)

Rebuilt as a lazy module in `study_overview.R` (the old `study_overview_ui.R` is now a retired no-op — a prior version eagerly ran every query on tab-open and crashed the app; flagging as a resolved historical issue, not a current one). Five live sub-tabs (a sixth, CV, is commented out):

| Tab | Sub-tab | Source file(s) | Controls/inputs present | Help candidate? | Help type | Notes |
|---|---|---|---|---|---|---|
| Study Overview | Blanks, Controls & Standards | `study_overview.R` (Phase 2 view) | filters TBD per view | Y | conceptual | |
| Study Overview | High-Aggregate & Low Bead | `study_overview.R` (Phase 2 view) | filters TBD per view | Y | conceptual | Relates to the Bead Count QC concept above — cross-link. |
| Study Overview | Samples by Arm | `study_overview.R` (Phase 2 view) | filters TBD per view | Y | conceptual | Uses `annotation_*` arm order/referent from the Annotations settings tab — cross-link. |
| Study Overview | Samples by Timepoint | `study_overview.R` (Phase 2 view) | filters TBD per view | Y | conceptual | Same cross-link note as Arm. |
| Study Overview | Sample Estimate Quality | `study_overview.R` (Phase 3 view) | method dimension (bayesian/frequentist) | Y | **compute-decision** | Reads `calib_*` + `curve_lookup`; surfaces the same fit-quality concepts as the Data tab's Diagnostics/LOQ table. |

---

## Totals

- **Tab/sub-tab rows enumerated:** 46 (3 Manage Project + 7 Import + 16 Data + 4 QC-Basic + 6 Study Settings + 5 Study Overview + 5 misc/home/landing not counted as candidates).
- **Marked help candidate (Y):** 40.
- **Marked compute-decision:** 9 rows across Standards dilution reference, description-field config, Fit/model selection, Diagnostics/LOQ, Precision weights ×2, Standard Curve controls, Precision Weights controls, Sample Estimate Quality.

## Acceptance criterion check

Every live tab/sub-tab in the running app (per source, not per the plan's assumptions) has a row above. Compute-decision rows point to `01a-decision-points.md` for the required deep-dive. The one area the plan expected to enumerate fully but that turned out to be dead code (Advanced Diagnostics) is excluded per explicit user direction, with the exclusion reasoned and cited above rather than silently dropped.
