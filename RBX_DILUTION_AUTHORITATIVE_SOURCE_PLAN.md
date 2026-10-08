# `.rbx`/`.srbx` Dilution Mapping: Problem, Design, and Implementation

**Status: shipped.** Both sub-features below are implemented, tested against
real instrument files, smoke-tested in a running local app, and committed to
`main` (`a252b29`) — not yet pushed to `origin` as of this writing.

**Scope:** Bead (`.rbx` / `.srbx`) import pathway only. No changes to xPONENT
or ELISA readers.

This document was originally written as a handoff plan by a different Claude
session, without direct repo access. Its initial diagnosis (Sections 1-3
below, kept for history) turned out to target the wrong code path — see
"How we got here" for the correction. Everything after that section reflects
what was actually built.

---

## 1. The problem

The `.rbx`/`.srbx` binary format carries two independent, sometimes
contradictory sources of "dilution" for a well:

1. **A numeric dilution field in the binary itself**, per sample record,
   which `reader_bead_rbx.R` extracts into a `dilution_map` data frame
   (`plateid, well, dilution`).
2. **A free-text `Description` field**, parsed for a `"1:N"` or `"N/M"`
   ratio to recover a dilution for curve-fitting / shape validation.

Tested against two real files in `dev/`: the PIH reference file (ratio-style
descriptions, e.g. `"Inhouse Ref 1:2952450"`) and `GBSIGG-WP4-417_test.srbx`
(a different lab's export, bare unstructured descriptions: `"1"`…`"11"` for
samples, `"S1"`…`"S11"` for standards, `"QC1"`/`"QC2"`/`"QC3"` for controls).

| Type | PIH (reference) | GBSIGG |
|---|---|---|
| Sample (X) | `"80 V1"` | `"1"`…`"11"` (repeats across replicate wells) |
| Standard (S) | `"Inhouse Ref 1:2952450"` | `"S1"`…`"S11"` |
| Control (C) | n/a | `"QC1"`, `"QC2"`, `"QC3"` |
| Blank (B) | n/a | `""` |

GBSIGG's true dilutions (500/5,000/50,000 for samples; 450/50,000/125,000
for controls) exist **only** in the binary's numeric field — never in text.
Any `.rbx`/`.srbx` file whose authoring software writes bare descriptions
hits this same gap; it's a structural problem, not a one-off data-quality
issue with this one lab.

---

## 2. How we got here (diagnosis history)

The original handoff plan assumed the fix belonged in
`assay_description_parse.R` (`ai_desc_assign()`), because that's the only
bead-parsing code the originating chat session had seen. **Phase 0
discovery (2026-10-05) found this file is dead code for bead/.rbx uploads**:

- `descriptor_bead.R` sets `preprocess = TRUE`, so `assay_import_module.R`
  always takes the `resolved_wells`/`ai_merge_resolved()` branch of
  `build_plates_map()` — never the legacy branch that calls
  `assay_description_parse.R`.
- The live engine is `assay_shape_rules.R` + `assay_shape_ui.R` (per-shape
  token **bindings**: `slot | pattern | constant | from_type | ignore`),
  an entirely different, newer model than the dead file's single positional
  element order.
- `assay_description_rule_ui.R` (the original plan's Phase 4 UI target) is
  never mounted anywhere in `src/` — zero call sites, confirmed by grep.
- The live engine already fixes the original plan's headline bug (a bare
  integer getting misread as a dilution and corrupting `PatientID`), by
  design, independently of this work.

**The real, remaining gap**: `reader_bead_rbx.R` parses `dilution_map`
correctly, but nothing in the live pipeline ever joined it onto a well's
resolved dilution. It was parsed, carried through `template_seed`, and
discarded.

**A second, more serious correction**, found by running the actual parser
against both real files (standalone harness, no Shiny): the binary's numeric
dilution field is **always `1` for Standards**, in both files — a Bio-Plex
binary format limitation (the XML export, not the raw binary, carries a
standard point's true value), not a parsing bug. The original plan's Goal 1
— "use the instrument value whenever numeric" — would have been a silent
regression: it would have overwritten the PIH file's correct, text-derived
standard dilutions (150…2,952,450) with `1` for every standard point.

This meant the fix had to split into two independent pieces, detailed below.

---

## 3. What shipped, part 1: instrument-authoritative dilution for Samples/Controls

**Goal:** when a `.rbx`/`.srbx` upload's binary `dilution_map` has a usable
numeric value for a Sample (X) or Control (C) well, that value is used —
never a value parsed out of Description text — and the well is never
blocked on a missing/bad `DilutionFactor`. Standards are explicitly excluded
(see above). Existing behavior for formats with no binary dilution map
(xPONENT, raw `.xlsx`) is unchanged.

**No `dilution_source` is surfaced in exported output** — confirmed with the
user this stays a transient, import-time-only UI signal, never threaded
into `plates_map`/`assay_response_long`/the committed `calib_*` tables.

### Implementation

- **`assay_well_inventory.R`**: new `instrument_dilution` column on the well
  inventory. The bead adapter (`.ai_inventory_bead()`) joins
  `raw$template_seed$dilution_map` onto the flat preview by normalized
  `(plate_key, well)`; every other adapter/format defaults it to `NA` via a
  generic fallback in `ai_well_inventory()`.
- **`assay_shape_rules.R`**: `ai_resolve_one()` takes an
  `instrument_dilution` parameter. When finite/positive **and** the well's
  type is X or C, it overrides whatever the text binding would have
  resolved, sets `dilution_src = "instrument"`, and suppresses the
  bad-dilution/missing-required issue for that well's `DilutionFactor`.
  `ai_resolve_inventory()`, `ai_shape_verdict()`, and `ai_ruleset_ready()`
  all thread the per-well value through (the verdict/gate conservatively
  requires *every* well sharing a description to be covered, not just one).
- **`assay_shape_ui.R`**: a coverage banner in the "Description rules check"
  panel (`output$overall`) — *"Dilution read directly from the instrument
  file for N/M Sample and Control well(s)"*.
- **`reader_bead_rbx.R`**: corrected a stale comment that claimed the binary
  field was authoritative for Standards too (it isn't — see above).

### Verified

Ran the real parser directly against both files in `dev/` (standalone
harness: sources only `rbx_binary_parser.R` + the bead reader's pure parsing
functions, no Shiny/DB):

- **GBSIGG**: all 66 Sample wells and 6 Control wells resolve to the correct
  instrument value (500/5,000/50,000; 450/50,000/125,000), zero blocking
  `DilutionFactor` errors.
- **PIH (regression check)**: Standards are **unchanged** — still the
  correct, text-derived values (150…2,952,450), never the binary's `1`
  placeholder.
- `test-assay-shape-rules.R`: 6 new test cases (146→162 passing; same 5
  pre-existing, unrelated failures — confirmed identical before/after via
  `git stash`).

---

## 4. What shipped, part 2: Standards dilution reference table

**The problem this solves:** for GBSIGG's Standards specifically, there is
**no machine-recoverable dilution at all** — not in the binary (always `1`),
not in the text (`"S1"`…`"S11"`, no ratio). No parsing change can conjure
the true 11-point series from this file. The user chose (over a
simpler "type it in per shape" option) to add a proper, reusable mechanism:
an experiment-scoped **label → dilution reference table**, entered once,
remembered thereafter.

Two corrections from the user during design review, both load-bearing:

1. **Don't assume `S<integer>` labeling.** `.rbx_type_from()`
   (`reader_bead_rbx.R`) derives the type code by stripping non-digits from
   the instrument's internal sample label. A lab whose label has no digits
   at all collapses *every* Standard on a plate to the bare type code `"S"`
   — an ordinal index isn't a safe key. **The key is the raw Description
   text**, not a type-code suffix or position.
2. **Experiment scope, not study scope, required from the start** — a study
   commonly runs different standard-curve designs across its experiments.

### Implementation

- **Migration**: `i-spi-compute/worker/migrations/005_standard_dilution_reference.sql`
  — registers `standard_dilution_reference` in the already-live
  `calib_settings_meta` table. `param_control_type = 'textInput'`, a safe
  fallback the generic settings editor already renders as plain text for
  any control type it doesn't specifically recognize. **Applied to the
  database by the user.**
- **`assay_std_reference_rules.R`** (new, pure logic): JSON
  parse/serialize of `{description, dilution}` pairs; the candidate
  pre-scan (`ai_std_reference_candidates()` — which distinct descriptions
  for a type still need manual entry, i.e. not resolved by an eligible
  instrument value, a text ratio, or an existing saved entry); a
  last-write-wins merge for the save path.
- **`assay_shape_rules.R`**: `ai_resolve_one()` gained a second, *lower*
  -priority `reference_dilution` parameter — applied only when neither the
  instrument override nor the text resolves the well, and **not type-gated**
  (any type whose dilution survives both earlier checks unresolved is
  eligible, not hardcoded to Standards). `dilution_src` distinguishes
  `"instrument"` / `"reference"` / `"text"` for UI provenance display.
- **`assay_std_reference_ui.R`** (new Shiny module, "Stage 1.5" of the
  pre-processor, mounted between the plate grid and the shape/description
  configuration step): loads the experiment's saved reference via
  `resolve_settings_scoped()`, shows only genuinely unresolved descriptions
  in an editable table (DT cell-edit pattern, same as `source_alias.R`),
  blocks on Save until every row has a positive value, merges and writes
  back via `set_setting()`. Parameterized by `specimen_type` (defaults to
  `"S"`) so Controls or another type could reuse it later by mounting a
  second instance — not by changing the module.
- **`assay_import_module.R`**: new stage wired in, gating both
  `shape$enabled` and `pre_ready()`; import steps renumbered 1-9
  (pre-processor path) / unchanged 1-6 (plain path).
- **`app.R`**: two new `source()` lines.

### Resolution precedence (final)

1. Binary `dilution_map` — **X and C only**, never S.
2. Text ratio via the existing shape-engine proposal — any type, unchanged.
3. Experiment-scoped reference table — fills whatever's still unresolved,
   any type.
4. Otherwise: blocked, same gate as before this work.

### Verified

Same standalone harness, both real files:

- **GBSIGG**: all 11 Standard descriptions correctly flagged as needing
  manual entry (confirmed the gate isn't fooled by a populated-but-ineligible
  `instrument_dilution` column); after a synthetic reference is supplied,
  every Standard well resolves with zero remaining errors.
- **PIH (regression check)**: zero manual entries needed — existing ratio
  text already resolves every Standard, so a lab that writes good
  descriptions is never bothered by the new screen.
- `test-assay-std-reference-rules.R` (new): 35 passing.

---

## 5. Manual smoke test (local `shiny::runApp()`, both real files)

Ran the actual app locally against the real dev database (not just the
standalone harness) and walked through both files by hand.

**Bug found and fixed during this pass:** the per-shape **live preview
table** (`output$preview_<type>` in `assay_shape_ui.R` — the table a user
actually watches while configuring each tab) called the resolution function
**without** passing either override, so every Standards/Samples/Controls
dilution showed blank/unresolved in that specific table even though the
real resolution underneath (verdict gate, final commit-time resolve) was
already correct. This made the feature look completely unhooked from the
screen the user was actually looking at, despite being functionally correct
everywhere else.

Fixed by:
- Threading `instrument_dilution`/`reference_dilution` into the preview's
  `ai_resolve_one()` call, computed per description with the same
  "every well sharing this text must be covered" conservatism used
  elsewhere.
- Adding `dilution_src` to `ai_resolve_one()`'s return value so the UI can
  state *which* source resolved a value, rather than re-deriving it.
- Annotating the Dilution column inline, e.g. `500 (from instrument file)`
  or `450 (from saved reference)`, instead of a bare number.
- Adding a note directly under the `DilutionFactor` binding-editor row when
  the whole group is already covered by an override: *"Every well in this
  group already has a dilution read from the instrument file — this
  binding is not used for them."* (There is no separate "dilution source"
  dropdown by design — the live engine configures per-component bindings,
  not a global mode toggle like the dead original-plan UI had. The goal was
  making the automatic override visible and legible wherever the user is
  actually looking, not adding a new control surface.)

**Confirmed working after the fix**, via the app's own summary panel on the
PIH file: *"Dilution read directly from the instrument file for 74/74
Sample and Control well(s)"*; Standards group fully resolved and approved
via text (zero manual reference entries, confirming no regression); the one
Samples group that doesn't fully resolve is an unrelated, pre-existing gap
(some PIH sample descriptions are a single bare token with no timepoint
format, e.g. `"JM27"`, so `TimePeriod` stays empty — Samples don't even
require `DilutionFactor` to resolve, so this has nothing to do with this
work).

Also caught and fixed before committing: a local-dev auth bypass
(`Sys.setenv(LOCAL_DEV = "1")`, explicitly commented *"do not push in
prod"*) had been left active in `app.R` for the smoke test — reverted to
commented-out before staging.

---

## 6. Rollout status

- [x] DB migration applied (`005_standard_dilution_reference.sql`).
- [x] Both sub-features implemented, unit-tested (162 + 35 passing, zero
      new failures), and verified against both real files via a standalone
      harness.
- [x] Manual smoke test in a locally running app, both real files, against
      the real dev database — one real bug found (preview table) and fixed.
- [x] Committed to `main`: commit `a252b29`.
- [ ] **Not yet pushed to `origin`** — pushing triggers this repo's
      `ghcr-build-publish.yml` automatically (push-to-main trigger,
      confirmed), so push *is* the build step; nothing else is needed on
      that side.
- [ ] Remote/deployed instance (if different from local) needs its pod
      restarted to pick up the new image after the push-triggered build
      completes — same category of gotcha as a stale compute-worker pod,
      but for the i-spi-refactor app pod specifically. This feature never
      touches the i-spi-compute worker.

## 7. Explicit follow-ups / out of scope

- The pre-existing `TimePeriod`-empty gap for single-token Sample
  descriptions (e.g. `"JM27"`, `"RP41"`) is unrelated to dilution and was
  not addressed here.
- `assay_std_reference_ui.R` is generic (`specimen_type` param) but only
  ever mounted for `"S"`. Reusing it for Controls or another type is a
  matter of mounting a second instance in `assay_import_module.R`, not
  changing the module.
- A `standard_curve_concentration` concept already exists (dead/commented
  code in `calib_data_access.R`, referencing the old `antigen_feature_settings`
  table) — a single stock-concentration scalar per antigen/feature, related
  but distinct from this plan's per-point dilution series (you'd combine
  the two to back-calculate absolute concentrations). Not addressed here;
  flagged only because it confirms this kind of per-experiment calibration
  metadata already has precedent in the app.
- Flow format has no `preprocess` flag (`descriptor_flow.R`), so none of
  this applies to it without separately opting flow into the pre-processor
  — out of scope, not acted on.
- Durable design principle from this work, saved to memory: context-specific
  settings entered at the point a gap is discovered (like this reference
  table) are part of a study's documentation, not disposable config — keep
  such entry points easy and timely rather than over-building them.
