# Strategic Plan: Use the `.rbx`/`.srbx` Binary Dilution Map as the Authoritative Dilution Source

**Status:** Draft for engineering handoff (Claude Code)
**Owner:** [you]
**Prepared by:** Claude (chat), from direct inspection of the attached R source and two sample instrument files
**Scope:** Bead (`.rbx` / `.srbx`) import pathway only. No changes intended to xPONENT or ELISA readers.

---

## 0. How to use this document (read this first, Claude Code)

This file is the handoff artifact for a multi-step refactor. It is intentionally
self-contained: it includes the diagnosis, the target design, a concrete file-by-file
task list with acceptance criteria, and a test plan. Work the tasks in order
(Phase 0 → Phase 4); each phase is meant to be a separate reviewable change.

**Test data location:** two real instrument files are available locally for manual
and automated regression testing, placed in the dev working directory so they are
NOT committed to the repo:

```
<dev-dir>/PIH_08Mar18_Plate_17_Reanalysed_31Jan2019.rbx   # reference file, text-based dilutions, no authoritative map needed
<dev-dir>/GBSIGG-WP4-417_test.srbx                        # new lab's file, THIS is the one that exposes the bug
```

Confirm these are already covered by whatever `.gitignore` pattern excludes the dev
data directory before starting; do not add them to version control, and do not
assume the CI test runner has access to them — tests built from these files should
either be marked local-only/skippable, or should embed small synthetic binary
fixtures that reproduce the same conditions (see Phase 3).

If anything in this document conflicts with what you find in the actual
repository (file names, function signatures, surrounding logic not shown to the
chat agent that produced this plan), **trust the repository** and update this
document's assumptions rather than silently working around the discrepancy —
flag it back to the user.

---

## 1. Problem statement

The `.rbx`/`.srbx` binary format carries two independent, potentially
contradictory sources of "dilution" for a well:

1. **A numeric dilution field in the binary itself**, per sample record
   (`parse_samples()` → `s$dilution`), which `reader_bead_rbx.R` already
   extracts into a separate `dilution_map` data frame
   (`plateid, well, dilution`) and threads through `template_seed`. The code
   comments already call this **"authoritative."**
2. **A free-text `Description` field**, which the generic bead/ELISA pipeline
   (`assay_description_parse.R`) parses with a configurable per-type rule
   (`PatientID_TimePeriod_DilutionFactor` for samples;
   `Source_DilutionFactor` for standards/blanks/controls) to recover a
   `specimen_dilution_factor` used in shape validation and (likely) downstream
   concentration/curve-fitting.

Today, **source (2) is the only one enforced by validation**, and it is the
only one with any UI around it (`assay_description_rule_ui.R`). Source (1) is
extracted and carried as data but — as far as this plan's author could verify
from the provided files — is never checked for presence, never preferred over
(2), and never used to *suppress* the requirement that (2) succeed.

### 1.1 Confirmed failure mode (traced by hand against the actual code)

Tested against `GBSIGG-WP4-417_test.srbx`. This lab's software writes bare,
unstructured descriptions instead of the delimited convention the reference
file uses:

| Type | Reference file (`PIH...rbx`) | New file (`GBSIGG...srbx`) |
|---|---|---|
| Sample (X) | `"80 V1"` | `"1"` … `"11"` (same number repeats across a sample's 3 dilution replicate wells) |
| Standard (S) | `"Inhouse Ref 1:2952450"` | `"S1"` … `"S11"` |
| Control (C) | n/a | `"QC1"`, `"QC2"`, `"QC3"` |
| Blank (B) | n/a | `""` |

The true dilutions for the new file (500 / 5,000 / 50,000 for samples;
450 / 50,000 / 125,000 for the three controls) exist **only** in the binary's
numeric dilution field — never in the text — and are already captured in
`dilution_map`.

Tracing `ai_parse_description_row()` / `ai_desc_assign()` in
`assay_description_parse.R` against these bare-token descriptions, with the
UI's default "Automatic" dilution mode:

- **Type X:** a lone numeric token (`"1"`) is itself a valid *bare-dilution*
  candidate, so `ai_desc_assign()` greedily claims it as `DilutionFactor` —
  leaving `PatientID` **and** `TimePeriod` empty. Both raise
  `severity = "error"`, `kind = "missing_required"` (lines ~255-262 of
  `assay_description_parse.R`). `DilutionFactor` itself ends up holding the
  *sample index* (1–11), not a dilution.
- **Type C:** `"QC1"` isn't numeric, so positional allocation applies;
  because `Source` is the greedy field (`.AI_DESC_GREEDY <- c("TimePeriod",
  "Source")`), the single atomic field to its right (`DilutionFactor`) grabs
  the token instead, leaving `Source` empty → another `missing_required`
  error, and a `DilutionFactor` that fails `ai_parse_dilution()` entirely
  (non-numeric).
- **Net effect on this 96-well plate:** ~66 X-well errors + 6 C-well errors,
  which `assay_plate_grid.R` / `assay_shape_ui.R` treat as blocking —
  `!any(issues$severity == "error")` gates the "confirmed" / "OK" state. The
  plate cannot be confirmed as-is, and even if the per-type rule is manually
  reconfigured to stop erroring, the resulting `specimen_dilution_factor`
  column is wrong (1–11, or `NA`) rather than the real titration dilutions.

### 1.2 Why this matters beyond this one file

This is not a one-off data-quality problem with this lab's export — it is a
structural gap: **any `.rbx`/`.srbx` file whose authoring software writes
short/bare descriptions will hit the same failure**, because the pipeline has
no notion of "this batch already has a trustworthy numeric dilution per well;
don't require the text to also encode one." Multiple labs feeding into the
same study on the same bead pipeline is the normal case per the app's design
(batch/multi-file import), so this will recur.

---

## 2. Goals

- **G1.** When a `.rbx`/`.srbx` upload's binary dilution map has a usable,
  numeric value for a given well, that value is used as
  `specimen_dilution_factor` for that well — never a value parsed out of the
  `Description` text.
- **G2.** When the binary dilution map covers a well, the per-type
  description-rule validator does not require (and does not attempt to
  recover) a `DilutionFactor` token from that well's `Description` text, and
  does not raise a `missing_required` error for it.
- **G3.** The import UI tells the user, per batch/file, how many wells are
  being sourced this way (e.g. *"Dilution values read directly from the
  instrument file for 96/96 wells — these will be used instead of the
  Description text"*), so this isn't a silent behavior change.
- **G4.** Existing behavior for sources that do **not** carry a binary
  dilution map (xPONENT/Excel imports; any `.rbx` whose dilution map come
  back empty or non-numeric for a well) is unchanged — text parsing remains
  the fallback, exactly as it works today for the reference file.
- **G5.** No regression on the existing reference file/workflow
  (`PIH_08Mar18_Plate_17_Reanalysed_31Jan2019.rbx`), including its own
  ratio-style descriptions (`"Inhouse Ref 1:2952450"`) continuing to parse
  the way they do today, when its dilution map does *not* fully cover the
  plate (if that's the case — verify in Phase 0).

## 2.1 Non-goals

- Not attempting to fix the general "bare numeric token is ambiguously a
  dilution vs. an ID" heuristic in `ai_desc_assign()` for *non-rbx* sources
  (xPONENT/ELISA). That heuristic is out of scope here; the fix for rbx is to
  stop relying on the text at all when a better source exists, which sidesteps
  the heuristic rather than repairing it.
- Not changing `rbx_binary_parser.R`'s core parsing stages (header, analyte
  map, quantification, geometry, stat blocks) — Phase-0 verification in
  Section 4 showed these already work correctly on both sample files.
- Not redesigning the per-type rule UI's layout; only adding to it (a status
  banner, and narrowing which elements are "required" per Section 3.3).

---

## 3. Target design

### 3.1 New concept: "externally-sourced" dilution, per (file, well)

Introduce an explicit, carried-through flag so that every downstream
consumer can tell, per well, whether its dilution came from the binary or
needs to come from text:

```
dilution_source ∈ { "instrument", "text", "default" }
```

- `"instrument"`: a finite, positive numeric value was present in the file's
  own dilution map for this well.
- `"text"`: no instrument value; fell back to `Description`-text parsing
  (today's only behavior).
- `"default"`: neither source produced a value; the existing `use_defaults`
  fallback of `1` applies, as today.

This flag should live alongside `specimen_dilution_factor` in whatever row
structure already carries it (likely the `plates_map` / resolved-wells
structure built in `build_plates_map`, not among the files reviewed for this
plan — **Phase 0 task:** locate it).

### 3.2 Merge order (authoritative source wins)

For any `.rbx`/`.srbx`-derived batch:

1. Parse `Description` text as today (`ai_parse_description_row()` et al.) to
   get `PatientID` / `TimePeriod` / `Source` / groups — **these identity
   fields are unaffected by this change** and still come from text.
2. Independently, parse-eligibility-check the `dilution_map` row for that
   well: numeric, finite, `> 0`.
3. If step 2 succeeds: overwrite `specimen_dilution_factor` with the
   instrument value, set `dilution_source = "instrument"`, and **suppress**
   the `DilutionFactor` requirement/anchor-search for that well entirely
   (see 3.3) — do not let step-1's text parsing claim a token as a dilution
   anchor in the first place, since that's what causes the PatientID/
   TimePeriod/Source corruption described in §1.1.
4. If step 2 fails (no map, or non-numeric/non-positive): behave exactly as
   today (text parse, with `use_defaults` fallback to `1`).

Implementation note: step 3's "suppress the anchor search" has to happen
**before** `ai_desc_assign()` runs for that well, not after — patching the
dilution value post-hoc doesn't fix the fact that the wrong token got
consumed as the anchor, corrupting the other fields. The cleanest point to do
this is a new `dilution_mode` value (3.3).

### 3.3 New `dilution_mode`: `"external"`

Add a fourth option alongside `"position" | "auto" | "content"` in
`ai_desc_assign()` (`assay_description_parse.R`):

- `"external"`: skip the `DilutionFactor`-anchor search entirely (treat
  `has_dil` as `FALSE` for the purposes of token allocation), **and** drop
  `DilutionFactor` from the "required" check loop in
  `ai_parse_description_row()` regardless of whether it's present in
  `order`. The actual `dilution_factor`/`dilution_ok` fields on the return
  value are filled in afterward, by the caller, from the authoritative
  source — not by this function.
- This mode should be selectable per (file batch, type) the same way
  `"auto"`/`"position"` are today, but for `.rbx`/`.srbx` imports it should
  be the **auto-selected default** whenever `dilution_map` has >0 usable
  rows for that batch (see 3.4), overridable by the user.

### 3.4 Where the "does this batch have a usable instrument dilution map"
check happens

Add a small, pure helper (name suggestion, adjust to repo conventions):

```r
# Returns TRUE if the dilution_map data frame has at least one finite,
# positive, non-NA `dilution` value. Used to decide whether the rbx/srbx
# batch should default to dilution_mode = "external".
rbx_has_authoritative_dilution <- function(dilution_map) {
  !is.null(dilution_map) &&
    nrow(dilution_map) > 0 &&
    any(is.finite(dilution_map$dilution) & dilution_map$dilution > 0)
}
```

Call this once in `.rbx_parse_raw()` (`reader_bead_rbx.R`) and carry the
result (plus a per-well coverage count, for the UI message in G3) forward in
`template_seed`, e.g.:

```r
template_seed = list(
  ...,
  dilution_map              = p$dil,
  dilution_map_authoritative = rbx_has_authoritative_dilution(p$dil),
  dilution_map_coverage      = list(covered = sum(is.finite(p$dil$dilution) & p$dil$dilution > 0),
                                     total   = nrow(p$dil))
)
```

### 3.5 UI change (G3)

In `assay_description_rule_ui.R`, when `dilution_map_authoritative` is
`TRUE` for the active upload:
- Default the "Dilution detection" radio button to a new third choice,
  `"From instrument file (recommended)"` → `dilution_mode = "external"`,
  for every type that has coverage.
- Show a one-line status banner with the coverage count from
  `dilution_map_coverage` (e.g. `"96/96 wells"`), so a partial-coverage file
  is visible as such, not silently mixed.
- Leave "Automatic"/"Positional" selectable for the user who wants to
  override (e.g. they trust their own text convention over the binary for
  some reason) — this is a default, not a lockout.

### 3.6 Validator message changes

`plate_validator_functions.R` currently emits messages like *"Need to modify
the Sample description column to include a minimum of
`[ID]_[timeperiod]_[dilution_factor]`..."* (line ~397) and the analogous
Standard/Blank messages (~413, ~667, ~682). These need a conditional branch:
when `dilution_mode == "external"` for the row's type, drop `dilution_factor`
from the stated required shape in the message text (and, per 3.3, the
check itself no longer fires for it).

---

## 4. Phase 0 — Discovery (do this first; blocks everything else)

The chat-based analysis that produced this plan did **not** have access to
`batch_layout_functions.R` (where `build_plates_map()`,
`check_and_report_description()`, `parse_description_with_defaults()`,
`assign_plate_numbers()`, and `clean_plate_id()` are stated to live) or to
`generate_layout_template_ref.R` / `reader_elisa_parsers.R`. Before writing
any new code:

- [ ] **D1.** Open `build_plates_map()`. Confirm exactly how `dilution_map`
  (already produced by `reader_bead_rbx.R` today) is or isn't consulted —
  it's plumbed through `assay_import_module.R` (`resolved_wells` /
  `template_seed$dilution_map`, see line ~184) but this plan's author could
  not confirm a merge step exists. **If one already exists, this whole plan
  narrows to "stop the text parser from corrupting other fields when an
  instrument value exists" (§3.3) — the merge itself may already be done.**
- [ ] **D2.** Confirm where `specimen_dilution_factor` is actually consumed
  downstream (standard curve fitting / concentration back-calculation) to
  make sure the merged instrument-sourced value reaches that calculation,
  not just the validation-display layer.
- [ ] **D3.** Confirm whether `resolved_wells` (mentioned in
  `assay_import_module.R` line ~179 and referenced in a comment in
  `reader_bead_rbx.R` line ~215 as "the LEGACY branch... taken when no
  resolved_wells is supplied") is a *second, newer* code path that may
  already bypass free-text parsing for rbx-sourced data. If so, verify which
  path the bead `.rbx` reader actually takes today (`use_pre` flag,
  `assay_import_module.R` ~L179) and whether this plan's fix belongs in the
  legacy path only, the resolved_wells path only, or both.
- [ ] **D4.** Re-run (or ask the chat agent to re-run) the Python-ported
  parser trace from the prior analysis turn against
  `PIH_08Mar18_Plate_17_Reanalysed_31Jan2019.rbx` specifically for its
  `dilution_map` coverage (does every well get a numeric dilution from the
  binary on that file too, or only partial?) — this determines whether G5
  (no regression) requires the "auto" default to stay off for that file, or
  whether it would also switch this reference file to
  `dilution_mode = "external"` and change its currently-working behavior.
  **Do not assume** — verify against the actual file.

Document findings from D1–D4 as a short addendum at the bottom of this file
(`## Phase 0 Findings`) before proceeding, so the rationale for Phase 1's
exact code changes is traceable later.

---

## 5. Phase 1 — Core data plumbing

**Files touched:** `reader_bead_rbx.R`, possibly `assay_import_module.R`

- [ ] **T1.1** Implement `rbx_has_authoritative_dilution()` (§3.4) and the
  per-well coverage summary; unit test with synthetic `dilution_map` frames
  (empty, all-NA, partial, full).
- [ ] **T1.2** Thread `dilution_map_authoritative` and `dilution_map_coverage`
  through `.rbx_parse_raw()`'s return value and into `template_seed`.
- [ ] **T1.3** Confirm (per Phase 0 findings) the exact structure the merge
  step in Phase 2 needs to read `dilution_map` against — it's currently keyed
  on `plateid` + `well` (see `reader_bead_rbx.R` ~L156-160); make sure that
  join key is stable/available at the point Phase 2 needs it.

**Acceptance criteria:** `template_seed$dilution_map_authoritative` is `TRUE`
for `GBSIGG-WP4-417_test.srbx` and reflects whatever Phase-0/D4 determined
for the PIH reference file. No change to existing `plate`/`header`/
`assay_long` outputs.

---

## 6. Phase 2 — Parser logic (`assay_description_parse.R`)

**Files touched:** `assay_description_parse.R`

- [ ] **T2.1** Add `"external"` as a valid `dilution_mode` value everywhere
  the enum is declared (`ai_desc_assign()`, `ai_parse_description_row()`,
  `ai_parse_rule()`, `ai_ruleset_from_legacy()`, and the `match.arg()` call
  sites — grep for `dilution_mode` to get the full list; it currently
  appears in at least the four functions whose signatures were visible to
  this plan).
- [ ] **T2.2** In `ai_desc_assign()`: when `dilution_mode == "external"`,
  treat `has_dil` as `FALSE` (skip the anchor search at §3.2 step 3) so the
  full token set allocates across the remaining identity fields normally —
  this alone fixes the PatientID/TimePeriod/Source corruption in §1.1,
  independent of anything else.
- [ ] **T2.3** In `ai_parse_description_row()`: when `dilution_mode ==
  "external"`, exclude `"DilutionFactor"` from the
  `missing_required` check loop unconditionally (don't rely on it being
  absent from `order`), for both the X branch (~L255-262) and the S/B/C
  branch (~L281-287).
- [ ] **T2.4** Confirm `use_defaults` behavior is untouched for
  `"external"` mode at the *identity*-field level (PatientID/TimePeriod/
  Source still default the same way) — only the dilution handling changes.
- [ ] **T2.5** Leave the actual overwrite of `dilution_factor` /
  `dilution_ok` with the instrument value to the **caller** (Phase 3), not
  inside this file — `assay_description_parse.R` is shared by ELISA too, and
  should stay a pure text-parsing module. The rbx-specific merge belongs in
  `reader_bead_rbx.R` or `build_plates_map()` (confirm in Phase 0, D1).

**Acceptance criteria:** with `dilution_mode = "external"` and
`order = c("PatientID", "TimePeriod", "DilutionFactor")`, parsing
`Description = "1"` for a Type-X well yields `subject_id = "1"`,
`timeperiod = ""` → defaulted to `"T0"` (per existing `use_defaults` logic,
unchanged), **zero** `missing_required` issues, and a `dilution_factor` of
`NA`/unset (to be overwritten by the merge step in Phase 3) rather than `1`.

---

## 7. Phase 3 — Merge step + mode auto-selection

**Files touched:** `reader_bead_rbx.R` and/or `build_plates_map()` (location
per Phase-0 finding D1); `assay_import_module.R` if the merge needs to happen
at assembly time rather than parse time.

- [ ] **T3.1** Implement the per-well overwrite described in §3.2 step 3:
  for every well where `dilution_map` has a finite, positive value, set
  `specimen_dilution_factor` from it and `dilution_source = "instrument"`;
  else leave the text-parsed (or defaulted) value and mark
  `dilution_source = "text"` / `"default"` accordingly.
- [ ] **T3.2** Auto-select `dilution_mode = "external"` as the default for
  any type with full or partial coverage from `dilution_map_authoritative`
  (§3.4), feeding into whatever builds the default `ai_ruleset_from_legacy()`
  call for a new upload.
- [ ] **T3.3** Make sure `dilution_source` rides along into whatever
  structure the shape UI (`assay_shape_ui.R`) and plate grid
  (`assay_plate_grid.R`) read, so a future UI enhancement (not required for
  this plan, but worth leaving the hook) could visually flag
  instrument-sourced wells if desired.

**Acceptance criteria:** loading `GBSIGG-WP4-417_test.srbx` end-to-end
produces `specimen_dilution_factor ∈ {500, 5000, 50000}` for the 66 Unknown
wells (grouped correctly by the existing well pairing) and
`{450, 50000, 125000}` for the 6 Control wells, with **zero** blocking
validation errors from the dilution field, while `PatientID` values `"1"`–
`"11"` and Control `"Source"`/subject ids populate as they do for a normal
import.

---

## 8. Phase 4 — UI and messaging

**Files touched:** `assay_description_rule_ui.R`, `plate_validator_functions.R`

- [ ] **T4.1** Add the coverage banner (§3.5) and the third radio option.
- [ ] **T4.2** Update the validator message strings (§3.6) to not mention
  `dilution_factor` as a required Description component when
  `dilution_mode == "external"` is in effect for that type.
- [ ] **T4.3** Manual smoke test: upload `GBSIGG-WP4-417_test.srbx` through
  the actual Shiny app (`app.R`) locally and confirm the banner, the
  pre-selected mode, and a clean (error-free) shape confirmation.

---

## 9. Phase 5 — Tests

**Files touched:** `test-assay-description-parse.R`, `test-assay-shape-rules.R`,
possibly a new `test-reader-bead-rbx.R` if one doesn't already exist.

Existing test `test-assay-description-parse.R` (~L75) already encodes the
*current* (soon to be superseded for this mode) expectation:
```r
expect_true(any(bad$issues$kind == "missing_required" & bad$issues$field == "TimePeriod"))
```
— make sure any new test for `dilution_mode = "external"` is additive
(a new test case), and double check this existing assertion still passes for
whatever non-external scenario it's actually exercising (it should, since
`"external"` is a new mode, not a change to `"auto"`/`"position"` behavior).

- [ ] **T5.1** Unit tests for `ai_desc_assign(..., dilution_mode =
  "external")`: single bare-numeral token → no anchor captured, no
  `DilutionFactor` in missing-required checks.
- [ ] **T5.2** Unit tests for `rbx_has_authoritative_dilution()`: empty,
  NULL, all-NA, partial, full coverage frames.
- [ ] **T5.3** Regression test fixtures: since the real `.rbx`/`.srbx`
  binaries should not be committed, either (a) write small synthetic binary
  fixtures that reproduce just the sample-record + dilution-field byte
  patterns `parse_samples()` looks for (lean on the structure documented in
  `rbx_binary_parser.R`'s own stage-6 parser), or (b) mark an
  integration-level test as requiring the local dev-data files and have it
  skip gracefully (`testthat::skip_if_not(file.exists(...))`) when they're
  absent — e.g. in CI. Prefer (a) for the unit-level logic, (b) only for a
  full end-to-end smoke test.
- [ ] **T5.4** End-to-end regression against both real files locally
  (not in CI): confirm `PIH_08Mar18...rbx` output is byte-for-byte/row-for-row
  identical to pre-refactor output (G5), and `GBSIGG...srbx` matches the
  Phase-3 acceptance criteria.

---

## 10. Rollout / guardrails

- Ship Phase 1–3 behind the existing per-type rule mechanism (i.e.,
  `"external"` mode has to be explicitly selected, whether by the new
  auto-default logic or manually) — there's no scenario where a non-rbx
  source silently gets `dilution_mode = "external"` applied, since
  `dilution_map_authoritative` can only be `TRUE` for a batch that actually
  came from a `.rbx`/`.srbx` reader.
- Keep the auto-default (T3.2) overridable in the UI (§3.5) so a user who
  disagrees with the instrument value for some reason isn't locked out of
  the old text-based behavior.
- After Phase 3 lands, re-confirm G5 explicitly: reload the PIH reference
  file and diff the resulting `plates_map`/`assay_response_long` against a
  pre-refactor run.

---

## 11. Open questions for the user (not blocking Phase 0 start)

1. Are there other known labs/instruments feeding `.rbx`/`.srbx` files whose
   `Description` convention we should sample now, to make sure Phase 4's UI
   default heuristic (auto-select "external" whenever coverage > 0) doesn't
   surprise a lab that *does* write good text and also happens to have a
   populated (but perhaps less trustworthy — e.g. operator-entered curve
   dilution placeholder) binary dilution field? Worth a quick look at 2-3
   more real files if available before hard-coding the auto-default
   direction in T3.2.
2. Should `dilution_source` (§3.1) be surfaced anywhere in exported
   output (e.g. the final CSV/long table), so a downstream analyst can tell
   which wells' dilutions came from the instrument vs. text vs. default?
   Not required for this plan's goals, but cheap to add while the plumbing
   is being touched (Phase 3).

---

## Phase 0 Findings

**Headline: the plan's root-cause file is largely dead code for bead/.rbx
uploads, and Goal G1 ("use the instrument dilution_map whenever numeric") is
actively wrong for Standards. Both are load-bearing corrections — see below.**

### D1 — Is `dilution_map` consulted in `build_plates_map()`?

No, and it never was. `build_plates_map()` (`generate_layout_template_ref.R`
L743) only has two branches:

1. `resolved` supplied (non-NULL, >0 rows) → `ai_merge_resolved()` copies
   identity + dilution straight from the pre-processor's confirmed output.
   **This is the branch bead/.rbx always takes** (see D3).
2. `resolved` NULL/empty → the "legacy" branch calls `parse_all_descriptions()`
   → `ai_desc_assign()` / `ai_parse_description_row()` in
   `assay_description_parse.R`. **This is the branch the plan's entire §1.1
   diagnosis is written against — and bead/.rbx never takes it.**

`dilution_map` itself is parsed correctly by `reader_bead_rbx.R` and threaded
into `template_seed`/`build_opts()`, but is then used in exactly one place
(`reader_bead.R` L159, `.bead_parse_layout()`), and only as a **boolean
gate** (`!is.null(opts$dilution_map)`) to decide whether to attempt an
unrelated text-regex extraction (`.rbx_dil_from()`) of a `"1:N"` ratio out of
the *already-resolved* `specimen_source`/`subject_id` columns of the
completed, re-uploaded template. The numeric `dilution_map$dilution` values
themselves are **never joined onto `plates_map` by `(plateid, well)`
anywhere in the codebase.** They are parsed, carried around, and discarded.

### D3 — Which code path actually resolves identity/dilution for bead/.rbx today?

Not `assay_description_parse.R`. `descriptor_bead.R` sets
`preprocess = TRUE` (same for `descriptor_elisa.R`; `descriptor_flow.R` has
no such flag). In `assay_import_module.R`, `use_pre <- isTRUE(descriptor$preprocess)`
is `TRUE` for every bead upload (xPONENT, raw .xlsx, *and* `.rbx`/`.srbx` —
they all share one descriptor), so `resolved_wells = shape$resolved()` is
always supplied and `build_plates_map()` always takes the `ai_merge_resolved()`
branch (D1, branch 1).

`shape$resolved()` comes from `ai_shape_server()` (`assay_shape_ui.R`), which
is **Stages 2–3 of an entirely separate, newer parsing engine**
(`assay_shape_rules.R`, 1203 lines) — not `ai_desc_assign()`/
`ai_parse_description_row()`. Its own header comment states this explicitly:
*"The drafted model (assay_description_parse.R) holds ONE positional element
order per specimen type. That cannot describe a batch whose controls arrive
as 'QC 1', 'SPIKE A' and 'LtyUp'..."* It reuses only two pure primitives from
`assay_description_parse.R` (`ai_desc_split()`, `ai_parse_dilution()`); the
allocation/anchor logic the plan wants to patch (§3.2/§3.3, T2.2/T2.3) lives
in `ai_propose_bindings()` here instead, under a completely different model
(per-shape **bindings**: `slot | pattern | constant | from_type | ignore`,
not a single positional "greedy field" order).

**`assay_description_rule_ui.R` (the plan's Phase 4 target) is dead code.**
`ai_rule_ui()`/`ai_rule_install()` are defined but never called anywhere else
in `src/` — grepped, zero call sites. The live per-type rule UI for bead
imports is `assay_shape_ui.R`'s Stage-2/3 screen, not this file.

**The good news:** `assay_shape_rules.R` already fixes the Type-X corruption
bug the plan diagnosed in §1.1, by design, with a comment (L432-440) that
could have been written for this exact plan: *"FOR TYPE X, A BARE INTEGER IS
NEVER PROPOSED AS THE DILUTION... Proposing the only integer as a dilution
ate the subject id and left PatientID unbound... Only an explicit ratio
(1:N) binds X's dilution here."* Confirmed by trace: a lone token `"1"` for
a Type-X well binds to `PatientID`, not `DilutionFactor`; `TimePeriod` is
left unbound (shows as incomplete, not silently wrong). `DilutionFactor` is
also already excluded from the `missing_required` check for *X* in the old
engine too (`assay_description_parse.R` L251-262, L258
`setdiff(order, "DilutionFactor")`) — T2.3 of the plan is already done, just
in a file that isn't on the live path.

**The remaining gap, confirmed by trace against both real files:** neither
engine, nor `reader_bead.R`'s regex injection, ever consults the binary's
own numeric `dilution_map`. For S/B/C, `AI_TYPE_REQUIRED` still requires
`DilutionFactor` to resolve from **text** (ratio, or for S/B/C only a bare
integer) or the shape blocks approval. For GBSIGG's bare tokens (`"QC1"`,
`"S1"`), nothing proposes a value — the user must hand-type a `constant`
binding per shape (11 distinct Standard shapes + 3 Control shapes). This is
*correctly non-blocking-but-tedious* rather than silently wrong, which is
better than the plan assumed, but the true values the instrument captured
are sitting unused the whole time.

### D4 — Empirical dilution_map coverage (ran the actual parser against both real files)

Wrote a standalone harness sourcing only `rbx_binary_parser.R` +
`process_rbx_files()` (no Shiny/DB) and ran it against both files in `dev/`.
Results, joined `dilution_map` back onto `combined_plates` by `(plateid, well)`:

| File | Type | binary `dilution` value | Text (Description) |
|---|---|---|---|
| PIH (reference) | S1–S10 | **always `1`** | `"Inhouse Ref 1:150"` … `"1:2952450"` (true value) |
| PIH (reference) | B | `1` (correct — blanks are neat) | `"B"` |
| PIH (reference) | X | **true value** (100/1000/5000/10000), matches any text ratio present (e.g. `"QC1 (Low) 1:2500"` → `2500`) | mixed; some have no ratio |
| GBSIGG | S1–S11 | **always `1`** | `"S1"`…`"S11"` (no ratio, no usable text at all) |
| GBSIGG | C1–C3 (QC1–3) | **true value** (450 / 50,000 / 125,000) | `"QC1"`/`"QC2"`/`"QC3"` (no ratio) |
| GBSIGG | X | **true value** (500 / 5,000 / 50,000) | bare `"1"`–`"11"` (no ratio) |

Coverage is 100% (every row has a non-NA `dilution` field) in **both** files
— so the plan's `rbx_has_authoritative_dilution()` coverage check (§3.4)
would report full coverage for Standards too, which is the trap:

**The binary's numeric `dilution` field is always `1` for Type S in both
real files — a hardware/format limitation, not a parsing bug.**
`rbx_binary_parser.R`'s own header says why: *"NOT in the binary (need the
XML export): observed/expected concentrations and the standard-curve fit
coefficients."* Bio-Plex Manager's binary simply does not store a
standard-curve point's defined dilution numerically; it's a stable
per-record placeholder. `reader_bead.R`'s comment ("its numeric dilution
field is 1 for standards, so the ratio is authoritative source") is correct;
`reader_bead_rbx.R`'s comment ("standards carry the curve dilution") is
stale/wrong and should be corrected when this is touched.

**Consequence for the plan as written: implementing G1 literally — "when
dilution_map has a usable numeric value, use it" — would be a severe
regression for Standards.** It would silently overwrite the PIH file's
correct, text-derived standard dilutions (150 … 2,952,450) with `1` for
every Standard well, destroying the one type of well precision-weighted
curve-fitting depends on most. **G1 must be type-scoped: instrument
`dilution_map` is authoritative for X (samples) and C (controls); it must
be explicitly excluded for S (standards) and the existing B handling, which
is already correct via the `constant = "1"` proposal.**

**For GBSIGG specifically, Standards have no machine-recoverable dilution
at all** — not in the binary (always `1`), not in the text (`"S1"`…`"S11"`,
no ratio). No code change to this pipeline can conjure the true 11-point
series from this file. The only paths are: (a) the user hand-types a
`constant` dilution per Standard shape (already possible today, just
tedious — 11 clicks), or (b) a future enhancement outside this plan's scope
(XML-export ingestion, or a study-level "standard curve dilution series"
input the pre-processor could pre-fill from). **This needs a decision from
you before Phase 3 (see open question below) — it changes what G1–G3 can
promise for Standards.**

### D2 — Where `specimen_dilution_factor` is consumed downstream

Outside the parse/import files, only `generate_flowjo_layout_template.R`
(flow format, out of scope — no `preprocess`/pre-processor, different
engine entirely) and `test-assay-shape-rules.R` reference it directly by
name. For bead, the resolved value rides `plates_map`/`assay_response_long`
through `assay_import_backend.R`'s `assemble_upload_frames()` into the
committed `calib_*` tables (per [[project-ispi-architecture]], the generic
`CALIB_TABLE_ORDER` export path) — confirmed it's a genuine terminal
output column, not just a validation-display value, so getting it right
does reach curve-fitting, not just the UI.

---

### Revised task list (supersedes Phase 1–5 file targets above)

**Status (2026-10-05): items 1–4 (Samples/Controls instrument override)
are implemented and tested** — `assay_well_inventory.R` (new
`instrument_dilution` column, joined from `dilution_map` in the bead
adapter), `assay_shape_rules.R` (`ai_resolve_one()`'s new
`instrument_dilution` param, type-gated to X/C; `ai_shape_verdict()`/
`ai_ruleset_ready()` respect it per-well), `assay_shape_ui.R` (coverage
banner in `output$overall`), `reader_bead_rbx.R` (comment fixed). Verified
against both real files via a standalone harness (bypassing Shiny):
GBSIGG's Samples/Controls now resolve to 500/5,000/50,000 and
450/50,000/125,000 respectively with zero DilutionFactor errors; the PIH
reference file's Standards are unchanged (still the correct text-derived
150…2,952,450, never the binary's `1` placeholder) — no regression. Added
6 new test cases to `test-assay-shape-rules.R` (146→162 passing; the same
5 pre-existing, unrelated failures remain). **Not yet done**: the
Standards reference-table feature (item 5 below / the whole "Decision"
section) — separate, bigger piece of work, not started.

1. **Phase 1 (plumbing) — mostly as planned**, but the merge/override point
   is `assay_shape_rules.R`'s per-well resolution output (the `res` data
   frame built around L836-900, after `specimen_dilution_factor` is already
   filled from shape bindings), **not** `reader_bead_rbx.R`/
   `build_plates_map()`. Thread `dilution_map` through `ai_well_inventory()`
   (`assay_well_inventory.R` L292, `.ai_inventory_bead()` adapter) so it
   rides on the inventory the shape engine already consumes, keyed on
   `(plate_key, well)` — the join key the inventory already uses internally
   (confirm it lines up with `dilution_map`'s `(plateid, well)`; `plate_key`
   vs `plateid` naming needs reconciling, they may already be the same
   value under different names — check before assuming).
2. **Type-scope the override**: apply the instrument value as an
   authoritative **post-resolution override** for specimen_type ∈ {X, C}
   only. Never for S. B keeps its existing `constant = "1"` behavior
   (already correct, instrument value also happens to be 1 there so this is
   moot either way).
3. **Relax `AI_TYPE_REQUIRED`/shape-approval gating** (`assay_shape_rules.R`
   L51-55, `ai_ruleset_ready()` L915) so a well whose `DilutionFactor` is
   satisfied by the instrument override doesn't block shape approval even
   if no text binding resolves it — this is the real-world equivalent of
   the plan's §3.3 "external mode," just expressed as a per-well
   satisfied-by-instrument flag rather than a `dilution_mode` enum value
   (that enum lives only in the dead legacy file).
4. **UI (G3)** belongs in `assay_shape_ui.R`, not
   `assay_description_rule_ui.R`. Needs: (a) a coverage banner for X/C
   wells ("96/96 wells sourced from the instrument file"), and (b) a
   **separate, honest banner for Standards** when their text can't be
   parsed either — something like "Standard dilution could not be read
   automatically for N wells — enter it manually below" — rather than
   implying the same automatic coverage applies.
5. **`reader_bead.R`'s regex-ratio injection (L145-193)**: leave as-is. Per
   `reader_bead_rbx.R`'s own comment (L44-49), it's intentionally a no-op
   safety net now that the shape engine consumes the ratio earlier — it is
   not the integration point and doesn't need to change.
6. **Fix the stale comment** in `reader_bead_rbx.R` L154-155 ("standards
   carry the curve dilution") — it's wrong per D4 and will mislead the next
   person who reads it.
7. Tests (plan's Phase 5) should target `assay_shape_rules.R`'s resolution
   function and `ai_well_inventory()`'s bead adapter, not
   `assay_description_parse.R`.

### Decision (2026-10-05, revised): experiment-scoped label→dilution reference, as a new generalizable import stage

Superseded the ordinal-series idea above. Three corrections from the user,
each grounded in a real finding:

1. **Don't assume `S<integer>` labeling.** Checked `.rbx_type_from()`
   (`reader_bead_rbx.R` L57-68): it derives the type code by stripping
   non-digits from the instrument's internal `sample_label`
   (`num <- gsub("[^0-9]", "", lab)`). If a lab's label has no digits at
   all (e.g. `"StdHigh"`, `"Top Std"`), **every one of that lab's standards
   collapses to the single type code `"S"`** — the positional "S1..S11"
   index the previous design leaned on isn't guaranteed to exist, let alone
   stay in a stable order. Confirmed this is a real, independent gap, not
   hypothetical.
2. **No `dilution_source` in exported output.** Confirmed — it stays a
   transient, import-time-only UI signal (G3's banner), never threaded into
   `plates_map`/`assay_response_long`/the committed `calib_*` tables.
3. **Experiment scope required from the start**, because a study commonly
   runs different standard-curve designs across its experiments. Dropped
   the "study-level default, override later" framing.

**Corrected key: the raw Description text, not a type-code suffix or
position.** Whatever string distinguishes the well to a human (`"S1"`,
`"StdHigh"`, `"Inhouse Ref 1:2952450"`) is the match key, paired explicitly
with the dilution value the user supplies — an ordinal position is never
inferred.

#### Storage

One new `calib_settings_meta` row: `param_name = "standard_dilution_reference"`,
`param_data_type = "character"`. Scope **requires** `experiment` (not just
project+study) — enforced the same way every other scoped setting already
is, via `.validate_scope_ladder()`. `feature`/`antigen` stay wildcarded
(`'__none__'`): a well's dilution doesn't depend on which antigen column is
being read off it.

Value: a JSON array of `{description, dilution}` pairs, e.g.

```json
[{"description":"S1","dilution":500},
 {"description":"S2","dilution":1500},
 {"description":"S3","dilution":4500}]
```

serialized/parsed with `jsonlite` (already a dependency; `assay_shape_rules.R`
already uses it for profile save/load, so this isn't a new library). This
isn't a novel use of the table, either — `calib_settings` already stores a
serialized list in one `param_character_value` for `model_form_list`
(`"logistic4,logistic5,..."`, parsed by `parse_model_form_list()` in
`calib_data_access.R`). A JSON array of pairs is the same idea, one level
richer because this setting needs two fields per entry instead of one.

(Tangential, not in scope: `calib_data_access.R` L201-224 has a *commented-out*,
superseded function referencing a `standard_curve_concentration` field from
the old `antigen_feature_settings` table — a single stock-concentration
scalar per antigen/feature, a different-but-related concept [the "C₀" you'd
combine with a dilution series to back-calculate each point's absolute
concentration]. Not this plan's problem to solve, just flagging the
adjacency since it confirms this kind of per-experiment calibration
metadata already has precedent and appetite in this app.)

#### New pipeline stage: "Stage 1.5" between grid and shape

This is the generalizable piece. A new Shiny module
(`assay_std_reference_ui.R`, name TBD), parameterized by `specimen_type`
(default `"S"`, so Controls or another type can reuse it later without new
code), mounted in `assay_import_module.R` alongside the existing two:

```r
grid   <- ai_plate_grid_server("grid", inventory_rv, detected_n_wells)
stdref <- ai_std_reference_server("stdref", inventory_rv,
              enabled = reactive(isTRUE(grid$ready())),
              scope = scope, specimen_type = "S")
shape  <- ai_shape_server("shape", inventory_rv,
              enabled = reactive(isTRUE(grid$ready()) && isTRUE(stdref$ready())),
              assay = reactive(descriptor$assay),
              reference = stdref$reference)   # new arg
pre_ready <- reactive({
  if (!use_pre) return(TRUE)
  isTRUE(grid$ready()) && isTRUE(stdref$ready()) && isTRUE(shape$ready())
})
```

**Detection (a lightweight pre-scan, decoupled from the interactive binding
engine — doesn't need the full shape-table machinery):** for every distinct
raw Description string among `specimen_type` wells in the current batch:

1. Skip (auto-resolved, no entry needed) if a binary `dilution_map` value
   covers it **and the type is eligible for that source** (today: X/C only,
   never S — see the Phase-2 type-scoping above; this pre-scan respects the
   same exclusion).
2. Skip if any token in the description parses as a dilution **ratio**
   (`ai_parse_dilution()`, reused as-is — already proven correct for the
   PIH file's `"Inhouse Ref 1:2952450"` convention).
3. Skip if an experiment-scoped reference entry for this exact description
   already exists (repeat batch in the same experiment — no reprompt).
4. Otherwise: **needs entry.** Surfaced in a simple editable table,
   pre-populated with the distinct not-yet-resolved descriptions found in
   *this* batch, blocking `stdref$ready()` until every row has a value.

This is intentionally assay-agnostic: it only touches `inventory_rv()` (the
already-normalized well inventory every pre-processor-enabled format
produces) and the existing `ai_parse_dilution()` primitive — nothing
bead-specific. ELISA (also `preprocess = TRUE`) is a drop-in second
consumer. Flow currently has no `preprocess` flag at all
(`descriptor_flow.R`), so it's out of reach without separately opting flow
into the pre-processor — noted, not acted on, out of this plan's scope.

#### Resolution precedence (final)

1. Binary `dilution_map` (finite, positive) — **X and C only**, never S
   (D4's finding: always `1` for Standards, a Bio-Plex format limitation).
2. Text ratio via the existing shape-engine proposal — any type, unchanged.
3. **New**: experiment-scoped reference table, matched by exact Description
   text — fills whatever's still unresolved. Not hardcoded to type S; any
   type whose `DilutionFactor` survives steps 1-2 unresolved is eligible,
   per the generalizable-pattern instruction.
4. Otherwise: blocked, same as today (shape approval gate).

Applied as a per-well override alongside/after shape resolution — the same
architectural slot as the X/C instrument override (revised task list item
2 above), not retrofitted into the shape engine's `slot`/`pattern`/
`constant`/`from_type` binding-kind system. The binding model stays
untouched; this is a layer on top of it.

#### Persistence / merge behavior

`set_setting()` replaces a row's value wholesale — there's no partial
JSON-field update at the SQL layer (confirmed from its `INSERT ...
ON CONFLICT DO UPDATE`, which overwrites the whole
`param_character_value`). So when a later batch in the same experiment
introduces a previously-unseen description, the merge has to happen in R:
read the existing JSON via `fetch_study_configuration()`/
`resolve_settings_scoped()`, append the new `{description, dilution}`
pairs the user just entered, re-serialize, write back the full array.

#### Open, non-blocking flags for Phase 1 of this sub-feature

- Match is exact-string on trimmed Description text. Case/whitespace
  normalization rules (if any) should match whatever `ai_shape_table()`
  already does for grouping, so the same well isn't treated as "new" by
  one mechanism and "known" by the other.
- A description used by two different specimen types (unlikely, but
  possible with free-text) must not cross-apply — scope each reference
  entry by `(specimen_type, description)`, not description alone.

This is additive scope beside the revised task list above, not a
replacement for it — X/C instrument-sourcing (items 1-3 above) is
independent, smaller, and lower-risk, and should land first.

### Status (2026-10-05): implemented and tested

Built on top of the X/C override (which was already in place). Files:

- **Migration** (written, **not yet applied to the DB** — flagged for
  the user to run deliberately):
  `i-spi-compute/worker/migrations/005_standard_dilution_reference.sql`
  — registers `standard_dilution_reference` in the already-live
  `calib_settings_meta` table (confirmed to exist via
  `dead/migrate_study_config_cascade.sql`'s DDL, already applied
  historically). `param_control_type = 'textInput'`, which
  `settings_cascade_ui.R`'s generic editor already renders as a safe
  plain-text fallback for any control type it doesn't specifically
  recognize — confirmed by reading `.value_control()`.
- **`src/assay_std_reference_rules.R`** (new, pure logic): parse/serialize
  the JSON `{description, dilution}` array, the candidate pre-scan
  (`ai_std_reference_candidates()` — ratio detection via the existing
  `ai_token_class()`, delimiter-independent since `:`/`/` are always
  reserved), and the last-write-wins merge.
- **`src/assay_shape_rules.R`**: `ai_resolve_one()` gained a second,
  lower-priority `reference_dilution` param (instrument > text >
  reference; **not** type-gated, unlike the instrument override).
  `ai_resolve_inventory()`, `ai_shape_verdict()`, `ai_ruleset_ready()`
  all thread it through the same way `instrument_dilution` already
  flowed.
- **`src/assay_std_reference_ui.R`** (new Shiny module, "Stage 1.5"):
  loads the experiment's saved reference via `resolve_settings_scoped()`,
  shows only genuinely unresolved descriptions in an editable DT table
  (mirroring `source_alias.R`'s cell-edit pattern), blocks on Save until
  every row has a positive value, merges and writes back via
  `set_setting()`.
- **`src/assay_shape_ui.R`** / **`src/assay_import_module.R`**: `reference`
  threaded through `ai_shape_server()` the same way `instrument_dilution`
  already was; new stage mounted between grid and shape, gating both
  `shape$enabled` and `pre_ready()`; UI steps renumbered 1-9 (pre-processor
  path) / unchanged 1-6 (plain path).
- **`src/app.R`**: two new `source()` lines.
- Tests: `test-assay-std-reference-rules.R` (new, 35 passing) +
  additions folded into `test-assay-shape-rules.R` already counted in
  its 162.

**Verified against both real files** (standalone harness, no Shiny):
GBSIGG's 11 Standard descriptions all correctly flagged as needing
manual entry (no ratio, no instrument coverage — type-gated correctly,
confirmed the gate isn't fooled by a populated-but-ineligible
`instrument_dilution` column); after a synthetic reference is supplied,
every Standard well resolves with zero remaining `DilutionFactor`
errors. The PIH reference file's Standards need **zero** manual entries
— their existing ratio text already resolves them, so no regression,
no unnecessary prompting for a lab that already writes good
descriptions.

**Not done / explicit follow-ups for whoever picks this up next:**
- The migration file exists but has not been run against the database.
- No manual Shiny smoke test yet (real browser, real DB) — only the
  pure-logic layer was exercised outside the app; the module's Shiny
  wiring (DT cell-edit handler, `set_setting()` write path, the
  conditionalPanel show/hide) has not been clicked through.
- `assay_std_reference_ui.R` is written generically (`specimen_type`
  param) but only ever mounted for `"S"` in
  `assay_import_module.R` — Controls or another type could reuse it by
  mounting a second instance, not by changing the module itself.
