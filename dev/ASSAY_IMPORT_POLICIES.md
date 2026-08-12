# Assay-Import Policies, Limits & Conventions (11.10)

Durable record of the rules the unified assay-import subsystem enforces, so they
are not lost or accidentally reverted. Developer-facing. (A separate, planned
**user-facing** guide will explain the concepts and their statistical / real-world
meaning — see "Planned" at the end.)

---

## 1. Antigen-name policy — no slashes, one vocabulary

**Rule.** Antigen names are canonicalised identically everywhere, so the response,
the antigen list, the curve lookup, and the antigen-family rows all agree and no
field contains a slash.

**Mechanism.** `clean_antigen_label()` (batch_layout_functions.R):
`/` → `_`, whitespace → `.`, remaining punctuation (incl. `\`) stripped.
e.g. `Fim 2/3` → `Fim.2_3`.

**Where applied.**
- `build_antigen_df` cleans `antigen_abbreviation` (existing behaviour).
- The shared assembler (`assemble_upload_frames`) applies `.ai_clean_antigen`
  (a wrapper over `clean_antigen_label`) to `assay_response_long$antigen`,
  `plates_map$antigen`, and `antigen_list$antigen_abbreviation` at commit — for
  **all** assays.
- Both validator reconciliations compare cleaned names, so a slash-only
  difference does not raise a false "antigen not in list" warning.
- The `.rbx` reader also cleans analyte names at the source
  (`process_rbx_files`), so its wide columns and response match from the start.

**Properties.** Idempotent (clean names unchanged), NA-safe (flow's
pre-threading antigen passes through). Extendable to other free-text fields if a
broader no-slash policy is ever wanted.

## 2. Column length limits (validation-enforced)

Over-length values are flagged as **validation errors before commit** (not left
to fail the DB COPY). Enforced in `validate_layout_sheets` via `AI_COL_LIMITS`
with template→DB name mapping (`name_map`).

| DB column | Limit | Template column (mapped) |
|---|---|---|
| `study_accession` | 15 | `study_name` |
| `experiment_accession` | 15 | `experiment_name` |
| `feature` | 15 | `feature` |
| `wavelength` | 15 | `wavelength` |
| `patientid` | 15 | `subject_id` |
| `source` | 25 | `specimen_source` |
| `antigen` | 64 | `antigen` |
| `plate` | 40 | `plate` |
| `plateid` | 100 | `plateid` |
| `nominal_sample_dilution` | 128 | `nominal_sample_dilution` |

`patientid` is **never auto-truncated by the assembler** (it is an identifier;
silent trimming could merge distinct subjects). Exception: the `.rbx` reader
truncates the parsed name to 15 at the source (see §4) because its descriptions
are instrument-generated, not user identifiers.

## 3. Commit semantics

- **Atomic.** The whole import commits inside one `pool::poolWithTransaction`;
  any failure rolls back everything.
- **curve_lookup is hard-fail.** If standards are present and curve-lookup
  registration fails, the transaction aborts (no silent partial curve state).
- **Insert safety.** `.insert_or_die` flattens list-columns to scalar strings
  (a list-column makes RPostgres drop the connection) and trims each frame to the
  table's real columns from `information_schema` (a stray column such as ELISA's
  `time_unit` would otherwise fail the COPY).
- **Empty wells.** Rows with `NA` `specimen_type` (empty wells) are excluded
  from every specimen frame (`!is.na(st) & st == prefix`) — an `NA` in the subset
  index would otherwise inject all-NA rows that fail NOT-NULL checks.
- **nominal_sample_dilution is test-sample only.** `calculate_nominal_sample_dilution`
  filters `specimen_type == "X"`; nsd never reflects standards/blanks/controls,
  even when those carry real dilutions.

## 4. `.rbx` (Bio-Plex binary) specifics

- **Dilution source.** The binary's numeric `dilution` field is unreliable for
  standards (it is 1). The authoritative dilution is the `1:N` ratio embedded in
  the sample description (standards: `Inhouse Ref 1:2952450`; diluted samples:
  `QC1 1:2500`). `process_rbx_files` parses `N` (`.rbx_dil_from`), strips the
  ratio from the name (`.rbx_strip_ratio`), truncates the name to 15, and builds
  the Description **order-aware** (`.rbx_build_description`): the name goes to the
  PatientID (X) / Source (B/S/C) slot and `N` to the DilutionFactor slot, per the
  user's `element_order` / `bcs_element_order`. So `build_plates_map` fills
  `specimen_dilution_factor` at **template generation** — no manual editing — and
  the standard-curve dilutions land in `xmap_standard.dilution`.
- **acquisition_date — ACKNOWLEDGED LIMITATION.** The `.rbx` binary does not
  carry an acquisition timestamp. The reader falls back to the uploaded file's
  modification time (in an accepted date format) so validation passes. This is
  **not** the true acquisition time; correct it in the plate_id sheet during
  template review if it matters downstream. (If the timestamp is later found to
  be decodable from the binary, extend `rbx_binary_parser.R` and remove the
  fallback.)
- **file_name / plateid.** `file_name` uses the binary's real `source_path`
  (needs directory separators for the metadata validator); `plateid` is keyed on
  the file name so header and plate map join consistently.
- **Ratio helpers single home.** `.rbx_dil_from` / `.rbx_strip_ratio` are defined
  once in `reader_bead.R` (sourced before `reader_bead_rbx.R`).

## 5. Feature (isotype)

- **Bead & flow require a feature.** The bead/flow tabs have a Feature (isotype)
  input; it is written into `assay_response_long$feature` and is part of the
  per-`(antigen, feature)` antigen-family landing and the curve-lookup key. If it
  is blank, validation refuses (bead) or the derived feature is used (flow's
  per-feature experiment split).
- **ELISA derives feature from the data** (no feature input).

## 6. Developer conventions

- **Never use `%||%` on a non-scalar.** The app's global `%||%` is comparison-
  based (`x == ""`), which errors on any length≠1 operand
  (`'length = N' in coercion to 'logical(1)'`). Use explicit
  `if (is.null(x)) default else x` for vectors, objects, function values, and
  validator `messages`/`warnings`. (This class of bug recurred five times during
  11.10; the stack is now clean.)
- **Pluggable readers.** A new format is added by registering a reader
  (`new_assay_reader` + `register_assay_format`) with `parse_raw` / `make_template`
  / `parse_layout` / `validate_sheets` / `assemble`; the contract, backend, and
  module do not change. `.rbx` was added this way.

---

## Planned — user-facing guide (next step)

A separate document/UI help is planned to explain, for end users, the **concepts,
limits, policies, and the statistical / real-world meaning** of the settings in
the cascade and the import UI (e.g. what nominal sample dilution means and how it
enters concentration back-calculation; what a feature/isotype represents; how
standard-curve dilutions drive curve fitting; why antigen names are normalised;
the 15-character identifier limit; and the `.rbx` acquisition-date caveat). This
policy file is the developer-facing source of truth those user materials should
stay consistent with.
