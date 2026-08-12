# Data dictionary — `madi_results.calib_standards` / `calib_blanks`

Persisted by the curveR batch worker (`i-spi-compute`), read-only for the
I-SPI app. One row per **observed well**. The worker writes every standard and
blank point — included *and* masked — with the transforms from
`curveRcore::preprocess_standards()` (v0.3.0+) already applied, so the app can
overlay observed points on the fitted curve with no re-derivation.

Rows are keyed by `curve_id` + `method`; the natural key (study / experiment /
plate / antigen / …) is reachable through `madi_results.curve_lookup`.

## Mask-aware contract (both tables)

* Set-level statistics (prozone peak, blank geometric mean, adaptive log floor,
  min-concentration anchor) are computed from the **included** rows only; the
  resulting transforms are applied to **all** rows. A masked row therefore
  shares the axes of the fitted points but never influenced them.
* `included = FALSE` rows are stored for display; the fit never saw them.
* `response_model` is on the model/fitting scale (e.g. `log10(MFI)` with the
  adaptive floor applied); `assay_response_raw` is the pristine pre-transform
  reading. Both blanks and standards use the **same** response floor.

## `calib_standards`

| Column | Type | Null? | Meaning |
|---|---|---|---|
| `curve_id` | `bigint` | no | FK-style key into `curve_lookup`; identifies the fitted curve. |
| `method` | `varchar(20)` | no | `'bayesian'` or `'frequentist'`. |
| `well` | `varchar(16)` | yes* | Plate well id of the observed standard. Part of the PK, so effectively required. |
| `dilution` | `numeric` | yes* | Dilution factor of the well. Part of the PK, so effectively required. |
| `concentration` | `numeric` | yes | Preprocessed concentration on the **raw** scale (`10^log10_concentration` when the fit uses log concentration). |
| `log10_concentration` | `numeric` | yes | The x value used by the fit (log10 concentration when `is_log_independent`). |
| `response_model` | `numeric` | yes | Model-space response (post prozone / blank / log transform) — the y the fit sees. |
| `assay_response_raw` | `numeric` | yes | Original response (e.g. MFI) before any transform. |
| `included` | `boolean` | no | `TRUE` = entered the fit; `FALSE` = masked. |
| `exclusion_reason` | `varchar(20)` | no | `none` when included; `masked` when excluded. `prozone` / `lod` reserved for future use. Default `none`. |
| `mask_reason` | `varchar` | yes | Free-text reason carried from `xmap_standard` (via `standard_unmasked`); `NULL` for included/unmasked rows. Complements the controlled `exclusion_reason`. |
| `job_id` | `text` | yes | Worker job that wrote the row. |
| `created_at` | `timestamptz` | no | Insert timestamp. Default `now()`. |

**Primary key:** `(curve_id, method, well, dilution)`.

*The synthetic `blank_option = "included"` anchor point (`well = 'blank_mean'`,
`dilution = NULL`) is intentionally **not** persisted here — it is a fit input,
not an observed well, and a NULL `dilution` would violate the PK. It is
recoverable from `blank_geomean` and the min-concentration anchor if needed.

## `calib_blanks`

| Column | Type | Null? | Meaning |
|---|---|---|---|
| `curve_id` | `bigint` | no | Curve the blank belongs to. |
| `method` | `varchar(20)` | no | `'bayesian'` or `'frequentist'`. |
| `well` | `varchar(16)` | yes* | Plate well id of the blank. Part of the PK. |
| `response_model` | `numeric` | yes | Blank response transformed the **same way** as the standards (same floor + log10). |
| `assay_response_raw` | `numeric` | yes | Original blank response, pre-transform. |
| `included` | `boolean` | no | `TRUE` = entered the blank geometric mean; `FALSE` = masked. |
| `exclusion_reason` | `varchar(20)` | no | `none` / `masked` as above. Default `none`. |
| `mask_reason` | `varchar` | yes | Free-text reason carried from `xmap_buffer` (via `blank_unmasked`); `NULL` for included/unmasked rows. |
| `job_id` | `text` | yes | Worker job that wrote the row. |
| `created_at` | `timestamptz` | no | Insert timestamp. Default `now()`. |

**Primary key:** `(curve_id, method, well)`.

`calib_blanks` intentionally has **no** `concentration` / `log10_concentration`.
A blank has no intrinsic concentration; where (or whether) to place it on the
x-axis is a pure display choice (a reference band, a sentinel x, or the
`include_blanks_conc` anchor `min_included_concentration − log10(2)`). That
decision belongs to the app's plotting code, not this contract. The worker
stores only the intrinsic transformed response + status.

## Notes

* **Blanks are never subtracted automatically.** `response_model` here is the
  transformed blank reading, not a subtracted quantity. Blank subtraction from
  the *standards* happens only under the explicit `blank_option` values
  `subtracted` / `subtracted_3x` / `subtracted_10x` (and `included` adds the
  blank mean as a fit point). See curveRcore ≥ 0.3.0.
* **`mask_reason`.** The worker's flattener emits a `mask_reason` column (free
  text carried from `xmap_standard` / `xmap_buffer` via the `*_unmasked` views);
  it is persisted now that both tables carry a `mask_reason varchar` column
  (added by `grant_calib_access.sql`). `exclusion_reason` remains the controlled
  `none|masked|prozone|lod` vocabulary; `mask_reason` is the optional free-text
  detail and is `NULL` for included rows.
