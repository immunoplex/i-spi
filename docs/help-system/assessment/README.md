# I-SPI In-App Help System — Assessment Summary

Status: Assessment complete, draft for review
Date: 2026-10-09
Plan: `dev/ISPI_HELP_SYSTEM_ASSESSMENT_PLAN.md`

This is the assessment and design phase only — no help content written, no UI built. It produces an inventory and a set of recommendations for you to review before Phase 2 (content authoring + UI integration) starts.

## What's here

| File | What it is |
|---|---|
| [`00-repo-inventory.md`](00-repo-inventory.md) | Which repos/packages are actually in play, and where they diverged from the plan's assumptions |
| [`01-ui-inventory.md`](01-ui-inventory.md) | Every tab/sub-tab in the live app, with a help-candidate call per row |
| [`01a-decision-points.md`](01a-decision-points.md) | Deep-dives on the 5 controls that actually change what gets computed |
| [`02-curveR-topic-index.md`](02-curveR-topic-index.md) | What the 5 curveR packages' own docs already cover, topic by topic, and whether each is linkable as-is |
| [`03-deployment-doc-reconciliation.md`](03-deployment-doc-reconciliation.md) | Deployment docs/manifests brought in line with the required i-spi-compute tier |
| [`04-architecture-recommendation.md`](04-architecture-recommendation.md) | How the help system should work technically — extending the engine already in the app, not building a new one |
| [`05-help-content-mapping.csv`](05-help-content-mapping.csv) | The actual working input for content authoring: one row per help candidate |
| [`05a-glossary-seed.md`](05a-glossary-seed.md) | Glossary terms, each marked card vs. its own entry |

## Headline finding

**A help-content engine already exists in this app** (`src/help_utils.R` + `src/help/settings/*.md`), currently scoped to the settings cascade only, and it already gets the hard calls right for reasons specific to this app (a modal works, a popover provably doesn't, because the app runs Bootstrap 3). Everything in this assessment is framed as *generalizing that engine app-wide*, not designing a new mechanism. The architecture recommendation (04) spells out exactly what extends and what's new.

## Scope covered

- **46** tab/sub-tab rows walked from `ui_handler.R` down through every module it mounts, **40** marked help-candidate.
- **5** compute-decision controls fully deep-dived (what changes downstream, existing rationale docs, authoritative curveR source, precedence order where one exists) — covering **9** inventory rows.
- **21** topics indexed across the in-scope curveR ecosystem (`curveR`, `curveRcore`, `curveRfreq`, `curveRbayes`, `curveRweights` — see scope note below), each with a specific vignette section and an explicit linkable-as-is-or-needs-bridging call.
- **40** help-candidate rows mapped to a `help_id`, category, candidate source material, and priority — full coverage of Phase 1's Y rows, verified by count.
- **40** glossary terms seeded, 9 of them substantial enough to warrant their own conceptual entry rather than a one-line card.
- **15 of 40** mapping-table rows have no existing source material to link to yet (`NEW` in the `candidate_source_material` column) — the actual gap list content authoring needs to write from scratch, as opposed to the majority that are mostly linking + light framing.
- Deployment docs and manifests brought in line with the required i-spi-compute tier, including a manifest (`i-spi-compute.yml`) that didn't exist at all until this pass, and the `calib_*` database schema now vendored into `deploy_ispi/db-dumps/`.

**Scoped out, by your direction:**
- Dilution analysis and outlier detection (dead code behind a commented-out UI tab, pending a major refactor) — seeded minimally in the glossary so the terms are ready, not built out further until that feature returns.
- **`curveRmetrics`, amended 2026-10-09.** The curveR ecosystem this assessment covers is `curveR`, `curveRcore`, `curveRfreq`, `curveRbayes`, `curveRweights` only, matching the plan's original assumption. `curveRmetrics` was briefly in scope earlier in this assessment (visible as a historical record in 00/02/05/05a, each marked with an amendment note) and has been removed: 7 `curveRmetrics`-only topics dropped from 02, 1 replacement "Detection limits" topic added citing `curveRcore::compute_detection_limits()` instead, and the mapping/glossary rows that cited `curveRmetrics` (the LOD/RDL/MDC/inflection-point family) updated to cite in-scope sources or flagged for from-scratch authoring where no in-scope source exists.

## Discrepancies worth your attention (already flagged in-line, collected here)

1. The plan's named description-parsing engine (`assay_description_parse.R`) is dead code; the live engine is `assay_shape_rules.R` — confirmed independently, matches what `RBX_DILUTION_AUTHORITATIVE_SOURCE_PLAN.md` already corrected.
2. A live instance of the exact problem this project exists to prevent: `help/settings/precision_measurement_error.md` and a hardcoded modal in `std_curve_calc_module.R` independently explain the same control. Flagged as the first real migration target once generalization work starts (04 §0.1).
3. The "cluster of i-spi-compute clones" question is resolved with code-level evidence: genuinely independent stacks (own API/Redis/worker/URL/key), not just worker replicas — confirmed via `compute_cluster_registry.R`, and **now applied directly to `ARCHITECTURE.md`** (new §3.6).
4. The `calib_*` database schema gap (neither test-deployment runbook ever loaded it) is **resolved** — the schema file is vendored into `deploy_ispi/db-dumps/calib_schema_v1.sql` and both runbooks load it.

## Open questions — resolved during this assessment (§10 of the plan)

All five of the plan's original open questions were resolved by you on 2026-10-09 and are recorded in full in `00-repo-inventory.md`'s §10 edit: modal popup (confirmed as a hard requirement, not just a preference), one neutral explanation + expandable detail, references via Zotero/Better BibTeX auto-export, proceed on direct filesystem access throughout, and fill curveR vignette gaps directly in Lancet style rather than only compensating in app-side text.

**One live open item remains**, surfaced during Phase 5: 8 glossary terms marked for their own entry (e.g. `glossary.model_forms`) don't yet have a row in the mapping table, because they're reached via `see_also` from multiple other entries rather than owned by one UI control. These need rows added to `05-help-content-mapping.csv` during content authoring rather than at this assessment stage — noted explicitly in `05a-glossary-seed.md`'s acceptance-check section.

## Deliverables checklist

- [x] `00-repo-inventory.md`
- [x] `01-ui-inventory.md`
- [x] `01a-decision-points.md`
- [x] `02-curveR-topic-index.md`
- [x] `03-deployment-doc-reconciliation.md` (tracking doc; actual `.md`/`.yml` fixes applied directly where safe, proposed where not — see that file)
- [x] `04-architecture-recommendation.md`
- [x] `05-help-content-mapping.csv`
- [x] `05a-glossary-seed.md`
- [x] This README

## Next step

Your call on when to greenlight Phase 2 (content authoring + UI integration) — the mapping table and glossary seed are the direct working input for it.
