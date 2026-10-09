# Phase 4 — Help-System Architecture Recommendation

Status: Draft for review
Date: 2026-10-09
Scope: §7 of `dev/ISPI_HELP_SYSTEM_ASSESSMENT_PLAN.md`

---

## 0. Framing: generalize `help_utils.R`, don't design a new mechanism

Phase 0 found a working, in-use help engine already in the app: `src/help_utils.R` + `src/help/settings/*.md`, currently scoped to the settings cascade only. It already gets the two hardest calls right, and gets them right for reasons specific to *this* app, not generically:

- **Modal, not popover or side panel** — proven necessary, not just preferred: a code comment in `help_utils.R` states the app is a shinydashboard (Bootstrap 3) page, so a `bslib::popover` (Bootstrap 5) would render but never activate. This resolves plan §7.2's "evaluate trade-offs" framing before this document even starts — there's no live trade-off to weigh, the alternatives don't work in this app.
- **Markdown + YAML frontmatter, one file per concept, concept-keyed rather than control-keyed** — already matches the plan's §7.1 proposal almost exactly (`title`, `body`, `references`; missing only `see_also` and `category`).
- **A click-triggered `Shiny.setInputValue` → `showModal`**, not an always-visible tooltip — appropriate for this app's already-dense settings table and tab layout, where an always-open explanation per row would be a lot of simultaneous on-screen text.

Every recommendation below is written as **extending this engine app-wide**, not replacing it. Section 0.1 is the one correction needed to the *existing* code before extension, found in Phase 1.

### 0.1 Fix before extending: `std_curve_calc_module.R`'s hardcoded measurement-error modal

Phase 1 found `help/settings/precision_measurement_error.md` (the existing help note) and a hardcoded `showModal()` in `std_curve_calc_module.R` (`meas_err_help`) independently explaining the same measurement-error toggle — two copies of the same prose, exactly the drift problem this whole project exists to prevent. When this generalization work starts, that hardcoded modal should become a call to the (generalized) help engine using the existing `precision_measurement_error` concept, not a second migration target. Treat this as the first real migration and a template for the rest.

---

## 1. Content storage and keying (§7.1)

### 1.1 File layout

Keep **one markdown file per concept**, YAML frontmatter + body, under `src/help/`. Generalize the directory structure from the settings-only `help/settings/` to category subdirectories, for human navigability while authoring — **not** as the lookup mechanism (the lookup stays id-keyed, same as today, so moving a file between directories later is never a breaking change):

```
src/help/
  settings/      (existing — unchanged)
  qc/            (bead count, dilution series, standard curve, precision weights)
  import/        (assay-type import, the per-type description rule, dilution-source precedence)
  glossary/      (terms)
  overview/      (1-2 entries: "about the statistics", app orientation)
```

### 1.2 ID convention

Keep the plan's dotted convention — `qc.bead_count.min_threshold`, `glossary.precision_weight`, `compute.standard_curve.model_form` — as the frontmatter `id:`. The leading segment should match the category directory (`qc.*` lives under `help/qc/`) as a human convention, enforced by the lint check in §3, not by code that parses the id string.

### 1.3 Schema — extend, don't replace

Today's note schema (`id`, `title`, `audience`, `params`, `references`, body):

```yaml
---
id: qc.standard_curve.model_form
title: Standard-curve model selection
audience: user
category: compute-decision        # NEW — procedural | conceptual | glossary | compute-decision
see_also: [glossary.precision_weight, qc.standard_curve.aic_selection]   # NEW
references:
  - text: "..."
    doi: "..."
---
Neutral, audience-general explanation. 1-3 short paragraphs.

::: more
Optional deeper paragraph(s) -- the bridging content Phase 2 identified
several curveR vignette sections need before they're end-user-readable.
Shown behind a "More detail" toggle in the same modal, not a second file.
:::
```

Two additions:
- **`category`** — directly carries Phase 1's classification (procedural / conceptual / glossary / compute-decision) into the content itself, so a lint check (§3) can cross-verify a UI inventory row's declared category against its linked note.
- **`see_also`** — a list of other `help_id`s. Rendered as a small linked list under the body (and under "more", if present) in the modal footer, same visual treatment as `references` but internal rather than external links.

**`params` stays exactly as it is today, scoped to settings-cascade use only.** It's the one genuinely settings-specific piece of the schema (one concept can cover several `calib_settings_meta` params, e.g. `precision_measurement_error` already covers two) — nothing elsewhere in the app needs that indirection, a help icon outside the settings cascade just references a `help_id` directly (§2.1). Don't generalize `params` into a catch-all "which UI elements" field; keep it meaning what it means today.

### 1.4 The `::: more` block

This is the concrete mechanism for the audience-layering decision in §4: a body that's always shown, plus one optional expandable section in the *same* note (not a second file, not a second modal). Parsed with a simple fenced-div convention (`::: more` / `:::`, already a known pandoc-ish pattern, trivial to regex out before/after the main `shiny::markdown()` call) rather than pulling in a markdown-extension dependency.

---

## 2. In-app linkage mechanism (§7.2)

### 2.1 Icon → modal, generalized

Generalize `settings_help_icon()`/`settings_help_content()` into two app-wide equivalents that any module can call:

```r
help_icon(help_id, ns, audience = "user")      # the "?" / info-circle icon, or NULL if no note
help_modal_body(help_id, audience = "user")    # title + body + [more toggle] + see_also + references
```

Internally these become thin wrappers: `settings_help_icon(param_name, ...)` keeps working unchanged (resolves `param_name` → concept `id` via the existing `by_param` index, scoped to `help/settings/`), while the new `help_icon(help_id, ...)` looks up `help_id` directly in the FULL registry (all categories, loaded once at startup — see §2.3). Both funnel into the same `showModal()` renderer, so there is exactly one modal-rendering code path for the whole app, not two.

### 2.2 Auto-linking glossary terms inside a note's body — recommend building it

The plan's §7.2 explicitly asks for a recommendation rather than leaving this open. **Build it**, as a small addition: a markdown-preprocessing step that turns `[[glossary.precision_weight]]` (or `[[glossary.precision_weight|precision weight]]` for custom link text) into a clickable span wired to the same `help_show` input event, before the body is handed to `shiny::markdown()`. Fall back to plain text (strip the brackets) if the id isn't found in the registry — never a broken link.

Reasoning for building it rather than relying on manual `see_also` links only: this domain has real jargon density (LLOQ, pcov, precision profile, dilution-source precedence…) that will recur inside many *other* notes' prose, and a reader mid-explanation benefits from resolving an unfamiliar term without losing their place — `see_also` at the bottom covers "related reading," not "I don't know what this word means, right here, right now." The implementation cost is one regex pass and reuses the existing click→modal plumbing; there is no new interaction pattern to design.

### 2.3 Registry: load once, merge categories

Replace the single `HELP_SETTINGS <- load_help("help/settings")` (in `global.R`) with a merge across every category directory into one `HELP_REGISTRY`, built once at app startup the same fail-soft way `load_help()` already works (missing directory → warn, empty registry; bad frontmatter → warn, skip that file; duplicate `id` across categories → warn, keep the first). `settings_help_icon()`'s existing `by_param` index continues to be built from the `help/settings/` subset specifically, so settings-cascade behavior is unchanged.

### 2.4 Persistent glossary side panel — recommend against, for now

Not recommended as a new floating/persistent panel. This app's shinydashboard sidebar + tab layout isn't built for an additional always-present panel, and the risk (z-index, scroll, mobile/narrow-viewport layout fights) isn't worth it when the auto-linking in §2.2 already gets a reader from any mention of a term to its explanation in one click. If a dedicated glossary browsing surface is wanted later, the cheap version is a plain tab listing every `category: glossary` note (reusing the existing tab mechanism, zero new UI infrastructure) — worth keeping as a Phase 2/3 content-authoring nice-to-have, not a Phase 4 architecture commitment.

---

## 3. Versioning and sync with code changes (§7.3)

Same problem already identified for the deployment docs (Phase 3), same category of fix: a lightweight, automated check rather than manual diligence.

**Recommend a testthat test**, `test-help-registry.R`, consistent with the existing `test-*.R` convention (`test-assay-std-reference-rules.R` et al., run via `testthat::test_file()`), doing two checks:

1. **Every `help_id` referenced in `src/*.R`** (grepped for the `help_icon("<id>"` / `settings_help_icon`-by-param call pattern) **has a matching note.** A missing note is silent today (`settings_help_icon()` returns `NULL`, the icon just doesn't appear) — fine as graceful runtime degradation, but it should fail a test, not just quietly vanish from the UI.
2. **Every note's declared `category`** (new field, §1.3) **matches what Phase 1's UI inventory classified that control as**, where the inventory is available — catches a note drifting out of sync with what the control actually does (e.g. a `conceptual` note on a control that became a `compute-decision` after a later refactor).

Pair this with one PR-template checklist line (the same mechanism recommended for the architecture docs in Phase 3): *"If this PR changes a control or computation a help note documents, did you update or add the note?"* — cheap, and catches the cases a mechanical check can't (content going stale without the `help_id` itself changing).

---

## 4. Audience layering within one entry (§7.4)

**Resolved by the user (2026-10-09): one neutral explanation with an expandable "more detail" section.** Mechanism: the `::: more` block in §1.4. Concretely:

- **Body** (always shown): neutral, audience-general — written so a lab scientist and a statistician both get a correct, useful answer from it alone.
- **`::: more` block** (behind a toggle, same modal): the deeper paragraph — this is specifically where Phase 2's "needs bridging" vignette content earns its keep: a short bridging paragraph here, then a `references` link to the actual vignette section for full depth, rather than trying to make the vignette section itself the only expansion.
- **`references`** (always shown, below both): external links — curveR vignette sections, papers. Unchanged from today's schema.

The existing `audience: user | dev | both` tag is a **different, orthogonal mechanism** (visibility: does this note show at all to this viewer) and is kept as-is — don't conflate it with the "more detail" toggle, which is about progressive disclosure within a note already shown to the user.

---

## 5. Acceptance against the plan's §7 criterion

One clear proposal per subsection, alternatives noted where they were genuinely weighed (§2.2 built vs. manual-only, §2.4 panel vs. none) rather than left open. Summary table:

| Question | Decision |
|---|---|
| Storage format | Markdown + YAML frontmatter, one file per concept, generalized from `help/settings/` to category subdirectories |
| Keying | Dotted `help_id` (`category.subject.detail`), unchanged convention from the plan |
| New schema fields | `category`, `see_also`; `::: more` block for progressive disclosure |
| Linkage mechanism | Click icon → `showModal()`, generalized from the existing settings-cascade pattern (confirmed correct for this app's Bootstrap 3 base) |
| Glossary auto-linking | Build it — `[[help_id]]` syntax in note bodies |
| Persistent side panel | Not now; a plain glossary-listing tab is the cheap fallback if wanted later |
| Audience layering | One neutral body + expandable `more` block (user-resolved); `audience` tag kept separate for visibility filtering |
| Sync/versioning | `test-help-registry.R` (id-exists check, category-drift check) + a PR-template line |
