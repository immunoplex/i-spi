# Phase 3 — Deployment / Architecture Doc Reconciliation

Status: Draft for review
Date: 2026-10-09
Scope: §6 of `dev/ISPI_HELP_SYSTEM_ASSESSMENT_PLAN.md`

---

## 0. `ARCHITECTURE.md` and 3 sibling files — status update 2026-10-09

At the time this phase ran, `deploy_ispi` had **uncommitted, in-progress changes** to `ARCHITECTURE.md`, `README-STANDALONE-ISPI.md`, `OFFLINE-IMAGES.md`, and `README.md` (plus `ARCHITECTURE.html`/`README.html` deleted, and a new untracked `batch-calculator.yml` deprecation stub) — someone else's in-flight work, deliberately left untouched at the time. **Since then, on your instruction, §1's proposed content has been applied directly to `ARCHITECTURE.md`** (new §3.6, "Scaling two different ways: worker replicas vs. multiple i-spi-compute stacks") — both paragraphs, including the per-pod `parallel::mclapply` parallelism detail that was in the original proposal but is called out separately here since it's easy to miss. `README-STANDALONE-ISPI.md`, `OFFLINE-IMAGES.md`, and `README.md` remain untouched — still presumed to be your in-flight work.

---

## 1. The "cluster of i-spi-compute clones" — APPLIED to `ARCHITECTURE.md` §3.6

The in-progress `ARCHITECTURE.md` draft already covered **worker replicas** correctly ("Running more worker replicas adds throughput — each pops the same Redis queue"). It did not yet cover the other, separate scaling mechanism the plan asked about. Verified directly against `i-spi-refactor/src/compute_cluster_registry.R` and `compute_api_client.R`; the content below is now live in `ARCHITECTURE.md` §3.6, reproduced here for the record:

It is **not just worker replicas** — I-SPI can route different projects/studies to **entirely separate, independent i-spi-compute stacks** (each its own API + Redis + worker, own URL, own API key), not a shared Redis queue with more consumers. Suggested paste-in, matching the draft's tone (place it right after the worker-replicas sentence in the sizing/scaling discussion):

> **A second, independent way to scale: multiple i-spi-compute stacks.** Beyond running more worker replicas against one Redis queue, I-SPI can also route different projects or studies to **entirely separate i-spi-compute deployments** — each with its own API, its own dedicated Redis, and its own worker(s), not a shared queue. A project/study's `compute_cluster` setting (a free-text label in the settings cascade) selects which one: the label is normalized (lower-cased, `-`→`_`) and matched against `ISPI_COMPUTE_URL__<LABEL>` / `ISPI_COMPUTE_API_KEY__<LABEL>` environment variable pairs; an unset or unrecognized label falls back to the "default" `ISPI_COMPUTE_URL`/`ISPI_COMPUTE_API_KEY` pair every deployment already has. Adding a clone means adding a new env var pair and restarting the I-SPI pod (env vars are read at process startup, scanned fresh on every lookup — no app code change). This is for genuine isolation (a separate compute pool for a different group, site, or workload) rather than throughput — for throughput within one stack, add worker replicas instead.
>
> Within a single worker pod, parallelism is a third, separate layer: `worker_curveR.R` fits multiple curve groups concurrently via forked child processes (`parallel::mclapply`), each opening its own database connection and sized by `plan_parallelism()` from the pod's CPU/memory budget (`WORKER_CORES`, `WORKER_MEM_MB`, clamped by `WORKER_MAX_PARALLEL`) — see `WORKER_PARALLELISM_NOTES.md` in the i-spi-refactor repo for the full tuning detail.

**Source for this content, if you want to verify independently:** `i-spi-refactor/src/compute_cluster_registry.R` (the full label→{base_url,api_key} resolution logic, including the fallback-to-default behavior), `i-spi-refactor/src/compute_api_client.R` (confirms each resolved pair becomes an independent HTTP client — nothing is shared across clusters at the request layer), `i-spi-refactor/WORKER_PARALLELISM_NOTES.md` (the per-forked-child DB connection / fan-out sizing detail).

---

## 2. `TEST-DEPLOYMENT-CIVO.md` and `TEST-DEPLOYMENT-LOCAL.md` — FIXED

Not part of the in-progress diff (not flagged as modified by `git status`), so these were still describing the fully retired model. Fixed directly in both files:

- **Sizing guidance** rewritten: the i-spi-compute **worker**, not I-SPI, is the CPU/memory-heavy component. Since this is a single-node test, I-SPI + i-spi-compute + PostgreSQL all share the node, so the sizing target is unchanged (≥8 vCPU / ≥16GB) but the *reason* and the *default resource requests* (see §3) now reflect reality.
- **`IMMUNOPLEX_REDIS_AUTH`/`IMMUNOPLEX_API_KEY` substitutions** changed from "only if you deploy the optional Batch Calculator" to **required**, now provisioning the `i-spi-compute` Secret (see §3) instead of the retired `batch-calculator.yml`.
- **Deploy sequence**: inserted an `i-spi-compute.yml` apply-and-wait step (Redis → API → worker readiness) between PostgreSQL and I-SPI, matching the real dependency order (I-SPI's env references the `i-spi-compute` Secret and expects the API to be reachable). The Batch Calculator step is now explicitly "retired, do not deploy."
- **Verification step (Phase 7/8)** reworded to describe the full chain (I-SPI → i-spi-compute API → Redis → worker → PostgreSQL → I-SPI) and points at `kubectl logs deploy/i-spi-compute-worker` as the first place to look if a submitted job never completes.
- **Offline image list** (`TEST-DEPLOYMENT-LOCAL.md` Phase 3) updated to include the i-spi-compute images as required, not optional-alongside-whoami.

**Gap found and now RESOLVED:** both runbooks load `db-dumps/i-spi-db.sql` into PostgreSQL but had never applied the `calib_*` schema (`i-spi-compute/db/calib_schema_v1.sql`) that the compute tier and the app's Data tab depend on — confirmed `i-spi-db.sql` contains zero `calib_*` table definitions. This predates the in-process→i-spi-compute transition (the app's `calib_*` tables wouldn't have existed in the old model either) — a previously-undocumented gap, not something this reconciliation introduced. The `psql ... < ../db-dumps/calib_schema_v1.sql` step was added to both runbooks, and **on your instruction (2026-10-09) the file itself is now vendored at `deploy_ispi/db-dumps/calib_schema_v1.sql`**, copied verbatim from `i-spi-compute/db/calib_schema_v1.sql`. Both runbook comments were updated to drop the "copy this file first" caveat accordingly. Keep it in sync with the source repo if that schema changes — there's no automated sync, it's a plain vendored copy.

## 3. `k8s-manifests/i-spi.yml` — FIXED (env wiring added)

This is exactly the plan's named item: `README-STANDALONE-ISPI.md`'s in-progress correction notes already describe `ISPI_COMPUTE_URL`/`ISPI_COMPUTE_API_KEY` as present, but the manifest itself only had a **commented-out** stub (`#   API_KEY: IMMUNOPLEX_API_KEY`) and no env vars wired into the Deployment at all. Fixed:
- Added `ISPI_COMPUTE_URL` (value `https://IMMUNOPLEX_HOSTNAME/i-spi-compute`) and `ISPI_COMPUTE_API_KEY` (`secretKeyRef` to the new `i-spi-compute` Secret, see below) to the Deployment's env list.
- Rewrote the file's header comment block and the "Batch / Bayesian fitting" footer comment, both of which still described in-process fitting.
- Resized `resources` down from the old "I-SPI is the heavy component" values (2 CPU/4Gi requests, 8 CPU/12Gi limits) to values appropriate for a now-light Shiny front end (500m/1Gi requests, 2 CPU/3Gi limits) — raise these for concurrent-user load, not for fitting.
- Removed the stray `API_KEY` stub from the `i-spi` Secret (the value now lives once, in the new `i-spi-compute` Secret, referenced by both `i-spi.yml` and `i-spi-compute.yml` rather than duplicated).

**I did not attempt a blind copy from `i-spi-refactor/dev/i-spi.yml`** (confirmed authoritative per your Phase 0 resolution) — that file uses a `SealedSecret` bound to the production cluster's sealed-secrets controller key and production-specific `ISPI_COMPUTE_URL__SIM` wiring for a second clone; neither is portable to a fresh test cluster. Instead I reproduced the *semantics* (the env vars I-SPI actually needs) using this folder's own established pattern (plain `Secret` + `IMMUNOPLEX_*` sed placeholders), which is what every other secret in this folder already does.

## 4. `k8s-manifests/i-spi-compute.yml` — ADDED (did not exist at all)

This was a bigger gap than "sync one file" — **no i-spi-compute manifest of any kind existed in `deploy_ispi/k8s-manifests/`**. Added `i-spi-compute.yml`, adapted from the authoritative `i-spi-refactor/dev/i-spi-compute.k8s.yaml` (production manifest) to this folder's standalone/sed-placeholder conventions:

| Changed from the production manifest | Why |
|---|---|
| Dropped `namespace: madi-preprod` on every resource | This install is single-namespace, applied with `kubectl -n immunoplex` like every other manifest here |
| `SealedSecret` → plain `Secret` with `IMMUNOPLEX_API_KEY`/`IMMUNOPLEX_REDIS_AUTH` placeholders | A SealedSecret's encrypted blob is bound to one specific cluster's controller key — undeployable on a fresh test cluster |
| Worker's `DB_HOST`/`DB_USER`/`DB_PASSWORD` point at this install's own `postgresql` Service/Secret | Production points at an external Dartmouth DB host (`mlr-c3d7-db.c.dartmouth.edu`) with a production credential — wrong for a standalone test, and a secret neither of us should want written into a test runbook |
| Dropped `imagePullSecrets: regcred` | Images are public on `ghcr.io`, same as `i-spi.yml` — no pull secret used anywhere else in this folder |
| Worker resources/WORKER_CORES: 2 CPU/4Gi (was 16 CPU/16Gi) | Sized for a test node, not production; comment tells the reader to raise both together for a real fitting workload |
| Added `Ingress` with this folder's TLS/cert-manager annotation convention (matching `i-spi.yml`'s ingress) and a new `i-spi-compute-stripprefix` Traefik middleware | The production manifest's ingress has no TLS block (different edge setup); this standalone install needs one, consistent with every other ingress here |

Also added the `i-spi-compute-stripprefix` middleware to `traefik.yml`'s `ConfigMap` (mirroring the existing `i-spi-stripprefix` pattern) and updated that file's header comment.

**Verified:** all three edited/added YAML files (`i-spi.yml`, `i-spi-compute.yml`, `traefik.yml`) parse cleanly as valid multi-document YAML (checked with R's `yaml` package, every document in each file). I could not validate against a live cluster (no `kubectl`/cluster access in this environment) — recommend a real `kubectl apply --dry-run=server` pass before relying on this for an actual test deployment.

## 5. `architecture.svg` and `deployment-order.svg` — proposed, NOT applied

Both confirmed stale by reading their embedded `<text>`/`<tspan>` content directly:

- **`architecture.svg`** still shows "I-SPI · in-process fitting: stanassay/Stan + JAGS" and a `batch-calculator.yml — OPTIONAL, NOT INVOKED BY THIS BUILD` block (3 dashed boxes: `batch-calculator-api`, `batch-calculator-redis`, `batch-calculator-worker`).
- **`deployment-order.svg`** still lists "I-SPI — in-process fitting — working instance" as the last apply step, with "`batch-calculator.yml` — OPTIONAL · only if I-SPI offloads to it" below it.
- **`auth-sequence.svg`** is unaffected — pure OIDC login-flow sequence diagram, no fitting/compute content at all. No change needed (resolves the "third diagram to check" flagged in Phase 0).

**I did not hand-edit these two SVGs.** Both are simple flat box-and-text diagrams (architecture.svg: 16 rect/group elements) that I could technically parse and rewrite as XML text, but I have no way to render and visually verify the result in this environment, and a diagram meant for real readers is exactly the wrong place to ship an unverified coordinate edit. Precise changes needed, for you (or whoever maintains the diagram source) to apply:

**`architecture.svg`:**
1. APPLICATION box: change "I-SPI · R Shiny app · :3838 · in-process fitting: stanassay/Stan + JAGS" to something like "I-SPI · R Shiny app · :3838 · submits jobs, reads results" (drop the fitting detail — it's no longer true of this box).
2. Replace the `batch-calculator.yml — OPTIONAL, NOT INVOKED BY THIS BUILD` section (3 dashed boxes) with a `i-spi-compute.yml — REQUIRED` section (3 solid boxes, same layout slot): `i-spi-compute-api` (:8000, job API), `i-spi-compute-redis` (:6379, dedicated job queue), `i-spi-compute-worker` (curveR: frequentist + Bayesian fitting, writes PG).
3. Add an arrow/label from I-SPI to `i-spi-compute-api` ("submit/poll jobs, X-API-Key auth") and from `i-spi-compute-worker` to PostgreSQL ("writes calib_* results"), replacing the old "fit + read/write results (madi_results)" arrow that currently goes straight from I-SPI to PostgreSQL.
4. Legend: "optional (not deployed by default)" no longer applies to the compute tier — if nothing else in the diagram is genuinely optional (only `whoami` is, per the existing labeling), consider dropping that legend line, or keep it scoped to `whoami` only.

**`deployment-order.svg`:** change the "I-SPI — in-process fitting — working instance" step to a step reading "i-spi-compute — Redis, API, worker (required)", inserted *before* "I-SPI" in the apply order (matching the dependency the Deployment fixes in §3 now require), and change "I-SPI" to something like "I-SPI — submits jobs, reads results". Remove the `batch-calculator.yml — OPTIONAL` step entirely (retired, not an optional extra anymore).

---

## Acceptance criterion check (plan §6)

> `ARCHITECTURE.md` (and the two diagrams) describe the i-spi-compute tier, including its cluster/replica behavior, in a way that is verified against the actual i-spi-compute source rather than inferred from the deployment manifests alone.

- `ARCHITECTURE.md`: in progress by you, not blocked on anything here except pasting in §1's new section once you're ready.
- Diagrams: content changes specified precisely above (§5), not yet applied — need your diagram tool, not a text edit.
- Everything else named in the plan's known-stale-items list (§6): `TEST-DEPLOYMENT-CIVO.md`/`TEST-DEPLOYMENT-LOCAL.md` fixed; `i-spi.yml` env wiring fixed; the cluster/replica question answered and verified against `compute_cluster_registry.R`/`compute_api_client.R` source directly, not guessed from manifests.
- One new, previously-undocumented gap surfaced along the way: the `calib_*` schema was never part of either test-deployment runbook (§2) — now resolved, the SQL file is vendored into `deploy_ispi/db-dumps/` and both runbooks load it.
