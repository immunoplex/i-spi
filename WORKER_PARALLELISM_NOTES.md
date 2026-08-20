# Worker parallelism + stale-job watchdog — deploy notes

## What changed
- **worker_curveR.R** — the multiplate-group loop now fits groups in parallel via
  `parallel::mclapply` (fork). Fan-out is sized by `plan_parallelism()`:
  `n_parallel = min(cores %/% per_fit, usable_mem %/% fit_mem)`, then clamped by
  `WORKER_MAX_PARALLEL`. Cores are the ceiling; memory only pulls fan-out **down**.
  Each forked child opens its **own** DB connection and is pinned to `per_fit`
  cores. `per_fit` = `chains` for Bayesian, `1` for frequentist.
- **supervisor.py** — stamps two liveness clocks into the Redis job hash:
  `heartbeat_at` every poll (loop alive), `progress_at` only when a group
  completes (work advancing).
- **app.py** — `JobStatus` now returns `heartbeat_at` / `progress_at`.
- **std_curve_calc_module.R** — the VERBOSE diagnostic box shows a `liveness:` line
  (report-only). Heartbeat threshold 30s; Bayesian progress threshold >2h so a
  legitimately slow 4-chain group never false-alarms.

## Env knobs (set per container build / k8s manifest)
| Variable | Meaning | Default |
|---|---|---|
| `WORKER_CORES` | CPUs available to this worker | `detectCores()` (not cgroup-aware — always set it) |
| `WORKER_MEM_MB` | memory budget in MiB | cgroup limit − headroom |
| `WORKER_MEM_FRACTION` | fraction of cgroup limit treated as usable | `0.90` |
| `WORKER_MEM_RESERVE_MB` | absolute MiB held back for R + OS + parent | `1500` |
| `WORKER_FIT_MEM_MB_FREQ` | per-concurrent frequentist fit estimate (MiB) | `640` |
| `WORKER_FIT_MEM_MB_BAYES` | per-concurrent Bayesian fit estimate (MiB) | `1280` |
| `WORKER_MAX_PARALLEL` | hard cap on group fan-out (1 = sequential) | unset |

### Example builds
- **16 CPU / 16 GiB (current):** `WORKER_CORES=16`. Frequentist → 16 groups at once;
  Bayesian @4 chains → 4 groups × 4 chains. Memory never binds here.
- **8 CPU / 20 GiB:** `WORKER_CORES=8`. Frequentist → 8; Bayesian @4 chains → 2.
- Retune `WORKER_FIT_MEM_MB_BAYES` from a measured peak:
  `echo 0 > /sys/fs/cgroup/memory/memory.max_usage_in_bytes`, run one heavy
  Bayesian group, then read `memory.max_usage_in_bytes` (÷1048576 = MiB).

## "Does this running job need a restart?" — report-only one-liner
Depends on the supervisor change (heartbeat_at). Uses heartbeat, not progress,
because Bayesian progress can legitimately be silent for ~2h.

```r
check_stale <- function(api, job_id, hb_limit = 30) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
  .parse_iso <- function(x) {
    if (is.null(x) || !length(x) || !nzchar(x)) return(NULL)
    x2 <- sub("([+-][0-9]{2}):?([0-9]{2})$", "", sub("Z$", "", x))
    t <- suppressWarnings(as.POSIXct(x2, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
    if (is.na(t)) NULL else t
  }
  j  <- api$get_job(job_id)
  hb <- as.numeric(Sys.time()) - as.numeric(.parse_iso(j$heartbeat_at %||% ""))
  if (identical(j$status, "running") && is.finite(hb) && hb > hb_limit)
    cat(sprintf("STALE: %s — no heartbeat for %ds (status=running). Worker loop wedged/dead; restart it.\n",
                job_id, round(hb)))
  else
    cat(sprintf("OK: %s status=%s, heartbeat %ss ago.\n",
                job_id, j$status, if (is.finite(hb)) round(hb) else "?"))
}
```

Restart when genuinely wedged (graceful → supervisor SIGTERM marks in-flight
cancelled): `kubectl rollout restart deployment/i-spi-compute-worker`.

## Open items / caveats
- **cmdstanr sampling model unconfirmed** — child processes vs in-process threads.
  On 16c/16G with 4×4 it cannot oversubscribe past 16 either way, so it's safe
  here; revisit `mc.cores = per_fit` as the chains cap before porting to another box.
- **No re-queue on crash** — `blpop` removes a job before it runs; an OOM/crash
  mid-fit orphans it at `status=running`. A startup reconciler (re-queue or fail
  stale `running` jobs) is the recommended follow-on if you want restarts to be
  lossless. Not included here (report-only scope).
- **`save_calib` concurrency** — now called from N connections at once (disjoint
  curve_ids). Validate there's no cross-group locking/sequence contention under load.
