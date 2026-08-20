#!/usr/bin/env bash
# =============================================================================
# docker_build_test.sh — build the I-SPI image and verify it after the
# dependency prune. Run from the directory that holds docker-compose.yml,
# Dockerfile, and src/.
#
# STAGES
#   0. Preflight        — tooling + expected files present
#   1. Build            — docker compose build (BuildKit)
#   2. Parse check      — R parses every sourced .R in the image (no DB needed)
#   3. Package check    — every library()/require() in the image loads (no DB)  <-- the prune test
#   4. Boot check       — compose up, HTTP probe, scan app logs (NEEDS the DB via .env)
#
# The interesting one after a prune is Stage 3: it catches a removed-but-still-
# needed package as a clean "there is no package called X" instead of a runtime
# surprise. Stages 2–3 need no database; Stage 4 does (global.R opens a Postgres
# pool eagerly at startup), so it is gated and skippable.
#
# USAGE
#   ./docker_build_test.sh                 # all stages
#   SMOKE_ONLY=1 ./docker_build_test.sh    # stages 0–3 only (no DB / no boot)
#   NOCACHE=1    ./docker_build_test.sh    # force a clean rebuild
#   KEEP_UP=1    ./docker_build_test.sh    # leave the container running after Stage 4
#
# OVERRIDABLE ENV (defaults match your docker-compose.yml)
#   IMAGE, CONTAINER, SERVICE, PORT, COMPOSE, BOOT_TIMEOUT
# =============================================================================
set -uo pipefail

# ---- config ---------------------------------------------------------------
### short form

BUILD_STAMP=$(date +%Y%m%d-%H%M%S) docker compose build

docker compose up -d

docker compose down

### end short form

IMAGE="${IMAGE:-madi-lumi-reader}"
CONTAINER="${CONTAINER:-madi-lumi-reader}"
SERVICE="${SERVICE:-madi-toolset}"
PORT="${PORT:-3838}"
COMPOSE="${COMPOSE:-docker compose}"          # override with 'docker-compose' if needed
APP_DIR="/srv/shiny-server"                    # where the Dockerfile COPYs ./src/
NOCACHE="${NOCACHE:-0}"
SMOKE_ONLY="${SMOKE_ONLY:-0}"
KEEP_UP="${KEEP_UP:-0}"
BOOT_TIMEOUT="${BOOT_TIMEOUT:-90}"             # seconds to wait for an HTTP response
TMPDIR_LOCAL="$(mktemp -d)"
trap 'rm -rf "$TMPDIR_LOCAL"' EXIT

# ---- pretty output --------------------------------------------------------
if [ -t 1 ]; then G=$'\033[32m'; R=$'\033[31m'; Y=$'\033[33m'; B=$'\033[1m'; Z=$'\033[0m'
else G=""; R=""; Y=""; B=""; Z=""; fi
stage(){ printf "\n${B}==== %s ====${Z}\n" "$*"; }
pass(){  printf "  ${G}PASS${Z} %s\n" "$*"; }
warn(){  printf "  ${Y}WARN${Z} %s\n" "$*"; }
fail(){  printf "  ${R}FAIL${Z} %s\n" "$*"; FAILED=1; }
FAILED=0

# =============================================================================
# Stage 0 — preflight
# =============================================================================
stage "0. Preflight"
command -v docker >/dev/null 2>&1 && pass "docker found" || { fail "docker not on PATH"; exit 1; }
$COMPOSE version >/dev/null 2>&1 && pass "compose found ($COMPOSE)" \
  || { fail "'$COMPOSE' not working — set COMPOSE=docker-compose if you use the v1 binary"; exit 1; }
# CLIs above answer without a daemon; this probe needs the engine actually running.
docker info >/dev/null 2>&1 && pass "docker daemon reachable" \
  || { fail "docker daemon not reachable — start Docker Desktop and wait until it reads 'Engine running' (Linux containers), then verify with 'docker info' and re-run"; exit 1; }
[ -f Dockerfile ]          && pass "Dockerfile present"          || fail "Dockerfile missing (run from the project root)"
[ -d src ]                 && pass "src/ present"                || fail "src/ missing"
[ -f src/app.R ]           && pass "src/app.R present"           || fail "src/app.R missing"
[ -f src/global.R ]        && pass "src/global.R present"        || fail "src/global.R missing"
if [ -f .env ]; then pass ".env present"; else warn ".env missing — Stage 4 (boot) will be skipped"; fi
# dead/ and dev/ should NOT ship in the image; flag if they are under src/
for d in src/dead src/dev; do
  [ -d "$d" ] && warn "$d exists under src/ — it will be COPYd into the image; consider a .dockerignore"
done
[ "$FAILED" -eq 1 ] && { printf "\n${R}Preflight failed.${Z}\n"; exit 1; }

# =============================================================================
# Stage 1 — build
# =============================================================================
stage "1. Build image ($IMAGE)"
export DOCKER_BUILDKIT=1 COMPOSE_DOCKER_CLI_BUILD=1
BUILD_ARGS=""; [ "$NOCACHE" = "1" ] && BUILD_ARGS="--no-cache"
printf "  building (this is long — many one-package layers)…\n"
t0=$(date +%s)
if $COMPOSE build $BUILD_ARGS; then
  t1=$(date +%s); pass "build completed in $((t1 - t0))s"
else
  fail "docker build failed — read the log above; a missing system lib or a dropped-but-needed R package shows here"
  exit 1
fi
# confirm the tag exists
docker image inspect "$IMAGE" >/dev/null 2>&1 && pass "image '$IMAGE' present" \
  || { fail "image '$IMAGE' not found after build — check the 'image:' field in docker-compose.yml"; exit 1; }

# Run a check script inside the freshly built image, no DB touched.
# We stream the script to Rscript via stdin rather than bind-mounting it: on
# Windows/Git Bash a -v mount and any "/tmp/..." argument get path-mangled by
# MSYS. Streaming avoids the mount, and MSYS_NO_PATHCONV/ARG_CONV_EXCL stop Git
# Bash from rewriting the container-side "/dev/stdin" arg. Both env vars are
# harmless no-ops on macOS/Linux.
run_in_image(){ # $1 = path to a local .R file, executed in $IMAGE (reads /srv/shiny-server)
  MSYS_NO_PATHCONV=1 MSYS2_ARG_CONV_EXCL='*' \
    docker run --rm -i "$IMAGE" Rscript /dev/stdin < "$1"
}

# =============================================================================
# Stage 2 — parse every sourced .R (syntax check, no DB, no sourcing)
# =============================================================================
stage "2. R parse check (top-level src, no DB)"
cat > "$TMPDIR_LOCAL/parse.R" <<'RS'
app <- "/srv/shiny-server"
# top-level only: dead/ and dev/ live in subdirs and are not sourced by app.R
files <- list.files(app, pattern="\\.R$", full.names=TRUE, recursive=FALSE)
bad <- character(0)
for (f in files) {
  ok <- tryCatch({ parse(file=f); TRUE },
                 error=function(e){ cat(sprintf("  parse error: %s\n    %s\n",
                                    basename(f), conditionMessage(e))); FALSE })
  if (!ok) bad <- c(bad, f)
}
cat(sprintf("parsed %d files, %d failed\n", length(files), length(bad)))
quit(status = if (length(bad)) 1 else 0)
RS
if run_in_image "$TMPDIR_LOCAL/parse.R"; then pass "all top-level .R parse cleanly"
else fail "one or more .R files have syntax errors (see above)"; fi

# =============================================================================
# Stage 3 — package availability (THE prune test, no DB)
# =============================================================================
stage "3. Package availability check (no DB)"
cat > "$TMPDIR_LOCAL/pkgs.R" <<'RS'
app <- "/srv/shiny-server"
# scan every top-level .R for library()/require() targets (excludes dead/ & dev/)
files <- list.files(app, pattern="\\.R$", full.names=TRUE, recursive=FALSE)
pkgs <- character(0)
for (f in files) {
  ln <- readLines(f, warn=FALSE)
  ln <- sub("#.*$", "", ln)                         # drop comments
  hit <- regmatches(ln, gregexpr("(?:library|require)\\(\\s*[\"']?[A-Za-z0-9._]+", ln, perl=TRUE))
  hit <- unlist(hit)
  if (length(hit)) pkgs <- c(pkgs, sub("(?:library|require)\\(\\s*[\"']?", "", hit, perl=TRUE))
}
# belt-and-suspenders: hard-require the infrastructure the app cannot boot without
critical <- c("shiny","bslib","DBI","RPostgres","pool","tidyverse","tidyr","DT",
              "plotly","glue","auth0","httr2","jsonlite")
pkgs <- sort(unique(c(pkgs, critical)))
cat(sprintf("checking %d packages…\n", length(pkgs)))
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly=TRUE)]
if (length(missing)) {
  cat("  MISSING (attached/needed but not installed):\n")
  for (m in missing) cat("    -", m, "\n")
  quit(status=1)
}
cat("  all attached/critical packages load\n")
quit(status=0)
RS
if run_in_image "$TMPDIR_LOCAL/pkgs.R"; then pass "every attached + critical package is installed"
else fail "a required package is missing from the image — add it to the Dockerfile"; fi

# =============================================================================
# Stage 3.5 — DB/TLS connectivity (direct dbConnect; needs .env, no app code)
# -----------------------------------------------------------------------------
# Isolates "can we reach Postgres over TLS with these creds" from "does the app
# serve" — so a DB/cert/credential problem is caught here, not buried in a 500.
# Mirrors global.R's connection: sslmode=require + sslcert=""/sslkey="" (no
# client cert). Runs via `compose run` so it uses the service's env_file exactly
# as the app does (Compose's parser is lenient; `docker run --env-file` is not).
# =============================================================================
if [ "$SMOKE_ONLY" = "1" ]; then
  stage "3.5 DB/TLS probe — SKIPPED (SMOKE_ONLY=1)"
elif [ ! -f .env ]; then
  stage "3.5 DB/TLS probe — SKIPPED (no .env)"
else
  stage "3.5 DB/TLS connectivity probe (direct dbConnect, no app code)"
  cat > "$TMPDIR_LOCAL/db.R" <<'RS'
ok <- tryCatch({
  con <- DBI::dbConnect(RPostgres::Postgres(),
    host     = Sys.getenv("db_host"), port = Sys.getenv("db_port", "5432"),
    dbname   = Sys.getenv("db"),      user = Sys.getenv("db_userid_x"),
    password = Sys.getenv("db_pwd_x"),
    sslmode  = "require",
    sslcert  = "/nonexistent/postgresql.crt",   # match global.R: absent path -> ENOENT -> skip cert
    sslkey   = "/nonexistent/postgresql.key",
    connect_timeout = 8)
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
  DBI::dbGetQuery(con, "SELECT 1 AS ok")
  TRUE
}, error = function(e) { cat("   dbConnect error:", conditionMessage(e), "\n"); FALSE })
if (isTRUE(ok)) cat("   connected (sslmode=require, no client cert)\n")
quit(status = if (isTRUE(ok)) 0 else 1)
RS
  # `compose run` uses the service's env_file (same lenient parse as the running
  # app), avoiding `docker run --env-file`'s rejection of whitespace-in-key lines.
  # -T disables the TTY so the script pipes in on stdin; --no-deps starts nothing else.
  if MSYS_NO_PATHCONV=1 MSYS2_ARG_CONV_EXCL='*' \
       $COMPOSE run --rm -T --no-deps "$SERVICE" Rscript /dev/stdin < "$TMPDIR_LOCAL/db.R"; then
    pass "database reachable (sslmode=require, no client certificate needed)"
  else
    fail "direct dbConnect failed — DB/TLS/credentials issue, independent of app code (see error above)"
  fi
fi

# =============================================================================
# Stage 4 — boot test (NEEDS a reachable DB via .env; global.R opens a pool)
# =============================================================================
if [ "$SMOKE_ONLY" = "1" ]; then
  stage "4. Boot test — SKIPPED (SMOKE_ONLY=1)"
elif [ ! -f .env ]; then
  stage "4. Boot test — SKIPPED (no .env; the app needs DB creds: db, db_host, db_port, db_userid_x, db_pwd_x, + auth vars)"
else
  stage "4. Boot test (compose up — needs the database reachable)"
  $COMPOSE up -d "$SERVICE" >/dev/null 2>&1 && pass "container started" \
    || { fail "compose up failed"; $COMPOSE logs --tail=40 "$SERVICE" || true; exit 1; }

  # 4a. HTTP probe — accept anything that proves the app answered (2xx/3xx/auth).
  printf "  probing http://localhost:%s/ (up to %ss)…\n" "$PORT" "$BOOT_TIMEOUT"
  code=""; deadline=$(( $(date +%s) + BOOT_TIMEOUT ))
  while [ "$(date +%s)" -lt "$deadline" ]; do
    code=$(curl -s -o /dev/null -w '%{http_code}' "http://localhost:${PORT}/" 2>/dev/null || echo 000)
    case "$code" in 200|301|302|401|403) break;; esac
    sleep 3
  done
  boot_ok=0
  case "$code" in
    200|301|302|401|403) pass "server responded (HTTP $code)"; boot_ok=1;;
    500|502|503)         fail "app returned HTTP $code — shiny-server is up but the app errors at startup";;
    *)                   fail "no usable HTTP response (last=$code) within ${BOOT_TIMEOUT}s";;
  esac

  # 4b. Missing-package scan (fast, always).
  printf "  collecting startup logs…\n"
  logs="$( { $COMPOSE logs --no-color --tail=200 "$SERVICE" 2>/dev/null; \
             docker exec "$CONTAINER" sh -c 'cat /var/log/shiny-server/*.log 2>/dev/null'; } || true )"
  if printf '%s' "$logs" | grep -qiE "there is no package called"; then
    fail "a package is missing at load time:"; printf '%s\n' "$logs" | grep -iE "there is no package called" | sed 's/^/      /'
  else pass "no 'missing package' errors in logs"; fi

  # 4c. On failure, surface the real cause (don't guess).
  if [ "$boot_ok" -eq 0 ]; then
    printf "\n  ${Y}--- boot diagnostics -------------------------------------------${Z}\n"
    printf "  HTTP %s response body (first 30 lines):\n" "$code"
    curl -s "http://localhost:${PORT}/" 2>/dev/null | head -30 | sed 's/^/      /'
    printf "\n  shiny-server logs (last 60 lines):\n"
    docker exec "$CONTAINER" sh -c 'ls -la /var/log/shiny-server/ 2>/dev/null; echo; tail -n +1 /var/log/shiny-server/*.log 2>/dev/null' 2>/dev/null | tail -60 | sed 's/^/      /'
    printf "\n  reproducing startup in-container (sources global.R, then auth_config.R):\n"
    # Plain top-level source (no tryCatch/local): progressr::handlers(global=TRUE)
    # in global.R refuses to run with a condition handler on the stack, so a
    # wrapper would mask the real error with 'should not be called with handlers…'.
    docker exec "$CONTAINER" Rscript -e 'setwd("/srv/shiny-server"); message("== global.R =="); source("global.R"); message("== auth_config.R =="); source("auth_config.R"); message("== startup OK ==")' 2>&1 | sed 's/^/      /'
    printf "\n  ${Y}Most common cause (Docker Desktop):${Z} if .env db_host is localhost/127.0.0.1\n"
    printf "  and Postgres runs on the Windows host, the container cannot reach it — set\n"
    printf "  db_host=host.docker.internal, or use the DB's real hostname/IP.\n"
    printf "  If both sources print 'ok', the 500 is a shiny-server *serving* issue\n"
    printf "  (app.R sits at the site_dir root), not an app init error.\n"
    printf "  ${Y}----------------------------------------------------------------${Z}\n"
  else
    if printf '%s' "$logs" | grep -qiE "could not connect|could not translate host|connection refused|no pg_hba|password authentication|server closed the connection|timeout expired|no route to host"; then
      warn "database connection problem in logs:"
      printf '%s\n' "$logs" | grep -iE "could not connect|could not translate host|connection refused|no pg_hba|password authentication|server closed the connection|timeout expired|no route to host" | head -4 | sed 's/^/      /'
    fi
  fi

  if [ "$KEEP_UP" = "1" ]; then
    pass "leaving container up (KEEP_UP=1) — visit http://localhost:${PORT}/  (stop with: $COMPOSE down)"
  else
    $COMPOSE down >/dev/null 2>&1 && pass "container stopped" || warn "could not stop container"
  fi
fi

# =============================================================================
# Summary
# =============================================================================
stage "Summary"
if [ "$FAILED" -eq 0 ]; then
  printf "  ${G}${B}ALL CHECKS PASSED${Z}\n"; exit 0
else
  printf "  ${R}${B}ONE OR MORE CHECKS FAILED${Z} — see FAIL lines above.\n"; exit 1
fi
