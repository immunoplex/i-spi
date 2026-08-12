#!/usr/bin/env bash
# Trace the ONE global conn (app.R:859 conn <- get_db_connection()) to its consumers.
# Run in-container. Goal: list every site that reads the *global* conn (free var),
# separated from sites where conn is a local (param / poolWithTransaction arg).
set -u
ROOT="${1:-/srv/shiny-server}"; cd "$ROOT" || exit 1

echo "===== where is get_db_connection defined + called? ====="
grep -REn --include=*.R "get_db_connection" . | grep -vE ":[[:space:]]*#" | sed 's|^\./||'

echo
echo "===== app.R: context around the global creation (859) + who uses conn after ====="
grep -nE "(^|[^A-Za-z0-9_.])conn([^A-Za-z0-9_]|$)|get_db_connection|poolWithTransaction|dataTabServer|reloadReactive|getProjectName|load_project" app.R \
  | grep -vE ":[[:space:]]*#"

echo
echo "===== files that reference bare conn but DEFINE no conn param and have NO"
echo "      poolWithTransaction wrapper -> these read a FREE (global) conn ========"
for f in $(grep -REln --include=*.R "(^|[^A-Za-z0-9_.])conn([^A-Za-z0-9_]|$)" . | sed 's|^\./||' | sort); do
  has_param=$(grep -cE "function\s*\([^)]*\bconn\b" "$f")
  has_txn=$(grep -cE "poolWithTransaction" "$f")
  uses=$(grep -cE "(^|[^A-Za-z0-9_.])conn([^A-Za-z0-9_]|$)" "$f")
  # a file that USES conn but never takes it as a param and never checks one out
  # is reading a free/global conn (or relying on the global by scoping)
  if [ "$has_param" -eq 0 ] && [ "$has_txn" -eq 0 ]; then
    printf "  FREE-conn?  %-34s uses=%s  (no conn param, no poolWithTransaction)\n" "$f" "$uses"
  fi
done

echo
echo "===== the 7 Bucket-E call sites, with 6 lines of context each ====="
for loc in "db_functions.R:140" "db_functions.R:665" "assay_import_backend.R:153" \
           "assay_import_backend.R:193" "assay_import_backend.R:368" \
           "derived_experiments.R:307" "derived_experiments.R:407"; do
  f="${loc%%:*}"; ln="${loc##*:}"
  echo "--- $loc ---"
  sed -n "$((ln-3)),$((ln+2))p" "$f" 2>/dev/null | sed 's/^/    /'
done
