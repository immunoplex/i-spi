#!/usr/bin/env bash
# =============================================================================
# conn_audit.sh  --  inventory every use of the legacy global `conn` across the
# deployed app, so it can be purged before removing conn's creation.
#
# Runs INSIDE the container (which holds the full /srv/shiny-server tree). From
# Git Bash always invoke via:  MSYS_NO_PATHCONV=1 docker exec madi-lumi-reader \
#     sh /srv/shiny-server/conn_audit.sh
# (copy this file in first: docker cp conn_audit.sh madi-lumi-reader:/srv/shiny-server/)
#
# It distinguishes:
#   (1) the DEFINITION / creation sites of `conn`  (what we want to delete last)
#   (2) real USES of the global `conn` (the work: reads/writes via conn)
#   (3) benign look-alikes that are NOT the global conn (param named conn,
#       dbConnect assignments to other names, the word "connection", etc.)
# and prints a per-file, per-line report plus a summary count.
# =============================================================================
set -u
ROOT="${1:-/srv/shiny-server}"
cd "$ROOT" || { echo "no $ROOT"; exit 1; }

# match the bare identifier `conn` as a whole word (not foo_conn, conn_str, etc.)
WORD='(^|[^A-Za-z0-9_.])conn([^A-Za-z0-9_]|$)'

echo "############################################################"
echo "# conn audit over $ROOT"
echo "############################################################"

echo
echo "===== (1) conn CREATION / ASSIGNMENT sites (delete these LAST) ====="
# conn <- ... , conn = ... , or <<- (global assign), and dbConnect(...) -> conn
grep -REn --include=*.R "(^|[^A-Za-z0-9_.])conn[[:space:]]*(<-|<<-|=)[^=]" . 2>/dev/null \
  | grep -vE ":[[:space:]]*#" | sed 's|^\./||'

echo
echo "===== (2) conn USES that are NOT assignments (the work to migrate) ====="
# every bare-word conn line, minus the assignment lines, minus comments
grep -REn --include=*.R "$WORD" . 2>/dev/null \
  | grep -vE ":[[:space:]]*#" \
  | grep -vE "(^|[^A-Za-z0-9_.])conn[[:space:]]*(<-|<<-|=)[^=]" \
  | sed 's|^\./||'

echo
echo "===== (3) per-file tally of bare-word conn occurrences (non-comment) ====="
for f in $(grep -REln --include=*.R "$WORD" . 2>/dev/null | sed 's|^\./||' | sort); do
  n=$(grep -En "$WORD" "$f" | grep -vE ":[[:space:]]*#" | grep -c .)
  [ "$n" -gt 0 ] && printf "  %-40s %s\n" "$f" "$n"
done

echo
echo "===== (4) function PARAMETERS named conn (benign: local, not the global) ====="
grep -REn --include=*.R "function\s*\([^)]*\bconn\b" . 2>/dev/null | sed 's|^\./||'

echo
echo "===== (5) dbConnect() calls -- where connections are actually opened ====="
grep -REn --include=*.R "dbConnect|dbPool|poolCreate" . 2>/dev/null \
  | grep -vE ":[[:space:]]*#" | sed 's|^\./||'

echo
echo "############################################################"
tot=$(grep -REn --include=*.R "$WORD" . 2>/dev/null | grep -vE ":[[:space:]]*#" | grep -c .)
files=$(grep -REln --include=*.R "$WORD" . 2>/dev/null | grep -c .)
echo "# SUMMARY: $tot non-comment bare-word 'conn' lines across $files file(s)"
echo "# Clear section (2) to zero (migrate each to pool), then section (1) is"
echo "# all that's left -- delete those to remove conn entirely."
echo "############################################################"
