#!/usr/bin/env bash
# =============================================================================
# conn_triage.sh -- partition every `conn` occurrence into ACTION buckets, so
# the purge is done safely (a blind conn->pool swap breaks transactions).
# Run in-container:  MSYS_NO_PATHCONV=1 docker exec madi-lumi-reader sh \
#     /srv/shiny-server/conn_triage.sh
# =============================================================================
set -u
ROOT="${1:-/srv/shiny-server}"; cd "$ROOT" || { echo "no $ROOT"; exit 1; }
W='(^|[^A-Za-z0-9_.])conn([^A-Za-z0-9_]|$)'
rr() { grep -REn --include=*.R "$1" . 2>/dev/null | grep -vE ":[[:space:]]*#" | sed 's|^\./||'; }

echo "===== BUCKET A: the global conn is CREATED here (the root to replace) ====="
rr '(^|[^A-Za-z0-9_.])conn[[:space:]]*<-[[:space:]]*get_db_connection'
rr '(^|[^A-Za-z0-9_.])conn[[:space:]]*<<-'

echo
echo "===== BUCKET B: functions that TAKE conn as a parameter (DI -- keep param,"
echo "                just ensure callers pass db_pool). These are NOT edits to"
echo "                the function body. =========================================="
grep -REn --include=*.R "function\s*\([^)]*\bconn\b" . 2>/dev/null | grep -vE ":[[:space:]]*#" | sed 's|^\./||'

echo
echo "===== BUCKET C: TRANSACTION / bulk-write use of conn (must use a checked-out"
echo "                connection, NOT a bare pool -- poolWithTransaction pattern) ="
rr 'dbBegin|dbCommit|dbRollback|dbWithTransaction|dbAppendTable|poolWithTransaction|COPY |upsert_batch'

echo
echo "===== BUCKET D: simple reads/execs on conn (dbGetQuery/dbExecute/.con=conn)"
echo "                -- safe to receive db_pool once the caller passes it ========"
rr 'dbGetQuery\([[:space:]]*conn|dbExecute\([[:space:]]*conn|\.con[[:space:]]*=[[:space:]]*conn|dbQuoteString\([[:space:]]*conn|dbQuoteIdentifier\([[:space:]]*conn'

echo
echo "===== BUCKET E: CALL SITES passing the bare global conn into a param ========"
echo "                (these are what actually consume app.R:859's global; change"
echo "                 the argument they pass from conn -> db_pool) ==============="
grep -REn --include=*.R "(^|[,(][[:space:]]*)conn[[:space:]]*=[[:space:]]*conn([[:space:]]*[,)])" . 2>/dev/null | grep -vE ":[[:space:]]*#" | sed 's|^\./||'

echo
echo "===== per-file totals (non-comment bare conn) ====="
for f in $(grep -REln --include=*.R "$W" . 2>/dev/null | sed 's|^\./||' | sort); do
  n=$(grep -En "$W" "$f" | grep -vE ":[[:space:]]*#" | grep -c .)
  [ "$n" -gt 0 ] && printf "  %-38s %s\n" "$f" "$n"
done
