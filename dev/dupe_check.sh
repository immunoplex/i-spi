#!/usr/bin/env bash
# Verify duplicate-definition dead-code candidates before removal.
# Run in-container.
set -u
ROOT="${1:-/srv/shiny-server}"; cd "$ROOT" || exit 1

for FN in getProjectName load_project; do
  echo "############################################################"
  echo "# $FN"
  echo "############################################################"
  echo "--- DEFINITION sites ---"
  grep -REn --include=*.R "(^|[^A-Za-z0-9_.])${FN}[[:space:]]*(<-|=)[[:space:]]*function" . \
    | grep -vE ":[[:space:]]*#" | sed 's|^\./||'
  echo "--- CALL sites (excluding definitions) ---"
  grep -REn --include=*.R "(^|[^A-Za-z0-9_.])${FN}[[:space:]]*\(" . \
    | grep -vE ":[[:space:]]*#" \
    | grep -vE "(^|[^A-Za-z0-9_.])${FN}[[:space:]]*(<-|=)[[:space:]]*function" \
    | sed 's|^\./||'
  echo
done

echo "############################################################"
echo "# SOURCE ORDER of the two files that define getProjectName"
echo "# (last one sourced WINS at runtime)"
echo "############################################################"
grep -REn --include=*.R "source\(['\"](global|ui_handler)\.R" . | grep -vE ":[[:space:]]*#" | sed 's|^\./||'
echo "(note: global.R is typically loaded via global.R auto-load, not source(); ui_handler.R is source()d in app.R)"
echo
echo "--- where is ui_handler.R sourced? ---"
grep -REn --include=*.R "source\(['\"]ui_handler" . | grep -vE ":[[:space:]]*#" | sed 's|^\./||'
