#!/usr/bin/env bash
# =============================================================================
# check_best_family.sh -- zero-caller verdict for the stale best_*/mcmc helper
# family in db_functions.R (and any other file). SAFE = 0 callers outside its
# own definition; KEEP = has callers (prints them). Run in-container.
# =============================================================================
set -u
ROOT="${ROOT:-/srv/shiny-server}"; cd "$ROOT" || { echo "no $ROOT"; exit 1; }

# discover every function whose name matches the stale-fit families, from defs
FNS=$(grep -REh --include=*.R "^[A-Za-z_.][A-Za-z0-9_.]*[[:space:]]*(<-|=)[[:space:]]*function" . \
      | sed -E 's/[[:space:]]*(<-|=).*//' | tr -d ' ' \
      | grep -E '^(fetch_best_|upsert_best_|update_combined_mcmc|fetch_combined_mcmc|fetch_best_.*_mcmc)' \
      | sort -u)

echo "Candidate stale-fit functions found:"; echo "$FNS" | sed 's/^/  /'
echo
printf "%-42s %s\n" "function" "verdict"
printf "%-42s %s\n" "--------" "-------"
safe_list=""
for fn in $FNS; do
  hits=$(grep -REn --include=*.R "(^|[^A-Za-z0-9_.])${fn}[[:space:]]*\(" . 2>/dev/null | grep -vE ":[[:space:]]*#")
  calls=$(printf "%s\n" "$hits" | grep -vE "(^|[^A-Za-z0-9_.])${fn}[[:space:]]*(<-|=)[[:space:]]*function")
  n=$(printf "%s" "$calls" | grep -c .)
  if [ "$n" -eq 0 ]; then
    printf "%-42s %s\n" "$fn" "SAFE (0 callers)"
    safe_list="$safe_list $fn"
  else
    printf "%-42s %s\n" "$fn" "KEEP ($n caller(s))"
    printf "%s\n" "$calls" | sed 's|^\./|     -> |'
  fi
done
echo
echo "SAFE-to-remove set:"; echo "  $safe_list"
