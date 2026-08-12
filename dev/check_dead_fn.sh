#!/usr/bin/env bash
# =============================================================================
# check_dead_fn.sh  --  prove a function has zero callers across the deployed
# tree, then (only if zero) show its definition span for removal.
#   usage (in container):  sh /srv/shiny-server/check_dead_fn.sh <fn> [file]
# =============================================================================
set -u
ROOT="${ROOT:-/srv/shiny-server}"; cd "$ROOT" || { echo "no $ROOT"; exit 1; }
FN="${1:?usage: check_dead_fn.sh <function_name> [defining_file]}"
DEFFILE="${2:-}"

echo "=== call sites of ${FN}(  across all .R (excluding its own definition) ==="
# a CALL = name followed by "(", not preceded by an identifier char; skip comments
hits=$(grep -REn --include=*.R "(^|[^A-Za-z0-9_.])${FN}[[:space:]]*\(" . 2>/dev/null \
       | grep -vE ":[[:space:]]*#")
# drop the definition line(s):  FN <- function( ... )
calls=$(printf "%s\n" "$hits" | grep -vE "(^|[^A-Za-z0-9_.])${FN}[[:space:]]*(<-|=)[[:space:]]*function")
n=$(printf "%s" "$calls" | grep -c .)

if [ "$n" -gt 0 ]; then
  echo "  $n caller(s) -> DO NOT REMOVE:"
  printf "%s\n" "$calls" | sed 's|^\./||'
else
  echo "  0 callers -> SAFE TO REMOVE"
fi

echo
echo "=== definition site(s) of ${FN} ==="
grep -REn --include=*.R "(^|[^A-Za-z0-9_.])${FN}[[:space:]]*(<-|=)[[:space:]]*function" . 2>/dev/null | sed 's|^\./||'

# If a defining file was given, show the exact line span (def -> matching close)
if [ -n "$DEFFILE" ] && [ -f "$DEFFILE" ]; then
  echo
  echo "=== $DEFFILE : line span of ${FN} (brace-matched) ==="
  awk -v fn="$FN" '
    $0 ~ ("(^|[^A-Za-z0-9_.])" fn "[[:space:]]*(<-|=)[[:space:]]*function") && !instart {
      instart=1; start=NR; depth=0
    }
    instart {
      n=gsub(/\{/,"{"); m=gsub(/\}/,"}"); depth+=n-m
      if (seenopen==0 && n>0) seenopen=1
      if (seenopen && depth<=0) { print "  lines " start " - " NR; instart=0; seenopen=0 }
    }
  ' "$DEFFILE"
fi
