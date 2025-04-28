#!/usr/bin/env bash
#
# Written by ChatGeePeeTee™
#
# extract-table-fast.sh
#
# Requirements:
#   • xmllint (libxml2)
#   • GNU sed, grep, tr
#
# Usage:
#   # just dump the <table>…</table> to stdout
#   ./extract-table-fast.sh input.html
#
#   # dump table AND write a TSV
#   ./extract-table-fast.sh input.html output.tsv
#
set -euo pipefail

if (( $# < 1 || $# > 2 )); then
  echo "Usage: $0 <html_file> [output.tsv]" >&2
  exit 1
fi

INPUT="$1"
TSV_OUT="${2-}"

# deps
command -v xmllint >/dev/null || { echo "Error: xmllint not found." >&2; exit 1; }
[[ -z "$TSV_OUT" ]] || command -v sed >/dev/null || { echo "Error: sed not found." >&2; exit 1; }
command -v tr >/dev/null || { echo "Error: tr not found." >&2; exit 1; }

[[ -r "$INPUT" ]] || { echo "Error: '$INPUT' not readable." >&2; exit 2; }

# force UTF-8
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# 1) extract the <table>…</table> once, forcing UTF-8
TABLE_XML=$(
  {
    printf '<?xml version="1.0" encoding="UTF-8"?>\n'
    xmllint --html --recover --encode UTF-8 --xpath '//table' "$INPUT" 2>/dev/null
  }
)

# 2) print the raw table
printf '%s\n' "$TABLE_XML"

# 3) if TSV requested, convert with a super‑fast sed pipeline
if [[ -n "$TSV_OUT" ]]; then
  printf '%s\n' "$TABLE_XML" | tr '\n' ' ' \
    | sed 's/<\/tr>/<\/tr>\n/g; s/<tr[^>]*>/\n<tr>/g' \
    | grep -oP '(?<=^<tr>).*?(?=</tr>)' \
    | sed -E \
        -e 's/<t[dh][^>]*>/\t/g' \
        -e 's/<[^>]*>//g' \
        -e 's/^\t//' \
        -e 's/\t$//' \
    > "$TSV_OUT"

  echo "→ TSV written to '$TSV_OUT'"
fi
