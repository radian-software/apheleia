#!/usr/bin/env bash
set -euo pipefail

find formatters/ -type f,l ! -name "*.formatted" | while read f;
do
    mode="$(echo "$f" | cut -d/ -f2-2)"
    formatter="$(basename "$f" | cut -d. -f1)"
    ./test-apheleia.el "$f" "$mode" "$formatter" | \
        cmp --silent - "$f".formatted || \
             echo >&2 "Test failed: $f doesn't match $f.formatted"
done
