#!/usr/bin/env bash

# Expects a single argument of the form "formatters/<mode>/<formatter>.<ext>"
f="$1"
mode="$(echo "$f" | cut -d/ -f2-2)"
formatter="$(basename "$f" | cut -d. -f1)"
if ! ./test-apheleia.el "$f" "$mode" "$formatter" | \
    cmp --silent - "$f".formatted
then
    echo >&2 "Test failed: $f does not match $f.formatted"
    exit 1
fi
