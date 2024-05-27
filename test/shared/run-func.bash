#!/usr/bin/env bash

set -euo pipefail

func="$1"
shift

# Avoid using git to get project directory, due to
# https://github.com/radian-software/apheleia/pull/89#issuecomment-1107319617

cd "$(dirname "$0")/../.."

exec emacs --batch -L . "$@"                  \
     --eval "(setq debug-on-error t)"         \
     --eval "(setq backtrace-line-length 0)"  \
     -f     "${func}"                         \
     2>&1 | sed -uE 's/^(.{320}).+$/\1...[truncated]/'
