#!/usr/bin/env bash

set -euo pipefail

func="$1"
shift

# Avoid using git to get project directory, due to
# https://github.com/radian-software/apheleia/pull/89#issuecomment-1107319617

cd "$(dirname "$0")/../.."

exec emacs --batch -L . "$@" \
     --eval "(setq debug-on-error t)" -f "${func}"
