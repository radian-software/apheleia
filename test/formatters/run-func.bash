#!/usr/bin/env bash

set -euo pipefail

# Avoid using git to get project directory, due to
# https://github.com/radian-software/apheleia/pull/89#issuecomment-1107319617

whoami 1>&2
echo $PATH 1>&2
ls -la $HOME 1>&2

cd "$(dirname "$0")"
repo="$(cd ../.. && pwd)"

exec emacs --batch -L "${repo}" -L . -l apheleia-ft \
     --eval "(setq debug-on-error t)" -f "$1"
