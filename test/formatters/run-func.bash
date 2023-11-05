#!/usr/bin/env bash

set -euo pipefail

# Avoid using git to get project directory, due to
# https://github.com/radian-software/apheleia/pull/89#issuecomment-1107319617

whoami
echo $PATH
ls -l $HOME
ls -l $HOME/.cargo
ls -l $HOME/.rustup
which rustup

cd "$(dirname "$0")"
repo="$(cd ../.. && pwd)"

exec emacs --batch -L "${repo}" -L . -l apheleia-ft \
     --eval "(setq debug-on-error t)" -f "$1"
