#!/usr/bin/env bash

set -euo pipefail

cd "$(dirname "$0")"
repo="$(git rev-parse --show-toplevel)"

exec emacs --batch -L "${repo}" -L . -l apheleia-ft -f "$1"
