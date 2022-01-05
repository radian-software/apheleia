#!/usr/bin/env bash

set -euo pipefail

repo="$(git rev-parse --show-toplevel)"

docker=(docker)
if [[ "$OSTYPE" != darwin* ]] && [[ "$EUID" != 0 ]]; then
    docker=(sudo -E "${docker[@]}")
fi

exec "${docker[@]}" run -it --rm -v "${repo}:/src"    \
     --entrypoint=/src/scripts/docker-pid1.bash "$@"
