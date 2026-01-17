#!/usr/bin/env bash

set -euo pipefail

repo="$(git rev-parse --show-toplevel)"

if [[ -z "${USE_PODMAN:-}" ]]; then
    docker=(docker)
    if [[ "$OSTYPE" != darwin* ]] && [[ "$EUID" != 0 ]]; then
        docker=(sudo -E "${docker[@]}")
    fi
else
    docker=(podman)
fi

it=()

if [[ -t 0 ]]; then
    it+=(-it)
fi

exec "${docker[@]}" run "${it[@]}" --rm -v "${repo}:/src"  \
     --entrypoint=/src/scripts/docker-pid1.bash "$@"
