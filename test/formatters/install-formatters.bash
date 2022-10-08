#!/usr/bin/env bash

set -euo pipefail

if [[ -n "${FORMATTERS:-}" ]]; then
    IFS=' ,' read -ra FORMATTERS <<< "${FORMATTERS}"
else
    readarray -t FORMATTERS < <(ls installers | grep '\.bash$' | sed 's/\.bash$//')
fi

if (( "${#FORMATTERS[@]}" > 0 )); then
    echo >&2 "Installing the following ${#FORMATTERS[@]} formatter(s):"
    for f in "${FORMATTERS[@]}"; do
        echo >&2 "  * ${f}"
    done
else
    echo >&2 "Installing NO formatters"
fi

echo >&2 "-- Setting up --"

export DEBIAN_FRONTEND=noninteractive
apt-get update

orig_wd="${PWD}"

mkdir /tmp/apheleia-work
cd /tmp/apheleia-work

latest_release() {
    curl -fsSL "https://api.github.com/repos/$1/releases/latest" | jq -r .tag_name
}

echo >&2 "-- Will install ${#FORMATTERS[@]} formatter(s) --"

for f in "${FORMATTERS[@]}"; do
    echo >&2 "-- Installing formatter ${f} --"
    . "${orig_wd}/installers/${f}.bash"
done

echo >&2 "-- Cleaning up --"

rm -rf /var/lib/apt/lists/*
rm -rf /tmp/apheleia-work
