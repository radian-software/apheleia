#!/usr/bin/env bash

set -e
set -o pipefail

if (( $# != 1 )); then
    echo "usage: docker-install.bash UID" >&2
    exit 1
fi

uid="$1"

packages="

# needed to run build system
make

# needed for 'make help'
bsdmainutils

# for checking diffs if you want
git

# just in case we want root
sudo

"

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get install -y $(grep -v "^#" <<< "$packages")
rm -rf /var/lib/apt/lists/*

useradd --uid="$uid" --create-home --groups sudo docker
passwd -d docker

rm "$0"
