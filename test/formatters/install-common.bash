#!/usr/bin/env bash

set -euxo pipefail

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get install -y ca-certificates curl gnupg lsb-release software-properties-common

mkdir -p /etc/apt/keyrings
curl -fsSL https://deb.nodesource.com/gpgkey/nodesource-repo.gpg.key | gpg --dearmor -o /etc/apt/keyrings/nodesource.gpg
NODE_MAJOR=20
echo "deb [signed-by=/etc/apt/keyrings/nodesource.gpg] https://deb.nodesource.com/node_$NODE_MAJOR.x nodistro main" | tee /etc/apt/sources.list.d/nodesource.list

add-apt-repository -n ppa:longsleep/golang-backports

apt-get update

packages="

bsdmainutils
emacs-nox
git
jq
make
nodejs
sudo
unzip
wget

"

apt-get install -y ${packages}

rm /etc/emacs/site-start.d/*

rm -rf /var/lib/apt/lists/*
