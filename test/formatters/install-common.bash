#!/usr/bin/env bash

set -euxo pipefail

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get install -y ca-certificates curl gnupg lsb-release software-properties-common

curl -fsSL https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add -

ubuntu_name="$(lsb_release -cs)"
node_repo="$(curl -fsSL https://deb.nodesource.com/setup_current.x | grep NODEREPO= | grep -Eo 'node_[0-9]+\.x' | head -n1)"

tee -a /etc/apt/sources.list.d/nodejs.list >/dev/null <<EOF
deb [arch=amd64] https://deb.nodesource.com/${node_repo} ${ubuntu_name} main
EOF

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
