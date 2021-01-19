#!/usr/bin/env bash

set -e
set -o pipefail

if (( $# != 1 )); then
    echo "usage: docker-install.bash UID" >&2
    exit 1
fi

uid="$1"

# Hashicorp repo for Terraform
curl -fsSL https://apt.releases.hashicorp.com/gpg | gpg --dearmor | \
    sudo tee /usr/share/keyrings/docker-ce-archive-keyring.gpg > /dev/null
sudo apt-add-repository -y "deb [arch=amd64] https://apt.releases.hashicorp.com $(lsb_release -cs) main"

apt_packages="
# needed to run build system
make

# needed for 'make help'
bsdmainutils

# for checking diffs if you want
git

# just in case we want root
sudo

# For formatters installable from opam
ocaml
opam

# For formatters installable from npm
nodejs
npm

# For formatters installable from pip
python3
python3-pip

# Formatters installable from apt
# latexindent
texlive-extra-utils
# gofmt
golang-go
terraform
"

opam_packages="
ocamlformat
"

npm_packages="
prettier
"

pip_packages="
black
"

stack_packages="
brittany
"

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get install -y $(grep -v "^#" <<< "$apt_packages")
opam install $(grep -v "^#" <<< "$opam_packages")
npm install --global $(grep -v "^#" <<< "$npm_packages")
pip install $(grep -v "^#" <<< "$pip_packages")
# Stack
curl -sSL https://get.haskellstack.org/ | sh
stack install $(grep -v "^#" <<< "$stack_packages")


rm -rf /var/lib/apt/lists/*

useradd --uid="$uid" --create-home --groups sudo docker
passwd -d docker

rm "$0"
