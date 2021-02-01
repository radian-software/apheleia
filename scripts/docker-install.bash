#!/usr/bin/env bash

set -e
set -o pipefail

if (( $# != 1 )); then
    echo "usage: docker-install.bash UID" >&2
    exit 1
fi

uid="$1"

# Hashicorp repo for Terraform
apt-get update -y
apt-get install software-properties-common -y
#curl -fsSL https://apt.releases.hashicorp.com/gpg | gpg --dearmor | \
#    tee /usr/share/keyrings/hashicorp-archive-keyring.gpg > /dev/null
curl -fsSL 'https://apt.releases.hashicorp.com/gpg' | apt-key add -
apt-add-repository -y "deb [arch=amd64] https://apt.releases.hashicorp.com $(lsb_release -cs) main"

# NodeJS repo for up-to-date Node
curl -fsSL 'https://deb.nodesource.com/gpgkey/nodesource.gpg.key' | apt-key add -
apt-add-repository -y "deb https://deb.nodesource.com/node_12.x $(lsb_release -cs) main"

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
m4

# For formatters installable from npm
nodejs

# For formatters installable from pip
python3
python3-pip

# For formatters installable from stack
libffi-dev
libgmp-dev
zlib1g-dev
# For brittany
libtinfo-dev

# Formatters installable from apt
# latexindent
texlive-extra-utils
# gofmt
golang-go
terraform
"

# Avoid Opam if possible. Opam is source-based and may incur long build times.
# Install binaries if possible.
opam_packages="
ocamlformat
"
npm_packages="
prettier
"
pip_packages="
black
"
# Avoid Stack if possible. Stack is source-based and may incur long build times.
# Install binaries if possible.
stack_packages="
brittany
"

export DEBIAN_FRONTEND=noninteractive
apt-get -y update
apt-get -y install $(grep -v "^#" <<< "$apt_packages")

# Install Ocaml and Opam
curl -fsSL --output install.sh \
    https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh
chmod +x install.sh
# Handle the unremoveable prompt from the install script
echo "" | ./install.sh
# Docker takes care of our sandboxing
opam init --disable-sandboxing -y
opam install --destdir=/usr/local -y $(grep -v "^#" <<< "$opam_packages")

npm install --global $(grep -v "^#" <<< "$npm_packages")

pip3 install $(grep -v "^#" <<< "$pip_packages")

# Install GHC and Stack
export STACK_ROOT="/usr/local/.stack"
curl -fsSL https://get.haskellstack.org/ | sh
# Older resolver else Brittany fails to build
stack --stack-root "$STACK_ROOT" install --local-bin-path /usr/local/bin \
    --fast --resolver lts-16.25 $(grep -v "^#" <<< "$stack_packages")



rm -rf /var/lib/apt/lists/*

useradd --uid="$uid" --create-home --groups sudo docker
passwd -d docker

rm "$0"
