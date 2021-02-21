#!/usr/bin/env bash

set -euxo pipefail

: "${SHARED_UID}"

export DEBIAN_FRONTEND=noninteractive

apt-get update
apt-get -y install software-properties-common

# For Terraform
curl -fsSL 'https://apt.releases.hashicorp.com/gpg' | apt-key add -
apt-add-repository -yn "deb [arch=amd64] https://apt.releases.hashicorp.com $(lsb_release -cs) main"

# For up-to-date NPM
curl -fsSL 'https://deb.nodesource.com/gpgkey/nodesource.gpg.key' | apt-key add -
apt-add-repository -y "deb https://deb.nodesource.com/node_12.x $(lsb_release -cs) main"

apt_packages="

# Handy utilities
bsdmainutils
git
make
sudo
wget

# Package managers
nodejs
python3
python3-pip

# Formatters
ghc                  # brittany (runtime dep)
golang-go            # gofmt
texlive-extra-utils  # latexindent
terraform

"

npm_packages="

prettier

"
pip_packages="

black

"

apt-get -y install $(sed 's/#.*//' <<< "$apt_packages")
rm -rf /var/lib/apt/lists/*

npm install -g $(sed 's/#.*//' <<< "$npm_packages")
pip3 install $(sed 's/#.*//' <<< "$pip_packages")

wget https://github.com/raxod502/apheleia-cdn/releases/download/brittany-0.12.2.0-rev2/brittany \
     -O /usr/local/bin/brittany
chmod +x /usr/local/bin/brittany

wget https://github.com/raxod502/apheleia-cdn/releases/download/ocamlformat-0.17.0/ocamlformat \
     -O /usr/local/bin/ocamlformat
chmod +x /usr/local/bin/ocamlformat

useradd --uid="${SHARED_UID}" --create-home --groups sudo docker
passwd -d docker

rm "$0"
