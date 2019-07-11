#!/usr/bin/env bash

set -e
set -o pipefail

if [[ -z "$1" ]]; then
    echo "docker.sh: no tag provided" 1>&2
    exit 1
else
    tag="$1"
fi

docker() {
    if [[ "$OSTYPE" != darwin* ]] && [[ "$EUID" != 0 ]]; then
        command sudo docker "$@"
    else
        command docker "$@"
    fi
}

script="$(cat <<"EOF"

apt-get update
apt-get install -y bsdmainutils make
cd /src
make help
exec bash

EOF
)"

docker run -it --rm -v "$PWD:/src" silex/emacs:"$tag" bash -c "$script"
