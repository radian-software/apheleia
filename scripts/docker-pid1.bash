#!/usr/bin/env bash

set -euo pipefail

cat <<"EOF" > /etc/sudoers.d/apheleia
%sudo ALL=(ALL:ALL) NOPASSWD: ALL
EOF

groupadd -g "$(stat -c %g "$PWD")" -o -p '!' -r apheleia
useradd -u "$(stat -c %u "$PWD")" -g "$(stat -c %g "$PWD")" \
        -o -p '!' -m -N -l -s /usr/bin/bash -G sudo apheleia

runuser -u apheleia touch /home/apheleia/.sudo_as_admin_successful

if (( "$#" == 0 )) || [[ -z "$1" ]]; then
    set -- bash
fi

if (( "$#" == 1 )) && [[ "$1" == *" "* ]]; then
    set -- bash -c "$1"
fi

exec runuser -u apheleia -- "$@"
