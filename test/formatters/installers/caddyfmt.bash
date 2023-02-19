ver="$(latest_release caddyserver/caddy)"

arch="$(uname -m)"
case "${arch}" in
    "x86_64")
        arch="amd64"
        ;;
    "aarch64")
        arch="arm64"
        ;;
    *)
        echo >&2 "unsupported architecture: ${arch}"
        exit 1
        ;;
esac

curl -L -s "https://github.com/caddyserver/caddy/releases/download/${ver}/caddy_$(echo $ver | sed 's|^v||g')_linux_${arch}.tar.gz" | \
    tar zxv -C /usr/local/bin/ caddy
