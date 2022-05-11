ver="$(latest_release mvdan/sh)"

arch="$(uname -m)"
case "${arch}" in
    "x86_64")
        arch="amd64"
        ;;
    "i386")
        arch="386"
        ;;
    "aarch64")
        arch="arm64"
        ;;
    *)
        echo >&2 "unsupported architecture: ${arch}"
        exit 1
        ;;
esac

wget "https://github.com/mvdan/sh/releases/download/${ver}/shfmt_${ver}_linux_${arch}" -O /usr/local/bin/shfmt
chmod +x /usr/local/bin/shfmt
