ver="$(basename $(curl -fs -o/dev/null -w %{redirect_url} https://github.com/mvdan/sh/releases/latest))"

arch="$(uname -m)"
case "${arch}" in
    "x86_64")
        arch="x64"
        ;;
    "i386")
        arch="ia32"
        ;;
    "aarch64")
        arch="arm64"
        ;;
    *)
        echo >&2 "unsupported architecture: ${arch}"
        exit 1
        ;;
esac

wget "https://github.com/mvdan/sh/releases/download/${ver}/shfmt_${ver}_$(uname | tr '[:upper:]' '[:lower:]')_${arch}" -O /usr/local/bin/shfmt
chmod +x /usr/local/bin/shfmt
