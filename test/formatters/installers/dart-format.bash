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

wget "https://storage.googleapis.com/dart-archive/channels/stable/release/latest/sdk/dartsdk-linux-${arch}-release.zip" -O dart.zip
unzip dart.zip
chmod a=u,go-w -R dart-sdk

sudo mkdir /opt/dart
sudo cp -R dart-sdk/. /opt/dart/.
sudo ln -s /opt/dart/bin/dart /usr/local/bin/
