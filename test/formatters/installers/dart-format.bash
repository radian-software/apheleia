ARCH=$(uname -m)
case $ARCH in
    "x86_64")
        ARCH="x64"
        ;;
    "i386")
        ARCH="ia32"
        ;;
    "aarch64")
        ARCH="arm64"
esac

mkdir /opt/dart && cd /opt/dart || exit

wget "https://storage.googleapis.com/dart-archive/channels/stable/release/latest/sdk/dartsdk-linux-$ARCH-release.zip"

unzip ./dart*.zip
echo "export PATH=\"$PATH:/opt/dart/dart-sdk/bin\"" >> /etc/bash.bashrc
