wget "https://storage.googleapis.com/dart-archive/channels/stable/release/latest/sdk/dartsdk-linux-x64-release.zip"

unzip dartsdk-linux-x64-release.zip
export PATH="$PATH:$(pwd)/dart-sdk/bin"
