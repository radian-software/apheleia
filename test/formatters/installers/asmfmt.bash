version=1.3.2

dest=$(mktemp -d)
curl -L "https://github.com/klauspost/asmfmt/releases/download/v$version/asmfmt-Linux_x86_64_$version.tar.gz" | tar -xvzC "$dest"
mv "$dest/asmfmt" /usr/local/bin/asmfmt
rm -rf "$dest"
