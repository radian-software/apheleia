ver="$(latest_release dhall-lang/dhall-haskell)"

wget "https://github.com/dhall-lang/dhall-haskell/releases/download/${ver}/dhall-${ver}-x86_64-linux.tar.bz2"
tar -xf dhall*.tar.bz2
cp bin/dhall /usr/local/bin/
