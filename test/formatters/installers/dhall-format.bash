apt-get install -y ghc libghc-zlib-dev cabal-install
cabal v2-update
cabal v2-install dhall --reorder-goals
cp "${HOME}/.cabal/bin/dhall" /usr/local/bin
