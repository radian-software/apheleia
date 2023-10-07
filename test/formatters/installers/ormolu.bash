apt-get install -y haskell-platform
cabal update
cabal install ormolu
cp "${HOME}/.cabal/bin/ormolu" /usr/local/bin
