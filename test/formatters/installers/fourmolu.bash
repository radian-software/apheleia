apt-get install -y haskell-platform
cabal update
cabal install fourmolu
cp "${HOME}/.cabal/bin/fourmolu" /usr/local/bin
