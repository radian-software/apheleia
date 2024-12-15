apt-get install -y ghc cabal-install
cabal v2-update
cabal v2-install fourmolu --reorder-goals
cp "${HOME}/.cabal/bin/fourmolu" /usr/local/bin
