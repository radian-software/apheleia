apt-get install -y ghc cabal-install
cabal v2-update
cabal v2-install ormolu --reorder-goals
cp "${HOME}/.cabal/bin/ormolu" /usr/local/bin
