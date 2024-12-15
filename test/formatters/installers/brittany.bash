# PPA recommended at https://www.haskell.org/cabal/download.html

apt-get install -y ghc cabal-install

cabal v2-update
cabal v2-install brittany --reorder-goals
cp -L "$HOME/.cabal/bin/brittany" /usr/local/bin/
