# PPA recommended at https://www.haskell.org/cabal/download.html
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys FF3AEACEF6F88286
echo "deb [arch=amd64] http://ppa.launchpad.net/hvr/ghc/ubuntu $(lsb_release -cs) main" > /etc/apt/sources.list.d/brittany.list

apt-get update
apt-get install -y cabal-install-3.4 ghc
ln -s /opt/cabal/bin/cabal /usr/local/bin/

cabal v2-update
cabal v2-install brittany --reorder-goals
cp -L "$HOME/.cabal/bin/brittany" /usr/local/bin/
