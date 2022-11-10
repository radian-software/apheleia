curl -fsSL https://deno.land/install.sh | sh

export DENO_INSTALL="$HOME/.deno"
export PATH="$DENO_INSTALL/bin:$PATH"
chmod +x $HOME/.deno/bin/deno

sudo mkdir /opt/deno
sudo cp -R "$HOME/.deno/." /opt/deno/.
sudo ln -s /opt/deno/bin/deno /usr/local/bin/
