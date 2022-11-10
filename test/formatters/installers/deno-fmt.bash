if [ ! -d "/opt/deno/" ]; then
    curl -fsSL https://deno.land/install.sh | sh

    sudo mkdir /opt/deno
    sudo cp -R "$HOME/.deno/." /opt/deno/.
    sudo ln -s /opt/deno/bin/deno /usr/local/bin/
fi
