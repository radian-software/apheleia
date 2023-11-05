curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- --default-toolchain stable --profile minimal --component rustfmt -y

chmod a+rx $HOME
chmod -R a+r $HOME/.cargo
chmod -R a+rx $HOME/.cargo/bin
chmod a+x $HOME/.rustup
chmod -R a+rw $HOME/.rustup
echo -e "\nln -s $HOME/.rustup \$HOME/.rustup" >>/etc/bash.bashrc
echo -e "ln -s $HOME/.cargo \$HOME/.cargo" >>/etc/bash.bashrc
echo -e "source \$HOME/.cargo/env" >>/etc/bash.bashrc
source $HOME/.cargo/env
