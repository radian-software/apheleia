curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- --default-toolchain stable --profile minimal --component rustfmt -y

chmod a+rx $HOME
chmod -R a+r $HOME/.cargo
chmod -R a+rx $HOME/.cargo/bin
chmod a+x $HOME/.rustup
chmod -R a+rw $HOME/.rustup
echo -e "\nln -sf $HOME/.rustup \$HOME/.rustup" >>/etc/skel/.bashrc
echo -e "ln -sf $HOME/.cargo \$HOME/.cargo" >>/etc/skel/.bashrc
echo -e "source \$HOME/.cargo/env" >>/etc/skel/.bashrc
source $HOME/.cargo/env
