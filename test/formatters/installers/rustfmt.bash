curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- --default-toolchain stable --profile minimal --component rustfmt -y

mv $HOME/.cargo /usr/local/.cargo
chmod -R a+r /usr/local/.cargo
chmod -R a+x /usr/local/.cargo/bin
mv $HOME/.rustup /usr/local/.rustup
chmod -R a+r /usr/local/.rustup
echo -e "\nln -s /usr/local/.rustup \$HOME/.rustup" >>/etc/bash.bashrc
echo -e "\nln -s /usr/local/.cargo \$HOME/.cargo" >>/etc/bash.bashrc
echo -e "source \$HOME/.cargo/env" >>/etc/bash.bashrc
