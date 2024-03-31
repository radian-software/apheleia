ver="$(latest_release numtide/treefmt)"

wget "https://github.com/numtide/treefmt/releases/download/${ver}/treefmt-x86_64-unknown-linux-gnu.tar.gz" -O - | tar -C /usr/local/bin -xz
chmod +x /usr/local/bin/treefmt

apt-get install -y rustfmt
cat <<EOT >> /tmp/treefmt.toml
[formatter.rust]
command = "rustfmt"
includes = ["*.rs"]
EOT
