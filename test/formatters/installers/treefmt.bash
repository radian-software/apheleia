ver="$(latest_release numtide/treefmt | sed 's/^v//')"

wget "https://github.com/numtide/treefmt/releases/download/v${ver}/treefmt_${ver}_linux_amd64.tar.gz" -O - | tar -C /usr/local/bin -xz
chmod +x /usr/local/bin/treefmt

apt-get install -y rustfmt
cat <<EOT >> /tmp/treefmt.toml
[formatter.rust]
command = "rustfmt"
includes = ["*.rs"]
EOT
