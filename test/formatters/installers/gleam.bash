ver="$(latest_release gleam-lang/gleam)"

wget "https://github.com/gleam-lang/gleam/releases/download/${ver}/gleam-${ver}-x86_64-unknown-linux-musl.tar.gz" -O /tmp/gleam.tar.gz
tar -xzf /tmp/gleam.tar.gz -C /usr/local/bin
chmod +x /usr/local/bin/gleam
