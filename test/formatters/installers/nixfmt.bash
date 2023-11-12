ver="$(latest_release serokell/nixfmt)"

wget "https://github.com/serokell/nixfmt/releases/download/${ver}/nixfmt-x86_64-linux" -O /usr/local/bin/nixfmt
chmod +x /usr/local/bin/nixfmt
