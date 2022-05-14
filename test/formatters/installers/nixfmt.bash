ver="$(latest_release serokell/nixfmt)"

wget "https://github.com/serokell/nixfmt/releases/download/${ver}/nixfmt" -O /usr/local/bin/nixfmt
chmod +x /usr/local/bin/nixfmt
