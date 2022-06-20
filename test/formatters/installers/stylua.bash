ver="$(latest_release JohnnyMorganz/StyLua | sed 's/^v//')"

wget "https://github.com/JohnnyMorganz/StyLua/releases/download/v${ver}/stylua-linux.zip" -O stylua.zip
unzip stylua.zip
chmod +x stylua
cp stylua /usr/local/bin/
