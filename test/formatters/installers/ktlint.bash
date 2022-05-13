ver="$(latest_release pinterest/ktlint)"
apt-get install -y default-jre
wget "https://github.com/pinterest/ktlint/releases/download/${ver}/ktlint" -O /usr/local/bin/ktlint
chmod +x /usr/local/bin/ktlint
