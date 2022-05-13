cd /usr/local/bin
ver="$(latest_release pinterest/ktlint)"
apt install -y default-jre
curl -sSLO https://github.com/pinterest/ktlint/releases/download/${ver}/ktlint && chmod a+x ktlint
