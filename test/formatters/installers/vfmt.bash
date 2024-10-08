ver="$(latest_release vlang/v)"

apt-get install -y gcc
wget "https://github.com/vlang/v/releases/download/${ver}/v_linux.zip" -O v.zip
unzip v.zip
chmod a=u,go-w -R v
sudo mkdir /opt/vlang
sudo cp -R v/. /opt/vlang/.
sudo /opt/vlang/v symlink
