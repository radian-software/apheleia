ver="$(latest_release vlang/v)"

apt-get install -y gcc libx11-dev

wget -nv "https://github.com/vlang/v/releases/download/${ver}/v_linux.zip" -O v.zip
unzip -q v.zip

sudo mkdir /opt/vlang
sudo cp -R v/. /opt/vlang/.
sudo /opt/vlang/v symlink

sudo v build-tools

# thx https://aur.archlinux.org/cgit/aur.git/tree/PKGBUILD?h=vlang
sudo touch /opt/vlang/cmd/tools/.disable_autorecompilation
