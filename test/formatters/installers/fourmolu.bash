ver="$(latest_release fourmolu/fourmolu | sed 's/^v//')"

wget "https://github.com/fourmolu/fourmolu/releases/download/v${ver}/fourmolu-${ver}-linux-x86_64.zip"
unzip fourmolu*.zip
cp fourmolu*/fourmolu /usr/local/bin/
