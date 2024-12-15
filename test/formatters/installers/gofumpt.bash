apt-get install -y golang-go
go install mvdan.cc/gofumpt@latest
cp -L "$HOME/go/bin/gofumpt" /usr/local/bin/
