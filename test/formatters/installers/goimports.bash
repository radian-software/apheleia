apt-get install -y golang-go
go install golang.org/x/tools/cmd/goimports@latest
cp -L "$HOME/go/bin/goimports" /usr/local/bin/
