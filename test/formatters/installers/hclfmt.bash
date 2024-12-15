apt-get install -y golang-go
go install github.com/hashicorp/hcl/v2/cmd/hclfmt@latest
cp -L "$HOME/go/bin/hclfmt" /usr/local/bin/
