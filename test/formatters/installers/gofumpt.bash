# Require at least version 1.17 of go
# https://github.com/mvdan/gofumpt/issues/231
curl -OL https://golang.org/dl/go1.19.3.linux-amd64.tar.gz
sudo tar -C /usr/local -xvf go1.19.3.linux-amd64.tar.gz
/usr/local/go/bin/go install mvdan.cc/gofumpt@latest
cp -L "$HOME/go/bin/gofumpt" /usr/local/bin/