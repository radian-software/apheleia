# Following instructions from
# https://docs.slint.dev/latest/docs/slint/guide/tooling/manual-setup/#from-pre-built-binaries-manual-download

# slint-lsp requires the following libraries
apt-get install -y libxkbcommon0 libinput10 libgbm1 libfontconfig1

curl -LSf -o /tmp/slint-lsp.tar.gz https://github.com/slint-ui/slint/releases/latest/download/slint-lsp-x86_64-unknown-linux-gnu.tar.gz
tar -zxvf /tmp/slint-lsp.tar.gz -C /tmp/
mv /tmp/slint-lsp/slint-lsp /usr/local/bin
