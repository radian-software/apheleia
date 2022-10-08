ver="$(latest_release rust-lang/rustfmt | sed 's/^v//')"

wget "https://github.com/rust-lang/rustfmt/releases/download/v${ver}/rustfmt_linux-x86_64_v${ver}.tar.gz" -O rustfmt.tar.gz
tar -xf rustfmt.tar.gz
cp rustfmt*/rustfmt /usr/local/bin/
