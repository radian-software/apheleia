apt-get install -y opam

opam init -n --disable-sandboxing --root /opt/ocamlformat
opam install ocamlformat -y --root /opt/ocamlformat
ln -s /opt/ocamlformat/default/bin/ocamlformat /usr/local/bin/
