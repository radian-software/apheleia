apt-get install -y opam

opam init -n --disable-sandboxing --root /opt/ocp-indent
opam install ocp-indent -y --root /opt/ocp-indent
ln -s /opt/ocp-indent/default/bin/ocp-indent /usr/local/bin/
