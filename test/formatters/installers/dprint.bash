npm install -g dprint
cat <<\EOF >/tmp/dprint.json
{
  "toml": {},
  "excludes": [],
  "plugins": [
    "https://plugins.dprint.dev/toml-0.5.4.wasm"
  ]
}
EOF
dprint config update
