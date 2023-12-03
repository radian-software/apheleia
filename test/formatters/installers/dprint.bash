npm install -g dprint

# Included: WASM plugins, preferring first  over 3rd party wrappers
# where redundant (e.g. -typescript and -json instead of -biome)
cat <<\EOF >/tmp/dprint.json
{
  "excludes": [],
  "plugins": [
    "https://plugins.dprint.dev/dockerfile-0.3.0.wasm",
    "https://plugins.dprint.dev/json-0.19.1.wasm",
    "https://plugins.dprint.dev/markdown-0.16.3.wasm",
    "https://plugins.dprint.dev/ruff-0.0.2.wasm",
    "https://plugins.dprint.dev/toml-0.5.4.wasm",
    "https://plugins.dprint.dev/typescript-0.88.7.wasm"
  ]
}
EOF

# dprint config update  # not done to keep formatting stable
