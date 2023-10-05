# pnp-bin

This is a minimal equvalent to `yarn bin [command]` it runs

### Usage

`pnp-bin </target/.pnp.cjs> [command]`

Where </target/.pnp.cjs> is the path to the `.pnp.cjs` file of your
plug-and-play project, and [command] is the name of the package binary
you're looking for. If [command] is omitted, `pnp-bin` will list the
names of all binaries found. Otherwise, it provides the location of
the command provided, if found.

Note that the location (path to) a binary might not match the name of
the binary: e.g. the `prettier` package provides a binary named
'prettier' that points to a file named 'prettier.cjs'. The location
may or may not be a real filesystem path; it may point at a virtual
file or into a zip archive.

To run a package bin once found, use

```bash
node --require <.pnp.cjs> [--loader <.pnp.loader.mjs>] <bin-location>`
```

The `loader` option is required for ESM packages, but not for commonjs
(if your package has a .pnp.loader.mjs, you probably need it).

The script dynamically loads the provided `.pnp.cjs`, which provides
the [pnp api](https://yarnpkg.com/advanced/pnpapi). This is then used
to locate the requested bin (executable).

### Performance

On a dev machine, this script took ~130ms vs. ~580ms for
`yarn bin <foo>`.
