#!/usr/bin/env node

const path = require("path");

const pnp = require(path.resolve(__dirname, "./.pnp.cjs"));
pnp.setup();

const { listBinaries, findBinary } = require(
  path.resolve(__dirname, "./index.js"),
);

const pnpPath = path.resolve(process.cwd(), process.argv[2]);

if (!pnpPath) {
  console.log(
    `Usage: ${process.argv[0]} <path to .pnp.cjs file> [name of binary]`,
  );
  process.exit(1);
}

if (process.argv[3]) {
  findBinary(pnpPath, process.argv[3]).then((path) => {
    if (path) {
      console.log(path);
    } else {
      console.error(`Could not find binary '${process.argv[3]}' in package`);
      process.exit(1);
    }
  });
} else {
  listBinaries(pnpPath).then((binaries) =>
    console.log(binaries.map((b) => b[0]).join("\n")),
  );
}
