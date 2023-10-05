const { PosixFS } = require("@yarnpkg/fslib");
const { ZipOpenFS } = require("@yarnpkg/libzip");
const path = require("path");

const zipOpenFs = new ZipOpenFS();
const crossFs = new PosixFS(zipOpenFs);

function getPackageJson(packagePath) {
  try {
    return JSON.parse(
      crossFs.readFileSync(path.resolve(packagePath, "package.json")),
    );
  } catch {
    return undefined;
  }
}

async function listPnpPackageLocations(pnpPath) {
  const p = require(pnpPath);

  const dependencies = p.getPackageInformation(p.topLevel).packageDependencies;
  const locators = [...dependencies.entries()].map((d) =>
    p.getLocator(d[0], d[1]),
  );
  return locators.map((locator) => [
    locator.name,
    p.getPackageInformation(locator).packageLocation,
  ]);
}

const listBinaries = (pnpPath) =>
  listPnpPackageLocations(pnpPath).then((locations) =>
    locations
      .map((l) => [l[1], getPackageJson(l[1])])
      .filter((m) => "bin" in m[1])
      .reduce((acc, m) => {
        if (typeof m[1]["bin"] === "string") {
          acc.push([m[1].name, path.resolve(m[0], m[1].bin)]);
        } else if (typeof m[1]["bin"] === "object") {
          acc.push(
            ...Object.entries(m[1].bin).map(([name, pth]) => [
              name,
              path.resolve(m[0], pth),
            ]),
          );
        }

        return acc;
      }, []),
  );

function findPackageBinary(location, binaryName) {
  if (location) {
    const packagePath = location[1];
    const packageJson = getPackageJson(packagePath);
    if (packageJson && packageJson.bin) {
      if (
        typeof packageJson.bin === "string" &&
        packageJson.name === binaryName
      ) {
        return path.resolve(packagePath, packageJson.bin);
      } else if (packageJson?.bin?.[binaryName]) {
        return path.resolve(packagePath, packageJson.bin[binaryName]);
      }
    }
  }
}

async function findBinary(pnpPath, binaryName) {
  const locations = await listPnpPackageLocations(pnpPath);

  // pulling the package.json from a package zip archive is expensive, so...
  // if there's a package with the same name as the binary, try that first
  const exactMatchLocation = locations.find((l) => l[0] === binaryName);
  if (exactMatchLocation) {
    const binary = findPackageBinary(exactMatchLocation, binaryName);
    if (binary) {
      return binary;
    }
  }

  let binary = undefined;
  locations
    .filter((l) => l[0] !== binaryName)
    .find((l) => (binary = findPackageBinary(l, binaryName)));
  return binary;
}

module.exports = { listPnpPackageLocations, listBinaries, findBinary };
