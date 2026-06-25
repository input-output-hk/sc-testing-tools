// Drive extraction over the list-test-suites per-suite JSON.
//
// list-test-suites (scripts/list-test-suites/list-test-suites.sh) JSON shape:
//   { root, projects: [ { packages: [ { packageDir, cabalFile,
//       testSuites: [ { name, mainIs, entryPoint, hsSourceDirs, ... } ] } ] } ] }
//
// For each suite we resolve the entry file, parse it, and extract the tree.

const path = require("path");
const fs = require("fs");

const { FollowState } = require("./resolver");
const { ExtractContext, extractTree } = require("./extract");

// Flatten test-suite discovery JSON into a list of { suite, packageDir, cabalFile, mainIs,
// entryPoint, hsSourceDirs }.
function flattenSuites(json) {
  const out = [];
  for (const project of json.projects || []) {
    for (const pkg of project.packages || []) {
      for (const ts of pkg.testSuites || []) {
        out.push({
          suite: ts.name,
          packageDir: pkg.packageDir,
          cabalFile: pkg.cabalFile,
          mainIs: ts.mainIs,
          entryPoint: ts.entryPoint,
          hsSourceDirs: ts.hsSourceDirs || [],
        });
      }
    }
  }
  return out;
}

// Build the absolute base dirs for a suite: packageDir joined with each
// hsSourceDir. These are the roots used for module->file resolution.
function suiteBaseDirs(root, packageDir, hsSourceDirs) {
  const dirs = (hsSourceDirs.length ? hsSourceDirs : ["."]).map((d) =>
    path.resolve(root, packageDir, d)
  );
  return dirs;
}

// Extract one suite. Each suite gets its own FollowState (budget per suite) but
// shares the module cache across suites for speed.
function extractSuite(suite, root, moduleCache, followLimit) {
  const result = {
    suite: suite.suite,
    package: suite.packageDir,
    entryFile: null,
    entryPoint: suite.entryPoint,
    tree: null,
  };

  if (!suite.mainIs || suite.mainIs === "MISSING") {
    result.note = "main-is is MISSING in cabal metadata; cannot locate entry file";
    return result;
  }

  const entryAbs = path.resolve(root, suite.packageDir, suite.mainIs);
  result.entryFile = path.relative(root, entryAbs);

  if (!fs.existsSync(entryAbs)) {
    result.note = `entry file not found: ${result.entryFile}`;
    return result;
  }

  const baseDirs = suiteBaseDirs(root, suite.packageDir, suite.hsSourceDirs);

  let entryModule;
  try {
    entryModule = moduleCache.parse(entryAbs);
  } catch (err) {
    result.note = `failed to parse entry file: ${err.message}`;
    return result;
  }

  const ctx = new ExtractContext({
    root,
    moduleCache,
    baseDirs,
    follow: new FollowState(followLimit),
  });

  try {
    const { tree, entryBinding } = extractTree(entryModule, ctx);
    result.tree = tree;
    result.entryBinding = entryBinding;
    result.crossFileFollows = ctx.follow.opens;
    if (tree === null) {
      result.note = "no tasty entry (main/tests/testGroup) located in entry file";
    }
  } catch (err) {
    result.note = `extraction error: ${err.message}`;
    result.tree = null;
  }

  return result;
}

module.exports = {
  flattenSuites,
  suiteBaseDirs,
  extractSuite,
};
