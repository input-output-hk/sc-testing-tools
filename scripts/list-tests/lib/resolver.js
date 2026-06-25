// Cross-file reference resolution.
//
// Maps a module name (resolved through the import table) to a concrete .hs file
// under one of the suite's base dirs (packageDir joined with each hsSourceDir),
// then loads + indexes it via the ModuleCache.
//
// Enforces:
//   - a cross-file FOLLOW limit (default 100; the tree depth itself is
//     unlimited), as a backstop against malicious/broken repos.
//   - a VISITED set of (file, binding) pairs for cycle detection.

const path = require("path");
const fs = require("fs");

// Tracks the follow budget + visited (file, binding) pairs for one extraction.
class FollowState {
  constructor(limit) {
    this.limit = typeof limit === "number" ? limit : 100;
    this.opens = 0; // cross-file opens performed
    this.visited = new Set(); // "file::binding" keys
  }

  // True if another cross-file follow is still permitted.
  hasBudget() {
    return this.opens < this.limit;
  }

  // Record a cross-file open (after a successful file resolution).
  spend() {
    this.opens += 1;
  }

  visitKey(file, binding) {
    return `${file}::${binding}`;
  }

  // True if (file, binding) was already visited (cycle).
  seen(file, binding) {
    return this.visited.has(this.visitKey(file, binding));
  }

  mark(file, binding) {
    this.visited.add(this.visitKey(file, binding));
  }
}

// Resolve a module name to a file, searching each base dir in order.
//   module A.B.C  ->  <baseDir>/A/B/C.hs
// Returns the first existing path, else null.
function moduleNameToFile(moduleName, baseDirs) {
  const rel = moduleName.split(".").join(path.sep) + ".hs";
  for (const base of baseDirs) {
    const candidate = path.join(base, rel);
    if (fs.existsSync(candidate)) return candidate;
  }
  return null;
}

module.exports = {
  FollowState,
  moduleNameToFile,
};
