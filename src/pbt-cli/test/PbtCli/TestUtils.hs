{-# LANGUAGE OverloadedStrings #-}

-- | Scratch directories for tests that need a repository on disk.
module PbtCli.TestUtils (
  withTempTree,
  writeFileIn,
) where

import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)

{- | Run an action with an empty directory of its own, removed afterwards.

Discovery is defined by what is on disk — which @.cabal@ files exist, which
@main-is@ paths resolve — so its tests need real files rather than fixtures in
memory.

The path carries a random suffix. A fixed @$TMPDIR\/pbt-cli-test-\<name\>@ was
tidier to inspect after an interrupted run, but it is shared state: two
concurrent runs of this suite — two checkouts, a shared CI runner, a re-run
started while the first is still going — would delete each other's fixtures
mid-test and fail in ways that look like discovery bugs. It also meant a
recursive delete of that path on the way *in*, which would take with it
whatever else happened to be sitting there.
-}
withTempTree :: String -> (FilePath -> IO a) -> IO a
withTempTree name = withSystemTempDirectory ("pbt-cli-test-" <> name <> "-")

-- | Write a file, creating its parent directories first.
writeFileIn :: FilePath -> FilePath -> String -> IO ()
writeFileIn root rel contents = do
  createDirectoryIfMissing True (takeDirectory (root </> rel))
  writeFile (root </> rel) contents
