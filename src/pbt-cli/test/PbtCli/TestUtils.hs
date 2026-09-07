{-# LANGUAGE OverloadedStrings #-}

-- | Scratch directories for tests that need a repository on disk.
module PbtCli.TestUtils (
  withTempTree,
  writeFileIn,
) where

import Control.Exception (bracket_)
import System.Directory (
  createDirectoryIfMissing,
  doesDirectoryExist,
  getTemporaryDirectory,
  removeDirectoryRecursive,
 )
import System.FilePath (takeDirectory, (</>))

{- | Run an action with an empty directory of its own, removed afterwards.

Discovery is defined by what is on disk — which @.cabal@ files exist, which
@main-is@ paths resolve — so its tests need real files rather than fixtures in
memory. The directory is named after the test to keep failures easy to inspect
when a run is interrupted.
-}
withTempTree :: String -> (FilePath -> IO a) -> IO a
withTempTree name act = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> ("pbt-cli-test-" <> name)
  bracket_ (reset dir) (removeIfPresent dir) (act dir)
 where
  reset dir = do
    removeIfPresent dir
    createDirectoryIfMissing True dir

  removeIfPresent dir = do
    there <- doesDirectoryExist dir
    if there then removeDirectoryRecursive dir else pure ()

-- | Write a file, creating its parent directories first.
writeFileIn :: FilePath -> FilePath -> String -> IO ()
writeFileIn root rel contents = do
  createDirectoryIfMissing True (takeDirectory (root </> rel))
  writeFile (root </> rel) contents
