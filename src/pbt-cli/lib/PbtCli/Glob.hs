{-# LANGUAGE LambdaCase #-}

{- | Minimal shell-style globbing, enough for the @packages:@ field of a
@cabal.project@ file.

The reference implementation (@scripts/list-test-suites/list-test-suites.sh@)
resolves package entries by letting @bash@ expand them, so tokens such as
@*\/*.cabal@ or @pkgs\/*@ work. This module reproduces that behaviour
without a shell and without pulling in a dependency: patterns are matched
segment by segment against the real filesystem, @*@ and @?@ never cross a
directory separator, and — as in @bash@ — a wildcard does not match a
leading dot.
-}
module PbtCli.Glob (
  isGlob,
  matchSegment,
  expandGlob,
) where

import Control.Monad (filterM, foldM)
import Data.List (sort)
import System.Directory (doesDirectoryExist, doesPathExist, listDirectory)
import System.FilePath (splitDirectories, (</>))

-- | Does this token contain any wildcard metacharacter?
isGlob :: String -> Bool
isGlob s = any (`elem` ("*?" :: String)) s

{- | Match a single path segment against a single pattern segment.

@*@ matches any run of characters, @?@ exactly one, and everything else is
literal. A pattern that does not itself start with @.@ never matches a name
that does, which is how shell globs hide dotfiles.
-}
matchSegment :: String -> String -> Bool
matchSegment pat name
  | not (startsWithDot pat) && startsWithDot name = False
  | otherwise = go pat name
 where
  startsWithDot = \case
    ('.' : _) -> True
    _ -> False

  go [] [] = True
  go [] _ = False
  -- A trailing run of '*' matches the rest of the segment, including nothing.
  go ('*' : ps) cs = go ps cs || (not (null cs) && go ('*' : ps) (drop 1 cs))
  go ('?' : ps) (_ : cs) = go ps cs
  go ('?' : _) [] = False
  go (p : ps) (c : cs) = p == c && go ps cs
  go (_ : _) [] = False

{- | Expand @pattern@, interpreted relative to @base@, into the paths that
actually exist. The returned paths keep the @base \</\> …@ prefix and are
sorted, so callers get a deterministic order.

A pattern with no wildcard is simply checked for existence, which makes this
function usable for every @packages:@ token, glob or not.
-}
expandGlob :: FilePath -> String -> IO [FilePath]
expandGlob base pattern
  | not (isGlob pattern) = do
      let p = base </> pattern
      exists <- doesPathExist p
      pure [p | exists]
  | otherwise = do
      -- Anchor on the pattern's own directory structure. Absolute patterns are
      -- not something cabal.project produces, and `splitDirectories` on a
      -- relative pattern gives exactly the segments we want to walk.
      let segments = filter (/= ".") (splitDirectories pattern)
      sort <$> foldM step [base] segments
 where
  step :: [FilePath] -> String -> IO [FilePath]
  step dirs seg
    | not (isGlob seg) =
        filterM doesPathExist (map (</> seg) dirs)
    | otherwise =
        concat <$> mapM (matchesIn seg) dirs

  -- Wildcard segment: list the directory and keep the entries that match.
  matchesIn :: String -> FilePath -> IO [FilePath]
  matchesIn seg dir = do
    isDir <- doesDirectoryExist dir
    if not isDir
      then pure []
      else do
        entries <- listDirectory dir
        pure [dir </> e | e <- sort entries, matchSegment seg e]
