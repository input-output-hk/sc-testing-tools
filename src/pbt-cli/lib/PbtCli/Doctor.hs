{-# LANGUAGE OverloadedStrings #-}

{- | @pbt-cli doctor@: check that everything the tool needs is present and that
the repository in front of it is actually discoverable.

Modelled on the @--check-health@ mode of the shell tools it replaces, with the
same rule about what counts as a failure: only a missing *required* dependency
or an undiscoverable root is an error. Version strings are informational and
no minimum is ever enforced, because @pbt-cli@ only shells out to @cabal@ and
does not care which version answers.
-}
module PbtCli.Doctor (
  doctor,
  Check (..),
  Status (..),
) where

import Control.Exception (SomeException, try)
import Data.List (intercalate)
import PbtCli.Discover (Discovery (..), discover, flattenSuites, isCompatible, projectFilesIn, srSuite)
import System.Directory (doesDirectoryExist, findExecutable)
import System.Exit (ExitCode (..))
import System.Process (readProcess)

-- | How a single check turned out.
data Status
  = Ok
  | -- | present-but-notable, or an absent optional dependency.
    Warn
  | -- | a required dependency is missing, or the repo cannot be scanned.
    Missing
  deriving (Eq, Show)

data Check = Check
  { chkName :: String
  , chkStatus :: Status
  , chkDetail :: String
  }
  deriving (Eq, Show)

{- | Run every check against @root@, print an aligned report, and return the
exit code: 'ExitFailure' @1@ if anything is 'Missing'.
-}
doctor :: FilePath -> IO ExitCode
doctor root = do
  checks <- sequence [checkRoot root, checkCabal, checkGhc] >>= \cs -> (cs <>) <$> repoChecks root
  putStrLn "pbt-cli — health check"
  putStrLn ""
  mapM_ (putStrLn . format) checks
  putStrLn ""
  let broken = [c | c <- checks, chkStatus c == Missing]
  if null broken
    then do
      putStrLn "All required dependencies present."
      pure ExitSuccess
    else do
      putStrLn $
        "Missing "
          <> show (length broken)
          <> " required item(s): "
          <> intercalate ", " (map chkName broken)
      pure (ExitFailure 1)
 where
  format c = tag (chkStatus c) <> " " <> pad 14 (chkName c) <> chkDetail c

  tag = \case
    Ok -> "[  OK  ]"
    Warn -> "[ WARN ]"
    Missing -> "[MISSING]"

  pad n s = s <> replicate (n - length s) ' '

checkRoot :: FilePath -> IO Check
checkRoot root = do
  exists <- doesDirectoryExist root
  pure $
    if exists
      then Check "root" Ok root
      else Check "root" Missing (root <> " is not a directory")

-- | @cabal@ is required: every command except @suites@ shells out to it.
checkCabal :: IO Check
checkCabal = executableCheck "cabal" True ["--version"]

{- | @ghc@ is only a warning. @cabal@ can be configured with a compiler that is
not on @PATH@, so a missing @ghc@ here is a hint, not a verdict.
-}
checkGhc :: IO Check
checkGhc = executableCheck "ghc" False ["--version"]

executableCheck :: String -> Bool -> [String] -> IO Check
executableCheck exe required args =
  findExecutable exe >>= \case
    Nothing ->
      pure . Check exe (if required then Missing else Warn) $
        "not found on PATH"
          <> if required then "" else " (optional: cabal may use a compiler that is not on PATH)"
    Just path -> do
      out <- try (readProcess path args "") :: IO (Either SomeException String)
      pure . Check exe Ok $ case out of
        Right s | (l : _) <- lines s -> l
        _ -> path

{- | What the repository itself looks like: projects, suites, and how many of
those suites @pbt-cli@ can stream and discover tests in.
-}
repoChecks :: FilePath -> IO [Check]
repoChecks root = do
  exists <- doesDirectoryExist root
  if not exists
    then pure []
    else do
      projects <- projectFilesIn root
      d <- discover root
      let suites = flattenSuites d
          compatible = filter (isCompatible . srSuite) suites
      pure
        [ Check "projects" (if null projects then Warn else Ok) $
            if null projects
              then "no cabal.project* found; treating every .cabal as one implicit project"
              else show (length projects) <> ": " <> intercalate ", " projects
        , Check "test suites" (if null suites then Warn else Ok) $
            show (length suites) <> " discovered"
        , Check "pbt suites" (if null compatible then Warn else Ok) $
            show (length compatible)
              <> " sc-testing-tools compatible"
              <> if null compatible
                then " (no suite uses defaultMainStreaming / defaultMainTestingInterface)"
                else ""
        , Check "orphans" (if null (discOrphans d) then Ok else Warn) $
            case discOrphans d of
              [] -> "none"
              os -> show (length os) <> " .cabal file(s) referenced by no project"
        ]
