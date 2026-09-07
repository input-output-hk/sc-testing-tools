{-# LANGUAGE OverloadedStrings #-}

{- | Differential test: the native discovery must agree with the shell tool it
was ported from.

@scripts\/list-test-suites\/list-test-suites.sh@ is the reference
implementation, and this repository is the fixture — it exercises the
interesting cases for real (two project files, an @import:@, a
@source-repository-package@ stanza, a package owned only by a variant project,
and both streaming and upstream suites).

The comparison is on parsed JSON, not bytes: key order inside a JSON object is
not significant, and aeson orders keys its own way.

This test *skips* rather than fails when the reference cannot be run — the
script is not in the repository (a release binary's source tree), or bash is
too old to run it (macOS ships bash 3.2, and the script needs @mapfile@ and
associative arrays from bash 4). A skip is the honest outcome there: nothing
was compared, and failing would only report the harness, not the port.
-}
module PbtCli.ReferenceSpec (referenceTests) where

import Data.Aeson (Value, decodeStrict', toJSON)
import Data.ByteString.Char8 qualified as BS8
import Data.List (isSuffixOf, sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import PbtCli.Discover (discover)
import PbtCli.Render (renderSuitesTsv)
import System.Directory (canonicalizePath, doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

referenceTests :: TestTree
referenceTests =
  testGroup
    "agreement with list-test-suites.sh"
    [ testCase "the same repository yields the same document" $
        withReference "" $ \root out ->
          case decodeStrict' (BS8.pack (Text.unpack out)) :: Maybe Value of
            Nothing -> skip "the reference script produced no parseable JSON"
            Just reference -> do
              mine <- toJSON <$> discover root
              if mine == reference
                then pure ()
                else
                  assertFailure . unlines $
                    [ "native discovery disagrees with list-test-suites.sh."
                    , "reference: " <> show reference
                    , "native:    " <> show mine
                    ]
    , testCase "the same repository yields the same TSV rows" $
        withReference "--tsv" $ \root out -> do
          mine <- renderSuitesTsv <$> discover root
          -- Two normalisations before comparing, both deliberate:
          --
          -- \* The reference's --tsv mode does not relativise its paths -- only
          --   its JSON mode does -- so with ROOT="." it emits "./src/pkg"
          --   where its own JSON says "src/pkg". pbt-cli emits the
          --   root-relative form in both, so the leading "./" is stripped here
          --   rather than reproducing an inconsistency the reference does not
          --   intend.
          --
          -- \* Rows are compared as sets: the reference emits them in .cabal
          --   discovery order and pbt-cli in document order (project, then
          --   package). Both orders are fine; the rows must agree.
          let reference = map stripDotSlashes (Text.lines out)
          sort reference @?= sort (Text.lines mine)
    ]
 where
  stripDotSlashes = Text.intercalate "\t" . map dropDotSlash . Text.splitOn "\t"
  dropDotSlash col = fromMaybe col (Text.stripPrefix "./" col)

{- | Run the reference script over the repository it lives in and hand its
stdout to @k@, or skip.

The script is invoked with @ROOT="."@ and the repository as the working
directory, which is the usage its README documents; passing an absolute ROOT
makes its @--tsv@ mode emit absolute paths.
-}
withReference :: String -> (FilePath -> Text -> IO ()) -> IO ()
withReference extraArg k = do
  located <- locateScript
  case located of
    Nothing -> skip "scripts/list-test-suites/list-test-suites.sh not found"
    Just (root, script) -> do
      let args = [script, "."] <> [extraArg | not (null extraArg)]
      (code, out, err) <-
        readCreateProcessWithExitCode (proc "bash" args){cwd = Just root} ""
      case code of
        ExitFailure n -> skip ("the reference script exited " <> show n <> ": " <> firstLine err)
        ExitSuccess -> k root (Text.pack out)
 where
  firstLine s = case lines s of
    (l : _) -> l
    [] -> ""

{- | Report that nothing was compared.

A skip is the honest outcome when the reference cannot run: failing would
report the harness rather than the port, and silently passing would hide that
no comparison happened.
-}
skip :: String -> IO ()
skip reason = putStrLn ("  (skipped: " <> reason <> ")")

{- | Walk up from the working directory looking for the reference script.

@cabal test@ runs a suite with the package directory as its working directory,
so the repository root is some number of levels up; searching for it beats
hard-coding @..\/..@, which breaks the moment the package moves.
-}
locateScript :: IO (Maybe (FilePath, FilePath))
locateScript = do
  here <- canonicalizePath "."
  go here (10 :: Int)
 where
  relative = "scripts" </> "list-test-suites" </> "list-test-suites.sh"

  go _ 0 = pure Nothing
  go dir n
    | isRootDir dir = pure Nothing
    | otherwise = do
        let candidate = dir </> relative
        there <- doesFileExist candidate
        if there
          then pure (Just (dir, candidate))
          else go (takeDirectory dir) (n - 1)

  isRootDir d = d == "/" || ("/" `isSuffixOf` d && length d <= 1)
