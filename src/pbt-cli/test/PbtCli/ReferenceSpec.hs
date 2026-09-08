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

import Data.Aeson (Value (..), decodeStrict', toJSON)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Char8 qualified as BS8
import Data.Foldable (toList)
import Data.List (isSuffixOf, sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import PbtCli.Discover (SuiteRef (..), TestSuite (..), discover, flattenSuites)
import PbtCli.Render (renderSuitesTsv)
import PbtCli.TestUtils (withTempTree, writeFileIn)
import System.Directory (canonicalizePath, createDirectoryLink, doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

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
    , fixtureAgreement
    , symlinkedPackageDivergence
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

{- | Differential tests on purpose-built scratch repositories.

The repository-level test above is one fixture, and one fixture is not a
harness: every behavioural bug found in review (@packages:@ globs matching
directories, a column-0 comment truncating a stanza, the walk following a
symlink loop) was a case where pbt-cli and the reference disagreed on a
five-line repo that this suite never built. These are those cases.
-}
fixtureAgreement :: TestTree
fixtureAgreement =
  testGroup
    "the same scratch repository yields the same document"
    [ differentialOn "dir-glob" $ \root -> do
        -- `packages: pkgs/*` -- the glob matches directories, not .cabal files.
        writeFileIn root "cabal.project" "packages:\n  pkgs/*\n"
        fixturePkg root "pkgs/alpha" "alpha" "alpha-test"
        fixturePkg root "pkgs/beta" "beta" "beta-test"
    , differentialOn "file-glob" $ \root -> do
        writeFileIn root "cabal.project" "packages:\n  */*.cabal\n"
        fixturePkg root "alpha" "alpha" "alpha-test"
        fixturePkg root "beta" "beta" "beta-test"
    , differentialOn "bare-dir" $ \root -> do
        writeFileIn root "cabal.project" "packages:\n  pkg/\n"
        fixturePkg root "pkg" "thing" "thing-test"
    , differentialOn "explicit-cabal" $ \root -> do
        writeFileIn root "cabal.project" "packages:\n  pkg/thing.cabal\n"
        fixturePkg root "pkg" "thing" "thing-test"
    , differentialOn "column-zero-comment" $ \root -> do
        writeFileIn root "cabal.project" "packages:\n  .\n"
        writeFileIn root "c.cabal" $
          unlines
            [ "name: c"
            , "test-suite c-test"
            , "  hs-source-dirs: test"
            , "-- a column-0 comment, which cabal ignores"
            , "  main-is: Spec.hs"
            ]
        writeFileIn root "test/Spec.hs" "main = defaultMainStreaming tests"
    , differentialOn "symlink-loop" $ \root -> do
        writeFileIn root "cabal.project" "packages:\n  pkg\n"
        fixturePkg root "pkg" "a" "a-test"
        createDirectoryLink ".." (root </> "pkg" </> "loop")
    , differentialOn "orphan-package" $ \root -> do
        writeFileIn root "cabal.project" "packages:\n  kept\n"
        fixturePkg root "kept" "kept" "kept-test"
        fixturePkg root "dropped" "dropped" "dropped-test"
    , differentialOn "variant-project-file" $ \root -> do
        writeFileIn root "cabal.project" "packages:\n  base-pkg\n"
        writeFileIn root "cabal.project.extra" "import: cabal.project\n\npackages:\n  extra-pkg\n"
        fixturePkg root "base-pkg" "base" "base-test"
        fixturePkg root "extra-pkg" "extra" "extra-test"
    , differentialOn "upstream-and-streaming" $ \root -> do
        writeFileIn root "cabal.project" "packages:\n  .\n"
        writeFileIn root "mixed.cabal" $
          unlines
            [ "name: mixed"
            , "test-suite good-test"
            , "  hs-source-dirs: good"
            , "  main-is: Spec.hs"
            , ""
            , "test-suite plain-test"
            , "  hs-source-dirs: plain"
            , "  main-is: Spec.hs"
            , ""
            , "test-suite gone-test"
            , "  hs-source-dirs: gone"
            , "  main-is: Nope.hs"
            ]
        writeFileIn root "good/Spec.hs" "main = defaultMainStreaming tests"
        writeFileIn root "plain/Spec.hs" "main = defaultMain tests"
    ]

-- | A minimal package with one streaming test suite.
fixturePkg :: FilePath -> FilePath -> String -> String -> IO ()
fixturePkg root dir name suite = do
  writeFileIn root (dir <> "/" <> name <> ".cabal") $
    unlines
      [ "name: " <> name
      , "version: 0.1"
      , ""
      , "test-suite " <> suite
      , "  type: exitcode-stdio-1.0"
      , "  hs-source-dirs: test"
      , "  main-is: Spec.hs"
      ]
  writeFileIn root (dir <> "/test/Spec.hs") "main = defaultMainStreaming tests"

{- | Build a scratch repository, then require that pbt-cli's discovery and the
reference script produce the same JSON document for it.

The root is canonicalised before use: the reference reports
@cd "$ROOT" && pwd@, which resolves symlinks, and @discover@'s 'makeAbsolute'
does not -- so on a system whose @$TMPDIR@ is itself a symlink the two would
disagree on the @root@ field alone.
-}
differentialOn :: String -> (FilePath -> IO ()) -> TestTree
differentialOn name populate = testCase name $ do
  located <- locateScript
  case located of
    Nothing -> skip "scripts/list-test-suites/list-test-suites.sh not found"
    Just (_, script) -> withTempTree ("ref-" <> name) $ \raw -> do
      root <- canonicalizePath raw
      populate root
      -- Whether to skip is decided by asking bash directly, not by how the
      -- script exited. Mapping any non-zero exit to a skip would turn this
      -- whole harness green whenever the reference broke -- passing while
      -- comparing nothing, which is the exact failure it exists to prevent.
      usable <- bashSupportsReference
      (code, out, err) <-
        readCreateProcessWithExitCode (proc "bash" [script, "."]){cwd = Just root} ""
      case code of
        ExitFailure n
          | not usable ->
              skip ("bash cannot run the reference script (exit " <> show n <> ")")
          | otherwise ->
              assertFailure
                ( "the reference script exited "
                    <> show n
                    <> " on fixture "
                    <> name
                    <> " -- every fixture here has a test suite, so this is a real failure: "
                    <> firstLine err
                )
        ExitSuccess -> case decodeStrict' (BS8.pack out) :: Maybe Value of
          Nothing -> assertFailure "the reference script produced no parseable JSON"
          Just reference -> do
            mine <- toJSON <$> discover root
            if mine == reference
              then pure ()
              else
                assertFailure . unlines $
                  [ "native discovery disagrees with list-test-suites.sh on fixture " <> name <> "."
                  , "reference: " <> show reference
                  , "native:    " <> show mine
                  ]
 where
  firstLine t = case lines t of
    (l : _) -> l
    [] -> ""

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

{- | A deliberate divergence from the reference, pinned so it stays deliberate.

A package directory named in @packages:@ that is itself a symlink: the field
resolves through the link, but neither scan follows one. The reference reports
the package under neither its project nor @orphans@ -- it simply vanishes.
pbt-cli honours it, on the grounds that the walk is a heuristic search whereas
@packages:@ is an instruction; dropping such a package was also a regression
against the commit before the symlink guard landed.

Asserting both halves means the divergence cannot become accidental in either
direction: if pbt-cli stops reporting it, or the reference starts, this fails.
-}
symlinkedPackageDivergence :: TestTree
symlinkedPackageDivergence =
  testCase "a symlinked package directory is reported, unlike the reference" $ do
    located <- locateScript
    case located of
      Nothing -> skip "scripts/list-test-suites/list-test-suites.sh not found"
      Just (_, script) -> withTempTree "ref-symlinked-package" $ \raw -> do
        root <- canonicalizePath raw
        writeFileIn root "cabal.project" "packages:\n  linked\n"
        fixturePkg root "real" "a" "a-test"
        createDirectoryLink "real" (root </> "linked")

        mine <- discover root
        map (tsName . srSuite) (flattenSuites mine) @?= ["a-test"]
        map srPackageDir (flattenSuites mine) @?= ["linked"]

        usable <- bashSupportsReference
        if not usable
          then skip "bash cannot run the reference script"
          else do
            (code, out, _) <-
              readCreateProcessWithExitCode (proc "bash" [script, "."]){cwd = Just root} ""
            -- The reference exits 0: it counts suites from its own scan, which
            -- does reach real/a.cabal. The divergence is in the document, not
            -- the exit code -- the package is reported under no project.
            code @?= ExitSuccess
            case decodeStrict' (BS8.pack out) :: Maybe Value of
              Nothing -> assertFailure "the reference script produced no parseable JSON"
              Just reference -> do
                referencePackageCount reference @?= 0
                assertBool
                  "the reference must not report what pbt-cli reports here"
                  (toJSON mine /= reference)

-- | How many packages the reference's document lists, across all projects.
referencePackageCount :: Value -> Int
referencePackageCount v = case v of
  Object doc -> case KeyMap.lookup "projects" doc of
    Just (Array projects) -> sum (map packagesIn (toList projects))
    _ -> 0
  _ -> 0
 where
  packagesIn = \case
    Object p -> case KeyMap.lookup "packages" p of
      Just (Array pkgs) -> length pkgs
      _ -> 0
    _ -> 0

{- | Can this bash run the reference script at all?

The script needs bash 4 (@mapfile@, associative arrays) and macOS ships 3.2 at
\/bin\/bash, so a skip has to remain possible. Probing the interpreter directly
keeps that decision independent of the script's own exit code, so a broken
reference fails the fixtures instead of silently skipping them.
-}
bashSupportsReference :: IO Bool
bashSupportsReference = do
  (code, _, _) <-
    readCreateProcessWithExitCode
      (proc "bash" ["-c", "declare -A _probe; mapfile -t _lines < /dev/null"])
      ""
  pure (code == ExitSuccess)

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
