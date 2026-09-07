{-# LANGUAGE OverloadedStrings #-}

module PbtCli.DiscoverSpec (discoverTests) where

import Data.Aeson (Value (..), toJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List (sort)
import Data.Text (Text)
import PbtCli.Discover (
  Discovery (..),
  EntryPoint (..),
  Orphan (..),
  Package (..),
  Project (..),
  SuiteRef (..),
  TestSuite (..),
  classifySource,
  compatibleOnly,
  discover,
  findSuite,
  flattenSuites,
  isCompatible,
  packageNameOf,
  projectFilesIn,
 )
import PbtCli.Render (renderSuiteNames)
import PbtCli.TestUtils (withTempTree, writeFileIn)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

discoverTests :: TestTree
discoverTests =
  testGroup
    "Discover"
    [ classifyTests
    , packageNameTests
    , projectFileTests
    , layoutTests
    , importTests
    , missingMainTests
    , orphanTests
    , globPackagesTests
    , compatibilityTests
    , jsonShapeTests
    , listingTests
    ]

-- ---------------------------------------------------------------------------

classifyTests :: TestTree
classifyTests =
  testGroup
    "classifySource"
    [ testCase "defaultMainStreaming is streaming" $
        classifySource "main = defaultMainStreaming tests" @?= Streaming
    , -- The *WithIngredients variant must be checked before the plain
      -- defaultMain marker, or it would be misread as upstream tasty.
      testCase "defaultMainStreamingWithIngredients is streaming, not upstream" $
        classifySource "main = defaultMainStreamingWithIngredients [x] tests" @?= Streaming
    , testCase "defaultMainTestingInterface is streaming" $
        classifySource "main = defaultMainTestingInterface tests" @?= Streaming
    , testCase "an import of the streaming module is enough" $
        classifySource "import Convex.Tasty.Streaming\nmain = wrapped tests" @?= Streaming
    , testCase "an import of the testing interface is enough" $
        classifySource "import Convex.TestingInterface (x)\nmain = go" @?= Streaming
    , testCase "plain defaultMain is upstream" $
        classifySource "import Test.Tasty\nmain = defaultMain tests" @?= Upstream
    , testCase "defaultMainWithIngredients is upstream" $
        classifySource "main = defaultMainWithIngredients [consoleTestReporter] tests" @?= Upstream
    , testCase "no recognised runner is unknown" $
        classifySource "main = putStrLn \"hello\"" @?= UnknownEntry
    ]

packageNameTests :: TestTree
packageNameTests =
  testGroup
    "packageNameOf"
    [ testCase "reads a column-0 name field" $
        packageNameOf "cabal-version: 3.0\nname:          convex-base\nversion: 1\n" @?= "convex-base"
    , testCase "is case-insensitive" $
        packageNameOf "Name: Widget\n" @?= "Widget"
    , -- An indented `name:` belongs to a stanza (a flag, a sublibrary), not to
      -- the package.
      testCase "ignores an indented name field" $
        packageNameOf "library\n  name: inner\n" @?= ""
    , testCase "is empty when absent" $ packageNameOf "" @?= ""
    ]

projectFileTests :: TestTree
projectFileTests =
  testGroup
    "projectFilesIn"
    [ testCase "finds cabal.project and its variants, skipping freeze and local" $
        withTempTree "discover-projectfiles" $ \root -> do
          mapM_
            (\f -> writeFileIn root f "")
            [ "cabal.project"
            , "cabal.project.schema-gen"
            , "cabal.project.freeze"
            , "cabal.project.local"
            , "not-a-project.txt"
            ]
          found <- projectFilesIn root
          found @?= ["cabal.project", "cabal.project.schema-gen"]
    ]

-- ---------------------------------------------------------------------------

layoutTests :: TestTree
layoutTests =
  testGroup
    "a single-project repository"
    [ testCase "reports the suite, its commands and its source dirs" $
        withTempTree "discover-single" $ \root -> do
          writeFileIn root "cabal.project" "packages:\n  pkg\n"
          writeFileIn root "pkg/widget.cabal" $
            unlines
              [ "name: widget"
              , "version: 0.1"
              , ""
              , "test-suite widget-test"
              , "  type: exitcode-stdio-1.0"
              , "  hs-source-dirs: test"
              , "  main-is: Spec.hs"
              ]
          writeFileIn root "pkg/test/Spec.hs" "main = defaultMainStreaming tests"

          d <- discover root
          case flattenSuites d of
            [sr] -> do
              let ts = srSuite sr
              tsName ts @?= "widget-test"
              tsMainIs ts @?= "test/Spec.hs"
              tsEntryPoint ts @?= Streaming
              tsHsSourceDirs ts @?= ["test"]
              srPackage sr @?= "widget"
              srPackageDir sr @?= "pkg"
              -- The default project needs no --project-file, and a streaming
              -- suite gets all three commands.
              srProjectFile sr @?= Nothing
              tsRunTestsCommand ts @?= "cabal test widget-test"
              tsStreamTestsCommand ts
                @?= Just "cabal test widget-test --test-options=--streaming-json"
              tsDiscoverCommand ts
                @?= Just "cabal test widget-test --test-options=--list-tests-json"
            other -> assertFailure ("expected exactly one suite, got " <> show (length other))
    , testCase "an upstream suite gets no streaming commands" $
        withTempTree "discover-upstream" $ \root -> do
          writeFileIn root "cabal.project" "packages:\n  .\n"
          writeFileIn root "plain.cabal" $
            unlines
              [ "name: plain"
              , "test-suite plain-test"
              , "  hs-source-dirs: test"
              , "  main-is: Main.hs"
              ]
          writeFileIn root "test/Main.hs" "main = defaultMain tests"
          d <- discover root
          case flattenSuites d of
            [sr] -> do
              tsEntryPoint (srSuite sr) @?= Upstream
              -- --streaming-json / --list-tests-json are ingredients from
              -- convex-tasty-streaming; an upstream suite does not have them,
              -- so offering the commands would be a lie.
              tsStreamTestsCommand (srSuite sr) @?= Nothing
              tsDiscoverCommand (srSuite sr) @?= Nothing
              isCompatible (srSuite sr) @?= False
            _ -> assertFailure "expected exactly one suite"
    , testCase "no cabal.project at all yields one implicit project" $
        withTempTree "discover-implicit" $ \root -> do
          writeFileIn root "loose.cabal" $
            unlines
              [ "name: loose"
              , "test-suite loose-test"
              , "  hs-source-dirs: test"
              , "  main-is: Spec.hs"
              ]
          writeFileIn root "test/Spec.hs" "main = defaultMainStreaming tests"
          d <- discover root
          map projProjectFile (discProjects d) @?= [Nothing]
          -- An implicit project is still the default project: no flag.
          map srProjectFile (flattenSuites d) @?= [Nothing]
          discOrphans d @?= []
    , testCase "a package with no test suites is still reported" $
        withTempTree "discover-nosuites" $ \root -> do
          writeFileIn root "cabal.project" "packages:\n  .\n"
          writeFileIn root "libonly.cabal" "name: libonly\nlibrary\n  hs-source-dirs: lib\n"
          d <- discover root
          map (map pkgName . projPackages) (discProjects d) @?= [["libonly"]]
          flattenSuites d @?= []
    ]

importTests :: TestTree
importTests =
  testGroup
    "project imports and ownership"
    [ testCase "a variant project renders only the packages it adds" $
        withTempTree "discover-import" $ \root -> do
          writeFileIn root "cabal.project" "packages:\n  base-pkg\n"
          writeFileIn root "cabal.project.extra" "import: cabal.project\n\npackages:\n  extra-pkg\n"
          suitePkg root "base-pkg" "base" "base-test"
          suitePkg root "extra-pkg" "extra" "extra-test"

          d <- discover root
          -- base-pkg is reachable from the default project, so the default
          -- project owns it even though cabal.project.extra imports it too.
          renderedUnder d @?= [(Just "cabal.project", ["base"]), (Just "cabal.project.extra", ["extra"])]

          -- The --project-file flag must follow the same ownership decision.
          lookupFlag d "base-test" @?= Just Nothing
          lookupFlag d "extra-test" @?= Just (Just "cabal.project.extra")
    , testCase "a suite owned only by a variant project carries its flag" $
        withTempTree "discover-variant-flag" $ \root -> do
          writeFileIn root "cabal.project" "packages:\n  base-pkg\n"
          writeFileIn root "cabal.project.tools" "packages:\n  tool-pkg\n"
          suitePkg root "base-pkg" "base" "base-test"
          suitePkg root "tool-pkg" "tool" "tool-test"
          d <- discover root
          case findSuite "tool-test" d of
            Just sr -> do
              srProjectFile sr @?= Just "cabal.project.tools"
              tsRunTestsCommand (srSuite sr)
                @?= "cabal test tool-test --project-file=cabal.project.tools"
              tsStreamTestsCommand (srSuite sr)
                @?= Just "cabal test tool-test --project-file=cabal.project.tools --test-options=--streaming-json"
            Nothing -> assertFailure "tool-test was not discovered"
    , -- A source-repository-package stanza names dependencies, and its
      -- indented `subdir:` entries look just like package paths. Treating them
      -- as local packages would invent packages that do not exist.
      testCase "source-repository-package bodies are not read as packages" $
        withTempTree "discover-srp" $ \root -> do
          writeFileIn root "cabal.project" $
            unlines
              [ "packages:"
              , "  pkg"
              , ""
              , "source-repository-package"
              , "  type: git"
              , "  location: https://example.invalid/repo.git"
              , "  tag: deadbeef"
              , "  subdir:"
              , "    decoy"
              ]
          suitePkg root "pkg" "real" "real-test"
          -- A .cabal actually exists at the decoy path, so only the
          -- stanza-skipping logic can keep it out of the project.
          suitePkg root "decoy" "decoy" "decoy-test"

          d <- discover root
          renderedUnder d @?= [(Just "cabal.project", ["real"])]
          -- ... and it is therefore an orphan, not a package.
          map orphCabalFile (discOrphans d) @?= ["decoy/decoy.cabal"]
    , testCase "a cycle of imports terminates" $
        withTempTree "discover-cycle" $ \root -> do
          writeFileIn root "cabal.project" "import: cabal.project.b\npackages:\n  pkg\n"
          writeFileIn root "cabal.project.b" "import: cabal.project\n"
          suitePkg root "pkg" "cyclic" "cyclic-test"
          d <- discover root
          map (tsName . srSuite) (flattenSuites d) @?= ["cyclic-test"]
    , testCase "comments and inline package entries are handled" $
        withTempTree "discover-comments" $ \root -> do
          writeFileIn root "cabal.project" $
            unlines
              [ "-- a leading comment"
              , "packages: pkg-a pkg-b -- trailing comment"
              , ""
              , "tests: True"
              , "  -- an indented line after a closing field is not a package"
              ]
          suitePkg root "pkg-a" "a" "a-test"
          suitePkg root "pkg-b" "b" "b-test"
          d <- discover root
          sort (map (tsName . srSuite) (flattenSuites d)) @?= ["a-test", "b-test"]
    , -- cabal accepts a comma-separated packages list; the shell tool this was
      -- ported from split on whitespace only and would drop such an entry.
      testCase "comma-separated package entries resolve" $
        withTempTree "discover-commas" $ \root -> do
          writeFileIn root "cabal.project" "packages:\n  pkg-a,\n  pkg-b\n"
          suitePkg root "pkg-a" "a" "a-test"
          suitePkg root "pkg-b" "b" "b-test"
          d <- discover root
          sort (map (tsName . srSuite) (flattenSuites d)) @?= ["a-test", "b-test"]
    ]

missingMainTests :: TestTree
missingMainTests =
  testGroup
    "an unresolvable main-is"
    [ testCase "is reported as MISSING rather than dropped" $
        withTempTree "discover-missing" $ \root -> do
          writeFileIn root "cabal.project" "packages:\n  .\n"
          writeFileIn root "gap.cabal" $
            unlines
              [ "name: gap"
              , "test-suite gap-test"
              , "  hs-source-dirs: test"
              , "  main-is: Nope.hs"
              ]
          d <- discover root
          case flattenSuites d of
            [sr] -> do
              tsEntryPoint (srSuite sr) @?= MissingEntry
              tsMainIs (srSuite sr) @?= "MISSING"
              -- Still runnable as far as we know, so the run command stands.
              tsRunTestsCommand (srSuite sr) @?= "cabal test gap-test"
              tsStreamTestsCommand (srSuite sr) @?= Nothing
            _ -> assertFailure "expected the suite to be reported anyway"
    , testCase "main-is is resolved against each hs-source-dir in order" $
        withTempTree "discover-multidir" $ \root -> do
          writeFileIn root "cabal.project" "packages:\n  .\n"
          writeFileIn root "multi.cabal" $
            unlines
              [ "name: multi"
              , "test-suite multi-test"
              , "  hs-source-dirs: first, second"
              , "  main-is: Spec.hs"
              ]
          -- Only the second dir has the file, so a naive "take the first dir"
          -- resolution would report MISSING.
          writeFileIn root "second/Spec.hs" "main = defaultMainStreaming tests"
          d <- discover root
          case flattenSuites d of
            [sr] -> do
              tsHsSourceDirs (srSuite sr) @?= ["first", "second"]
              tsMainIs (srSuite sr) @?= "second/Spec.hs"
              tsEntryPoint (srSuite sr) @?= Streaming
            _ -> assertFailure "expected exactly one suite"
    , testCase "a stanza with no hs-source-dirs defaults to the package dir" $
        withTempTree "discover-nodirs" $ \root -> do
          writeFileIn root "cabal.project" "packages:\n  .\n"
          writeFileIn root "flat.cabal" $
            unlines
              [ "name: flat"
              , "test-suite flat-test"
              , "  main-is: Spec.hs"
              ]
          writeFileIn root "Spec.hs" "main = defaultMainStreaming tests"
          d <- discover root
          case flattenSuites d of
            [sr] -> do
              tsHsSourceDirs (srSuite sr) @?= ["."]
              tsEntryPoint (srSuite sr) @?= Streaming
            _ -> assertFailure "expected exactly one suite"
    ]

orphanTests :: TestTree
orphanTests =
  testGroup
    "orphans"
    [ testCase "a .cabal no project references is an orphan" $
        withTempTree "discover-orphan" $ \root -> do
          writeFileIn root "cabal.project" "packages:\n  kept\n"
          suitePkg root "kept" "kept" "kept-test"
          suitePkg root "dropped" "dropped" "dropped-test"
          d <- discover root
          map orphCabalFile (discOrphans d) @?= ["dropped/dropped.cabal"]
          map orphPackageDir (discOrphans d) @?= ["dropped"]
          -- An orphan contributes no suites.
          map (tsName . srSuite) (flattenSuites d) @?= ["kept-test"]
    , testCase "build artefact directories are pruned" $
        withTempTree "discover-prune" $ \root -> do
          writeFileIn root "cabal.project" "packages:\n  pkg\n"
          suitePkg root "pkg" "pkg" "pkg-test"
          -- These would otherwise show up as orphans on every scan.
          writeFileIn root "dist-newstyle/vendored.cabal" "name: vendored\n"
          writeFileIn root "node_modules/dep/dep.cabal" "name: dep\n"
          d <- discover root
          discOrphans d @?= []
    ]

globPackagesTests :: TestTree
globPackagesTests =
  testGroup
    "globbed package entries"
    [ testCase "*/*.cabal picks up every package directory" $
        withTempTree "discover-glob" $ \root -> do
          writeFileIn root "cabal.project" "packages:\n  */*.cabal\n"
          suitePkg root "alpha" "alpha" "alpha-test"
          suitePkg root "beta" "beta" "beta-test"
          d <- discover root
          sort (map (tsName . srSuite) (flattenSuites d)) @?= ["alpha-test", "beta-test"]
          discOrphans d @?= []
    , testCase "a bare directory entry takes the .cabal inside it" $
        withTempTree "discover-baredir" $ \root -> do
          writeFileIn root "cabal.project" "packages:\n  nested/pkg/\n"
          suitePkg root "nested/pkg" "nested" "nested-test"
          d <- discover root
          map (tsName . srSuite) (flattenSuites d) @?= ["nested-test"]
    ]

compatibilityTests :: TestTree
compatibilityTests =
  testGroup
    "compatibleOnly"
    [ testCase "keeps streaming suites and drops the rest" $
        withTempTree "discover-compatonly" $ \root -> do
          writeFileIn root "cabal.project" "packages:\n  mixed\n"
          writeFileIn root "mixed/mixed.cabal" $
            unlines
              [ "name: mixed"
              , "test-suite good-test"
              , "  hs-source-dirs: good"
              , "  main-is: Spec.hs"
              , ""
              , "test-suite plain-test"
              , "  hs-source-dirs: plain"
              , "  main-is: Spec.hs"
              ]
          writeFileIn root "mixed/good/Spec.hs" "main = defaultMainStreaming tests"
          writeFileIn root "mixed/plain/Spec.hs" "main = defaultMain tests"

          d <- discover root
          sort (map (tsName . srSuite) (flattenSuites d)) @?= ["good-test", "plain-test"]
          map (tsName . srSuite) (flattenSuites (compatibleOnly d)) @?= ["good-test"]
    , testCase "prunes packages and projects left empty" $
        withTempTree "discover-compatprune" $ \root -> do
          writeFileIn root "cabal.project" "packages:\n  .\n"
          writeFileIn root "plain.cabal" $
            unlines
              [ "name: plain"
              , "test-suite plain-test"
              , "  hs-source-dirs: test"
              , "  main-is: Spec.hs"
              ]
          writeFileIn root "test/Spec.hs" "main = defaultMain tests"
          d <- discover root
          -- Nothing compatible: no empty project or package shells left over.
          discProjects (compatibleOnly d) @?= []
    ]

jsonShapeTests :: TestTree
jsonShapeTests =
  testGroup
    "JSON shape"
    [ testCase "a suite serialises exactly the schema's seven fields" $
        withTempTree "discover-json" $ \root -> do
          writeFileIn root "cabal.project" "packages:\n  pkg\n"
          suitePkg root "pkg" "shape" "shape-test"
          d <- discover root
          -- The schema sets additionalProperties: false, so an extra field
          -- would break every existing consumer of this document.
          suiteKeys (toJSON d)
            @?= [ "discoverCommand"
                , "entryPoint"
                , "hsSourceDirs"
                , "mainIs"
                , "name"
                , "runTestsCommand"
                , "streamTestsCommand"
                ]
    , testCase "the document has exactly root, projects and orphans" $
        withTempTree "discover-json-top" $ \root -> do
          writeFileIn root "cabal.project" "packages:\n  pkg\n"
          suitePkg root "pkg" "shape" "shape-test"
          d <- discover root
          sort (topKeys (toJSON d)) @?= ["orphans", "projects", "root"]
    , testCase "root is absolute even for a relative argument" $ do
        d <- discover "."
        assertBool "root should be absolute" (take 1 (discRoot d) == "/")
    ]

listingTests :: TestTree
listingTests =
  testGroup
    "renderSuiteNames"
    [ -- The default `suites` output. Bare names, so the list composes:
      -- `pbt-cli suites | xargs pbt-cli run` has to work.
      testCase "is one bare suite name per line" $
        withTempTree "discover-names" $ \root -> do
          writeFileIn root "cabal.project" "packages:\n  a-pkg\n  b-pkg\n"
          suitePkg root "a-pkg" "a" "a-test"
          suitePkg root "b-pkg" "b" "b-test"
          d <- discover root
          renderSuiteNames d @?= "a-test\nb-test\n"
    , testCase "is empty, not blank, when there is nothing to list" $
        withTempTree "discover-names-empty" $ \root -> do
          writeFileIn root "cabal.project" "packages:\n  .\n"
          writeFileIn root "libonly.cabal" "name: libonly\nlibrary\n  hs-source-dirs: lib\n"
          d <- discover root
          renderSuiteNames d @?= ""
    ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Write a minimal package with one streaming test suite.
suitePkg
  :: FilePath
  -- ^ repository root
  -> FilePath
  -- ^ package directory, relative to root
  -> String
  -- ^ package name (also the @.cabal@ basename)
  -> String
  -- ^ test-suite name
  -> IO ()
suitePkg root dir name suite = do
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

-- | Which package names each project renders, in document order.
renderedUnder :: Discovery -> [(Maybe FilePath, [Text])]
renderedUnder d =
  [(projProjectFile p, map pkgName (projPackages p)) | p <- discProjects d]

-- | The @--project-file@ a named suite would be invoked with.
lookupFlag :: Discovery -> Text -> Maybe (Maybe FilePath)
lookupFlag d name = srProjectFile <$> findSuite name d

-- | The keys of the first test-suite object in a serialised discovery.
suiteKeys :: Value -> [Text]
suiteKeys v = sort (objectKeysAt ["projects", "packages", "testSuites"] v)

topKeys :: Value -> [Text]
topKeys = objectKeysAt []

-- | Walk a path of array-valued keys, then report the keys of what is found.
objectKeysAt :: [Text] -> Value -> [Text]
objectKeysAt path v = keysOf (foldl step (Just v) path)
 where
  step acc key = case acc of
    Just (Object km) -> case lookupKey key km of
      Just (Array xs) | not (null xs) -> Just (head' xs)
      other -> other
    _ -> Nothing

  keysOf = \case
    Just (Object km) -> keyNames km
    _ -> []

  head' = foldr (\x _ -> x) (Object mempty)

lookupKey :: Text -> KeyMap Value -> Maybe Value
lookupKey k = KeyMap.lookup (Key.fromText k)

keyNames :: KeyMap Value -> [Text]
keyNames = map Key.toText . KeyMap.keys
