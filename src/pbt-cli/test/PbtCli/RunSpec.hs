{-# LANGUAGE OverloadedStrings #-}

module PbtCli.RunSpec (runTests) where

import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text (Text)
import PbtCli.Cabal (
  Invocation (..),
  TestOptions (..),
  noTestOptions,
  renderCommand,
  testInvocation,
  testOptionArgs,
 )
import PbtCli.Discover (EntryPoint (..), SuiteRef (..), TestSuite (..), isCompatible)
import PbtCli.Render (renderSuiteBanner, tagEventWithSuite)
import PbtCli.Run (groupByProject, selectSuites)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

runTests :: TestTree
runTests =
  testGroup
    "Run"
    [ testOptionTests
    , invocationTests
    , renderTests
    , selectionTests
    , groupingTests
    , eventTaggingTests
    , incompatibilityTests
    ]

testOptionTests :: TestTree
testOptionTests =
  testGroup
    "testOptionArgs"
    [ testCase "no options means no flags" $
        argsOf noTestOptions @?= []
    , testCase "streaming-json is a single flag" $
        argsOf noTestOptions{toStreamingJson = True} @?= ["--test-option=--streaming-json"]
    , -- Cabal splits --test-options (plural) on whitespace, which mangles any
      -- Tasty pattern containing a space — and Tasty group names have spaces.
      -- The singular --test-option appends one argument verbatim, so a
      -- value-taking flag has to become two of them.
      testCase "a pattern with spaces stays one argument" $
        argsOf noTestOptions{toPattern = Just "auction tests"}
          @?= ["--test-option=-p", "--test-option=auction tests"]
    , testCase "test ids are passed as two arguments" $
        argsOf noTestOptions{toTestIds = Just "0,3,7"}
          @?= ["--test-option=--test-id", "--test-option=0,3,7"]
    , testCase "threat model names are passed as two arguments" $
        argsOf noTestOptions{toThreatModelName = Just "TokenForgery"}
          @?= ["--test-option=--threat-model-name", "--test-option=TokenForgery"]
    , testCase "extra options are passed through in order" $
        argsOf noTestOptions{toExtra = ["--quickcheck-tests", "500"]}
          @?= ["--test-option=--quickcheck-tests", "--test-option=500"]
    , testCase "combined options keep flags before filters" $
        argsOf noTestOptions{toStreamingJson = True, toTestIds = Just "4"}
          @?= [ "--test-option=--streaming-json"
              , "--test-option=--test-id"
              , "--test-option=4"
              ]
    ]
 where
  argsOf = testOptionArgs

invocationTests :: TestTree
invocationTests =
  testGroup
    "testInvocation"
    [ testCase "a default-project suite gets no --project-file" $
        invArgs (testInvocation "/usr/bin/cabal" "/repo" Nothing ["widget-test"] noTestOptions)
          @?= ["test", "widget-test"]
    , testCase "a variant-project suite carries the flag" $
        invArgs
          ( testInvocation
              "/usr/bin/cabal"
              "/repo"
              (Just "cabal.project.schema-gen")
              ["gen-test"]
              noTestOptions
          )
          @?= ["test", "gen-test", "--project-file=cabal.project.schema-gen"]
    , testCase "several targets go into one cabal call" $
        invArgs (testInvocation "cabal" "/repo" Nothing ["a-test", "b-test"] noTestOptions)
          @?= ["test", "a-test", "b-test"]
    , testCase "no targets means cabal's all target" $
        invArgs (testInvocation "cabal" "/repo" Nothing [] noTestOptions)
          @?= ["test", "all"]
    , testCase "the project flag precedes the test options" $
        invArgs
          ( testInvocation
              "cabal"
              "/repo"
              (Just "cabal.project.tools")
              ["t"]
              noTestOptions{toListTestsJson = True}
          )
          @?= ["test", "t", "--project-file=cabal.project.tools", "--test-option=--list-tests-json"]
    , testCase "the working directory is the repository root" $
        invWorkingDir (testInvocation "cabal" "/repo" Nothing ["t"] noTestOptions) @?= Just "/repo"
    ]

renderTests :: TestTree
renderTests =
  testGroup
    "renderCommand"
    [ testCase "a plain command needs no quoting" $
        renderCommand (testInvocation "cabal" "/repo" Nothing ["widget-test"] noTestOptions)
          @?= "cabal test widget-test"
    , -- --dry-run output is meant to be pasted into a shell, so a value with a
      -- space must come back quoted or the pasted command would not do the
      -- same thing.
      testCase "an argument with a space is quoted" $
        renderCommand
          (testInvocation "cabal" "/repo" Nothing ["t"] noTestOptions{toPattern = Just "auction tests"})
          @?= "cabal test t --test-option=-p '--test-option=auction tests'"
    , testCase "an embedded single quote is escaped" $
        renderCommand
          (testInvocation "cabal" "/repo" Nothing ["t"] noTestOptions{toPattern = Just "it's"})
          @?= "cabal test t --test-option=-p '--test-option=it'\\''s'"
    ]

selectionTests :: TestTree
selectionTests =
  testGroup
    "selectSuites"
    [ testCase "no names selects everything" $
        fmap (map name) (selectSuites available []) @?= Right ["a-test", "b-test", "c-test"]
    , testCase "named suites are selected in the order given" $
        fmap (map name) (selectSuites available ["c-test", "a-test"]) @?= Right ["c-test", "a-test"]
    , testCase "an unknown name is an error naming the suite" $
        fmap (map name) (selectSuites available ["nope"])
          @?= Left "unknown test suite: nope"
    ]
 where
  name = tsName . srSuite

groupingTests :: TestTree
groupingTests =
  testGroup
    "groupByProject"
    [ -- One cabal call per project file is the point: it keeps `pbt-cli run`
      -- to two invocations for this repo instead of one per suite.
      testCase "suites are grouped by their project file" $
        map (fmap (map (tsName . srSuite))) (groupByProject available)
          @?= [ (Nothing, ["a-test", "b-test"])
              , (Just "cabal.project.tools", ["c-test"])
              ]
    , testCase "first-appearance order of projects is preserved" $
        map fst (groupByProject (reverse available))
          @?= [Just "cabal.project.tools", Nothing]
    , testCase "an empty selection groups to nothing" $
        map fst (groupByProject []) @?= []
    ]

eventTaggingTests :: TestTree
eventTaggingTests =
  testGroup
    "run --json event tagging"
    [ -- `run --json` can cover many suites and the event schema carries no
      -- suite identity, so a consumer would see several indistinguishable
      -- suite_started events. The schema does not forbid additional
      -- properties, so naming the suite on each event is schema-valid.
      testCase "a suite field is added to an event object" $
        tagEventWithSuite "widget-test" (object ["event" .= ("test_started" :: Text), "id" .= (3 :: Int)])
          @?= object
            [ "event" .= ("test_started" :: Text)
            , "id" .= (3 :: Int)
            , "suite" .= ("widget-test" :: Text)
            ]
    , testCase "existing fields are preserved" $
        case tagEventWithSuite "s" (object ["event" .= ("suite_done" :: Text), "passed" .= (2 :: Int)]) of
          Object km ->
            (KeyMap.lookup "passed" km, KeyMap.lookup "event" km)
              @?= (Just (Number 2), Just (String "suite_done"))
          other -> assertFailure ("expected an object, got " <> show other)
    , -- The schema never produces one, but a malformed line must not crash the
      -- forwarder.
      testCase "a non-object event is passed through unchanged" $
        tagEventWithSuite "s" (String "not an event") @?= String "not an event"
    , testCase "the banner names the suite" $
        renderSuiteBanner "widget-test" @?= "== widget-test =="
    ]

incompatibilityTests :: TestTree
incompatibilityTests =
  testGroup
    "compatibility of a selection"
    [ -- What run --stream / --json check before invoking anything: an
      -- upstream-tasty suite would ignore --streaming-json, print its usual
      -- console output and exit 0, leaving a consumer waiting for events.
      testCase "an upstream suite is not compatible" $
        map (isCompatible . srSuite) (available <> [upstreamRef]) @?= [True, True, True, False]
    ]

-- ---------------------------------------------------------------------------

available :: [SuiteRef]
available =
  [ suiteRef "a-test" Nothing
  , suiteRef "b-test" Nothing
  , suiteRef "c-test" (Just "cabal.project.tools")
  ]

-- | A suite on plain upstream tasty: no streaming ingredients.
upstreamRef :: SuiteRef
upstreamRef =
  let sr = suiteRef "plain-test" Nothing
   in sr{srSuite = (srSuite sr){tsEntryPoint = Upstream}}

suiteRef :: Text -> Maybe FilePath -> SuiteRef
suiteRef nm projectFile =
  SuiteRef
    { srSuite =
        TestSuite
          { tsName = nm
          , tsMainIs = "test/Spec.hs"
          , tsEntryPoint = Streaming
          , tsRunTestsCommand = "cabal test " <> nm
          , tsStreamTestsCommand = Nothing
          , tsDiscoverCommand = Nothing
          , tsHsSourceDirs = ["test"]
          }
    , srProjectFile = projectFile
    , srPackage = "pkg"
    , srPackageDir = "pkg"
    }
