{-# LANGUAGE OverloadedStrings #-}

module PbtCli.EventsSpec (eventsTests) where

import Data.ByteString.Char8 qualified as BS8
import PbtCli.Events (
  Event (..),
  Failure (..),
  SuiteOutcome (..),
  TestInfo (..),
  decodeEvent,
  eventTag,
  eventsFrom,
  isJsonObjectLine,
  jsonLines,
  suiteOutcome,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

eventsTests :: TestTree
eventsTests =
  testGroup
    "Events"
    [ decodeTests
    , noiseTests
    , outcomeTests
    ]

decodeTests :: TestTree
decodeTests =
  testGroup
    "decodeEvent"
    [ testCase "suite_started carries the test tree" $
        case decodeEvent suiteStartedLine of
          Right (EventSuiteStarted tests _) ->
            tests
              @?= [ TestInfo 0 "first" ["group", "unit tests"]
                  , TestInfo 1 "second" ["group"]
                  ]
          other -> assertFailure ("expected suite_started, got " <> show other)
    , testCase "test_started carries the id" $
        case decodeEvent "{\"event\":\"test_started\",\"id\":7}" of
          Right ev@EventTestStarted{} -> evId ev @?= 7
          other -> assertFailure ("expected test_started, got " <> show other)
    , testCase "a passing test_done has no failure" $
        case decodeEvent passLine of
          Right ev@EventTestDone{} -> do
            evId ev @?= 0
            evSuccess ev @?= True
            evDuration ev @?= 0.217
            evDescription ev @?= "First bid equals minimum bid"
            evFailure ev @?= Nothing
          other -> assertFailure ("expected test_done, got " <> show other)
    , testCase "a failing test_done carries the reason and message" $
        case decodeEvent failLine of
          Right ev@EventTestDone{} -> do
            evSuccess ev @?= False
            evFailure ev @?= Just (Failure "TestFailed" "Expected 1 but got 2")
          other -> assertFailure ("expected test_done, got " <> show other)
    , testCase "suite_done carries the tally" $
        case decodeEvent doneLine of
          Right ev@EventSuiteDone{} -> (evPassed ev, evFailed ev, evDuration ev) @?= (55, 0, 79.6)
          other -> assertFailure ("expected suite_done, got " <> show other)
    , -- The library can add event kinds (test_progress, test_trace, and
      -- whatever comes next) without pbt-cli being rebuilt, so an unknown tag
      -- must decode rather than abort the stream.
      testCase "an unknown event tag decodes to EventOther" $
        case decodeEvent "{\"event\":\"invented_later\",\"whatever\":1}" of
          Right ev@EventOther{} -> eventTag ev @?= "invented_later"
          other -> assertFailure ("expected EventOther, got " <> show other)
    , testCase "test_trace is passed through, not rejected" $
        case decodeEvent "{\"event\":\"test_trace\",\"id\":3,\"category\":\"c\",\"trace\":{},\"covered\":[]}" of
          Right ev@EventOther{} -> eventTag ev @?= "test_trace"
          other -> assertFailure ("expected EventOther, got " <> show other)
    , testCase "a non-JSON line fails to decode" $
        case decodeEvent "Running 1 test suites..." of
          Left _ -> pure ()
          Right ev -> assertFailure ("expected a decode failure, got " <> show ev)
    , testCase "a JSON object with no event tag fails to decode" $
        case decodeEvent "{\"hello\":1}" of
          Left _ -> pure ()
          Right ev -> assertFailure ("expected a decode failure, got " <> show ev)
    ]

noiseTests :: TestTree
noiseTests =
  testGroup
    "filtering cabal's output"
    [ testCase "only JSON object lines survive" $
        jsonLines mixedOutput @?= [passLine, doneLine]
    , testCase "build chatter is rejected" $
        map isJsonObjectLine ["Resolving dependencies...", "Running 1 test suites...", "Build profile: -w ghc-9.6.6"]
          @?= [False, False, False]
    , -- A line that starts with '{' but is truncated (the suite was killed
      -- mid-write) must not be forwarded as if it were an event.
      testCase "a truncated JSON line is rejected" $
        isJsonObjectLine "{\"event\":\"test_do" @?= False
    , testCase "a bare JSON array is rejected" $
        isJsonObjectLine "[1,2,3]" @?= False
    , testCase "leading whitespace is tolerated" $
        isJsonObjectLine "  {\"event\":\"test_started\",\"id\":1}" @?= True
    , testCase "eventsFrom decodes past the noise" $
        map eventTag (eventsFrom mixedOutput) @?= ["test_done", "suite_done"]
    ]

outcomeTests :: TestTree
outcomeTests =
  testGroup
    "suiteOutcome"
    [ testCase "reports the summary when the suite finished" $
        suiteOutcome (eventsFrom mixedOutput) @?= Just (SuiteOutcome 55 0 79.6)
    , -- A suite that crashed emits no suite_done. Treating that as a pass
      -- would report a broken run as green.
      testCase "is Nothing when the suite never finished" $
        suiteOutcome (eventsFrom [passLine]) @?= Nothing
    ]

-- ---------------------------------------------------------------------------

suiteStartedLine :: BS8.ByteString
suiteStartedLine =
  "{\"event\":\"suite_started\",\"coverageIndex\":[],\"tests\":\
  \[{\"id\":0,\"name\":\"first\",\"path\":[\"group\",\"unit tests\"]},\
  \{\"id\":1,\"name\":\"second\",\"path\":[\"group\"]}]}"

passLine :: BS8.ByteString
passLine =
  "{\"event\":\"test_done\",\"id\":0,\"success\":true,\"duration\":0.217,\
  \\"description\":\"First bid equals minimum bid\"}"

failLine :: BS8.ByteString
failLine =
  "{\"event\":\"test_done\",\"id\":1,\"success\":false,\"duration\":0.456,\
  \\"description\":\"Positive tests\",\
  \\"failure\":{\"reason\":\"TestFailed\",\"message\":\"Expected 1 but got 2\"}}"

doneLine :: BS8.ByteString
doneLine = "{\"event\":\"suite_done\",\"passed\":55,\"failed\":0,\"duration\":79.6}"

-- What `cabal test --test-options=--streaming-json` actually writes: the
-- suite's NDJSON interleaved with cabal's own progress reporting.
mixedOutput :: [BS8.ByteString]
mixedOutput =
  [ "Resolving dependencies..."
  , "Build profile: -w ghc-9.6.6 -O1"
  , "Running 1 test suites..."
  , passLine
  , doneLine
  , "Test suite convex-vesting-test: PASS"
  ]
