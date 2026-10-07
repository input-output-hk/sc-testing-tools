{- | Tests for the order 'Convex.Tasty.Streaming.EventSink.EventSink' writes
events in: a test's @test_started@ once, before anything else about it.
-}
module Convex.Tasty.EventSinkSpec (tests) where

import Convex.Tasty.HUnit (Assertion, testCase, (@?=))
import Convex.Tasty.Streaming.EventSink (emitTo, newEventSink)
import Convex.Tasty.Streaming.Types (Event (..), TestOutcome (..))
import Data.Aeson qualified as Aeson
import Data.Foldable (traverse_)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "EventSink"
    [ testCase "a trace that gets ahead of the reporter announces its test" traceAnnouncesItsTest
    , testCase "test_started goes out once per test" startedGoesOutOnce
    , testCase "a trace with no test announces nothing" unresolvedTraceAnnouncesNothing
    ]

-- | What a sink writes, given the events emitted to it, in order.
written :: [Event] -> IO [Event]
written evts = do
  out <- newIORef []
  sink <- newEventSink (\e -> modifyIORef' out (e :))
  traverse_ (emitTo sink) evts
  reverse <$> readIORef out

trace :: Int -> Event
trace i = TestTrace{ettTestId = i, ettCategory = "positive", ettCovered = [], ettTrace = Aeson.Null}

done :: Int -> Event
done i =
  TestDone
    { edId = i
    , edOutcome = TestSuccess
    , edDuration = 0
    , edDescription = ""
    , edThreatModel = Nothing
    , edMonitoringStats = Nothing
    }

{- | The interleaving seen in a real run: test 20 failed after one iteration,
and test 21's first trace was written before the reporter's threads had
caught up with either test.
-}
traceAnnouncesItsTest :: Assertion
traceAnnouncesItsTest = do
  out <- written [TestStarted 20, trace 21, done 20, TestStarted 21, trace 21, done 21]
  out @?= [TestStarted 20, TestStarted 21, trace 21, done 20, trace 21, done 21]

startedGoesOutOnce :: Assertion
startedGoesOutOnce = do
  out <- written [TestStarted 3, TestStarted 3, trace 3, TestStarted 3, done 3]
  out @?= [TestStarted 3, trace 3, done 3]

-- | The recorder gives a trace whose test it could not find the id -1.
unresolvedTraceAnnouncesNothing :: Assertion
unresolvedTraceAnnouncesNothing = do
  out <- written [trace (-1), TestStarted 0, trace (-1)]
  out @?= [trace (-1), TestStarted 0, trace (-1)]
