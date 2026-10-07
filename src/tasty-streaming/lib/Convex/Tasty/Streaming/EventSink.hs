{- | The NDJSON event stream that the streaming reporter and the trace recorder
write to, from different threads.
-}
module Convex.Tasty.Streaming.EventSink (
  EventSink,
  newEventSink,
  emitTo,
) where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar)
import Convex.Tasty.Streaming.Types (Event (..))
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet

{- | Writes events one at a time, keeping each test's events in lifecycle
order: its @test_started@ goes out once, before anything else about it.

That order needs enforcing because two threads report on a running test.
The reporter announces the test once its own thread sees the test's status
change, but by then the test may already have streamed traces from the
thread running it. So whichever of them reports on a test first announces it.
-}
data EventSink = EventSink
  { esWrite :: Event -> IO ()
  , esStarted :: MVar IntSet
  -- ^ The tests announced so far. Holding it is also what keeps lines from interleaving.
  }

-- | A sink that writes through the given function, never running it concurrently.
newEventSink :: (Event -> IO ()) -> IO EventSink
newEventSink write = EventSink write <$> newMVar IntSet.empty

-- | Write an event, announcing its test first if that has not happened yet.
emitTo :: EventSink -> Event -> IO ()
emitTo EventSink{esWrite = write, esStarted = startedVar} evt =
  modifyMVar_ startedVar $ \started -> case evt of
    TestStarted i -> announce i started
    _ -> do
      started' <- maybe (pure started) (`announce` started) (reportedTest evt)
      started' <$ write evt
 where
  announce i started
    | IntSet.member i started = pure started
    | otherwise = IntSet.insert i started <$ write (TestStarted i)

{- | The test an event other than @test_started@ is about, if any. A trace
whose test was not found carries a negative id, and announces nothing.
-}
reportedTest :: Event -> Maybe Int
reportedTest evt = case evt of
  TestProgress{epId = i} -> Just i
  TestTrace{ettTestId = i} | i >= 0 -> Just i
  TestDone{edId = i} -> Just i
  _ -> Nothing
