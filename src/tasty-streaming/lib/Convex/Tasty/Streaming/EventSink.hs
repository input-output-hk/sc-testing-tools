{- | The NDJSON event stream that the streaming reporter and the trace recorder
write to, from different threads.
-}
module Convex.Tasty.Streaming.EventSink (
  EventSink,
  newEventSink,
  emitTo,
  encodeLine,
) where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, withMVar)
import Control.Exception (evaluate)
import Convex.Tasty.Streaming.Types (Event (..))
import Data.Aeson (encode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (traverse_)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet

{- | Writes events one line at a time, keeping each test's events in lifecycle
order: its @test_started@ goes out once, before anything else about it.

That order needs enforcing because two threads report on a running test.
The reporter announces the test once its own thread sees the test's status
change, but by then the test may already have streamed traces from the
thread running it. So whichever of them reports on a test first announces it.
-}
data EventSink = EventSink
  { esWrite :: ByteString -> IO ()
  , esStarted :: MVar IntSet
  -- ^ The tests announced so far. Holding it is also what keeps lines from interleaving.
  }

-- | A sink that writes encoded lines through the given function, never running it concurrently.
newEventSink :: (ByteString -> IO ()) -> IO EventSink
newEventSink write = EventSink write <$> newMVar IntSet.empty

-- | An event as one NDJSON line, newline included.
encodeLine :: Event -> ByteString
encodeLine evt = BL.toStrict (encode evt) <> "\n"

{- | Write an event, announcing its test first if that has not happened yet.

The event is encoded before the sink is locked: encoding forces a trace's
lazy payload, which is slow to build and can throw, and neither should hold
up the other writers or leave anything half-written. An announcement counts
as made once it is written, so failing to write the event after it cannot
get the test announced twice.
-}
emitTo :: EventSink -> Event -> IO ()
emitTo EventSink{esWrite = write, esStarted = startedVar} evt = case evt of
  TestStarted i -> announce i
  _ -> do
    line <- evaluate (encodeLine evt)
    traverse_ announce (reportedTest evt)
    withMVar startedVar (\_ -> write line)
 where
  announce i = modifyMVar_ startedVar $ \started ->
    if IntSet.member i started
      then pure started
      else IntSet.insert i started <$ write (encodeLine (TestStarted i))

{- | The test an event other than @test_started@ is about, if any. A trace
whose test was not found carries a negative id, and announces nothing.
-}
reportedTest :: Event -> Maybe Int
reportedTest evt = case evt of
  TestProgress{epId = i} -> Just i
  TestTrace{ettTestId = i} | i >= 0 -> Just i
  TestDone{edId = i} -> Just i
  _ -> Nothing
