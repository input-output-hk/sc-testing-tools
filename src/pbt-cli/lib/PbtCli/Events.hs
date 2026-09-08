{-# LANGUAGE OverloadedStrings #-}

{- | The NDJSON events a streaming test suite emits, as much of them as
@pbt-cli@ needs.

This is deliberately a *partial* view of @Convex.Tasty.Streaming.Types.Event@.
@pbt-cli@ has to keep working against suites built with an older or newer
@convex-tasty-streaming@ than the one it was compiled against, so every event
also keeps its raw 'Value' and any tag this module does not know about decodes
to 'EventOther' instead of failing. Only the fields the CLI actually renders or
makes decisions on are given names; @test_trace@ payloads, coverage indices,
threat-model summaries and QuickCheck monitoring stats are passed through
untouched.

The canonical, complete schema lives in
@src\/tasty-streaming\/schema\/streaming-events.schema.json@.
-}
module PbtCli.Events (
  -- * Events
  Event (..),
  TestInfo (..),
  Failure (..),
  eventTag,
  eventRaw,

  -- * Parsing a suite's stdout
  decodeEvent,
  isJsonObjectLine,
  jsonLines,
  eventsFrom,

  -- * Interpreting a run
  suiteOutcome,
  SuiteOutcome (..),
) where

import Data.Aeson (FromJSON (..), Value (..), eitherDecodeStrict', withObject, (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (parseEither)
import Data.ByteString.Char8 qualified as BS8
import Data.Maybe (mapMaybe)
import Data.Text (Text)

-- | One test in the tree, as reported by @suite_started@.
data TestInfo = TestInfo
  { tiId :: Int
  , tiName :: Text
  -- ^ the leaf label.
  , tiPath :: [Text]
  -- ^ group names from the root down to the test's parent.
  }
  deriving (Eq, Show)

instance FromJSON TestInfo where
  parseJSON = withObject "TestInfo" $ \o ->
    TestInfo <$> o .: "id" <*> o .: "name" <*> o .: "path"

-- | Why a test failed.
data Failure = Failure
  { failReason :: Text
  , failMessage :: Text
  }
  deriving (Eq, Show)

instance FromJSON Failure where
  parseJSON = withObject "Failure" $ \o ->
    Failure <$> o .: "reason" <*> o .: "message"

-- | A streaming event. Each constructor keeps the line's raw JSON.
data Event
  = {- | @suite_started@: the whole test tree, before anything runs. This is
    also the single event @--list-tests-json@ emits.
    -}
    EventSuiteStarted
      { evTests :: [TestInfo]
      , evSource :: Value
      }
  | -- | @test_started@
    EventTestStarted
      { evId :: Int
      , evSource :: Value
      }
  | -- | @test_done@
    EventTestDone
      { evId :: Int
      , evSuccess :: Bool
      , evDuration :: Double
      , evDescription :: Text
      , evFailure :: Maybe Failure
      , evSource :: Value
      }
  | -- | @suite_done@
    EventSuiteDone
      { evPassed :: Int
      , evFailed :: Int
      , evDuration :: Double
      , evSource :: Value
      }
  | {- | any other tag — @test_progress@, @test_trace@, or something added to
    the library after this binary was built.
    -}
    EventOther
      { evOtherTag :: Text
      , evSource :: Value
      }
  deriving (Eq, Show)

instance FromJSON Event where
  parseJSON v = withObject "Event" go v
   where
    go o = do
      tag :: Text <- o .: "event"
      case tag of
        "suite_started" -> EventSuiteStarted <$> o .: "tests" <*> pure v
        "test_started" -> EventTestStarted <$> o .: "id" <*> pure v
        "test_done" -> do
          i <- o .: "id"
          ok <- o .: "success"
          dur <- o .: "duration"
          desc <- o .: "description"
          fl <- if ok then pure Nothing else o .:? "failure"
          pure (EventTestDone i ok dur desc fl v)
        "suite_done" ->
          EventSuiteDone <$> o .: "passed" <*> o .: "failed" <*> o .: "duration" <*> pure v
        other -> pure (EventOther other v)

-- | The event's @event@ tag.
eventTag :: Event -> Text
eventTag = \case
  EventSuiteStarted{} -> "suite_started"
  EventTestStarted{} -> "test_started"
  EventTestDone{} -> "test_done"
  EventSuiteDone{} -> "suite_done"
  EventOther t _ -> t

-- | The event's original JSON, for pass-through output.
eventRaw :: Event -> Value
eventRaw = evSource

{- | Decode one line as an event.

'Left' covers both "not JSON at all" (cabal's own chatter) and "JSON, but not
a recognisable event", which the caller normally wants to treat the same way:
ignore it.
-}
decodeEvent :: BS8.ByteString -> Either String Event
decodeEvent line = do
  v <- eitherDecodeStrict' line
  parseEither parseJSON (v :: Value)

{- | Keep only the lines of a suite's stdout that are JSON objects.

@cabal test@ interleaves its own output — @Resolving dependencies@,
@Running 1 test suites...@, build progress — with the suite's NDJSON. This is
the equivalent of the @jq -R 'fromjson? // empty'@ idiom the tasty-streaming
README recommends, and for the same reason: there is no way to ask cabal to
stop talking, so the consumer filters.
-}
jsonLines :: [BS8.ByteString] -> [BS8.ByteString]
jsonLines = filter isJsonObjectLine

{- | Is this single line a JSON object?

The cheap @{@ check comes first so cabal's chatter is rejected without paying
for a parse attempt on every build-progress line.
-}
isJsonObjectLine :: BS8.ByteString -> Bool
isJsonObjectLine l =
  case BS8.uncons (BS8.dropWhile (`elem` (" \t" :: String)) l) of
    Just ('{', _) -> case Aeson.decodeStrict' l :: Maybe Value of
      Just (Object _) -> True
      _ -> False
    _ -> False

-- | Every decodable event in a suite's stdout, in order.
eventsFrom :: [BS8.ByteString] -> [Event]
eventsFrom = mapMaybe (either (const Nothing) Just . decodeEvent) . jsonLines

-- | What a completed run amounted to.
data SuiteOutcome = SuiteOutcome
  { outPassed :: Int
  , outFailed :: Int
  , outDuration :: Double
  }
  deriving (Eq, Show)

{- | The run's @suite_done@ tally, if the suite got that far.

'Nothing' means the suite never reported a summary — it crashed, was killed, or
was not a streaming suite at all — which is why callers must not treat a
missing summary as success.
-}
suiteOutcome :: [Event] -> Maybe SuiteOutcome
suiteOutcome evs = case [e | e@EventSuiteDone{} <- evs] of
  [] -> Nothing
  (e : _) -> Just (SuiteOutcome (evPassed e) (evFailed e) (evDuration e))
