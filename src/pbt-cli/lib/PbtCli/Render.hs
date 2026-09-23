{-# LANGUAGE OverloadedStrings #-}

-- | Human-readable rendering: suite tables, test trees, and live run output.
module PbtCli.Render (
  -- * JSON
  encodeJsonPretty,
  encodeJsonCompact,

  -- * Suites
  renderSuiteNames,
  renderSuitesTable,
  renderSuitesTsv,

  -- * Tests
  renderTestTree,
  renderTestList,

  -- * Streaming
  renderEvent,
  renderEventWith,
  renderSuiteBanner,
  testNameIndex,
  tagEventWithSuite,
) where

import Data.Aeson (ToJSON, Value (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (Config (..), Indent (Spaces), NumberFormat (Generic), defConfig, encodePretty', keyOrder)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import PbtCli.Discover (
  Discovery (..),
  EntryPoint (..),
  Orphan (..),
  Package (..),
  Project (..),
  SuiteRef (..),
  TestSuite (..),
  entryPointText,
  flattenSuites,
  isCompatible,
 )
import PbtCli.Events (Event (..), Failure (..), TestInfo (..))
import System.FilePath ((</>))

-- ---------------------------------------------------------------------------
-- JSON
-- ---------------------------------------------------------------------------

{- | Pretty JSON with discovery's keys in their documented order.

aeson orders an object's keys by its own key map, which is alphabetical — so
without this a suite would print @discoverCommand@ before @name@. The order
below is the one @list-test-suites.schema.json@ documents and the shell tool
emits, which is also the order that reads best: identity first, then
classification, then the commands. Keys not listed keep aeson's order and sort
after the listed ones.
-}
encodeJsonPretty :: (ToJSON a) => a -> LBS8.ByteString
encodeJsonPretty =
  encodePretty'
    defConfig
      { confIndent = Spaces 2
      , confCompare = documentedKeyOrder
      , confNumFormat = Generic
      , confTrailingNewline = True
      }

{- | Add a @suite@ field to an event, naming the suite that produced it.

@run --json@ can cover many suites, and the event schema carries no suite
identity — a consumer would see several @suite_started@ events with no way to
tell them apart. The streaming-events schema does not set
@additionalProperties: false@, so an added field is schema-valid, and a
consumer that ignores unknown fields is unaffected.

A non-object event (which the schema does not produce) is returned unchanged.
-}
tagEventWithSuite :: Text -> Value -> Value
tagEventWithSuite suite = \case
  Object km -> Object (KeyMap.insert "suite" (String suite) km)
  other -> other

documentedKeyOrder :: Text -> Text -> Ordering
documentedKeyOrder =
  keyOrder
    [ -- document
      "root"
    , "projects"
    , "orphans"
    , -- project
      "projectFile"
    , "packages"
    , -- package (and orphan)
      "name"
    , "cabalFile"
    , "packageDir"
    , "testSuites"
    , -- test suite
      "mainIs"
    , "entryPoint"
    , "runTestsCommand"
    , "streamTestsCommand"
    , "discoverCommand"
    , "hsSourceDirs"
    ]

{- | Single-line JSON, for piping into @jq@ or another process.

Key order is aeson's here, not the documented one: a JSON object is unordered
by definition, every consumer parses it, and 'encodeJsonPretty' is the variant
meant to be read.
-}
encodeJsonCompact :: (ToJSON a) => a -> LBS8.ByteString
encodeJsonCompact = Aeson.encode

-- ---------------------------------------------------------------------------
-- Suites
-- ---------------------------------------------------------------------------

{- | One suite name per line — the default @suites@ output.

Just the names, with no decoration, so the common uses compose:
@pbt-cli suites | wc -l@, or feeding the list straight back into
@pbt-cli run@. Compatibility is expressed by filtering
(@--compatible-only@) rather than by annotating, so the output stays a clean
list of arguments.
-}
renderSuiteNames :: Discovery -> Text
renderSuiteNames d = Text.unlines [tsName (srSuite sr) | sr <- flattenSuites d]

{- | A table of every discovered suite, grouped by project file.

The @COMPAT@ column is the answer to "which suites can I stream and discover
tests in", which is the question this command exists to answer, so it gets a
symbol rather than another word of prose.
-}
renderSuitesTable :: Discovery -> Text
renderSuitesTable d =
  Text.unlines $
    concat
      [ ["root: " <> Text.pack (discRoot d), ""]
      , concatMap projectBlock (discProjects d)
      , orphanBlock
      , [summary]
      ]
 where
  allSuites = flattenSuites d
  compatCount = length (filter (isCompatible . srSuite) allSuites)

  projectBlock proj =
    [ "project: " <> maybe "(implicit — no cabal.project)" Text.pack (projProjectFile proj)
    ]
      <> ( if null rows
             then ["  (no test suites)", ""]
             else map ("  " <>) (table header rows) <> [""]
         )
   where
    rows =
      [ [ tsName ts
        , pkgName pkg
        , compatMark ts
        , entryPointText (tsEntryPoint ts)
        , tsMainIs ts
        ]
      | pkg <- projPackages proj
      , ts <- pkgTestSuites pkg
      ]

  header = ["SUITE", "PACKAGE", "PBT", "ENTRY POINT", "MAIN-IS"]

  -- A compatible suite supports --list-tests-json / --streaming-json.
  compatMark ts = if isCompatible ts then "yes" else "-"

  orphanBlock
    | null (discOrphans d) = []
    | otherwise =
        ["orphan .cabal files (referenced by no project):"]
          <> ["  " <> Text.pack (orphCabalFile o) | o <- discOrphans d]
          <> [""]

  summary =
    Text.pack (show (length allSuites))
      <> " test suite(s), "
      <> Text.pack (show compatCount)
      <> " sc-testing-tools compatible"

{- | The legacy tab-separated format:
@suite \\t packageDir \\t mainPath \\t entryPoint \\t hsSourceDirs@.

Kept byte-compatible with @list-test-suites.sh --tsv@ so existing editor
integrations and shell pipelines keep working: @mainPath@ is relative to the
scanned root (not to the package) and the source dirs are @;@-joined so the
column count is fixed even for a multi-dir stanza.
-}
renderSuitesTsv :: Discovery -> Text
renderSuitesTsv d = Text.unlines (map row (flattenSuites d))
 where
  row sr =
    Text.intercalate
      "\t"
      [ tsName ts
      , Text.pack (srPackageDir sr)
      , mainPath sr
      , entryPointText (tsEntryPoint ts)
      , Text.intercalate ";" (tsHsSourceDirs ts)
      ]
   where
    ts = srSuite sr

  mainPath sr
    | tsEntryPoint (srSuite sr) == MissingEntry = "MISSING"
    | otherwise = Text.pack (srPackageDir sr </> Text.unpack (tsMainIs (srSuite sr)))

{- | Lay out rows as a fixed-width table with a header and an underline.

Columns are sized to their widest cell and the last column is left unpadded,
so a long @main-is@ path never drags trailing spaces across the terminal.
-}
table :: [Text] -> [[Text]] -> [Text]
table header rows = layout header : layout underline : map layout rows
 where
  widths =
    [ maximum (0 : [Text.length (cell i r) | r <- header : rows])
    | i <- [0 .. columns - 1]
    ]

  columns = maximum (0 : map length (header : rows))

  underline = [Text.replicate (Text.length h) "-" | h <- header]

  cell i r = if i < length r then r !! i else ""

  layout r =
    Text.stripEnd . Text.intercalate "  " $
      [padTo w (cell i r) | (i, w) <- zip [0 ..] widths]

  padTo w t = t <> Text.replicate (w - Text.length t) " "

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

{- | The test tree as nested groups, with each test's id.

The id is what @--test-id@ takes, so it is shown first and unpadded: the point
of running discovery is usually to pick ids to re-run.
-}
renderTestTree :: [TestInfo] -> Text
renderTestTree = Text.unlines . go 0
 where
  go depth infos = concatMap (leaf depth) here <> concatMap (branch depth) groups
   where
    here = [t | t <- infos, null (tiPath t)]
    deeper = [t | t <- infos, not (null (tiPath t))]
    groups = groupOnFirst deeper

  leaf depth t = [indent depth <> "#" <> Text.pack (show (tiId t)) <> " " <> tiName t]

  branch depth (label, children) =
    (indent depth <> label <> "/")
      : go (depth + 1) [c{tiPath = drop 1 (tiPath c)} | c <- children]

  indent depth = Text.replicate depth "  "

-- | One line per test: @id\<TAB\>full/path/name@. Convenient for shell loops.
renderTestList :: [TestInfo] -> Text
renderTestList infos =
  Text.unlines
    [ Text.pack (show (tiId t)) <> "\t" <> Text.intercalate " / " (tiPath t <> [tiName t])
    | t <- sortOn tiId infos
    ]

-- | Group by the first path component, preserving first-appearance order.
groupOnFirst :: [TestInfo] -> [(Text, [TestInfo])]
groupOnFirst infos = [(k, [t | t <- infos, firstOf t == Just k]) | k <- keys]
 where
  firstOf t = case tiPath t of
    (p : _) -> Just p
    [] -> Nothing

  keys = dedupe [p | t <- infos, Just p <- [firstOf t]]

  dedupe = foldr (\x acc -> x : filter (/= x) acc) []

-- ---------------------------------------------------------------------------
-- Streaming
-- ---------------------------------------------------------------------------

{- | 'renderEventWith' with no name index: a @test_done@ whose @description@ is
empty renders without one.
-}
renderEvent :: Event -> Maybe Text
renderEvent = renderEventWith (const Nothing)

{- | Names for every test in a @suite_started@ event, keyed by id.

Feed this to 'renderEventWith'. Tasty providers differ in whether they set a
@test_done@ description — the shimmed @Convex.Tasty.HUnit@ ones do, plain
@Test.Tasty.HUnit@ ones send @""@ — so without the tree a live run would print
a column of anonymous PASS lines.
-}
testNameIndex :: [TestInfo] -> IntMap Text
testNameIndex infos =
  IntMap.fromList [(tiId t, Text.intercalate " / " (tiPath t <> [tiName t])) | t <- infos]

{- | A one-line human rendering of a streaming event, or 'Nothing' for events
with nothing to say on a console.

@test_started@ is dropped rather than printed: it is immediately followed by
the @test_done@ line for the same test, and echoing both doubles the output
for no added information. @test_trace@ and @test_progress@ are likewise
suppressed — they are for tooling, and a trace payload is far too large for a
terminal line.
-}
renderEventWith :: (Int -> Maybe Text) -> Event -> Maybe Text
renderEventWith nameOf = \case
  EventSuiteStarted tests _ ->
    Just ("running " <> Text.pack (show (length tests)) <> " test(s)")
  EventTestStarted{} -> Nothing
  EventTestDone i ok dur desc mFailure _ ->
    Just $
      Text.concat
        [ if ok then "  PASS  " else "  FAIL  "
        , if Text.null desc then fromMaybe ("#" <> Text.pack (show i)) (nameOf i) else desc
        , " ("
        , formatSeconds dur
        , ")"
        , maybe "" failureSuffix mFailure
        ]
  EventSuiteDone passed failed dur _ ->
    Just $
      Text.concat
        [ if failed == 0 then "OK: " else "FAILED: "
        , Text.pack (show passed)
        , " passed, "
        , Text.pack (show failed)
        , " failed in "
        , formatSeconds dur
        ]
  EventOther tag _
    | tag `elem` ["test_progress", "test_trace"] -> Nothing
    | otherwise -> Just ("· " <> tag)
 where
  failureSuffix f =
    "\n          " <> failReason f <> ": " <> indentContinuation (failMessage f)

  -- Keep a multi-line failure message aligned under its test.
  indentContinuation = Text.intercalate "\n          " . Text.lines

{- | A banner naming the suite whose events follow.

@run --stream@ invokes cabal once per suite, so without this the events of six
suites would arrive as one undifferentiated list of PASS lines.
-}
renderSuiteBanner :: Text -> Text
renderSuiteBanner suite = "== " <> suite <> " =="

formatSeconds :: Double -> Text
formatSeconds s = Text.pack (showFixed3 s) <> "s"
 where
  showFixed3 x =
    let scaled = round (x * 1000) :: Integer
        (whole, frac) = scaled `divMod` 1000
     in show whole <> "." <> pad (show frac)
  pad str = replicate (3 - length str) '0' <> str
