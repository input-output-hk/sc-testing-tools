{-# LANGUAGE OverloadedStrings #-}

-- | The @pbt-cli@ command line.
module PbtCli.Options (
  -- * Commands
  Command (..),
  SuitesOpts (..),
  TestsOpts (..),
  RunOpts (..),
  ThreatModelsOpts (..),
  DoctorOpts (..),

  -- * Shared
  SuitesFormat (..),
  TestsFormat (..),
  RunOutput (..),
  Filters (..),

  -- * Parsing
  parseCommand,
  commandParserInfo,
) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Options.Applicative
import PbtCli.Version (versionLine)

-- | What @pbt-cli@ was asked to do.
data Command
  = Suites SuitesOpts
  | Tests TestsOpts
  | Run RunOpts
  | ThreatModels ThreatModelsOpts
  | Doctor DoctorOpts
  deriving (Eq, Show)

{- | How to print a discovery result.

'SuitesList' is the default: a bare list of names is what a person scanning
the output, or a shell pipeline consuming it, actually wants from a listing
command. Every richer view is one flag away.
-}
data SuitesFormat
  = -- | one suite name per line (the default).
    SuitesList
  | -- | the full document as indented JSON, in the documented field order.
    SuitesJson
  | -- | the full document as single-line JSON, for piping.
    SuitesJsonCompact
  | -- | aligned table with package, compatibility and @main-is@.
    SuitesTable
  | -- | the legacy 5-column tab-separated format.
    SuitesTsv
  deriving (Eq, Show)

-- | How to print a suite's test tree.
data TestsFormat
  = -- | @id\<TAB\>path@, one test per line (the default).
    TestsList
  | -- | the suite's own @suite_started@ payload, verbatim.
    TestsJson
  | -- | nested groups with ids.
    TestsTree
  deriving (Eq, Show)

{- | What @run@ puts on stdout.

The default hands the child's stdio straight through, so a plain
@pbt-cli run@ looks exactly like the @cabal test@ it wraps. The other two
modes ask the suites for @--streaming-json@ and then either render the events
or forward them.
-}
data RunOutput
  = -- | cabal's and Tasty's own console output (the default).
    RunConsole
  | -- | a live, human-readable rendering of the streaming events.
    RunStream
  | -- | the streaming events as NDJSON, each tagged with its suite.
    RunJson
  deriving (Eq, Show)

{- | Test-selection options common to the commands that actually run a suite.

These are forwarded to the test binary rather than interpreted here, so a
suite built against a newer @convex-tasty-streaming@ than this binary still
gets them intact.
-}
data Filters = Filters
  { fltPattern :: Maybe String
  -- ^ Tasty's @-p@.
  , fltTestIds :: Maybe String
  -- ^ @--test-id@, comma-separated.
  , fltThreatModelName :: Maybe String
  -- ^ @--threat-model-name@, comma-separated prefixes.
  , fltTestOption :: [String]
  -- ^ escape hatch: extra raw test options.
  }
  deriving (Eq, Show)

data SuitesOpts = SuitesOpts
  { suoRoot :: FilePath
  , suoFormat :: SuitesFormat
  , suoCompatibleOnly :: Bool
  }
  deriving (Eq, Show)

data TestsOpts = TestsOpts
  { tsoRoot :: FilePath
  , tsoSuite :: Text
  , tsoFormat :: TestsFormat
  , tsoFilters :: Filters
  , tsoDryRun :: Bool
  }
  deriving (Eq, Show)

data RunOpts = RunOpts
  { rnoRoot :: FilePath
  , rnoSuites :: [Text]
  -- ^ empty means every discovered suite.
  , rnoCompatibleOnly :: Bool
  , rnoOutput :: RunOutput
  , rnoFilters :: Filters
  , rnoDryRun :: Bool
  }
  deriving (Eq, Show)

data ThreatModelsOpts = ThreatModelsOpts
  { tmoRoot :: FilePath
  , tmoSuite :: Text
  , tmoJson :: Bool
  , tmoDryRun :: Bool
  }
  deriving (Eq, Show)

newtype DoctorOpts = DoctorOpts
  { dcoRoot :: FilePath
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Parsers
-- ---------------------------------------------------------------------------

-- | The full parser, with @--help@ and @--version@ attached.
commandParserInfo :: ParserInfo Command
commandParserInfo =
  info
    (parseCommand <**> helper <**> versionOption)
    ( fullDesc
        <> progDesc
          "Discover and run the test suites of a repository built on the \
          \sc-testing-tools property-based testing stack."
        <> header "pbt-cli — a cabal test wrapper for sc-testing-tools suites"
        <> footer
          "A suite is 'sc-testing-tools compatible' when its main-is uses \
          \defaultMainStreaming / defaultMainTestingInterface, which is what \
          \provides --list-tests-json, --streaming-json and the threat-model \
          \options. Run 'pbt-cli <command> --help' for per-command options."
    )

versionOption :: Parser (a -> a)
versionOption =
  infoOption
    versionLine
    (long "version" <> short 'V' <> help "Print the version and target platform, then exit")

parseCommand :: Parser Command
parseCommand =
  hsubparser
    ( command
        "suites"
        ( info
            (Suites <$> suitesOpts)
            ( progDesc
                "Discover test suites without compiling anything, and report which are \
                \sc-testing-tools compatible"
            )
        )
        <> command
          "tests"
          ( info
              (Tests <$> testsOpts)
              (progDesc "List the tests inside one suite (authoritative; builds the suite)")
          )
        <> command
          "run"
          ( info
              (Run <$> runOpts)
              ( progDesc
                  "Run every suite, or just the named ones; --stream renders the \
                  \streaming events live and --json forwards them as NDJSON"
              )
          )
        <> command
          "threat-models"
          ( info
              (ThreatModels <$> threatModelsOpts)
              (progDesc "List the threat models a suite can run")
          )
        <> command
          "doctor"
          ( info
              (Doctor <$> doctorOpts)
              (progDesc "Check that everything pbt-cli needs is present and the repo is discoverable")
          )
    )

rootOption :: Parser FilePath
rootOption =
  strOption
    ( long "root"
        <> short 'C'
        <> metavar "DIR"
        <> value "."
        <> showDefault
        <> help "Repository root to scan and run cabal in"
    )

{- | @--root@, plus an optional positional @ROOT@ for the commands that take no
other positional argument.

The positional form is what @list-test-suites.sh@ accepted, so
@pbt-cli suites .@ and @pbt-cli suites /path/to/repo@ keep working for anyone
migrating off the shell tool. When both are given the positional wins, since it
is the more specific thing the user typed.
-}
rootWithPositional :: Parser FilePath
rootWithPositional =
  fromMaybe
    <$> rootOption
    <*> optional
      ( strArgument
          ( metavar "[ROOT]"
              <> help "Repository root to scan (positional alternative to --root)"
          )
      )

suiteArgument :: Parser Text
suiteArgument =
  strArgument
    ( metavar "SUITE"
        <> help "Test-suite name, as reported by 'pbt-cli suites'"
    )

dryRunSwitch :: Parser Bool
dryRunSwitch =
  switch
    ( long "dry-run"
        <> help "Print the cabal command(s) that would run, and exit"
    )

compatibleOnlySwitch :: Parser Bool
compatibleOnlySwitch =
  switch
    ( long "compatible-only"
        <> help "Consider only sc-testing-tools compatible (streaming) suites"
    )

filtersParser :: Parser Filters
filtersParser =
  Filters
    <$> optional
      ( strOption
          ( long "pattern"
              <> short 'p'
              <> metavar "PAT"
              <> help "Tasty pattern: run only tests whose name or path matches"
          )
      )
    <*> optional
      ( strOption
          ( long "test-id"
              <> metavar "IDS"
              <> help "Comma-separated test ids to run (discover them with 'pbt-cli tests')"
          )
      )
    <*> optional
      ( strOption
          ( long "threat-model-name"
              <> metavar "NAMES"
              <> help "Comma-separated threat-model name prefixes to run"
          )
      )
    <*> many
      ( strOption
          ( long "test-option"
              <> metavar "OPT"
              <> help "Extra option to pass through to the test binary verbatim (repeatable)"
          )
      )

suitesOpts :: Parser SuitesOpts
suitesOpts =
  SuitesOpts
    <$> rootWithPositional
    <*> suitesFormat
    <*> compatibleOnlySwitch

suitesFormat :: Parser SuitesFormat
suitesFormat =
  flag' SuitesJson (long "json" <> help "The full discovery document as indented JSON")
    <|> flag' SuitesJsonCompact (long "compact" <> help "The discovery document as single-line JSON")
    <|> flag' SuitesTable (long "table" <> help "Aligned table with package, compatibility and main-is")
    <|> flag' SuitesTsv (long "tsv" <> help "Legacy 5-column tab-separated output")
    <|> pure SuitesList

testsOpts :: Parser TestsOpts
testsOpts =
  TestsOpts
    <$> rootOption
    <*> suiteArgument
    <*> testsFormat
    <*> filtersParser
    <*> dryRunSwitch

testsFormat :: Parser TestsFormat
testsFormat =
  flag' TestsJson (long "json" <> help "The suite's own suite_started payload, verbatim")
    <|> flag' TestsTree (long "tree" <> help "Nested groups with test ids")
    <|> pure TestsList

runOpts :: Parser RunOpts
runOpts =
  RunOpts
    <$> rootOption
    <*> many
      ( strArgument
          ( metavar "SUITE..."
              <> help "Suites to run; omit to run every discovered suite"
          )
      )
    <*> compatibleOnlySwitch
    <*> runOutput
    <*> filtersParser
    <*> dryRunSwitch

runOutput :: Parser RunOutput
runOutput =
  flag'
    RunJson
    ( long "json"
        <> help "Emit the streaming events as NDJSON, each tagged with its suite"
    )
    <|> flag'
      RunStream
      ( long "stream"
          <> help "Render the streaming events live, instead of cabal's console output"
      )
    <|> pure RunConsole

threatModelsOpts :: Parser ThreatModelsOpts
threatModelsOpts =
  ThreatModelsOpts
    <$> rootOption
    <*> suiteArgument
    <*> switch (long "json" <> help "Emit the suite's JSON payload instead of one name per line")
    <*> dryRunSwitch

doctorOpts :: Parser DoctorOpts
doctorOpts = DoctorOpts <$> rootWithPositional
