{-# LANGUAGE OverloadedStrings #-}

{- | Static, no-compile discovery of a Haskell repository's cabal projects,
packages and test suites.

This is a native port of @scripts\/list-test-suites\/list-test-suites.sh@. It
emits the same JSON document, validating against that tool's
@list-test-suites.schema.json@, so anything already consuming the script's
output can consume @pbt-cli suites --json@ unchanged. The port exists because
the shipped @pbt-cli@ binary must work on its own: a downloaded binary has no
repository checkout to find a bash script in.

Nothing here compiles or even configures anything — it reads @*.cabal@ and
@cabal.project*@ as text, which is why it takes milliseconds.

The one deliberate divergence from the shell implementation: a trailing comma
on a @packages:@ entry is stripped, because @cabal@ itself accepts
comma-separated lists and the shell version would silently drop such a package.
-}
module PbtCli.Discover (
  -- * Types
  Discovery (..),
  Project (..),
  Package (..),
  TestSuite (..),
  Orphan (..),
  EntryPoint (..),
  entryPointText,

  -- * Discovery
  discover,

  -- * Compatibility
  isCompatible,
  compatibleOnly,

  -- * Flattened views
  SuiteRef (..),
  flattenSuites,
  findSuite,

  -- * Reusable pieces
  entryPointOfFile,
  classifySource,
  parseTestSuites,
  packageNameOf,
  projectFilesIn,
  findCabalFiles,
  readFileLenient,
) where

import Control.Exception (evaluate)
import Control.Monad (filterM)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, foldl', isInfixOf, isPrefixOf, isSuffixOf, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import PbtCli.Glob (expandGlob, isGlob)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute)
import System.FilePath (takeDirectory, takeExtension, (</>))
import System.IO (
  IOMode (ReadMode),
  hGetContents,
  hPutStrLn,
  hSetEncoding,
  mkTextEncoding,
  stderr,
  withFile,
 )

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

{- | How a test suite's @main-is@ module runs its tests.

Only 'Streaming' suites provide the @--streaming-json@ and @--list-tests-json@
ingredients, because those come from the @convex-tasty-streaming@ library. That
makes 'Streaming' the operational definition of "sc-testing-tools compatible".
-}
data EntryPoint
  = -- | @defaultMainStreaming@ and friends: structured discovery and streaming.
    Streaming
  | -- | plain @Test.Tasty.defaultMain@: runnable, but no structured output.
    Upstream
  | -- | a @main-is@ module with no recognised runner.
    UnknownEntry
  | -- | the @main-is@ source file was not found on disk.
    MissingEntry
  deriving (Eq, Ord, Show)

instance ToJSON EntryPoint where
  toJSON = toJSON . entryPointText

-- | The wire name, as it appears in the JSON and in @--tsv@ output.
entryPointText :: EntryPoint -> Text
entryPointText = \case
  Streaming -> "STREAMING"
  Upstream -> "upstream"
  UnknownEntry -> "unknown"
  MissingEntry -> "MISSING"

-- | A single @test-suite@ stanza, with the commands that drive it.
data TestSuite = TestSuite
  { tsName :: Text
  -- ^ the stanza name, which is also the @cabal test@ target.
  , tsMainIs :: Text
  -- ^ @main-is@ relative to the package dir, or the literal @"MISSING"@.
  , tsEntryPoint :: EntryPoint
  , tsRunTestsCommand :: Text
  -- ^ always present.
  , tsStreamTestsCommand :: Maybe Text
  -- ^ 'Streaming' suites only.
  , tsDiscoverCommand :: Maybe Text
  -- ^ 'Streaming' suites only.
  , tsHsSourceDirs :: [Text]
  -- ^ as written in the stanza, order preserved; @["."]@ when absent.
  }
  deriving (Eq, Show)

instance ToJSON TestSuite where
  toJSON ts =
    object
      [ "name" .= tsName ts
      , "mainIs" .= tsMainIs ts
      , "entryPoint" .= tsEntryPoint ts
      , "runTestsCommand" .= tsRunTestsCommand ts
      , "streamTestsCommand" .= tsStreamTestsCommand ts
      , "discoverCommand" .= tsDiscoverCommand ts
      , "hsSourceDirs" .= tsHsSourceDirs ts
      ]

-- | A package, rendered under exactly one project (see 'primaryOwner').
data Package = Package
  { pkgName :: Text
  , pkgCabalFile :: FilePath
  -- ^ relative to the scanned root.
  , pkgPackageDir :: FilePath
  -- ^ relative to the scanned root.
  , pkgTestSuites :: [TestSuite]
  -- ^ empty when the package declares none.
  }
  deriving (Eq, Show)

instance ToJSON Package where
  toJSON p =
    object
      [ "name" .= pkgName p
      , "cabalFile" .= pkgCabalFile p
      , "packageDir" .= pkgPackageDir p
      , "testSuites" .= pkgTestSuites p
      ]

-- | One @cabal.project*@ file, or the implicit project when there is none.
data Project = Project
  { projProjectFile :: Maybe FilePath
  {- ^ 'Nothing' is the synthetic project used when the repo has no
  @cabal.project@ at all.
  -}
  , projPackages :: [Package]
  }
  deriving (Eq, Show)

instance ToJSON Project where
  toJSON p =
    object
      [ "projectFile" .= projProjectFile p
      , "packages" .= projPackages p
      ]

-- | A @.cabal@ file no project's @packages:@ field reaches.
data Orphan = Orphan
  { orphCabalFile :: FilePath
  , orphPackageDir :: FilePath
  }
  deriving (Eq, Show)

instance ToJSON Orphan where
  toJSON o =
    object
      [ "cabalFile" .= orphCabalFile o
      , "packageDir" .= orphPackageDir o
      ]

-- | The whole discovery result.
data Discovery = Discovery
  { discRoot :: FilePath
  -- ^ absolute path of the scanned root.
  , discProjects :: [Project]
  , discOrphans :: [Orphan]
  }
  deriving (Eq, Show)

instance ToJSON Discovery where
  toJSON d =
    object
      [ "root" .= discRoot d
      , "projects" .= discProjects d
      , "orphans" .= discOrphans d
      ]

-- ---------------------------------------------------------------------------
-- Compatibility
-- ---------------------------------------------------------------------------

{- | Is this suite sc-testing-tools compatible — that is, does it support
@--list-tests-json@, @--streaming-json@ and the threat-model options?
-}
isCompatible :: TestSuite -> Bool
isCompatible ts = tsEntryPoint ts == Streaming

{- | Drop every non-'Streaming' suite, and then every package and project left
without suites. Used by @--compatible-only@.
-}
compatibleOnly :: Discovery -> Discovery
compatibleOnly d =
  d
    { discProjects =
        [ proj{projPackages = keptPackages}
        | proj <- discProjects d
        , let keptPackages =
                [ pkg{pkgTestSuites = kept}
                | pkg <- projPackages proj
                , let kept = filter isCompatible (pkgTestSuites pkg)
                , not (null kept)
                ]
        , not (null keptPackages)
        ]
    }

-- ---------------------------------------------------------------------------
-- Flattened view
-- ---------------------------------------------------------------------------

{- | A suite together with the context needed to actually run it: which project
file owns it and which package it lives in.

The JSON document is intentionally nested (and its schema forbids extra
fields), so this flattened view is what the @run@ / @stream@ / @tests@
commands work with.
-}
data SuiteRef = SuiteRef
  { srSuite :: TestSuite
  , srProjectFile :: Maybe FilePath
  -- ^ 'Nothing' means the default project: pass no @--project-file@.
  , srPackage :: Text
  , srPackageDir :: FilePath
  }
  deriving (Eq, Show)

{- | Every suite in the discovery, in document order, paired with its project
file and package.

@srProjectFile@ is 'Nothing' both for the implicit project and for the default
@cabal.project@, because in either case @cabal@ needs no @--project-file@ flag.
-}
flattenSuites :: Discovery -> [SuiteRef]
flattenSuites d =
  [ SuiteRef
      { srSuite = ts
      , srProjectFile = nonDefaultProject (projProjectFile proj)
      , srPackage = pkgName pkg
      , srPackageDir = pkgPackageDir pkg
      }
  | proj <- discProjects d
  , pkg <- projPackages proj
  , ts <- pkgTestSuites pkg
  ]

-- | Look a suite up by its @cabal test@ target name.
findSuite :: Text -> Discovery -> Maybe SuiteRef
findSuite name = listToMaybe . filter ((== name) . tsName . srSuite) . flattenSuites

nonDefaultProject :: Maybe FilePath -> Maybe FilePath
nonDefaultProject = \case
  Nothing -> Nothing
  Just "cabal.project" -> Nothing
  Just other -> Just other

-- ---------------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------------

{- | Scan @root@ and report every project, package and test suite.

Warnings (orphan @.cabal@ files, packages claimed by several non-default
projects) go to stderr, matching the shell tool, so stdout stays a single
parseable JSON document.
-}
discover :: FilePath -> IO Discovery
discover root0 = do
  root <- makeAbsolute root0
  cabalRels <- findCabalFiles root
  projectRels <- projectFilesIn root

  owners <-
    if null projectRels
      then -- No cabal.project anywhere: one synthetic project owns everything.
        pure (Map.fromList [(c, [implicitProject]) | c <- cabalRels])
      else
        foldl' (\acc m -> Map.unionWith laterOwnersLast m acc) Map.empty
          <$> mapM (ownersOfProject root) projectRels

  let projectOrder = if null projectRels then [implicitProject] else projectRels
      orphanRels = [c | c <- cabalRels, not (Map.member c owners)]

  mapM_ warnOrphan orphanRels

  primaries <- Map.fromList <$> mapM (resolvePrimary owners) (Map.keys owners)
  projects <- mapM (buildProject root cabalRels primaries) projectOrder

  pure
    Discovery
      { discRoot = root
      , discProjects = projects
      , discOrphans =
          [Orphan{orphCabalFile = c, orphPackageDir = relDir c} | c <- orphanRels]
      }
 where
  -- Keep owners in project-file order: the map is folded newest-first, so the
  -- accumulated list goes second and new entries are appended after it.
  laterOwnersLast new old = old ++ filter (`notElem` old) new

  warnOrphan c =
    hPutStrLn stderr ("warning: orphan .cabal not referenced by any project: " ++ c)

  resolvePrimary owners c =
    (c,) <$> primaryOwner c (fromMaybe [] (Map.lookup c owners))

-- | Sentinel for the synthetic project used when no @cabal.project@ exists.
implicitProject :: FilePath
implicitProject = "\0implicit"

isImplicit :: FilePath -> Bool
isImplicit = (== implicitProject)

{- | Pick the single project that renders a package.

The default project wins if it reaches the package at all; otherwise the
sorted-first non-default project does, with a warning when more than one claims
it. This has to agree with the @--project-file@ flag chosen in 'suiteCommands',
or a suite would be nested under one project and invoked through another.
-}
primaryOwner :: FilePath -> [FilePath] -> IO (Maybe FilePath)
primaryOwner cabalRel os
  | any isDefault os = pure (listToMaybe (filter isDefault os))
  | otherwise = case sort os of
      [] -> pure Nothing
      (chosen : rest) -> do
        if null rest
          then pure ()
          else
            hPutStrLn stderr $
              "warning: package "
                ++ cabalRel
                ++ " exclusively owned by multiple non-default project files ("
                ++ unwords (sort os)
                ++ "); picking '"
                ++ chosen
                ++ "'"
        pure (Just chosen)
 where
  isDefault p = p == "cabal.project" || isImplicit p

buildProject
  :: FilePath
  -- ^ absolute root
  -> [FilePath]
  -- ^ all .cabal files, relative, in discovery order
  -> Map FilePath (Maybe FilePath)
  -- ^ .cabal -> primary owner
  -> FilePath
  -- ^ this project file (or the implicit sentinel)
  -> IO Project
buildProject root cabalRels primaries projRel = do
  pkgs <- mapM (buildPackage root flag) mine
  pure
    Project
      { projProjectFile = if isImplicit projRel then Nothing else Just projRel
      , projPackages = pkgs
      }
 where
  mine = [c | c <- cabalRels, Map.lookup c primaries == Just (Just projRel)]

  -- The flag comes from the same primary owner as the nesting above.
  flag
    | isImplicit projRel || projRel == "cabal.project" = Nothing
    | otherwise = Just projRel

buildPackage :: FilePath -> Maybe FilePath -> FilePath -> IO Package
buildPackage root flag cabalRel = do
  contents <- readFileLenient (root </> cabalRel)
  let pkgDir = relDir cabalRel
  stanzas <- parseTestSuites root pkgDir contents
  pure
    Package
      { pkgName = packageNameOf contents
      , pkgCabalFile = cabalRel
      , pkgPackageDir = pkgDir
      , pkgTestSuites = map (suiteCommands flag) stanzas
      }

-- | Attach the three commands to a parsed stanza.
suiteCommands :: Maybe FilePath -> TestSuite -> TestSuite
suiteCommands flag ts =
  ts
    { tsRunTestsCommand = base
    , tsStreamTestsCommand = streamingOnly (base <> " --test-options=--streaming-json")
    , tsDiscoverCommand = streamingOnly (base <> " --test-options=--list-tests-json")
    }
 where
  base =
    "cabal test "
      <> tsName ts
      <> maybe "" (\f -> " --project-file=" <> Text.pack f) flag

  streamingOnly cmd
    | tsEntryPoint ts == Streaming = Just cmd
    | otherwise = Nothing

-- ---------------------------------------------------------------------------
-- .cabal parsing
-- ---------------------------------------------------------------------------

-- | The package's @name:@ field, or @""@ when it has none.
packageNameOf :: String -> Text
packageNameOf contents =
  fromMaybe "" $
    listToMaybe
      [ Text.strip (Text.pack (drop 5 line))
      | line <- lines contents
      , map toLower (take 5 line) == "name:"
      ]

{- | Parse the @test-suite@ stanzas of one @.cabal@ file.

A stanza runs from a @test-suite \<name\>@ line at column 0 until the next
column-0 declaration. @main-is@ is resolved against each of the stanza's
@hs-source-dirs@ in order; the first path that exists on disk wins and its
source is classified. When none exists the suite is reported as @MISSING@,
which keeps a typo'd (or generated-but-absent) entry point visible instead of
silently dropping the suite.
-}
parseTestSuites
  :: FilePath
  -- ^ absolute root
  -> FilePath
  -- ^ package dir, relative to root
  -> String
  -- ^ the @.cabal@ contents
  -> IO [TestSuite]
parseTestSuites root pkgDir contents =
  mapM finish (collect Nothing (map stripCR (lines contents)))
 where
  stripCR l = if not (null l) && last l == '\r' then init l else l

  -- Gather (name, main-is, hs-source-dirs) triples in file order.
  collect :: Maybe (Text, Maybe String, [String]) -> [String] -> [(Text, Maybe String, [String])]
  collect acc [] = flush acc
  collect acc (l : ls)
    | isColumnZero l = case testSuiteName l of
        Just nm -> flush acc ++ collect (Just (nm, Nothing, ["."])) ls
        Nothing -> flush acc ++ collect Nothing ls
    | otherwise = case acc of
        Nothing -> collect Nothing ls
        Just (nm, mainIs, dirs) -> case indentedField l of
          Just ("main-is", v) -> collect (Just (nm, Just v, dirs)) ls
          Just ("hs-source-dirs", v) -> collect (Just (nm, mainIs, splitDirsField v)) ls
          _ -> collect acc ls

  flush = maybe [] (: [])

  isColumnZero = \case
    (c : _) -> not (isSpace c)
    [] -> False

  testSuiteName l = case words l of
    (kw : nm : _) | map toLower kw == "test-suite" -> Just (Text.pack nm)
    _ -> Nothing

  indentedField l =
    let (leading, rest) = span isSpace l
     in if null leading
          then Nothing
          else case break (== ':') rest of
            (nameField, ':' : v) -> Just (map toLower (trim nameField), trim v)
            _ -> Nothing

  finish (nm, mainIs, dirs) = case mainIs of
    Nothing -> pure (mkSuite nm "MISSING" MissingEntry dirs)
    Just m -> do
      -- Report main-is relative to the package dir, which is the source dir
      -- that matched joined with the stanza's own main-is value.
      resolved <- firstExisting [(d </> m, root </> pkgDir </> d </> m) | d <- dirs, not (null d)]
      case resolved of
        Nothing -> pure (mkSuite nm "MISSING" MissingEntry dirs)
        Just (rel, abs') -> do
          ep <- entryPointOfFile abs'
          pure (mkSuite nm (Text.pack rel) ep dirs)

  -- Commands are filled in later by 'suiteCommands', which is the only place
  -- that knows the owning project file.
  mkSuite nm mis ep dirs =
    TestSuite
      { tsName = nm
      , tsMainIs = mis
      , tsEntryPoint = ep
      , tsRunTestsCommand = ""
      , tsStreamTestsCommand = Nothing
      , tsDiscoverCommand = Nothing
      , tsHsSourceDirs = map Text.pack dirs
      }

-- | @hs-source-dirs@ is whitespace- and/or comma-separated.
splitDirsField :: String -> [String]
splitDirsField s = case filter (not . null) (splitOnAny " \t," s) of
  [] -> ["."]
  ds -> ds

{- | Classify a @main-is@ source file by the tasty runner it calls.

'Streaming' is tested first so the @*WithIngredients@ variants are never
mistaken for plain upstream @defaultMain@.
-}
entryPointOfFile :: FilePath -> IO EntryPoint
entryPointOfFile fp = do
  exists <- doesFileExist fp
  if not exists
    then pure MissingEntry
    else classifySource <$> readFileLenient fp

-- | The pure half of 'entryPointOfFile'.
classifySource :: String -> EntryPoint
classifySource src
  | any (`isInfixOf` src) streamingMarkers = Streaming
  | any (`isInfixOf` src) upstreamMarkers = Upstream
  | otherwise = UnknownEntry
 where
  streamingMarkers =
    [ "defaultMainStreamingWithIngredients"
    , "defaultMainStreaming"
    , "defaultMainTestingInterface"
    , "Convex.Tasty.Streaming"
    , "Convex.TestingInterface"
    ]
  upstreamMarkers =
    [ "defaultMainWithIngredients"
    , "defaultMain"
    ]

-- ---------------------------------------------------------------------------
-- cabal.project parsing
-- ---------------------------------------------------------------------------

{- | The @cabal.project@ / @cabal.project.*@ files at the top level of @root@,
sorted.

Only the top level is scanned because that is the only place @cabal@ honours
them, and @.freeze@ / @.local@ are excluded: they configure a project rather
than declaring one.
-}
projectFilesIn :: FilePath -> IO [FilePath]
projectFilesIn root = do
  entries <- listDirectory root
  files <- filterM (\e -> doesFileExist (root </> e)) entries
  pure (sort (filter isProjectFile files))
 where
  isProjectFile e =
    (e == "cabal.project" || "cabal.project." `isPrefixOf` e)
      && not (".freeze" `isSuffixOf` e)
      && not (".local" `isSuffixOf` e)

-- | Which @.cabal@ files (relative to root) this project file claims.
ownersOfProject :: FilePath -> FilePath -> IO (Map FilePath [FilePath])
ownersOfProject root projRel = do
  cabals <- collectProjectPackages root Set.empty (root </> projRel)
  pure (Map.fromList [(c, [projRel]) | c <- Set.toList cabals])

{- | Recursively collect the @.cabal@ paths a project file's @packages:@ reach,
following @import:@ with a visited set so a diamond of imports terminates.
-}
collectProjectPackages
  :: FilePath
  -- ^ absolute root, for relativising results
  -> Set FilePath
  -- ^ already-visited project files
  -> FilePath
  -- ^ absolute project file to read
  -> IO (Set FilePath)
collectProjectPackages root visited pfile
  | Set.member pfile visited = pure Set.empty
  | otherwise = do
      exists <- doesFileExist pfile
      if not exists
        then pure Set.empty
        else do
          contents <- readFileLenient pfile
          go
            (Set.insert pfile visited)
            (takeDirectory pfile)
            Set.empty
            NotInPackages
            (map stripComment (lines contents))
 where
  go _ _ acc _ [] = pure acc
  go vis pdir acc st (l : ls)
    -- A source-repository-package stanza declares dependencies, not local
    -- packages, so its indented body is skipped entirely. A dedented line
    -- ends the stanza and is then processed normally.
    | st == InSourceRepo && indented l = go vis pdir acc InSourceRepo ls
    | "source-repository-package" `isPrefixOf` l = go vis pdir acc InSourceRepo ls
    | Just imp <- nonEmptyField "import" l = do
        let target = pdir </> imp
        inner <- collectProjectPackages root vis target
        -- Thread the visited set forward so sibling imports of a shared file
        -- are not read twice.
        go (Set.insert target vis) pdir (Set.union acc inner) NotInPackages ls
    | Just rest <- fieldValue "packages" l = do
        found <- resolveTokens pdir (tokens rest)
        go vis pdir (Set.union acc found) InPackages ls
    | not (indented l) = go vis pdir acc NotInPackages ls
    | st == InPackages = do
        found <- resolveTokens pdir (tokens l)
        go vis pdir (Set.union acc found) InPackages ls
    | otherwise = go vis pdir acc st ls

  resolveTokens pdir ts =
    Set.fromList . concat <$> mapM (resolvePackageEntry root pdir) ts

  -- cabal accepts comma-separated package lists; drop the separator so such
  -- an entry still resolves.
  tokens = filter (not . null) . map (dropWhileEnd (== ',')) . words

  indented = \case
    (c : _) -> isSpace c
    [] -> True

data PkgState = NotInPackages | InPackages | InSourceRepo
  deriving (Eq)

{- | Resolve one @packages:@ token to concrete @.cabal@ paths, relative to root.

A token is an explicit @.cabal@ file, a glob, or a bare directory — in which
case the @.cabal@ files directly inside it are taken, non-recursively, exactly
as @cabal@ does.
-}
resolvePackageEntry :: FilePath -> FilePath -> String -> IO [FilePath]
resolvePackageEntry root pdir entry = do
  direct <-
    if isGlob entry || takeExtension entry == ".cabal"
      then filterM doesFileExist =<< expandGlob pdir entry
      else pure []
  matches <-
    if not (null direct)
      then pure direct
      else do
        let target = pdir </> entry
        isDir <- doesDirectoryExist target
        if not isDir
          then pure []
          else do
            entries <- listDirectory target
            filterM doesFileExist [target </> e | e <- sort entries, takeExtension e == ".cabal"]
  pure (map (dropPrefixPath (root ++ "/") . normalisePath) matches)

-- ---------------------------------------------------------------------------
-- Filesystem walk
-- ---------------------------------------------------------------------------

{- | Every @.cabal@ file under @root@, relative to it and sorted.

@dist-newstyle@, @tasty-investigate@, @.git@ and @node_modules@ are pruned:
they hold build artefacts and vendored code whose @.cabal@ files are not part
of the repository's own structure.
-}
findCabalFiles :: FilePath -> IO [FilePath]
findCabalFiles root = sort <$> walk ""
 where
  walk rel = do
    let dir = if null rel then root else root </> rel
    entries <- listDirectory dir
    concat <$> mapM (visit rel) (sort entries)

  visit rel e
    | e `elem` prunedDirs = pure []
    | otherwise = do
        let childRel = if null rel then e else rel </> e
        isDir <- doesDirectoryExist (root </> childRel)
        if isDir
          then walk childRel
          else pure [childRel | takeExtension e == ".cabal"]

  prunedDirs = ["dist-newstyle", "tasty-investigate", ".git", "node_modules"]

-- ---------------------------------------------------------------------------
-- Small helpers
-- ---------------------------------------------------------------------------

{- | Read a file without letting a stray non-UTF-8 byte abort discovery.

Vendored @.cabal@ files and generated sources sometimes carry latin-1 bytes.
The classifier only looks for ASCII markers, so transliterating one byte is
harmless, whereas a decoding exception would lose the entire scan.
-}
readFileLenient :: FilePath -> IO String
readFileLenient fp = do
  exists <- doesFileExist fp
  if not exists
    then pure ""
    else withFile fp ReadMode $ \h -> do
      enc <- mkTextEncoding "UTF-8//TRANSLIT"
      hSetEncoding h enc
      s <- hGetContents h
      -- Force before the handle closes: hGetContents is lazy.
      _ <- evaluate (length s)
      pure s

-- | Strip a cabal @--@ line comment.
stripComment :: String -> String
stripComment = \case
  [] -> []
  ('-' : '-' : _) -> []
  (c : cs) -> c : stripComment cs

{- | @field: value@ at any indentation, case-insensitive on the field name.

Returns 'Nothing' when the line is not that field, so callers can pattern-match
their way through a project file one field at a time.
-}
fieldValue :: String -> String -> Maybe String
fieldValue name l =
  case break (== ':') (dropWhile isSpace l) of
    (n, ':' : v) | map toLower (trim n) == name -> Just (trim v)
    _ -> Nothing

-- | 'fieldValue', but an empty value does not count as the field being present.
nonEmptyField :: String -> String -> Maybe String
nonEmptyField name l = case fieldValue name l of
  Just v | not (null v) -> Just v
  _ -> Nothing

{- | Collapse @\/.\/@ and duplicate slashes and strip leading @.\/@, so paths
built from a bare @.@ or a trailing-slash directory token compare equal to the
ones the filesystem walk produced.
-}
normalisePath :: FilePath -> FilePath
normalisePath = stripLeadingDots . collapse
 where
  collapse ('/' : '/' : rest) = collapse ('/' : rest)
  collapse ('/' : '.' : '/' : rest) = collapse ('/' : rest)
  collapse "/." = ""
  collapse (c : cs) = c : collapse cs
  collapse [] = []

  stripLeadingDots ('.' : '/' : rest) = stripLeadingDots rest
  stripLeadingDots p = p

-- | Drop @prefix@ from @path@ when present; otherwise leave it alone.
dropPrefixPath :: String -> FilePath -> FilePath
dropPrefixPath prefix path
  | prefix `isPrefixOf` path = drop (length prefix) path
  | otherwise = path

-- | The directory part of a relative path, as @.@ rather than @""@ at the top.
relDir :: FilePath -> FilePath
relDir p = case takeDirectory p of
  "" -> "."
  d -> d

-- | The first pair whose second component exists on disk.
firstExisting :: [(a, FilePath)] -> IO (Maybe (a, FilePath))
firstExisting [] = pure Nothing
firstExisting (p@(_, fp) : ps) = do
  ok <- doesFileExist fp
  if ok then pure (Just p) else firstExisting ps

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

splitOnAny :: [Char] -> String -> [String]
splitOnAny seps = foldr step [[]]
 where
  step c acc@(cur : rest)
    | c `elem` seps = [] : acc
    | otherwise = (c : cur) : rest
  step _ [] = [[]]
