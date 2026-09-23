{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Convex.TestingInterface (
  -- * Testing interface
  TestingInterface (..),
  ModelState,
  ThreatModelsFor (..),

  -- * Redeemer tagging (Tier 2)
  RedeemerTagger (..),
  RedeemerTag (..),
  autoRedeemerTag,
  labelRedeemer,

  -- * Address labeling
  AddressLabeler (..),
  mockWalletAddressLabeler,

  -- * Running Tests
  propRunActions,
  propRunActionsWithOptions,
  RunOptions (..),
  defaultRunOptions,
  defaultMainTestingInterface,
  genAction,
  runActions,

  -- * Trace recording
  TraceRecorder (..),

  -- * Threat model coverage policy
  zeroCoverageVerdict,
  ThreatModelCategory (..),
  ZeroCoverageKind (..),
  zeroCoverageKind,
  skippedMessage,

  -- * The Testing Monad
  TestingMonadT (..),
  runTestingMonadT,
  mockchainSucceedsWithOptions,
  mockchainFailsWithOptions,
  Options (..),
  defaultOptions,
  modifyTransactionLimits,

  -- * Coverage helpers

  -- ** Coverage with tasty-streaming
  withCoverageIndices,
  covDataToSrcLocRanges,

  -- ** Coverage without tasty-streaming
  withCoverage,
  CoverageConfig (..),
  printCoverageReport,
  writeCoverageReport,
  silentCoverageReport,
  printCoverageJSON,
  writeCoverageJSON,
  printCoverageJSONPretty,
  writeCoverageJSONPretty,
  CoverageSummary (..),
  coverageSummary,

  -- * Re-exports from QuickCheck
  Gen,
  Arbitrary (..),
  frequency,
  oneof,
  elements,

  -- * Re-exports from Tasty
  TestTree,
) where

import Control.Monad (forM, unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Convex.Tasty.QuickCheck (testProperty)
import Test.HUnit (Assertion)
import Test.QuickCheck (Arbitrary (..), Gen, Property, counterexample, discard, elements, frequency, oneof, property)
import Test.QuickCheck.Monadic (PropertyM, monadicIO, monitor, pick, run)
import Test.Tasty (DependencyType (..), TestTree, askOption, localOption, sequentialTestGroup, testGroup, withResource)
import Test.Tasty.ExpectedFailure (ignoreTestBecause)
import Test.Tasty.HUnit (assertFailure, testCaseSteps)

import Cardano.Api qualified as C
import Cardano.Ledger.Core qualified as L
import Control.Exception (SomeException, catch, evaluate, throwIO, try)
import Control.Lens ((&), (.~), (^.))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State.Class (MonadState, get)
import Control.Monad.Trans (MonadTrans (..))
import Convex.Class (MonadBlockchain, MonadMockchain, MonadUtxoQuery, coverageData, getMockChainState, getTxs, getUtxo)
import Convex.CoinSelection (BalanceTxError (..), BalancingError (..), coverageFromBalanceTxError)
import Convex.CoinSelection.Class (BalancingT (..), MonadBalance)
import Convex.MockChain (MockChainState (..), MockchainT (..), fromLedgerUTxO, initialStateFor, runMockchainIO, runMockchainT)
import Convex.MockChain.Defaults qualified as Defaults
import Convex.MonadLog (MonadLog)
import Convex.NodeParams (NodeParams (..))
import Convex.Tasty.Streaming.SrcLoc (SrcLocRange (..), withSrcLoc)
import Convex.Tasty.Streaming.TMSummary (CoverageIndexStorage (..), Fault (..), TMRecorder, ThreatModelCategory (..), ThreatModelSummary (..), TraceRecorder (..), faultLabel, threatModelGroupName, tmRecord)
import Convex.TestingInterface.Options (defaultMainTestingInterface)
import Convex.TestingInterface.Trace (
  AddressLabeler (..),
  IterationStatus (..),
  IterationTrace (..),
  RedeemerTag (..),
  RedeemerTagger (..),
  ThreatModelTrace (..),
  ThreatModelTraceOutcome (..),
  ThreatModelValidation (..),
  Transition (..),
  TransitionResult (..),
  TxSummary (..),
 )
import Convex.TestingInterface.Trace.RedeemerTag (autoRedeemerTag, labelRedeemer)
import Convex.TestingInterface.Trace.TxSummary (summarizeTx)
import Convex.ThreatModel (SigningWallet (AutoSign), ThreatModel (..), ThreatModelCheckEntry (..), ThreatModelOutcome (..), TxValidity (..), ValidityReport (..), getThreatModelName, runThreatModelCheckTraced, threatModelEnvs)
import Convex.ThreatModel.All (allThreatModels)
import Convex.ThreatModel.TxModifier (TxModifier (..), renderTxMod)
import Convex.Wallet (verificationKeyHash)
import Convex.Wallet.MockWallet qualified as Wallet
import Data.Aeson (ToJSON (..), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (foldl', for_, traverse_)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.List (deleteFirstsBy, isPrefixOf)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Word (Word32)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import PlutusTx.Coverage (
  CovLoc (..),
  CoverageAnnotation (..),
  CoverageData (..),
  CoverageIndex,
  CoverageReport (..),
  Metadata (..),
  coverageAnnotations,
  coverageMetadata,
  coveredAnnotations,
  ignoredAnnotations,
  _metadataSet,
 )
import Prettyprinter qualified as Pretty
import System.Exit (ExitCode)

{- | A testing interface defines the state and behavior of one or more smart contracts.

The type parameter @state@ represents the model's view of the world. It should
track all relevant information needed to validate that the contract is behaving
correctly.

Minimal complete definition: 'Action', 'initialize', 'arbitraryAction', 'perform'
-}
class (Show state, Eq state, Show (Action state), ToJSON state) => TestingInterface state where
  {- | Actions that can be performed on the contract.
  This is typically a data type with one constructor per contract operation.
  -}
  data Action state

  {- | The initial state of the model, before any actions are performed.
  Any transactions submitted during initialization will not be subjected to tests.
  If you want to test the initialization transactions, you can add an initialization action,
  and keep the 'initialize' method minimal.
  -}
  initialize :: (MonadIO m) => TestingMonadT m state

  {- | Generate a random action given the current state.
  The generated action should be appropriate for the current state.
  -}
  arbitraryAction :: state -> Gen (Action state)

  {- | Precondition that must hold before an action can be executed.
  Return 'False' to indicate that an action is not valid in the current state.
  Default: all actions are always valid.
  -}
  precondition :: state -> Action state -> Bool
  precondition _ _ = True

  {- | Perform the action on the real blockchain (mockchain).
  This should execute the actual transaction(s) that implement the action.
  The current model state is provided to allow access to tracked blockchain state.
  The returned state should reflect the expected effect of the action on the contract state.
  -}
  perform :: (MonadIO m) => state -> Action state -> TestingMonadT m state

  {- | Validate that the blockchain state matches the model state.
  Default: no validation (always succeeds).
  -}
  validate :: (MonadIO m) => state -> TestingMonadT m Bool
  validate _ = pure True

  {- | Called after each successful action to wrap the enclosing QuickCheck property.
  This hook runs only after 'perform' and 'validate' succeed. Use it for
  property-level checks, labels, and counterexamples that should be attached to
  valid state transitions.
  Default: no additional checks.
  -}
  monitoring :: state -> Action state -> Property -> Property
  monitoring _ _ = id

  {- | Whether to discard (skip) test cases where the invalid action fails due to
  a user-level error (e.g., off-chain balancing failure) rather than an
  on-chain validator rejection during negative testing.

  When 'True', negative tests that throw user exceptions are discarded
  (via QuickCheck's 'discard'), so only on-chain rejections count as
  successful negative tests.

  When 'False' (the default), user exceptions also cause the test case
  to be discarded — meaning both off-chain and on-chain failures are
  treated the same way.

  Override this in your 'TestingInterface' instance if you need finer
  control over which failure modes are accepted in negative testing.
  -}
  discardNegativeTestForUserExceptions :: Bool
  discardNegativeTestForUserExceptions = False

  {- | Optional 'RedeemerTagger' (Tier 2) that maps a script input's parsed
  Plutus redeemer 'Data' to a human-readable 'RedeemerTag' (label +
  optional JSON payload). The label surfaces in the streamed
  'TxInputSummary' as @redeemerKind@ / @redeemerPayload@.

  Default: a no-op tagger, so Tier 1 ('redeemerRaw' / 'redeemerConstr')
  is still streamed and Tier 2 fields stay @Nothing@. See
  'autoRedeemerTag' and 'labelRedeemer' for ergonomics.
  -}
  redeemerTagger :: RedeemerTagger
  redeemerTagger = RedeemerTagger (const Nothing)

  {- | Optional 'AddressLabeler' that maps an address's credential hash to a
  human-readable label. The label surfaces in the streamed
  'TxInputSummary' \/ 'TxOutputSummary' as @addressLabel@.

  Default: 'mockWalletAddressLabeler', which labels the standard mock
  wallets ('Convex.Wallet.MockWallet.mockWallets') as @"Wallet 1".."Wallet
  10"@. Since those are the wallets nearly every test already uses, this
  default costs nothing and rarely needs overriding — extend it with
  '(<>)' to add labels for a model's own script hashes or extra keys.
  -}
  addressLabeler :: AddressLabeler
  addressLabeler = mockWalletAddressLabeler

class (TestingInterface state) => ThreatModelsFor state where
  {- | Threat models the contract claims to resist. This is a coverage
  claim, checked in both directions: a listed model that never applies to
  any generated transaction fails the suite (the claim is unverifiable), and
  one that finds a vulnerability fails it too (the claim is false).

  Default: nothing claimed. Listing a model here is the outcome of triage —
  you ran it, saw that it applies, and saw the contract hold. Models you
  have not triaged belong in 'candidateModels', which is where they start.
  -}
  threatModels :: [ThreatModel ()]
  threatModels = []

  {- | Threat models run for information rather than as a claim: reported,
  but never failed for not applying. This is where every model starts, and
  what an empty instance runs.

  Default: every parameterless threat model not already spoken for by
  another slot. A detection still fails — a finding is a finding, whether or
  not you asked for the model — but the message asks you to triage it into
  'threatModels', 'expectedVulnerabilities', 'acceptedFindings' or
  'notApplicable' rather than accusing the contract of a broken promise.

  Set to @[]@ to opt out of the survey entirely.
  -}
  candidateModels :: [ThreatModel ()]
  candidateModels =
    defaultThreatModelsExcluding
      ( threatModels @state
          <> map fst (expectedVulnerabilities @state)
          <> map fst (acceptedFindings @state)
          <> map fst (notApplicable @state)
      )

  {- | Vulnerabilities the contract is known to have, each with the reason
  it is declared. Inverted pass/fail: detecting one is the required outcome,
  and failing to detect it means the contract (or the attack) improved and
  this declaration is now stale.

  The reason travels into the failure message and the streamed summary, so a
  stale entry explains itself. Write what a reader six months from now needs
  in order to tell a deliberate declaration from an unexamined one.

  Use this only for *genuine* vulnerabilities. A benign finding — an attack
  that "succeeds" against a design artifact that is not exploitable —
  belongs in 'acceptedFindings': listing it here advertises the contract as
  vulnerable, and fails the suite the moment the finding stops being
  reproduced.
  -}
  expectedVulnerabilities :: [(ThreatModel (), String)]
  expectedVulnerabilities = []

  {- | Findings that are known, accepted artifacts of the contract's design
  rather than exploitable bugs, each with the reason it is accepted. A
  detection is the required outcome, and the report labels it as accepted by
  design rather than as a vulnerability.

  Judged exactly like 'expectedVulnerabilities' - an acceptance is a
  declaration too, so the case fails when the run disproves it ("NO LONGER
  DETECTED") or never verifies it. What differs is only what the
  declaration means: this slot says the attack lands on something harmless,
  where 'expectedVulnerabilities' says the contract has a bug nobody has
  fixed. Prefer this one for anything that is not genuinely exploitable -
  the other advertises the contract as vulnerable in every report.
  -}
  acceptedFindings :: [(ThreatModel (), String)]
  acceptedFindings = []

  {- | Threat models reviewed and found not to apply to this contract, each
  with the reason. The inverse claim to 'threatModels': these must /not/
  apply, and one that starts applying fails the suite.

  That is the point of the slot. "Does not apply here" is a prediction about
  the contract's transaction shapes, and predictions break — a contract that
  grows a script output, or a harness change that widens what counts as
  applicable, should tell you rather than pass in silence. Deleting a model
  from the lists instead records the same triage where nothing can check it.
  -}
  notApplicable :: [(ThreatModel (), String)]
  notApplicable = []

{- | Default 'AddressLabeler': labels the credential hashes of the standard
mock wallets ('Convex.Wallet.MockWallet.mockWallets') as @"Wallet 1".."Wallet
10"@.
-}

{- | The default 'candidateModels' survey: every parameterless threat model
except the given ones, which is how a slot excludes the models it has
already spoken for.

Models are compared by name, totally: an unnamed model is never equal to
anything (not even another unnamed model), so an unnamed entry in the
excluded list simply excludes nothing. It must not error on unnamed models -
they are legal in the triaged slots, where they get index-based fallback
names before anything runs (see 'nameFallbacks').
-}
defaultThreatModelsExcluding :: [ThreatModel ()] -> [ThreatModel ()]
defaultThreatModelsExcluding excluded = deleteFirstsBy eqName allThreatModels excluded
 where
  eqName a b = case (getThreatModelName a, getThreatModelName b) of
    (Just s, Just t) -> s == t
    _ -> False

mockWalletAddressLabeler :: AddressLabeler
mockWalletAddressLabeler = AddressLabeler (`Map.lookup` table)
 where
  table =
    Map.fromList
      [ (C.serialiseToRawBytesHexText (verificationKeyHash w), "Wallet " <> T.pack (show i))
      | (i, w) <- zip [1 :: Int ..] Wallet.mockWallets
      ]

{- | Tests run in the mockchain monad extended with balancing error handling.

Leaving handling of balancing errors to the testing interface is important because
the errors can contain data for code coverage.
-}
newtype TestingMonadT m a = TestingMonadT
  { unTestingMonadT :: ExceptT (BalanceTxError C.ConwayEra) (MockchainT C.ConwayEra m) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , C.MonadError (BalanceTxError C.ConwayEra)
    , C.MonadIO
    , MonadState (MockChainState C.ConwayEra)
    , MonadLog
    , MonadBlockchain C.ConwayEra
    , MonadMockchain C.ConwayEra
    , MonadUtxoQuery
    )

deriving via (BalancingT (TestingMonadT m)) instance (Monad m) => MonadBalance C.ConwayEra (TestingMonadT m)

runTestingMonadT
  :: NodeParams C.ConwayEra
  -> TestingMonadT m a
  -> m (Either (BalanceTxError C.ConwayEra) a, MockChainState C.ConwayEra)
runTestingMonadT params (TestingMonadT action) =
  runMockchainT (runExceptT action) params (initialStateFor params Wallet.initialUTxOs)

-- Let the TestingMonad fail in IO
instance (MonadIO m) => MonadFail (TestingMonadT m) where
  fail s = liftIO $ fail s

instance MonadTrans TestingMonadT where
  lift = TestingMonadT . lift . lift

-- | Opaque wrapper for model state
newtype ModelState state = ModelState {unModelState :: state}
  deriving (Eq, Show)

{- | Per-threat-model accumulated results across all QuickCheck iterations.
Key is the threat model name, value is the list of iteration results.
Each iteration result is a pair of the threat model outcome and related error messages.
-}

{- | Identity of a threat model within one suite run: the slot it was
declared in, plus its name.

The name alone is not an identity. Parameterised variants share one
('largeValueAttackWith' 10 and 1000 are both "Large Value Attack"), so a
variant listed as an accepted finding used to record its 'TMFailed'
outcomes under the same key as a same-named model in 'threatModels' -
failing the claimed model's test case, and then suppressing it via the
early-stop check. Slot-qualifying the key keeps the two apart.

Two models with the same name in the /same/ slot still collide; that is a
declaration mistake rather than a harness one.
-}
data ThreatModelId = ThreatModelId
  { tmiCategory :: ThreatModelCategory
  , tmiName :: String
  }
  deriving (Eq, Ord, Show)

type ThreatModelResults = Map.Map ThreatModelId [(ThreatModelOutcome, [String])]

{- | A model together with the reason its slot records for it. The three
triaged slots require a reason; 'threatModels' and 'candidateModels' carry
none, and the test cases built for those two never read the field.
-}
type DeclaredModel = (ThreatModel (), String)

-- | Models from a slot that carries no reason.
withoutReason :: [ThreatModel ()] -> [DeclaredModel]
withoutReason = map (\tm -> (tm, ""))

-- | Models from a slot that carries a reason, given fallback names.
declaredWith :: String -> [(ThreatModel (), String)] -> [DeclaredModel]
declaredWith prefix ms = zip (nameFallbacks prefix (map fst ms)) (map snd ms)

-- | Try up to 100 times to generate a value satisfying a predicate
suchThatMaybe :: Gen a -> (a -> Bool) -> Gen (Maybe a)
suchThatMaybe gen p = go (100 :: Int)
 where
  go 0 = pure Nothing
  go retries = do
    a <- gen
    if p a then pure (Just a) else go (retries - 1)

-- | Generate a valid actions
genAction :: (TestingInterface state, Monad m) => state -> PropertyM m (Maybe (Action state))
genAction s = pick $ arbitraryAction s `suchThatMaybe` precondition s

-- | Options for running property tests
data RunOptions = RunOptions
  { verbose :: Bool
  -- ^ Print actions as they are executed
  , maxActions :: Int
  -- ^ Maximum number of actions to generate
  , mcOptions :: Options C.ConwayEra
  , disableNegativeTesting :: Maybe String
  {- ^ If @Just reason@, negative tests are skipped (shown as IGNORED) with the given reason.
  If @Nothing@, negative tests run normally. Default: @Nothing@.
  -}
  , threatModelFilter :: [String]
  {- ^ If non-empty, run only threat models whose names start with any value
  in this list. The filter applies to every slot of 'ThreatModelsFor', so a
  narrowed run builds test cases for the matching models only. If empty, all
  threat models run. Default: @[]@.
  -}
  }

defaultRunOptions :: RunOptions
defaultRunOptions =
  RunOptions
    { verbose = False
    , maxActions = 10
    , mcOptions = defaultOptions
    , disableNegativeTesting = Nothing
    , threatModelFilter = []
    }

{- | Give every unnamed model in a group its index-based fallback name, so
that recording, filtering, and reporting all use one and the same name.
'positiveTest' records outcomes under the model's name and the per-model
test cases look their results up by name again - an unnamed model would be
recorded under a shared literal "Unnamed" key that no test case ever looks
up, making e.g. an unnamed expected vulnerability pass silently without its
assertion ever running.
-}
nameFallbacks :: String -> [ThreatModel ()] -> [ThreatModel ()]
nameFallbacks prefix = zipWith giveName [1 :: Int ..]
 where
  giveName i tm = case getThreatModelName tm of
    Just _ -> tm
    Nothing -> Named (prefix <> " " <> show i) tm

{- | The name of a model that went through 'nameFallbacks'. Every model
reaching the run/record/report machinery is Named; if a code path ever
bypasses the normalization, trip loudly here instead of silently recording
outcomes under a name no test case looks up.
-}
modelName :: ThreatModel () -> String
modelName tm =
  fromMaybe
    (error "modelName: unnamed threat model - models must go through nameFallbacks before running")
    (getThreatModelName tm)

{- | Does this model pass @--threat-model-name@? An empty filter admits
everything; otherwise the model's rendered name must start with one of the
given prefixes.
-}
matchesThreatModelFilter :: RunOptions -> ThreatModel () -> Bool
matchesThreatModelFilter RunOptions{threatModelFilter} tm =
  case threatModelFilter of
    [] -> True
    names -> any (`isPrefixOf` modelName tm) names

{- | Main property for testing a testing interface.
Generates random action sequences and checks that the implementation matches the model.
-}
propRunActions :: forall state. (HasCallStack, ThreatModelsFor state) => String -> TestTree
propRunActions name =
  withFrozenCallStack (withSrcLoc (propRunActionsWithOptions @state name defaultRunOptions))

-- | Run testing interface tests with custom options
propRunActionsWithOptions
  :: forall state
   . (HasCallStack, ThreatModelsFor state)
  => String
  -> RunOptions
  -> TestTree
propRunActionsWithOptions groupName opts =
  withFrozenCallStack $
    withSrcLoc $
      askOption $ \(recorder :: TraceRecorder) ->
        -- Fallback names are assigned before filtering, so an unnamed model's
        -- index does not shift with the filter.
        let keep = filter (matchesThreatModelFilter opts . fst)
            tms = keep (withoutReason (nameFallbacks "Threat model" (threatModels @state)))
            cands = keep (withoutReason (nameFallbacks "Candidate model" (candidateModels @state)))
            evs = keep (declaredWith "Expected vulnerability" (expectedVulnerabilities @state))
            afs = keep (declaredWith "Accepted finding" (acceptedFindings @state))
            nas = keep (declaredWith "Not applicable" (notApplicable @state))
            {- The one place a model is tagged with what the suite claims
            about it: the claim decides how the model's outcomes are tallied
            and reported, which Tasty group its test
            case lands in, the category that travels into the model's trace
            entries, and whether zero attack coverage fails the case.

            Which slot a model was declared in *is* the claim - there is
            nothing left to infer about it. -}
            categorized =
              [ (Claimed, tms, threatModelTestCase)
              , (Surveyed, cands, threatModelTestCase)
              , (Expected, evs, triagedFindingTestCase)
              , (Accepted, afs, triagedFindingTestCase)
              , (NotApplicable, nas, notApplicableTestCase)
              ]
            modelsToRun = concat [map ((,) claim . fst) models | (claim, models, _) <- categorized]
         in if all null [tms, cands, evs, afs, nas]
              then
                -- No threat models: simple structure (backward compatible)
                withResource (newIORef (0 :: Int)) (\_ -> pure ()) $ \getPosRef ->
                  withResource (newIORef (0 :: Int)) (\_ -> pure ()) $ \getNegRef ->
                    testGroup
                      groupName
                      [ testProperty "Positive tests" (positiveTest @state opts groupName Nothing [] recorder getPosRef)
                      , negativeTestTree recorder getNegRef
                      ]
              else
                -- Has threat models: two-phase approach with IORef
                withResource (newIORef Map.empty) (\_ -> pure ()) $ \getTmResultsRef ->
                  withResource (newIORef (0 :: Int)) (\_ -> pure ()) $ \getPosRef ->
                    withResource (newIORef (0 :: Int)) (\_ -> pure ()) $ \getNegRef ->
                      sequentialTestGroup groupName AllFinish $
                        [ -- Every category's models run in this one property:
                          -- claimed models stop early on a detection, expected
                          -- vulnerabilities and accepted findings always run,
                          -- quietly. They differ only in reporting, below.
                          testProperty "Positive tests" (positiveTest @state opts groupName (Just getTmResultsRef) modelsToRun recorder getPosRef)
                        , negativeTestTree recorder getNegRef
                        ]
                          <> perCategoryGroups categorized getTmResultsRef
 where
  negativeTestTree :: (HasCallStack) => TraceRecorder -> IO (IORef Int) -> TestTree
  negativeTestTree recorder getNegRef =
    withFrozenCallStack $
      case disableNegativeTesting opts of
        Nothing -> testProperty "Negative tests" (negativeTest @state opts groupName recorder getNegRef)
        Just reason -> ignoreTestBecause reason $ testProperty "Negative tests" (negativeTest @state opts groupName recorder getNegRef)

  -- One per-model group per non-empty category, each reporting its own
  -- models' outcomes once the positive tests have recorded them.
  perCategoryGroups categorized getTmResultsRef =
    [ testGroup group $ map (testCaseFor claim getTmResultsRef group) models
    | (claim, models, testCaseFor) <- categorized
    , not (null models)
    , let group = threatModelGroupName claim
    ]

{- | The models to run on one iteration of the positive property, given what
earlier iterations already recorded.

A detection fails the test case for a 'Claimed' or a 'Surveyed' model, so
once one has failed there is nothing left to learn from re-running it and it
is dropped. The triaged slots always run: their verdict depends on whether
the finding reproduces consistently, not on a single hit.

The caller has already applied the @--threat-model-name@ filter (see
'propRunActionsWithOptions'), so this does not re-apply it.
-}
modelsForIteration
  :: ThreatModelResults
  -> [(ThreatModelCategory, ThreatModel ())]
  -> [(ThreatModelCategory, ThreatModel ())]
modelsForIteration existingResults = filter keep
 where
  isTMFailed (TMFailed _) = True
  isTMFailed _ = False
  alreadyFailed tmid = any (isTMFailed . fst) (fromMaybe [] (Map.lookup tmid existingResults))
  keep (cat, tm)
    | cat == Claimed || cat == Surveyed = not (alreadyFailed (ThreatModelId cat (modelName tm)))
    | otherwise = True

-- | Negative test: check that invalid actions fail
negativeTest
  :: forall state
   . (TestingInterface state)
  => RunOptions
  -> String
  -- ^ Group name for test ID resolution
  -> TraceRecorder
  -- ^ Callback for recording iteration traces
  -> IO (IORef Int)
  -- ^ Iteration counter accessor
  -> Property
negativeTest opts groupName recorder getIterRef = monadicIO $ do
  -- Bump and read iteration index
  iterIdx <- run $ do
    iterRef <- getIterRef
    idx <- readIORef iterRef
    modifyIORef iterRef (+ 1)
    pure idx
  enabled <- run $ trEnabled recorder
  if enabled
    then negativeTestTraced @state opts groupName recorder iterIdx
    else negativeTestFast @state opts

-- | Traced path for negative tests: runs 'runActionsTraced', builds traces.
negativeTestTraced
  :: forall state
   . (TestingInterface state)
  => RunOptions
  -> String
  -> TraceRecorder
  -> Int
  -> PropertyM IO Property
negativeTestTraced opts groupName recorder iterIdx = do
  let RunOptions{mcOptions = Options{coverageRef, params}} = opts
  -- Phase 1: Run the valid prefix, capturing the final mockchain state
  (prefixResult, prefixState) <- runTestingMonadT params $ do
    initialState <- runInitialization @state opts

    (finalState, transitions) <- runActionsTraced opts initialState

    -- Generate an action that VIOLATES the precondition in that state
    result <- lift $ pick $ do
      maybeInvalid <- arbitraryAction finalState `suchThatMaybe` (not . precondition finalState)
      case maybeInvalid of
        Nothing -> discard -- tell QuickCheck to skip this case
        Just bad -> pure (bad, finalState)
    pure (result, transitions)

  -- Phase 2: Run the bad action starting from the state left by the valid prefix
  case prefixResult of
    Left err -> do
      monitor (counterexample $ "Valid prefix failed: " ++ show err)
      -- Record failed iteration trace
      let trace =
            IterationTrace
              { itIndex = iterIdx
              , itStatus = IterationFailure (formatBalanceTxError err)
              , itTransitions = []
              , itThreatModels = []
              }
      run $ recordIteration recorder groupName "negative" [] (toJSON trace)
      pure (property False)
    Right ((badAction, finalState), transitions) -> do
      let monadAction = runExceptT $ unTestingMonadT $ perform finalState badAction
      result' <- run $ try @SomeException $ runMockchainIO monadAction params prefixState
      -- We distinguish between validation errors and user errors:
      -- if the action failed at the off-chain level (e.g. balancing), we discard the test,
      -- but if it failed after submission (i.e. validator rejection), we count it as a success.
      let badActionText = T.pack (show badAction)
          badTransition status =
            Transition
              { trStepIndex = length transitions
              , trAction = badActionText
              , trStateBefore = toJSON finalState
              , trStateAfter = toJSON finalState
              , trTransaction = Nothing
              , trResult = status
              }
      case result' of
        -- we try another round of bad actions
        Left ex | discardNegativeTestForUserExceptions @state -> do
          let trace =
                IterationTrace
                  { itIndex = iterIdx
                  , itStatus = IterationDiscarded (T.pack (show ex))
                  , itTransitions = transitions <> [badTransition (TransitionFailure (T.pack (show ex)))]
                  , itThreatModels = []
                  }
          run $ recordIteration recorder groupName "negative" [] (toJSON trace)
          discard
        Left ex -> do
          let trace =
                IterationTrace
                  { itIndex = iterIdx
                  , itStatus = IterationSuccess
                  , itTransitions = transitions <> [badTransition (TransitionFailure (T.pack (show ex)))]
                  , itThreatModels = []
                  }
          run $ recordIteration recorder groupName "negative" [] (toJSON trace)
          pure (property True)
        Right result ->
          case result of
            (Left err, MockChainState{mcsCoverageData}) -> do
              let covData = mcsCoverageData <> coverageFromBalanceTxError err
              -- Good: the invalid action failed via BalanceTxError (validator rejection)
              for_ coverageRef $ \ref -> liftIO $ modifyIORef ref (<> covData)
              let trace =
                    IterationTrace
                      { itIndex = iterIdx
                      , itStatus = IterationSuccess
                      , itTransitions = transitions <> [badTransition (TransitionFailure (formatBalanceTxError err))]
                      , itThreatModels = []
                      }
              run $ recordIteration recorder groupName "negative" (covDataToSrcLocRanges covData) (toJSON trace)
              pure (property True)
            (Right _, MockChainState{mcsCoverageData = covData}) -> do
              -- Bad: the invalid action succeeded — contract is too permissive
              for_ coverageRef $ \ref -> liftIO $ modifyIORef ref (<> covData)
              monitor (counterexample $ "Expected failure for invalid action but it succeeded")
              let trace =
                    IterationTrace
                      { itIndex = iterIdx
                      , itStatus = IterationFailure "Invalid action succeeded unexpectedly"
                      , itTransitions = transitions <> [badTransition (TransitionSuccess T.empty)]
                      , itThreatModels = []
                      }
              run $ recordIteration recorder groupName "negative" (covDataToSrcLocRanges covData) (toJSON trace)
              pure (property False)

-- | Fast path for negative tests: runs 'runActions' (no tracing overhead).
negativeTestFast
  :: forall state
   . (TestingInterface state)
  => RunOptions
  -> PropertyM IO Property
negativeTestFast opts = do
  let RunOptions{mcOptions = Options{coverageRef, params}} = opts
  -- Phase 1: Run the valid prefix, capturing the final mockchain state
  (prefixResult, prefixState) <- runTestingMonadT params $ do
    initialState <- runInitialization @state opts

    finalState <- runActions opts initialState

    -- Generate an action that VIOLATES the precondition in that state
    result <- lift $ pick $ do
      maybeInvalid <- arbitraryAction finalState `suchThatMaybe` (not . precondition finalState)
      case maybeInvalid of
        Nothing -> discard
        Just bad -> pure (bad, finalState)
    pure result

  -- Phase 2: Run the bad action starting from the state left by the valid prefix
  case prefixResult of
    Left err -> do
      monitor (counterexample $ "Valid prefix failed: " ++ show err)
      pure (property False)
    Right (badAction, finalState) -> do
      let monadAction = runExceptT $ unTestingMonadT $ perform finalState badAction
      result' <- run $ try @SomeException $ runMockchainIO monadAction params prefixState
      case result' of
        Left _ | discardNegativeTestForUserExceptions @state -> discard
        Left _ -> pure (property True)
        Right result ->
          case result of
            (Left err, MockChainState{mcsCoverageData = covData}) -> do
              for_ coverageRef $ \ref -> liftIO $ modifyIORef ref (<> (covData <> coverageFromBalanceTxError err))
              pure (property True)
            (Right _, MockChainState{mcsCoverageData = covData}) -> do
              for_ coverageRef $ \ref -> liftIO $ modifyIORef ref (<> covData)
              monitor (counterexample $ "Expected failure for invalid action but it succeeded")
              pure (property False)

{- | Positive test with optional threat model outcome collection.
When threat models list is empty, it behaves as a simple positive test.
When threat models are present, each is run in isolation with exception handling.
-}
positiveTest
  :: forall state
   . (TestingInterface state)
  => RunOptions
  -> String
  -- ^ Group name for test ID resolution
  -> Maybe (IO (IORef ThreatModelResults))
  -- ^ IORef for collecting results (Nothing = no threat models, don't collect)
  -> [(ThreatModelCategory, ThreatModel ())]
  {- ^ The threat models to run, each tagged with the @ThreatModelsFor@ list it
  came from. Only 'Claimed' ones early-stop on TMFailed and honour the
  @--threat-model@ filter; 'Expected' and 'Accepted' ones always run.
  -}
  -> TraceRecorder
  -- ^ Callback for recording iteration traces
  -> IO (IORef Int)
  -- ^ Iteration counter accessor (bumped each QuickCheck iteration)
  -> Property
positiveTest opts groupName mGetTmResultsRef tms recorder getIterRef = monadicIO $ do
  -- Bump and read iteration index
  iterIdx <- run $ do
    iterRef <- getIterRef
    idx <- readIORef iterRef
    modifyIORef iterRef (+ 1)
    pure idx
  enabled <- run $ trEnabled recorder
  if enabled
    then positiveTestTraced @state opts groupName mGetTmResultsRef tms recorder iterIdx
    else positiveTestFast @state opts mGetTmResultsRef tms

-- | Traced path: runs 'runActionsTraced', builds 'IterationTrace', records it.
positiveTestTraced
  :: forall state
   . (TestingInterface state)
  => RunOptions
  -> String
  -> Maybe (IO (IORef ThreatModelResults))
  -> [(ThreatModelCategory, ThreatModel ())]
  -> TraceRecorder
  -> Int
  -> PropertyM IO Property
positiveTestTraced opts groupName mGetTmResultsRef tms recorder iterIdx = do
  let RunOptions{mcOptions = Options{coverageRef, params}} = opts
  result <- runTestingMonadT params $ do
    initialState <- runInitialization @state opts
    initTxs <- getTxs
    state0 <- get

    (finalState, transitions) <- runActionsTraced opts initialState

    allTxs <- getTxs
    let envs = threatModelEnvs params (drop (length initTxs) $ reverse allTxs) state0
    existingResults <- case mGetTmResultsRef of
      Just getTmRef -> liftIO $ do
        tmRef <- getTmRef
        readIORef tmRef
      Nothing -> pure Map.empty
    let allToRun = modelsForIteration existingResults tms
    -- An iteration that generated no transactions records no outcomes at
    -- all: running the models over an empty env list would record a
    -- TMSkipped per model, indistinguishable from a genuine precondition
    -- miss, and the vacuity check would blame model applicability for what
    -- is a test-generation issue. With nothing recorded, an all-empty run
    -- reports "No tests were generated by positive tests" instead.
    tmResultsWithCov <-
      if null envs
        then pure []
        else liftIO $ forM allToRun $ \(category, tm) -> do
          let tmid = ThreatModelId category (modelName tm)
          ((outcome, traceEntries, monitors), tmFinalState) <-
            runMockchainIO (runThreatModelCheckTraced AutoSign tm envs) params state0
          pure (tmid, category, outcome, traceEntries, mcsCoverageData tmFinalState, monitors)

    pure (finalState, transitions, tmResultsWithCov)

  case result of
    (Left err, MockChainState{mcsCoverageData}) -> do
      let covData = mcsCoverageData <> coverageFromBalanceTxError err
      for_ coverageRef $ \ref -> liftIO $ modifyIORef ref (<> covData)
      let trace =
            IterationTrace
              { itIndex = iterIdx
              , itStatus = IterationFailure (formatBalanceTxError err)
              , itTransitions = []
              , itThreatModels = []
              }
      run $ recordIteration recorder groupName "positive" (covDataToSrcLocRanges covData) (toJSON trace)
      pure (property False)
    (Right (finalState, transitions, tmResultsWithCov), MockChainState{mcsCoverageData}) -> do
      let covData = mcsCoverageData <> mconcat [cov | (_, _, _, _, cov, _) <- tmResultsWithCov]
      monitor (counterexample $ "Final state: " ++ show finalState)
      traverse_ (\ref -> liftIO $ modifyIORef ref (<> covData)) coverageRef
      case mGetTmResultsRef of
        Just getTmResultsRef -> run $ do
          let tmResults = [(n, summarizeThreatModelIteration o entries) | (n, _, o, entries, _, _) <- tmResultsWithCov]
          tmRef <- getTmResultsRef
          modifyIORef tmRef $ \existing ->
            foldl'
              (\m (name, outcomeAndEntries) -> Map.insertWith (<>) name [outcomeAndEntries] m)
              existing
              tmResults
        Nothing -> pure ()
      tmTraces <- liftIO $ toThreatModelTraces (findTestIdIO recorder groupName) (redeemerTagger @state) (addressLabeler @state) [(tmiName n, cat, o, e, c) | (n, cat, o, e, c, _) <- tmResultsWithCov]
      let trace =
            IterationTrace
              { itIndex = iterIdx
              , itStatus = IterationSuccess
              , itTransitions = transitions
              , itThreatModels = tmTraces
              }
      run $ recordIteration recorder groupName "positive" (covDataToSrcLocRanges mcsCoverageData) (toJSON trace)
      let allMonitors = foldr (.) id [m | (_, _, _, _, _, m) <- tmResultsWithCov]
      monitor allMonitors
      pure (property True)

-- | Fast path: runs 'runActions' (no UTxO snapshots, no tx summaries, no JSON).
positiveTestFast
  :: forall state
   . (TestingInterface state)
  => RunOptions
  -> Maybe (IO (IORef ThreatModelResults))
  -> [(ThreatModelCategory, ThreatModel ())]
  -> PropertyM IO Property
positiveTestFast opts mGetTmResultsRef tms = do
  let RunOptions{mcOptions = Options{coverageRef, params}} = opts
  result <- runTestingMonadT params $ do
    initialState <- runInitialization @state opts
    initTxs <- getTxs
    state0 <- get

    finalState <- runActions opts initialState

    allTxs <- getTxs
    let envs = threatModelEnvs params (drop (length initTxs) $ reverse allTxs) state0
    existingResults <- case mGetTmResultsRef of
      Just getTmRef -> liftIO $ do
        tmRef <- getTmRef
        readIORef tmRef
      Nothing -> pure Map.empty
    let allToRun = modelsForIteration existingResults tms
    -- An iteration that generated no transactions records no outcomes at
    -- all: running the models over an empty env list would record a
    -- TMSkipped per model, indistinguishable from a genuine precondition
    -- miss, and the vacuity check would blame model applicability for what
    -- is a test-generation issue. With nothing recorded, an all-empty run
    -- reports "No tests were generated by positive tests" instead.
    tmResultsWithCov <-
      if null envs
        then pure []
        else liftIO $ forM allToRun $ \(category, tm) -> do
          let tmid = ThreatModelId category (modelName tm)
          ((outcome, traceEntries, monitors), tmFinalState) <-
            runMockchainIO (runThreatModelCheckTraced AutoSign tm envs) params state0
          -- Summarise here, not at the use site. This path keeps no trace,
          -- so nothing downstream reads the entries - but each one holds two
          -- transactions and two UTxO sets, and left as a thunk they would
          -- stay reachable from the results 'IORef' until the per-model test
          -- cases run, long after the last iteration.
          summary <- evaluate (summarizeThreatModelIteration outcome traceEntries)
          pure (tmid, summary, mcsCoverageData tmFinalState, monitors)

    let tmResults = [(n, summary) | (n, summary, _, _) <- tmResultsWithCov]
        tmCoverage = mconcat [cov | (_, _, cov, _) <- tmResultsWithCov]
        tmMonitors = [m | (_, _, _, m) <- tmResultsWithCov]

    pure (finalState, tmResults, tmCoverage, tmMonitors)

  case result of
    (Left err, MockChainState{mcsCoverageData = covData}) -> do
      for_ coverageRef $ \ref -> liftIO $ modifyIORef ref (<> (covData <> coverageFromBalanceTxError err))
      pure (property False)
    (Right (finalState, tmResults, tmCoverage, tmMonitors), MockChainState{mcsCoverageData = covData}) -> do
      monitor (counterexample $ "Final state: " ++ show finalState)
      traverse_ (\ref -> liftIO $ modifyIORef ref (<> covData <> tmCoverage)) coverageRef
      case mGetTmResultsRef of
        Just getTmResultsRef -> run $ do
          tmRef <- getTmResultsRef
          modifyIORef tmRef $ \existing ->
            foldl'
              (\m (name, outcomeAndEntries) -> Map.insertWith (<>) name [outcomeAndEntries] m)
              existing
              tmResults
        Nothing -> pure ()
      let allMonitors = foldr (.) id tmMonitors
      monitor allMonitors
      pure (property True)

{- | The part every per-model test case does the same way: look the model's
recorded outcomes up under its slot-qualified key, tally them, surface any
errors as warnings, record the summary, and dispatch the two cases that mean
the same thing in every slot — no transactions at all, and none the model
could be tried on.

Only the tested case differs by slot, so that is all a caller supplies:
zero coverage is reported the same way everywhere, by 'reportZeroCoverage',
which is where the per-slot meaning of "nothing applied" lives. The summary
is recorded here, before the verdict runs, so that a verdict which fails
can re-record it with a fault (see 'failWithFault') and win.
-}
perModelCase
  :: ThreatModelCategory
  -> IO (IORef ThreatModelResults)
  -> String
  -- ^ Tasty group name (for keying summaries)
  -> ThreatModel ()
  -> ((String -> IO ()) -> TMRecorder -> String -> ThreatModelSummary -> [(ThreatModelOutcome, [String])] -> IO ())
  -- ^ What to do when the model was actually tested
  -> TestTree
perModelCase claim getTmResultsRef groupName tm onTested =
  let name = modelName tm
      key = groupName <> "/" <> name
   in askOption $ \(recorder :: TMRecorder) ->
        testCaseSteps name $ \step -> do
          tmRef <- getTmResultsRef
          allResults <- readIORef tmRef
          let outcomeEntries = fromMaybe [] (Map.lookup (ThreatModelId claim name) allResults)
              outcomes = map fst outcomeEntries
              summary = tallyOutcomes claim name outcomes
              ThreatModelSummary{tmsTotal = total, tmsTested = tested} = summary

          -- Errors are warnings: they say nothing either way about the verdict.
          reportErrors step outcomes
          tmRecord recorder key summary

          if total == 0
            then step "No tests were generated by positive tests"
            else
              if tested == 0
                then reportZeroCoverage step recorder key claim summary outcomeEntries
                else onTested step recorder key summary outcomeEntries

-- | Create a test case for displaying threat model results
threatModelTestCase
  :: ThreatModelCategory
  {- ^ Which slot the model was declared in, which is what the suite claims
  about it (see 'propRunActionsWithOptions')
  -}
  -> IO (IORef ThreatModelResults)
  -> String
  -- ^ Tasty group name (for keying summaries)
  -> DeclaredModel
  -- ^ The threat model, and the reason its slot records
  -> TestTree
threatModelTestCase claim getTmResultsRef groupName (tm, _noReason) =
  perModelCase claim getTmResultsRef groupName tm onTested
 where
  onTested step recorder key summary outcomeEntries = do
    let outcomes = map fst outcomeEntries
        ThreatModelSummary{tmsTotal = total, tmsPassed = numPassed} = summary
    step $ "Tested " <> show numPassed <> "/" <> show total <> " tests (" <> skipCounts summary <> ")"
    case [msg | TMFailed msg <- outcomes] of
      [] -> pure ()
      (firstFailure : rest) ->
        failWithFault recorder key summary (if claim == Surveyed then Declaration else Contract) $
          [ if claim == Surveyed
              then "an untriaged model detected a vulnerability after " <> show (numPassed + 1) <> " tests."
              else "vulnerability detected after " <> show (numPassed + 1) <> " tests."
          , case claim of
              -- Nobody asked for this model, so the run demands a triage
              -- decision rather than a contract fix.
              Surveyed ->
                "  It is in 'candidateModels'. Move it to 'threatModels' if the contract should resist it, or to 'expectedVulnerabilities' / 'acceptedFindings' with a reason."
              _ ->
                "  'threatModels' claims the contract resists this."
          , ""
          , firstFailure
          ]
            <> ["... and " <> show (length rest) <> " more similar failure(s) suppressed" | not (null rest)]

{- | Build a test case for a finding the suite has already triaged: an
expected vulnerability ('ThreatModelsFor.expectedVulnerabilities') or an
accepted finding ('ThreatModelsFor.acceptedFindings').

Both are declarations of the same shape — "this attack lands here, and I
have decided what that means" — so they are judged identically: inverted
pass/fail (a detection is the required outcome), always run against every
transaction rather than early-stopping, quiet output, and a 'Declaration'
failure when the run disproves the declaration or never verifies it.

What differs is only what the declaration *means*, and that has to stay
legible: an expected vulnerability says the contract has a bug nobody has
fixed, an accepted finding says the attack lands on something harmless. The
slot carries that distinction into the reports and into the streamed
'ThreatModelCategory', and it picks the wording below; it must not decide
the policy, or the two drift apart again.
-}
triagedFindingTestCase
  :: ThreatModelCategory
  {- ^ Which slot the model was declared in, which is what the suite claims
  about it (see 'propRunActionsWithOptions')
  -}
  -> IO (IORef ThreatModelResults)
  -> String
  -- ^ Tasty group name (for keying summaries)
  -> DeclaredModel
  -- ^ The triaged model, and the reason its slot records
  -> TestTree
triagedFindingTestCase claim getTmResultsRef groupName (tm, why) =
  perModelCase claim getTmResultsRef groupName tm onTested
 where
  accepted = claim == Accepted
  onTested step recorder key summary outcomeEntries = do
    let ThreatModelSummary{tmsTotal = total, tmsFailed = numFound, tmsTested = tested} = summary
        validationErrors = distinctValidationErrors outcomeEntries
        validationErrorLines = case validationErrors of
          [] -> []
          _ ->
            let (shown, remaining) = splitAt 3 validationErrors
             in ["Validation errors:"]
                  <> map ("  " <>) shown
                  <> ["  ... and " <> show (length remaining) <> " more" | not (null remaining)]
    if numFound > 0
      then
        step $
          if accepted
            then "Finding detected (" <> show numFound <> "/" <> show tested <> " tests, " <> skipCounts summary <> ") - accepted by design, not counted as a vulnerability: " <> why
            else "Vulnerability detected (" <> show numFound <> "/" <> show total <> " tests, " <> skipCounts summary <> ")"
      else
        -- The run disproves the declaration: the attack no longer lands. Same
        -- fault in both slots; only the consequence for the reader differs.
        failWithFault recorder key summary Declaration $
          ( if accepted
              then
                [ "NO LONGER DETECTED - this finding no longer occurs (" <> show tested <> " of " <> show total <> " transactions attacked)."
                , "  It was accepted as a benign artifact; that acceptance is now stale."
                , "  Remove it from 'acceptedFindings', or move it to 'threatModels' to"
                , "  assert the contract resists it."
                , "  Accepted because: " <> why
                ]
              else
                [ "RESOLVED - this vulnerability is no longer detected (" <> show tested <> " of " <> show total <> " transactions attacked)."
                , "  Good news for the contract; this declaration is now stale."
                , "  Move it to 'threatModels' if the contract now resists it, to"
                , "  'acceptedFindings' if the finding was benign, or remove it."
                , "  Declared because: " <> why
                ]
          )
            <> validationErrorLines

{- | Tally one threat model's per-iteration outcomes into its summary. The
category records which 'ThreatModelsFor' list the model came from, so that a
consumer of the summary can tell a 'tmsFailed' count that means
"vulnerability" ('Claimed') from one that means "detected as expected"
('Expected') or "accepted by design" ('Accepted'). Shared by all three
per-model test cases.
-}
tallyOutcomes :: ThreatModelCategory -> String -> [ThreatModelOutcome] -> ThreatModelSummary
tallyOutcomes category name outcomes =
  ThreatModelSummary
    { tmsName = T.pack name
    , tmsCategory = category
    , tmsTested = numPassed + numFailed
    , tmsTotal = length outcomes
    , tmsPassed = numPassed
    , tmsFailed = numFailed
    , tmsSkipped = length [() | TMSkipped <- outcomes]
    , tmsSkippedPhase1 = length [() | TMSkippedPhase1 <- outcomes]
    , tmsErrors = length [() | TMError _ <- outcomes]
    , tmsFault = Nothing
    }
 where
  numPassed = length [() | TMPassed <- outcomes]
  numFailed = length [() | TMFailed _ <- outcomes]

{- | Fail a threat-model test case, naming whose fault it is on the first
line of the message and on the recorded summary — or naming none, when the
run does not yet establish one.

A detection on a 'Surveyed' model is the declaration's fault, not the
contract's: whatever the triage later concludes, the action the run demands
is to put the model in a slot. Attributing it to the contract would page
whoever routes on @fault@ for a finding nobody has classified yet.

A detection on a 'NotApplicable' model is the contract's, and the
difference is what each slot knows. 'Surveyed' carries no prior, so the
finding may always have been there and may be benign. 'NotApplicable'
records that the model could not apply here at all, so a detection means
both that it now applies and that the contract accepted the attack - which
is what introducing a vulnerability looks like. Making the two agree would
throw that prior away.

Only one cell of the slot-by-outcome matrix is the contract's fault, so a
bare failure is routinely misread as "the contract is broken" when it means
"this declaration is stale". Recording it too lets a dashboard route the
two apart without parsing prose.
-}
failWithFault :: TMRecorder -> String -> ThreatModelSummary -> Fault -> [String] -> IO ()
failWithFault recorder key summary fault ls = do
  tmRecord recorder key summary{tmsFault = Just fault}
  assertFailure $ unlines $ case ls of
    (firstLine : rest) -> (faultLabel fault <> ": " <> firstLine) : rest
    [] -> [faultLabel fault]

{- | Report the errors among the outcomes as warning steps, never failing the
test: the count, the first three messages, and how many more there were.
-}
reportErrors :: (String -> IO ()) -> [ThreatModelOutcome] -> IO ()
reportErrors step outcomes = case [msg | TMError msg <- outcomes] of
  [] -> pure ()
  errors -> do
    -- Count the erroring iterations, not the distinct messages: 100
    -- iterations failing the same way is a systematic fault, and the status
    -- line below reports the same 100 via 'skipCounts'. Only what is
    -- printed is deduplicated.
    step $ "WARNING: " <> show (length errors) <> " error(s) during threat model execution"
    let distinct = modelErrors outcomes
    mapM_ (step . ("  " <>)) (take 3 distinct)
    case drop 3 distinct of
      [] -> pure ()
      remaining -> step $ "  ... and " <> show (length remaining) <> " more"

{- | The distinct messages of the errors the model itself raised, as opposed
to the ledger's verdicts on the modified transactions: these come from
'TMError', which is reached before any precondition is evaluated.
-}
modelErrors :: [ThreatModelOutcome] -> [String]
modelErrors outcomes = nubOrd [msg | TMError msg <- outcomes]

-- | The skip and error counts as they appear inside every status line's parentheses.
skipCounts :: ThreatModelSummary -> String
skipCounts summary =
  show (tmsSkipped summary)
    <> " precondition skipped, "
    <> show (tmsSkippedPhase1 summary)
    <> " phase 1/rebalance skipped, "
    <> show (tmsErrors summary)
    <> " errors"

{- | The status line for a run where the model was never tested: every
outcome was a precondition miss, an environmental skip (phase 1
invalidation / rebalancing failure), or an error. Shared by all three
per-model test cases; the wording says which of the three it was.
-}
skippedMessage :: ThreatModelSummary -> String
skippedMessage summary = case zeroCoverageKind summary of
  PreconditionNeverMet -> line "Precondition never met" "applicable"
  AttackNeverCarriedOut -> line "Attack never carried out" "carried out"
  ModelErrored -> line "Threat model errored" "completed"
 where
  -- The verb carries the distinction: only in the first case was nothing
  -- "applicable". In the other two the model DID apply to some transaction,
  -- so saying nothing was applicable would name the wrong fault.
  line headline verb =
    "SKIPPED: "
      <> headline
      <> " ("
      <> skipCounts summary
      <> ", 0/"
      <> show (tmsTotal summary)
      <> " tests "
      <> verb
      <> ")"

-- | The three ways a model ends up with zero attack coverage.
data ZeroCoverageKind
  = -- | The model applied to no generated transaction at all
    PreconditionNeverMet
  | {- | The model applied to some transaction, but every attempt ended in a
    Phase 1 invalidation or a rebalancing failure
    -}
    AttackNeverCarriedOut
  | {- | The model itself errored (e.g. no signing wallet could be detected),
    which happens before any precondition is evaluated
    -}
    ModelErrored

{- | Which of the three it was. An environmental skip proves the precondition
held at least once, so it outranks an error, which says nothing either way.
-}
zeroCoverageKind :: ThreatModelSummary -> ZeroCoverageKind
zeroCoverageKind summary
  | tmsSkippedPhase1 summary > 0 = AttackNeverCarriedOut
  | tmsErrors summary > 0 = ModelErrored
  | otherwise = PreconditionNeverMet

{- | Build a test case for a model declared not to apply (see
'ThreatModelsFor.notApplicable'). The inverse of 'threatModelTestCase':
never applying is the passing outcome, and applying at all fails, because
the declaration predicted that it could not.

That is the whole value of the slot. Deleting a model from the lists
records the same triage decision somewhere nothing can check it, so a
contract that later grows the surface the model looks for - or a harness
change that widens what counts as applicable - goes unnoticed.
-}
notApplicableTestCase
  :: ThreatModelCategory
  {- ^ Which slot the model was declared in, which is what the suite claims
  about it (see 'propRunActionsWithOptions')
  -}
  -> IO (IORef ThreatModelResults)
  -> String
  -- ^ Tasty group name (for keying summaries)
  -> DeclaredModel
  -- ^ The threat model declared not to apply, and why
  -> TestTree
notApplicableTestCase claim getTmResultsRef groupName (tm, why) =
  perModelCase claim getTmResultsRef groupName tm onTested
 where
  {- Unlike a 'Surveyed' hit, this slot carries a prior: the model was
  recorded as unable to apply here. A detection therefore means two things
  changed at once - the model now applies, and the contract accepted the
  attack - which in practice is someone having introduced the vulnerability.
  So this is the contract's fault, where an untriaged survey hit (which
  carries no prior, and may always have been benign) is the declaration's.
  Detection here is 'tmsFailed', i.e. the mutated transaction still
  validated. -}
  onTested _step recorder key summary _entries
    | tmsFailed summary > 0 =
        failWithFault recorder key summary Contract $
          [ "VULNERABLE - a model recorded as not applicable now applies, and the contract"
          , "  accepted its attack (" <> show (tmsFailed summary) <> " of " <> show (tmsTested summary) <> " attacked transactions)."
          , "  This contract previously had no surface for it, so the likely cause is a"
          , "  change that introduced the vulnerability. Fix the contract."
          , "  Once it resists the attack, move the model to 'threatModels'."
          , "  Declared not applicable because: " <> why
          ]
    | otherwise =
        failWithFault recorder key summary Declaration $
          [ "APPLIES NOW - tested on " <> show (tmsTested summary) <> " of " <> show (tmsTotal summary) <> " transactions, but listed in 'notApplicable'."
          , "  The contract resisted the attack, so this is a stale declaration rather"
          , "  than a bug: the contract grew a surface this model looks for, or the"
          , "  harness widened what counts as applicable."
          , "  Move it to 'threatModels' to claim that resistance."
          , "  Declared not applicable because: " <> why
          ]

{- | The coverage policy for a model that was never tested (no outcome was
'TMPassed' or 'TMFailed'): 'Left' a failure message, or 'Right' the status
lines to report instead.

A claimed model promises that the contract resists the attack, an expected
vulnerability that it does not; either promise is unchecked when the attack
was never carried out, whatever the reason:

* 'PreconditionNeverMet': the model does not apply to any generated
  transaction. For a model the suite declared - 'Claimed', 'Expected' or
  'Accepted' - that is a fault in the test setup (it advertises coverage it
  cannot have), so it fails. A 'Surveyed' model was never declared, so there
  it is only reported, and for 'NotApplicable' it is the confirming outcome.

* 'AttackNeverCarriedOut': the precondition held somewhere, but every
  attempt hit a Phase 1 invalidation or a rebalancing failure. Per
  iteration these are environmental skips and never fail anything (a
  harness limit is not a contract bug), but a model skipped this way on
  EVERY iteration provides exactly as much coverage as one never run.

* 'ModelErrored': the model never got as far as a precondition, so the
  suite learned nothing at all.

In the latter two the declared slots fail, naming the distinct reasons so
the setup can be fixed; a 'Surveyed' model gets a loud warning instead,
since the user did not opt into it and failing would block them on a
limitation they may not be able to lift.

An 'Accepted' finding is judged exactly like an 'Expected' one: the
acceptance is a declaration too, so a run that never verifies it fails,
naming the same reasons.

Before this policy, the failure was guarded by "every skip was a
precondition miss", so a single environmental skip silenced it and a model
that never rebalanced stayed green forever.
-}
zeroCoverageVerdict :: ThreatModelCategory -> ThreatModelSummary -> [String] -> Either (Fault, String) [String]
zeroCoverageVerdict claim summary reasons = case claim of
  -- A surveyed model was never declared, so one that simply does not apply
  -- to this contract is reported, not failed; the other kinds still warn,
  -- since the user did not opt in and may not be able to lift a harness
  -- limitation.
  Surveyed
    | PreconditionNeverMet <- kind -> Right [skippedMessage summary]
    | otherwise ->
        Right $
          ("WARNING: zero attack coverage - " <> headline <> ".")
            : withReasons "  The model provides no evidence about this contract"
  -- Never applying is what this slot predicts, so only a precondition miss
  -- confirms it. An environmental skip proves the precondition held at least
  -- once, which falsifies the declaration; an error leaves it unverified.
  NotApplicable -> case kind of
    PreconditionNeverMet -> Right ["Confirmed not applicable: " <> headline]
    ModelErrored ->
      Right $
        ("WARNING: not confirmed - " <> headline <> ".")
          : withReasons "  The model never reached a precondition, so nothing here shows whether it applies"
    AttackNeverCarriedOut ->
      Left
        ( Declaration
        , unlines $
            ("APPLIES NOW - the precondition held, but no attack was carried out: " <> headline <> ".")
              : withReasons "  The model applies here, which is what 'notApplicable' denies. Move it to 'threatModels' and fix whatever stops the attack being built, or keep it here only if you can say why the precondition is spurious"
        )
  Claimed ->
    failure
      (lead "Threat model never applied" "Threat model never tested")
      ("Zero attack coverage means the claim in 'threatModels' is unchecked. " <> remedy <> ", move it to 'notApplicable' with a reason, or remove it")
  Expected ->
    failure
      (lead "Expected vulnerability never exercised" "Expected vulnerability never tested")
      ("Nothing confirms the vulnerability listed in 'expectedVulnerabilities'. " <> remedy <> ", or remove it")
  Accepted ->
    failure
      (lead "Accepted finding never applied" "Accepted finding never tested")
      ("Nothing confirms the finding listed in 'acceptedFindings', so the acceptance suppresses a model that proves nothing. " <> remedy <> ", or remove it")
 where
  kind = zeroCoverageKind summary
  -- A model that never applied is a claim you cannot have; one that applied
  -- but could not be attacked is a limit of the generator or the harness.
  faultOfKind = case kind of
    PreconditionNeverMet -> Declaration
    _ -> Setup
  failure opening advice = Left (faultOfKind, unlines $ (opening <> ": " <> headline <> ".") : withReasons advice)
  -- A model that never applied is a different fault from one that applied and
  -- could not be attacked, and the opening line is what a reader sees first.
  lead neverApplied neverTested = case kind of
    PreconditionNeverMet -> neverApplied
    _ -> neverTested
  counts = " on any of the " <> show (tmsTotal summary) <> " generated transactions (" <> skipCounts summary <> ")"
  headline = case kind of
    PreconditionNeverMet -> "the precondition was not met" <> counts
    AttackNeverCarriedOut -> "the precondition held, but the attack could not be carried out" <> counts
    ModelErrored -> "the model errored before it could attack anything" <> counts
  remedy = case kind of
    PreconditionNeverMet -> "Make the positive tests generate transactions it applies to"
    AttackNeverCarriedOut -> "Make the positive tests produce transactions the attack can be built on"
    ModelErrored -> "Fix the error so the model can run"
  -- Never leave the sentence hanging on a colon: a Phase 1 rejection can
  -- carry no error message at all.
  withReasons line
    | null reasons = [line <> "."]
    | otherwise =
        (line <> ":")
          : let (shown, remaining) = splitAt 5 reasons
             in map ("  - " <>) shown
                  <> ["  ... and " <> show (length remaining) <> " more" | not (null remaining)]

{- | Apply the zero-coverage policy and report it: a failure fails the test
case, status lines are reported as steps. The single place that says what
counts as a reason - the model's own errors, plus the ledger's verdicts on
whatever it did manage to submit.
-}
reportZeroCoverage :: (String -> IO ()) -> TMRecorder -> String -> ThreatModelCategory -> ThreatModelSummary -> [(ThreatModelOutcome, [String])] -> IO ()
reportZeroCoverage step recorder key claim summary outcomeEntries =
  either
    (\(fault, msg) -> failWithFault recorder key summary fault (lines msg))
    (mapM_ step)
    $ zeroCoverageVerdict claim summary
    $ modelErrors (map fst outcomeEntries) <> distinctValidationErrors outcomeEntries

{- | Reduce an iteration's trace entries to the outcome and the distinct
error strings they yielded.

Forcing the result to WHNF forces the strings too, which is what lets a
caller that has no further use for the entries drop them: they are the only
thing holding the iteration's transactions and UTxO sets alive.
-}
summarizeThreatModelIteration :: ThreatModelOutcome -> [ThreatModelCheckEntry] -> (ThreatModelOutcome, [String])
summarizeThreatModelIteration outcome entries =
  totalLength `seq` (outcome, msgs)
 where
  msgs = distinctValidationErrorsFromEntries entries
  totalLength = sum (map length msgs)

distinctValidationErrorsFromEntries :: [ThreatModelCheckEntry] -> [String]
distinctValidationErrorsFromEntries entries =
  nubOrd
    [ msg
    | entry <- entries
    , msg <-
        maybe [] errors (tmceValidation entry)
          <> maybe [] (\e -> ["Rebalancing failed: " <> e]) (tmceRebalanceError entry)
    ]

distinctValidationErrors :: [(ThreatModelOutcome, [String])] -> [String]
distinctValidationErrors outcomeEntries =
  nubOrd [msg | (_, msgs) <- outcomeEntries, msg <- msgs]

{- | Generate up to 'maxActions' actions and run them. Stops early when no
action satisfying the precondition can be generated.
-}
runActions
  :: (TestingInterface state, MonadIO m)
  => RunOptions
  -> state
  -> TestingMonadT (PropertyM m) state
runActions opts = go (maxActions opts)
 where
  go 0 s = pure s
  go i s = do
    mAction <- lift $ genAction s
    case mAction of
      Just action -> runAction opts s action >>= go (i - 1)
      Nothing -> pure s

-- | Execute a single action and update the model state
runAction
  :: (TestingInterface state, MonadIO m)
  => RunOptions
  -> state
  -> Action state
  -> TestingMonadT (PropertyM m) state
runAction opts modelState action = do
  when (verbose opts) $
    liftIO $
      putStrLn $
        "Performing: " ++ show action

  -- Check precondition
  unless (precondition modelState action) $
    fail $
      "Precondition failed for action: " ++ show action

  -- Perform the action on the blockchain
  modelState' <- perform modelState action

  -- Validate blockchain state matches model
  valid <- validate modelState'
  unless valid $
    fail "Blockchain state does not match model state"

  lift $ monitor (monitoring modelState' action)

  pure modelState'

{- | Like 'runActions' but accumulates a trace of each transition.
The trace captures the model state before\/after each action and
a summary of the transaction produced. If an action fails (via
@ExceptT@ or @MonadFail@), the monad short-circuits and the
partial trace is lost — use the 'IORef' variant in 'positiveTest'
for partial-failure capture if needed.
-}
runActionsTraced
  :: forall state m
   . (TestingInterface state, MonadIO m)
  => RunOptions
  -> state
  -> TestingMonadT (PropertyM m) (state, [Transition])
runActionsTraced opts initialState = go 0 initialState []
 where
  tagger = redeemerTagger @state
  labeler = addressLabeler @state
  go stepIdx state acc
    | stepIdx >= maxActions opts = pure (state, reverse acc)
    | otherwise = do
        mAction <- lift $ genAction state
        case mAction of
          Nothing -> pure (state, reverse acc)
          Just action -> do
            let stateBefore = toJSON state
                actionText = T.pack (show action)
            -- Snapshot the UTxO and txById map before running the action
            utxoBefore <- fromLedgerUTxO C.shelleyBasedEra <$> getUtxo
            txByIdBefore <- mcsTxById <$> getMockChainState
            -- Run the action (may throw, short-circuiting the monad)
            newState <- runAction opts state action
            -- If we get here, the action succeeded
            mTxSummary <- getLastTxSummary tagger labeler txByIdBefore utxoBefore
            let transition =
                  Transition
                    { trStepIndex = stepIdx
                    , trAction = actionText
                    , trStateBefore = stateBefore
                    , trStateAfter = toJSON newState
                    , trTransaction = mTxSummary
                    , trResult = TransitionSuccess (fromMaybe T.empty (mTxSummary >>= txsId))
                    }
            go (stepIdx + 1) newState (transition : acc)

{- | Check whether a new transaction appeared in the mockchain since
the given snapshot, and if so, return a compact summary.
-}
getLastTxSummary
  :: (MonadMockchain C.ConwayEra m)
  => RedeemerTagger
  -> AddressLabeler
  -> Map.Map C.TxId (C.Tx C.ConwayEra)
  -- ^ @mcsTxById@ snapshot taken before the action
  -> C.UTxO C.ConwayEra
  -- ^ UTxO snapshot taken before the action
  -> m (Maybe TxSummary)
getLastTxSummary tagger labeler txByIdBefore utxoBefore = do
  st <- getMockChainState
  let txByIdAfter = mcsTxById st
      newTxIds = Map.keys (Map.difference txByIdAfter txByIdBefore)
  case newTxIds of
    [] -> pure Nothing
    (txId : _) ->
      case Map.lookup txId txByIdAfter of
        Nothing -> pure Nothing
        Just tx -> pure (Just (summarizeTx tagger labeler tx utxoBefore))

{- | Convert traced threat model results into 'ThreatModelTrace' values
suitable for inclusion in an 'IterationTrace'.

Each 'ThreatModelCheckEntry' (one per 'Validate' call) produces a
'ThreatModelTrace' with the actual modifications, original\/modified
transactions, and outcome.
-}
toThreatModelTraces
  :: (String -> IO (Maybe Int))
  -> RedeemerTagger
  -> AddressLabeler
  -> [(String, ThreatModelCategory, ThreatModelOutcome, [ThreatModelCheckEntry], CoverageData)]
  -> IO [ThreatModelTrace]
toThreatModelTraces findTestId tagger labeler results = concat <$> traverse go results
 where
  go (name, category, outcome, [], covData) = do
    mtestId <- findTestId name
    -- No Validate calls: emit a single lightweight trace with just the outcome
    pure
      [ ThreatModelTrace
          { tmtName = T.pack name
          , tmtCategory = category
          , tmtTestId = testId
          , tmtTargetTxIndex = 0
          , tmtModifications = []
          , tmtOriginalTx = emptyTxSummary
          , tmtModifiedTx = Nothing
          , tmtValidation = Nothing
          , tmtOutcome = outcomeToTrace outcome
          , tmtCovered = covDataToSrcLocRanges covData
          }
      | Just testId <- [mtestId] -- when no test id is found, the test is filtered out and we also don't want to output a trace.
      ]
  go (name, category, outcome, entries, covData) = do
    mtestId <- findTestId name
    -- One ThreatModelTrace per Validate call
    pure
      [ ThreatModelTrace
          { tmtName = T.pack name
          , tmtCategory = category
          , tmtTestId = testId
          , tmtTargetTxIndex = tmceEnvIndex entry
          , tmtModifications = renderModifications (tmceModifications entry)
          , tmtOriginalTx = summarizeTx tagger labeler (tmceOriginalTx entry) (tmceOriginalUtxo entry)
          , tmtModifiedTx = case tmceModifiedTx entry of
              Just tx -> Just (summarizeTx tagger labeler tx (tmceModifiedUtxo entry))
              Nothing -> Nothing
          , tmtValidation = entryValidation entry
          , tmtOutcome = outcomeToTrace outcome
          , tmtCovered = covDataToSrcLocRanges covData
          }
      | entry <- entries
      , Just testId <- [mtestId] -- when no test id is found, the test is filtered out and we also don't want to output a trace.
      ]

  entryValidation entry = case (tmceValidation entry, tmceRebalanceError entry) of
    (Just report, _) ->
      -- Deduplicated like every other rendering of this list
      -- ('distinctValidationErrorsFromEntries', and the counterexample in
      -- 'Convex.ThreatModel'): n inputs locked by the same script report the
      -- same multi-hundred-byte error n times.
      let distinctErrors = map T.pack (nubOrd (errors report))
       in Just $ case validity report of
            Valid -> TMVValid
            Phase1Invalid -> TMVPhase1Invalid distinctErrors
            Phase2Invalid -> TMVPhase2Invalid distinctErrors
    (Nothing, Just err) -> Just (TMVRebalanceFailed (T.pack err))
    -- Not expected: 'runThreatModelCheckTraced' always sets exactly one of the
    -- two. Report the verdict as unknown rather than inventing one.
    (Nothing, Nothing) -> Nothing

  outcomeToTrace TMPassed = TMTOPassed
  outcomeToTrace (TMFailed msg) = TMTOFailed (T.pack msg)
  outcomeToTrace TMSkipped = TMTOSkipped "precondition not met"
  outcomeToTrace TMSkippedPhase1 = TMTOSkippedPhase1 "phase 1 invalidation or rebalancing failure"
  outcomeToTrace (TMError msg) = TMTOError (T.pack msg)

  renderModifications (TxModifier mods) = map (renderTxMod labeler) mods

  emptyTxSummary =
    TxSummary
      { txsId = Nothing
      , txsInputs = []
      , txsOutputs = []
      , txsMint = Nothing
      , txsFee = 0
      , txsSigners = []
      , txsValidRange = Nothing
      , txsWithdrawals = []
      }

{- | Format a 'BalanceTxError' for display in trace output.
For script execution errors, extracts just the error message and
the last non-coverage log entry (typically the user's trace message),
filtering out coverage annotation noise (CoverLocation/CoverBool).
-}
formatBalanceTxError :: BalanceTxError C.ConwayEra -> T.Text
formatBalanceTxError (ABalancingError (ScriptExecutionErr errs)) =
  T.intercalate "; " $ map formatScriptErr errs
 where
  formatScriptErr (_witness, errMsg, logs) =
    let
      -- Filter out coverage annotation log messages
      userLogs = filter (not . isCoverageAnnotation) logs
      -- Show the last user log (most informative) alongside the error
      suffix = case userLogs of
        [] -> ""
        _ -> " | " <> last userLogs
     in
      errMsg <> suffix
  isCoverageAnnotation msg =
    "CoverLocation (" `T.isPrefixOf` msg
      || "CoverBool (" `T.isPrefixOf` msg
formatBalanceTxError err = T.pack (show err)

-- | Initialize the blockchain and validate the model state
runInitialization
  :: forall state m
   . (TestingInterface state, MonadIO m)
  => RunOptions
  -> TestingMonadT (PropertyM m) state
runInitialization opts = do
  initialState <- initialize @state

  when (verbose opts) $
    lift $
      monitor (counterexample $ "Initial state: " ++ show initialState)

  valid <- validate initialState
  unless valid $
    fail "Blockchain state does not match model state after initialization"

  pure initialState

{- | Pass coverage index data to tasty-streaming.

@
main = defaultMainStreaming $ withCoverageIndices [covIdx] tests
@
-}
withCoverageIndices :: [CoverageIndex] -> TestTree -> TestTree
withCoverageIndices idxs = localOption $ CoverageIndexStorage $ covDataToSrcLocRanges $ CoverageData $ mconcat idxs ^. coverageAnnotations

-- | Convert Plutus coverage data to a format suitable for tasty-streaming.
covDataToSrcLocRanges :: CoverageData -> [SrcLocRange]
covDataToSrcLocRanges (CoverageData anns) = catMaybes (map toSrcLocRange $ Set.toList anns)
 where
  toSrcLocRange (CoverLocation (CovLoc f sl el sc ec)) = Just (SrcLocRange (T.pack f) sl sc el ec)
  toSrcLocRange (CoverBool _ _) = Nothing

{- | Configuration for coverage collection and reporting.

Use with 'withCoverage' to set up coverage tracking for your test suite.
-}
data CoverageConfig = CoverageConfig
  { coverageIndices :: [CoverageIndex]
  {- ^ Coverage indices from compiled scripts (obtained via @'PlutusTx.Code.getCovIdx'@).
  Multiple indices are combined with @'<>'@.
  -}
  , coverageReport :: CoverageReport -> IO ()
  {- ^ Action to perform with the final coverage report.
  Use 'printCoverageReport', 'writeCoverageReport', or 'silentCoverageReport'.
  -}
  }

-- | Print a coverage report to stdout using prettyprinter.
printCoverageReport :: CoverageReport -> IO ()
printCoverageReport = print . Pretty.pretty

-- | Write a coverage report to a file.
writeCoverageReport :: FilePath -> CoverageReport -> IO ()
writeCoverageReport fp cr = do
  writeFile fp (show (Pretty.pretty cr))
  printCoveragePath fp

printCoveragePath :: FilePath -> IO ()
printCoveragePath fp = putStrLn $ "Coverage report available at: " <> fp

-- | Collect coverage data but discard the report.
silentCoverageReport :: CoverageReport -> IO ()
silentCoverageReport _ = pure ()

-- | Compact representation of a source location for JSON output.
data JsonCovLoc = JsonCovLoc
  { jclFile :: String
  , jclStartLine :: Int
  , jclStartCol :: Int
  , jclEndLine :: Int
  , jclEndCol :: Int
  }
  deriving (Generic)

instance ToJSON JsonCovLoc where
  toJSON (JsonCovLoc f sl sc el ec) =
    Aeson.object
      [ Key.fromString "file" .= f
      , Key.fromString "startLine" .= sl
      , Key.fromString "startCol" .= sc
      , Key.fromString "endLine" .= el
      , Key.fromString "endCol" .= ec
      ]

-- | Compact representation of a coverage annotation for JSON output.
data JsonAnnotation
  = JsonLocation JsonCovLoc
  | JsonBool JsonCovLoc Bool

instance ToJSON JsonAnnotation where
  toJSON (JsonLocation loc) =
    Aeson.object
      [ Key.fromString "type" .= ("location" :: String)
      , Key.fromString "loc" .= loc
      ]
  toJSON (JsonBool loc b) =
    Aeson.object
      [ Key.fromString "type" .= ("bool" :: String)
      , Key.fromString "loc" .= loc
      , Key.fromString "value" .= b
      ]

-- | A covered annotation with optional function name metadata.
data JsonCovered = JsonCovered
  { jcAnnotation :: JsonAnnotation
  , jcSymbols :: [String]
  }
  deriving (Generic)

instance ToJSON JsonCovered where
  toJSON (JsonCovered ann syms) =
    Aeson.object
      [ Key.fromString "annotation" .= ann
      , Key.fromString "symbols" .= syms
      ]

-- | Minimal coverage summary matching what Pretty.pretty shows.
data CoverageSummary = CoverageSummary
  { csCovered :: [JsonCovered]
  , csUncovered :: [JsonAnnotation]
  , csIgnored :: [JsonAnnotation]
  }
  deriving (Generic)

instance ToJSON CoverageSummary where
  toJSON (CoverageSummary cov uncov ign) =
    Aeson.object
      [ Key.fromString "covered" .= cov
      , Key.fromString "uncovered" .= uncov
      , Key.fromString "ignored" .= ign
      ]

-- | Convert a CovLoc to compact JSON representation.
toJsonCovLoc :: CovLoc -> JsonCovLoc
toJsonCovLoc (CovLoc f sl el sc ec) = JsonCovLoc f sl sc el ec

-- | Convert a CoverageAnnotation to compact JSON representation.
toJsonAnnotation :: CoverageAnnotation -> JsonAnnotation
toJsonAnnotation (CoverLocation loc) = JsonLocation (toJsonCovLoc loc)
toJsonAnnotation (CoverBool loc b) = JsonBool (toJsonCovLoc loc) b

-- | Extract symbol names from Metadata.
extractSymbols :: Set.Set Metadata -> [String]
extractSymbols = foldr go []
 where
  go (ApplicationHeadSymbol s) acc = s : acc
  go IgnoredAnnotation acc = acc

-- | Convert a CoverageReport to a compact summary (same info as Pretty.pretty shows).
coverageSummary :: CoverageReport -> CoverageSummary
coverageSummary (CoverageReport idx covData) =
  CoverageSummary
    { csCovered =
        [ JsonCovered (toJsonAnnotation ann) (extractSymbols $ metadataFor ann)
        | ann <- Set.toList $ allAnns `Set.intersection` coveredAnns'
        ]
    , csUncovered = map toJsonAnnotation . Set.toList $ uncoveredAnns
    , csIgnored = map toJsonAnnotation . Set.toList $ ignoredAnns' Set.\\ coveredAnns'
    }
 where
  allAnns = idx ^. coverageAnnotations
  coveredAnns' = covData ^. coveredAnnotations
  ignoredAnns' = idx ^. ignoredAnnotations
  uncoveredAnns = allAnns Set.\\ (coveredAnns' <> ignoredAnns')
  metadataFor ann = maybe Set.empty _metadataSet $ Map.lookup ann (idx ^. coverageMetadata)

-- | Print a coverage report as compact JSON to stdout.
printCoverageJSON :: CoverageReport -> IO ()
printCoverageJSON = LBS.putStrLn . Aeson.encode . coverageSummary

-- | Write a coverage report as compact JSON to a file.
writeCoverageJSON :: FilePath -> CoverageReport -> IO ()
writeCoverageJSON fp report = do
  LBS.writeFile fp $ Aeson.encode $ coverageSummary report
  printCoveragePath fp

-- | Print a coverage report as pretty-printed JSON to stdout.
printCoverageJSONPretty :: CoverageReport -> IO ()
printCoverageJSONPretty = LBS.putStrLn . Aeson.encodePretty . coverageSummary

-- | Write a coverage report as pretty-printed JSON to a file.
writeCoverageJSONPretty :: FilePath -> CoverageReport -> IO ()
writeCoverageJSONPretty fp report = do
  LBS.writeFile fp $ Aeson.encodePretty $ coverageSummary report
  printCoveragePath fp

{- | Run a test suite with Plutus script coverage collection.

Creates the coverage 'IORef', wires it into 'Options' and 'RunOptions',
runs the user's action, and on exit produces a 'CoverageReport' from the
accumulated data.

The report is generated when the inner action throws an 'ExitCode' exception
(which is how @tasty@'s 'Test.Tasty.defaultMain' signals completion). The
original exception is re-thrown after the report action runs.

@
main :: IO ()
main = withCoverage config $ \\opts runOpts ->
  defaultMain $ testGroup \"my tests\"
    [ testCase \"t1\" (mockchainSucceedsWithOptions opts myTest)
    , myPropertyTests runOpts
    ]
 where
  config = CoverageConfig
    { coverageIndices = [myScriptCovIdx]
    , coverageReport  = printCoverageReport
    }
@
-}
withCoverage
  :: CoverageConfig
  -> (Options C.ConwayEra -> RunOptions -> IO ())
  -> IO ()
withCoverage CoverageConfig{coverageIndices, coverageReport = reportAction} k = do
  ref <- newIORef mempty
  let opts = defaultOptions{coverageRef = Just ref}
      runOpts = defaultRunOptions{mcOptions = opts}
  k opts runOpts
    `catch` \(e :: ExitCode) -> do
      covData <- readIORef ref
      let combinedIdx = mconcat coverageIndices
          report = CoverageReport combinedIdx covData
      -- Don't do anything if there's no coverage data.
      -- Then maybe no tests ran (f.e. with --list-tests-json),
      -- or the coverage has been handled differently (f.e. with --streaming-json).
      unless (covData == mempty) $ reportAction report
      throwIO e

-- | Options for running the testing monad.
data Options era = Options
  { params :: NodeParams era
  , coverageRef :: Maybe (IORef CoverageData)
  }

defaultOptions :: Options C.ConwayEra
defaultOptions =
  Options
    { params = Defaults.nodeParams
    , coverageRef = Nothing
    }

-- | Modify the maximum transaction size in the protocol parameters of the given options
modifyTransactionLimits :: Options C.ConwayEra -> Word32 -> Options C.ConwayEra
modifyTransactionLimits opts@Options{params = Defaults.pParams -> pp} newVal =
  -- TODO: use lenses to make this cleaner
  opts
    { params = (params opts){npProtocolParameters = C.LedgerProtocolParameters $ pp & L.ppMaxTxSizeL .~ newVal}
    }

-- | Run the 'TestingMonadT' action with the given options and fail if there is an error
mockchainSucceedsWithOptions :: Options C.ConwayEra -> TestingMonadT IO a -> Assertion
mockchainSucceedsWithOptions Options{params, coverageRef} action =
  runTestingMonadT params action
    >>= \(res, st) -> do
      let covData = st ^. coverageData
      for_ coverageRef $ \ref -> modifyIORef ref (<> covData)
      case res of
        Right _ -> pure ()
        Left err -> do
          for_ coverageRef $ \ref -> modifyIORef ref (<> coverageFromBalanceTxError err)
          fail $ show err

{- | Run the 'TestingMonadT' action with the given options, fail if it
    succeeds, and handle the error appropriately.
-}
mockchainFailsWithOptions :: Options C.ConwayEra -> TestingMonadT IO a -> (BalanceTxError C.ConwayEra -> Assertion) -> Assertion
mockchainFailsWithOptions Options{params, coverageRef} action handleError =
  runTestingMonadT params action
    >>= \(res, st) -> do
      let covData = st ^. coverageData
      for_ coverageRef $ \ref -> modifyIORef ref (<> covData)
      case res of
        Right _ -> fail "mockchainFailsWithOptions: Did not fail"
        Left err -> do
          for_ coverageRef $ \ref -> modifyIORef ref (<> coverageFromBalanceTxError err)
          handleError err
