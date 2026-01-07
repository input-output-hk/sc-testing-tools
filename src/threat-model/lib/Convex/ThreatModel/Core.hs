{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

-- | Core types and monad for threat model testing
module Convex.ThreatModel.Core (
  -- * Threat context
  ThreatContext (..),
  captureContext,

  -- * Threat results
  ThreatResult (..),
  isThreatDetected,
  isThreatNotDetected,
  isThreatModificationFailed,

  -- * Threat model monad
  ThreatModelT (..),
  runThreatModel,
  recordThreat,
  getContext,
  getOriginalTx,
  getNodeParams,
  getMockchainState,
) where

import Cardano.Api qualified as C
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT (..), ask, asks)
import Control.Monad.State (MonadState, StateT (..), get, modify)
import Convex.Class (MockChainState, MonadMockchain, getMockChainState)
import Convex.NodeParams (NodeParams, askNodeParams)
import Data.Text (Text)

-- | Context for threat model testing, captured from the MockChain at a specific point
data ThreatContext era = ThreatContext
  { tcOriginalTx :: C.Tx era
  -- ^ The original valid transaction to test against
  , tcNodeParams :: NodeParams era
  -- ^ Node parameters for transaction validation
  , tcMockchainState :: MockChainState era
  -- ^ The mockchain state at the time of capture
  }

-- | Result of running a threat model test
data ThreatResult era
  = -- | The threat was properly detected (transaction was rejected)
    ThreatDetected
      { trDescription :: Text
      , trOriginalTx :: C.Tx era
      , trModifiedTx :: C.Tx era
      , trValidationError :: Text
      }
  | -- | The threat was not detected (transaction was accepted - potential vulnerability!)
    ThreatNotDetected
      { trDescription :: Text
      , trOriginalTx :: C.Tx era
      , trModifiedTx :: C.Tx era
      , trExpectedError :: Text
      }
  | -- | Could not build the modified transaction (modification failed)
    ThreatModificationFailed
      { trDescription :: Text
      , trError :: Text
      }
  deriving (Show)

-- | Check if a threat was detected (good outcome)
isThreatDetected :: ThreatResult era -> Bool
isThreatDetected ThreatDetected{} = True
isThreatDetected _ = False

-- | Check if a threat was not detected (bad outcome - potential vulnerability)
isThreatNotDetected :: ThreatResult era -> Bool
isThreatNotDetected ThreatNotDetected{} = True
isThreatNotDetected _ = False

-- | Check if threat modification failed (neutral outcome)
isThreatModificationFailed :: ThreatResult era -> Bool
isThreatModificationFailed ThreatModificationFailed{} = True
isThreatModificationFailed _ = False

-- | Monad for running threat model tests
newtype ThreatModelT era m a = ThreatModelT
  { unThreatModelT :: ReaderT (ThreatContext era) (StateT [ThreatResult era] m) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (ThreatContext era)
    , MonadState [ThreatResult era]
    )

-- | Run a threat model test against a transaction
runThreatModel
  :: (MonadMockchain era m, MonadIO m)
  => C.Tx era
  -- ^ The transaction to test
  -> ThreatModelT era m ()
  -- ^ The threat model test to run
  -> m [ThreatResult era]
runThreatModel tx threatTest = do
  ctx <- captureContext tx
  (_, results) <- runStateT (runReaderT (unThreatModelT threatTest) ctx) []
  pure results

-- | Capture the current MockChain context along with a transaction
captureContext
  :: (MonadMockchain era m)
  => C.Tx era
  -> m (ThreatContext era)
captureContext tx = do
  state <- getMockChainState
  params <- askNodeParams
  pure $
    ThreatContext
      { tcOriginalTx = tx
      , tcNodeParams = params
      , tcMockchainState = state
      }

-- | Record a threat result
recordThreat :: (Monad m) => ThreatResult era -> ThreatModelT era m ()
recordThreat result = modify (result :)

-- | Get the current threat context
getContext :: (Monad m) => ThreatModelT era m (ThreatContext era)
getContext = ask

-- | Get the original transaction being tested
getOriginalTx :: (Monad m) => ThreatModelT era m (C.Tx era)
getOriginalTx = asks tcOriginalTx

-- | Get the node parameters
getNodeParams :: (Monad m) => ThreatModelT era m (NodeParams era)
getNodeParams = asks tcNodeParams

-- | Get the mockchain state
getMockchainState :: (Monad m) => ThreatModelT era m (MockChainState era)
getMockchainState = asks tcMockchainState
