module RewardWithdrawal.Spec.Unit where

import Cardano.Api qualified as C
import Control.Monad.Except (MonadError)
import Convex.Class (MonadMockchain)
import Convex.CoinSelection (BalanceTxError)
import Convex.MockChain.Utils (mockchainFails, mockchainSucceeds)
import Convex.Tasty.HUnit (testCase)
import Convex.Utils (failOnError)
import Convex.Wallet (Wallet)
import Convex.Wallet.MockWallet qualified as MockWallet
import RewardWithdrawal.Spec.Common (fixedOwner, registerCredential, withdrawZero)
import Test.Tasty (TestTree, testGroup)

-------------------------------------------------------------------------------
-- Unit tests for the RewardWithdrawal script
--
-- These exercise the "withdraw zero trick": a script attached to a stake
-- credential is triggered by a zero-lovelace withdrawal, purely to run its
-- validation logic against the whole transaction, without spending any
-- script UTxO. The lock scenarios show the trick doing useful work: the
-- rewarding script vets outputs paid to its own payment address.
-------------------------------------------------------------------------------

unitTests :: TestTree
unitTests =
  testGroup
    "unit tests"
    [ testCase
        "owner triggers the reward-withdrawal script via a zero-lovelace withdrawal"
        (mockchainSucceeds $ failOnError ownerWithdrawsZero)
    , testCase
        "Fail: zero-lovelace withdrawal without the owner's signature"
        (mockchainFails (failOnError missingOwnerSignature) (\_ -> pure ()))
    , testCase
        "owner locks ADA at the script address under a matching lock datum"
        (mockchainSucceeds $ failOnError (ownerLocks 10_000_000 10_000_000))
    , testCase
        "Fail: lock datum amount does not match the output's value"
        (mockchainFails (failOnError (ownerLocks 10_000_000 9_000_000)) (\_ -> pure ()))
    ]

{- | The shared scenario: the owner's script-guarded stake credential is
registered, then a zero-lovelace withdrawal triggering the script is signed
and paid for by @signer@, optionally also locking lovelace at the script's
own payment address.
-}
withdrawZeroScenario
  :: (MonadMockchain C.ConwayEra m, MonadFail m, MonadError (BalanceTxError C.ConwayEra) m)
  => Wallet
  -> Maybe (Integer, Integer)
  -> m ()
withdrawZeroScenario signer lock = do
  registerCredential
  withdrawZero signer lock

-- | The owner triggers the script. It only checks 'txSignedBy', so it accepts.
ownerWithdrawsZero
  :: (MonadMockchain C.ConwayEra m, MonadFail m, MonadError (BalanceTxError C.ConwayEra) m)
  => m ()
ownerWithdrawsZero = withdrawZeroScenario fixedOwner Nothing

{- | The owner's withdrawal also pays @lockedAmount@ to the script's own
payment address under a @LockDatum@ claiming @datumAmount@. The validator
accepts only when the two agree.
-}
ownerLocks
  :: (MonadMockchain C.ConwayEra m, MonadFail m, MonadError (BalanceTxError C.ConwayEra) m)
  => Integer
  -> Integer
  -> m ()
ownerLocks lockedAmount datumAmount = withdrawZeroScenario fixedOwner (Just (lockedAmount, datumAmount))

{- | The withdrawal is signed only by an outsider, not by the owner named in
the script's parameters. 'txSignedBy' fails and the validator rejects with
"OSM".
-}
missingOwnerSignature
  :: (MonadMockchain C.ConwayEra m, MonadFail m, MonadError (BalanceTxError C.ConwayEra) m)
  => m ()
missingOwnerSignature = withdrawZeroScenario MockWallet.w2 Nothing
