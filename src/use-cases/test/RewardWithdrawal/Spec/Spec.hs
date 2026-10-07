{-# LANGUAGE NumericUnderscores #-}

import Cardano.Api qualified as C
import Convex.Tasty.Streaming (defaultMainStreaming)
import Convex.TestingInterface (Options, RunOptions (mcOptions), defaultOptions, defaultRunOptions, modifyTransactionLimits, withCoverageIndices)
import RewardWithdrawal.Scripts (rewardWithdrawalCovIdx)
import RewardWithdrawal.Spec.Prop qualified
import RewardWithdrawal.Spec.Unit qualified
import Test.Tasty (TestTree, testGroup)

{- | Whether the validator was compiled with coverage annotations.

The coverage index is empty unless the PlutusTx plugin ran with
@coverage-all@, so reading it off the compiled code stays correct however
coverage was switched on or off.
-}
coverageEnabled :: Bool
coverageEnabled = rewardWithdrawalCovIdx /= mempty

{- | Mockchain options for the suite.

Coverage annotations push the transactions past the 16384 byte default max
tx size, so raise the limit when they are present, as MultiPlayerPingPong
does. Without coverage the mainnet default stays in place, so the suite still
catches transaction size regressions.
-}
mockchainOptions :: Options C.ConwayEra
mockchainOptions
  | coverageEnabled = modifyTransactionLimits defaultOptions 80_000
  | otherwise = defaultOptions

main :: IO ()
main =
  defaultMainStreaming $
    withCoverageIndices [rewardWithdrawalCovIdx] (tests defaultRunOptions{mcOptions = mockchainOptions})

tests :: RunOptions -> TestTree
tests runOpts =
  testGroup
    "reward withdrawal tests"
    [ RewardWithdrawal.Spec.Unit.unitTests (mcOptions runOpts)
    , RewardWithdrawal.Spec.Prop.propBasedTests runOpts
    ]
