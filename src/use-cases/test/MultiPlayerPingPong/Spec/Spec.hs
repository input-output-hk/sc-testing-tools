{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cardano.Api qualified as C
import Convex.Tasty.Streaming (defaultMainStreaming)
import Convex.TestingInterface (Options, RunOptions (mcOptions), defaultOptions, defaultRunOptions, modifyTransactionLimits)
import MultiPlayerPingPong.Scripts (multiPlayerPingPongCovIdx)
import MultiPlayerPingPong.Spec.Prop (propBasedTests)
import MultiPlayerPingPong.Spec.Unit (unitTests)
import Test.Tasty (TestTree, testGroup)

--------------------------------------------------------------------------------
-- Main Test Entry Point
--------------------------------------------------------------------------------

{- | Whether the validator was compiled with coverage annotations.

The coverage index is empty unless the PlutusTx plugin ran with
@coverage-all@, so reading it off the compiled code stays correct however
coverage was switched on or off: the @ghc-options@ in the cabal file, a
@-fplugin-opt@ passed to the build, or a pragma in the script module.
-}
coverageEnabled :: Bool
coverageEnabled = multiPlayerPingPongCovIdx /= mempty

{- | Mockchain options for the suite.

Coverage annotations push the transactions well past the 16384 byte default
max tx size, so raise the limit when they are present. 80000 is not
arbitrary: at 64000 the threat model attack transactions start being rejected
on size before they reach the validator, which is not what those tests are
measuring.

Without coverage the mainnet default stays in place, so the suite still
catches transaction size regressions.
-}
mockchainOptions :: Options C.ConwayEra
mockchainOptions
  | coverageEnabled = modifyTransactionLimits defaultOptions 80_000
  | otherwise = defaultOptions

main :: IO ()
main = defaultMainStreaming tests

tests :: TestTree
tests =
  testGroup
    "multi-player ping-pong tests"
    [ unitTests mockchainOptions
    , propBasedTests defaultRunOptions{mcOptions = mockchainOptions}
    ]
