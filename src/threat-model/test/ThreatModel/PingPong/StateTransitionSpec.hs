{-# LANGUAGE OverloadedStrings #-}

module ThreatModel.PingPong.StateTransitionSpec (spec) where

import Test.Hspec (Spec, describe, it, pendingWith)

spec :: Spec
spec = describe "PingPong State Transition Threats" $ do
  it "rejects invalid Pinged->Pinged transition" $ do
    pendingWith "To be implemented after fixing compilation issues"

  it "rejects transitions from terminal Stopped state" $ do
    pendingWith "To be implemented after fixing compilation issues"
