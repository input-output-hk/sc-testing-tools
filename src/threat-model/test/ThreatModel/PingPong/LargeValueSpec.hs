{-# LANGUAGE OverloadedStrings #-}

module ThreatModel.PingPong.LargeValueSpec (spec) where

import Test.Hspec (Spec, describe, it, pendingWith)

spec :: Spec
spec = describe "PingPong Large Value Threats" $ do
  it "handles value inflation appropriately" $ do
    pendingWith "To be implemented after fixing compilation issues"
