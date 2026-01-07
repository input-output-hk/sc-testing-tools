{-# LANGUAGE OverloadedStrings #-}

module ThreatModel.PingPong.LargeDatumSpec (spec) where

import Test.Hspec (Spec, describe, it, pendingWith)

spec :: Spec
spec = describe "PingPong Large Datum Threats" $ do
  it "handles large datums appropriately" $ do
    pendingWith "To be implemented after fixing compilation issues"
