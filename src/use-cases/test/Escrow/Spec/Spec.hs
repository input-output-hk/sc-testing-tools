{-# LANGUAGE OverloadedStrings #-}

module Main where

import Convex.Tasty.Streaming (defaultMainStreaming)
import Escrow.Spec.Prop (propBasedTests)
import Escrow.Spec.Unit (unitTests)
import Test.Tasty (TestTree, testGroup)

--------------------------------------------------------------------------------
-- Main Test Entry Point
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMainStreaming tests

tests :: TestTree
tests =
  testGroup
    "escrow tests"
    [ unitTests
    , propBasedTests
    ]
