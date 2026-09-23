{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Convex.Tasty.Streaming (defaultMainStreaming)
import PbtCli.DiscoverSpec (discoverTests)
import PbtCli.EventsSpec (eventsTests)
import PbtCli.GlobSpec (globTests)
import PbtCli.ReferenceSpec (referenceTests)
import PbtCli.RunSpec (runTests)
import Test.Tasty (TestTree, testGroup)

main :: IO ()
main = defaultMainStreaming tests

tests :: TestTree
tests =
  testGroup
    "pbt-cli"
    [ globTests
    , discoverTests
    , eventsTests
    , runTests
    , referenceTests
    ]
