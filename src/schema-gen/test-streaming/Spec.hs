module Main (main) where

import Convex.Tasty.Streaming (defaultMainStreaming)
import Test.Tasty (testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main =
  defaultMainStreaming $
    testGroup
      "convex-schema-gen-streaming"
      [testCase "dummy" (1 @?= (1 :: Int))]
