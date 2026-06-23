module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main =
  defaultMain $
    testGroup
      "convex-schema-gen-plain"
      [testCase "dummy" (1 @?= (1 :: Int))]
