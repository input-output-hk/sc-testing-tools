module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- NOTE: This test has no functional purpose for convex-schema-gen. It exists
-- ONLY as a fixture for scripts/list-test-suites.sh: using upstream
-- defaultMain makes the discovery tool classify this suite as `upstream`
-- (no streaming commands) -- the plain counterpart to the test-streaming
-- fixture.
main :: IO ()
main =
  defaultMain $
    testGroup
      "convex-schema-gen-plain"
      [testCase "dummy" (1 @?= (1 :: Int))]
