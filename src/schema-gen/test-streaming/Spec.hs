module Main (main) where

import Convex.Tasty.Streaming (defaultMainStreaming)
import Test.Tasty (testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- NOTE: This test has no functional purpose for convex-schema-gen (the
-- package generates a JSON schema; it needs no tests). It exists ONLY as a
-- fixture for scripts/list-test-suites.sh: using defaultMainStreaming makes
-- the discovery tool classify this suite as STREAMING, and being in
-- convex-schema-gen places it under the non-default cabal.project.schema-gen
-- to exercise the --project-file + streaming-command path.
main :: IO ()
main =
  defaultMainStreaming $
    testGroup
      "convex-schema-gen-streaming"
      [testCase "dummy" (1 @?= (1 :: Int))]
