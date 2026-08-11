module Main (main) where

import Convex.Tasty.Streaming (defaultMainStreaming)
import Tests qualified

-- NOTE: This test has no functional purpose for convex-schema-gen (the
-- package generates a JSON schema; it needs no tests). It exists ONLY as a
-- fixture for scripts/list-test-suites/list-test-suites.sh: using defaultMainStreaming makes
-- the discovery tool classify this suite as STREAMING, and being in
-- convex-schema-gen places it under the non-default cabal.project.schema-gen
-- to exercise the --project-file + streaming-command path.
-- This test is used to verify that tasty-streaming works together with tasty-discover,
-- and that the discovered tests can be run with defaultMainStreaming.
main :: IO ()
main = do
  -- Get discovered tests and ingredients
  discoveredTests <- Tests.tests

  -- Run with custom configuration
  -- Note that you shouldn't use defaultMainStreamingWithIngredients with Tests.ingredients here.
  -- Since that would put the default ingredients in front of the streaming ingredients, which would break the streaming behavior.
  defaultMainStreaming discoveredTests
