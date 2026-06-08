{- | Drop-in shim for "Test.Tasty.QuickCheck" that captures the source
location of each 'testProperty' definition and propagates it to the
streaming reporter.

Migration is a single-line import change:

@
-- before
import Test.Tasty.QuickCheck (testProperty)

-- after
import Convex.Tasty.QuickCheck (testProperty)
@

Call sites remain byte-for-byte identical. Re-exports everything from
"Test.Tasty.QuickCheck" except 'QC.testProperty', which is replaced by
the location-tracking shim defined here.
-}
module Convex.Tasty.QuickCheck (
  testProperty,
  module Test.Tasty.QuickCheck,
) where

import Convex.Tasty.Streaming.QCStats (QCStatsRecorder (..), recordQCStatsFromState)
import Convex.Tasty.Streaming.SrcLoc (SrcLocOpt (..), withSrcLoc)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Test.QuickCheck.Property qualified as QCP
import Test.Tasty (TestName, TestTree, askOption)
import Test.Tasty.QuickCheck hiding (testProperty)
import Test.Tasty.QuickCheck qualified as QC

{- | Like 'QC.testProperty' but captures the call site as a source-location
range that the streaming ingredient will emit alongside the test.
-}
testProperty :: (HasCallStack, Testable a) => TestName -> a -> TestTree
testProperty name prop =
  withFrozenCallStack
    ( withSrcLoc $
        let baseProp = property prop
         in askOption $ \(SrcLocOpt mLoc) ->
              case mLoc of
                Nothing -> QC.testProperty name baseProp
                Just _ ->
                  askOption $ \(recorder :: QCStatsRecorder) ->
                    let postTest =
                          QCP.callback $ QCP.PostTest QCP.NotCounterexample $ \st _ ->
                            recordQCStatsFromState recorder mLoc name st
                        postFinalFailure =
                          QCP.callback $ QCP.PostFinalFailure QCP.NotCounterexample $ \st _ ->
                            recordQCStatsFromState recorder mLoc name st
                        instrumented = postFinalFailure (postTest baseProp)
                     in QC.testProperty name instrumented
    )
