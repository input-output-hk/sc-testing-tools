import Convex.Tasty.Streaming (defaultMainStreaming)
import Convex.TestingInterface (RunOptions, defaultRunOptions)
import RewardWithdrawal.Spec.Prop qualified
import RewardWithdrawal.Spec.Unit qualified
import Test.Tasty (TestTree, testGroup)

main :: IO ()
main = defaultMainStreaming (tests defaultRunOptions)

tests :: RunOptions -> TestTree
tests runOpts =
  testGroup
    "reward withdrawal tests"
    [ RewardWithdrawal.Spec.Unit.unitTests
    , RewardWithdrawal.Spec.Prop.propBasedTests runOpts
    ]
