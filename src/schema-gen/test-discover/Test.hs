module Test where

import Test.Tasty.HUnit (testCase, (@?=))

unit_dummy :: IO ()
unit_dummy = 1 @?= (1 :: Int)
