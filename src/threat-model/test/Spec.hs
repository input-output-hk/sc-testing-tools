{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec (describe, hspec, it, pendingWith)

main :: IO ()
main = hspec $ do
  describe "Threat Model Tests" $ do
    it "placeholder test" $ do
      pendingWith "Threat model tests to be implemented"
