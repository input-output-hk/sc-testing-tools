{-# LANGUAGE OverloadedStrings #-}

module PbtCli.GlobSpec (globTests) where

import PbtCli.Glob (expandGlob, isGlob, matchSegment)
import PbtCli.TestUtils (withTempTree, writeFileIn)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

globTests :: TestTree
globTests =
  testGroup
    "Glob"
    [ testGroup
        "isGlob"
        [ testCase "plain path is not a glob" $ isGlob "src/foo" @?= False
        , testCase "star is a glob" $ isGlob "*/*.cabal" @?= True
        , testCase "question mark is a glob" $ isGlob "pkg?" @?= True
        ]
    , testGroup
        "matchSegment"
        [ testCase "literal matches itself" $ matchSegment "foo.cabal" "foo.cabal" @?= True
        , testCase "literal rejects another name" $ matchSegment "foo.cabal" "bar.cabal" @?= False
        , testCase "star matches a suffix" $ matchSegment "*.cabal" "convex-base.cabal" @?= True
        , testCase "star respects the extension" $ matchSegment "*.cabal" "notes.md" @?= False
        , testCase "bare star matches any visible name" $ matchSegment "*" "anything" @?= True
        , -- Shell globs hide dotfiles and cabal.project globs inherit that, so
          -- `packages: */*.cabal` must not reach into .hidden/.
          testCase "star does not match a leading dot" $ matchSegment "*" ".hidden" @?= False
        , testCase "an explicit dot does match a dotfile" $ matchSegment ".*" ".hidden" @?= True
        , testCase "question mark matches exactly one char" $ matchSegment "pkg?" "pkg1" @?= True
        , testCase "question mark requires a char" $ matchSegment "pkg?" "pkg" @?= False
        ]
    , testCase "expandGlob resolves a two-level pattern" $
        withTempTree "glob-two-level" $ \root -> do
          writeFileIn root ("a" </> "one.cabal") ""
          writeFileIn root ("b" </> "two.cabal") ""
          writeFileIn root ("b" </> "notes.md") ""
          found <- expandGlob root "*/*.cabal"
          found @?= [root </> "a" </> "one.cabal", root </> "b" </> "two.cabal"]
    , testCase "expandGlob on a literal path checks existence" $
        withTempTree "glob-literal" $ \root -> do
          writeFileIn root "here.cabal" ""
          present <- expandGlob root "here.cabal"
          absent <- expandGlob root "gone.cabal"
          (present, absent) @?= ([root </> "here.cabal"], [])
    ]
