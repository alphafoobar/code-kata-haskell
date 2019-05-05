module Kata.BinarySearch.SearchSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Kata.BinarySearch.Search

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "search" $ do
    it "returns -1 if it can't find the element in an empty list" $ do
      1 `chop` [] `shouldBe` -1
