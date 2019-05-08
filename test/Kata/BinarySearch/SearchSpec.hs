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
  describe "search empty list" $ do
    it "`Nothing` if it can't find the element in an empty list" $ do
      1 `chop` [] `shouldBe` Nothing
  describe "search list of 1" $ do
    it "`Nothing` if it can't find the element" $ do
      0 `chop` [1] `shouldBe` Nothing
    it "`Just 0` if it finds the element in the first element" $ do
      1 `chop` [1] `shouldBe` Just 0
  describe "search list of 2" $ do
    it "`Just 0` if it finds the element in the first element" $ do
      1 `chop` [1, 3] `shouldBe` Just 0
    it "`Just 1` if it finds the element in the next element" $ do
      3 `chop` [1, 3] `shouldBe` Just 1
  describe "search list of 2 and NOT found" $ do
    it "`Nothing` if it can't find the element 0" $ do
      0 `chop` [1, 3] `shouldBe` Nothing
    it "`Nothing` if it can't find the element 2" $ do
      2 `chop` [1, 3] `shouldBe` Nothing
    it "`Nothing` if it can't find the element 4" $ do
      4 `chop` [1, 3] `shouldBe` Nothing
  describe "search list of 3 and found" $ do
    it "`Just 0` if it can find the element" $ do
      1 `chop` [1, 3, 5] `shouldBe` Just 0
    it "`Just 1` if it can find the element" $ do
      3 `chop` [1, 3, 5] `shouldBe` Just 1
    it "`Just 2` if it can find the element" $ do
      5 `chop` [1, 3, 5] `shouldBe` Just 2
  describe "search list of 3 and NOT found" $ do
    it "`Nothing` if it can't find the element 0" $ do
      0 `chop` [1, 3, 5] `shouldBe` Nothing
    it "`Nothing` if it can't find the element 2" $ do
      2 `chop` [1, 3, 5] `shouldBe` Nothing
    it "`Nothing` if it can't find the element 4" $ do
      4 `chop` [1, 3, 5] `shouldBe` Nothing
    it "`Nothing` if it can't find the element 6" $ do
      6 `chop` [1, 3, 5] `shouldBe` Nothing
  describe "search list of 4 and found" $ do
    it "`Just 0` if it can find the element" $ do
      1 `chop` [1, 3, 5, 7] `shouldBe` Just 0
    it "`Just 1` if it can find the element" $ do
      3 `chop` [1, 3, 5, 7] `shouldBe` Just 1
    it "`Just 2` if it can find the element" $ do
      5 `chop` [1, 3, 5, 7] `shouldBe` Just 2
  describe "search list of 3 and NOT found" $ do
    it "`Nothing` if it can't find the element 0" $ do
      0 `chop` [1, 3, 5, 7] `shouldBe` Nothing
    it "`Nothing` if it can't find the element 2" $ do
      2 `chop` [1, 3, 5, 7] `shouldBe` Nothing
    it "`Nothing` if it can't find the element 4" $ do
      4 `chop` [1, 3, 5, 7] `shouldBe` Nothing
    it "`Nothing` if it can't find the element 6" $ do
      6 `chop` [1, 3, 5, 7] `shouldBe` Nothing
    it "`Nothing` if it can't find the element 8" $ do
      8 `chop` [1, 3, 5, 7] `shouldBe` Nothing
