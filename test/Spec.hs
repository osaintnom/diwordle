-- test/Spec.hs
module Main (main) where

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Sample Test" $ do
    it "verifies that True is True" $ do
      True `shouldBe` True
