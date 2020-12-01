module Misc.ListFunctionsSpec (spec) where

import Misc.ListFunctions (subsetsOfSize)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "subsetsOfSize" $ do
    it "handles sample input" $ do
      subsetsOfSize 5 ([] :: [Int]) `shouldBe` []
      subsetsOfSize 0 ([1, 2, 3] :: [Int]) `shouldBe` [[]]
      subsetsOfSize 2 ([1, 2, 3, 4] :: [Int])
        `shouldBe` [[1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4]]
