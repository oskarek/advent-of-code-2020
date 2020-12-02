module AdventPrelude.ListSpec (spec) where

import AdventPrelude.List (occurrences, subsetsOfSize)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "subsetsOfSize" $ do
    it "handles sample input" $ do
      subsetsOfSize 5 ([] :: [Int]) `shouldBe` []
      subsetsOfSize 0 ([1, 2, 3] :: [Int]) `shouldBe` [[]]
      subsetsOfSize 2 ([1, 2, 3, 4] :: [Int])
        `shouldBe` [[1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4]]

  describe "occurrences" $ do
    it "handles sample input" $ do
      occurrences 2 ([] :: [Int]) `shouldBe` 0
      occurrences 4 ([1, 4, 6, 62, 15, 4, 4, 0] :: [Int]) `shouldBe` 3
      occurrences 2 ([1, 4, 6, 6, 5, 4, 40, 0] :: [Int]) `shouldBe` 0