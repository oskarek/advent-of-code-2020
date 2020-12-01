module Day1Spec (spec) where

import qualified Day1.Day1 as Day1
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "solveN" $ do
    it "handles the example input" $ do
      let input = [1721, 979, 366, 299, 675, 1456] :: [Int]
      Day1.solveN 2 input `shouldBe` Just [1721, 299]
      Day1.solveN 3 input `shouldBe` Just [979, 366, 675]
