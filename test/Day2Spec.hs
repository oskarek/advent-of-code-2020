module Day2Spec (spec) where

import qualified Day2.Day2 as Day2
import Day2.Types
import Test.Hspec (Spec, describe, it, shouldBe)

input :: [(Constraint, Password)]
input =
  [ (Constraint 1 3 'a', "abcde"),
    (Constraint 1 3 'b', "cdefg"),
    (Constraint 2 9 'c', "ccccccccc")
  ]

spec :: Spec
spec = do
  describe "isValid1" $ do
    it "handles the example input" $ do
      Day2.isValid1 <$> input `shouldBe` [True, False, True]

  describe "isValid2" $ do
    it "handles the example input" $ do
      Day2.isValid2 <$> input `shouldBe` [True, False, False]
