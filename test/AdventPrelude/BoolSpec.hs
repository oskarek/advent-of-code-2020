module AdventPrelude.BoolSpec (spec) where

import AdventPrelude.Bool (xor)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "xor" $ do
    it "is correct" $ do
      True `xor` True `shouldBe` False
      True `xor` False `shouldBe` True
      False `xor` True `shouldBe` True
      False `xor` False `shouldBe` False