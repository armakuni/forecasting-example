module ExampleSpec where

import Test.Hspec

spec :: Spec
spec = describe "example" $
    it "should work" $
        1 `shouldBe` (1 :: Int)