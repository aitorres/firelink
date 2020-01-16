module IfInstructionSpec where

import Test.Hspec
import qualified TestUtils as U

spec :: Spec
spec = describe "hola" $
    it "should" $ 1 `shouldBe` 1
