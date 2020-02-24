module IntLiteralsSpec where

import           FireLink.FrontEnd.Tokens
import           Test.Hspec
import           Utils                    (scanToken)

spec :: Spec
spec = describe "Lexer" $ do

  it "accepts `0` as a valid int literal" $ do
    let x = "0"
    let atok = scanToken x
    atok `shouldBe` TkIntLit

  it "accepts `1` as a valid int literal" $ do
    let x = "1"
    let atok = scanToken x
    atok `shouldBe` TkIntLit

  it "accepts `100` as a valid int literal" $ do
    let x = "100"
    let atok = scanToken x
    atok `shouldBe` TkIntLit
