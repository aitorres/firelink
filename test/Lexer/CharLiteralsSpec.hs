module CharLiteralsSpec where

import           FireLink.FrontEnd.Tokens
import           Test.Hspec
import           Utils                    (scanToken)

spec :: Spec
spec = describe "Lexer" $ do

  it "accepts `|a|` as a valid char literal" $ do
    let x = "|a|"
    let atok = scanToken x
    atok `shouldBe` TkCharLit

  it "accepts `|b|` as a valid char literal" $ do
    let x = "|b|"
    let atok = scanToken x
    atok `shouldBe` TkCharLit

  it "accepts `|\0|` as a valid char literal" $ do
    let x = "|\0|"
    let atok = scanToken x
    atok `shouldBe` TkCharLit

  it "accepts `|\t|` as a valid char literal" $ do
    let x = "|\t|"
    let atok = scanToken x
    atok `shouldBe` TkCharLit

  it "accepts `|\\n|` as a valid char literal" $ do
    let x = "|\\n|"
    let atok = scanToken x
    atok `shouldBe` TkCharLit
