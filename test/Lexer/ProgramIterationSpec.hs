module ProgramIterationSpec where

import           FireLink.FrontEnd.Tokens
import           Test.Hspec
import           Utils                    (scanToken)

spec :: Spec
spec = describe "Lexer" $ do
  it "accepts `upgrading` as a valid token" $ do
    let x = "upgrading"
    let atok = scanToken x
    atok `shouldBe` TkFor

  it "accepts `with` as a valid token" $ do
    let x = "with"
    let atok = scanToken x
    atok `shouldBe` TkWith

  it "accepts `soul` as a valid token" $ do
    let x = "soul"
    let atok = scanToken x
    atok `shouldBe` TkSoul

  it "accepts `souls` as a valid token" $ do
    let x = "souls"
    let atok = scanToken x
    atok `shouldBe` TkSoul

  it "accepts `until level` as a valid token" $ do
    let x = "until level"
    let atok = scanToken x
    atok `shouldBe` TkLevel

  it "accepts `max level reached` as a valid token" $ do
    let x = "max level reached"
    let atok = scanToken x
    atok `shouldBe` TkEndFor

  it "accepts `repairing` as a valid token" $ do
    let x = "repairing"
    let atok = scanToken x
    atok `shouldBe` TkForEach

  it "accepts `with titanite from` as a valid token" $ do
    let x = "with titanite from"
    let atok = scanToken x
    atok `shouldBe` TkWithTitaniteFrom

  it "accepts `weaponry repaired` as a valid token" $ do
    let x = "weaponry repaired"
    let atok = scanToken x
    atok `shouldBe` TkEndForEach

  it "accepts `while the` as a valid token" $ do
    let x = "while the"
    let atok = scanToken x
    atok `shouldBe` TkWhile

  it "accepts `covenant is active` as a valid token" $ do
    let x = "covenant is active"
    let atok = scanToken x
    atok `shouldBe` TkCovenantIsActive

  it "accepts `covenant left` as a valid token" $ do
    let x = "covenant left"
    let atok = scanToken x
    atok `shouldBe` TkEndWhile
