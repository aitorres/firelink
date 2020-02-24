module ScalarTypesSpec where

import           FireLink.FrontEnd.Tokens
import           Test.Hspec
import           Utils                    (scanToken)

spec :: Spec
spec = describe "Lexer" $ do
  -- Integers
  it "accepts `humanity` as a valid token" $ do
    let x = "humanity"
    let atok = scanToken x
    atok `shouldBe` TkBigInt

  it "accepts `big humanity` as a valid token" $ do
    let x = "big humanity"
    let atok = scanToken x
    atok `shouldBe` TkBigInt

  it "accepts `small humanity` as a valid token" $ do
    let x = "small humanity"
    let atok = scanToken x
    atok `shouldBe` TkSmallInt

  -- Tri-booleans
  it "accepts `bonfire` as a valid token" $ do
    let x = "bonfire"
    let atok = scanToken x
    atok `shouldBe` TkBool

  it "accepts `lit` as a valid token" $ do
    let x = "lit"
    let atok = scanToken x
    atok `shouldBe` TkLit

  it "accepts `unlit` as a valid token" $ do
    let x = "unlit"
    let atok = scanToken x
    atok `shouldBe` TkUnlit

  it "accepts `undiscovered` as a valid token" $ do
    let x = "undiscovered"
    let atok = scanToken x
    atok `shouldBe` TkUndiscovered

  -- Hollow
  it "accepts `hollow` as a valid token" $ do
    let x = "hollow"
    let atok = scanToken x
    atok `shouldBe` TkFloat

  -- Sign
  it "accepts `sign` as a valid token" $ do
    let x = "sign"
    let atok = scanToken x
    atok `shouldBe` TkChar

  it "accepts `ascii_of` as a valid token" $ do
    let x = "ascii_of"
    let atok = scanToken x
    atok `shouldBe` TkAsciiOf
