module ScalarTypesSpec where

import           Lexer
import           Test.Hspec
import           Tokens
import           Utils      (getAbstractToken)

spec :: Spec
spec = describe "Lexer" $ do
  -- Integers
  it "accepts `humanity` as a valid token" $ do
    let x = "humanity"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkBigInt

  it "accepts `big humanity` as a valid token" $ do
    let x = "big humanity"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkBigInt

  it "accepts `small humanity` as a valid token" $ do
    let x = "small humanity"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkSmallInt

  -- Tri-booleans
  it "accepts `bonfire` as a valid token" $ do
    let x = "bonfire"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkBool

  it "accepts `lit` as a valid token" $ do
    let x = "lit"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkLit

  it "accepts `unlit` as a valid token" $ do
    let x = "unlit"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkUnlit

  it "accepts `undiscovered` as a valid token" $ do
    let x = "undiscovered"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkUndiscovered

  -- Hollow
  it "accepts `hollow` as a valid token" $ do
    let x = "hollow"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkFloat

  -- Sign
  it "accepts `sign` as a valid token" $ do
    let x = "sign"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkChar

  it "accepts `ascii_of` as a valid token" $ do
    let x = "ascii_of"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkAsciiOf
