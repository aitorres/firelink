module SpecialTypesSpec where

import Test.Hspec
import Lexer
import Tokens
import Utils (getAbstractToken)

spec :: Spec
spec = describe "Lexer" $ do
  it "accepts `abyss` as a valid token for comments" $ do
    let x = "abyss"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkNull

  it "accepts `arrow to` as a valid token for comments" $ do
    let x = "arrow to"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkPointer

  it "accepts `knight` as a valid token for comments" $ do
    let x = "knight"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkAlias

  it "accepts `requiring help of` as a valid token for comments" $ do
    let x = "requiring help of"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkAliasListBegin

  it "accepts `help received` as a valid token for comments" $ do
    let x = "help received"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkAliasListEnd
