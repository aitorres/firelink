module SpecialTypesSpec where

import Test.Hspec
import Lexer

spec :: Spec
spec = describe "Lexer" $ do
  it "accepts `abyss` as a valid token for comments" $ do
    let x = "abyss"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkAbyss
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `arrow to` as a valid token for comments" $ do
    let x = "arrow to"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkArrowTo
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `knight` as a valid token for comments" $ do
    let x = "knight"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkKnight
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `requiring help of` as a valid token for comments" $ do
    let x = "requiring help of"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkAliasesListBegin
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `help received` as a valid token for comments" $ do
    let x = "help received"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkAliasesListEnd
      Nothing ->
        error "rejected as an invalid token"
