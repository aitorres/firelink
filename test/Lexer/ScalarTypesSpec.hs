module Lexer.ScalarTypesSpec where

import Test.Hspec
import Lexer
import Lexer.Utils (getAbstractToken)

spec :: Spec
spec = describe "Lexer" $ do
  -- Integers
  it "accepts `humanity` as a valid token" $ do
    let x = "humanity"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkBigInt
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `big humanity` as a valid token" $ do
    let x = "big humanity"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkBigInt
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `small humanity` as a valid token" $ do
    let x = "small humanity"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkSmallInt
      Nothing ->
        error "rejected as an invalid token"

  -- Tri-booleans
  it "accepts `bonfire` as a valid token" $ do
    let x = "bonfire"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkBool
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `lit` as a valid token" $ do
    let x = "lit"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkLit
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `unlit` as a valid token" $ do
    let x = "unlit"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkUnlit
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `undiscovered` as a valid token" $ do
    let x = "undiscovered"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkUndiscovered
      Nothing ->
        error "rejected as an invalid token"

  -- Hollow
  it "accepts `hollow` as a valid token" $ do
    let x = "hollow"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkFloat
      Nothing ->
        error "rejected as an invalid token"

  -- Sign
  it "accepts `sign` as a valid token" $ do
    let x = "sign"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkChar
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `ascii_of` as a valid token" $ do
    let x = "ascii_of"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkAsciiOf
      Nothing ->
        error "rejected as an invalid token"