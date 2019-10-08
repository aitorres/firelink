module Lexer.IdSpec where

import Test.Hspec
import Lexer
import Lexer.Utils (getAbstractToken)

spec :: Spec
spec = describe "Lexer" $ do
  it "accepts `a` as a valid id" $ do
    let x = "a"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkId
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `b` as a valid id" $ do
    let x = "b"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkId
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `testy` as a valid id" $ do
    let x = "testy"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkId
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `test123` as a valid id" $ do
    let x = "test123"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkId
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `test_123` as a valid id" $ do
    let x = "test_123"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkId
      Nothing ->
        error "rejected as an invalid token"

  it "rejects `T` as a valid token" $ do
    let x = "T"
    s <- scanTokens x
    s `shouldBe` Nothing

  it "rejects `Test` as a valid token" $ do
    let x = "Test"
    s <- scanTokens x
    s `shouldBe` Nothing

  it "rejects `_test` as a valid token" $ do
    let x = "_test"
    s <- scanTokens x
    s `shouldBe` Nothing
