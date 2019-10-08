module Lexer.StringLiteralsSpec where

import Test.Hspec
import Lexer
import Lexer.Utils (getAbstractToken)

spec :: Spec
spec = describe "Lexer" $ do
  it "accepts `@@` as a valid string literal" $ do
    let x = "@@"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkStringLit
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `@ @` as a valid string literal" $ do
    let x = "@ @"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkStringLit
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `@test@` as a valid string literal" $ do
    let x = "@test@"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkStringLit
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `@test one@` as a valid string literal" $ do
    let x = "@test one@"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkStringLit
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `@\t@` as a valid string literal" $ do
    let x = "@\t@"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkStringLit
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `@   @` as a valid string literal" $ do
    let x = "@   @"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkStringLit
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `@123123@` as a valid string literal" $ do
    let x = "@123123@"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkStringLit
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `@asdf .-,- 123@` as a valid string literal" $ do
    let x = "@asdf .-,- 123@"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkStringLit
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `@\\n@` as a valid string literal" $ do
    let x = "@\\n@"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkStringLit
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `@lorem ipsum@` as a valid string literal" $ do
    let x = "@lorem ipsum@"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkStringLit
      Nothing ->
        error "rejected as an invalid token"