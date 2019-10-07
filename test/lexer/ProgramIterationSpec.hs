module ProgramIterationSpec where

import Test.Hspec
import Lexer

spec :: Spec
spec = describe "Lexer" $ do
  it "accepts `upgrading` as a valid token" $ do
    let x = "upgrading"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkFor
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `with` as a valid token" $ do
    let x = "with"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkWith
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `soul` as a valid token" $ do
    let x = "soul"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkSoul
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `souls` as a valid token" $ do
    let x = "souls"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkSoul
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `until level` as a valid token" $ do
    let x = "until level"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkLevel
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `max level reached` as a valid token" $ do
    let x = "max level reached"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkEndFor
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `repairing` as a valid token" $ do
    let x = "repairing"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkForEach
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `with titanite from` as a valid token" $ do
    let x = "with titanite from"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkWithTitaniteFrom
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `weaponry repaired` as a valid token" $ do
    let x = "weaponry repaired"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkEndForEach
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `while the` as a valid token" $ do
    let x = "while the"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkWhile
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `covenant is active` as a valid token" $ do
    let x = "covenant is active"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkCovenantIsActive
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `covenant left` as a valid token" $ do
    let x = "covenant left"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkEndWhile
      Nothing ->
        error "rejected as an invalid token"
