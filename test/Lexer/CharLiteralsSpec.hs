module Lexer.CharLiteralsSpec where

import Test.Hspec
import Lexer
import Lexer.Utils (getAbstractToken)

spec :: Spec
spec = describe "Lexer" $ do

  it "accepts `|a|` as a valid char literal" $ do
    let x = "|a|"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkCharLit
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `|b|` as a valid char literal" $ do
    let x = "|b|"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkCharLit
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `|\0|` as a valid char literal" $ do
    let x = "|\0|"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkCharLit
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `|\t|` as a valid char literal" $ do
    let x = "|\t|"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkCharLit
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `|\\n|` as a valid char literal" $ do
    let x = "|\\n|"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkCharLit
      Nothing ->
        error "rejected as an invalid token"