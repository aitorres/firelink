module Lexer.FloatLiteralsSpec where

import Test.Hspec
import Lexer
import Lexer.Utils (getAbstractToken)

spec :: Spec
spec = describe "Lexer" $ do

  it "accepts `0.0` as a valid float literal" $ do
    let x = "0.0"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkFloatLit
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `1.0` as a valid float literal" $ do
    let x = "1.0"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkFloatLit
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `1.55` as a valid float literal" $ do
    let x = "1.55"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkFloatLit
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `1000000000000000000.55` as a valid float literal" $ do
    let x = "1000000000000000000.55"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkFloatLit
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `1.100000000000000000000000000006` as a valid float literal" $ do
    let x = "1.100000000000000000000000000006"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkFloatLit
      Nothing ->
        error "rejected as an invalid token"
