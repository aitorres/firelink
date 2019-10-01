module IntLiteralsSpec where

import Test.Hspec
import Lexer

spec :: Spec
spec = describe "Lexer" $ do

  it "accepts `0` as a valid int literal" $ do
    let x = "0"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkInt
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `1` as a valid int literal" $ do
    let x = "1"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkInt
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `100` as a valid int literal" $ do
    let x = "100"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkInt
      Nothing ->
        error "rejected as an invalid token"
