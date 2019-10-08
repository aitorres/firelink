module Lexer.GeneralSpec where

import Test.Hspec
import Lexer
import Lexer.Utils (getAbstractToken)

spec :: Spec
spec = describe "Lexer" $ do
  it "accepts `const` as a valid token" $ do
    let x = "const"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkConst
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `var` as a valid token" $ do
    let x = "var"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkVar
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `of type` as a valid token" $ do
    let x = "of type"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkOfType
      Nothing ->
        error "rejected as an invalid token"

  it "accepts an assign operator (<<=) as a valid token" $ do
    let x = "<<="
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkAsig
      Nothing ->
        error "rejected as an invalid token"
