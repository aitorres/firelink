module GeneralSpec where

import Test.Hspec
import Lexer

spec :: Spec
spec = describe "Lexer.General" $ do
  it "accepts a const token as valid" $ do
    let x = "const"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkConst
      Nothing ->
        error "parsed as an invalid token"

  it "accepts a const token as valid" $ do
    let x = "var"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkVar
      Nothing ->
        error "parsed as an invalid token"
