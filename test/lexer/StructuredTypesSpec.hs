module StructuredTypesSpec where

import Test.Hspec
import Lexer

spec :: Spec
spec = describe "Lexer" $ do
  it "accepts `titanite` as a valid token for comments" $ do
    let x = "titanite"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkTitanite
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `bezel` as a valid token for comments" $ do
    let x = "bezel"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkBezel
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `link` as a valid token for comments" $ do
    let x = "link"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkLink
      Nothing ->
        error "rejected as an invalid token"
