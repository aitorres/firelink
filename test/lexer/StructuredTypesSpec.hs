module StructuredTypesSpec where

import Test.Hspec
import Lexer

spec :: Spec
spec = describe "Lexer" $ do
  it "accepts `titanite` as a valid token for enum" $ do
    let x = "titanite"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkEnum
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `bezel` as a valid token for struct" $ do
    let x = "bezel"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkRecord
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `link` as a valid token for union" $ do
    let x = "link"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkUnionStruct
      Nothing ->
        error "rejected as an invalid token"
