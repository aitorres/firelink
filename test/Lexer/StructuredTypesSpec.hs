module StructuredTypesSpec where

import Test.Hspec
import Lexer
import Tokens
import Utils (getAbstractToken)

spec :: Spec
spec = describe "Lexer" $ do
  it "accepts `titanite` as a valid token for enum" $ do
    let x = "titanite"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkEnum

  it "accepts `bezel` as a valid token for struct" $ do
    let x = "bezel"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkRecord

  it "accepts `link` as a valid token for union" $ do
    let x = "link"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkUnionStruct
