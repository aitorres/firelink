module StructuredTypesSpec where

import Test.Hspec
import Lexer
import Tokens
import Utils (getAbstractToken)

spec :: Spec
spec = describe "Lexer" $ do
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

  it "accepts `is_active` as a valid token" $ do
    let x = "is_active"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkIsActive
