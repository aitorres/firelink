module GeneralSpec where

import           FireLink.FrontEnd.Lexer
import           FireLink.FrontEnd.Tokens
import           Test.Hspec
import           Utils                    (getAbstractToken)

spec :: Spec
spec = describe "Lexer" $ do
  it "accepts `const` as a valid token" $ do
    let x = "const"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkConst

  it "accepts `var` as a valid token" $ do
    let x = "var"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkVar

  it "accepts `of type` as a valid token" $ do
    let x = "of type"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkOfType

  it "accepts an assign operator (<<=) as a valid token" $ do
    let x = "<<="
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkAsig
