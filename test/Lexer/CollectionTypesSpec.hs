module CollectionTypesSpec where

import           FireLink.FrontEnd.Lexer
import           FireLink.FrontEnd.Tokens
import           Test.Hspec
import           Utils                    (getAbstractToken)

spec :: Spec
spec = describe "Lexer" $ do
  it "accepts `>-miracle` as a valid token for strings" $ do
    let x = ">-miracle"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkString

  it "accepts `>-chest` as a valid token for arrays" $ do
    let x = ">-chest"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkArray

  it "accepts `<$` as a valid token for array access (open)" $ do
    let x = "<$"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkArrayOpen

  it "accepts `$>` as a valid token for array access (close)" $ do
    let x = "$>"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkArrayClose

  it "accepts `armor` as a valid token for comments" $ do
    let x = "armor"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkSet

  it "accepts `{$` as a valid token for comments" $ do
    let x = "{$"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkSetOpen

  it "accepts `$}` as a valid token for comments" $ do
    let x = "$}"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkSetClose
