module FloatLiteralsSpec where

import           FireLink.FrontEnd.Lexer
import           FireLink.FrontEnd.Tokens
import           Test.Hspec
import           Utils                    (getAbstractToken)

spec :: Spec
spec = describe "Lexer" $ do

  it "accepts `0.0` as a valid float literal" $ do
    let x = "0.0"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkFloatLit

  it "accepts `1.0` as a valid float literal" $ do
    let x = "1.0"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkFloatLit

  it "accepts `1.55` as a valid float literal" $ do
    let x = "1.55"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkFloatLit

  it "accepts `1000000000000000000.55` as a valid float literal" $ do
    let x = "1000000000000000000.55"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkFloatLit

  it "accepts `1.100000000000000000000000000006` as a valid float literal" $ do
    let x = "1.100000000000000000000000000006"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkFloatLit
