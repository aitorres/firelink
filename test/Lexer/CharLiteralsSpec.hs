module CharLiteralsSpec where

import Test.Hspec
import Lexer
import Tokens
import Utils (getAbstractToken)

spec :: Spec
spec = describe "Lexer" $ do

  it "accepts `|a|` as a valid char literal" $ do
    let x = "|a|"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkCharLit

  it "accepts `|b|` as a valid char literal" $ do
    let x = "|b|"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkCharLit

  it "accepts `|\0|` as a valid char literal" $ do
    let x = "|\0|"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkCharLit

  it "accepts `|\t|` as a valid char literal" $ do
    let x = "|\t|"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkCharLit

  it "accepts `|\\n|` as a valid char literal" $ do
    let x = "|\\n|"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkCharLit
