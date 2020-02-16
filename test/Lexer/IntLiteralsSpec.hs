module IntLiteralsSpec where

import           Lexer
import           Test.Hspec
import           Tokens
import           Utils      (getAbstractToken)

spec :: Spec
spec = describe "Lexer" $ do

  it "accepts `0` as a valid int literal" $ do
    let x = "0"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkIntLit

  it "accepts `1` as a valid int literal" $ do
    let x = "1"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkIntLit

  it "accepts `100` as a valid int literal" $ do
    let x = "100"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkIntLit
