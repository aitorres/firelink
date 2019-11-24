module StringLiteralsSpec where

import Test.Hspec
import Lexer
import Utils (getAbstractToken)

spec :: Spec
spec = describe "Lexer" $ do
  it "accepts `@@` as a valid string literal" $ do
    let x = "@@"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkStringLit

  it "accepts `@ @` as a valid string literal" $ do
    let x = "@ @"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkStringLit

  it "accepts `@test@` as a valid string literal" $ do
    let x = "@test@"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkStringLit

  it "accepts `@test one@` as a valid string literal" $ do
    let x = "@test one@"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkStringLit

  it "accepts `@\t@` as a valid string literal" $ do
    let x = "@\t@"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkStringLit

  it "accepts `@   @` as a valid string literal" $ do
    let x = "@   @"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkStringLit

  it "accepts `@123123@` as a valid string literal" $ do
    let x = "@123123@"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkStringLit

  it "accepts `@asdf .-,- 123@` as a valid string literal" $ do
    let x = "@asdf .-,- 123@"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkStringLit

  it "accepts `@\\n@` as a valid string literal" $ do
    let x = "@\\n@"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkStringLit

  it "accepts `@lorem ipsum@` as a valid string literal" $ do
    let x = "@lorem ipsum@"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkStringLit
