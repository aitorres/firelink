module StringLiteralsSpec where

import           FireLink.FrontEnd.Tokens
import           Test.Hspec
import           Utils                    (scanToken)

spec :: Spec
spec = describe "Lexer" $ do
  it "accepts `@@` as a valid string literal" $ do
    let x = "@@"
    let atok = scanToken x
    atok `shouldBe` TkStringLit

  it "accepts `@ @` as a valid string literal" $ do
    let x = "@ @"
    let atok = scanToken x
    atok `shouldBe` TkStringLit

  it "accepts `@test@` as a valid string literal" $ do
    let x = "@test@"
    let atok = scanToken x
    atok `shouldBe` TkStringLit

  it "accepts `@test one@` as a valid string literal" $ do
    let x = "@test one@"
    let atok = scanToken x
    atok `shouldBe` TkStringLit

  it "accepts `@\t@` as a valid string literal" $ do
    let x = "@\t@"
    let atok = scanToken x
    atok `shouldBe` TkStringLit

  it "accepts `@   @` as a valid string literal" $ do
    let x = "@   @"
    let atok = scanToken x
    atok `shouldBe` TkStringLit

  it "accepts `@123123@` as a valid string literal" $ do
    let x = "@123123@"
    let atok = scanToken x
    atok `shouldBe` TkStringLit

  it "accepts `@asdf .-,- 123@` as a valid string literal" $ do
    let x = "@asdf .-,- 123@"
    let atok = scanToken x
    atok `shouldBe` TkStringLit

  it "accepts `@\\n@` as a valid string literal" $ do
    let x = "@\\n@"
    let atok = scanToken x
    atok `shouldBe` TkStringLit

  it "accepts `@lorem ipsum@` as a valid string literal" $ do
    let x = "@lorem ipsum@"
    let atok = scanToken x
    atok `shouldBe` TkStringLit
