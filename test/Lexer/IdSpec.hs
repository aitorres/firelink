module IdSpec where

import           FireLink.FrontEnd.Lexer
import           FireLink.FrontEnd.Tokens
import           Test.Hspec
import           Utils                    (scanToken)

spec :: Spec
spec = describe "Lexer" $ do
  it "accepts `a` as a valid id" $ do
    let x = "a"
    let atok = scanToken x
    atok `shouldBe` TkId

  it "accepts `b` as a valid id" $ do
    let x = "b"
    let atok = scanToken x
    atok `shouldBe` TkId

  it "accepts `testy` as a valid id" $ do
    let x = "testy"
    let atok = scanToken x
    atok `shouldBe` TkId

  it "accepts `test123` as a valid id" $ do
    let x = "test123"
    let atok = scanToken x
    atok `shouldBe` TkId

  it "accepts `test_123` as a valid id" $ do
    let x = "test_123"
    let atok = scanToken x
    atok `shouldBe` TkId

  it "rejects `T` as a valid token" $ do
    let x = "T"
    let (errors, _) = scanTokens x
    errors `shouldNotSatisfy` null

  it "rejects `Test` as a valid token" $ do
    let x = "Test"
    let (errors, _) = scanTokens x
    errors `shouldNotSatisfy` null

  it "rejects `_test` as a valid token" $ do
    let x = "_test"
    let (errors, _) = scanTokens x
    errors `shouldNotSatisfy` null
