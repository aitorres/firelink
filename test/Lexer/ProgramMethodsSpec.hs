module ProgramMethodsSpec where

import Test.Hspec
import Lexer
import Utils (getAbstractToken)

spec :: Spec
spec = describe "Lexer" $ do
  it "accepts `spell` as a valid token for comments" $ do
    let x = "spell"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkSpell

  it "accepts `ashen estus flask consumed` as a valid token" $ do
    let x = "ashen estus flask consumed"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkSpellEnd

  it "accepts `cast` as a valid token" $ do
    let x = "cast"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkCast

  it "accepts `offering` as a valid token" $ do
    let x = "offering"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkOffering

  it "accepts `invocation` as a valid token" $ do
    let x = "invocation"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkInvocation

  it "accepts `requesting` as a valid token" $ do
    let x = "requesting"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkRequesting

  it "accepts `with skill of type` as a valid token" $ do
    let x = "with skill of type"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkInvocationType

  it "accepts `after this return to your world` as a valid token" $ do
    let x = "after this return to your world"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkInvocationEnd

  it "accepts `val` as a valid token" $ do
    let x = "val"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkVal

  it "accepts `ref` as a valid token" $ do
    let x = "ref"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkRef

  it "accepts `go back` as a valid token" $ do
    let x = "go back"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkReturn

  it "accepts `go back with` as a valid token" $ do
    let x = "go back with"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkReturnWith

  it "accepts `summon` as a valid token" $ do
    let x = "summon"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkSummon

  it "accepts `granting` as a valid token" $ do
    let x = "granting"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkGranting
