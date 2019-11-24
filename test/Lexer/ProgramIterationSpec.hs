module ProgramIterationSpec where

import Test.Hspec
import Lexer
import Utils (getAbstractToken)

spec :: Spec
spec = describe "Lexer" $ do
  it "accepts `upgrading` as a valid token" $ do
    let x = "upgrading"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkFor

  it "accepts `with` as a valid token" $ do
    let x = "with"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkWith

  it "accepts `soul` as a valid token" $ do
    let x = "soul"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkSoul

  it "accepts `souls` as a valid token" $ do
    let x = "souls"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkSoul

  it "accepts `until level` as a valid token" $ do
    let x = "until level"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkLevel

  it "accepts `max level reached` as a valid token" $ do
    let x = "max level reached"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkEndFor

  it "accepts `repairing` as a valid token" $ do
    let x = "repairing"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkForEach

  it "accepts `with titanite from` as a valid token" $ do
    let x = "with titanite from"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkWithTitaniteFrom

  it "accepts `weaponry repaired` as a valid token" $ do
    let x = "weaponry repaired"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkEndForEach

  it "accepts `while the` as a valid token" $ do
    let x = "while the"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkWhile

  it "accepts `covenant is active` as a valid token" $ do
    let x = "covenant is active"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkCovenantIsActive

  it "accepts `covenant left` as a valid token" $ do
    let x = "covenant left"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkEndWhile
