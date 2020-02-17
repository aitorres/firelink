module ProgramSelectionSpec where

import           Lexer
import           Test.Hspec
import           Tokens
import           Utils      (getAbstractToken)

spec :: Spec
spec = describe "Lexer" $ do
  it "accepts `trust your inventory` as a valid token" $ do
    let x = "trust your inventory"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkIf

  it "accepts `:` as a valid token" $ do
    let x = ":"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkColon

  it "accepts `liar!` as a valid token" $ do
    let x = "liar!"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkElse

  it "accepts `inventory closed` as a valid token" $ do
    let x = "inventory closed"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkEndIf

  it "accepts `enter dungeon with` as a valid token" $ do
    let x = "enter dungeon with"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkSwitch

  it "accepts `empty dungeon` as a valid token" $ do
    let x = "empty dungeon"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkSwitchDefault

  it "accepts `dungeon exited` as a valid token" $ do
    let x = "dungeon exited"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkEndSwitch
