module ProgramSelectionSpec where

import Test.Hspec
import Lexer

spec :: Spec
spec = describe "Lexer" $ do
  it "accepts `trust your inventory` as a valid token" $ do
    let x = "trust your inventory"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkIf
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `:` as a valid token" $ do
    let x = ":"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkColon
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `liar!` as a valid token" $ do
    let x = "liar!"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkElse
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `inventory closed` as a valid token" $ do
    let x = "inventory closed"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkEndIf
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `enter dungeon with` as a valid token" $ do
    let x = "enter dungeon with"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkSwitch
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `empty dungeon` as a valid token" $ do
    let x = "empty dungeon"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkSwitchDefault
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `dungeon exited` as a valid token" $ do
    let x = "dungeon exited"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkEndSwitch
      Nothing ->
        error "rejected as an invalid token"
