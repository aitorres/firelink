module CollectionTypesSpec where

import Test.Hspec
import Lexer

spec :: Spec
spec = describe "Lexer" $ do
  it "accepts `>-miracle` as a valid token for comments" $ do
    let x = ">-miracle"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkMiracle
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `>-chest` as a valid token for comments" $ do
    let x = ">-chest"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkChest
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `<$` as a valid token for comments" $ do
    let x = "<$"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkChestOpen
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `$>` as a valid token for comments" $ do
    let x = "$>"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkChestClose
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `armor` as a valid token for comments" $ do
    let x = "armor"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkArmor
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `{$` as a valid token for comments" $ do
    let x = "{$"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkArmorOpen
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `$}` as a valid token for comments" $ do
    let x = "$}"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkArmorClose
      Nothing ->
        error "rejected as an invalid token"
