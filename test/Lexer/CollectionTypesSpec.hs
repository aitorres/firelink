module CollectionTypesSpec where

import Test.Hspec
import Lexer
import Utils (getAbstractToken)

spec :: Spec
spec = describe "Lexer" $ do
  it "accepts `>-miracle` as a valid token for strings" $ do
    let x = ">-miracle"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkString
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `>-chest` as a valid token for arrays" $ do
    let x = ">-chest"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkArray
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `<$` as a valid token for array access (open)" $ do
    let x = "<$"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkArrayOpen
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `$>` as a valid token for array access (close)" $ do
    let x = "$>"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkArrayClose
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `armor` as a valid token for comments" $ do
    let x = "armor"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkSet
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `{$` as a valid token for comments" $ do
    let x = "{$"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkSetOpen
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `$}` as a valid token for comments" $ do
    let x = "$}"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkSetClose
      Nothing ->
        error "rejected as an invalid token"
