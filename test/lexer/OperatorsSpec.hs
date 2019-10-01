module OperatorsSpec where

import Test.Hspec
import Lexer

spec :: Spec
spec = describe "Lexer" $ do
  -- Arithmetic
  it "accepts `+` as a valid token" $ do
    let x = "+"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkPlus
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `-` as a valid token" $ do
    let x = "-"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkMinus
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `*` as a valid token" $ do
    let x = "*"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkMult
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `/` as a valid token" $ do
    let x = "/"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkDiv
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `%` as a valid token" $ do
    let x = "%"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkMod
      Nothing ->
        error "rejected as an invalid token"

  -- Comparison
  it "accepts `lt` as a valid token" $ do
    let x = "lt"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkLt
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `gt` as a valid token" $ do
    let x = "gt"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkGt
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `lte` as a valid token" $ do
    let x = "lte"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkLte
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `gte` as a valid token" $ do
    let x = "gte"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkGte
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `eq` as a valid token" $ do
    let x = "eq"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkEq
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `neq` as a valid token" $ do
    let x = "neq"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkNeq
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `and` as a valid token" $ do
    let x = "and"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkAnd
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `or` as a valid token" $ do
    let x = "or"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkOr
      Nothing ->
        error "rejected as an invalid token"

  -- Collections
  it "accepts `>-<` as a valid token" $ do
    let x = ">-<"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkConcat
      Nothing ->
        error "rejected as an invalid token"

  -- Unary
  it "accepts `not` as a valid token" $ do
    let x = "not"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkNot
      Nothing ->
        error "rejected as an invalid token"

  -- Special
  it "accepts `<` as a valid token" $ do
    let x = "<"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkLteLit
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `{` as a valid token" $ do
    let x = "{"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkBraceOpen
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `}` as a valid token" $ do
    let x = "}"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkBraceOpen
      Nothing ->
        error "rejected as an invalid token"