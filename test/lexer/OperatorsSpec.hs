module OperatorsSpec where

import Test.Hspec
import Lexer
import Utils (getAbstractToken)

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

  it "accepts `union` as a valid token" $ do
    let x = "union"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkUnion
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `intersect` as a valid token" $ do
    let x = "intersect"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkIntersect
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `diff` as a valid token" $ do
    let x = "diff"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkDiff
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `size` as a valid token" $ do
    let x = "size"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkSize
      Nothing ->
        error "rejected as an invalid token"

  -- Structured

  it "accepts `~>` as a valid token for comments" $ do
    let x = "~>"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkAccessor
      Nothing ->
        error "rejected as an invalid token"

  -- Pointers

  it "accepts `aim a` as a valid token" $ do
    let x = "aim a"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkAimA
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `throw a` as a valid token" $ do
    let x = "throw a"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkThrowA
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `recover a` as a valid token" $ do
    let x = "recover a"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkRecoverA
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
        atok `shouldBe` TkBraceClosed
      Nothing ->
        error "rejected as an invalid token"