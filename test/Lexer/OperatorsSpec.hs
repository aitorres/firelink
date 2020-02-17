module OperatorsSpec where

import           FireLink.FrontEnd.Lexer
import           FireLink.FrontEnd.Tokens
import           Test.Hspec
import           Utils                    (getAbstractToken)

spec :: Spec
spec = describe "Lexer" $ do
  -- Arithmetic
  it "accepts `+` as a valid token" $ do
    let x = "+"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkPlus

  it "accepts `-` as a valid token" $ do
    let x = "-"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkMinus

  it "accepts `*` as a valid token" $ do
    let x = "*"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkMult

  it "accepts `/` as a valid token" $ do
    let x = "/"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkDiv

  it "accepts `%` as a valid token" $ do
    let x = "%"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkMod

  -- Comparison
  it "accepts `lt` as a valid token" $ do
    let x = "lt"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkLt

  it "accepts `gt` as a valid token" $ do
    let x = "gt"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkGt

  it "accepts `lte` as a valid token" $ do
    let x = "lte"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkLte

  it "accepts `gte` as a valid token" $ do
    let x = "gte"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkGte

  it "accepts `eq` as a valid token" $ do
    let x = "eq"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkEq

  it "accepts `neq` as a valid token" $ do
    let x = "neq"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkNeq

  it "accepts `and` as a valid token" $ do
    let x = "and"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkAnd

  it "accepts `or` as a valid token" $ do
    let x = "or"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkOr

  -- Collections
  it "accepts `>-<` as a valid token" $ do
    let x = ">-<"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkConcat

  it "accepts `union` as a valid token" $ do
    let x = "union"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkUnion

  it "accepts `intersect` as a valid token" $ do
    let x = "intersect"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkIntersect

  it "accepts `diff` as a valid token" $ do
    let x = "diff"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkDiff

  it "accepts `size` as a valid token" $ do
    let x = "size"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkSize

  -- Structured

  it "accepts `~>` as a valid token for comments" $ do
    let x = "~>"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkAccessor

  -- Pointers

  it "accepts `aim a` as a valid token" $ do
    let x = "aim a"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkRequestMemory

  it "accepts `throw a` as a valid token" $ do
    let x = "throw a"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkAccessMemory

  it "accepts `recover a` as a valid token" $ do
    let x = "recover a"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkFreeMemory

  -- Unary
  it "accepts `not` as a valid token" $ do
    let x = "not"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkNot

  -- Special
  it "accepts `<` as a valid token" $ do
    let x = "<"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkLteLit

  it "accepts `{` as a valid token" $ do
    let x = "{"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkBraceOpen

  it "accepts `}` as a valid token" $ do
    let x = "}"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkBraceClose

  it "accepts `(` as a valid token" $ do
    let x = "("
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkParensOpen

  it "accepts `)` as a valid token" $ do
    let x = ")"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkParensClose
