module ProgramStructureSpec where

import Test.Hspec
import Lexer

spec :: Spec
spec = describe "Lexer" $ do
  it "accepts `--` as a valid token for comments" $ do
    let x = "--"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkComment
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `hello ashen one` as a valid token" $ do
    let x = "hello ashen one"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkProgramBegin
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `farewell ashen one` as a valid token" $ do
    let x = "farewell ashen one"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkProgramEnd
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `in your inventory` as a valid token" $ do
    let x = "in your inventory"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkDeclarationEnd
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `traveling somewhere` as a valid token" $ do
    let x = "traveling somewhere"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkInstructionBegin
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `you died` as a valid token" $ do
    let x = "you died"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkInstructionEnd
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `\\` as a valid token" $ do
    let x = "\\"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkSeq
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `with orange saponite say` as a valid token" $ do
    let x = "with orange saponite say"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkPrint
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `transpose into` as a valid token" $ do
    let x = "transpose into"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkRead
      Nothing ->
        error "rejected as an invalid token"
