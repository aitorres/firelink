module ProgramMethodsSpec where

import Test.Hspec
import Lexer
import Utils (getAbstractToken)

spec :: Spec
spec = describe "Lexer" $ do
  it "accepts `spell` as a valid token for comments" $ do
    let x = "spell"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkSpell
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `ashen estus flask consumed` as a valid token" $ do
    let x = "ashen estus flask consumed"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkSpellEnd
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `cast` as a valid token" $ do
    let x = "cast"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkCast
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `offering` as a valid token" $ do
    let x = "offering"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkOffering
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `invocation` as a valid token" $ do
    let x = "invocation"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkInvocation
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `requesting` as a valid token" $ do
    let x = "requesting"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkRequesting
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `with skill of type` as a valid token" $ do
    let x = "with skill of type"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkInvocationType
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `after this return to your world` as a valid token" $ do
    let x = "after this return to your world"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkInvocationEnd
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `val` as a valid token" $ do
    let x = "val"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkVal
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `ref` as a valid token" $ do
    let x = "ref"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkRef
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `go back` as a valid token" $ do
    let x = "go back"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkReturn
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `go back with` as a valid token" $ do
    let x = "go back with"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkReturnWith
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `summon` as a valid token" $ do
    let x = "summon"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkSummon
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `granting` as a valid token" $ do
    let x = "granting"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkGranting
      Nothing ->
        error "rejected as an invalid token"
