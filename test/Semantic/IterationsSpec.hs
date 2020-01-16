module IterationsSpec where

import Test.Hspec
import qualified TestUtils as U
import qualified SymTable as ST
import qualified Tokens as T

spec :: Spec
spec = describe "Iterations declarations" $ do
  it "rejects iteration variable reassignment" $ do
    let p = "hello ashen one\n\

    \ traveling somewhere \n\
    \ with var i of type humanity <<= 1 in your inventory \n\
    \ upgrading i with 1 soul until level 10 \n\
    \ traveling somewhere \n\
    \ i <<= 1 + 1 \\ \n\
    \ with orange saponite say @hello world@ \n\
    \ you died \n\
    \ max level reached \n\
    \ you died \n\
    \ farewell ashen one"
    errors <- U.extractErrors p
    errors `shouldNotSatisfy` null
    let ST.SemanticError _ T.Token {T.cleanedString=varName, T.posn=pn} = head errors
    varName `shouldBe` "i"
    T.row pn `shouldBe` 6
    T.col pn `shouldBe` 2

  it "rejects constant usage as an iteration variable" $ do
    let p = "hello ashen one\n\

    \ traveling somewhere \n\
    \ with const i of type humanity <<= 1 in your inventory \n\
    \ upgrading i with 1 soul until level 10 \n\
    \ traveling somewhere \n\
    \ with orange saponite say @hello world@ \n\
    \ you died \n\
    \ max level reached \n\
    \ you died \n\
    \ farewell ashen one"
    errors <- U.extractErrors p
    errors `shouldNotSatisfy` null
    let ST.SemanticError _ T.Token {T.cleanedString=varName, T.posn=pn} = head errors
    varName `shouldBe` "i"
    T.row pn `shouldBe` 4
    T.col pn `shouldBe` 12