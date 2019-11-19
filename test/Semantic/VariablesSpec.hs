module VariablesSpec where

import Test.Hspec
import qualified Utils as U
import qualified SymTable as ST
import qualified Lexer as L

program :: String -> String
program e = "hello ashen one\n\
\traveling somewhere\n\
\with\n\
\var a of type humanity <<= 1,\n\
\const b of type humanity <<= 0,\n\
\const c of type <3>-chest of type humanity <<= <$1, 2, 3$>,\n\
\var d of type <3>-chest of type humanity <<= <$1, 2, 3$>\n\
\in your inventory\n\
\" ++ e ++ "\\\n\
\go back\n\
\you died\n\
\farewell ashen one\n"

test :: U.TestFunction String ST.Dictionary
test prog = U.test (program prog)

testVoid :: U.TestFunction String ()
testVoid prog = U.testVoid (program prog)

spec :: Spec
spec = describe "Constants" $ do
  it "rejects constant reassignments" $ do
    let p = program $ "b <<= 1"
    (_, _, errors) <- U.extractSymTable p
    errors `shouldNotSatisfy` null
    let ST.SemanticError _ (L.Token _ (Just varName) pn) = head errors
    varName `shouldBe` "b"
    L.row pn `shouldBe` 9
    L.col pn `shouldBe` 1

  it "rejects constant array reassignments" $ do
    let p = program $ "c<$0$> <<= 1"
    (_, _, errors) <- U.extractSymTable p
    errors `shouldNotSatisfy` null
    let ST.SemanticError _ (L.Token _ (Just varName) pn) = head errors
    varName `shouldBe` "c"
    L.row pn `shouldBe` 9
    L.col pn `shouldBe` 1

  it "allows variable reassignments" $ do
    let p = program $ "a <<= 3"
    (_, _, errors) <- U.extractSymTable p
    errors `shouldSatisfy` null

  it "allows variable arrays reassignments" $ do
    let p = program $ "a<$0$> <<= 3"
    (_, _, errors) <- U.extractSymTable p
    errors `shouldSatisfy` null