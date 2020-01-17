module VariablesSpec where

import Test.Hspec
import qualified TestUtils as U
import qualified SymTable as ST
import qualified Tokens as T

program :: String -> String
program e =
  "hello ashen one\n\
  \  traveling somewhere\n\
  \  with\n\
  \    var a of type humanity <<= 1,\n\
  \    const b of type humanity <<= 0,\n\
  \    const c of type <3>-chest of type humanity <<= <$1, 2, 3$>,\n\
  \    var d of type <3>-chest of type humanity <<= <$1, 2, 3$>\n\
  \  in your inventory\n\
  \    "++ e ++ "\\\n\
  \    go back\n\
  \  you died\n\
  \farewell ashen one\n"

test :: U.TestFunction String ST.Dictionary
test prog = U.test (program prog)

testVoid :: U.TestFunction String ()
testVoid prog = U.testVoid (program prog)

spec :: Spec
spec = describe "Constants" $ do
  it "rejects constant reassignments" $ do
    let p = program "b <<= 1"
    errors <- U.extractErrors p
    errors `shouldNotSatisfy` null
    let ST.SemanticError _ T.Token {T.cleanedString=varName, T.posn=pn} = head errors
    varName `shouldBe` "b"
    T.row pn `shouldBe` 9
    T.col pn `shouldBe` 5

  it "allows to use constants on expressions" $ do
    let p = program "with orange saponite say b"
    errors <- U.extractErrors p
    errors `shouldSatisfy` null
  it "rejects constant array reassignments" $ do
    let p = program "c<$0$> <<= 1"
    errors <- U.extractErrors p
    errors `shouldNotSatisfy` null
    let ST.SemanticError _ T.Token {T.cleanedString=varName, T.posn=pn} = head errors
    varName `shouldBe` "c"
    T.row pn `shouldBe` 9
    T.col pn `shouldBe` 5

  it "allows variable reassignments" $ do
    let p = program "a <<= 3"
    errors <- U.extractErrors p
    errors `shouldSatisfy` null

  it "allows variable arrays reassignments" $ do
    let p = program "d<$0$> <<= 3"
    errors <- U.extractErrors p
    errors `shouldSatisfy` null