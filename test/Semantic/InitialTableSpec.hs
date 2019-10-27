module InitialTableSpec where

import Test.Hspec
import qualified Grammar as G
import qualified SymTable as ST
import qualified Lexer as L
import qualified Parser as P
import qualified Control.Monad.RWS as RWS
import Data.Maybe (fromJust)

program = "\
\ hello ashen one \
\ traveling somewhere \
\   go back \
\ you died \
\ farewell ashen one"

testEntryExistence e cat = do
    tokens <- L.scanTokens program
    (_, (dict, _, _), _) <- RWS.runRWST (P.parse $ fromJust tokens) () ST.initialState
    let entry = head $ ST.findChain e dict
    ST.name entry `shouldBe` e
    ST.category entry `shouldBe` cat
    ST.scope entry `shouldBe` 0
    ST.entryType entry `shouldSatisfy` (\Nothing -> True)
    ST.extra entry `shouldSatisfy` null

spec :: Spec
spec = describe "Initial table" $ do
    it "Should contain `int`" $ testEntryExistence "int" ST.Type
    it "Should contain `bigInt`" $ testEntryExistence "bigInt" ST.Type
    it "Should contain `float`" $ testEntryExistence "float" ST.Type
    it "Should contain `char`" $ testEntryExistence "char" ST.Type
    it "Should contain `3bool`" $ testEntryExistence "3bool" ST.Type
    it "Should contain `array`" $ testEntryExistence "array" ST.Constructor
    it "Should contain `string`" $ testEntryExistence "string" ST.Constructor
    it "Should contain `set`" $ testEntryExistence "set" ST.Constructor
    it "Should contain `string`" $ testEntryExistence "string" ST.Constructor
    it "Should contain `pointer`" $ testEntryExistence "pointer" ST.Constructor
