module InitialTableSpec where

import Test.Hspec
import qualified SymTable as ST
import qualified Lexer as L
import qualified Parser as P
import qualified Control.Monad.RWS as RWS
import Data.Maybe (fromJust)

program :: String
program = "\
\ hello ashen one \
\ traveling somewhere \
\   go back \
\ you died \
\ farewell ashen one"

testEntryExistence :: String -> ST.Category -> IO ()
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
    it "Should contain `small humanity`" $ testEntryExistence "small humanity" ST.Type
    it "Should contain `humanity`" $ testEntryExistence "humanity" ST.Type
    it "Should contain `hollow`" $ testEntryExistence "hollow" ST.Type
    it "Should contain `sign`" $ testEntryExistence "sign" ST.Type
    it "Should contain `bonfire`" $ testEntryExistence "bonfire" ST.Type
    it "Should contain `>-chest`" $ testEntryExistence ">-chest" ST.Constructor
    it "Should contain `>-miracle`" $ testEntryExistence ">-miracle" ST.Constructor
    it "Should contain `set`" $ testEntryExistence "set" ST.Constructor
    it "Should contain `arrow to`" $ testEntryExistence "arrow to" ST.Constructor
