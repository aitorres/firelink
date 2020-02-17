module InitialTableSpec where

import qualified Control.Monad.RWS          as RWS
import qualified FireLink.FrontEnd.Lexer    as L
import qualified FireLink.FrontEnd.Parser   as P
import qualified FireLink.FrontEnd.SymTable as ST
import           Test.Hspec

program :: String
program = "\
\ hello ashen one \
\ traveling somewhere \
\   go back \
\ you died \
\ farewell ashen one"

testEntryExistence :: String -> ST.Category -> IO ()
testEntryExistence e cat = do
    let ([], tokens) = L.scanTokens program
    (_, ST.SymTable {ST.stDict=dict}, _) <- RWS.runRWST (P.parse tokens) () ST.initialState
    let entry = head $ ST.findChain e dict
    ST.name entry `shouldBe` e
    ST.category entry `shouldBe` cat
    ST.scope entry `shouldBe` 0
    ST.entryType entry `shouldSatisfy` (\Nothing -> True)

spec :: Spec
spec = describe "Initial table" $ do
    it "Should contain `small humanity`" $ testEntryExistence "small humanity" ST.Type
    it "Should contain `humanity`" $ testEntryExistence "humanity" ST.Type
    it "Should contain `hollow`" $ testEntryExistence "hollow" ST.Type
    it "Should contain `sign`" $ testEntryExistence "sign" ST.Type
    it "Should contain `bonfire`" $ testEntryExistence "bonfire" ST.Type
    it "Should contain `>-chest`" $ testEntryExistence ">-chest" ST.Constructor
    it "Should contain `>-miracle`" $ testEntryExistence ">-miracle" ST.Constructor
    it "Should contain `armor`" $ testEntryExistence "armor" ST.Constructor
    it "Should contain `arrow to`" $ testEntryExistence "arrow to" ST.Constructor
    it "Should contain `bezel`" $ testEntryExistence "bezel" ST.Constructor
    it "Should contain `link`" $ testEntryExistence "link" ST.Constructor
