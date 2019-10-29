module TypeAliasesSpec where

import Test.Hspec
import qualified Utils as U
import qualified SymTable as ST
import qualified Grammar as G
import qualified Control.Monad.RWS as RWS

program :: String -> String
program e = "hello ashen one\

\ requiring help of \
\ " ++ e ++ " \
\ help received \

\ traveling somewhere \
\ go back \
\ you died \
\ farewell ashen one"

test' :: String -> ST.Scope -> ([ST.Extra] -> ST.Extra) -> (ST.Extra -> Bool) -> IO ST.Dictionary
test' prog scope extractor predicate = do
    let varName = "x"
    (_, (dict, _, _), _) <- U.extractSymTable prog
    let chain = filter (\d -> ST.name d == varName) $ ST.findChain varName dict
    chain `shouldNotSatisfy` null
    let entry = head chain
    ST.name entry `shouldBe` varName
    ST.category entry `shouldBe` ST.Type
    ST.scope entry `shouldBe` scope
    ST.entryType entry `shouldSatisfy` (\Nothing -> True)
    let extra' = ST.extra entry
    extra' `shouldSatisfy` (==1) . length
    extractor extra' `shouldSatisfy` predicate
    return dict

test :: String -> ST.Scope -> ([ST.Extra] -> ST.Extra) -> (ST.Extra -> Bool) -> IO ST.Dictionary
test = test' . program

spec :: Spec
spec = describe "Variable Declarations" $ do
    it "allows to define aliases to just primitive types" $
        RWS.void $ test "knight x humanity" 1 U.extractSimpleFromExtra
            (\(ST.Simple "humanity") -> True)
    it "allows to define aliases to data types with size (strings alikes)" $
        RWS.void $ test "knight x <12>-miracle" 1 U.extractCompoundFromExtra
            (\(ST.Compound ">-miracle" (G.IntLit 12)) -> True)
    it "allows to define aliases to recursive data types (no size) (set alike)" $
        RWS.void $ test "knight x armor of type humanity" 1 U.extractRecursiveFromExtra
            (\(ST.Recursive "armor" (ST.Simple "humanity")) -> True)
    it "allows to define aliases to recursive data types (with size) (arrays alike)" $
        RWS.void $ test "knight x <10>-chest of type sign" 1 U.extractCompoundRecFromExtra
            (\(ST.CompoundRec ">-chest" (G.IntLit 10) (ST.Simple "sign")) -> True)
    it "allows to define aliases to custom user defined record data types" $ do
        dict <- test "knight x bezel { y of type humanity }" 1 U.extractRecordFieldsFromExtra
            (\(ST.RecordFields 2) -> True)
        let varName = "y"
        let scope = 2
        let chain = filter (\d -> ST.scope d == scope) $ filter (\d -> ST.name d == varName) $ ST.findChain varName dict
        chain `shouldNotSatisfy` null
        let entry = head chain
        ST.name entry `shouldBe` varName
        ST.category entry `shouldBe` ST.RecordItem
        ST.scope entry `shouldBe` scope
        ST.entryType entry `shouldBe` Just "humanity"
