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

type TestFunction a b
    = a
    -> ST.DictionaryEntry
    -> ([ST.Extra] -> ST.Extra)
    -> (ST.Extra -> Bool)
    -> IO b

test' :: TestFunction ST.Dictionary ()
test' dict expectedEntry extractor predicate = do
    let varName = ST.name expectedEntry
    let chain = filter (\d -> ST.name d == varName) $ ST.findChain varName dict
    chain `shouldNotSatisfy` null
    let actualEntry = head chain
    ST.name actualEntry `shouldBe` varName
    ST.category actualEntry `shouldBe` ST.category expectedEntry
    ST.scope actualEntry `shouldBe` ST.scope expectedEntry
    ST.entryType actualEntry `shouldBe` ST.entryType expectedEntry
    let extra' = ST.extra actualEntry
    extra' `shouldNotSatisfy` null
    extractor extra' `shouldSatisfy` predicate

test :: TestFunction String ST.Dictionary
test prog expectedEntry extractor predicate = do
    (_, (dict, _, _), _) <- U.extractSymTable $ program prog
    test' dict expectedEntry extractor predicate
    return dict

testVoid :: TestFunction String ()
testVoid prog expectedEntry extractor predicate = RWS.void $ test prog expectedEntry extractor predicate

alias :: ST.DictionaryEntry
alias = ST.DictionaryEntry
    { ST.scope = 1
    , ST.category = ST.Type
    , ST.entryType = Nothing
    , ST.name = "x"
    , ST.extra = []
    }

spec :: Spec
spec = describe "Variable Declarations" $ do
    it "allows to define aliases to just primitive types" $
        testVoid "knight x humanity" alias{ST.entryType = Just "humanity"}
            U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
    it "allows to define aliases to data types with size (strings alikes)" $
        testVoid "knight x <12>-miracle" alias{ST.entryType = Just ">-miracle"}
            U.extractCompoundFromExtra (\(ST.Compound ">-miracle" (G.IntLit 12)) -> True)
    it "allows to define aliases to recursive data types (no size) (set alike)" $
        testVoid "knight x armor of type humanity" alias{ST.entryType = Just "armor"}
            U.extractRecursiveFromExtra
            (\(ST.Recursive "armor" (ST.Simple "humanity")) -> True)
    it "allows to define aliases to recursive data types (with size) (arrays alike)" $
        testVoid "knight x <10>-chest of type sign" alias{ST.entryType = Just ">-chest"}
            U.extractCompoundRecFromExtra
            (\(ST.CompoundRec ">-chest" (G.IntLit 10) (ST.Simple "sign")) -> True)
    it "allows to define aliases to custom user defined record data types" $ do
        dict <- test "knight x bezel { y of type humanity }" alias{ST.entryType = Just "bezel"}
            U.extractRecordFieldsFromExtra
            (\(ST.RecordFields 2) -> True)
        test' dict alias
            { ST.name="y"
            , ST.category=ST.RecordItem
            , ST.scope=2
            , ST.entryType=Just "humanity"} U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
    it "allows to define aliases to custom user defined record data types with more than 1 field" $ do
        dict <- test "knight x bezel { y of type humanity, z of type sign }"
            alias{ST.entryType = Just "bezel"} U.extractRecordFieldsFromExtra
            (\(ST.RecordFields 2) -> True)
        test' dict alias
            { ST.name="y"
            , ST.category=ST.RecordItem
            , ST.scope=2
            , ST.entryType=Just "humanity"} U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
        test' dict alias
            { ST.name="z"
            , ST.category=ST.RecordItem
            , ST.scope=2
            , ST.entryType=Just "sign"} U.extractSimpleFromExtra (\(ST.Simple "sign") -> True)
