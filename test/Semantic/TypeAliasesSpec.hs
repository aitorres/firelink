module TypeAliasesSpec where

import Test.Hspec
import qualified Utils as U
import qualified SymTable as ST
import qualified Grammar as G

program :: String -> String
program e = "hello ashen one\

\ requiring help of \
\ " ++ e ++ " \
\ help received \

\ traveling somewhere \
\ go back \
\ you died \
\ farewell ashen one"

test :: U.TestFunction String ST.Dictionary
test prog = U.test (program prog)

testVoid :: U.TestFunction String ()
testVoid prog = U.testVoid (program prog)

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
            U.extractFieldsFromExtra
            (\(ST.Fields 2) -> True)
        U.testEntry dict alias
            { ST.name="y"
            , ST.category=ST.RecordItem
            , ST.scope=2
            , ST.entryType=Just "humanity"} U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
    it "allows to define aliases to custom user defined record data types with more than 1 field" $ do
        dict <- test "knight x bezel { y of type humanity, z of type sign }"
            alias{ST.entryType = Just "bezel"} U.extractFieldsFromExtra
            (\(ST.Fields 2) -> True)
        U.testEntry dict alias
            { ST.name="y"
            , ST.category=ST.RecordItem
            , ST.scope=2
            , ST.entryType=Just "humanity"} U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
        U.testEntry dict alias
            { ST.name="z"
            , ST.category=ST.RecordItem
            , ST.scope=2
            , ST.entryType=Just "sign"} U.extractSimpleFromExtra (\(ST.Simple "sign") -> True)
