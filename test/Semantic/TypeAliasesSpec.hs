module TypeAliasesSpec where

import qualified Data.Map                   as Map
import           FireLink.FrontEnd.Errors
import qualified FireLink.FrontEnd.Grammar  as G
import qualified FireLink.FrontEnd.SymTable as ST
import qualified FireLink.Utils             as UU
import           Test.Hspec
import qualified TestUtils                  as U

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
spec = describe "Aliases Declarations" $ do
    it "allows to define aliases to just primitive types" $
        testVoid "knight x humanity" alias{ST.entryType = Just "humanity"}
            U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
    it "allows to define aliases to data types with size (strings alikes)" $
        testVoid "knight x <12>-miracle" alias{ST.entryType = Just ">-miracle"}
            U.extractCompoundFromExtra (\(ST.Compound ">-miracle" G.Expr{G.expAst=G.IntLit 12}) -> True)
    it "allows to define aliases to recursive data types (no size) (set alike)" $
        testVoid "knight x armor of type humanity" alias{ST.entryType = Just "armor"}
            U.extractRecursiveFromExtra
            (\(ST.Recursive "armor" (ST.Simple "humanity")) -> True)
    it "allows to define aliases to recursive data types (with size) (arrays alike)" $
        testVoid "knight x <10>-chest of type sign" alias{ST.entryType = Just ">-chest"}
            U.extractCompoundRecFromExtra
            (\(ST.CompoundRec ">-chest" G.Expr{G.expAst=G.IntLit 10} (ST.Simple "sign")) -> True)
    it "allows to define aliases to custom user defined record data types" $ do
        dict <- test "knight x bezel { y of type humanity }" alias{ST.entryType = Just "bezel"}
            U.extractFieldsFromExtra
            (\(ST.Fields ST.Record 2) -> True)
        U.testEntry dict alias
            { ST.name="y"
            , ST.category=ST.RecordItem
            , ST.scope=2
            , ST.entryType=Just "humanity"} U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
    it "allows to define aliases to custom user defined record data types with more than 1 field" $ do
        dict <- test "knight x bezel { y of type humanity, z of type sign }"
            alias{ST.entryType = Just "bezel"} U.extractFieldsFromExtra
            (\(ST.Fields ST.Record 2) -> True)
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
    it "allows to use a type alias as a type on a declaration" $ do
        let p = "hello ashen one\n\

        \requiring help of\n\
        \   knight x bezel { x of type humanity }\n\
        \help received\n\

        \traveling somewhere\n\
        \with\n\
        \   var y of type x\n\
        \in your inventory\n\
        \   go back\n\
        \you died\n\
        \ farewell ashen one\n"
        (_, ST.SymTable {ST.stDict=dict}, errors) <- U.extractSymTable p
        errors `shouldSatisfy` null
        U.testEntry dict alias
            { ST.name = "y"
            , ST.category = ST.Variable
            , ST.entryType = Just "x"
            , ST.scope = 1
            } U.extractSimpleFromExtra (\(ST.Simple "x") -> True)
    it "rejects to use a non-declared type alias as a type on a declaration" $ do
        let p = "hello ashen one\n\

        \traveling somewhere\n\
        \with\n\
        \   var y of type x\n\
        \in your inventory\n\
        \   go back\n\
        \you died\n\
        \ farewell ashen one\n"
        (_, ST.SymTable {ST.stDict=dict}, errors) <- U.extractSymTable p
        errors `shouldNotSatisfy` null
        let Error _ pn = head errors
        UU.row pn `shouldBe` 4
        UU.column pn `shouldBe` 18
        Map.findWithDefault [] "y" dict `shouldSatisfy` null
