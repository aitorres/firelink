module DeclarationsSpec where

import Test.Hspec
import qualified Utils as U
import qualified SymTable as ST
import qualified Grammar as G

program :: String -> String
program e = "hello ashen one\
\ traveling somewhere \
\ with \
\   var x of type " ++ e ++ "\
\ in your inventory \
\ go back \
\ you died \
\ farewell ashen one"

varEntry :: ST.DictionaryEntry
varEntry = ST.DictionaryEntry
    { ST.scope = 1
    , ST.category = ST.Variable
    , ST.entryType = Nothing
    , ST.name = "x"
    , ST.extra = []
    }

test :: U.TestFunction String ST.Dictionary
test prog = U.test (program prog)

testVoid :: U.TestFunction String ()
testVoid prog = U.testVoid (program prog)

spec :: Spec
spec = do
    describe "Variable Declarations" $ do
        it "allows to declare variables of type `humanity`" $
            testVoid "humanity" varEntry{ST.entryType = Just "humanity"}
                U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
        it "allows to declare variables of type `small humanity`" $
            testVoid "small humanity" varEntry{ST.entryType = Just "small humanity"}
                U.extractSimpleFromExtra (\(ST.Simple "small humanity") -> True)
        it "allows to declare variables of type `hollow`" $
            testVoid "hollow" varEntry{ST.entryType = Just "hollow"}
                U.extractSimpleFromExtra (\(ST.Simple "hollow") -> True)
        it "allows to declare variables of type `sign`" $
            testVoid "sign" varEntry{ST.entryType = Just "sign"}
                U.extractSimpleFromExtra (\(ST.Simple "sign") -> True)
        it "allows to declare variables of type `bonfire`" $
            testVoid "bonfire" varEntry{ST.entryType = Just "bonfire"}
                U.extractSimpleFromExtra (\(ST.Simple "bonfire") -> True)
        it "allows to declare variables of type `<n>-miracle`" $
            testVoid "<1>-miracle"  varEntry{ST.entryType = Just ">-miracle"} U.extractCompoundFromExtra
                (\(ST.Compound ">-miracle" (G.IntLit 1)) -> True)
        it "allows to declare variables of recursive type `<n>-chest of type humanity" $
            testVoid "<1>-chest of type humanity"  varEntry{ST.entryType = Just ">-chest"}
            U.extractCompoundRecFromExtra
                (\(ST.CompoundRec ">-chest"
                    (G.IntLit 1)
                    (ST.Simple "humanity")) -> True)
        it "allows to declare variables of recursive type `<n>-chest of type <m>-chest of type humanity" $
            testVoid "<1>-chest of type <2>-chest of type humanity" varEntry{ST.entryType = Just ">-chest"}
                U.extractCompoundRecFromExtra (\(ST.CompoundRec
                    ">-chest"
                    (G.IntLit 1)
                    (ST.CompoundRec
                        ">-chest"
                        (G.IntLit 2)
                        (ST.Simple "humanity"))) -> True)
        it "allows to declare variables of recursive type `<n>-chest of type <n>-miracle" $
            testVoid "<1>-chest of type <2>-miracle" varEntry{ST.entryType = Just ">-chest"}
                U.extractCompoundRecFromExtra (\(ST.CompoundRec
                    ">-chest"
                    (G.IntLit 1)
                    (ST.Compound
                        ">-miracle"
                        (G.IntLit 2))) -> True)
        it "allows to declare variables of recursive type `armor of type sign" $
            testVoid "armor of type sign" varEntry{ST.entryType = Just "armor",  ST.scope = 1}
                U.extractRecursiveFromExtra (\(ST.Recursive
                    "armor"
                    (ST.Simple
                        "sign")) -> True)
        it "allows to declare variables of recursive type `armor of type <n>-chest of type sign" $
            testVoid "armor of type <1>-chest of type sign" varEntry{ST.entryType = Just "armor",  ST.scope = 1}
                U.extractRecursiveFromExtra (\(ST.Recursive
                    "armor"
                    (ST.CompoundRec
                        ">-chest"
                        (G.IntLit 1)
                        (ST.Simple "sign"))) -> True)
        it "allows declare variables of recursive type `armor of type armor of type sign" $
            testVoid "armor of type armor of type sign" varEntry{ST.entryType = Just "armor",  ST.scope = 1}
                U.extractRecursiveFromExtra (\(ST.Recursive
                    "armor"
                    (ST.Recursive
                        "armor"
                        (ST.Simple "sign"))) -> True)
        it "allows declare variable of `record` type" $ do
            dict <- test "bezel { y of type humanity, z of type sign }"
                varEntry{ST.entryType = Just "bezel"} U.extractRecordFieldsFromExtra
                (\(ST.RecordFields 2) -> True)
            U.testEntry dict varEntry
                { ST.name="y"
                , ST.category=ST.RecordItem
                , ST.scope=2
                , ST.entryType=Just "humanity"} U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
            U.testEntry dict varEntry
                { ST.name="z"
                , ST.category=ST.RecordItem
                , ST.scope=2
                , ST.entryType=Just "sign"} U.extractSimpleFromExtra (\(ST.Simple "sign") -> True)
        it "allows declare `union` type variables" $ do
            dict <- test "link { y of type humanity, z of type sign }"
                varEntry{ST.entryType = Just "link"} U.extractRecordFieldsFromExtra
                (\(ST.RecordFields 2) -> True)
            U.testEntry dict varEntry
                { ST.name="y"
                , ST.category=ST.RecordItem
                , ST.scope=2
                , ST.entryType=Just "humanity"} U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
            U.testEntry dict varEntry
                { ST.name="z"
                , ST.category=ST.RecordItem
                , ST.scope=2
                , ST.entryType=Just "sign"} U.extractSimpleFromExtra (\(ST.Simple "sign") -> True)

        it "allows to declare 2 or more variables" $ do
            let p = "hello ashen one \

            \ traveling somewhere \

            \ with \
            \ var x of type sign, \
            \ var y of type humanity \
            \ in your inventory \

            \ go back \
            \ you died \

            \ farewell ashen one"
            (_, (dict, _, _), _) <- U.extractSymTable p

            U.testEntry dict varEntry{ST.entryType = Just "sign"} U.extractSimpleFromExtra
                (\(ST.Simple "sign") -> True)
            U.testEntry dict varEntry{ST.entryType = Just "humanity", ST.name = "y"}
                U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
        it "allows to declare record and simple var (in that order)" $ do
            let p = "hello ashen one \

            \ traveling somewhere \

            \ with \
            \ var y of type bezel { z of type sign, a of type bonfire }, \
            \ var b of type humanity \
            \ in your inventory \

            \ go back \
            \ you died \

            \ farewell ashen one"
            (_, (dict, _, _), _) <- U.extractSymTable p
            U.testEntry dict varEntry
                { ST.name = "y", ST.entryType = Just "bezel" }
                U.extractRecordFieldsFromExtra (\(ST.RecordFields 2) -> True)
            U.testEntry dict varEntry
                { ST.name = "b", ST.entryType = Just "humanity" }
                U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
    describe "Checks for variable redeclarations and scope chain" $
        it "allows to declare the same variable name in a deeper level" $ do
            let p = "hello ashen one \

            \ traveling somewhere \

            \ with \
            \   var x of type humanity \
            \ in your inventory \


            \ trust your inventory \
            \ lit: \
            \   traveling somewhere \
            \   with \
            \       var x of type sign \
            \   in your inventory \

            \   go back \
            \   you died \
            \ inventory closed \
            \ you died \

            \ farewell ashen one"
            (_, (dict, _, _), _) <- U.extractSymTable p
            U.testEntry dict varEntry
                { ST.entryType = Just "humanity" }
                U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
            U.testEntry dict varEntry
                { ST.entryType = Just "sign", ST.scope = 2 }
                U.extractSimpleFromExtra (\(ST.Simple "sign") -> True)
