module RecordLikeTypesDeclSpec where

import Test.Hspec
import qualified Utils as U
import qualified SymTable as ST

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
spec =
    describe "Record like variable declarations" $ do
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
        it "allows to declare record with already used name properties" $ do
            let p = "hello ashen one\n \

            \ traveling somewhere\n \

            \ with\n \
            \   var x of type bezel { \
            \         x of type bezel { \
            \           x of type humanity \
            \         } \
            \       } \
            \ in your inventory\n \

            \ go back \

            \ you died \

            \ farewell ashen one"
            (_, (dict, _, _), errors) <- U.extractSymTable p
            errors `shouldSatisfy` null
            U.testEntry dict varEntry
                { ST.entryType = Just "bezel" }
                U.extractRecordFieldsFromExtra (\(ST.RecordFields 2) -> True)
            U.testEntry dict varEntry
                { ST.entryType = Just "bezel", ST.scope = 2, ST.category = ST.RecordItem }
                U.extractRecordFieldsFromExtra (\(ST.RecordFields 3) -> True)
            U.testEntry dict varEntry
                { ST.entryType = Just "humanity", ST.scope = 3, ST.category = ST.RecordItem }
                U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
