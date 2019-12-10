module RecordLikeTypesDeclSpec where

import Test.Hspec
import qualified Utils as U
import qualified SymTable as ST
import qualified Tokens as T

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
    describe "Record like variable declarations" $ do
        it "allows declare variable of `record` type" $ do
            dict <- test "bezel { y of type humanity, z of type sign }"
                varEntry{ST.entryType = Just "bezel"} U.extractFieldsFromExtra
                (\(ST.Fields ST.Record 2) -> True)
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
                varEntry{ST.entryType = Just "link"} U.extractFieldsFromExtra
                (\(ST.Fields ST.Union 2) -> True)
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
                U.extractFieldsFromExtra (\(ST.Fields ST.Record 2) -> True)
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
                U.extractFieldsFromExtra (\(ST.Fields ST.Record 2) -> True)
            U.testEntry dict varEntry
                { ST.entryType = Just "bezel", ST.scope = 2, ST.category = ST.RecordItem }
                U.extractFieldsFromExtra (\(ST.Fields ST.Record 3) -> True)
            U.testEntry dict varEntry
                { ST.entryType = Just "humanity", ST.scope = 3, ST.category = ST.RecordItem }
                U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
    describe "Record like variable accessing" $ do
        it "allows to access ST.records properties" $
            U.shouldNotError "hello ashen one\n\

            \traveling somewhere\n\

            \with\n\
            \   var x of type bezel {\n\
            \       y of type humanity\n\
            \   }\n\
            \in your inventory\n\

            \   with orange saponite say x ~> y\n\

            \you died\n\

            \farewell ashen one"
        it "allows to access ST.records properties on deeper levels" $
            U.shouldNotError "hello ashen one\n\

            \traveling somewhere\n\

            \with\n\
            \   var x of type bezel {\n\
            \       y of type bezel {\n\
            \           z of type humanity\n\
            \       }\n\
            \   }\n\
            \in your inventory\n\

            \   with orange saponite say x ~> y ~> z\n\

            \you died\n\

            \farewell ashen one"
        it "allows to access ST.records properties with the same name" $
            U.shouldNotError "hello ashen one\n\

            \traveling somewhere\n\

            \with\n\
            \   var x of type bezel {\n\
            \       x of type humanity\n\
            \   }\n\
            \in your inventory\n\

            \   with orange saponite say x ~> x\n\

            \you died\n\

            \farewell ashen one"
        it "rejects to access ST.records properties that doesn't exist" $ do
            let p = "hello ashen one\n\

            \traveling somewhere\n\

            \with\n\
            \   var x of type bezel {\n\
            \       y of type humanity\n\
            \   }\n\
            \in your inventory\n\

            \   with orange saponite say x ~> z\n\

            \you died\n\

            \farewell ashen one"
            (_, _, errors) <- U.extractSymTable p
            errors `shouldNotSatisfy` null
            let ST.SemanticError _ T.Token {T.cleanedString=varName, T.posn=pn} = head errors
            varName `shouldBe` "z"
            T.col pn `shouldBe` 34
            T.row pn `shouldBe` 8
        it "allows to access ST.record properties of arrays of records" $
            U.shouldNotError "hello ashen one\n\

            \traveling somewhere\n\

            \with\n\
            \   var x of type <1>-chest of type bezel {\n\
            \       y of type bezel {\n\
            \           z of type humanity\n\
            \       }\n\
            \   }\n\
            \in your inventory\n\

            \   with orange saponite say x <$ 0 $> ~> y ~> z\n\

            \you died\n\

            \farewell ashen one"
        it "rejects to access ST.record properties of arrays of any type that is not record" $ do
            let p = "hello ashen one\n\

            \traveling somewhere\n\

            \with\n\
            \   var x of type <1>-chest of type humanity\n\
            \in your inventory\n\

            \   with orange saponite say x<$0$> ~> z\n\

            \you died\n\

            \farewell ashen one"
            (_, _, errors) <- U.extractSymTable p
            errors `shouldNotSatisfy` null
            let ST.SemanticError _ T.Token {T.cleanedString=varName, T.posn=pn} = head errors
            varName `shouldBe` "z"
            T.col pn `shouldBe` 39
            T.row pn `shouldBe` 6
        it "accepts valid x<$0$> ~> y ~> z<$0$> ~> a" $
            U.shouldNotError "hello ashen one\n\

            \traveling somewhere\n\

            \with\n\
            \   var x of type <1>-chest of type bezel {\n\
            \       y of type bezel {\n\
            \           z of type <1>-chest of type bezel {\n\
            \               a of type humanity\n\
            \           }\n\
            \       }\n\
            \   }\n\
            \in your inventory\n\

            \   with orange saponite say x<$0$> ~> y ~> z<$0$> ~> a\n\

            \you died\n\

            \farewell ashen one"
        it "rejects invalid x<$0$> ~> y ~> z<$0$> ~> b" $ do
            let p = "hello ashen one\n\

            \traveling somewhere\n\

            \with\n\
            \   var x of type <1>-chest of type bezel {\n\
            \       y of type bezel {\n\
            \           z of type <1>-chest of type bezel {\n\
            \               a of type humanity\n\
            \           }\n\
            \       }\n\
            \   }\n\
            \in your inventory\n\

            \   with orange saponite say x<$0$> ~> y ~> z<$0$> ~> b\n\

            \you died\n\

            \farewell ashen one"
            (_, _, errors) <- U.extractSymTable p
            errors `shouldNotSatisfy` null
            let ST.SemanticError _ T.Token {T.cleanedString=varName, T.posn=pn} = head errors
            varName `shouldBe` "b"
            T.col pn `shouldBe` 54
            T.row pn `shouldBe` 12
