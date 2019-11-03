module CallableSpec where

import Test.Hspec
import qualified Utils as U
import qualified SymTable as ST
import qualified Grammar as G
import qualified Lexer as L

varEntry :: ST.DictionaryEntry
varEntry = ST.DictionaryEntry
    { ST.scope = 1
    , ST.category = ST.Function
    , ST.entryType = Just "humanity"
    , ST.name = "fun"
    , ST.extra = []
    }

spec :: Spec
spec =
    describe "Functions/Procedures declarations" $ do
        it "allows to declare functions with no arguments" $ do
            let p = "hello ashen one\n\

            \invocation fun\n\
            \with skill of type humanity\n\
            \traveling somewhere\n\
            \   go back with 1\n\
            \you died\n\
            \after this return to your world\n\


            \ traveling somewhere \
            \ with orange saponite say @hello world@ \
            \ you died \
            \ farewell ashen one"
            (_, (dict, _, _), _) <- U.extractSymTable p
            U.testEntry dict varEntry U.extractCodeblockFromExtra
                (\(ST.CodeBlock (G.CodeBlock [G.InstReturnWith (G.IntLit 1)])) -> True)
            U.testEntry dict varEntry U.extractEmptyFunctionFromExtra
                (\ST.EmptyFunction -> True)
        it "allows to declare functions with one val argument" $ do
            let p = "hello ashen one\n\

            \invocation fun\n\
            \requesting\n\
            \   val x of type humanity\n\
            \with skill of type humanity\n\
            \traveling somewhere\n\
            \   go back with 1\n\
            \you died\n\
            \after this return to your world\n\


            \ traveling somewhere \
            \ with orange saponite say @hello world@ \
            \ you died \
            \ farewell ashen one"
            (_, (dict, _, _), _) <- U.extractSymTable p
            U.testEntry dict varEntry U.extractFieldsFromExtra
                (\(ST.Fields 2) -> True)
            U.testEntry dict varEntry
                { ST.scope = 2
                , ST.name = "x"
                , ST.category = ST.ValueParam
                } U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
        it "allows to declare functions with one ref argument" $ do
            let p = "hello ashen one\n\

            \invocation fun\n\
            \requesting\n\
            \   ref x of type humanity\n\
            \with skill of type humanity\n\
            \traveling somewhere\n\
            \   go back with 1\n\
            \you died\n\
            \after this return to your world\n\


            \ traveling somewhere \
            \ with orange saponite say @hello world@ \
            \ you died \
            \ farewell ashen one"
            (_, (dict, _, _), _) <- U.extractSymTable p
            U.testEntry dict varEntry
                { ST.scope = 2
                , ST.name = "x"
                , ST.category = ST.RefParam
                } U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
        it "allows to declare functions with two or more arguments" $ do
            let p = "hello ashen one\n\

            \invocation fun\n\
            \requesting\n\
            \   ref x of type humanity,\n\
            \   val y of type humanity\n\
            \with skill of type humanity\n\
            \traveling somewhere\n\
            \   go back with 1\n\
            \you died\n\
            \after this return to your world\n\


            \ traveling somewhere \
            \ with orange saponite say @hello world@ \
            \ you died \
            \ farewell ashen one"
            (_, (dict, _, _), _) <- U.extractSymTable p
            U.testEntry dict varEntry
                { ST.scope = 2
                , ST.name = "x"
                , ST.category = ST.RefParam
                } U.extractArgPositionFromExtra (\(ST.ArgPosition 0)-> True)
            U.testEntry dict varEntry
                { ST.scope = 2
                , ST.name = "y"
                , ST.category = ST.ValueParam
                } U.extractArgPositionFromExtra (\(ST.ArgPosition 1) -> True)
        it "rejects to declare functions with repeated arguments" $ do
            let p = "hello ashen one\n\

            \invocation fun\n\
            \requesting\n\
            \   ref x of type humanity,\n\
            \   val x of type sign\n\
            \with skill of type humanity\n\
            \traveling somewhere\n\
            \   go back with 1\n\
            \you died\n\
            \after this return to your world\n\


            \ traveling somewhere \
            \ with orange saponite say @hello world@ \
            \ you died \
            \ farewell ashen one"
            (_, (_, _, _), errors) <- U.extractSymTable p
            errors `shouldNotSatisfy` null
            let ST.SemanticError _ (L.Token _ (Just varName) pn) = head errors
            varName `shouldBe` "x"
            L.row pn `shouldBe` 5
            L.col pn `shouldBe` 8
        it "rejects to declare variables on the first scope of a function whose names are on the arg list" $ do
            let p = "hello ashen one\n\

            \invocation fun\n\
            \requesting\n\
            \   ref x of type humanity\n\
            \with skill of type humanity\n\

            \traveling somewhere\n\
            \with\n\
            \   var x of type sign\n\
            \in your inventory\n\
            \   go back with 1\n\
            \you died\n\

            \after this return to your world\n\


            \ traveling somewhere \
            \ with orange saponite say @hello world@ \
            \ you died \
            \ farewell ashen one"
            (_, (_, _, _), errors) <- U.extractSymTable p
            errors `shouldNotSatisfy` null
            let ST.SemanticError _ (L.Token _ (Just varName) pn) = head errors
            varName `shouldBe` "x"
            L.row pn `shouldBe` 8
            L.col pn `shouldBe` 8
        it "allows to declare variables on the second (2<=) scope of a function whose names are on the arg list" $ do
            let p = "hello ashen one\n\

            \invocation fun\n\
            \requesting\n\
            \   ref x of type humanity\n\
            \with skill of type humanity\n\

            \traveling somewhere\n\

            \   while the lit covenant is active:\n\

            \   traveling somewhere\n\
            \   with\n\
            \       var x of type sign\n\
            \   in your inventory\n\
            \       go back with 1\n\
            \   you died\n\

            \   covenant left\n\
            \you died\n\

            \after this return to your world\n\


            \ traveling somewhere \
            \ with orange saponite say @hello world@ \
            \ you died \
            \ farewell ashen one"
            (_, (dict, _, _), errors) <- U.extractSymTable p
            errors `shouldSatisfy` null
            U.testEntry dict varEntry
                { ST.scope = 3
                , ST.name = "x"
                , ST.entryType = Just "humanity"
                } U.extractSimpleFromExtra (\(ST.Simple "sign") -> True)
        it "allows to declare more than 1 function" $ do
            let p = "hello ashen one\n\

            \invocation fun1\n\
            \with skill of type humanity\n\

            \traveling somewhere\n\
            \   go back with 1\n\
            \you died\n\

            \after this return to your world\n\

            \invocation fun2\n\
            \with skill of type sign\n\

            \traveling somewhere\n\
            \   go back with 1\n\
            \you died\n\

            \after this return to your world\n\


            \ traveling somewhere \
            \ with orange saponite say @hello world@ \
            \ you died \
            \ farewell ashen one"
            (_, (dict, _, _), errors) <- U.extractSymTable p
            errors `shouldSatisfy` null
            U.testEntry dict varEntry
                { ST.scope = 1
                , ST.name = "fun1"
                , ST.entryType = Just "humanity"
                } U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
            U.testEntry dict varEntry
                { ST.scope = 1
                , ST.name = "fu2"
                , ST.entryType = Just "sign"
                } U.extractSimpleFromExtra (\(ST.Simple "sign") -> True)
