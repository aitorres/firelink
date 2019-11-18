module ProceduresSpec where

import Test.Hspec
import qualified Utils as U
import qualified SymTable as ST
import qualified Grammar as G
import qualified Lexer as L

varEntry :: ST.DictionaryEntry
varEntry = ST.DictionaryEntry
    { ST.scope = 1
    , ST.category = ST.Procedure
    , ST.entryType = Nothing
    , ST.name = "fun"
    , ST.extra = []
    }

spec :: Spec
spec = do
    describe "Procedures declarations" $ do
        it "allows to declare procedures with no arguments" $ do
            let p = "hello ashen one\n\

            \spell fun\n\
            \traveling somewhere\n\
            \   go back\n\
            \you died\n\
            \ashen estus flask consumed\n\


            \ traveling somewhere \
            \ with orange saponite say @hello world@ \
            \ you died \
            \ farewell ashen one"
            (_, (dict, _, _), _) <- U.extractSymTable p
            U.testEntry dict varEntry U.extractCodeblockFromExtra
                (\(ST.CodeBlock (G.CodeBlock [G.InstReturn])) -> True)
            U.testEntry dict varEntry U.extractEmptyFunctionFromExtra
                (\ST.EmptyFunction -> True)
        it "allows to declare procedures with one val argument" $ do
            let p = "hello ashen one\n\

            \spell fun\n\
            \requesting\n\
            \   val x of type humanity\n\
            \to the estus flask\n\
            \traveling somewhere\n\
            \   go back\n\
            \you died\n\
            \ashen estus flask consumed\n\


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
                , ST.entryType = Just "humanity"
                } U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
        it "allows to declare procedures with one ref argument" $ do
            let p = "hello ashen one\n\

            \spell fun\n\
            \requesting\n\
            \   ref x of type humanity\n\
            \to the estus flask\n\
            \traveling somewhere\n\
            \   go back\n\
            \you died\n\
            \ashen estus flask consumed\n\


            \ traveling somewhere \
            \ with orange saponite say @hello world@ \
            \ you died \
            \ farewell ashen one"
            (_, (dict, _, _), _) <- U.extractSymTable p
            U.testEntry dict varEntry
                { ST.scope = 2
                , ST.name = "x"
                , ST.category = ST.RefParam
                , ST.entryType = Just "humanity"
                } U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
        it "allows to declare procedures with two or more arguments" $ do
            let p = "hello ashen one\n\

            \spell fun\n\
            \requesting\n\
            \   ref x of type humanity,\n\
            \   val y of type humanity\n\
            \to the estus flask\n\
            \traveling somewhere\n\
            \   go back\n\
            \you died\n\
            \ashen estus flask consumed\n\


            \ traveling somewhere \
            \ with orange saponite say @hello world@ \
            \ you died \
            \ farewell ashen one"
            (_, (dict, _, _), _) <- U.extractSymTable p
            U.testEntry dict varEntry
                { ST.scope = 2
                , ST.name = "x"
                , ST.category = ST.RefParam
                , ST.entryType = Just "humanity"
                } U.extractArgPositionFromExtra (\(ST.ArgPosition 0)-> True)
            U.testEntry dict varEntry
                { ST.scope = 2
                , ST.name = "y"
                , ST.category = ST.ValueParam
                , ST.entryType = Just "humanity"
                } U.extractArgPositionFromExtra (\(ST.ArgPosition 1) -> True)
        it "rejects to declare procedures with repeated arguments" $ do
            let p = "hello ashen one\n\

            \spell fun\n\
            \requesting\n\
            \   ref x of type humanity,\n\
            \   val x of type sign\n\
            \to the estus flask\n\
            \traveling somewhere\n\
            \   go back\n\
            \you died\n\
            \ashen estus flask consumed\n\


            \ traveling somewhere \
            \ with orange saponite say @hello world@ \
            \ you died \
            \ farewell ashen one"
            (_, (_, _, _), errors) <- U.extractSymTable p
            errors `shouldNotSatisfy` null
            let ST.SemanticError _ (L.Token _ varName pn) = head errors
            varName `shouldBe` "x"
            L.row pn `shouldBe` 5
            L.col pn `shouldBe` 8
        it "rejects to declare variables on the first scope of a procedure whose names are on the arg list" $ do
            let p = "hello ashen one\n\

            \spell fun\n\
            \requesting\n\
            \   ref x of type humanity\n\
            \to the estus flask\n\

            \traveling somewhere\n\
            \with\n\
            \   var x of type sign\n\
            \in your inventory\n\
            \   go back\n\
            \you died\n\

            \ashen estus flask consumed\n\


            \ traveling somewhere \
            \ with orange saponite say @hello world@ \
            \ you died \
            \ farewell ashen one"
            (_, (_, _, _), errors) <- U.extractSymTable p
            errors `shouldNotSatisfy` null
            let ST.SemanticError _ (L.Token _ varName pn) = head errors
            varName `shouldBe` "x"
            L.row pn `shouldBe` 8
            L.col pn `shouldBe` 8
        it "allows to declare variables on the second (2<=) scope of a procedure whose names are on the arg list" $ do
            let p = "hello ashen one\n\

            \spell fun\n\
            \requesting\n\
            \   ref x of type humanity\n\
            \to the estus flask\n\

            \traveling somewhere\n\

            \   while the lit covenant is active:\n\

            \   traveling somewhere\n\
            \   with\n\
            \       var x of type sign\n\
            \   in your inventory\n\
            \       go back\n\
            \   you died\n\

            \   covenant left\n\
            \you died\n\

            \ashen estus flask consumed\n\


            \ traveling somewhere \
            \ with orange saponite say @hello world@ \
            \ you died \
            \ farewell ashen one"
            (_, (dict, _, _), errors) <- U.extractSymTable p
            errors `shouldSatisfy` null
            U.testEntry dict varEntry
                { ST.scope = 3
                , ST.name = "x"
                , ST.category = ST.Variable
                , ST.entryType = Just "sign"
                } U.extractSimpleFromExtra (\(ST.Simple "sign") -> True)
        it "allows to declare more than 1 procedure" $ do
            let p = "hello ashen one\n\

            \spell fun1\n\

            \traveling somewhere\n\
            \   go back\n\
            \you died\n\

            \ashen estus flask consumed\n\

            \spell fun2\n\

            \traveling somewhere\n\
            \   go back\n\
            \you died\n\

            \ashen estus flask consumed\n\


            \ traveling somewhere \
            \ with orange saponite say @hello world@ \
            \ you died \
            \ farewell ashen one"
            (_, (dict, _, _), errors) <- U.extractSymTable p
            errors `shouldSatisfy` null
            U.testEntry dict varEntry
                { ST.scope = 1
                , ST.name = "fun1"
                } U.extractEmptyFunctionFromExtra (const True)
            U.testEntry dict varEntry
                { ST.scope = 1
                , ST.name = "fun2"
                } U.extractEmptyFunctionFromExtra (const True)
    describe "Functions calls" $ do
        it "allows to call declared procedures with no parameters" $ do
            let p = "hello ashen one\n\

            \spell fun\n\

            \traveling somewhere\n\
            \   go back\n\
            \you died\n\

            \ashen estus flask consumed\n\


            \ traveling somewhere \
            \ with orange saponite say summon fun \
            \ you died \
            \ farewell ashen one"
            (_, _, errors) <- U.extractSymTable p
            errors `shouldSatisfy` null
        it "allows to call declared procedures with parameters" $ do
            let p = "hello ashen one\n\

            \spell fun\n\

            \traveling somewhere\n\
            \   go back\n\
            \you died\n\

            \ashen estus flask consumed\n\


            \ traveling somewhere\n\
            \ with orange saponite say summon fun\n\
            \ you died \
            \ farewell ashen one"
            (_, _, errors) <- U.extractSymTable p
            errors `shouldSatisfy` null
        it "rejects to call non-declared procedures" $ do
            let p = "hello ashen one\n\

            \spell fun\n\

            \traveling somewhere\n\
            \   go back\n\
            \you died\n\

            \ashen estus flask consumed\n\


            \ traveling somewhere\n\
            \   cast fun2\n\
            \ you died \
            \ farewell ashen one"
            (_, _, errors) <- U.extractSymTable p
            errors `shouldNotSatisfy` null
            let ST.SemanticError _ (L.Token _ varName pn) = head errors
            varName `shouldBe` "fun2"
            L.row pn `shouldBe` 8
            L.col pn `shouldBe` 9
        it "allows recursion" $ do
            let p = "hello ashen one\n\

            \spell fun\n\

            \traveling somewhere\n\
            \   cast fun \\\n\
            \   go back\n\
            \you died\n\

            \ashen estus flask consumed\n\


            \ traveling somewhere\n\
            \   cast fun\n\
            \ you died \
            \ farewell ashen one"
            (_, _, errors) <- U.extractSymTable p
            errors `shouldSatisfy` null
        -- it "allows corecursion" $ do
        --     let p = "hello ashen one\n\

        --     \spell fun\n\
        --     \with skill of type humanity\n\

        --     \traveling somewhere\n\
        --     \   go back with summon fun1\n\
        --     \you died\n\

        --     \ashen estus flask consumed\n\

        --     \spell fun1\n\
        --     \with skill of type humanity\n\

        --     \traveling somewhere\n\
        --     \   go back with summon fun\n\
        --     \you died\n\

        --     \ashen estus flask consumed\n\


        --     \ traveling somewhere\n\
        --     \   with orange saponite say summon fun\n\
        --     \ you died \
        --     \ farewell ashen one"
        --     (_, _, errors) <- U.extractSymTable p
        --     errors `shouldSatisfy` null
