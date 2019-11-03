module CallableSpec where

import Test.Hspec
import qualified Utils as U
import qualified SymTable as ST
import qualified Grammar as G

varEntry :: ST.DictionaryEntry
varEntry = ST.DictionaryEntry
    { ST.scope = 1
    , ST.category = ST.Function
    , ST.entryType = Just "humanity"
    , ST.name = "fun"
    , ST.extra = []
    }

spec :: Spec
spec = describe "Functions/Procedures declarations" $ do
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
        U.testEntry dict varEntry U.extractFieldsFromExtra
            (\(ST.Fields 2) -> True)
        U.testEntry dict varEntry U.extractCodeblockFromExtra
            (\(ST.CodeBlock (G.CodeBlock [G.InstReturnWith (G.IntLit 1)])) -> True)
    it "allows to declare functions with one argument" $ do
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
        U.testEntry dict varEntry U.extractCodeblockFromExtra
            (\(ST.CodeBlock (G.CodeBlock [G.InstReturnWith (G.IntLit 1)])) -> True)
        U.testEntry dict varEntry
            { ST.scope = 2
            , ST.name = "x"
            } U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
