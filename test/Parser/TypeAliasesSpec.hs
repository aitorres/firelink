module Parser.TypeAliasesSpec where

import Test.Hspec
import Parser.Utils
import Grammar

spec :: Spec
spec = describe "Type Aliases" $ do
    it "accepts a program with no type aliases" $
        runTestForValidProgram "\
        \ hello ashen one \

        \ traveling somewhere \
        \   with orange saponite say @@ \
        \ you died \

        \ farewell ashen one" (\(Program [] _ _) -> True)

    it "accepts a program with one type alias" $
        runTestForValidProgram "\
        \ hello ashen one \

        \ requiring help of \
        \   knight solaire humanity \
        \ help received \

        \ traveling somewhere \
        \   with orange saponite say @@ \
        \ you died \

        \ farewell ashen one" (\(Program [Alias (Id "solaire") BigInt] _ _) -> True)

    it "rejects when the alias list is empty" $
        runTestForInvalidProgram "\
        \ hello ashen one \
        \ requiring help of \
        \ help received \

        \ traveling somewhere \
        \   with orange saponite say @@ \
        \ you died \

        \ farewell ashen one"
