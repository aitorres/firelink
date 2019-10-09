module Parser.TypeAliasesSpec where

import Test.Hspec
import Parser.Utils
import Parser
import Data.Maybe
import Lexer

spec :: Spec
spec = describe "Type Aliases" $ do
    it "accepts a program with no type aliases" $
        runTestForValidProgram "" (\(Program [] _ _) -> True)

    it "accepts a program with one type alias" $
        runTestForValidProgram "\
            \ requiring help of \
            \   knight solaire humanity \
            \ help received" (\(Program [Alias (Id "solaire") BigInt] _ _) -> True)

    it "rejects when the alias list is empty" $
        runTestForInvalidProgram "\
        \ requiring help of \
        \ help received"
