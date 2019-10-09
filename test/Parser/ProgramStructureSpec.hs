module Parser.ProgramStructureSpec where

import Test.Hspec
import Parser.Utils
import Grammar

spec :: Spec
spec = describe "ProgramStructure" $ do
    it "rejects `program without main` as a valid program" $
        runTestForInvalidProgram "\
        \ hello ashen one \
        \ farewell ashen one"

    it "accepts a program with an empty main" $
        runTestForValidProgram "\
        \ hello ashen one\

        \ traveling somewhere \
        \ you died \

        \ farewell ashen one" (\(Program [] [] (CodeBlock [] [])) -> True)
