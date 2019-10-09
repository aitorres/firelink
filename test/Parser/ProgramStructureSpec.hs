module ProgramStructureSpec where

import Test.Hspec
import Utils
import Grammar

spec :: Spec
spec = describe "ProgramStructure" $ do
    it "rejects `program without main` as a valid program" $
        runTestForInvalidProgram "\
        \ hello ashen one \
        \ farewell ashen one"

    it "rejects a program with an empty main" $
        runTestForInvalidProgram "\
        \ hello ashen one\

        \ traveling somewhere \
        \ you died \

        \ farewell ashen one"
