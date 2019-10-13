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

    it "accepts program with only 1 instruction" $
        runTestForValidProgram "\
        \ hello ashen one \

        \ traveling somewhere \
        \   go back \
        \ you died \

        \ farewell ashen one" (\(Program _ _ (CodeBlock _ [InstReturn])) -> True)
    it "accepts program with only 2+ instruction" $
        runTestForValidProgram "\
        \ hello ashen one \

        \ traveling somewhere \
        \   with orange saponite say @hello world@ \\ \
        \   transpose into patata \
        \ you died \

        \ farewell ashen one" (\(Program _ _ (CodeBlock _ [
            InstPrint (StringLit "hello world"),
            InstRead (Id "patata")])) -> True)
