module ProgramStructureSpec where

import           FireLink.FrontEnd.Grammar
import           Test.Hspec
import           Utils

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

    it "accepts a program with no aliases but with functions" $
        runTestForValidProgram "\
        \ hello ashen one\

        \ invocation f \
        \ with skill of type sign \
        \ traveling somewhere \
        \   go back \
        \ you died \
        \ after this return to your world \

        \ traveling somewhere \
        \   go back \
        \ you died \

        \ farewell ashen one" (\(Program _) -> True)

    it "accepts a program with both aliases and functions" $
        runTestForValidProgram "\
        \ hello ashen one\

        \ requiring help of \
        \   knight solaire humanity, \
        \   knight fourKings hollow \
        \ help received \

        \ traveling somewhere \
        \   go back \
        \ you died \

        \ farewell ashen one" (\(Program _) -> True)


    it "accepts a program with both aliases and functions" $
        runTestForValidProgram "\
        \ hello ashen one\

        \ requiring help of \
        \   knight solaire humanity, \
        \   knight fourKings hollow \
        \ help received \

        \ invocation f \
        \ with skill of type sign \
        \ traveling somewhere \
        \   go back \
        \ you died \
        \ after this return to your world \

        \ traveling somewhere \
        \   go back \
        \ you died \

        \ farewell ashen one" (\(Program _) -> True)

    it "accepts a program with no aliases but with functions" $
        runTestForValidProgram "\
        \ hello ashen one\

        \ traveling somewhere \
        \ with \
        \   var i of type humanity \
        \ in your inventory \
        \   go back \
        \ you died \

        \ farewell ashen one" (\(Program (CodeBlock [InstReturn])) -> True)
