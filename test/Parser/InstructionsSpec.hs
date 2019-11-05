module InstructionsSpec where

import Test.Hspec
import Grammar
import Utils
import Lexer

buildProgram :: String -> String
buildProgram s = "\
\ hello ashen one \
\   traveling somewhere \
\       " ++ s ++ "\
\   you died \
\ farewell ashen one"

spec :: Spec
spec = describe "Instructions" $ do
    it "accepts program with only 1 instruction" $
        runTestForValidProgram "\
        \ hello ashen one \

        \ traveling somewhere \
        \   go back \
        \ you died \

        \ farewell ashen one" (\(Program (CodeBlock [InstReturn])) -> True)
    it "accepts program with only 2+ instruction" $
        runTestForValidProgram "\
        \ hello ashen one \

        \ traveling somewhere \
        \ with \
        \   var patata of type humanity \
        \ in your inventory \

        \   with orange saponite say @hello world@ \\ \
        \   transpose into patata \
        \ you died \

        \ farewell ashen one" (\(Program (CodeBlock [
            InstPrint (StringLit "hello world"),
            InstRead (IdExpr (Id (Token _ (Just "patata") _)))])) -> True)

    it "accepts assigning as an instruction" $
        runTestForValidProgram (buildProgram "a <<= 1")
        (\(Program (CodeBlock [
            InstAsig (IdExpr (Id (Token _ (Just "a") _))) (IntLit 1)
        ])) -> True)
