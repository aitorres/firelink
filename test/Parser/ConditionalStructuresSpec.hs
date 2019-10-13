module ConditionalStructuresSpec where

import Test.Hspec
import Utils
import Grammar

buildProgram c = "\
\ hello ashen one \

\ traveling somewhere \
\   trust your inventory \
\   " ++ c ++ " \
\   inventory closed \
\ you died \

\ farewell ashen one"

spec :: Spec
spec = describe "Classic Selection" $ do
    it "rejects empty selection block" $
        runTestForInvalidProgram $ buildProgram ""
    it "rejects selection block with only an else statement" $
        runTestForInvalidProgram $ buildProgram "\
        \ liar!: \
        \   traveling somewhere \
        \       with orange saponite say @hello world@ \
        \   you died"
    it "accepts a selection block with only one if statment" $
        runTestForValidProgram (buildProgram "\
        \ lit: \
        \   traveling somewhere \
        \       with orange saponite say @hello world@ \
        \ you died") (\(Program _ _ (CodeBlock _ [InstIf [
            GuardedCase
                TrueLit
                (CodeBlock _ [InstPrint (StringLit "hello world")])
            ]])) -> True)

    it "accepts a selection block with sevearal if/elseif statments and parses them in order" $
        runTestForValidProgram (buildProgram "\
        \ lit: \
        \   traveling somewhere \
        \       with orange saponite say @hello world@ \
        \   you died \
        \ unlit: \
        \   traveling somewhere \
        \       with orange saponite say @goodbye@ \
        \   you died") (\(Program _ _ (CodeBlock _ [InstIf [
            GuardedCase
                TrueLit
                (CodeBlock _ [InstPrint (StringLit "hello world")]),
            ElseCase (CodeBlock _ [InstPrint (StringLit "goodbye")])
            ]])) -> True)
