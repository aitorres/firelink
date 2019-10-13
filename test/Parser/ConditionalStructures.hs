module ConditionalStructures where

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