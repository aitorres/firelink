module FunctionsSpec where

import Test.Hspec
import Grammar
import Utils

spec :: Spec
spec = describe "Functions declarations" $ do
    let buildProgram c = "\
    \ hello ashen one \
    \   " ++ c ++ " \
    \ traveling somewhere \
    \   with orange saponite say @@ \
    \ you died \
    \ farewell ashen one"

    it "accepts no-args functions" $
        runTestForValidProgram (buildProgram "\
        \ invocation fun \
        \ with skill of type humanity \
        \   traveling somewhere \
        \   with orange saponite say @@ \
        \   you died \
        \ after this return to your world") (\(Program _ [
            Function (Id "fun") [] BigInt _
        ] _) -> True)
