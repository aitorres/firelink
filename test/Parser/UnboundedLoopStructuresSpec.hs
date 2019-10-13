module UnboundedLoopStructuresSpec where

import Test.Hspec
import Utils
import Grammar

buildProgram c = "\
    \ hello ashen one \

    \ traveling somewhere \
    \   " ++ c ++ " \
    \ you died \
    \ farewell ashen one"

spec :: Spec
spec = describe "Unbounded looping" $ do
    it "rejects an empty unbounded loop" $
        runTestForInvalidProgram $ buildProgram "\
        \ while the lit covenant is active: \
        \ covenant left"
    it "accepts a well-formed unbounded loop" $
        runTestForValidProgram (buildProgram "\
        \ while the unlit covenant is active: \
            \ traveling somewhere \
            \   with orange saponite say @@ \
            \ you died \
        \ covenant left")  (\(Program _ _ (
            CodeBlock _ [
                InstWhile FalseLit (CodeBlock _ _)
                ])) -> True)