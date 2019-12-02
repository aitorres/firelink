module BoundedLoopStructuresSpec where

import Test.Hspec
import Utils
import Grammar
import Lexer

buildProgram :: String -> String
buildProgram c = "\
    \ hello ashen one \

    \ traveling somewhere \
    \   " ++ c ++ " \
    \ you died \
    \ farewell ashen one"

spec :: Spec
spec = describe "Bounded loop structures" $ do
    it "rejects bounded loop structures when programmer confuses the id with an expression" $
        runTestForInvalidProgram $ buildProgram "\
        \ upgrading |a| with 2 souls until level 123 \
        \   traveling somewhere \
        \       with orange saponite say @hello@ \
        \   you died \
        \ max level reached"
    it "accepts well formed bounded iterations" $
        runTestForValidProgram (buildProgram "\
        \ upgrading aa with 2 souls until level 123 \
        \   traveling somewhere \
        \       with orange saponite say @hello@ \
        \   you died \
        \ max level reached") (\(Program (CodeBlock [
            InstFor (Id Token {cleanedString="aa"}) Expr{expAst=(IntLit 2)} Expr{expAst=(IntLit 123)} (CodeBlock _)
        ])) -> True)
