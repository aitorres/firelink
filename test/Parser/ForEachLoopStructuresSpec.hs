module ForEachLoopStructuresSpec where

import Test.Hspec
import Utils
import Grammar
import Tokens

buildProgram :: String -> String
buildProgram c = "\
    \ hello ashen one \

    \ traveling somewhere \
    \   " ++ c ++ " \
    \ you died \
    \ farewell ashen one"

spec :: Spec
spec = describe "Loops over structures" $ do
    it "rejects when id is replaced by an expression" $
        runTestForInvalidProgram $ buildProgram "\
        \ repairing 1 with titanite from b \
        \ traveling somewhere \
        \   with orange saponite say @@ \
        \ you died \
        \ weaponry repaired"
    it "accepts on well-formed loops" $
        runTestForValidProgram (buildProgram "\
        \ repairing a with titanite from b \
        \ traveling somewhere \
        \   with orange saponite say @@ \
        \ you died \
        \ weaponry repaired") (\(Program (
            CodeBlock [
                InstForEach (Id Token {cleanedString="a"}) Expr{expAst=IdExpr (Id Token {cleanedString="b"})}
                    (CodeBlock [InstPrint Expr{expAst=StringLit ""}])
                ])) -> True)
