module ForEachLoopStructuresSpec where

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
        \ weaponry repaired") (\(Program _ _ (
            CodeBlock _ [
                InstForEach (Id "a") (Id "b") (CodeBlock _ [InstPrint (StringLit "")])
                ])) -> True)
