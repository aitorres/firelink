module ExpressionsSpec where

import Test.Hspec
import Utils
import Grammar

buildProgramWithExpr e = "\
\ hello ashen one \

\ traveling somewhere \
\   with \
\      const patata of type humanity <<= " ++ e ++ " \
\   in your inventory \
\   with orange saponite say @Hello world@ \
\ you died \

\ farewell ashen one"

spec :: Spec
spec = describe "Expressions" $ do
    it "accepts `1 + 2` as an expression" $
        runTestForValidProgram (buildProgramWithExpr "1 + 2")
        (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration Const (Id "patata") BigInt (Add (IntLit 1) (IntLit 2))]
                _)) -> True)
    it "accepts `1 + 2 + 3` as an expressions and associates to the left" $
        runTestForValidProgram (buildProgramWithExpr "1 + 2 + 3")
        (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration Const (Id "patata") BigInt (Add (Add (IntLit 1) (IntLit 2)) (IntLit 3))]
                _)) -> True)
