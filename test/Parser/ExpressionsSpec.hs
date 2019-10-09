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
    it "accepts `1 + 2 + 3` as an expression and associates to the left" $
        runTestForValidProgram (buildProgramWithExpr "1 + 2 + 3")
        (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration Const (Id "patata") BigInt (Add (Add (IntLit 1) (IntLit 2)) (IntLit 3))]
                _)) -> True)
    it "accepts `1 - 2` as an expression" $
        runTestForValidProgram (buildProgramWithExpr "1 - 2")
        (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration Const (Id "patata") BigInt (Substract (IntLit 1) (IntLit 2))]
                _)) -> True)
    it "accepts `1 - 2 - 3` as an expression and associates to the left" $
        runTestForValidProgram (buildProgramWithExpr "1 - 2 - 3")
        (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration Const (Id "patata") BigInt (Substract (Substract (IntLit 1) (IntLit 2)) (IntLit 3))]
                _)) -> True)
    it "accepts `1 - 2 + 3` as an expression and associates to the left (1 - 2) + 3" $
        runTestForValidProgram (buildProgramWithExpr "1 - 2 + 3")
        (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration Const (Id "patata") BigInt (Add (Substract (IntLit 1) (IntLit 2)) (IntLit 3))]
                _)) -> True)
    it "accepts `1 + 2 - 3` as an expression and associates to the left (1 + 2) - 3" $
        runTestForValidProgram (buildProgramWithExpr "1 + 2 - 3")
        (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration Const (Id "patata") BigInt (Substract (Add (IntLit 1) (IntLit 2)) (IntLit 3))]
                _)) -> True)
    it "accepts `1 * 2` as an expression" $
        runTestForValidProgram (buildProgramWithExpr "1 * 2")
        (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration Const (Id "patata") BigInt (Multiply (IntLit 1) (IntLit 2))]
                _)) -> True)
    it "accepts `1 * 2 * 3` as an expression and associates to the left" $
        runTestForValidProgram (buildProgramWithExpr "1 * 2 * 3")
        (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration Const (Id "patata") BigInt (Multiply (Multiply (IntLit 1) (IntLit 2)) (IntLit 3))]
                _)) -> True)
    it "accepts `1 * 2 + 3` as an expression and associates to the left" $
        runTestForValidProgram (buildProgramWithExpr "1 * 2 + 3")
        (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration Const (Id "patata") BigInt (
                    Add
                        (Multiply (IntLit 1) (IntLit 2))
                        (IntLit 3)
                        )]
                _)) -> True)
    it "accepts `1 + 2 * 3` as an expression and associates to the left" $
        runTestForValidProgram (buildProgramWithExpr "1 + 2 * 3")
        (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration Const (Id "patata") BigInt (
                    Add
                        (IntLit 1)
                        (Multiply (IntLit 2) (IntLit 3))
                        )]
                _)) -> True)
    it "accepts `1 / 2` as an expression" $
        runTestForValidProgram (buildProgramWithExpr "1 / 2")
        (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration Const (Id "patata") BigInt (Divide (IntLit 1) (IntLit 2))]
                _)) -> True)
    it "accepts `1 / 2 / 3` as an expression and associates to the left" $
        runTestForValidProgram (buildProgramWithExpr "1 / 2 / 3")
        (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration Const (Id "patata") BigInt (
                    Divide
                        (Divide (IntLit 1) (IntLit 2))
                        (IntLit 3)
                        )]
                _)) -> True)
    it "accepts `1 / 2 + 3` as an expression and associates to the left" $
        runTestForValidProgram (buildProgramWithExpr "1 / 2 + 3")
        (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration Const (Id "patata") BigInt (
                    Add
                        (Divide (IntLit 1) (IntLit 2))
                        (IntLit 3)
                        )]
                _)) -> True)
    it "accepts `1 + 2 / 3` as an expression and associates to the left" $
        runTestForValidProgram (buildProgramWithExpr "1 + 2 / 3")
        (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration Const (Id "patata") BigInt (
                    Add
                        (IntLit 1)
                        (Divide (IntLit 2) (IntLit 3))
                        )]
                _)) -> True)
    it "accepts `1 % 2` as an expression" $
        runTestForValidProgram (buildProgramWithExpr "1 % 2")
        (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration Const (Id "patata") BigInt (Mod (IntLit 1) (IntLit 2))]
                _)) -> True)
    it "accepts `1 % 2 % 3` as an expression and associates to the left" $
        runTestForValidProgram (buildProgramWithExpr "1 % 2 % 3")
        (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration Const (Id "patata") BigInt (
                    Mod
                        (Mod (IntLit 1) (IntLit 2))
                        (IntLit 3)
                        )]
                _)) -> True)
    it "accepts `1 % 2 + 3` as an expression and associates to the left" $
        runTestForValidProgram (buildProgramWithExpr "1 % 2 + 3")
        (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration Const (Id "patata") BigInt (
                    Add
                        (Mod (IntLit 1) (IntLit 2))
                        (IntLit 3)
                        )]
                _)) -> True)
    it "accepts `1 + 2 % 3` as an expression and parse % as more precedent" $
        runTestForValidProgram (buildProgramWithExpr "1 + 2 % 3")
        (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration Const (Id "patata") BigInt (
                    Add
                        (IntLit 1)
                        (Mod (IntLit 2) (IntLit 3))
                        )]
                _)) -> True)
    it "accepts `- 1` as an expression and associates to the left" $
        runTestForValidProgram (buildProgramWithExpr "- 1")
        (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration Const (Id "patata") BigInt (
                    Negative
                        (IntLit 1))]
                _)) -> True)
    it "rejects `- - 1` as an expression and associates to the left" $
        runTestForValidProgram (buildProgramWithExpr "- - 1")
        (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration Const (Id "patata") BigInt (
                    Negative
                        (Negative (IntLit 1)))]
                _)) -> True)
    it "accepts `1 lt 2` as an expression and associates to the left" $
        runTestForValidProgram (buildProgramWithExpr "1 lt 2")
        (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration Const (Id "patata") BigInt (
                    Lt (IntLit 1) (IntLit 2))]
                _)) -> True)
    it "accepts `1 lt 2 + 3` as an expression and associates to the right (1 lt (2 + 3))" $
        runTestForValidProgram (buildProgramWithExpr "1 lt 2 + 3")
        (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration Const (Id "patata") BigInt (
                    Lt (IntLit 1) (Add (IntLit 2) (IntLit 3)))]
                _)) -> True)
