module LiteralsSpec where

import Test.Hspec
import Utils
import Grammar

buildProgramWithLiteral l = "\
\ hello ashen one \

\ traveling somewhere \
\   with \
\      const patata of type humanity <<= " ++ l ++ " \
\   in your inventory \
\   with orange saponite say @Hello world@ \
\ you died \

\ farewell ashen one"

spec :: Spec
spec = describe "Literal Values" $ do
    it "accepts `123` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "123")
        (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration Const (Id "patata") BigInt (IntLit 123)]
                _)) -> True)
    it "accepts `lit` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "lit")
        (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration Const (Id "patata") BigInt TrueLit]
                _)) -> True)
    it "accepts `unlit` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "unlit")
        (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration Const (Id "patata") BigInt FalseLit]
                _)) -> True)
    it "accepts `undiscovered` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "undiscovered")
        (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration Const (Id "patata") BigInt UndiscoveredLit]
                _)) -> True)

    it "accepts `|a|` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "|a|")
        (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration Const (Id "patata") BigInt (CharLit 'a')]
                _)) -> True)

    it "accepts `|\\n|` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "|\\n|")
        (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration Const (Id "patata") BigInt (CharLit '\n')]
                _)) -> True)
    it "accepts `@@` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "@@")
        (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration Const (Id "patata") BigInt (StringLit "")]
                _)) -> True)
    it "accepts `@hello@` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "@hello@")
        (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration Const (Id "patata") BigInt (StringLit "hello")]
                _)) -> True)
    it "accepts `@\\@@` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "@\\@@")
        (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration Const (Id "patata") BigInt (StringLit "@")]
                _)) -> True)
    it "accepts `1.123` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "1.123")
        (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration Const (Id "patata") BigInt (FloatLit 1.123)]
                _)) -> True)
    it "accepts `0.0` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "0.0")
        (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration Const (Id "patata") BigInt (FloatLit 0)]
                _)) -> True)
