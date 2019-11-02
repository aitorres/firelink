module LiteralsSpec where

import Test.Hspec
import Utils
import Grammar

buildProgramWithLiteral :: String -> String
buildProgramWithLiteral l = "\
\ hello ashen one \

\ traveling somewhere \
\   go back with " ++ l ++ " \
\ you died \

\ farewell ashen one"

spec :: Spec
spec = describe "Literal Values" $ do
    it "accepts `abyss` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "abyss")
        (\(Program (
            CodeBlock [InstReturnWith NullLit])) -> True)
    it "accepts `123` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "123")
        (\(Program (
            CodeBlock [InstReturnWith (IntLit 123)])) -> True)
    it "accepts `lit` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "lit")
        (\(Program (
            CodeBlock [InstReturnWith TrueLit])) -> True)
    it "accepts `unlit` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "unlit")
        (\(Program (
            CodeBlock [InstReturnWith FalseLit])) -> True)
    it "accepts `undiscovered` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "undiscovered")
        (\(Program (
            CodeBlock [InstReturnWith UndiscoveredLit])) -> True)

    it "accepts `|a|` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "|a|")
        (\(Program (
            CodeBlock [InstReturnWith (CharLit 'a')])) -> True)

    it "accepts `|\\n|` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "|\\n|")
        (\(Program (
            CodeBlock [InstReturnWith (CharLit '\n')])) -> True)
    it "accepts `@@` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "@@")
        (\(Program (
            CodeBlock [InstReturnWith (StringLit "")])) -> True)
    it "accepts `@hello@` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "@hello@")
        (\(Program (
            CodeBlock [InstReturnWith (StringLit "hello")])) -> True)
    it "accepts `@\\@@` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "@\\@@")
        (\(Program (
            CodeBlock [InstReturnWith (StringLit "@")])) -> True)
    it "accepts `1.123` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "1.123")
        (\(Program (
            CodeBlock [InstReturnWith (FloatLit 1.123)])) -> True)
    it "accepts `0.0` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "0.0")
        (\(Program (
            CodeBlock [InstReturnWith (FloatLit 0)])) -> True)

    -- array literals
    it "accepts `<$$>` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "<$$>")
        (\(Program (
            CodeBlock [InstReturnWith (ArrayLit [])])) -> True)
    it "accepts `<$1$>` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "<$1$>")
        (\(Program (
            CodeBlock [InstReturnWith (ArrayLit [IntLit 1])])) -> True)
    it "rejects `<$1,$>` as a literal" $
        runTestForInvalidProgram (buildProgramWithLiteral "<$1,$>")
    it "rejects `<$,1$>` as a literal" $
        runTestForInvalidProgram (buildProgramWithLiteral "<$,1$>")
    it "accepts `<$1, 2$>` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "<$1, 2$>")
        (\(Program (
            CodeBlock [InstReturnWith (ArrayLit [IntLit 1, IntLit 2])])) -> True)
    it "accepts `<$<$$>$>` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "<$<$$>$>")
        (\(Program (
            CodeBlock [InstReturnWith (ArrayLit [ArrayLit []])])) -> True)

    -- set literals
    it "accepts `{$$}` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "{$$}")
        (\(Program (
            CodeBlock [InstReturnWith (SetLit [])])) -> True)
    it "accepts `{$1$}` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "{$1$}")
        (\(Program (
            CodeBlock [InstReturnWith (SetLit [IntLit  1])])) -> True)
    it "rejects `{$1,$}` as a literal" $
        runTestForInvalidProgram (buildProgramWithLiteral "{$1,$}")
    it "rejects `{$,1$}` as a literal" $
        runTestForInvalidProgram (buildProgramWithLiteral "{$,1$}")
    it "accepts `{$1, 2$}` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "{$1, 2$}")
        (\(Program (
            CodeBlock [InstReturnWith (SetLit [IntLit 1, IntLit 2])])) -> True)
    it "accepts `{${$$}$}` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "{${$$}$}")
        (\(Program (
            CodeBlock [InstReturnWith (SetLit [SetLit []])])) -> True)
