module LiteralsSpec where

import           FireLink.FrontEnd.Grammar
import           Test.Hspec
import           Utils

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
            CodeBlock [InstReturnWith Expr{expAst=NullLit}, InstReturn] _)) -> True)
    it "accepts `123` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "123")
        (\(Program (
            CodeBlock [InstReturnWith Expr{expAst=(IntLit 123)}, InstReturn] _)) -> True)
    it "accepts `lit` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "lit")
        (\(Program (
            CodeBlock [InstReturnWith Expr{expAst=TrueLit}, InstReturn] _)) -> True)
    it "accepts `unlit` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "unlit")
        (\(Program (
            CodeBlock [InstReturnWith Expr{expAst=FalseLit}, InstReturn] _)) -> True)
    it "accepts `undiscovered` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "undiscovered")
        (\(Program (
            CodeBlock [InstReturnWith Expr{expAst=UndiscoveredLit}, InstReturn] _)) -> True)

    it "accepts `|a|` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "|a|")
        (\(Program (
            CodeBlock [InstReturnWith Expr{expAst=(CharLit 'a')}, InstReturn] _)) -> True)

    it "accepts `|\\n|` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "|\\n|")
        (\(Program (
            CodeBlock [InstReturnWith Expr{expAst=(CharLit '\n')}, InstReturn] _)) -> True)
    it "accepts `@@` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "@@")
        (\(Program (
            CodeBlock [InstReturnWith Expr{expAst=(StringLit "")}, InstReturn] _)) -> True)
    it "accepts `@hello@` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "@hello@")
        (\(Program (
            CodeBlock [InstReturnWith Expr{expAst=(StringLit "hello")}, InstReturn] _)) -> True)
    it "accepts `@\\@@` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "@\\@@")
        (\(Program (
            CodeBlock [InstReturnWith Expr{expAst=(StringLit "@")}, InstReturn] _)) -> True)
    it "accepts `1.123` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "1.123")
        (\(Program (
            CodeBlock [InstReturnWith Expr{expAst=(FloatLit 1.123)}, InstReturn] _)) -> True)
    it "accepts `0.0` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "0.0")
        (\(Program (
            CodeBlock [InstReturnWith Expr{expAst=(FloatLit 0)}, InstReturn] _)) -> True)

    -- array literals
    it "accepts `<$$>` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "<$$>")
        (\(Program (
            CodeBlock [InstReturnWith Expr{expAst=(ArrayLit [])}, InstReturn] _)) -> True)
    it "accepts `<$1$>` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "<$1$>")
        (\(Program (
            CodeBlock [InstReturnWith Expr{expAst=(ArrayLit [
                Expr{expAst=IntLit 1}])}, InstReturn] _)) -> True)
    it "rejects `<$1,$>` as a literal" $
        runTestForInvalidProgram (buildProgramWithLiteral "<$1,$>")
    it "rejects `<$,1$>` as a literal" $
        runTestForInvalidProgram (buildProgramWithLiteral "<$,1$>")
    it "accepts `<$1, 2$>` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "<$1, 2$>")
        (\(Program (
            CodeBlock [InstReturnWith Expr{expAst=(ArrayLit [
                Expr{expAst=IntLit 1},
                Expr{expAst=IntLit 2}])}, InstReturn] _)) -> True)
    it "accepts `<$<$$>$>` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "<$<$$>$>")
        (\(Program (
            CodeBlock [InstReturnWith Expr{expAst=(ArrayLit [
                Expr{expAst=ArrayLit []}])}, InstReturn] _)) -> True)

    -- set literals
    it "accepts `{$$}` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "{$$}")
        (\(Program (
            CodeBlock [InstReturnWith Expr{expAst=SetLit []}, InstReturn] _)) -> True)
    it "accepts `{$1$}` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "{$1$}")
        (\(Program (
            CodeBlock [InstReturnWith Expr{expAst=(SetLit [
                Expr{expAst=IntLit  1}])}, InstReturn] _)) -> True)
    it "rejects `{$1,$}` as a literal" $
        runTestForInvalidProgram (buildProgramWithLiteral "{$1,$}")
    it "rejects `{$,1$}` as a literal" $
        runTestForInvalidProgram (buildProgramWithLiteral "{$,1$}")
    it "accepts `{$1, 2$}` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "{$1, 2$}")
        (\(Program (
            CodeBlock [InstReturnWith Expr{expAst=(SetLit [
                Expr{expAst=IntLit 1},
                Expr{expAst=IntLit 2}])}, InstReturn] _)) -> True)
    it "accepts `{${$$}$}` as a literal" $
        runTestForValidProgram (buildProgramWithLiteral "{${$$}$}")
        (\(Program (
            CodeBlock [InstReturnWith Expr{expAst=(SetLit [
                Expr{expAst=SetLit []}
                ])}, InstReturn] _)) -> True)
