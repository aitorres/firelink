module ExpressionsSpec where

import Test.Hspec
import Utils
import Grammar
import Lexer

buildProgramWithExpr :: String -> String
buildProgramWithExpr e = "\
\ hello ashen one \

\ traveling somewhere \
\   go back with " ++ e ++ " \
\ you died \

\ farewell ashen one"

runTestForExpr :: String -> (Program -> Bool) -> IO ()
runTestForExpr expr = runTestForValidProgram (buildProgramWithExpr expr)

spec :: Spec
spec = describe "Expressions" $ do
    it "accepts `1 + 2` as an expression" $
        runTestForExpr "1 + 2" (\(Program (
            CodeBlock
                [InstReturnWith (Add (IntLit 1) (IntLit 2))])) -> True)
    it "accepts `1 + 2 + 3` as an expression and associates to the left" $
        runTestForExpr "1 + 2 + 3" (\(Program (
            CodeBlock
                [InstReturnWith (Add (Add (IntLit 1) (IntLit 2)) (IntLit 3))])) -> True)
    it "accepts `1 - 2` as an expression" $
        runTestForExpr "1 - 2" (\(Program (
            CodeBlock
                [InstReturnWith (Substract (IntLit 1) (IntLit 2))])) -> True)
    it "accepts `1 - 2 - 3` as an expression and associates to the left" $
        runTestForExpr "1 - 2 - 3" (\(Program (
            CodeBlock
                [InstReturnWith (
                    Substract
                        (Substract (IntLit 1) (IntLit 2))
                        (IntLit 3))])) -> True)
    it "accepts `1 - 2 + 3` as an expression and associates to the left (1 - 2) + 3" $
        runTestForExpr "1 - 2 + 3" (\(Program (
            CodeBlock
                [InstReturnWith (
                    Add
                        (Substract (IntLit 1) (IntLit 2))
                        (IntLit 3))])) -> True)
    it "accepts `1 + 2 - 3` as an expression and associates to the left (1 + 2) - 3" $
        runTestForExpr "1 + 2 - 3" (\(Program (
            CodeBlock
                [InstReturnWith (
                    Substract
                        (Add (IntLit 1) (IntLit 2))
                        (IntLit 3))])) -> True)
    it "accepts `1 * 2` as an expression" $
        runTestForExpr "1 * 2" (\(Program (
            CodeBlock
                [InstReturnWith (Multiply (IntLit 1) (IntLit 2))])) -> True)
    it "accepts `1 * 2 * 3` as an expression and associates to the left" $
        runTestForExpr "1 * 2 * 3" (\(Program (
            CodeBlock
                [InstReturnWith (
                    Multiply
                        (Multiply (IntLit 1) (IntLit 2))
                        (IntLit 3))])) -> True)
    it "accepts `1 * 2 + 3` as an expression and associates to the left" $
        runTestForExpr "1 * 2 + 3" (\(Program (
            CodeBlock
                [InstReturnWith (
                    Add
                        (Multiply (IntLit 1) (IntLit 2))
                        (IntLit 3))])) -> True)
    it "accepts `1 + 2 * 3` as an expression and associates to the left" $
        runTestForExpr "1 + 2 * 3" (\(Program (
            CodeBlock
                [InstReturnWith (
                    Add
                        (IntLit 1)
                        (Multiply (IntLit 2) (IntLit 3)))])) -> True)
    it "accepts `1 / 2` as an expression" $
        runTestForExpr "1 / 2" (\(Program (
            CodeBlock
                [InstReturnWith (Divide (IntLit 1) (IntLit 2))])) -> True)
    it "accepts `1 / 2 / 3` as an expression and associates to the left" $
        runTestForExpr "1 / 2 / 3" (\(Program (
            CodeBlock
                [InstReturnWith (
                    Divide
                        (Divide (IntLit 1) (IntLit 2))
                        (IntLit 3))])) -> True)
    it "accepts `1 / 2 + 3` as an expression and associates to the left" $
        runTestForExpr "1 / 2 + 3" (\(Program (
            CodeBlock
                [InstReturnWith (
                    Add
                        (Divide (IntLit 1) (IntLit 2))
                        (IntLit 3))])) -> True)
    it "accepts `1 + 2 / 3` as an expression and associates to the left" $
        runTestForExpr "1 + 2 / 3" (\(Program (
            CodeBlock
                [InstReturnWith (
                    Add
                        (IntLit 1)
                        (Divide (IntLit 2) (IntLit 3)))])) -> True)
    it "accepts `1 % 2` as an expression" $
        runTestForExpr "1 % 2" (\(Program (
            CodeBlock
                [InstReturnWith (Mod (IntLit 1) (IntLit 2))])) -> True)
    it "accepts `1 % 2 % 3` as an expression and associates to the left" $
        runTestForExpr "1 % 2 % 3" (\(Program (
            CodeBlock
                [InstReturnWith (
                    Mod
                        (Mod (IntLit 1) (IntLit 2))
                        (IntLit 3))])) -> True)
    it "accepts `1 % 2 + 3` as an expression and associates to the left" $
        runTestForExpr "1 % 2 + 3" (\(Program (
            CodeBlock
                [InstReturnWith (
                    Add
                        (Mod (IntLit 1) (IntLit 2))
                        (IntLit 3))])) -> True)
    it "accepts `1 + 2 % 3` as an expression and parse % as more precedent" $
        runTestForExpr "1 + 2 % 3" (\(Program (
            CodeBlock
                [InstReturnWith (
                    Add
                        (IntLit 1)
                        (Mod (IntLit 2) (IntLit 3)))])) -> True)
    it "accepts `- 1` as an expression and associates to the left" $
        runTestForExpr "- 1" (\(Program (
            CodeBlock
                [InstReturnWith (Negative (IntLit 1))])) -> True)
    it "rejects `- - 1` as an expression and associates to the left" $
        runTestForExpr "- - 1" (\(Program (
            CodeBlock
                [InstReturnWith (Negative (Negative (IntLit 1)))])) -> True)
    it "accepts `1 lt 2` as an expression and associates to the left" $
        runTestForExpr "1 lt 2" (\(Program (
            CodeBlock
                [InstReturnWith (Lt (IntLit 1) (IntLit 2))])) -> True)
    it "accepts `1 lt 2 + 3` as an expression and associates to the right (1 lt (2 + 3))" $
        runTestForExpr "1 lt 2 + 3" (\(Program (
            CodeBlock
                [InstReturnWith (
                    Lt
                        (IntLit 1)
                        (Add (IntLit 2) (IntLit 3)))])) -> True)
    it "accepts `1 lte 2` as an expression and associates to the left" $
        runTestForExpr "1 lte 2" (\(Program (
            CodeBlock
                [InstReturnWith (Lte (IntLit 1) (IntLit 2))])) -> True)
    it "accepts `1 lte 2 + 3` as an expression and associates to the right (1 lte (2 + 3))" $
        runTestForExpr "1 lte 2 + 3" (\(Program (
            CodeBlock
                [InstReturnWith (
                    Lte
                        (IntLit 1)
                        (Add (IntLit 2) (IntLit 3)))])) -> True)
    it "accepts `1 gt 2` as an expression and associates to the left" $
        runTestForExpr "1 gt 2" (\(Program (
            CodeBlock
                [InstReturnWith (Gt (IntLit 1) (IntLit 2))])) -> True)
    it "accepts `1 gt 2 + 3` as an expression and associates to the right (1 gt (2 + 3))" $
        runTestForExpr "1 gt 2 + 3" (\(Program (
            CodeBlock
                [InstReturnWith (
                    Gt
                        (IntLit 1)
                        (Add (IntLit 2) (IntLit 3)))])) -> True)
    it "accepts `1 gte 2` as an expression and associates to the left" $
        runTestForExpr "1 gte 2" (\(Program (
            CodeBlock
                [InstReturnWith (Gte (IntLit 1) (IntLit 2))])) -> True)
    it "accepts `1 gte 2 + 3` as an expression and associates to the right (1 gte (2 + 3))" $
        runTestForExpr "1 gte 2 + 3" (\(Program (
            CodeBlock
                [InstReturnWith (
                    Gte
                        (IntLit 1)
                        (Add (IntLit 2) (IntLit 3)))])) -> True)
    it "accepts `1 eq 2` as an expression and associates to the left" $
        runTestForExpr "1 eq 2" (\(Program (
            CodeBlock
                [InstReturnWith (Eq (IntLit 1) (IntLit 2))])) -> True)
    it "accepts `1 eq 2 + 3` as an expression and associates to the right (1 eq (2 + 3))" $
        runTestForExpr "1 eq 2 + 3" (\(Program (
            CodeBlock
                [InstReturnWith (
                    Eq
                        (IntLit 1)
                        (Add (IntLit 2) (IntLit 3)))])) -> True)
    it "accepts `1 eq 2 eq 3 as an expression and associates to the left (1 eq 2) eq 3" $
        runTestForExpr "1 eq 2 eq 3" (\(Program (
            CodeBlock
                [InstReturnWith (
                    Eq
                        (Eq (IntLit 1) (IntLit 2))
                        (IntLit 3))])) -> True)
    it "accepts `1 neq 2` as an expression and associates to the left" $
        runTestForExpr "1 neq 2" (\(Program (
            CodeBlock
                [InstReturnWith (Neq (IntLit 1) (IntLit 2))])) -> True)
    it "accepts `1 neq 2 + 3` as an expression and associates to the right (1 neq (2 + 3))" $
        runTestForExpr "1 neq 2 + 3" (\(Program (
            CodeBlock
                [InstReturnWith (
                    Neq
                        (IntLit 1)
                        (Add (IntLit 2) (IntLit 3)))])) -> True)
    it "accepts `1 neq 2 neq 3 as an expression and associates to the left (1 neq 2) neq 3" $
        runTestForExpr "1 neq 2 neq 3" (\(Program (
            CodeBlock
                [InstReturnWith (
                    Neq
                        (Neq (IntLit 1) (IntLit 2))
                        (IntLit 3))])) -> True)
    it "accepts `1 eq 2 neq 3 as an expression and associates to the left (1 eq 2) neq 3" $
        runTestForExpr "1 eq 2 neq 3" (\(Program (
            CodeBlock
                [InstReturnWith (
                    Neq
                        (Eq (IntLit 1) (IntLit 2))
                        (IntLit 3))])) -> True)

    it "accepts `lit and unlit` as an expression" $
        runTestForExpr "lit and unlit" (\(Program (
            CodeBlock
                [InstReturnWith (And TrueLit FalseLit)])) -> True)
    it "accepts `lit or unlit` as an expression" $
        runTestForExpr "lit or unlit" (\(Program (
            CodeBlock
                [InstReturnWith (Or TrueLit FalseLit)])) -> True)
    it "accepts `not unlit` as an expression" $
        runTestForExpr "not unlit" (\(Program (
            CodeBlock
                [InstReturnWith (Not FalseLit)])) -> True)
    it "accepts `lit and unlit and undiscovered` as an expression associating to the left `(lit and unlit) and undiscovered`" $
        runTestForExpr "lit and unlit and undiscovered" (\(Program (
            CodeBlock
                [InstReturnWith (And (And TrueLit FalseLit) UndiscoveredLit)])) -> True)
    it "accepts `not lit and unlit` as an expression and `not` has more precedence" $
        runTestForExpr "not lit and unlit" (\(Program (
            CodeBlock
                [InstReturnWith
                    (And (Not TrueLit) FalseLit)])) -> True)

    it "accepts `ascii_of |n|` as an expression" $
        runTestForExpr "ascii_of |n|" (\(Program (
            CodeBlock
                [InstReturnWith (AsciiOf (CharLit 'n'))])) -> True)

    it "accepts `ascii_of |n| + ascii_of |f|` as an expression and parses it as `(ascii_of |n|) + (ascii_of |f|)" $
        runTestForExpr "ascii_of |n| + ascii_of |f|" (\(Program (
            CodeBlock
                [InstReturnWith (
                    Add
                        (AsciiOf (CharLit 'n'))
                        (AsciiOf (CharLit 'f')))])) -> True)

    it "accepts `<$$> >-< @@` as an expression" $
        runTestForExpr "<$$> >-< @@" (\(Program (
            CodeBlock
                [InstReturnWith (ColConcat (ArrayLit []) (StringLit ""))])) -> True)
    it "accepts `<$$> >-< @@ >-< {$$}` as an expression and should associate to the left `(<$$> >-< @@) >-< {$$}`" $
        runTestForExpr "<$$> >-< @@ >-< {$$}" (\(Program (
            CodeBlock
                [InstReturnWith (
                    ColConcat
                        (ColConcat (ArrayLit []) (StringLit ""))
                        (SetLit []))])) -> True)
    it "accepts `lit union lit` as an expression" $
        runTestForExpr "lit union lit" (\(Program (
            CodeBlock
                [InstReturnWith (SetUnion TrueLit TrueLit)])) -> True)
    it "accepts `lit intersect lit` as an expression" $
        runTestForExpr "lit intersect lit" (\(Program (
            CodeBlock
                [InstReturnWith (SetIntersect TrueLit TrueLit)])) -> True)
    it "accepts `lit diff lit` as an expression" $
        runTestForExpr "lit diff lit" (\(Program (
            CodeBlock
                [InstReturnWith (SetDiff TrueLit TrueLit)])) -> True)
    it "accepts `size lit` as an expression" $
        runTestForExpr "size lit" (\(Program (
            CodeBlock
                [InstReturnWith (
                    SetSize TrueLit)])) -> True)

    it "accepts `lit union lit union lit` as an expression and associates to the left" $
        runTestForExpr "lit union lit union lit" (\(Program (
            CodeBlock
                [InstReturnWith (
                    SetUnion (SetUnion TrueLit TrueLit) TrueLit)])) -> True)
    it "accepts `lit intersect lit intersect lit` as an expression and associates to the left" $
        runTestForExpr "lit intersect lit intersect lit" (\(Program (
            CodeBlock
                [InstReturnWith (
                    SetIntersect (SetIntersect TrueLit TrueLit) TrueLit)])) -> True)
    it "accepts `lit diff lit diff lit` as an expression" $
        runTestForExpr "lit diff lit diff lit" (\(Program (
            CodeBlock
                [InstReturnWith (
                    SetDiff (SetDiff TrueLit TrueLit) TrueLit)])) -> True)
    it "accepts `lit diff lit union lit` as an expression" $
        runTestForExpr "lit diff lit union lit" (\(Program (
            CodeBlock
                [InstReturnWith (
                    SetDiff
                        TrueLit
                        (SetUnion TrueLit TrueLit)
                        )])) -> True)

    it "accepts `lit union lit intersect lit` as an expression and associates to the left" $
        runTestForExpr "lit union lit intersect lit" (\(Program (
            CodeBlock
                [InstReturnWith (
                    SetIntersect
                        (SetUnion TrueLit TrueLit)
                        TrueLit
                        )])) -> True)
    it "accepts `lit intersect lit union lit` as an expression and associates to the left" $
        runTestForExpr "lit intersect lit union lit" (\(Program (
            CodeBlock
                [InstReturnWith (
                    SetUnion
                        (SetIntersect TrueLit TrueLit)
                        TrueLit)])) -> True)

    it "accepts `a ~> b` as an expression" $
        runTestForExpr "a ~> b" (\(Program (
            CodeBlock
                [InstReturnWith (Access (IdExpr (Id (Token _ (Just "a") _))) (Id (Token _ (Just "b") _)))])) -> True)
    it "accepts `a ~> b ~> c` as an expression" $
        runTestForExpr "a ~> b ~> c" (\(Program (
            CodeBlock
                [InstReturnWith (
                    Access
                        (Access (IdExpr (Id (Token _ (Just "a") _))) (Id (Token _ (Just "b") _)))
                        (Id (Token _ (Just "c") _)))])) -> True)
    it "accepts `a + b ~> c` as an expression" $
        runTestForExpr "a + b ~> c" (\(Program (
            CodeBlock
                [InstReturnWith (
                    Add
                        (IdExpr (Id (Token _ (Just "a") _)))
                        (Access
                            (IdExpr (Id (Token _ (Just "b") _)))
                            (Id (Token _ (Just "c") _))))])) -> True)

    it "accepts `(a)` as an expression" $
        runTestForExpr "(a)" (\(Program (
            CodeBlock
                [InstReturnWith (IdExpr (Id (Token _ (Just "a") _)))])) -> True)

    it "accepts `a<$i$>` as an expression" $
        runTestForExpr "a<$i$>" (\(Program (
            CodeBlock
                [InstReturnWith (
                    IndexAccess
                        (IdExpr (Id (Token _ (Just "a") _)))
                        (IdExpr (Id (Token _ (Just "i") _))))])) -> True)
    it "accepts `a+b<$i$>` as an expression" $
        runTestForExpr "a+b<$i$>" (\(Program (
            CodeBlock
                [InstReturnWith (
                    Add
                        (IdExpr (Id (Token _ (Just "a") _)))
                        (IndexAccess
                            (IdExpr (Id (Token _ (Just "b") _)))
                            (IdExpr (Id (Token _ (Just "i") _)))))])) -> True)
    it "rejects `a<$$>` as an expression" $
        runTestForInvalidProgram "a<$$>"

    it "accepts `abyss` as an expression" $
        runTestForExpr "abyss" (\(Program (
            CodeBlock
                [InstReturnWith NullLit])) -> True)
    it "accepts `throw a a` as an expression" $
        runTestForExpr "throw a a" (\(Program (
            CodeBlock
                [InstReturnWith (MemAccess (IdExpr (Id (Token _ (Just "a") _))))])) -> True)
    it "rejects `aim a a` as an expression" $
        runTestForInvalidProgram "aim a a"
    it "rejects `recover a a` as an expresion" $
        runTestForInvalidProgram "recover a a"

    it "accepts `summon f` a an expression" $
        runTestForExpr "summon f" (\(Program (
            CodeBlock
                [InstReturnWith (EvalFunc (Id (Token _ (Just "f") _)) [])])) -> True)
