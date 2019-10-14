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

runTestForExpr expr = runTestForValidProgram (buildProgramWithExpr expr)

spec :: Spec
spec = describe "Expressions" $ do
    it "accepts `1 + 2` as an expression" $
        runTestForExpr "1 + 2" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (Add (IntLit 1) (IntLit 2))]
                _)) -> True)
    it "accepts `1 + 2 + 3` as an expression and associates to the left" $
        runTestForExpr "1 + 2 + 3" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (Add (Add (IntLit 1) (IntLit 2)) (IntLit 3))]
                _)) -> True)
    it "accepts `1 - 2` as an expression" $
        runTestForExpr "1 - 2" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (Substract (IntLit 1) (IntLit 2))]
                _)) -> True)
    it "accepts `1 - 2 - 3` as an expression and associates to the left" $
        runTestForExpr "1 - 2 - 3" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (
                    Substract
                        (Substract (IntLit 1) (IntLit 2))
                        (IntLit 3))]
                _)) -> True)
    it "accepts `1 - 2 + 3` as an expression and associates to the left (1 - 2) + 3" $
        runTestForExpr "1 - 2 + 3" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (
                    Add
                        (Substract (IntLit 1) (IntLit 2))
                        (IntLit 3))]
                _)) -> True)
    it "accepts `1 + 2 - 3` as an expression and associates to the left (1 + 2) - 3" $
        runTestForExpr "1 + 2 - 3" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (
                    Substract
                        (Add (IntLit 1) (IntLit 2))
                        (IntLit 3))]
                _)) -> True)
    it "accepts `1 * 2` as an expression" $
        runTestForExpr "1 * 2" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (Multiply (IntLit 1) (IntLit 2))]
                _)) -> True)
    it "accepts `1 * 2 * 3` as an expression and associates to the left" $
        runTestForExpr "1 * 2 * 3" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (
                    Multiply
                        (Multiply (IntLit 1) (IntLit 2))
                        (IntLit 3))]
                _)) -> True)
    it "accepts `1 * 2 + 3` as an expression and associates to the left" $
        runTestForExpr "1 * 2 + 3" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (
                    Add
                        (Multiply (IntLit 1) (IntLit 2))
                        (IntLit 3))]
                _)) -> True)
    it "accepts `1 + 2 * 3` as an expression and associates to the left" $
        runTestForExpr "1 + 2 * 3" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (
                    Add
                        (IntLit 1)
                        (Multiply (IntLit 2) (IntLit 3)))]
                _)) -> True)
    it "accepts `1 / 2` as an expression" $
        runTestForExpr "1 / 2" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (Divide (IntLit 1) (IntLit 2))]
                _)) -> True)
    it "accepts `1 / 2 / 3` as an expression and associates to the left" $
        runTestForExpr "1 / 2 / 3" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (
                    Divide
                        (Divide (IntLit 1) (IntLit 2))
                        (IntLit 3))]
                _)) -> True)
    it "accepts `1 / 2 + 3` as an expression and associates to the left" $
        runTestForExpr "1 / 2 + 3" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (
                    Add
                        (Divide (IntLit 1) (IntLit 2))
                        (IntLit 3))]
                _)) -> True)
    it "accepts `1 + 2 / 3` as an expression and associates to the left" $
        runTestForExpr "1 + 2 / 3" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (
                    Add
                        (IntLit 1)
                        (Divide (IntLit 2) (IntLit 3)))]
                _)) -> True)
    it "accepts `1 % 2` as an expression" $
        runTestForExpr "1 % 2" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (Mod (IntLit 1) (IntLit 2))]
                _)) -> True)
    it "accepts `1 % 2 % 3` as an expression and associates to the left" $
        runTestForExpr "1 % 2 % 3" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (
                    Mod
                        (Mod (IntLit 1) (IntLit 2))
                        (IntLit 3))]
                _)) -> True)
    it "accepts `1 % 2 + 3` as an expression and associates to the left" $
        runTestForExpr "1 % 2 + 3" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (
                    Add
                        (Mod (IntLit 1) (IntLit 2))
                        (IntLit 3))]
                _)) -> True)
    it "accepts `1 + 2 % 3` as an expression and parse % as more precedent" $
        runTestForExpr "1 + 2 % 3" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (
                    Add
                        (IntLit 1)
                        (Mod (IntLit 2) (IntLit 3)))]
                _)) -> True)
    it "accepts `- 1` as an expression and associates to the left" $
        runTestForExpr "- 1" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (Negative (IntLit 1))]
                _)) -> True)
    it "rejects `- - 1` as an expression and associates to the left" $
        runTestForExpr "- - 1" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (Negative (Negative (IntLit 1)))]
                _)) -> True)
    it "accepts `1 lt 2` as an expression and associates to the left" $
        runTestForExpr "1 lt 2" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (Lt (IntLit 1) (IntLit 2))]
                _)) -> True)
    it "accepts `1 lt 2 + 3` as an expression and associates to the right (1 lt (2 + 3))" $
        runTestForExpr "1 lt 2 + 3" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (
                    Lt
                        (IntLit 1)
                        (Add (IntLit 2) (IntLit 3)))]
                _)) -> True)
    it "accepts `1 lte 2` as an expression and associates to the left" $
        runTestForExpr "1 lte 2" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (Lte (IntLit 1) (IntLit 2))]
                _)) -> True)
    it "accepts `1 lte 2 + 3` as an expression and associates to the right (1 lte (2 + 3))" $
        runTestForExpr "1 lte 2 + 3" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (
                    Lte
                        (IntLit 1)
                        (Add (IntLit 2) (IntLit 3)))]
                _)) -> True)
    it "accepts `1 gt 2` as an expression and associates to the left" $
        runTestForExpr "1 gt 2" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (Gt (IntLit 1) (IntLit 2))]
                _)) -> True)
    it "accepts `1 gt 2 + 3` as an expression and associates to the right (1 gt (2 + 3))" $
        runTestForExpr "1 gt 2 + 3" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (
                    Gt
                        (IntLit 1)
                        (Add (IntLit 2) (IntLit 3)))]
                _)) -> True)
    it "accepts `1 gte 2` as an expression and associates to the left" $
        runTestForExpr "1 gte 2" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (Gte (IntLit 1) (IntLit 2))]
                _)) -> True)
    it "accepts `1 gte 2 + 3` as an expression and associates to the right (1 gte (2 + 3))" $
        runTestForExpr "1 gte 2 + 3" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (
                    Gte
                        (IntLit 1)
                        (Add (IntLit 2) (IntLit 3)))]
                _)) -> True)
    it "accepts `1 eq 2` as an expression and associates to the left" $
        runTestForExpr "1 eq 2" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (Eq (IntLit 1) (IntLit 2))]
                _)) -> True)
    it "accepts `1 eq 2 + 3` as an expression and associates to the right (1 eq (2 + 3))" $
        runTestForExpr "1 eq 2 + 3" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (
                    Eq
                        (IntLit 1)
                        (Add (IntLit 2) (IntLit 3)))]
                _)) -> True)
    it "accepts `1 eq 2 eq 3 as an expression and associates to the left (1 eq 2) eq 3" $
        runTestForExpr "1 eq 2 eq 3" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (
                    Eq
                        (Eq (IntLit 1) (IntLit 2))
                        (IntLit 3))]
                _)) -> True)
    it "accepts `1 neq 2` as an expression and associates to the left" $
        runTestForExpr "1 neq 2" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (Neq (IntLit 1) (IntLit 2))]
                _)) -> True)
    it "accepts `1 neq 2 + 3` as an expression and associates to the right (1 neq (2 + 3))" $
        runTestForExpr "1 neq 2 + 3" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (
                    Neq
                        (IntLit 1)
                        (Add (IntLit 2) (IntLit 3)))]
                _)) -> True)
    it "accepts `1 neq 2 neq 3 as an expression and associates to the left (1 neq 2) neq 3" $
        runTestForExpr "1 neq 2 neq 3" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (
                    Neq
                        (Neq (IntLit 1) (IntLit 2))
                        (IntLit 3))]
                _)) -> True)
    it "accepts `1 eq 2 neq 3 as an expression and associates to the left (1 eq 2) neq 3" $
        runTestForExpr "1 eq 2 neq 3" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (
                    Neq
                        (Eq (IntLit 1) (IntLit 2))
                        (IntLit 3))]
                _)) -> True)

    it "accepts `lit and unlit` as an expression" $
        runTestForExpr "lit and unlit" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (And TrueLit FalseLit)]
                _)) -> True)
    it "accepts `lit or unlit` as an expression" $
        runTestForExpr "lit or unlit" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (Or TrueLit FalseLit)]
                _)) -> True)
    it "accepts `not unlit` as an expression" $
        runTestForExpr "not unlit" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (Not FalseLit)]
                _)) -> True)
    it "accepts `lit and unlit and undiscovered` as an expression associating to the left `(lit and unlit) and undiscovered`" $
        runTestForExpr "lit and unlit and undiscovered" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (And (And TrueLit FalseLit) UndiscoveredLit)]
                _)) -> True)
    it "accepts `not lit and unlit` as an expression and `not` has more precedence" $
        runTestForExpr "not lit and unlit" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _
                    (And (Not TrueLit) FalseLit)]
                _)) -> True)

    it "accepts `ascii_of |n|` as an expression" $
        runTestForExpr "ascii_of |n|" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (AsciiOf (CharLit 'n'))]
                _)) -> True)

    it "accepts `ascii_of |n| + ascii_of |f|` as an expression and parses it as `(ascii_of |n|) + (ascii_of |f|)" $
        runTestForExpr "ascii_of |n| + ascii_of |f|" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (
                    Add
                        (AsciiOf (CharLit 'n'))
                        (AsciiOf (CharLit 'f')))]
                _)) -> True)

    it "accepts `<$$> >-< @@` as an expression" $
        runTestForExpr "<$$> >-< @@" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (ColConcat (ArrayLit []) (StringLit ""))]
                _)) -> True)
    it "accepts `<$$> >-< @@ >-< {$$}` as an expression and should associate to the left `(<$$> >-< @@) >-< {$$}`" $
        runTestForExpr "<$$> >-< @@ >-< {$$}" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (
                    ColConcat
                        (ColConcat (ArrayLit []) (StringLit ""))
                        (SetLit []))]
                _)) -> True)
    it "accepts `lit union lit` as an expression" $
        runTestForExpr "lit union lit" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (SetUnion TrueLit TrueLit)]
                _)) -> True)
    it "accepts `lit intersect lit` as an expression" $
        runTestForExpr "lit intersect lit" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (SetIntersect TrueLit TrueLit)]
                _)) -> True)
    it "accepts `lit diff lit` as an expression" $
        runTestForExpr "lit diff lit" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (SetDiff TrueLit TrueLit)]
                _)) -> True)
    it "accepts `size lit` as an expression" $
        runTestForExpr "size lit" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (
                    SetSize TrueLit)]
                _)) -> True)

    it "accepts `lit union lit union lit` as an expression and associates to the left" $
        runTestForExpr "lit union lit union lit" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (
                    SetUnion (SetUnion TrueLit TrueLit) TrueLit)]
                _)) -> True)
    it "accepts `lit intersect lit intersect lit` as an expression and associates to the left" $
        runTestForExpr "lit intersect lit intersect lit" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (
                    SetIntersect (SetIntersect TrueLit TrueLit) TrueLit)]
                _)) -> True)
    it "accepts `lit diff lit diff lit` as an expression" $
        runTestForExpr "lit diff lit diff lit" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (
                    SetDiff (SetDiff TrueLit TrueLit) TrueLit)]
                _)) -> True)
    it "accepts `lit diff lit union lit` as an expression" $
        runTestForExpr "lit diff lit union lit" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (
                    SetDiff
                        TrueLit
                        (SetUnion TrueLit TrueLit)
                        )]
                _)) -> True)

    it "accepts `lit union lit intersect lit` as an expression and associates to the left" $
        runTestForExpr "lit union lit intersect lit" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (
                    SetIntersect
                        (SetUnion TrueLit TrueLit)
                        TrueLit
                        )]
                _)) -> True)
    it "accepts `lit intersect lit union lit` as an expression and associates to the left" $
        runTestForExpr "lit intersect lit union lit" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (
                    SetUnion
                        (SetIntersect TrueLit TrueLit)
                        TrueLit)]
                _)) -> True)

    it "accepts `a ~> b` as an expression" $
        runTestForExpr "a ~> b" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (Access (IdExpr (Id "a")) (Id "b"))]
                _)) -> True)
    it "accepts `a ~> b ~> c` as an expression" $
        runTestForExpr "a ~> b ~> c" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (
                    Access
                        (Access (IdExpr (Id "a")) (Id "b"))
                        (Id "c"))]
                _)) -> True)
    it "accepts `a + b ~> c` as an expression" $
        runTestForExpr "a + b ~> c" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (
                    Access
                        (Add (IdExpr (Id "a")) (IdExpr (Id "b")))
                        (Id "c"))]
                _)) -> True)

    it "accepts `(a)` as an expression" $
        runTestForExpr "(a)" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (IdExpr (Id "a"))]
                _)) -> True)

    it "accepts `a<$i$>` as an expression" $
        runTestForExpr "a<$i$>" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (
                    IndexAccess
                        (IdExpr (Id "a"))
                        (IdExpr (Id "i")))]
                _)) -> True)
    it "accepts `a+b<$i$>` as an expression" $
        runTestForExpr "a+b<$i$>" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (
                    IndexAccess
                        (Add (IdExpr (Id "a")) (IdExpr (Id "b")))
                        (IdExpr (Id "i")))]
                _)) -> True)
    it "rejects `a<$$>` as an expression" $
        runTestForInvalidProgram "a<$$>"

    it "accepts `abyss` as an expression" $
        runTestForExpr "abyss" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ NullLit]
                _)) -> True)
    it "accepts `throw a a` as an expression" $
        runTestForExpr "throw a a" (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration _ _ _ (MemAccess (IdExpr (Id "a")))]
                _)) -> True)
    it "rejects `aim a a` as an expression" $
        runTestForInvalidProgram "aim a a"
    it "rejects `recover a a` as an expresion" $
        runTestForInvalidProgram "recover a a"
