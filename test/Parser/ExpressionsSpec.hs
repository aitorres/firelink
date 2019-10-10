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

runTestForExpr expr expr' = runTestForValidProgram (buildProgramWithExpr expr)
    (\(Program _ _ (
        CodeBlock
            [InitializedDeclaration Const (Id "patata") BigInt expr']
            _)) -> True)

spec :: Spec
spec = describe "Expressions" $ do
    it "accepts `1 + 2` as an expression" $
        runTestForExpr "1 + 2" $ Add (IntLit 1) (IntLit 2)
    it "accepts `1 + 2 + 3` as an expression and associates to the left" $
        runTestForExpr "1 + 2 + 3" $ Add (Add (IntLit 1) (IntLit 2)) (IntLit 3)
    it "accepts `1 - 2` as an expression" $
        runTestForExpr "1 - 2" $Substract (IntLit 1) (IntLit 2)
    it "accepts `1 - 2 - 3` as an expression and associates to the left" $
        runTestForExpr "1 - 2 - 3" $ Substract
                                        (Substract (IntLit 1) (IntLit 2))
                                        (IntLit 3)
    it "accepts `1 - 2 + 3` as an expression and associates to the left (1 - 2) + 3" $
        runTestForExpr "1 - 2 + 3" $ Add (Substract (IntLit 1) (IntLit 2)) (IntLit 3)
    it "accepts `1 + 2 - 3` as an expression and associates to the left (1 + 2) - 3" $
        runTestForExpr "1 + 2 - 3" $ Substract (Add (IntLit 1) (IntLit 2)) (IntLit 3)
    it "accepts `1 * 2` as an expression" $
        runTestForExpr "1 * 2" $ Multiply (IntLit 1) (IntLit 2)
    it "accepts `1 * 2 * 3` as an expression and associates to the left" $
        runTestForExpr "1 * 2 * 3" $ Multiply (Multiply (IntLit 1) (IntLit 2)) (IntLit 3)
    it "accepts `1 * 2 + 3` as an expression and associates to the left" $
        runTestForExpr "1 * 2 + 3" $ Add
                                        (Multiply (IntLit 1) (IntLit 2))
                                        (IntLit 3)
    it "accepts `1 + 2 * 3` as an expression and associates to the left" $
        runTestForExpr "1 + 2 * 3" $ Add
                                        (IntLit 1)
                                        (Multiply (IntLit 2) (IntLit 3))
    it "accepts `1 / 2` as an expression" $
        runTestForExpr "1 / 2" $ Divide (IntLit 1) (IntLit 2)
    it "accepts `1 / 2 / 3` as an expression and associates to the left" $
        runTestForExpr "1 / 2 / 3" $ Divide
                                        (Divide (IntLit 1) (IntLit 2))
                                        (IntLit 3)
    it "accepts `1 / 2 + 3` as an expression and associates to the left" $
        runTestForExpr "1 / 2 + 3" $ Add
                                        (Divide (IntLit 1) (IntLit 2))
                                        (IntLit 3)
    it "accepts `1 + 2 / 3` as an expression and associates to the left" $
        runTestForExpr "1 + 2 / 3" $ Add
                                        (IntLit 1)
                                        (Divide (IntLit 2) (IntLit 3))
    it "accepts `1 % 2` as an expression" $
        runTestForExpr "1 % 2" $ Mod (IntLit 1) (IntLit 2)
    it "accepts `1 % 2 % 3` as an expression and associates to the left" $
        runTestForExpr "1 % 2 % 3" $ Mod
                                        (Mod (IntLit 1) (IntLit 2))
                                        (IntLit 3)
    it "accepts `1 % 2 + 3` as an expression and associates to the left" $
        runTestForExpr "1 % 2 + 3" $ Add
                                        (Mod (IntLit 1) (IntLit 2))
                                        (IntLit 3)
    it "accepts `1 + 2 % 3` as an expression and parse % as more precedent" $
        runTestForExpr "1 + 2 % 3" $ Add
                                        (IntLit 1)
                                        (Mod (IntLit 2) (IntLit 3))
    it "accepts `- 1` as an expression and associates to the left" $
        runTestForExpr "- 1" $ Negative (IntLit 1)
    it "rejects `- - 1` as an expression and associates to the left" $
        runTestForExpr "- - 1" $ Negative $ Negative $ IntLit 1
    it "accepts `1 lt 2` as an expression and associates to the left" $
        runTestForExpr "1 lt 2" $ Lt (IntLit 1) (IntLit 2)
    it "accepts `1 lt 2 + 3` as an expression and associates to the right (1 lt (2 + 3))" $
        runTestForExpr "1 lt 2 + 3" $ Lt
                                        (IntLit 1)
                                        (Add (IntLit 2) (IntLit 3))
    it "accepts `1 lte 2` as an expression and associates to the left" $
        runTestForExpr "1 lte 2" $ Lte (IntLit 1) (IntLit 2)
    it "accepts `1 lte 2 + 3` as an expression and associates to the right (1 lte (2 + 3))" $
        runTestForExpr "1 lte 2 + 3" $ Lte
                                        (IntLit 1)
                                        (Add (IntLit 2) (IntLit 3))
    it "accepts `1 gt 2` as an expression and associates to the left" $
        runTestForExpr "1 gt 2" $ Gt (IntLit 1) (IntLit 2)
    it "accepts `1 gt 2 + 3` as an expression and associates to the right (1 gt (2 + 3))" $
        runTestForExpr "1 gt 2 + 3" $ Gt
                                        (IntLit 1)
                                        (Add (IntLit 2) (IntLit 3))
    it "accepts `1 gte 2` as an expression and associates to the left" $
        runTestForExpr "1 gte 2" $ Gte (IntLit 1) (IntLit 2)
    it "accepts `1 gte 2 + 3` as an expression and associates to the right (1 gte (2 + 3))" $
        runTestForExpr "1 gte 2 + 3" $ Gte
                                        (IntLit 1)
                                        (Add (IntLit 2) (IntLit 3))
    it "accepts `1 eq 2` as an expression and associates to the left" $
        runTestForExpr "1 eq 2" $ Eq (IntLit 1) (IntLit 2)
    it "accepts `1 eq 2 + 3` as an expression and associates to the right (1 eq (2 + 3))" $
        runTestForExpr "1 eq 2 + 3" $ Eq
                                        (IntLit 1)
                                        (Add (IntLit 2) (IntLit 3))
    it "accepts `1 eq 2 eq 3 as an expression and associates to the left (1 eq 2) eq 3" $
        runTestForExpr "1 eq 2 eq 3" $ Eq
                                        (Eq (IntLit 1) (IntLit 2))
                                        (IntLit 3)
    it "accepts `1 neq 2` as an expression and associates to the left" $
        runTestForExpr "1 neq 2" $ Neq (IntLit 1) (IntLit 2)
    it "accepts `1 neq 2 + 3` as an expression and associates to the right (1 neq (2 + 3))" $
        runTestForExpr "1 neq 2 + 3" $ Neq
                                        (IntLit 1)
                                        (Add (IntLit 2) (IntLit 3))
    it "accepts `1 neq 2 neq 3 as an expression and associates to the left (1 neq 2) neq 3" $
        runTestForExpr "1 neq 2 neq 3" $ Neq
                                        (Neq (IntLit 1) (IntLit 2))
                                        (IntLit 3)
    it "accepts `1 eq 2 neq 3 as an expression and associates to the left (1 eq 2) neq 3" $
        runTestForExpr "1 eq 2 neq 3" $ Neq
                                        (Eq (IntLit 1) (IntLit 2))
                                        (IntLit 3)

    it "accepts `lit and unlit` as an expression" $
        runTestForExpr "lit and unlit" $ And TrueLit FalseLit
    it "accepts `lit or unlit` as an expression" $
        runTestForExpr "lit or unlit" $ Or TrueLit FalseLit
    it "accepts `not unlit` as an expression" $
        runTestForExpr "not unlit" $ Not FalseLit
    it "accepts `lit and unlit and undiscovered` as an expression associating to the left `(lit and unlit) and undiscovered`" $
        runTestForExpr "lit and unlit" $ And (And TrueLit FalseLit) UndiscoveredLit
    it "accepts `not lit and unlit` as an expression and `not` has more precedence" $
        runTestForExpr "not lit and unlit" $ Not $ And TrueLit FalseLit

    it "accepts `ascii_of |n|` as an expression" $
        runTestForExpr "ascii_of |n|" $ AsciiOf $ CharLit 'n'

    it "accepts `ascii_of |n| + ascii_of |f|` as an expression and parses it as `(ascii_of |n|) + (ascii_of |f|)" $
        runTestForExpr "ascii_of |n| + ascii_of |f|" $ Add
                                                        (AsciiOf $ CharLit 'n')
                                                        (AsciiOf $ CharLit 'f')

    it "accepts `<$$> >-< @@` as an expression" $
        runTestForExpr "<$$> >-< @@" $ ColConcat (ArrayLit []) (StringLit "")
    it "accepts `<$$> >-< @@ >-< {$$}` as an expression and should associate to the left `(<$$> >-< @@) >-< {$$}`" $
        runTestForExpr "<$$> >-< @@" $ ColConcat
                                            (ColConcat (ArrayLit []) (StringLit ""))
                                            (SetLit [])
    it "accepts `lit union lit` as an expression" $
        runTestForExpr "lit union lit" $ SetUnion TrueLit TrueLit
    it "accepts `lit intersect lit` as an expression" $
        runTestForExpr "lit intersect lit" $ SetIntersect TrueLit TrueLit
    it "accepts `lit diff lit` as an expression" $
        runTestForExpr "lit diff lit" $ SetDiff TrueLit TrueLit
    it "accepts `size lit` as an expression" $
        runTestForExpr "size lit" $ SetSize TrueLit
