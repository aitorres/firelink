module ExpressionsSpec where

import Test.Hspec
import Utils
import Grammar
import Tokens

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
                [InstReturnWith Expr{expAst=(
                    Add Expr{expAst=(IntLit 1)} Expr{expAst=(IntLit 2)}
                    )}])) -> True)
    it "accepts `1 + 2 + 3` as an expression and associates to the left" $
        runTestForExpr "1 + 2 + 3" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Add Expr{expAst=(Add Expr{expAst=(IntLit 1)} Expr{expAst=(IntLit 2)})} Expr{expAst=(IntLit 3)}
                    )}])) -> True)
    it "accepts `1 - 2` as an expression" $
        runTestForExpr "1 - 2" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(Substract Expr{expAst=(IntLit 1)} Expr{expAst=(IntLit 2)})}])) -> True)
    it "accepts `1 - 2 - 3` as an expression and associates to the left" $
        runTestForExpr "1 - 2 - 3" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Substract
                        Expr{expAst=(Substract Expr{expAst=(IntLit 1)} Expr{expAst=(IntLit 2)})}
                        Expr{expAst=(IntLit 3)})}])) -> True)
    it "accepts `1 - 2 + 3` as an expression and associates to the left (1 - 2) + 3" $
        runTestForExpr "1 - 2 + 3" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Add
                        Expr{expAst=(Substract Expr{expAst=(IntLit 1)} Expr{expAst=(IntLit 2)})}
                        Expr{expAst=(IntLit 3)})}])) -> True)
    it "accepts `1 + 2 - 3` as an expression and associates to the left (1 + 2) - 3" $
        runTestForExpr "1 + 2 - 3" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Substract
                        Expr{expAst=(Add Expr{expAst=(IntLit 1)} Expr{expAst=(IntLit 2)})}
                        Expr{expAst=(IntLit 3)})}])) -> True)
    it "accepts `1 * 2` as an expression" $
        runTestForExpr "1 * 2" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Multiply Expr{expAst=(IntLit 1)} Expr{expAst=(IntLit 2)})}])) -> True)
    it "accepts `1 * 2 * 3` as an expression and associates to the left" $
        runTestForExpr "1 * 2 * 3" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Multiply
                        Expr{expAst=(Multiply
                            Expr{expAst=(IntLit 1)}
                            Expr{expAst=(IntLit 2)}
                            )}
                        Expr{expAst=(IntLit 3)}
                    )}
                ])) -> True)
    it "accepts `1 * 2 + 3` as an expression and associates to the left" $
        runTestForExpr "1 * 2 + 3" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Add
                        Expr{expAst=(Multiply Expr{expAst=(IntLit 1)} Expr{expAst=(IntLit 2)})}
                        Expr{expAst=(IntLit 3)}
                    )}
                ])) -> True)
    it "accepts `1 + 2 * 3` as an expression and associates to the left" $
        runTestForExpr "1 + 2 * 3" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Add
                        Expr{expAst=(IntLit 1)}
                        Expr{expAst=(Multiply Expr{expAst=(IntLit 2)} Expr{expAst=(IntLit 3)})}
                    )}
                ])) -> True)
    it "accepts `1 / 2` as an expression" $
        runTestForExpr "1 / 2" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Divide
                        Expr{expAst=(IntLit 1)}
                        Expr{expAst=(IntLit 2)}
                    )}
                ])) -> True)
    it "accepts `1 / 2 / 3` as an expression and associates to the left" $
        runTestForExpr "1 / 2 / 3" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Divide
                        Expr{expAst=(Divide Expr{expAst=(IntLit 1)} Expr{expAst=(IntLit 2)})}
                        Expr{expAst=(IntLit 3)}
                    )}
                ])) -> True)
    it "accepts `1 / 2 + 3` as an expression and associates to the left" $
        runTestForExpr "1 / 2 + 3" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Add
                        Expr{expAst=(Divide
                            Expr{expAst=(IntLit 1)}
                            Expr{expAst=(IntLit 2)})}
                        Expr{expAst=(IntLit 3)}
                    )}
                ])) -> True)
    it "accepts `1 + 2 / 3` as an expression and associates to the left" $
        runTestForExpr "1 + 2 / 3" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Add
                        Expr{expAst=(IntLit 1)}
                        Expr{expAst=(Divide
                            Expr{expAst=(IntLit 2)}
                            Expr{expAst=(IntLit 3)})}
                    )}
                ])) -> True)
    it "accepts `1 % 2` as an expression" $
        runTestForExpr "1 % 2" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Mod
                        Expr{expAst=(IntLit 1)}
                        Expr{expAst=(IntLit 2)}
                    )}
                ])) -> True)
    it "accepts `1 % 2 % 3` as an expression and associates to the left" $
        runTestForExpr "1 % 2 % 3" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Mod
                        Expr{expAst=(Mod Expr{expAst=(IntLit 1)} Expr{expAst=(IntLit 2)})}
                        Expr{expAst=(IntLit 3)}
                    )}
                ])) -> True)
    it "accepts `1 % 2 + 3` as an expression and associates to the left" $
        runTestForExpr "1 % 2 + 3" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Add
                        Expr{expAst=(Mod Expr{expAst=(IntLit 1)} Expr{expAst=(IntLit 2)})}
                        Expr{expAst=(IntLit 3)}
                    )}
                ])) -> True)
    it "accepts `1 + 2 % 3` as an expression and parse % as more precedent" $
        runTestForExpr "1 + 2 % 3" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Add
                        Expr{expAst=(IntLit 1)}
                        Expr{expAst=(Mod Expr{expAst=(IntLit 2)} Expr{expAst=(IntLit 3)})}
                        )}])) -> True)
    it "accepts `- 1` as an expression and associates to the left" $
        runTestForExpr "- 1" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Negative Expr{expAst=(IntLit 1)})}])) -> True)
    it "rejects `- - 1` as an expression and associates to the left" $
        runTestForExpr "- - 1" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Negative Expr{expAst=(
                        Negative Expr{expAst=(IntLit 1)}
                        )}
                    )}
                ])) -> True)
    it "accepts `1 lt 2` as an expression and associates to the left" $
        runTestForExpr "1 lt 2" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Lt
                        Expr{expAst=(IntLit 1)}
                        Expr{expAst=(IntLit 2)}
                    )}
                ])) -> True)
    it "accepts `1 lt 2 + 3` as an expression and associates to the right (1 lt (2 + 3))" $
        runTestForExpr "1 lt 2 + 3" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Lt
                        Expr{expAst=(IntLit 1)}
                        Expr{expAst=(Add
                            Expr{expAst=(IntLit 2)}
                            Expr{expAst=(IntLit 3)}
                            )}
                        )}
                    ])) -> True)
    it "accepts `1 lte 2` as an expression and associates to the left" $
        runTestForExpr "1 lte 2" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Lte
                        Expr{expAst=(IntLit 1)}
                        Expr{expAst=(IntLit 2)}
                    )}
                ])) -> True)
    it "accepts `1 lte 2 + 3` as an expression and associates to the right (1 lte (2 + 3))" $
        runTestForExpr "1 lte 2 + 3" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Lte
                        Expr{expAst=(IntLit 1)}
                        Expr{expAst=(Add
                            Expr{expAst=(IntLit 2)}
                            Expr{expAst=(IntLit 3)}
                            )}
                    )}
                ])) -> True)
    it "accepts `1 gt 2` as an expression and associates to the left" $
        runTestForExpr "1 gt 2" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Gt
                        Expr{expAst=(IntLit 1)}
                        Expr{expAst=(IntLit 2)}
                        )}
                    ])) -> True)
    it "accepts `1 gt 2 + 3` as an expression and associates to the right (1 gt (2 + 3))" $
        runTestForExpr "1 gt 2 + 3" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Gt
                        Expr{expAst=(IntLit 1)}
                        Expr{expAst=(Add
                            Expr{expAst=(IntLit 2)}
                            Expr{expAst=(IntLit 3)}
                            )}
                        )}])) -> True)
    it "accepts `1 gte 2` as an expression and associates to the left" $
        runTestForExpr "1 gte 2" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Gte
                        Expr{expAst=(IntLit 1)}
                        Expr{expAst=(IntLit 2)}
                        )}])) -> True)
    it "accepts `1 gte 2 + 3` as an expression and associates to the right (1 gte (2 + 3))" $
        runTestForExpr "1 gte 2 + 3" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Gte
                        Expr{expAst=(IntLit 1)}
                        Expr{expAst=(Add Expr{expAst=(IntLit 2)} Expr{expAst=(IntLit 3)})}
                        )}])) -> True)
    it "accepts `1 eq 2` as an expression and associates to the left" $
        runTestForExpr "1 eq 2" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Eq
                        Expr{expAst=(IntLit 1)}
                        Expr{expAst=(IntLit 2)}
                        )}
                    ])) -> True)
    it "accepts `1 eq 2 + 3` as an expression and associates to the right (1 eq (2 + 3))" $
        runTestForExpr "1 eq 2 + 3" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Eq
                        Expr{expAst=(IntLit 1)}
                        Expr{expAst=(Add Expr{expAst=(IntLit 2)} Expr{expAst=(IntLit 3)})}
                    )}
                ])) -> True)
    it "accepts `1 eq 2 eq 3 as an expression and associates to the left (1 eq 2) eq 3" $
        runTestForExpr "1 eq 2 eq 3" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Eq
                        Expr{expAst=(Eq
                            Expr{expAst=(IntLit 1)}
                            Expr{expAst=(IntLit 2)})}
                        Expr{expAst=(IntLit 3)}
                    )}
                ])) -> True)
    it "accepts `1 neq 2` as an expression and associates to the left" $
        runTestForExpr "1 neq 2" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Neq
                        Expr{expAst=(IntLit 1)}
                        Expr{expAst=(IntLit 2)}
                    )}])) -> True)
    it "accepts `1 neq 2 + 3` as an expression and associates to the right (1 neq (2 + 3))" $
        runTestForExpr "1 neq 2 + 3" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Neq
                        Expr{expAst=(IntLit 1)}
                        Expr{expAst=(Add Expr{expAst=(IntLit 2)} Expr{expAst=(IntLit 3)})}
                        )}
                    ])) -> True)
    it "accepts `1 neq 2 neq 3 as an expression and associates to the left (1 neq 2) neq 3" $
        runTestForExpr "1 neq 2 neq 3" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Neq
                        Expr{expAst=(Neq
                            Expr{expAst=(IntLit 1)}
                            Expr{expAst=(IntLit 2)})}
                        Expr{expAst=(IntLit 3)}
                    )}])) -> True)
    it "accepts `1 eq 2 neq 3 as an expression and associates to the left (1 eq 2) neq 3" $
        runTestForExpr "1 eq 2 neq 3" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Neq
                        Expr{expAst=(Eq
                            Expr{expAst=(IntLit 1)}
                            Expr{expAst=(IntLit 2)}
                            )}
                        Expr{expAst=(IntLit 3)}
                    )}
                ])) -> True)

    it "accepts `lit and unlit` as an expression" $
        runTestForExpr "lit and unlit" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    And
                        Expr{expAst=TrueLit}
                        Expr{expAst=FalseLit}
                    )}
                ])) -> True)
    it "accepts `lit or unlit` as an expression" $
        runTestForExpr "lit or unlit" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Or
                        Expr{expAst=TrueLit}
                        Expr{expAst=FalseLit}
                    )}
                ])) -> True)
    it "accepts `not unlit` as an expression" $
        runTestForExpr "not unlit" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Not Expr{expAst=FalseLit})
                    }]
                )) -> True)
    it "accepts `lit and unlit and undiscovered` as an expression associating to the left `(lit and unlit) and undiscovered`" $
        runTestForExpr "lit and unlit and undiscovered" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    And
                        Expr{expAst=(And Expr{expAst=TrueLit} Expr{expAst=FalseLit})}
                        Expr{expAst=UndiscoveredLit})
                    }]
                )) -> True)
    it "accepts `not lit and unlit` as an expression and `not` has more precedence" $
        runTestForExpr "not lit and unlit" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    And
                        Expr{expAst=(Not Expr{expAst=TrueLit})}
                        Expr{expAst=FalseLit}
                    )}])) -> True)

    it "accepts `ascii_of |n|` as an expression" $
        runTestForExpr "ascii_of |n|" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    AsciiOf
                        Expr{expAst=(CharLit 'n')}
                    )}
                ])) -> True)

    it "accepts `ascii_of |n| + ascii_of |f|` as an expression and parses it as `(ascii_of |n|) + (ascii_of |f|)" $
        runTestForExpr "ascii_of |n| + ascii_of |f|" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Add
                        Expr{expAst=(AsciiOf Expr{expAst=(CharLit 'n')})}
                        Expr{expAst=(AsciiOf Expr{expAst=(CharLit 'f')})}
                    )}
                ])) -> True)

    it "accepts `<$$> >-< @@` as an expression" $
        runTestForExpr "<$$> >-< @@" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    ColConcat
                        Expr{expAst=(ArrayLit [])}
                        Expr{expAst=(StringLit "")}
                    )}]
                )) -> True)
    it "accepts `<$$> >-< @@ >-< {$$}` as an expression and should associate to the left `(<$$> >-< @@) >-< {$$}`" $
        runTestForExpr "<$$> >-< @@ >-< {$$}" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    ColConcat
                        Expr{expAst=(ColConcat
                            Expr{expAst=(ArrayLit [])}
                            Expr{expAst=(StringLit "")})}
                        Expr{expAst=(SetLit [])}
                        )}
                ])) -> True)
    it "accepts `lit union lit` as an expression" $
        runTestForExpr "lit union lit" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    SetUnion
                        Expr{expAst=TrueLit}
                        Expr{expAst=TrueLit})}])) -> True)
    it "accepts `lit intersect lit` as an expression" $
        runTestForExpr "lit intersect lit" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    SetIntersect
                        Expr{expAst=TrueLit}
                        Expr{expAst=TrueLit})}])) -> True)
    it "accepts `lit diff lit` as an expression" $
        runTestForExpr "lit diff lit" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    SetDiff
                        Expr{expAst=TrueLit}
                        Expr{expAst=TrueLit})}])) -> True)
    it "accepts `size lit` as an expression" $
        runTestForExpr "size lit" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    SetSize Expr{expAst=TrueLit})}])) -> True)

    it "accepts `lit union lit union lit` as an expression and associates to the left" $
        runTestForExpr "lit union lit union lit" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    SetUnion
                        Expr{expAst=(SetUnion
                            Expr{expAst=TrueLit}
                            Expr{expAst=TrueLit})}
                        Expr{expAst=TrueLit}
                        )}]
                    )) -> True)
    it "accepts `lit intersect lit intersect lit` as an expression and associates to the left" $
        runTestForExpr "lit intersect lit intersect lit" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    SetIntersect
                        Expr{expAst=(SetIntersect
                            Expr{expAst=TrueLit}
                            Expr{expAst=TrueLit})}
                        Expr{expAst=TrueLit}
                        )}]
                    )) -> True)
    it "accepts `lit diff lit diff lit` as an expression" $
        runTestForExpr "lit diff lit diff lit" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    SetDiff
                        Expr{expAst=(SetDiff
                            Expr{expAst=TrueLit}
                            Expr{expAst=TrueLit})}
                        Expr{expAst=TrueLit}
                    )}]
                )) -> True)
    it "accepts `lit diff lit union lit` as an expression" $
        runTestForExpr "lit diff lit union lit" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    SetDiff
                        Expr{expAst=TrueLit}
                        Expr{expAst=(SetUnion Expr{expAst=TrueLit} Expr{expAst=TrueLit})}
                        )}])) -> True)

    it "accepts `lit union lit intersect lit` as an expression and associates to the left" $
        runTestForExpr "lit union lit intersect lit" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    SetIntersect
                        Expr{expAst=(SetUnion Expr{expAst=TrueLit} Expr{expAst=TrueLit})}
                        Expr{expAst=TrueLit}
                        )}])) -> True)
    it "accepts `lit intersect lit union lit` as an expression and associates to the left" $
        runTestForExpr "lit intersect lit union lit" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    SetUnion
                        Expr{expAst=(SetIntersect Expr{expAst=TrueLit} Expr{expAst=TrueLit})}
                        Expr{expAst=TrueLit})}])) -> True)

    it "accepts `a ~> b` as an expression" $
        runTestForExpr "a ~> b" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Access
                        Expr{expAst=(IdExpr (Id Token {cleanedString="a"}))}
                        (Id Token {cleanedString="b"})
                    )}
                ])) -> True)
    it "accepts `a ~> b ~> c` as an expression" $
        runTestForExpr "a ~> b ~> c" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Access
                        Expr{expAst=(Access
                            Expr{expAst=(IdExpr (Id Token {cleanedString="a"}))}
                            (Id Token {cleanedString="b"}))}
                        (Id Token {cleanedString="c"}))}])) -> True)
    it "accepts `a + b ~> c` as an expression" $
        runTestForExpr "a + b ~> c" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    Add
                        Expr{expAst=(IdExpr (Id Token {cleanedString="a"}))}
                        Expr{expAst=(Access
                            Expr{expAst=(IdExpr (Id Token {cleanedString="b"}))}
                            (Id Token {cleanedString="c"}))})}])) -> True)

    it "accepts `(a)` as an expression" $
        runTestForExpr "(a)" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(IdExpr (Id Token {cleanedString="a"}))}])) -> True)

    it "accepts `a<$i$>` as an expression" $
        runTestForExpr "a<$i$>" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=(
                    IndexAccess
                        Expr{expAst=(IdExpr (Id Token {cleanedString="a"}))}
                        Expr{expAst=(IdExpr (Id Token {cleanedString="i"}))})}])) -> True)
    it "accepts `a+b<$i$>` as an expression" $
        runTestForExpr "a+b<$i$>" (\(Program (
            CodeBlock
                [InstReturnWith
                    Expr{expAst=Add
                        Expr{expAst=(IdExpr (Id Token {cleanedString="a"}))}
                        Expr{expAst=(IndexAccess
                            Expr{expAst=(IdExpr (Id Token {cleanedString="b"}))}
                            Expr{expAst=(IdExpr (Id Token {cleanedString="i"}))})
                        }
                    }])) -> True)
    it "rejects `a<$$>` as an expression" $
        runTestForInvalidProgram "a<$$>"

    it "accepts `abyss` as an expression" $
        runTestForExpr "abyss" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=NullLit}])) -> True)
    it "accepts `throw a a` as an expression" $
        runTestForExpr "throw a a" (\(Program (
            CodeBlock
                [InstReturnWith Expr {
                    expAst=MemAccess Expr {
                        expAst=IdExpr (Id Token {cleanedString="a"})
                    }}])) -> True)
    it "rejects `aim a a` as an expression" $
        runTestForInvalidProgram "aim a a"
    it "rejects `recover a a` as an expresion" $
        runTestForInvalidProgram "recover a a"

    it "accepts `summon f` a an expression" $
        runTestForExpr "summon f" (\(Program (
            CodeBlock
                [InstReturnWith Expr{expAst=EvalFunc (Id Token {cleanedString="f"}) []}])) -> True)
