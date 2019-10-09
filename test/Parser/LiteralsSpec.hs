module Parser.LiteralsSpec where

import Test.Hspec
import Parser.Utils
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
spec = describe "Literal Values" $
    it "accepts 123 as an expression" $
        runTestForValidProgram (buildProgramWithLiteral "123")
        (\(Program _ _ (
            CodeBlock
                [InitializedDeclaration Const (Id "patata") BigInt (IntLit 123)]
                _)) -> True)
