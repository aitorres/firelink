module Parser.TypesSpec where

import Test.Hspec
import Parser.Utils
import Grammar

buildProgramWithType t = "\
\ hello ashen one \

\ traveling somewhere \
\   with \
\      const patata of type " ++ t ++ " \
\   in your inventory \
\   with orange saponite say @Hello world@ \
\ you died \

\ farewell ashen one"

spec :: Spec
spec = describe "Data types" $ do
    it "allows `humanity` as data type declaration" $
        runTestForValidProgram (buildProgramWithType "humanity")
            (\(Program _ _ (
                CodeBlock
                    [UninitializedDeclaration Const (Id "patata") BigInt]
                    _)) -> True)
    it "allows `big humanity` as data type declaration" $
        runTestForValidProgram (buildProgramWithType "big humanity")
            (\(Program _ _ (
                CodeBlock
                    [UninitializedDeclaration Const (Id "patata") BigInt]
                    _)) -> True)
    it "allows `small humanity` as data type declaration" $
        runTestForValidProgram (buildProgramWithType "small humanity")
            (\(Program _ _ (
                CodeBlock
                    [UninitializedDeclaration Const (Id "patata") SmallInt]
                    _)) -> True)
