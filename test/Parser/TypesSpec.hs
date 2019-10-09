module TypesSpec where

import Test.Hspec
import Utils
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

    it "allows `bonfire` as data type declaration" $
        runTestForValidProgram (buildProgramWithType "bonfire")
            (\(Program _ _ (
                CodeBlock
                    [UninitializedDeclaration Const (Id "patata") Bool]
                    _)) -> True)
    it "allows `hollow` as data type declaration" $
        runTestForValidProgram (buildProgramWithType "hollow")
            (\(Program _ _ (
                CodeBlock
                    [UninitializedDeclaration Const (Id "patata") Float]
                    _)) -> True)
    it "allows `sign` as data type declaration" $
        runTestForValidProgram (buildProgramWithType "sign")
            (\(Program _ _ (
                CodeBlock
                    [UninitializedDeclaration Const (Id "patata") CharT]
                    _)) -> True)
    it "allows `<n>-miracle` as data type declaration" $
        runTestForValidProgram (buildProgramWithType "<2>-miracle")
            (\(Program _ _ (
                CodeBlock
                    [UninitializedDeclaration Const (Id "patata") (StringType (IntLit 2))]
                    _)) -> True)

    it "allows `<n>-chest of type humanity` as data type declaration" $
        runTestForValidProgram (buildProgramWithType "<2>-chest of type humanity")
            (\(Program _ _ (
                CodeBlock
                    [UninitializedDeclaration Const (Id "patata") (Array BigInt (IntLit 2))]
                    _)) -> True)
    it "rejects `<n>-chest` as data type declaration" $
        runTestForInvalidProgram (buildProgramWithType "<2>-chest")
    it "rejects `<n>-chest of type <3>-chest` as data type declaration" $
        runTestForInvalidProgram (buildProgramWithType "<2>-chest of type <3>-chest")

    it "rejects `armor` as data type declaration" $
        runTestForInvalidProgram (buildProgramWithType "armor")
    it "rejects `armor of type armor` as data type declaration" $
        runTestForInvalidProgram (buildProgramWithType "armor of type armor")
    it "allows `armor of type humanity` as data type declaration" $
        runTestForValidProgram (buildProgramWithType "armor of type humanity")
            (\(Program _ _ (
                CodeBlock
                    [UninitializedDeclaration Const (Id "patata") (Set BigInt)]
                    _)) -> True)

    it "rejects empty `titanite` as data type declaration" $
        runTestForInvalidProgram (buildProgramWithType "titanite { }")
    it "allows `titanite { opt1 }` as data type declaration" $
        runTestForValidProgram (buildProgramWithType "titanite { opt1 }")
            (\(Program _ _ (
                CodeBlock
                    [UninitializedDeclaration Const (Id "patata") (Enum [EnumItem (Id "opt1")])]
                    _)) -> True)
    it "allows `titanite { opt1, opt2 }` as data type declaration" $
        runTestForValidProgram (buildProgramWithType "titanite { opt1, opt2 }")
            (\(Program _ _ (
                CodeBlock
                    [UninitializedDeclaration Const (Id "patata") (
                        Enum [
                            EnumItem (Id "opt1"),
                            EnumItem (Id "opt2")
                            ])]
                    _)) -> True)

    it "rejects empty `bezel` as data type declaration" $
        runTestForInvalidProgram (buildProgramWithType "bezel { }")
    it "allows `bezel { opt1 of type humanity }` as data type declaration" $
        runTestForValidProgram (buildProgramWithType "bezel { opt1 of type humanity }")
            (\(Program _ _ (
                CodeBlock
                    [UninitializedDeclaration Const (Id "patata") (Record [StructItem (Id "opt1") BigInt])]
                    _)) -> True)
    it "allows `bezel { opt1 of type humanity, opt2 of type sign }` as data type declaration" $
        runTestForValidProgram (buildProgramWithType "bezel { opt1 of type humanity, opt2 of type sign }")
            (\(Program _ _ (
                CodeBlock
                    [UninitializedDeclaration Const (Id "patata") (
                        Record [
                            StructItem (Id "opt1") BigInt,
                            StructItem (Id "opt2") CharT
                            ])]
                    _)) -> True)

    it "rejects empty `link` as data type declaration" $
        runTestForInvalidProgram (buildProgramWithType "link { }")
    it "allows `link { opt1 of type humanity }` as data type declaration" $
        runTestForValidProgram (buildProgramWithType "link { opt1 of type humanity }")
            (\(Program _ _ (
                CodeBlock
                    [UninitializedDeclaration Const (Id "patata") (UnionStruct [StructItem (Id "opt1") BigInt])]
                    _)) -> True)
    it "allows `link { opt1 of type humanity, opt2 of type sign }` as data type declaration" $
        runTestForValidProgram (buildProgramWithType "link { opt1 of type humanity, opt2 of type sign }")
            (\(Program _ _ (
                CodeBlock
                    [UninitializedDeclaration Const (Id "patata") (
                        UnionStruct [
                            StructItem (Id "opt1") BigInt,
                            StructItem (Id "opt2") CharT
                            ])]
                    _)) -> True)
    it "rejects empty `arrow to` as data type declaration" $
        runTestForInvalidProgram (buildProgramWithType "arrow to")
    it "allows `arrow to humanity` as data type declaration" $
        runTestForValidProgram (buildProgramWithType "arrow to humanity")
            (\(Program _ _ (
                CodeBlock
                    [UninitializedDeclaration Const (Id "patata") (Pointer BigInt)]
                    _)) -> True)
    it "allows `myAlias` as data type declaration" $
        runTestForValidProgram (buildProgramWithType "myAlias")
            (\(Program _ _ (
                CodeBlock
                    [UninitializedDeclaration Const (Id "patata") (AliasType (Id "myAlias"))]
                    _)) -> True)
