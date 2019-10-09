module Parser.DeclarationSpec where

import Test.Hspec
import Parser.Utils
import Parser

spec :: Spec
spec = describe "Declarations for new scopes" $ do
    it "accepts no declaration block" $
        runTestForValidProgram "\
        \ hello ashen one \

        \ traveling somewhere \
        \   with orange saponite say @Hello world@ \
        \ you died \

        \ farewell ashen one" (\(Program _ _ (CodeBlock [] _)) -> True)

    it "rejects empty declaration block" $
        runTestForInvalidProgram "\
        \ hello ashen one \

        \ traveling somewhere \
        \   with \
        \   in your inventory \
        \   with orange saponite say @Hello world@ \
        \ you died \

        \ farewell ashen one"

    it "accepts const declarations" $
        runTestForValidProgram "\
        \ hello ashen one \

        \ traveling somewhere \
        \   with \
        \      const patata of type humanity \
        \   in your inventory \
        \   with orange saponite say @Hello world@ \
        \ you died \

        \ farewell ashen one" (\(Program _ _ (
            CodeBlock
                [UninitializedDeclaration Const (Id "patata") BigInt]
                _)) -> True)
