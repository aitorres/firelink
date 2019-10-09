module Parser.TypeAliasesSpec where

import Test.Hspec
import Parser.Utils
import Parser
import Data.Maybe
import Lexer

spec :: Spec
spec = describe "Type Aliases" $ do
    it "accepts a program with no type aliases" $ do
        let program = buildProgramWithEmptyMain ""
        putStrLn program
        tokens <- scanTokens program
        let ast = extractValidAST tokens
        ast `shouldSatisfy` (\(Program [] _ _) -> True)

    it "accepts a program with one type alias" $ do
        let program = buildProgramWithEmptyMain "\
            \ requiring help of \
            \   knight solaire humanity \
            \ help received"
        tokens <- scanTokens program
        let ast = extractValidAST tokens
        ast `shouldSatisfy` (\(Program [Alias (Id "solaire") BigInt] _ _) -> True)

    it "rejects when the alias list is empty" $ do
        let program = buildProgramWithEmptyMain "\
        \ requiring help of \
        \ help received"
        tokens <- scanTokens program
        let ast = extractInvalidAST tokens
        isParseError ast `shouldBe` True
