module Parser.Utils where

import AST (AST (..))
import Lexer (Tokens, scanTokens)
import Test.Hspec (shouldSatisfy, shouldBe)
import Data.Maybe (fromJust)
import Parser
import Grammar

isParseError :: AST f -> Bool
isParseError (ValidAST _) = False
isParseError _ = True

extractValidAST :: Maybe Tokens -> Program
extractValidAST m = case parse $ fromJust m of
    ValidAST f -> f

extractInvalidAST :: Maybe Tokens -> AST a
extractInvalidAST m = case parse $ fromJust m of
    InvalidAST s -> InvalidAST s


runTestForValidProgram program predicate = do
    tokens <- scanTokens program
    let ast = extractValidAST tokens
    ast `shouldSatisfy` predicate

runTestForInvalidProgram program = do
    tokens <- scanTokens program
    let ast = extractInvalidAST tokens
    isParseError ast `shouldBe` True
