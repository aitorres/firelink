module Parser.Utils where

import AST (AST (..))
import Lexer (Tokens, scanTokens)
import Test.Hspec (shouldSatisfy, shouldBe)
import Data.Maybe (fromJust)
import Parser (parse, Program(..))

isParseError :: AST f -> Bool
isParseError (ValidAST _) = False
isParseError _ = True

extractValidAST :: Maybe Tokens -> Program
extractValidAST m = case parse $ fromJust m of
    ValidAST f -> f

extractInvalidAST :: Maybe Tokens -> AST a
extractInvalidAST m = case parse $ fromJust m of
    InvalidAST s -> InvalidAST s


buildProgramWithEmptyMain :: String -> String
buildProgramWithEmptyMain a = "hello ashen one " ++ a ++ " \
    \ traveling somewhere \
    \  with orange saponite say @Hello world@ \
    \ you died \
    \ farewell ashen one"

runTestForValidProgram programS predicate = do
    let program = buildProgramWithEmptyMain programS
    tokens <- scanTokens program
    let ast = extractValidAST tokens
    ast `shouldSatisfy` predicate

runTestForInvalidProgram programS = do
    let program = buildProgramWithEmptyMain programS
    tokens <- scanTokens program
    let ast = extractInvalidAST tokens
    isParseError ast `shouldBe` True
