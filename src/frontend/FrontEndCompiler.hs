module FrontEndCompiler (frontEnd) where

import Errors
import Grammar (Program(..))
import SymTable (SymTable(..), initialState)
import Tokens (Token(..))
import Control.Monad.RWS (runRWST)
import Parser (parse)
import Lexer (scanTokens)

type CompilerResult = Either (CompilerError, [Token]) (Program, SymTable, [Token])

lexer :: String -> IO CompilerResult
lexer contents =
    let (errors, tokens) = scanTokens contents in
    if not $ null errors then
        return $ Left (CompilerError LexError errors, tokens)
    else
        parserAndSemantic tokens

parserAndSemantic :: [Token] -> IO CompilerResult
parserAndSemantic tokens = do
    (ast, table, errors) <- runRWST (parse tokens) () initialState
    if not $ null errors then return $ Left (CompilerError SemanticError errors, tokens)
    else return $ Right (ast, table, tokens)


frontEnd :: String -> IO CompilerResult
frontEnd = lexer