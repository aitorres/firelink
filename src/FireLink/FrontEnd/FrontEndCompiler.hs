module FireLink.FrontEnd.FrontEndCompiler (frontEnd) where

import           Control.Monad.RWS          (runRWST)
import           FireLink.FrontEnd.Errors
import           FireLink.FrontEnd.Grammar  (Program (..))
import           FireLink.FrontEnd.Lexer    (scanTokens)
import           FireLink.FrontEnd.Parser   (parse)
import           FireLink.FrontEnd.SymTable (SymTable (..), initialState)
import           FireLink.FrontEnd.Tokens   (Token (..))

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
