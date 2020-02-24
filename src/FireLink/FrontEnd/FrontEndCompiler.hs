module FireLink.FrontEnd.FrontEndCompiler (frontEnd) where

import           Control.Monad.RWS           (runRWST)
import qualified Data.Map                    as Map
import           FireLink.FrontEnd.Errors
import           FireLink.FrontEnd.Grammar   (Program (..))
import           FireLink.FrontEnd.Lexer     (scanTokens)
import           FireLink.FrontEnd.Parser    (parse)
import           FireLink.FrontEnd.Preparser (preparse)
import           FireLink.FrontEnd.SymTable  (DictionaryEntry (..),
                                              SymTable (..), initialState,
                                              preparsedState)
import           FireLink.FrontEnd.Tokens    (Token (..))
import           Text.Printf                 (printf)

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
    (_, preparseTable, preparseErrors) <- runRWST (preparse tokens) () initialState
    (ast, table, parseErrors) <- runRWST (parse tokens) () $ preparsedState preparseTable
    let errors = removeDuplicateErrors $ preparseErrors ++ parseErrors
    if not $ null errors then return $ Left (CompilerError SemanticError errors, tokens)
    else return $ Right (ast, table, tokens)

frontEnd :: String -> IO CompilerResult
frontEnd = lexer
