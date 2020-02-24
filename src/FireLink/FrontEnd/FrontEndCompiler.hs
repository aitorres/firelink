module FireLink.FrontEnd.FrontEndCompiler (frontEnd) where

import           Control.Monad.RWS          (runRWST, liftIO)
import           FireLink.FrontEnd.Errors
import           FireLink.FrontEnd.Grammar  (Program (..))
import           FireLink.FrontEnd.Lexer    (scanTokens)
import           FireLink.FrontEnd.Preparser   (preparse)
import           FireLink.FrontEnd.Parser   (parse)
import           FireLink.FrontEnd.SymTable (SymTable (..), DictionaryEntry (..), initialState, preparsedState)
import           FireLink.FrontEnd.Tokens   (Token (..))
import qualified Data.Map                           as Map
import           Text.Printf                        (printf)

type CompilerResult = Either (CompilerError, [Token]) (Program, SymTable, [Token])

lexer :: String -> IO CompilerResult
lexer contents =
    let (errors, tokens) = scanTokens contents in
    if not $ null errors then
        return $ Left (CompilerError LexError errors, tokens)
    else
        parserAndSemantic tokens

prettyPrintSymTable :: SymTable -> IO ()
prettyPrintSymTable SymTable{stDict=dict} = do
    let dictList = Map.toList dict
    mapM_ printKey dictList

printKey :: (String, [DictionaryEntry]) -> IO ()
printKey (name, keys) = do
    printf "Entries for name \"%s\": \n" name
    mapM_ printKey' keys
    putStrLn ""
    where
        printKey' :: DictionaryEntry -> IO ()
        printKey' st = do
            putStrLn ""
            putStr " - "
            print st

parserAndSemantic :: [Token] -> IO CompilerResult
parserAndSemantic tokens = do
    (_, pretable, _) <- runRWST (preparse tokens) () initialState
    let SymTable {stDict=predict} = pretable
    liftIO $ prettyPrintSymTable pretable
    (ast, table, errors) <- runRWST (parse tokens) () $ preparsedState pretable
    if not $ null errors then return $ Left (CompilerError SemanticError errors, tokens)
    else return $ Right (ast, table, tokens)


frontEnd :: String -> IO CompilerResult
frontEnd = lexer
