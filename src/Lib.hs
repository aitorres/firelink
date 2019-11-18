module Lib
    ( mainFunc
    ) where
import qualified Control.Monad.RWS as RWS
import qualified Lexer as L
import Parser (parse)
import qualified SymTable as ST
import System.Environment (getArgs)
import System.IO (openFile, IOMode(..), hGetContents)
import qualified Data.Map as Map
import Data.List.Split (splitOn)
import Data.List (intercalate)
import Text.Printf (printf)

-- prettyPrintSymTable :: ST.SymTable -> IO ()
-- prettyPrintSymTable (dict, _, _) = do
--     let dictList = Map.toList dict
--     mapM_ 

formatLexError :: String -> L.LexError -> String
formatLexError fullStr (L.LexError (L.AlexPn offset r c, _, _, s)) =
    printf "\x1b[1m\x1b[31mYOU DIED!!\x1b[0m Lexical error at line \x1b[1m\x1b[31m%d\x1b[0m, column \x1b[1m\x1b[31m%d\x1b[0m:\n%s\n" r c fs
    where
        allLines = splitOn "\n" fullStr
        maxSize = foldl max (-1) $ map length allLines
        buildRuler = flip replicate '~'
        rule = buildRuler maxSize ++ "\n"
        relevantLines = drop (r-1) allLines
        firstLine = head relevantLines ++ "\n"
        restLines = take 4 $ tail relevantLines
        errorRuler = "\x1b[1m\x1b[31m" ++ buildRuler (c-1) ++ "^" ++ buildRuler (maxSize - c) ++ "\x1b[0m\n"
        fs = firstLine ++ errorRuler ++ intercalate "\n" restLines


printLexErrors :: String -> [L.LexError] -> IO ()
printLexErrors str [] = return ()
printLexErrors str (error:xs) = do
    putStrLn $ formatLexError str error
    printLexErrors str xs

lexer :: String -> IO ()
lexer contents = case L.scanTokens contents of
    Left errors -> do
        printLexErrors contents errors
        putStrLn "Fix your lexical mistakes, ashen one."
    Right tokens -> parserAndSemantic tokens

parserAndSemantic :: L.Tokens -> IO ()
parserAndSemantic tokens = do
    parsedProgram <- RWS.runRWST (parse tokens) () ST.initialState
    print parsedProgram

mainFunc :: IO ()
mainFunc = do
    args <- getArgs
    if null args then do
        putStrLn "\x1b[31m\x1b[1mFireLink\x1b[0m"
        putStrLn "Your journey must be started from a \x1b[1m\x1b[31m<file path>\x1b[0m, ashen one."
        putStrLn "Usage: \t stack run <path>"
    else do
        let programFile = head args
        handle <- openFile programFile ReadMode
        contents <- hGetContents handle
        lexer contents
