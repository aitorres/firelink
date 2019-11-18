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

red :: String
red = "\x1b[31m"

bold :: String
bold = "\x1b[1m"

nocolor :: String
nocolor = "\x1b[0m"

prettyPrintSymTable :: ST.SymTable -> IO ()
prettyPrintSymTable (dict, _, _) = do
    let dictList = Map.toList dict
    mapM_ printKey dictList

printKey :: (String, [ST.DictionaryEntry]) -> IO ()
printKey (name, entries) =
    printf "Entries for name \"%s\"" name

formatLexError :: String -> L.LexError -> String
formatLexError fullStr (L.LexError (L.AlexPn offset r c, _, _, s)) =
    printf "%s%sYOU DIED!!%s Lexical error at line %s%s%d%s, column %s%s%d%s:\n%s\n"
        red bold nocolor
        red bold r nocolor
        red bold c nocolor
        fs
    where
        allLines = splitOn "\n" fullStr
        maxSize = foldl max (-1) $ map length allLines
        buildRuler = flip replicate '~'
        rule = buildRuler maxSize ++ "\n"
        relevantLines = drop (r-1) allLines
        firstLine = head relevantLines ++ "\n"
        restLines = take 4 $ tail relevantLines
        errorRuler = bold ++ red ++ buildRuler (c-1) ++ "^" ++ buildRuler (maxSize - c) ++ nocolor ++ "\n"
        fs = firstLine ++ errorRuler ++ intercalate "\n" restLines


printLexErrors :: String -> [L.LexError] -> IO ()
printLexErrors str [] = return ()
printLexErrors str (error:xs) = do
    putStrLn $ formatLexError str error
    printLexErrors str xs

lexer :: String -> IO ()
lexer contents = do
    let (errors, tokens) = L.scanTokens contents
    if not $ null errors then do
        printLexErrors contents errors
        putStrLn "Fix your lexical mistakes, ashen one."
    else parserAndSemantic tokens

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
