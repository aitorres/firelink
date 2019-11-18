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
import Data.List (intercalate, groupBy)
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

groupTokensByRowNumber :: L.Tokens -> [L.Tokens]
groupTokensByRowNumber = groupBy (\(L.Token _ _ pn) (L.Token _ _ pn') ->
    L.row pn == L.row pn')

lengthOfLine :: L.Tokens -> Int
lengthOfLine tokens = let tk@(L.Token aToken maybeString pn) = last tokens in
        L.col pn + length (show tk)

joinTokens :: Int -> L.Tokens -> String
joinTokens _ [] = ""
joinTokens c (tk@(L.Token _ _ pn) : tks) =
    replicate (L.col pn - c) ' ' ++ show tk ++ joinTokens (L.col pn + length (show tk)) tks


formatLexError :: (L.LexError, L.Tokens) -> String
formatLexError (L.LexError (L.AlexPn offset r c, _, _, s), tokens) =
    printf "%s%sYOU DIED!!%s Lexical error at line %s%s%d%s, column %s%s%d%s:\n%s\n"
        red bold nocolor
        red bold r nocolor
        red bold c nocolor
        fs
    where
        tokensByRowNumber = groupTokensByRowNumber tokens
        maxSize = foldl max (-1) $ map lengthOfLine tokensByRowNumber
        buildRuler = flip replicate '~'
        rule = buildRuler maxSize ++ "\n"
        firstLine = joinTokens 0 (head tokensByRowNumber) ++ "\n"
        restLines = map (joinTokens 0) $ tail tokensByRowNumber
        errorRuler = bold ++ red ++ buildRuler (c-1) ++ "^" ++ buildRuler (maxSize - c) ++ nocolor ++ "\n"
        fs = firstLine ++ errorRuler ++ intercalate "\n" restLines


printLexErrors :: [(L.LexError, L.Tokens)] -> IO ()
printLexErrors [] = return ()
printLexErrors (errorPair:xs) = do
    putStrLn $ formatLexError errorPair
    printLexErrors xs

groupLexErrorWithTokenContext :: [L.LexError] -> L.Tokens -> [(L.LexError, L.Tokens)]
groupLexErrorWithTokenContext [] _ = []
groupLexErrorWithTokenContext (error@(L.LexError (pn, _, _, _)):errors) tokens =
    (error, tks) : groupLexErrorWithTokenContext errors tokens
    where
        tail = dropWhile (\(L.Token _ _ pn') -> L.row pn /= L.row pn') tokens
        tks = takeWhile (\(L.Token _ _ pn') -> L.row pn' - L.row pn <= 4) tail

lexer :: String -> IO ()
lexer contents = do
    let (errors, tokens) = L.scanTokens contents
    if not $ null errors then do
        printLexErrors $ groupLexErrorWithTokenContext errors tokens
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
