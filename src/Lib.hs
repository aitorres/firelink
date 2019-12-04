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
import Data.List (intercalate, groupBy)
import Text.Printf (printf)
import Utils

prettyPrintSymTable :: ST.SymTable -> IO ()
prettyPrintSymTable (dict, _, _) = do
    let dictList = Map.toList dict
    mapM_ printKey dictList

printKey :: (String, [ST.DictionaryEntry]) -> IO ()
printKey (name, keys) = do
    printf "Entries for name \"%s\": \n" name
    mapM_ printKey keys
    putStrLn ""
    where
        printKey :: ST.DictionaryEntry -> IO ()
        printKey st = do
            putStrLn ""
            putStr " - "
            print st

groupTokensByRowNumber :: [Either L.LexError L.Token] -> [[Either L.LexError L.Token]]
groupTokensByRowNumber = groupBy (\e e' -> getRow e == getRow e')
    where
        getRow e = case e of
                    Left (L.LexError (pn, _)) -> L.row pn
                    Right L.Token {L.posn=pn} -> L.row pn

lengthOfLine :: [Either L.LexError L.Token] -> Int
lengthOfLine tokens = case last tokens of
    Right tk@L.Token {L.posn=pn} -> L.col pn + length (show tk)
    Left (L.LexError (pn, s)) -> L.col pn + length s

joinTokens :: Int -> [Either L.LexError L.Token] -> String
joinTokens _ [] = ""
joinTokens c (e:tks) = case e of
    Right tk@L.Token {L.capturedString=s, L.posn=pn} ->
        replicate (L.col pn - c) ' ' ++ show tk ++ joinTokens (L.col pn + length s) tks
    Left (L.LexError (pn, s)) ->
        replicate (L.col pn - c) ' ' ++ s ++ joinTokens (L.col pn + length s) tks

buildRuler :: Int -> String
buildRuler = flip replicate '~'

formatLexError :: (L.LexError, [Either L.LexError L.Token]) -> String
formatLexError (L.LexError (L.AlexPn _ r c, _), tokens) =
    printf "%s%sYOU DIED!!%s Lexical error at line %s%s%d%s, column %s%s%d%s:\n%s\n"
        red bold nocolor
        red bold r nocolor
        red bold c nocolor
        fs
    where
        tokensByRowNumber = groupTokensByRowNumber tokens
        maxSize = foldl max (-1) $ map lengthOfLine tokensByRowNumber
        firstLine = joinTokens 1 (head tokensByRowNumber) ++ "\n"
        restLines = map (joinTokens 1) $ tail tokensByRowNumber
        errorRuler = bold ++ red ++ buildRuler (c-1) ++ "^" ++ buildRuler (maxSize - c) ++ nocolor ++ "\n"
        fs = firstLine ++ errorRuler ++ intercalate "\n" restLines


printLexErrors :: [(L.LexError, [Either L.LexError L.Token])] -> IO ()
printLexErrors [] = return ()
printLexErrors (errorPair:xs) = do
    putStrLn $ formatLexError errorPair
    printLexErrors xs

groupLexErrorWithTokenContext :: [L.LexError] -> L.Tokens -> [(L.LexError, L.Tokens)]
groupLexErrorWithTokenContext [] _ = []
groupLexErrorWithTokenContext (err@(L.LexError (pn, _)):errors) tokens =
    (err, tks) : groupLexErrorWithTokenContext errors tokens
    where
        tail' = dropWhile (\L.Token {L.posn=pn'} -> L.row pn /= L.row pn') tokens
        tks = takeWhile (\L.Token {L.posn=pn'} -> L.row pn' - L.row pn <= 4) tail'

insertLexErrorOnContext :: L.LexError -> L.Tokens -> [Either L.LexError L.Token]
insertLexErrorOnContext l [] = [Left l]
insertLexErrorOnContext e@(L.LexError (pn, _)) (tk@L.Token {L.posn=pn'} : tks) =
    if (L.row pn' >= L.row pn) && (L.col pn' >= L.col pn)
    then Left e : Right tk : map Right tks
    else Right tk : insertLexErrorOnContext e tks

joinTokensOnly :: L.Tokens -> String
joinTokensOnly = joinTokens (-1) . map Right

numberOfRows :: [L.Token] -> Int
numberOfRows = foldl (\cu L.Token {L.posn=pn} -> cu `max` L.row pn) (-1)

maxDigits :: [L.Token] -> Int
maxDigits = length . show . numberOfRows

printProgram :: L.Tokens -> IO ()
printProgram tks =
    mapM_ printRowAndLine groupedByRowNumber
    where
        groupedByRowNumber :: [L.Tokens]
        groupedByRowNumber = groupBy (\L.Token {L.posn=t1} L.Token {L.posn=t2} -> L.row t1 == L.row t2) tks


        printRowAndLine :: L.Tokens -> IO ()
        printRowAndLine tks' = do
            let L.Token {L.posn=pn} = head tks'
            let row = L.row pn
            let lengthOfRowNumber = length $ show row
            putStr $ show row
            putStr $ replicate (maxDigits tks - lengthOfRowNumber) ' '
            putStr "|"
            putStrLn $ joinTokensOnly tks'

lexer :: String -> IO ()
lexer contents = do
    let (errors, tokens) = L.scanTokens contents
    if not $ null errors then do
        printLexErrors
            $ map (\(err, tks) -> (err, insertLexErrorOnContext err tks))
            $ groupLexErrorWithTokenContext errors tokens
        putStrLn "Fix your lexical mistakes, ashen one."
    else
        parserAndSemantic tokens

printSemErrors :: [ST.SemanticError] -> L.Tokens -> IO ()
printSemErrors [] _ = do
    putStrLn "Fix your semantic mistakes, ashen one."
    return ()
printSemErrors (semError:semErrors) tokens = do
    let (ST.SemanticError errMessage L.Token{L.posn=p}) = semError
    let tks = filter (\L.Token{L.posn=p'} -> L.row p' == L.row p) tokens
    let preContext = filter (\L.Token{L.posn=p'} -> L.row p' == L.row p - 1) tokens
    let postContext = filter (\L.Token{L.posn=p'} -> L.row p' == L.row p + 1) tokens
    let nDigits = 2 + maxDigits tks
    RWS.when (not $ null preContext) $ printProgram preContext
    printProgram tks
    putStr $ buildRuler (nDigits + L.col p)
    putStrLn "^"
    putStrLn $ bold ++ red ++ "YOU DIED!!" ++ nocolor ++ " " ++ errMessage
    RWS.when (not $ null postContext) $ printProgram postContext
    putStrLn ""
    printSemErrors semErrors tokens

parserAndSemantic :: L.Tokens -> IO ()
parserAndSemantic tokens = do
    (_, table, errors) <- RWS.runRWST (parse tokens) () ST.initialState
    -- print errors
    -- prettyPrintSymTable table
    if not $ null errors then printSemErrors errors tokens
    else do
        prettyPrintSymTable table
        printProgram tokens

mainFunc :: IO ()
mainFunc = do
    args <- getArgs
    if null args then do
        putStrLn $ red ++ bold ++ "FireLink" ++ nocolor
        putStrLn $ "Your journey must be started from a " ++ bold ++ red ++ "<file path>" ++ nocolor ++ ", ashen one."
        putStrLn "Usage: \t stack run <path>"
    else do
        let programFile = head args
        handle <- openFile programFile ReadMode
        contents <- hGetContents handle
        lexer contents
