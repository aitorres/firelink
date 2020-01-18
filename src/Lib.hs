module Lib
    ( compile
    ) where
import qualified Control.Monad.RWS as RWS
import qualified Tokens as T
import Lexer
import Parser (parse)
import qualified SymTable as ST
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.FilePath (takeExtension)
import System.IO (openFile, IOMode(..), hGetContents)
import qualified Data.Map as Map
import Data.List (intercalate, groupBy)
import Text.Printf (printf)
import Utils

prettyPrintSymTable :: ST.SymTable -> IO ()
prettyPrintSymTable ST.SymTable{ST.stDict=dict} = do
    let dictList = Map.toList dict
    mapM_ printKey dictList

printKey :: (String, [ST.DictionaryEntry]) -> IO ()
printKey (name, keys) = do
    printf "Entries for name \"%s\": \n" name
    mapM_ printKey' keys
    putStrLn ""
    where
        printKey' :: ST.DictionaryEntry -> IO ()
        printKey' st = do
            putStrLn ""
            putStr " - "
            print st

groupTokensByRowNumber :: [Either T.LexError T.Token] -> [[Either T.LexError T.Token]]
groupTokensByRowNumber = groupBy (\e e' -> getRow e == getRow e')
    where
        getRow e = case e of
                    Left (T.LexError (pn, _)) -> fst pn
                    Right T.Token {T.posn=pn} -> fst pn

lengthOfLine :: [Either T.LexError T.Token] -> Int
lengthOfLine tokens = case last tokens of
    Right tk@T.Token {T.posn=pn} -> snd pn + length (show tk)
    Left (T.LexError (pn, s)) -> snd pn + length s

joinTokens :: Int -> [Either T.LexError T.Token] -> String
joinTokens _ [] = ""
joinTokens c (e:tks) = case e of
    Right tk@T.Token {T.capturedString=s, T.posn=pn} ->
        replicate (snd pn - c) ' ' ++ show tk ++ joinTokens (snd pn + length s) tks
    Left (T.LexError (pn, s)) ->
        replicate (snd pn - c) ' ' ++ s ++ joinTokens (snd pn + length s) tks

buildRuler :: Int -> String
buildRuler = flip replicate '~'

formatLexError :: (T.LexError, [Either T.LexError T.Token]) -> String
formatLexError (T.LexError ((r, c), _), tokens) =
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


printLexErrors :: [(T.LexError, [Either T.LexError T.Token])] -> IO ()
printLexErrors [] = return ()
printLexErrors (errorPair:xs) = do
    putStrLn $ formatLexError errorPair
    printLexErrors xs

groupLexErrorWithTokenContext :: [T.LexError] -> [T.Token] -> [(T.LexError, [T.Token])]
groupLexErrorWithTokenContext [] _ = []
groupLexErrorWithTokenContext (err@(T.LexError (pn, _)):errors) tokens =
    (err, tks) : groupLexErrorWithTokenContext errors tokens
    where
        tail' = dropWhile (\T.Token {T.posn=pn'} -> fst pn /= fst pn') tokens
        tks = takeWhile (\T.Token {T.posn=pn'} -> fst pn' - fst pn <= 4) tail'

insertLexErrorOnContext :: T.LexError -> [T.Token] -> [Either T.LexError T.Token]
insertLexErrorOnContext l [] = [Left l]
insertLexErrorOnContext e@(T.LexError (pn, _)) (tk@T.Token {T.posn=pn'} : tks) =
    if (fst pn' >= fst pn) && (snd pn' >= snd pn)
    then Left e : Right tk : map Right tks
    else Right tk : insertLexErrorOnContext e tks

joinTokensOnly :: [T.Token] -> String
joinTokensOnly = joinTokens (-1) . map Right

numberOfRows :: [T.Token] -> Int
numberOfRows = foldl (\cu T.Token {T.posn=pn} -> cu `max` fst pn) (-1)

maxDigits :: [T.Token] -> Int
maxDigits = length . show . numberOfRows

printProgram :: [T.Token] -> IO ()
printProgram tks =
    mapM_ printRowAndLine groupedByRowNumber
    where
        groupedByRowNumber :: [[T.Token]]
        groupedByRowNumber = groupBy (\T.Token {T.posn=t1} T.Token {T.posn=t2} -> fst t1 == fst t2) tks


        printRowAndLine :: [T.Token] -> IO ()
        printRowAndLine tks' = do
            let T.Token {T.posn=pn} = head tks'
            let row = fst pn
            let lengthOfRowNumber = length $ show row
            putStr $ show row
            putStr $ replicate (maxDigits tks - lengthOfRowNumber) ' '
            putStr "|"
            putStrLn $ joinTokensOnly tks'

lexer :: String -> IO ()
lexer contents = do
    let (errors, tokens) = scanTokens contents
    if not $ null errors then do
        printLexErrors
            $ map (\(err, tks) -> (err, insertLexErrorOnContext err tks))
            $ groupLexErrorWithTokenContext errors tokens
        putStrLn "Fix your lexical mistakes, ashen one."
        exitFailure
    else
        parserAndSemantic tokens

printSemErrors :: [ST.SemanticError] -> [T.Token] -> IO ()
printSemErrors [] _ = do
    putStrLn "Fix your semantic mistakes, ashen one."
    return ()
printSemErrors (semError:semErrors) tokens = do
    let (ST.SemanticError errMessage T.Token{T.posn=p}) = semError
    let tks = filter (\T.Token{T.posn=p'} -> fst p' == fst p) tokens
    let preContext = filter (\T.Token{T.posn=p'} -> fst p' == fst p - 1) tokens
    let postContext = filter (\T.Token{T.posn=p'} -> fst p' == fst p + 1) tokens
    let nDigits = 2 + maxDigits tks
    RWS.when (not $ null preContext) $ printProgram preContext
    printProgram tks
    putStr $ buildRuler (nDigits + snd p)
    putStrLn "^"
    putStrLn $ bold ++ red ++ "YOU DIED!!" ++ nocolor ++ " " ++ errMessage
    RWS.when (not $ null postContext) $ printProgram postContext
    putStrLn "\n"
    printSemErrors semErrors tokens

parserAndSemantic :: [T.Token] -> IO ()
parserAndSemantic tokens = do
    (_, table, errors) <- RWS.runRWST (parse tokens) () ST.initialState
    if not $ null errors then do
        printSemErrors errors tokens
        exitFailure
    else do
        prettyPrintSymTable table
        printProgram tokens
        exitSuccess

raiseCompilerError :: String -> IO ()
raiseCompilerError msg = do
    putStrLn $ bold ++ red ++ "YOU DIED!!" ++ nocolor ++ " Compiler error"
    putStrLn msg
    exitFailure


failByNonExistantFile :: String -> IO ()
failByNonExistantFile f =
    raiseCompilerError $ "Your journey cannot be started from " ++ f ++ ", ashen one, as " ++ bold ++ "the file could not be found" ++ nocolor ++ "."

failByFileExtension :: IO ()
failByFileExtension =
    raiseCompilerError $ "The filename MUST have the " ++ bold ++ ".souls" ++ nocolor ++ " extension, ashen one."

compile :: IO ()
compile = do
    args <- getArgs
    if null args then do
        putStrLn $ red ++ bold ++ "FireLink" ++ nocolor
        putStrLn $ "Your journey must be started from a " ++ bold ++ red ++ "<file path>" ++ nocolor ++ ", ashen one."
        putStrLn "Usage: \t stack run <path>"
        exitFailure
    else do
        let programFile = head args
        fileExists <- doesFileExist programFile
        if fileExists then
            if takeExtension programFile /= ".souls" then failByFileExtension
            else do
                handle <- openFile programFile ReadMode
                contents <- hGetContents handle
                lexer contents
        else failByNonExistantFile programFile
