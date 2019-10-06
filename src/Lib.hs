module Lib
    ( mainFunc
    ) where
import Lexer (Token (..), AlexUserState(..), alexMonadScan, scanTokens)
import Parser (parse)
import System.Environment (getArgs)
import System.IO (openFile, IOMode(..), hGetContents)

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
        tokens <- scanTokens contents
        case tokens of
            Just validTokens -> do
                let parsedProgram = parse validTokens
                putStrLn $ show parsedProgram
            Nothing ->
                putStrLn "Fix your lexical mistakes, ashen one."
