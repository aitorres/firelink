module Lib
    ( someFunc
    ) where
import Lexer (Token (..), AlexUserState(..), alexMonadScan, scanTokens)
import System.Environment (getArgs)
import System.IO (openFile, IOMode(..), hGetContents)

someFunc :: IO ()
someFunc = do
    args <- getArgs
    if null args then
        putStrLn "Mano qlq"
    else do
        let programFile = head args
        handle <- openFile programFile ReadMode
        contents <- hGetContents handle
        tokens <- scanTokens contents
        print tokens
