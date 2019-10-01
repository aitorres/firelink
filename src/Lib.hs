module Lib
    ( someFunc
    ) where
import Lexer (Token (..), AlexUserState(..), alexMonadScan, scanTokens)
import System.Environment (getArgs)

someFunc :: IO ()
someFunc = do
    args <- getArgs
    if null args then
        putStrLn "Mano qlq"
    else do
        let program = head args
        tokens <- scanTokens program
        print tokens
