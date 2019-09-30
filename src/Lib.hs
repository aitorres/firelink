module Lib
    ( someFunc
    ) where
import Lexer (Token (..), alexScanTokens)

someFunc :: IO ()
someFunc = putStrLn "Hello world!"
