module Lib
    ( someFunc
    ) where
import Lexer (Token (..), alexMonadScan, runAlex)

someFunc :: IO ()
someFunc =
    case runAlex "" alexMonadScan of
        Left s -> putStrLn s
        _ -> putStrLn "holis"
    -- putStrLn "Hello world!"
