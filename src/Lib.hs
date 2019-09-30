module Lib
    ( someFunc
    ) where
import Lexer (Token (..), AlexUserState(..), alexMonadScan, readTokens)

someFunc :: IO ()
someFunc = case readTokens "b const" of
        LexFailure errors -> print errors
        LexSuccess tokens -> print tokens
