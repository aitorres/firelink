module Utils where
import Lexer (Token(..), AbstractToken(..))

getAbstractToken :: Token -> AbstractToken
getAbstractToken = aToken

