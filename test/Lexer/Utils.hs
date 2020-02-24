module Utils where
import           FireLink.FrontEnd.Lexer  (scanTokens)
import           FireLink.FrontEnd.Tokens (AbstractToken (..), Token (..))

getAbstractToken :: Token -> AbstractToken
getAbstractToken = aToken

scanToken :: String -> AbstractToken
scanToken x =
    let ([], toks) = scanTokens x in
        getAbstractToken $ head toks

