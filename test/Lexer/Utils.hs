module Utils where
import           FireLink.FrontEnd.Tokens (AbstractToken (..), Token (..))

getAbstractToken :: Token -> AbstractToken
getAbstractToken = aToken

