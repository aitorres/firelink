module FireLink.FrontEnd.Errors where

import           FireLink.FrontEnd.Utils

data Error = Error String Position
    deriving Show

data ErrorCategory = LexError
    | SemanticError

data CompilerError = CompilerError
    { ceErrorCategory :: !ErrorCategory
    , ceErrors        :: ![Error]
    }
