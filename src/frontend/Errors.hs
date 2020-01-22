module Errors where

import Utils

data Error = Error String Position
    deriving Show

data ErrorCategory = LexError | SemanticError

data CompilerError = CompilerError
    { ceErrorCategory :: !ErrorCategory
    , ceErrors :: ![Error]
    }
