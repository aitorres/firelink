module FireLink.FrontEnd.Errors where

import           FireLink.Utils

data Error = Error String Position
    deriving (Show, Eq)

data ErrorCategory = LexError
    | SemanticError

data CompilerError = CompilerError
    { ceErrorCategory :: !ErrorCategory
    , ceErrors        :: ![Error]
    }

removeDuplicateErrors :: [Error] -> [Error]
removeDuplicateErrors = foldl seen []
    where
        seen :: [Error] -> Error -> [Error]
        seen s x = if x `elem` s then s else x:s
