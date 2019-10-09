module Parser.Utils where

import AST (AST (..))

isParseError :: AST f -> Bool
isParseError (ValidAST _) = False
isParseError _ = True

extractValidAST :: AST f -> f
extractValidAST (ValidAST f) = f
