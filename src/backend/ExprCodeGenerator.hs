module ExprCodeGenerator where

import CodeGenerator
import Grammar (BaseExpr(..))

instance GenerateCode BaseExpr where
    genCode _ = error "not implemented yet"
