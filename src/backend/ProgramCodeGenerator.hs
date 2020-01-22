module ProgramCodeGenerator where

import CodeGenerator
import Grammar (Program(..))

instance GenerateCode Program where
    genCode _ = error "not implemented yet"
