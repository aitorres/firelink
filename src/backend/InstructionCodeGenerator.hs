module InstructionCodeGenerator where

import CodeGenerator
import ExprCodeGenerator
import Grammar (Instruction(..))

instance GenerateCode Instruction where
    genCode (InstPrint expr) = genCode expr >> return Nothing

