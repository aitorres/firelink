module InstructionCodeGenerator where

import CodeGenerator
import ExprCodeGenerator (genCode')
import Grammar (Instruction(..), Expr(..), BaseExpr(..), Id(..))
import TACType
import Control.Monad.RWS (tell)

instance GenerateCode Instruction where
    genCode (InstPrint expr) = genCode expr

    genCode (InstAsig lvalue@Expr {expAst = IdExpr id} rvalue) = do
        operand <- genCode' lvalue
        rValueAddress <- genCode' rvalue
        tell [ThreeAddressCode
                { tacOperand = Assign
                , tacLvalue = Just operand
                , tacRvalue1 = Just rValueAddress
                , tacRvalue2 = Nothing
                }]

