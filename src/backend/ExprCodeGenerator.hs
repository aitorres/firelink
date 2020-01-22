module ExprCodeGenerator where

import CodeGenerator
import Grammar (BaseExpr(..), Expr(..), Op2(..), Id(..))
import Tokens (Token(..))
import Control.Monad.RWS
import qualified TACType as TAC

instance GenerateCode Expr where
    genCode = genCode . expAst

instance GenerateCode BaseExpr where
    genCode (Op2 op lexpr rexpr) = do
        lId <- genCode lexpr
        rId <- genCode rexpr
        newId <- newtemp
        let lvalue = Just $ TAC.Constant $ ConstantString newId
        tell [TAC.ThreeAddressCode
                { TAC.tacOperand = operation
                , TAC.tacLvalue = lvalue
                , TAC.tacRvalue1 = lId
                , TAC.tacRvalue2 = rId
                }]
        return lvalue
        where
            operation :: TAC.Operation
            operation
                | op == Add = TAC.Suma
                | op == Substract = TAC.Resta
                | op == Multiply = TAC.Multiplicacion
                | op == Divide = TAC.Division
                | otherwise = error $ "Operation for " ++ show op ++ " has not implemented for TAC yet"


    genCode (IntLit n) = do
        newId <- newtemp
        let lvalue = Just $ TAC.Constant $ ConstantString newId
        tell [TAC.ThreeAddressCode
                { TAC.tacOperand = TAC.Suma -- TODO: ask if this should be nothing, using Add is not ok
                , TAC.tacLvalue = lvalue
                , TAC.tacRvalue1 = Just $ TAC.Constant $ ConstantInt n
                , TAC.tacRvalue2 = Nothing
                }]
        return lvalue

    genCode (IdExpr (Id Token {cleanedString=s})) = return $ Just $ TAC.Constant $ ConstantString s

    -- TODO: Do type casting correctly
    genCode (Caster expr _) = genCode expr
    genCode e = error $ "This expression hasn't been implemented " ++ show e
