module ExprCodeGenerator where

import CodeGenerator
import Grammar (BaseExpr(..), Expr(..), Op2(..), Id(..))
import Tokens (Token(..))
import TypeChecking (Type(..))
import Control.Monad.RWS
import Control.Monad (void)
import SymTable (DictionaryEntry(..))
import qualified TACType as TAC

instance GenerateCode Expr where
    genCode = void . genCode'

genCode' :: Expr -> CodeGenMonad (TAC.Operand DictionaryEntry Type)
genCode' Expr {expAst=ast, expType=t} = genCodeForExpr t ast

genCodeForExpr :: Type -> BaseExpr -> CodeGenMonad (TAC.Operand DictionaryEntry Type)
genCodeForExpr t (Op2 op lexpr rexpr) = do
    lId <- genCode' lexpr
    rId <- genCode' rexpr
    newId <- newtemp
    let lvalue = TAC.Constant (newId, t)
    tell [TAC.ThreeAddressCode
            { TAC.tacOperand = operation
            , TAC.tacLvalue = Just lvalue
            , TAC.tacRvalue1 = Just lId
            , TAC.tacRvalue2 = Just rId
            }]
    return lvalue
    where
        operation :: TAC.Operation
        operation
            | op == Add = TAC.Add
            | op == Substract = TAC.Sub
            | op == Multiply = TAC.Mult
            | op == Divide = TAC.Div
            | otherwise = error $ "Operation for " ++ show op ++ " has not implemented for TAC yet"


genCodeForExpr t (IntLit n) = do
    newId <- newtemp
    let lvalue = TAC.Constant (newId, t)
    tell [TAC.ThreeAddressCode
            { TAC.tacOperand = TAC.Assign
            , TAC.tacLvalue = Just lvalue
            , TAC.tacRvalue1 = Just $ TAC.Constant (show n, t)
            , TAC.tacRvalue2 = Nothing
            }]
    return lvalue

genCodeForExpr t (IdExpr (Id Token {cleanedString=s})) = return $ TAC.Constant (s, t)

-- TODO: Do type casting correctly
genCodeForExpr t (Caster expr _) = genCode' expr
genCodeForExpr _ e = error $ "This expression hasn't been implemented " ++ show e
