module ExprCodeGenerator where

import CodeGenerator
import Grammar ( BaseExpr(..)
               , Expr(..), Op2(..), Id(..)
               , Op1(..), comparableOp2)
import Tokens (Token(..))
import TypeChecking (Type(..))
import Control.Monad.RWS
import Control.Monad (void)
import SymTable (DictionaryEntry(..), findChain, Dictionary)
import qualified TACType as TAC

instance GenerateCode Expr where
    genCode = void . genCode'

genCode' :: Expr -> CodeGenMonad OperandType
genCode' Expr {expAst=ast, expType=t} = genCodeForExpr t ast

genCodeForExpr :: Type -> BaseExpr -> CodeGenMonad OperandType
genCodeForExpr t (Op2 op lexpr rexpr) = do
    lId <- genCode' lexpr
    rId <- genCode' rexpr
    newId <- newtemp
    let lvalue = TAC.Variable newId
    tell [TAC.ThreeAddressCode
            { TAC.tacOperand = operation
            , TAC.tacLvalue = Just lvalue
            , TAC.tacRvalue1 = Just lId
            , TAC.tacRvalue2 = Just rId
            }]
    return lvalue
    where
        operation :: TAC.Operation
        operation = mapOp2ToTacOperation op

genCodeForExpr t (IntLit n) = do
    newId <- newtemp
    let lvalue = TAC.Variable newId
    tell [TAC.ThreeAddressCode
            { TAC.tacOperand = TAC.Assign
            , TAC.tacLvalue = Just lvalue
            , TAC.tacRvalue1 = Just $ TAC.Constant (show n, t)
            , TAC.tacRvalue2 = Nothing
            }]
    return lvalue

genCodeForExpr t (IdExpr (Id Token {cleanedString=idName} idScope)) = do
    symEntry <- findSymEntry <$> ask
    return $ TAC.Variable $ TACVariable symEntry
    where
        findSymEntry :: Dictionary -> DictionaryEntry
        findSymEntry = head . filter (\s -> scope s == idScope) . findChain idName

-- TODO: Do type casting correctly
genCodeForExpr t (Caster expr _) = genCode' expr
genCodeForExpr _ e = error $ "This expression hasn't been implemented " ++ show e

genCodeForBooleanExpr :: Expr -> OperandType -> OperandType -> CodeGenMonad ()

genCodeForBooleanExpr expr trueLabel falseLabel = case expAst expr of
    TrueLit -> genGoTo trueLabel
    FalseLit -> genGoTo falseLabel
    Op1 Not expr -> genCodeForBooleanExpr expr falseLabel trueLabel
    Op2 op lhs rhs | op `elem` comparableOp2 -> do
        leftExprId <- genCode' lhs
        rightExprId <- genCode' rhs
        tell [TAC.ThreeAddressCode
                { TAC.tacOperand = mapOp2ToTacOperation op
                , TAC.tacLvalue = Just leftExprId
                , TAC.tacRvalue1 = Just rightExprId
                , TAC.tacRvalue2 = Just trueLabel
                }]
        genGoTo falseLabel

mapOp2ToTacOperation :: Op2 -> TAC.Operation
mapOp2ToTacOperation op = case op of
    Lt -> TAC.Lt
    Gt -> TAC.Gt
    Lte -> TAC.Lte
    Gte -> TAC.Gte
    Eq -> TAC.Eq
    Neq -> TAC.Neq
    Add -> TAC.Add
    Substract -> TAC.Sub
    Multiply -> TAC.Mult
    Divide -> TAC.Div
