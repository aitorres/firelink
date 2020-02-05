module ExprCodeGenerator where

import CodeGenerator
import Grammar ( BaseExpr(..)
               , Expr(..), Op2(..), Id(..)
               , Op1(..), comparableOp2, booleanOp2)
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
genCodeForExpr _ (Op2 op lexpr rexpr) = do
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

genCodeForExpr _ (IdExpr (Id Token {cleanedString=idName} idScope)) = do
    symEntry <- findSymEntry <$> ask
    return $ TAC.Variable $ TACVariable symEntry
    where
        findSymEntry :: Dictionary -> DictionaryEntry
        findSymEntry = head . filter (\s -> scope s == idScope) . findChain idName

-- TODO: Do type casting correctly
genCodeForExpr t (Caster expr _) = genCode' expr

genCodeForExpr _ (Op1 Negate expr) = do
    rId <- genCode' expr
    lvalue <- TAC.Variable <$> newtemp
    tell [TAC.ThreeAddressCode
            { TAC.tacOperand = TAC.Minus
            , TAC.tacLvalue = Just lvalue
            , TAC.tacRvalue1 = Just rId
            , TAC.tacRvalue2 = Nothing
            }]
    return lvalue

genCodeForExpr _ e = error $ "This expression hasn't been implemented " ++ show e

genCodeForBooleanExpr :: Expr -> OperandType -> OperandType -> CodeGenMonad ()

genCodeForBooleanExpr expr trueLabel falseLabel = case expAst expr of
    -- Boolean literals
    TrueLit -> genGoTo trueLabel
    FalseLit -> genGoTo falseLabel

    -- Boolean negation
    Op1 Not expr -> genCodeForBooleanExpr expr falseLabel trueLabel

    IdExpr _ -> do
        symEntry <- genCode' expr
        tell [TAC.ThreeAddressCode
                { TAC.tacOperand = TAC.Eq
                , TAC.tacLvalue = Just symEntry
                , TAC.tacRvalue1 = Just $ TAC.Constant ("true", TrileanT)
                , TAC.tacRvalue2 = Just trueLabel
                }]
        genGoTo falseLabel
    -- Boolean comparation
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

    -- Conjunction and disjunction
    Op2 op lhs rhs | op `elem` booleanOp2 -> do
        lhsTrueLabel <- if op == Or then return trueLabel else newLabel
        lhsFalseLabel <- if op == Or then newLabel else return falseLabel
        let rhsTrueLabel = trueLabel
        let rhsFalseLabel = falseLabel
        genCodeForBooleanExpr lhs lhsTrueLabel lhsFalseLabel
        genLabel $ if op == Or then lhsFalseLabel else lhsTrueLabel
        genCodeForBooleanExpr rhs rhsTrueLabel rhsFalseLabel

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
    Mod -> TAC.Mod
