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

genCodeForExpr t (IntLit n) = return $ TAC.Constant (show n, t)
genCodeForExpr t (FloatLit n) = return $ TAC.Constant (show n, t)

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
        let isTrueNotFall = not $ isFall trueLabel
        let isFalseNotFall = not $ isFall falseLabel
        if isTrueNotFall && isFalseNotFall then do
            tell [TAC.ThreeAddressCode
                    { TAC.tacOperand = mapOp2ToTacOperation op
                    , TAC.tacLvalue = Just leftExprId
                    , TAC.tacRvalue1 = Just rightExprId
                    , TAC.tacRvalue2 = Just trueLabel
                    }]
            genGoTo falseLabel
        else if isTrueNotFall then
            tell [TAC.ThreeAddressCode
                    { TAC.tacOperand = mapOp2ToTacOperation op
                    , TAC.tacLvalue = Just leftExprId
                    , TAC.tacRvalue1 = Just rightExprId
                    , TAC.tacRvalue2 = Just trueLabel
                    }]
        else when isFalseNotFall (tell [TAC.ThreeAddressCode
                    { TAC.tacOperand = complement $ mapOp2ToTacOperation op
                    , TAC.tacLvalue = Just leftExprId
                    , TAC.tacRvalue1 = Just rightExprId
                    , TAC.tacRvalue2 = Just falseLabel
                    }])

    -- Conjunction and disjunction
    Op2 op lhs rhs | op `elem` booleanOp2 -> do
        lhsTrueLabel <-
            -- for `or` we need to generate a new `true` label if the current one is `fall`
            if op == Or then
                (if isFall trueLabel then newLabel else return trueLabel)
            -- for `and` we just need to `fall`
            else
                return fall
        lhsFalseLabel <-
            if op == Or then
                return fall
            else
                (if isFall falseLabel then newLabel else return falseLabel)
        let rhsTrueLabel = trueLabel
        let rhsFalseLabel = falseLabel

        if op == Or then do
            genCodeForBooleanExpr lhs lhsTrueLabel lhsFalseLabel
            genCodeForBooleanExpr rhs rhsTrueLabel rhsFalseLabel
            when (isFall trueLabel) (genLabel lhsTrueLabel)
        else do
            genCodeForBooleanExpr lhs lhsTrueLabel lhsFalseLabel
            genCodeForBooleanExpr rhs rhsTrueLabel rhsFalseLabel
            when (isFall falseLabel) (genLabel lhsFalseLabel)

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

complement :: TAC.Operation -> TAC.Operation
complement TAC.Lt = TAC.Gte
complement TAC.Lte = TAC.Gt
complement TAC.Gt = TAC.Lte
complement TAC.Gte = TAC.Lt
complement TAC.Eq = TAC.Neq
complement TAC.Neq = TAC.Eq
