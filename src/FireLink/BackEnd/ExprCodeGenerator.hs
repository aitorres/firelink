module FireLink.BackEnd.ExprCodeGenerator where

import           Control.Monad                  (void)
import           Control.Monad.RWS
import           Data.Char                      (ord)
import           FireLink.BackEnd.CodeGenerator
import           FireLink.FrontEnd.Grammar      (BaseExpr (..), Expr (..),
                                                 Id (..), Op1 (..), Op2 (..),
                                                 booleanOp2, comparableOp2)
import           FireLink.FrontEnd.SymTable     (Dictionary,
                                                 DictionaryEntry (..),
                                                 findSymEntryById, getOffset,
                                                 getUnionAttrId)
import           FireLink.FrontEnd.Tokens       (Token (..))
import           FireLink.FrontEnd.TypeChecking (Type (..))
import qualified TACType                        as TAC

instance GenerateCode Expr where
    genCode = void . genCode'

genCode' :: Expr -> CodeGenMonad OperandType
genCode' e@Expr {expAst=ast@(Access struct _), expType=t} = do
    ret <- genCodeForExpr t ast

    when (isUnionT $ expType struct ) $ do
        trueLabel <- newLabel
        falseLabel <- newLabel
        let activeExpr = IsActive e
        genCodeForBooleanExpr activeExpr trueLabel falseLabel
        genLabel falseLabel
        raiseRunTimeError "RUNTIME ERROR: Trying to access inactive property of Union"
        genLabel trueLabel
    return ret
    where
        isUnionT :: Type -> Bool
        isUnionT (UnionT _ _) = True
        isUnionT _          = False
genCode' Expr {expAst=ast, expType=t} = genCodeForExpr t ast

genCodeForExpr :: Type -> BaseExpr -> CodeGenMonad OperandType
-- | Ids
genCodeForExpr _ (IdExpr exprId) = do
    symEntry <- findSymEntryById exprId <$> ask
    return $ TAC.Id $ TACVariable symEntry $ getOffset symEntry

-- | Property access
genCodeForExpr t (Access expr propId) = do
    let Expr { expAst = eAst, expType = eT } = expr
    TAC.Id x <- genCodeForExpr eT eAst
    let rO = getTACSymEntryOffset x
    propSymEntry <- findSymEntryById propId <$> ask
    let propOffset = getOffset propSymEntry
    return $ TAC.Id $ TACVariable propSymEntry $ propOffset + rO

genCodeForExpr _ (EvalFunc fId params) = do
    paramsLength <- genParams params
    ret <- TAC.Id <$> newtemp
    funEntry <- findSymEntryById fId <$> ask
    tell [TAC.ThreeAddressCode
            { TAC.tacOperand = TAC.Call
            , TAC.tacLvalue = Just ret
            , TAC.tacRvalue1 = Just $ TAC.Label $ name funEntry
            , TAC.tacRvalue2 = Just $ TAC.Constant (show paramsLength, SmallIntT)
            }]
    return ret

genCodeForExpr TrileanT exp = do
    trueLabel <- newLabel
    falseLabel <- newLabel
    next <- newLabel
    lvalue <- TAC.Id <$> newtemp
    genCodeForBooleanExpr exp trueLabel falseLabel
    genLabel trueLabel
    genIdAssignment lvalue $ TAC.Constant ("true", TrileanT)
    genGoTo next
    genLabel falseLabel
    genIdAssignment lvalue $ TAC.Constant ("false", TrileanT)
    genLabel next
    return lvalue

genCodeForExpr _ (Op2 op lexpr rexpr) = do
    lId <- genCode' lexpr
    rId <- genCode' rexpr
    genOp2Code operation lId rId
    where
        operation :: TAC.Operation
        operation = mapOp2ToTacOperation op

genCodeForExpr t (IntLit n) = return $ TAC.Constant (show n, t)
genCodeForExpr t (FloatLit n) = return $ TAC.Constant (show n, t)
genCodeForExpr t (CharLit c) = return $ TAC.Constant (show $ ord c, t)

genCodeForExpr t (Caster expr newType) = do
    tempOp <- genCode' expr
    let oldType = expType expr
    lvalue <- TAC.Id <$> newtemp
    tell [TAC.ThreeAddressCode
            { TAC.tacOperand = TAC.Cast (show oldType) (show newType)
            , TAC.tacLvalue = Just lvalue
            , TAC.tacRvalue1 = Just tempOp
            , TAC.tacRvalue2 = Nothing
            }]
    return lvalue

genCodeForExpr _ (Op1 Negate expr) = do
    rId <- genCode' expr
    lvalue <- TAC.Id <$> newtemp
    tell [TAC.ThreeAddressCode
            { TAC.tacOperand = TAC.Minus
            , TAC.tacLvalue = Just lvalue
            , TAC.tacRvalue1 = Just rId
            , TAC.tacRvalue2 = Nothing
            }]
    return lvalue

{-
This implementation works because in TAC and in MIPS characters are just numbers
-}
genCodeForExpr _ (AsciiOf expr) = genCode' expr

genCodeForExpr _ e = error $ "This expression hasn't been implemented " ++ show e

genParams :: [Expr] -> CodeGenMonad Int
genParams params = do
    operands <- mapM genCode' params
    tell $ map createParam operands
    return $ length params
    where
        createParam :: OperandType -> TAC
        createParam o = TAC.ThreeAddressCode
            { TAC.tacOperand = TAC.Param
            , TAC.tacLvalue = Nothing
            , TAC.tacRvalue1 = Just o
            , TAC.tacRvalue2 = Nothing
            }


genOp2Code :: TAC.Operation -> OperandType -> OperandType -> CodeGenMonad OperandType
genOp2Code operation lId rId = do
    lvalue <- TAC.Id <$> newtemp
    tell [TAC.ThreeAddressCode
            { TAC.tacOperand = operation
            , TAC.tacLvalue = Just lvalue
            , TAC.tacRvalue1 = Just lId
            , TAC.tacRvalue2 = Just rId
            }]
    return lvalue

genCodeForBooleanExpr :: BaseExpr -> OperandType -> OperandType -> CodeGenMonad ()

genCodeForBooleanExpr expr trueLabel falseLabel = case expr of
    -- Boolean literals
    TrueLit -> unless (isFall trueLabel) $ genGoTo trueLabel
    FalseLit -> unless (isFall falseLabel) $ genGoTo falseLabel

    -- Boolean negation
    Op1 Not expr -> genCodeForBooleanExpr (expAst expr) falseLabel trueLabel

    IdExpr _ -> do
        symEntry <- genCodeForExpr TrileanT expr
        let isTrueNotFall = not $ isFall trueLabel
        let isFalseNotFall = not $ isFall falseLabel
        if isTrueNotFall && isFalseNotFall then do
            tell [TAC.ThreeAddressCode
                    { TAC.tacOperand = TAC.Eq
                    , TAC.tacLvalue = Just symEntry
                    , TAC.tacRvalue1 = Just $ TAC.Constant ("true", TrileanT)
                    , TAC.tacRvalue2 = Just trueLabel
                    }]
            genGoTo falseLabel
        else if isTrueNotFall then
            tell [TAC.ThreeAddressCode
                    { TAC.tacOperand = TAC.Eq
                    , TAC.tacLvalue = Just symEntry
                    , TAC.tacRvalue1 = Just $ TAC.Constant ("true", TrileanT)
                    , TAC.tacRvalue2 = Just trueLabel
                    }]
        else when isFalseNotFall (tell [TAC.ThreeAddressCode
                    { TAC.tacOperand = TAC.Eq
                    , TAC.tacLvalue = Just symEntry
                    , TAC.tacRvalue1 = Just $ TAC.Constant ("false", TrileanT)
                    , TAC.tacRvalue2 = Just falseLabel
                    }])
    -- Boolean comparation
    Op2 op lhs rhs | op `elem` comparableOp2 -> do
        leftExprId <- genCode' lhs
        rightExprId <- genCode' rhs
        genBooleanComparison leftExprId rightExprId trueLabel falseLabel op

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
            genCodeForBooleanExpr (expAst lhs) lhsTrueLabel lhsFalseLabel
            genCodeForBooleanExpr (expAst rhs) rhsTrueLabel rhsFalseLabel
            when (isFall trueLabel) (genLabel lhsTrueLabel)
        else do
            genCodeForBooleanExpr (expAst lhs) lhsTrueLabel lhsFalseLabel
            genCodeForBooleanExpr (expAst rhs) rhsTrueLabel rhsFalseLabel
            when (isFall falseLabel) (genLabel lhsFalseLabel)
    (EvalFunc _ _) -> do
        resultOperand <- genCodeForExpr TrileanT expr
        genBooleanComparison resultOperand (TAC.Constant ("true", TrileanT)) trueLabel falseLabel Eq

    IsActive Expr { expAst = Access unionExpr propId } -> do
        -- Get union's base direction (where real is_active is stored)
        realArgOperand <- genCode' unionExpr

        -- Get received argument's attr number for comparison
        propSymEntry <- findSymEntryById propId <$> ask
        let propArgPos = getUnionAttrId propSymEntry

        -- Assign received argument's attr number to a new temp
        propArgOperand <- TAC.Id <$> newtemp
        genIdAssignment propArgOperand $ TAC.Constant (show propArgPos, BigIntT)

        -- Gen boolean comparison
        genBooleanComparison propArgOperand realArgOperand trueLabel falseLabel Eq

    e -> error $ "Unexpected boolean expression: No pattern on genCodeForBooleanExpr for  " ++ show e

-- Used in boolean expr generation as well as switch case generation
genBooleanComparison :: OperandType -> OperandType -> OperandType -> OperandType -> Op2 -> CodeGenMonad ()
genBooleanComparison leftExprId rightExprId trueLabel falseLabel op = do
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

mapOp2ToTacOperation :: Op2 -> TAC.Operation
mapOp2ToTacOperation op = case op of
    Lt        -> TAC.Lt
    Gt        -> TAC.Gt
    Lte       -> TAC.Lte
    Gte       -> TAC.Gte
    Eq        -> TAC.Eq
    Neq       -> TAC.Neq
    Add       -> TAC.Add
    Substract -> TAC.Sub
    Multiply  -> TAC.Mult
    Divide    -> TAC.Div
    Mod       -> TAC.Mod

complement :: TAC.Operation -> TAC.Operation
complement TAC.Lt  = TAC.Gte
complement TAC.Lte = TAC.Gt
complement TAC.Gt  = TAC.Lte
complement TAC.Gte = TAC.Lt
complement TAC.Eq  = TAC.Neq
complement TAC.Neq = TAC.Eq
