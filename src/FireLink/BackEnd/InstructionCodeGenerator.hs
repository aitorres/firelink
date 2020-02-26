module FireLink.BackEnd.InstructionCodeGenerator where

import           Control.Monad.RWS                  (ask, tell, unless, when)
import           FireLink.BackEnd.CodeGenerator
import           FireLink.BackEnd.ExprCodeGenerator (genBooleanComparison,
                                                     genCode',
                                                     genCodeForBooleanExpr,
                                                     genCodeForExpr, genOp2Code)
import           FireLink.FrontEnd.Grammar          (BaseExpr (..),
                                                     CodeBlock (..), Expr (..),
                                                     Id (..), IfCase (..),
                                                     Instruction (..),
                                                     Program (..),
                                                     SwitchCase (..))
import qualified FireLink.FrontEnd.Grammar          as G (Id (..), Op2 (..))
import           FireLink.FrontEnd.SymTable         (getUnionAttrId, wordSize)
import           FireLink.FrontEnd.Tokens           as T (Token (..))
import           FireLink.FrontEnd.TypeChecking     (Type (..))
import           TACType


instance GenerateCode Program where
    genCode (Program codeblock@(CodeBlock _ maxOffset)) = do
        setTempOffset alignedOffset
        genCode codeblock
        where
            alignedOffset =
                if maxOffset `mod` wordSize == 0
                then maxOffset
                else maxOffset +  wordSize - (maxOffset `mod` wordSize)

instance GenerateCode CodeBlock where
    genCode (CodeBlock instrs _) = mapM_ genCode instrs

instance GenerateCode Instruction where
    genCode instruction = do
        next <- newLabel
        genCodeForInstruction instruction next

genCodeForInstruction :: Instruction -> OperandType -> CodeGenMonad ()

-- Utility instructions
genCodeForInstruction (InstPrint expr) _ = genCode expr

-- Assignments, currently supported for id assignments, structs & unions
genCodeForInstruction (InstAsig lvalue rvalue) next =
    if supportedLvalue lvalue then do
        operand <- genCode' lvalue
        if expType rvalue /= TrileanT then do
            rValueAddress <- genCode' rvalue
            genIdAssignment operand rValueAddress
        else do
            trueLabel <- newLabel
            falseLabel <- newLabel
            genCodeForBooleanExpr (expAst rvalue) trueLabel falseLabel
            genLabel trueLabel
            genIdAssignment operand $ Constant ("true", TrileanT)
            genGoTo next
            genLabel falseLabel
            genIdAssignment operand $ Constant ("false", TrileanT)
            genLabel next

        -- The following code takes care of the isActive attribute for unions
        when (isUnionT $ expAst lvalue) $ do
            let Expr { expAst = (Access unionExpr (G.Id Token {T.cleanedString=idName} idScope)) } = lvalue
            propertySymEntry <- findSymEntry idName idScope <$> ask
            let argPos = getUnionAttrId propertySymEntry
            unionExprOp <- genCode' unionExpr
            tell [ThreeAddressCode
                { tacOperand = Assign
                , tacLvalue = Just unionExprOp
                , tacRvalue1 = Just $ Constant (show argPos, BigIntT)
                , tacRvalue2 = Nothing
                }]

    else error $ "Lvalue currently not supported for assignments: " ++ show lvalue
    where
        supportedLvalue :: Expr -> Bool
        supportedLvalue Expr {expAst = IdExpr _ }  = True
        supportedLvalue Expr {expAst = Access _ _} = True
        supportedLvalue _                          = False

        isUnionT :: BaseExpr -> Bool
        isUnionT (Access Expr { expType = UnionT _ _ } _) = True
        isUnionT _                                        = False

{-
Conditional selection statement
The code-generation depends on the position of a guard in the list

If it is the last one, then its next instruction is right there,
so there is no need to make a `goto falseLabel` because there is no falseLabel

Otherwise, the next instruction of a guard block is the next instruction right
after the if whole block
-}
genCodeForInstruction (InstIf ifcases) next = do
    let initInstructions = init ifcases
    let lastInstruction = last ifcases
    mapM_ (genCodeForIfCase next False) initInstructions
    genCodeForIfCase next True lastInstruction
    genLabel next

{-
Selection by cases statement (switch)
The code-generation is similar to that of a conditional statement.

Note that the base expression is evaluated just once, in order to avoid
undesired repetition of side effects, and also as an optimization.

If we are generating the last case, the next instruction is right beneath,
so no `goto falseLabel` is generated.

Otherwise, the next instruction of a swith case is the next instruction right
after the switch whole block.

One consideration: if a default case is provided, then it's generated as a normal
switch case in which the given expr matches the base expr (basically,
a comparison of the base expression against itself is raised). This can be
further optimized to remove the unnecessary comparison.
-}
genCodeForInstruction (InstSwitch baseExpr switchCases) next = do
    bExprOperand <- genCode' baseExpr
    let initCases = init switchCases
    let lastCase = last switchCases
    mapM_ (genCodeForSwitchCase next bExprOperand False) initCases
    genCodeForSwitchCase next bExprOperand True lastCase
    genLabel next

{-
Indeterminate looping statement
Code-generation is similar as on the slides
-}
genCodeForInstruction (InstWhile guard codeblock) next = do
    (begin, trueLabel, falseLabel) <- setUpIteration next
    genLabel begin
    genCodeForBooleanExpr (expAst guard) trueLabel falseLabel
    genLabel trueLabel
    genCode codeblock
    genGoTo begin
    genLabel next

{-
Bounded looping statement
Code generation is similar to that of an indeterminate loop,
in which the guard is to check whether the iteration variable has already
reached the bound, and additional instructions are added underneath the code block
in order to successfully update the value of the iteration variable
after every iteration.
-}
genCodeForInstruction (InstFor id step bound codeblock) next = do
    (begin, trueLabel, falseLabel) <- setUpIteration next
    let idAst = IdExpr id
    idOperand <- genCodeForExpr BigIntT idAst
    boundOperand <- genCode' bound
    genLabel begin
    genBooleanComparison idOperand boundOperand trueLabel falseLabel G.Lt
    genLabel trueLabel
    genCode codeblock
    incOperand <- genIncrement idOperand step
    genIdAssignment idOperand incOperand
    genGoTo begin
    genLabel next
    where
            genIncrement :: OperandType -> Expr -> CodeGenMonad OperandType
            genIncrement idOp step = do
                stepOp <- genCode' step
                genOp2Code Add idOp stepOp

genCodeForInstruction i _ = error $ "This instruction hasn't been implemented " ++ show i

-- Aux function for iterations
setUpIteration :: OperandType -> CodeGenMonad (OperandType, OperandType, OperandType)
setUpIteration next = do
    begin <- newLabel
    trueLabel <- newLabel
    return (begin, trueLabel, next)

genCodeForIfCase :: OperandType -> Bool -> IfCase -> CodeGenMonad ()
genCodeForIfCase next isLast (GuardedCase expr codeblock) = do
    trueLabel <- if isLast then return fall else newLabel
    falseLabel <- if isLast then return next else newLabel
    genCodeForBooleanExpr (expAst expr) trueLabel falseLabel
    unless isLast $ genLabel trueLabel
    genCode codeblock
    unless isLast $ genGoTo next
    unless isLast $ genLabel falseLabel

genCodeForSwitchCase :: OperandType -> OperandType -> Bool -> SwitchCase -> CodeGenMonad ()
genCodeForSwitchCase next bExprOperand isLast sCase = do
    trueLabel <- if isLast then return fall else newLabel
    falseLabel <- if isLast then return next else newLabel
    case sCase of
        Case expr codeblock -> do
            caseExprOperand <- genCode' expr
            genBooleanComparison bExprOperand caseExprOperand trueLabel falseLabel G.Eq
            unless isLast $ genLabel trueLabel
            genCode codeblock
        DefaultCase codeblock -> do
            genBooleanComparison bExprOperand bExprOperand trueLabel falseLabel G.Eq
            unless isLast $ genLabel trueLabel
            genCode codeblock
    unless isLast $ genGoTo next
    unless isLast $ genLabel falseLabel
