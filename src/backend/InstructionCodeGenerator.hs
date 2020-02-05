module InstructionCodeGenerator where

import CodeGenerator
import ExprCodeGenerator (genCode', genCodeForBooleanExpr)
import Grammar (Instruction(..), Expr(..), BaseExpr(..), Id(..), IfCase(..), CodeBlock(..), Program(..))
import TACType
import TypeChecking (Type(..))
import Control.Monad.RWS (tell, unless)


instance GenerateCode CodeBlock where
    genCode (CodeBlock instrs) = mapM_ genCode instrs

instance GenerateCode Program where
    genCode (Program codeblock) = genCode codeblock

instance GenerateCode Instruction where
    genCode instruction = do
        next <- newLabel
        genCodeForInstruction instruction next
        genLabel next

genCodeForInstruction :: Instruction -> OperandType -> CodeGenMonad ()

-- Utility instructions
genCodeForInstruction (InstPrint expr) _ = genCode expr

-- Assignments, currently only supported for id assignments
genCodeForInstruction (InstAsig lvalue@Expr {expAst = IdExpr id} rvalue) next =
    if expType rvalue /= TrileanT then do
        operand <- genCode' lvalue
        rValueAddress <- genCode' rvalue
        genIdAssignment operand rValueAddress
    else do
        trueLabel <- newLabel
        falseLabel <- newLabel
        genCodeForBooleanExpr rvalue trueLabel falseLabel
        operand <- genCode' lvalue
        genLabel trueLabel
        genIdAssignment operand $ Constant ("true", TrileanT)
        genGoTo next
        genLabel falseLabel
        genIdAssignment operand $ Constant ("false", TrileanT)
    where
        genIdAssignment :: OperandType -> OperandType -> CodeGenMonad ()
        genIdAssignment lValue rValue =
            tell [ThreeAddressCode
                { tacOperand = Assign
                , tacLvalue = Just lValue
                , tacRvalue1 = Just rValue
                , tacRvalue2 = Nothing
                }]



-- Conditional selection statement
genCodeForInstruction (InstIf ifcases) next = do
    let initInstructions = init ifcases
    let lastInstruction = last ifcases
    mapM_ (genCodeForIfCase next False) initInstructions
    genCodeForIfCase next True lastInstruction

genCodeForIfCase :: OperandType -> Bool -> IfCase -> CodeGenMonad ()
genCodeForIfCase next isLast (GuardedCase expr codeblock) = do
    trueLabel <- newLabel
    falseLabel <- if isLast then return next else newLabel
    genCodeForBooleanExpr expr trueLabel falseLabel
    genLabel trueLabel
    genCode codeblock
    genGoTo next
    unless isLast $ genLabel falseLabel
