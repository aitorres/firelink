module InstructionCodeGenerator where

import CodeGenerator
import ExprCodeGenerator (genCode', genCodeForBooleanExpr)
import Grammar (Instruction(..), Expr(..), BaseExpr(..), Id(..), IfCase(..), CodeBlock(..), Program(..))
import TACType
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
genCodeForInstruction (InstAsig lvalue@Expr {expAst = IdExpr id} rvalue) _ = do
    operand <- genCode' lvalue
    rValueAddress <- genCode' rvalue
    tell [ThreeAddressCode
            { tacOperand = Assign
            , tacLvalue = Just operand
            , tacRvalue1 = Just rValueAddress
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
