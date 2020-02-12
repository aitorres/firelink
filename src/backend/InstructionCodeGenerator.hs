module InstructionCodeGenerator where

import CodeGenerator
import ExprCodeGenerator (genCode', genCodeForBooleanExpr)
import Grammar (Instruction(..), Expr(..), BaseExpr(..), Id(..), IfCase(..), CodeBlock(..), Program(..))
import TACType
import TypeChecking (Type(..))
import Control.Monad.RWS (tell, unless, lift)


instance GenerateCode CodeBlock where
    genCode (CodeBlock instrs) = mapM_ genCode instrs

instance GenerateCode Program where
    genCode (Program codeblock) = genCode codeblock

instance GenerateCode Instruction where
    genCode instruction = do
        next <- newLabel
        genCodeForInstruction instruction next

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
        lift $ print (trueLabel, falseLabel)
        genCodeForBooleanExpr rvalue trueLabel falseLabel
        operand <- genCode' lvalue
        genLabel trueLabel
        genIdAssignment operand $ Constant ("true", TrileanT)
        genGoTo next
        genLabel falseLabel
        genIdAssignment operand $ Constant ("false", TrileanT)
        genLabel next
    where
        genIdAssignment :: OperandType -> OperandType -> CodeGenMonad ()
        genIdAssignment lValue rValue =
            tell [ThreeAddressCode
                { tacOperand = Assign
                , tacLvalue = Just lValue
                , tacRvalue1 = Just rValue
                , tacRvalue2 = Nothing
                }]



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
Indeterminate looping statement
Code-generation is similar as on the slides
-}
genCodeForInstruction (InstWhile guard codeblock) next = do
    begin <- newLabel
    trueLabel <- newLabel
    let falseLabel = next
    genLabel begin
    genCodeForBooleanExpr guard trueLabel falseLabel
    genLabel trueLabel
    genCode codeblock
    genGoTo begin
    genLabel next

genCodeForIfCase :: OperandType -> Bool -> IfCase -> CodeGenMonad ()
genCodeForIfCase next isLast (GuardedCase expr codeblock) = do
    trueLabel <- if isLast then return fall else newLabel
    falseLabel <- if isLast then return next else newLabel
    genCodeForBooleanExpr expr trueLabel falseLabel
    unless isLast $ genLabel trueLabel
    genCode codeblock
    unless isLast $ genGoTo next
    unless isLast $ genLabel falseLabel
