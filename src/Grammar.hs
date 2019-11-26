module Grammar where

import Lexer (Token)

type Instructions = [Instruction]
type Exprs = [Expr]
type Params = [Expr]
type IfCases = [IfCase]
type SwitchCases = [SwitchCase]

newtype Id
  = Id Token
  deriving Show

data Expr
  = TrueLit
  | FalseLit
  | UndiscoveredLit
  | NullLit
  | IntLit Int
  | FloatLit Float
  | CharLit Char
  | StringLit String
  | ArrayLit Exprs
  | SetLit Exprs
  | EvalFunc Id Params
  | Add Expr Expr
  | Substract Expr Expr
  | Multiply Expr Expr
  | Divide Expr Expr
  | Mod Expr Expr
  | Negative Expr
  | Lt Expr Expr
  | Gt Expr Expr
  | Lte Expr Expr
  | Gte Expr Expr
  | Eq Expr Expr
  | Neq Expr Expr
  | And Expr Expr
  | Or Expr Expr
  | Not Expr
  | Access Expr Id
  | IndexAccess Expr Expr
  | MemAccess Expr
  | IdExpr Id
  | AsciiOf Expr
  | ColConcat Expr Expr
  | SetUnion Expr Expr
  | SetIntersect Expr Expr
  | SetDiff Expr Expr
  | SetSize Expr
  deriving Show

newtype Program
  = Program CodeBlock
  deriving Show

data Instruction
  = InstAsig Expr Expr
  | InstCallProc Id Params
  | InstCallFunc Id Params
  | InstReturn
  | InstReturnWith Expr
  | InstPrint Expr
  | InstRead Expr
  | InstIf IfCases
  | InstForEach Id Expr CodeBlock
  | InstFor Id Expr Expr CodeBlock
  | InstSwitch Expr SwitchCases
  | InstWhile Expr CodeBlock
  | InstMalloc Expr
  deriving Show

data IfCase
  = GuardedCase Expr CodeBlock
  | ElseCase CodeBlock
  deriving Show

data SwitchCase
  = Case Expr CodeBlock
  | DefaultCase CodeBlock
  deriving Show

newtype CodeBlock
  = CodeBlock Instructions
  deriving Show

data ArgType = Val | Ref
  deriving (Show, Eq)

data GrammarType
  = Simple Token (Maybe Expr)
  | Compound Token GrammarType (Maybe Expr)
  | Record Token [(Id, GrammarType)]
  | Callable (Maybe GrammarType) [(ArgType, Id, GrammarType)]
  deriving Show
