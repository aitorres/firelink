module Grammar where

import Lexer (Token)
import Data.List (intercalate)

type Instructions = [Instruction]
type Params = [Expr]
type IfCases = [IfCase]
type SwitchCases = [SwitchCase]

newtype Id
  = Id Token

instance Show Id where
  show (Id tk) = show tk

data Expr
  = TrueLit
  | FalseLit
  | UndiscoveredLit
  | NullLit
  | IntLit Int
  | FloatLit Float
  | CharLit Char
  | StringLit String
  | ArrayLit [Expr]
  | SetLit [Expr]
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

joinExprList :: [Expr] -> String
joinExprList = intercalate ", " . map show

instance Show Expr where
  show TrueLit = "lit"
  show FalseLit = "unlit"
  show UndiscoveredLit = "unlit"
  show NullLit = "abyss"
  show (IntLit a) = show a
  show (FloatLit a) = show a
  show (CharLit a) = [a]
  show (StringLit s) = s
  show (ArrayLit exprs) = "<$" ++ joinExprList exprs ++ "$>"
  show (SetLit exprs) = "{$" ++ joinExprList exprs ++ "$}"
  show (EvalFunc i params) = show i ++ "(" ++ joinExprList params ++ ")"
  show (Add e e') = show e ++ " + " ++ show e'
  show (Substract e e') = show e ++ " - " ++ show e'
  show (Multiply e e') = show e ++ " * " ++ show e'
  show (Divide e e') = show e ++ " / " ++ show e'
  show (Mod e e') = show e ++ " % " ++ show e'
  show (Negative e) = "- " ++ show e
  show (Lt e e') = show e ++ " lt " ++ show e'
  show (Gt e e') = show e ++ " gt " ++ show e'
  show (Lte e e') = show e ++ " lte " ++ show e'
  show (Gte e e') = show e ++ " gte " ++ show e'
  show (Eq e e') = show e ++ " eq " ++ show e'
  show (Neq e e') = show e ++ " neq " ++ show e'
  show (And e e') = show e ++ " and " ++ show e'
  show (Or e e') = show e ++ " or " ++ show e'
  show (Not e) = "not " ++ show e
  show (Access e i) = show e ++ "~>" ++ show i
  show (IndexAccess e e') = show e ++ "<$" ++ show e' ++ "$>"
  show (MemAccess e) = "throw a " ++ show e
  show (IdExpr i) = show i
  show (AsciiOf e) = "ascii_of " ++ show e
  show (ColConcat e e') = show e ++ ">-<" ++ show e'
  show (SetUnion e e') = show e ++ " union " ++ show e'
  show (SetIntersect e e') = show e ++ " intersect " ++ show e'
  show (SetDiff e e') = show e ++ " diff " ++ show e'
  show (SetSize s) = "size " ++ show s

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
  | InstFreeMem Expr
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
