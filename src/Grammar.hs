module Grammar where


type Declarations = [Declaration]
type Instructions = [Instruction]
type Exprs = [Expr]
type MethodDeclarations = [MethodDeclaration]
type Methods = [Method]
type Params = [Expr]
type AliasList = [Alias]
type EnumItems = [EnumItem]
type IfCases = [IfCase]
type SwitchCases = [SwitchCase]
type StructItems = [StructItem]

newtype Id
  = Id String
  deriving Show

data Alias
  = Alias Id Type
  deriving Show

data Declaration
  = UninitializedDeclaration VarType Id Type
  | InitializedDeclaration VarType Id Type Expr
  deriving Show

data Type
  = BigInt
  | SmallInt
  | FloatT
  | CharT
  | BoolT
  | StringType Expr
  | Array Type Expr
  | Set Type
  | Enum EnumItems
  | Record StructItems
  | UnionStruct StructItems
  | Pointer Type
  | AliasType Id
  deriving Show

newtype EnumItem
  = EnumItem Id
  deriving Show

data StructItem
  = StructItem Id Type
  deriving Show

data VarType
  = Const
  | Var
  deriving Show

data MethodDeclaration
   = MethodDeclaration ParamType Id Type
   deriving Show

data ParamType
  = Val
  | Ref
  deriving Show

data Method
  = Function Id MethodDeclarations Type CodeBlock
  | Procedure Id MethodDeclarations CodeBlock
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

data Program
  = Program AliasList Methods CodeBlock
  deriving Show

data Instruction
  = InstAsig Id Expr
  | InstCallProc Id Params
  | InstCallFunc Id Params
  | InstReturn
  | InstReturnWith Expr
  | InstPrint Expr
  | InstRead Id
  | InstIf IfCases
  | InstForEach Id Id CodeBlock
  | InstFor Id Expr Expr CodeBlock
  | InstSwitch Id SwitchCases
  | InstWhile Expr CodeBlock
  deriving Show

data IfCase
  = GuardedCase Expr CodeBlock
  | ElseCase CodeBlock
  deriving Show

data SwitchCase
  = Case Expr CodeBlock
  | DefaultCase CodeBlock
  deriving Show

data CodeBlock
  = CodeBlock Declarations Instructions
  deriving Show
