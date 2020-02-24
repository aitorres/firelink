module FireLink.FrontEnd.Grammar where

import           Data.List                      (intercalate)
import           FireLink.FrontEnd.Tokens       (Token (..))
import           FireLink.FrontEnd.TypeChecking (Type (..))
import qualified FireLink.Utils                 as U

type Instructions = [Instruction]
type Params = [Expr]
type IfCases = [IfCase]
type SwitchCases = [SwitchCase]

data Id
  = Id Token Int
  deriving Eq

extractIdName :: Id -> String
extractIdName (Id Token {cleanedString=s} _) = s

instance Show Id where
  show (Id tk s) = show tk ++ ":" ++ show s

data BaseExpr
  -- Literals
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
  | StructLit [(Id, Expr)]

  | Op1 Op1 Expr
  | Op2 Op2 Expr Expr
  | Access Expr Id
  | IndexAccess Expr Expr
  | MemAccess Expr
  | IdExpr Id
  | AsciiOf Expr
  | IsActive Expr
  | Size Expr
  | EvalFunc Id Params
  | Caster Expr Type
  deriving Eq

data Op1 = Negate | Not
  deriving Eq

isValidLvalue :: Expr -> Bool
isValidLvalue Expr {expAst=e} = isValidLvalue' e

isValidLvalue' :: BaseExpr -> Bool
isValidLvalue' Access{}      = True
isValidLvalue' MemAccess{}   = True
isValidLvalue' IndexAccess{} = True
isValidLvalue' IdExpr{}      = True
isValidLvalue' _             = False

arithmeticOp1 :: [Op1]
arithmeticOp1 = [Negate]

booleanOp1 :: [Op1]
booleanOp1 = [Not]

data Op2
  = Add | Substract | Multiply | Divide | Mod | Lt | Gt | Lte | Gte | Eq | Neq | And | Or
  | SetUnion | SetIntersect | SetDifference | ColConcat
  deriving Eq

arithmeticOp2 :: [Op2]
arithmeticOp2 = [Add, Substract, Multiply, Divide, Mod]

comparableOp2 :: [Op2]
comparableOp2 = [Lt, Gt, Lte, Gte, Eq, Neq]

booleanOp2 :: [Op2]
booleanOp2 = [And, Or]

setOp2 :: [Op2]
setOp2 = [SetUnion, SetIntersect, SetDifference]

arrayOp2 :: [Op2]
arrayOp2 = [ColConcat]

instance Show Op1 where
  show Negate = "-"
  show Not    = "not"

instance Show Op2 where
  show Add           = "+"
  show Substract     = "-"
  show Multiply      = "*"
  show Divide        = "/"
  show Mod           = "%"
  show Lt            = "lt"
  show Gt            = "gt"
  show Lte           = "lte"
  show Gte           = "gte"
  show Eq            = "eq"
  show Neq           = "neq"
  show And           = "and"
  show Or            = "or"
  show SetUnion      = "union"
  show SetIntersect  = "intersect"
  show SetDifference = "diff"
  show ColConcat     = ">-<"

joinProps :: [(Id, Expr)] -> String
joinProps = intercalate ", " . map (\(i, e) -> show i ++ " <<= " ++ show e)

instance Show BaseExpr where
  show TrueLit = "lit"
  show FalseLit = "unlit"
  show UndiscoveredLit = "undiscovered"
  show NullLit = "abyss"
  show (IntLit a) = show a
  show (FloatLit a) = show a
  show (CharLit a) = [a]
  show (StringLit s) = s
  show (ArrayLit exprs) = "<$" ++ U.joinWithCommas exprs ++ "$>"
  show (StructLit props) = "{ ++ " ++ joinProps props ++ " }"
  show (SetLit exprs) = "{$" ++ U.joinWithCommas exprs ++ "$}"
  show (Op1 o e) = show o ++ " " ++ show e
  show (Op2 o e e') = show e ++ " " ++ show o ++ " " ++ show e'
  show (Access e i) = show e ++ "~>" ++ show i
  show (IndexAccess e e') = show e ++ "<$" ++ show e' ++ "$>"
  show (MemAccess e) = "throw a " ++ show e
  show (IdExpr i) = show i
  show (AsciiOf e) = "ascii_of " ++ show e
  show (IsActive e) = "is_active " ++ show e
  show (Size s) = "size " ++ show s
  show (EvalFunc i p) = "summon " ++ show i ++ " granting " ++ U.joinWithCommas p ++ " to the knight"
  show (Caster a _) = "(casting) " ++ show a

data Expr = Expr {
  expType :: !Type,
  expAst :: !BaseExpr,
  expTok :: !Token
} deriving Eq

instance Show Expr where
  show exp = show (expAst exp) ++ ":" ++ show (expType exp)

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
  deriving Show

data SwitchCase
  = Case Expr CodeBlock
  | DefaultCase CodeBlock
  deriving Show

newtype CodeBlock
  = CodeBlock Instructions
  deriving Show

data RecoverableError
  = MissingProgramEnd
  | MissingDeclarationListEnd
  | MissingInstructionListEnd
  | MissingAliasListEnd
  | MissingClosingBrace
  | MissingFunCallEnd
  | MissingProcCallEnd
  | MissingIfEnd
  | MissingSwitchEnd
  | MissingForEnd
  | MissingForEachEnd
  | MissingWhileEnd
  | MissingColon
  | MissingClosingParens

instance Show RecoverableError where
  show MissingProgramEnd = "Unclosed program block: forgot to say " ++ U.bold ++ "farewell ashen one" ++ U.nocolor
  show MissingDeclarationListEnd = "Unclosed declaration block: you need to state what's " ++ U.bold ++ "in your inventory" ++ U.nocolor
  show MissingInstructionListEnd = "Unclosed instruction block: remember that " ++ U.bold ++ "you died" ++ U.nocolor
  show MissingAliasListEnd = "Unclosed alias list: be thankful of the " ++ U.bold ++ "help received" ++ U.nocolor
  show MissingClosingBrace = "Unclosed brace: mismatched { without its closing " ++ U.bold ++ "}" ++ U.nocolor
  show MissingClosingParens = "Unclosed parenthesis: mismatched ( without its closing " ++ U.bold ++ ")" ++ U.nocolor
  show MissingFunCallEnd = "Unclosed function call: state your grants " ++ U.bold ++ "to the knight" ++ U.nocolor
  show MissingProcCallEnd = "Unclosed procedure call: state your requests " ++ U.bold ++ "to the estus flask" ++ U.nocolor
  show MissingIfEnd = "Unclosed conditional instruction: leave your " ++ U.bold ++ "inventory closed" ++ U.nocolor
  show MissingSwitchEnd = "Unclosed switch instruction: was the " ++ U.bold ++ "dungeon exited" ++ U.nocolor ++ "?"
  show MissingForEnd = "Unclosed bounded iteration: check for " ++ U.bold ++ "max level reached" ++ U.nocolor
  show MissingForEachEnd = "Unclosed structured iteration: have your " ++ U.bold ++ "weaponry repaired" ++ U.nocolor
  show MissingWhileEnd = "Unclosed conditioned iteration: was the " ++ U.bold ++ "covenant left" ++ U.nocolor ++ "?"
  show MissingColon = "Missing colon (" ++ U.bold ++ ":" ++ U.nocolor ++ ") where it was needed"
