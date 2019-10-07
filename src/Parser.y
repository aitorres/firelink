{
module Parser (parse) where

import Lexer (Token (..), AbstractToken (..), AlexPosn (..))
import AST (AST)
import Data.Maybe
}

%name                                                                     parse
%tokentype                                                              { Token }
%error                                                                  { parseErrors }
%monad                                                                  { AST }

%left comma

%left granting

%left plus minus
%left mult div mod

%left and or

%left accessor
%right arrOpen arrClose

%left not UMINUS

%nonassoc lt lte gt gte eq neq granting

%token
  programBegin                                                          { Token TkProgramBegin _ _ }
  programEnd                                                            { Token TkProgramEnd _ _ }

  aliasListBegin                                                        { Token TkAliasListBegin _ _ }
  aliasListEnd                                                          { Token TkAliasListEnd _ _ }
  alias                                                                 { Token TkAlias _ _ }

  id                                                                    { Token TkId $$ _ }

  ofType                                                                { Token TkOfType _ _ }

  paramRequest                                                          { Token TkRequesting _ _ }
  parVal                                                                { Token TkVal _ _ }
  parRef                                                                { Token TkRef _ _ }

  bigInt                                                                { Token TkBigInt _ _ }
  smallInt                                                              { Token TkSmallInt _ _ }
  float                                                                 { Token TkFloat _ _ }
  char                                                                  { Token TkChar _ _ }
  bool                                                                  { Token TkBool _ _ }
  ltelit                                                                { Token TkLteLit _ _ }
  string                                                                { Token TkString _ _ }
  array                                                                 { Token TkArray _ _ }
  set                                                                   { Token TkSet _ _ }
  enum                                                                  { Token TkEnum _ _ }
  unionStruct                                                           { Token TkUnionStruct _ _ }
  record                                                                { Token TkRecord _ _ }
  pointer                                                               { Token TkPointer _ _ }

  intLit                                                                { Token TkIntLit $$ _ }
  floatLit                                                              { Token TkFloatLit $$ _ }
  charLit                                                               { Token TkCharLit $$ _ }
  stringLit                                                             { Token TkStringLit $$ _ }

  functionBegin                                                         { Token TkInvocation _ _ }
  functionType                                                          { Token TkInvocationType _ _ }
  functionEnd                                                           { Token TkInvocationEnd _ _ }

  procedureBegin                                                        { Token TkSpell _ _ }
  procedureEnd                                                          { Token TkSpellEnd _ _ }

  comma                                                                 { Token TkComma _ _ }
  brOpen                                                                { Token TkBraceOpen _ _ }
  brClose                                                               { Token TkBraceClosed _ _ }

  with                                                                  { Token TkWith _ _ }
  declarend                                                             { Token TkDeclarationEnd _ _ }

  const                                                                 { Token TkConst _ _ }
  var                                                                   { Token TkVar _ _ }
  asig                                                                  { Token TkAsig _ _ }

  instructionsBegin                                                     { Token TkInstructionBegin _ _ }
  instructionsEnd                                                       { Token TkInstructionEnd _ _ }
  seq                                                                   { Token TkSeq _ _ }

  cast                                                                  { Token TkCast _ _ }
  offering                                                              { Token TkOffering _ _ }

  summon                                                                { Token TkSummon _ _ }
  granting                                                              { Token TkGranting _ _ }

  return                                                                { Token TkReturn _ _ }
  returnWith                                                            { Token TkReturnWith _ _ }

  print                                                                 { Token TkPrint _ _ }
  read                                                                  { Token TkRead _ _ }

  whileBegin                                                            { Token TkWhile _ _ }
  whileEnd                                                              { Token TkEndWhile _ _ }
  covenantIsActive                                                      { Token TkCovenantIsActive _ _ }

  ifBegin                                                               { Token TkIf _ _ }
  ifEnd                                                                 { Token TkEndIf _ _ }
  colon                                                                 { Token TkColon _ _ }
  else                                                                  { Token TkElse _ _ }

  switchBegin                                                           { Token TkSwitch _ _ }
  switchDefault                                                         { Token TkSwitchDefault _ _ }
  switchEnd                                                             { Token TkEndSwitch _ _ }

  forBegin                                                              { Token TkFor _ _ }
  forEnd                                                                { Token TKEndFor _ _ }
  souls                                                                 { Token TkSoul _ _ }
  untilLevel                                                            { Token TkLevel _ _ }

  forEachBegin                                                          { Token TkForEach _ _ }
  forEachEnd                                                            { Token TkEndForEach _ _ }
  withTitaniteFrom                                                      { Token TkWithTitaniteFrom _ _ }

  parensOpen                                                            { Token TkParensOpen _ _ }
  parensClosed                                                          { Token TkParensClosed _ _ }

  plus                                                                  { Token TkPlus _ _ }
  minus                                                                 { Token TkMinus _ _ }
  mult                                                                  { Token TkMult _ _ }
  div                                                                   { Token TkDiv _ _ }
  mod                                                                   { Token TkMod _ _ }
  lt                                                                    { Token TkLt _ _ }
  gt                                                                    { Token TkGt _ _ }
  lte                                                                   { Token TkLte _ _ }
  gte                                                                   { Token TkGte _ _ }
  eq                                                                    { Token TkEq _ _ }
  neq                                                                   { Token TkNeq _ _ }
  not                                                                   { Token TkNot _ _ }
  and                                                                   { Token TkAnd _ _ }
  or                                                                    { Token TkOr _ _ }

  arrOpen                                                               { Token TkArrayOpen _ _ }
  arrClose                                                              { Token TkArrayClose _ _ }

  accessor                                                              { Token TkAccessor _ _ }

%%

PROGRAM
  : programBegin ALIASES METHODS CODEBLOCK programEnd                   { Program $2 $3 $4 }

ALIASES
  : aliasListBegin ALIASL aliasListEnd                                  { $2 }
  | {- empty -}                                                         { [] }

ALIASL
  : ALIASL ALIAS                                                        { $2 : $1 }
  | ALIAS                                                               { [$1] }

ALIAS
  : alias ID TYPE                                                       { Alias $2 $3 }

EXPR
  : intLit                                                              { IntLit $1 }
  | charLit                                                             { CharLit $1 }
  | floatLit                                                            { FloatLit $1 }
  | stringLit                                                           { StringLit $1 }
  | parensOpen EXPR parensClosed                                        { $2 }
  | ID accessor EXPR                                                    { Access $1 $3 }
  | ID arrOpen EXPR arrClose                                            { Access $1 $3 }
  | EXPR plus EXPR                                                      { Add $1 $3 }
  | EXPR minus EXPR                                                     { Substract $1 $3 }
  | EXPR mult EXPR                                                      { Multiply $1 $3 }
  | EXPR div EXPR                                                       { Divide $1 $3 }
  | EXPR mod EXPR                                                       { Mod $1 $3 }
  | minus EXPR %prec UMINUS                                             { Negative $2 }
  | EXPR lt EXPR                                                        { Lt $1 $3 }
  | EXPR gt EXPR                                                        { Gt $1 $3 }
  | EXPR lte EXPR                                                       { Lte $1 $3 }
  | EXPR gte EXPR                                                       { Gte $1 $3 }
  | EXPR eq EXPR                                                        { Eq $1 $3 }
  | EXPR neq EXPR                                                       { Neq $1 $3 }
  | EXPR and EXPR                                                       { And $1 $3 }
  | EXPR or EXPR                                                        { Or $1 $3 }
  | not EXPR                                                            { Not $2 }
  | FUNCALL                                                             { EvalFunc (fst $1) (snd $1) }
  | ID                                                                  { IdExpr $1 }

METHODS
  : {- empty -}                                                         { [] }
  | METHODL                                                             { $1 }

METHODL
  : METHODL METHOD                                                      { $2 : $1 }
  | METHOD                                                              { [$1] }

METHOD
  : FUNC                                                                { $1 }
  | PROC                                                                { $1 }

FUNC
  : functionBegin ID METHODPARS functionType TYPE CODEBLOCK functionEnd { Function $2 $3 $5 $6 }

PROC
  : procedureBegin ID METHODPARS CODEBLOCK procedureEnd                 { Procedure $2 $3 $4 }

METHODPARS
  : paramRequest PARS                                                   { $2 }
  | {- empty -}                                                         { [] }

PARS
  : PARS comma PAR                                                      { $3 : $1 }
  | PAR                                                                 { [$1] }

PAR
  : PARTYPE ID ofType TYPE                                              { MethodDeclaration $1 $2 $4 }

PARTYPE
  : parVal                                                              { Val }
  | parRef                                                              { Ref }

TYPE
  : ID                                                                  { AliasType $1 }
  | bigInt                                                              { BigInt }
  | smallInt                                                            { SmallInt }
  | float                                                               { Float }
  | char                                                                { Char }
  | bool                                                                { Bool }
  | ltelit EXPR array                                                   { Array $2 }
  | ltelit EXPR string                                                  { StringType $2 }
  | set                                                                 { Set }
  | enum brOpen ENUMITS brClose                                         { Enum $3 }
  | unionStruct brOpen STRUCTITS brClose                                { UnionStruct $3 }
  | record  brOpen STRUCTITS brClose                                    { Record $3 }
  | pointer TYPE                                                        { Pointer $2 }

ENUMITS
  : ENUMITS comma ID                                                    { (EnumItem $3) : $1 }
  | ID                                                                  { [EnumItem $1] }

STRUCTITS
  : STRUCTITS comma STRUCTIT                                            { $3 : $1 }
  | STRUCTIT                                                            { [$1] }

STRUCTIT
  : ID ofType TYPE                                                      { StructItem $1 $3 }

ID
  : id                                                                  { Id $1 }

CODEBLOCK
  : instructionsBegin DECLARS INSTRL instructionsEnd                    { CodeBlock $2 $3 }
  | instructionsBegin INSTRL instructionsEnd                            { CodeBlock [] $2 }

DECLARS
  : with DECLARSL declarend                                             { $2 }

DECLARSL
  : DECLARSL comma DECLAR                                               { $3 : $1 }
  | DECLAR                                                              { [$1] }

VARTYPE
  : const                                                               { Const }
  | var                                                                 { Var }

DECLAR
  : VARTYPE ID ofType TYPE                                              { UninitializedDeclaration $1 $2 $4 }
  | VARTYPE ID ofType TYPE asig EXPR                                    { InitializedDeclaration $1 $2 $4 $6 }

INSTRL
  : INSTRL seq INSTR                                                    { $3 : $1 }
  | INSTR                                                               { [$1] }

INSTR
  : ID asig EXPR                                                        { InstAsig $1 $3 }
  | cast ID PROCPARS                                                    { InstCallProc $2 $3 }
  | FUNCALL                                                             { InstCallFunc (fst $1) (snd $1) }
  | return                                                              { InstReturn }
  | returnWith EXPR                                                     { InstReturnWith $2 }
  | print EXPR                                                          { InstPrint $2 }
  | read ID                                                             { InstRead $2 }
  | whileBegin EXPR covenantIsActive CODEBLOCK whileEnd                 { InstWhile $2 $4 }
  | ifBegin IFCASES ELSECASE ifEnd                                      { InstIf (reverse ($3 : $2)) }
  | ifBegin IFCASES ifEnd                                               { InstIf (reverse $2) }
  | switchBegin ID SWITCHCASES DEFAULTCASE switchEnd                    { InstSwitch $2 (reverse ($4 : $3)) }
  | switchBegin ID SWITCHCASES switchEnd                                { InstSwitch $2 (reverse $3) }
  | forBegin ID with EXPR souls untilLevel EXPR CODEBLOCK forEnd        { InstFor $2 $4 $7 $8 }
  | forEachBegin ID withTitaniteFrom ID CODEBLOCK forEachEnd            { InstForEach $2 $4 $5 }

FUNCALL
  : summon ID FUNCPARS                                                  { ($2, $3) }

IFCASES
  : IFCASES IFCASE                                                      { $2 : $1 }
  | IFCASE                                                              { [$1] }

IFCASE
  : EXPR colon CODEBLOCK                                                { GuardedCase $1 $3 }

ELSECASE
  : else colon CODEBLOCK                                                { ElseCase $3 }

SWITCHCASES
  : SWITCHCASES SWITCHCASE                                              { $2 : $1 }
  | SWITCHCASE                                                          { [$1] }

SWITCHCASE
  : ID colon CODEBLOCK                                                  { IdCase $1 $3 }

DEFAULTCASE
  : switchDefault colon CODEBLOCK                                       { DefaultCase $3 }

FUNCPARS
  : granting PARSLIST                                                   { $2 }
  | granting EXPR                                                       { [$2] }
  | {- empty -}                                                         { [] }

PROCPARS
  : offering PARSLIST                                                   { $2 }
  | offering EXPR                                                       { [$2] }
  | {- empty -}                                                         { [] }

PARSLIST
  : EXPR comma PARSLIST                                                 { $1 : $3 }
  | {- empty -}                                                         { [] }

{

parseErrors :: [Token] -> AST a
parseErrors errors =
  let (Token abst _ (AlexPn _ l c)) = errors !! 0
      name = show abst
      line = show l
      column = show c
      header = "\x1b[1m\x1b[31mYOU DIED!!\x1b[0m Parser error: "
      endmsg = "\n\nFix your syntax errors, ashen one."
      position = "line \x1b[1m\x1b[31m" ++ line ++ "\x1b[0m, column \x1b[1m\x1b[31m" ++ column ++ "\x1b[0m."
      msg = header ++ "Unexpected token \x1b[1m\x1b[31m" ++ name ++ "\x1b[0m at " ++ position ++ endmsg
  in  fail msg

type Declarations = [Declaration]
type Instructions = [Instruction]
type MethodDeclarations = [MethodDeclaration]
type Methods = [Method]
type Params = [Expr]
type AliasList = [Alias]
type EnumItems = [EnumItem]
type IfCases = [IfCase]
type SwitchCases = [SwitchCase]
type StructItems = [StructItem]

data Id
  = Id (Maybe String)
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
  | Float
  | Char
  | Bool
  | StringType Expr
  | Array Expr
  | Set
  | Enum EnumItems
  | Record StructItems
  | UnionStruct StructItems
  | Pointer Type
  | AliasType Id
  deriving Show

data EnumItem
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
  = Lit
  | Unlit
  | Undiscovered
  | IntLit (Maybe String)
  | FloatLit (Maybe String)
  | CharLit (Maybe String)
  | StringLit (Maybe String)
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
  | Access Id Expr
  | IdExpr Id
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
  = IdCase Id CodeBlock
  | DefaultCase CodeBlock
  deriving Show

data CodeBlock
  = CodeBlock Declarations Instructions
  deriving Show
}
