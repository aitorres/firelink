{
module Parser (
  parse) where

import Lexer (Token (..), AbstractToken (..), AlexPosn (..))
import AST (AST)
import Data.Maybe
import Grammar
}

%name                                                                     parse
%tokentype                                                              { Token }
%error                                                                  { parseErrors }
%monad                                                                  { AST }

%left comma

%left granting offering

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
  toTheKnight                                                           { Token TkInvocationParsEnd _ _ }

  summon                                                                { Token TkSummon _ _ }
  granting                                                              { Token TkGranting _ _ }
  toTheEstusFlask                                                       { Token TkSpellParsEnd _ _ }

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
  forEnd                                                                { Token TkEndFor _ _ }
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

PROGRAM :: { Program }
PROGRAM
  : programBegin ALIASES METHODS CODEBLOCK programEnd                   { Program $2 $3 $4 }

ALIASES :: { AliasList }
ALIASES
  : aliasListBegin ALIASL aliasListEnd                                  { $2 }
  | {- empty -}                                                         { [] }

ALIASL :: { AliasList }
ALIASL
  : ALIASL ALIAS                                                        { $2 : $1 }
  | ALIAS                                                               { [$1] }

ALIAS :: { Alias }
ALIAS
  : alias ID TYPE                                                       { Alias $2 $3 }

EXPR :: { Expr }
EXPR
  : intLit                                                              { IntLit $ (read (fromJust $1) :: Int) }
  | floatLit                                                            { FloatLit $ (read (fromJust $1) :: Float) }
  | charLit                                                             { CharLit $ fromJust $1 }
  | stringLit                                                           { StringLit $ fromJust $1 }
  | parensOpen EXPR parensClosed                                        { $2 }
  | ID accessor EXPR                                                    { Access $1 $3 }
  | ID arrOpen EXPR arrClose                                            { Access $1 $3 }
  | minus EXPR                                                          { Negative $2 }
  | not EXPR                                                            { Not $2 }
  | EXPR plus EXPR                                                      { Add $1 $3 }
  | EXPR minus EXPR                                                     { Substract $1 $3 }
  | EXPR mult EXPR                                                      { Multiply $1 $3 }
  | EXPR div EXPR                                                       { Divide $1 $3 }
  | EXPR mod EXPR                                                       { Mod $1 $3 }
  | EXPR lt EXPR                                                        { Lt $1 $3 }
  | EXPR gt EXPR                                                        { Gt $1 $3 }
  | EXPR lte EXPR                                                       { Lte $1 $3 }
  | EXPR gte EXPR                                                       { Gte $1 $3 }
  | EXPR eq EXPR                                                        { Eq $1 $3 }
  | EXPR neq EXPR                                                       { Neq $1 $3 }
  | EXPR and EXPR                                                       { And $1 $3 }
  | EXPR or EXPR                                                        { Or $1 $3 }
  | FUNCALL                                                             { EvalFunc (fst $1) (snd $1) }
  | ID                                                                  { IdExpr $1 }

METHODS :: { Methods }
METHODS
  : {- empty -}                                                         { [] }
  | METHODL                                                             { $1 }

METHODL :: { Methods }
METHODL
  : METHODL METHOD                                                      { $2 : $1 }
  | METHOD                                                              { [$1] }

METHOD :: { Method }
METHOD
  : FUNC                                                                { $1 }
  | PROC                                                                { $1 }

FUNC :: { Method }
FUNC
  : functionBegin ID METHODPARS functionType TYPE CODEBLOCK functionEnd { Function $2 $3 $5 $6 }

PROC :: { Method }
PROC
  : procedureBegin ID METHODPARS CODEBLOCK procedureEnd                 { Procedure $2 $3 $4 }

METHODPARS :: { MethodDeclarations }
METHODPARS
  : paramRequest PARS                                                   { $2 }
  | {- empty -}                                                         { [] }

PARS :: { MethodDeclarations }
PARS
  : PARS comma PAR                                                      { $3 : $1 }
  | PAR                                                                 { [$1] }

PAR :: { MethodDeclaration }
PAR
  : PARTYPE ID ofType TYPE                                              { MethodDeclaration $1 $2 $4 }

PARTYPE :: { ParamType }
PARTYPE
  : parVal                                                              { Val }
  | parRef                                                              { Ref }

TYPE :: { Type }
TYPE
  : ID                                                                  { AliasType $1 }
  | bigInt                                                              { BigInt }
  | smallInt                                                            { SmallInt }
  | float                                                               { Float }
  | char                                                                { Char }
  | bool                                                                { Bool }
  | ltelit EXPR array ofType TYPE                                       { Array $5 $2 }
  | ltelit EXPR string                                                  { StringType $2 }
  | set ofType TYPE                                                     { Set $3 }
  | enum brOpen ENUMITS brClose                                         { Enum $ reverse $3 }
  | unionStruct brOpen STRUCTITS brClose                                { UnionStruct $3 }
  | record  brOpen STRUCTITS brClose                                    { Record $ reverse $3 }
  | pointer TYPE                                                        { Pointer $2 }

ENUMITS :: { EnumItems }
ENUMITS
  : ENUMITS comma ID                                                    { (EnumItem $3) : $1 }
  | ID                                                                  { [EnumItem $1] }

STRUCTITS :: { StructItems }
STRUCTITS
  : STRUCTITS comma STRUCTIT                                            { $3 : $1 }
  | STRUCTIT                                                            { [$1] }

STRUCTIT :: { StructItem }
STRUCTIT
  : ID ofType TYPE                                                      { StructItem $1 $3 }

ID :: { Id }
ID
  : id                                                                  { Id $ fromJust $1 }

CODEBLOCK :: { CodeBlock }
CODEBLOCK
  : instructionsBegin DECLARS INSTRL instructionsEnd                    { CodeBlock $2 $3 }
  | instructionsBegin INSTRL instructionsEnd                            { CodeBlock [] $2 }

DECLARS :: { Declarations }
DECLARS
  : with DECLARSL declarend                                             { $2 }

DECLARSL :: { Declarations }
DECLARSL
  : DECLARSL comma DECLAR                                               { $3 : $1 }
  | DECLAR                                                              { [$1] }

VARTYPE :: { VarType }
VARTYPE
  : const                                                               { Const }
  | var                                                                 { Var }

DECLAR :: { Declaration }
DECLAR
  : VARTYPE ID ofType TYPE                                              { UninitializedDeclaration $1 $2 $4 }
  | VARTYPE ID ofType TYPE asig EXPR                                    { InitializedDeclaration $1 $2 $4 $6 }

INSTRL :: { Instructions }
INSTRL
  : INSTRL seq INSTR                                                    { $3 : $1 }
  | INSTR                                                               { [$1] }

INSTR :: { Instruction }
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

FUNCALL :: { (Id, Params) }
FUNCALL
  : summon ID FUNCPARS                                                  { ($2, $3) }

IFCASES :: { IfCases }
IFCASES
  : IFCASES IFCASE                                                      { $2 : $1 }
  | IFCASE                                                              { [$1] }

IFCASE :: { IfCase }
  : EXPR colon CODEBLOCK                                                { GuardedCase $1 $3 }

ELSECASE :: { IfCase }
  : else colon CODEBLOCK                                                { ElseCase $3 }

SWITCHCASES :: { SwitchCases }
  : SWITCHCASES SWITCHCASE                                              { $2 : $1 }
  | SWITCHCASE                                                          { [$1] }

SWITCHCASE :: { SwitchCase }
  : ID colon CODEBLOCK                                                  { IdCase $1 $3 }

DEFAULTCASE :: { SwitchCase }
  : switchDefault colon CODEBLOCK                                       { DefaultCase $3 }

FUNCPARS :: { Params }
  : granting PARSLIST toTheKnight                                       { $2 }
  | {- empty -}                                                         { [] }

PROCPARS :: { Params }
  : offering PARSLIST toTheEstusFlask                                   { $2 }
  | {- empty -}                                                         { [] }

PARSLIST :: { Params }
  : PARSLIST comma EXPR                                                 { $3 : $1 }
  | EXPR                                                                { [$1] }

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

}
