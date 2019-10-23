{
module Parser (
  parse) where

import Lexer (Token (..), AbstractToken (..), AlexPosn (..))
import SymTable (ParserState)
import Data.Maybe
import Grammar
}

%name                                                                     parse
%tokentype                                                              { Token }
%error                                                                  { parseErrors }
%monad                                                                  { AST }

%nonassoc lt lte gt gte size memAccessor
%right arrOpen arrClose

%left accessor
%left eq neq
%left plus minus
%left mult div mod

%left and or

%left colConcat
%left diff
%left union intersect


%left not asciiOf


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
  trueLit                                                               { Token TkLit _ _ }
  falseLit                                                              { Token TkUnlit _ _ }
  unknownLit                                                            { Token TkUndiscovered _ _ }
  nullLit                                                               { Token TkNull _ _ }

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
  asciiOf                                                               { Token TkAsciiOf _ _ }
  colConcat                                                             { Token TkConcat _ _ }
  union                                                                 { Token TkUnion _ _ }
  intersect                                                             { Token TkIntersect _ _ }
  diff                                                                  { Token TkDiff _ _ }
  size                                                                  { Token TkSize _ _ }

  arrOpen                                                               { Token TkArrayOpen _ _ }
  arrClose                                                              { Token TkArrayClose _ _ }
  setOpen                                                               { Token TkSetOpen _ _ }
  setClose                                                              { Token TkSetClose _ _ }

  accessor                                                              { Token TkAccessor _ _ }
  memAccessor                                                           { Token TkAccessMemory _ _ }

%%

PROGRAM :: { Program }
PROGRAM
  : programBegin ALIASES METHODS CODEBLOCK programEnd                   { Program $4 }

ALIASES :: { [Int] }
ALIASES
  : aliasListBegin ALIASL aliasListEnd                                  { [] }
  | {- empty -}                                                         { [] }

ALIASL :: { [Int] }
ALIASL
  : ALIASL comma ALIAS                                                  { [] }
  | ALIAS                                                               { [] }

ALIAS :: { [Int] }
ALIAS
  : alias ID TYPE                                                       { [] }

EXPR :: { Expr }
EXPR
  : intLit                                                              { IntLit $ (read (fromJust $1) :: Int) }
  | floatLit                                                            { FloatLit $ (read (fromJust $1) :: Float) }
  | charLit                                                             { CharLit $ head $ fromJust $1 }
  | stringLit                                                           { StringLit $ fromJust $1 }
  | trueLit                                                             { TrueLit }
  | falseLit                                                            { FalseLit }
  | nullLit                                                             { NullLit }
  | arrOpen EXPRL arrClose                                              { ArrayLit $ reverse $2 }
  | setOpen EXPRL setClose                                              { SetLit $ reverse $2 }
  | unknownLit                                                          { UndiscoveredLit }
  | parensOpen EXPR parensClosed                                        { $2 }
  | EXPR accessor ID                                                    { Access $1 $3 }
  | EXPR arrOpen EXPR arrClose                                          { IndexAccess $1 $3 }
  | memAccessor EXPR                                                    { MemAccess $2 }
  | minus EXPR                                                          { Negative $2 }
  | not EXPR                                                            { Not $2 }
  | asciiOf EXPR                                                        { AsciiOf $2 }
  | size EXPR                                                           { SetSize $2 }
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
  | EXPR colConcat EXPR                                                 { ColConcat $1 $3 }
  | EXPR union EXPR                                                     { SetUnion $1 $3 }
  | EXPR intersect EXPR                                                 { SetIntersect $1 $3 }
  | EXPR diff EXPR                                                      { SetDiff $1 $3 }
  | FUNCALL                                                             { EvalFunc (fst $1) (snd $1) }
  | ID                                                                  { IdExpr $1 }

EXPRL :: { Exprs }
EXPRL
  : {- empty -}                                                         { [] }
  | EXPRLNOTEMPTY                                                       { $1 }

EXPRLNOTEMPTY :: { Exprs }
EXPRLNOTEMPTY
  : EXPR                                                                { [$1] }
  | EXPRLNOTEMPTY comma EXPR                                           { $3:$1 }

METHODS :: { [Int] }
METHODS
  : {- empty -}                                                         { [] }
  | METHODL                                                             { [] }

METHODL :: { [Int] }
METHODL
  : METHODL METHOD                                                      { [] }
  | METHOD                                                              { [] }

METHOD :: { [Int] }
METHOD
  : FUNC                                                                { [] }
  | PROC                                                                { [] }

FUNC :: { [Int] }
FUNC
  : functionBegin ID METHODPARS functionType TYPE CODEBLOCK functionEnd { [] }

PROC :: { [Int] }
PROC
  : procedureBegin ID METHODPARS CODEBLOCK procedureEnd                 { [] }

METHODPARS :: { [Int] }
METHODPARS
  : paramRequest PARS                                                   { [] }
  | {- empty -}                                                         { [] }

PARS :: { [Int] }
PARS
  : PARS comma PAR                                                      { [] }
  | PAR                                                                 { [] }

PAR :: { [Int] }
PAR
  : PARTYPE ID ofType TYPE                                              { [] }

PARTYPE :: { [Int] }
PARTYPE
  : parVal                                                              { [] }
  | parRef                                                              { [] }

TYPE :: { [Int] }
TYPE
  : ID                                                                  { [] }
  | bigInt                                                              { [] }
  | smallInt                                                            { [] }
  | float                                                               { [] }
  | char                                                                { [] }
  | bool                                                                { [] }
  | ltelit EXPR array ofType TYPE                                       { [] }
  | ltelit EXPR string                                                  { [] }
  | set ofType TYPE                                                     { [] }
  | enum brOpen ENUMITS brClose                                         { [] }
  | unionStruct brOpen STRUCTITS brClose                                { [] }
  | record  brOpen STRUCTITS brClose                                    { [] }
  | pointer TYPE                                                        { [] }

ENUMITS :: { [Int] }
ENUMITS
  : ENUMITS comma ID                                                    { [] }
  | ID                                                                  { [] }

STRUCTITS :: { [Int] }
STRUCTITS
  : STRUCTITS comma STRUCTIT                                            { [] }
  | STRUCTIT                                                            { [] }

STRUCTIT :: { [Int] }
STRUCTIT
  : ID ofType TYPE                                                      { [] }

ID :: { Id }
ID
  : id                                                                  { Id $ fromJust $1 }

CODEBLOCK :: { CodeBlock }
CODEBLOCK
  : instructionsBegin DECLARS INSTRL instructionsEnd                    { CodeBlock $ reverse $3 }
  | instructionsBegin INSTRL instructionsEnd                            { CodeBlock $ reverse $2 }

DECLARS :: { [Int] }
DECLARS
  : with DECLARSL declarend                                             { [] }

DECLARSL :: { [Int] }
DECLARSL
  : DECLARSL comma DECLAR                                               { [] }
  | DECLAR                                                              { [] }

VARTYPE :: { [Int] }
VARTYPE
  : const                                                               { [] }
  | var                                                                 { [] }

DECLAR :: { [Int] }
DECLAR
  : VARTYPE ID ofType TYPE                                              { [] }
  | VARTYPE ID ofType TYPE asig EXPR                                    { [] }

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
  | whileBegin EXPR covenantIsActive colon CODEBLOCK whileEnd           { InstWhile $2 $5 }
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
  : EXPR colon CODEBLOCK                                                { Case $1 $3 }

DEFAULTCASE :: { SwitchCase }
  : switchDefault colon CODEBLOCK                                       { DefaultCase $3 }

FUNCPARS :: { Params }
  : granting PARSLIST toTheKnight                                       { reverse $2 }
  | {- empty -}                                                         { [] }

PROCPARS :: { Params }
  : offering PARSLIST toTheEstusFlask                                   { reverse $2 }
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
