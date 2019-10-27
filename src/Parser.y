{
module Parser (
  parse) where

import Lexer (Token (..), AbstractToken (..), AlexPosn (..))
import qualified SymTable as ST
import Data.Maybe
import qualified Grammar as G
import qualified Control.Monad.RWS as RWS
}

%name                                                                     parse
%tokentype                                                              { Token }
%error                                                                  { parseErrors }
%monad                                                                  { ST.ParserMonad }

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

  id                                                                    { Token TkId _ _ }

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

PROGRAM :: { G.Program }
PROGRAM
  : programBegin ALIASES METHODS CODEBLOCK programEnd                   { G.Program $4 }

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

EXPR :: { G.Expr }
EXPR
  : intLit                                                              { G.IntLit $ (read (fromJust $1) :: Int) }
  | floatLit                                                            { G.FloatLit $ (read (fromJust $1) :: Float) }
  | charLit                                                             { G.CharLit $ head $ fromJust $1 }
  | stringLit                                                           { G.StringLit $ fromJust $1 }
  | trueLit                                                             { G.TrueLit }
  | falseLit                                                            { G.FalseLit }
  | nullLit                                                             { G.NullLit }
  | arrOpen EXPRL arrClose                                              { G.ArrayLit $ reverse $2 }
  | setOpen EXPRL setClose                                              { G.SetLit $ reverse $2 }
  | unknownLit                                                          { G.UndiscoveredLit }
  | parensOpen EXPR parensClosed                                        { $2 }
  | EXPR accessor ID                                                    { G.Access $1 $3 }
  | EXPR arrOpen EXPR arrClose                                          { G.IndexAccess $1 $3 }
  | memAccessor EXPR                                                    { G.MemAccess $2 }
  | minus EXPR                                                          { G.Negative $2 }
  | not EXPR                                                            { G.Not $2 }
  | asciiOf EXPR                                                        { G.AsciiOf $2 }
  | size EXPR                                                           { G.SetSize $2 }
  | EXPR plus EXPR                                                      { G.Add $1 $3 }
  | EXPR minus EXPR                                                     { G.Substract $1 $3 }
  | EXPR mult EXPR                                                      { G.Multiply $1 $3 }
  | EXPR div EXPR                                                       { G.Divide $1 $3 }
  | EXPR mod EXPR                                                       { G.Mod $1 $3 }
  | EXPR lt EXPR                                                        { G.Lt $1 $3 }
  | EXPR gt EXPR                                                        { G.Gt $1 $3 }
  | EXPR lte EXPR                                                       { G.Lte $1 $3 }
  | EXPR gte EXPR                                                       { G.Gte $1 $3 }
  | EXPR eq EXPR                                                        { G.Eq $1 $3 }
  | EXPR neq EXPR                                                       { G.Neq $1 $3 }
  | EXPR and EXPR                                                       { G.And $1 $3 }
  | EXPR or EXPR                                                        { G.Or $1 $3 }
  | EXPR colConcat EXPR                                                 { G.ColConcat $1 $3 }
  | EXPR union EXPR                                                     { G.SetUnion $1 $3 }
  | EXPR intersect EXPR                                                 { G.SetIntersect $1 $3 }
  | EXPR diff EXPR                                                      { G.SetDiff $1 $3 }
  | FUNCALL                                                             { G.EvalFunc (fst $1) (snd $1) }
  | ID                                                                  { G.IdExpr $1 }

EXPRL :: { G.Exprs }
EXPRL
  : {- empty -}                                                         { [] }
  | EXPRLNOTEMPTY                                                       { $1 }

EXPRLNOTEMPTY :: { G.Exprs }
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

TYPE :: { G.Type }
TYPE
  : ID                                                                  { let G.Id t = $1 in G.Simple t Nothing }
  | bigInt                                                              { G.Simple $1 Nothing }
  | smallInt                                                            { G.Simple $1 Nothing }
  | float                                                               { G.Simple $1 Nothing }
  | char                                                                { G.Simple $1 Nothing }
  | bool                                                                { G.Simple $1 Nothing }
  | ltelit EXPR array ofType TYPE                                       { G.Compound $3 $5 (Just $2) }
  | ltelit EXPR string                                                  { G.Simple $3 (Just $2) }
  | set ofType TYPE                                                     { G.Compound $1 $3 Nothing }
  | pointer TYPE                                                        { G.Compound $1 $2 Nothing }

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

ID :: { G.Id }
ID
  : id                                                                  { G.Id $1 }

CODEBLOCK :: { G.CodeBlock }
CODEBLOCK
  : instructionsBegin DECLARS INSTRL instructionsEnd                    { G.CodeBlock $3 }
  | instructionsBegin INSTRL instructionsEnd                            { G.CodeBlock $2 }

DECLARS :: { [Declaration] }
DECLARS
  : with DECLARSL declarend                                             { reverse $2 }

DECLARSL :: { [Declaration] }
DECLARSL
  : DECLARSL comma DECLAR                                               { $3:$1 }
  | DECLAR                                                              { [$1] }

VARTYPE :: { ST.Category }
VARTYPE
  : const                                                               { ST.Constant }
  | var                                                                 { ST.Variable }

DECLAR :: { Declaration }
DECLAR
  : VARTYPE ID ofType TYPE                                              { ($1, $2, $4, Nothing) }
  | VARTYPE ID ofType TYPE asig EXPR                                    { ($1, $2, $4, Just $6) }

INSTRL :: { G.Instructions }
INSTRL
  : INSTRL seq INSTR                                                    { $3 : $1 }
  | INSTR                                                               { [$1] }

INSTR :: { G.Instruction }
INSTR
  : ID asig EXPR                                                        { G.InstAsig $1 $3 }
  | cast ID PROCPARS                                                    { G.InstCallProc $2 $3 }
  | FUNCALL                                                             { G.InstCallFunc (fst $1) (snd $1) }
  | return                                                              { G.InstReturn }
  | returnWith EXPR                                                     { G.InstReturnWith $2 }
  | print EXPR                                                          { G.InstPrint $2 }
  | read ID                                                             { G.InstRead $2 }
  | whileBegin EXPR covenantIsActive colon CODEBLOCK whileEnd           { G.InstWhile $2 $5 }
  | ifBegin IFCASES ELSECASE ifEnd                                      { G.InstIf (reverse ($3 : $2)) }
  | ifBegin IFCASES ifEnd                                               { G.InstIf (reverse $2) }
  | switchBegin ID SWITCHCASES DEFAULTCASE switchEnd                    { G.InstSwitch $2 (reverse ($4 : $3)) }
  | switchBegin ID SWITCHCASES switchEnd                                { G.InstSwitch $2 (reverse $3) }
  | forBegin ID with EXPR souls untilLevel EXPR CODEBLOCK forEnd        { G.InstFor $2 $4 $7 $8 }
  | forEachBegin ID withTitaniteFrom ID CODEBLOCK forEachEnd            { G.InstForEach $2 $4 $5 }

FUNCALL :: { (G.Id, G.Params) }
FUNCALL
  : summon ID FUNCPARS                                                  { ($2, $3) }

IFCASES :: { G.IfCases }
IFCASES
  : IFCASES IFCASE                                                      { $2 : $1 }
  | IFCASE                                                              { [$1] }

IFCASE :: { G.IfCase }
  : EXPR colon CODEBLOCK                                                { G.GuardedCase $1 $3 }

ELSECASE :: { G.IfCase }
  : else colon CODEBLOCK                                                { G.ElseCase $3 }

SWITCHCASES :: { G.SwitchCases }
  : SWITCHCASES SWITCHCASE                                              { $2 : $1 }
  | SWITCHCASE                                                          { [$1] }

SWITCHCASE :: { G.SwitchCase }
  : EXPR colon CODEBLOCK                                                { G.Case $1 $3 }

DEFAULTCASE :: { G.SwitchCase }
  : switchDefault colon CODEBLOCK                                       { G.DefaultCase $3 }

FUNCPARS :: { G.Params }
  : granting PARSLIST toTheKnight                                       { reverse $2 }
  | {- empty -}                                                         { [] }

PROCPARS :: { G.Params }
  : offering PARSLIST toTheEstusFlask                                   { reverse $2 }
  | {- empty -}                                                         { [] }

PARSLIST :: { G.Params }
  : PARSLIST comma EXPR                                                 { $3 : $1 }
  | EXPR                                                                { [$1] }

{

type Declaration = (ST.Category, G.Id, G.Type, Maybe G.Expr)

parseErrors :: [Token] -> ST.ParserMonad a
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
