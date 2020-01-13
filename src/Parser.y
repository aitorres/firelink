{
module Parser (
  parse) where

import qualified Tokens as T
import qualified SymTable as ST
import Data.Maybe
import qualified Grammar as G
import qualified Control.Monad.RWS as RWS
import qualified TypeChecking as T
import Utils
}

%name                                                                     parse
%tokentype                                                              { T.Token }
%error                                                                  { parseErrors }
%monad                                                                  { ST.ParserMonad }


%nonassoc size memAccessor

%left ARRCLOSE

%left eq neq
%nonassoc lt lte gt gte
%left plus minus
%left mult div mod
%left NEG

%left and or


%left colConcat
%left diff
%left union intersect


%left asciiOf
%right not
%left accessor
%left arrOpen arrClose


%token
  programBegin                                                          { T.Token {T.aToken=T.TkProgramBegin} }
  programEnd                                                            { T.Token {T.aToken=T.TkProgramEnd} }

  aliasListBegin                                                        { T.Token {T.aToken=T.TkAliasListBegin} }
  aliasListEnd                                                          { T.Token {T.aToken=T.TkAliasListEnd} }
  alias                                                                 { T.Token {T.aToken=T.TkAlias} }

  id                                                                    { T.Token {T.aToken=T.TkId} }

  ofType                                                                { T.Token {T.aToken=T.TkOfType} }

  paramRequest                                                          { T.Token {T.aToken=T.TkRequesting} }
  parVal                                                                { T.Token {T.aToken=T.TkVal} }
  parRef                                                                { T.Token {T.aToken=T.TkRef} }

  bigInt                                                                { T.Token {T.aToken=T.TkBigInt} }
  smallInt                                                              { T.Token {T.aToken=T.TkSmallInt} }
  float                                                                 { T.Token {T.aToken=T.TkFloat} }
  char                                                                  { T.Token {T.aToken=T.TkChar} }
  bool                                                                  { T.Token {T.aToken=T.TkBool} }
  ltelit                                                                { T.Token {T.aToken=T.TkLteLit} }
  string                                                                { T.Token {T.aToken=T.TkString} }
  array                                                                 { T.Token {T.aToken=T.TkArray} }
  set                                                                   { T.Token {T.aToken=T.TkSet} }
  enum                                                                  { T.Token {T.aToken=T.TkEnum} }
  unionStruct                                                           { T.Token {T.aToken=T.TkUnionStruct} }
  record                                                                { T.Token {T.aToken=T.TkRecord} }
  pointer                                                               { T.Token {T.aToken=T.TkPointer} }

  intLit                                                                { T.Token {T.aToken=T.TkIntLit} }
  floatLit                                                              { T.Token {T.aToken=T.TkFloatLit} }
  charLit                                                               { T.Token {T.aToken=T.TkCharLit} }
  stringLit                                                             { T.Token {T.aToken=T.TkStringLit} }
  trueLit                                                               { T.Token {T.aToken=T.TkLit} }
  falseLit                                                              { T.Token {T.aToken=T.TkUnlit} }
  unknownLit                                                            { T.Token {T.aToken=T.TkUndiscovered} }
  nullLit                                                               { T.Token {T.aToken=T.TkNull} }

  functionBegin                                                         { T.Token {T.aToken=T.TkInvocation} }
  functionType                                                          { T.Token {T.aToken=T.TkInvocationType} }
  functionEnd                                                           { T.Token {T.aToken=T.TkInvocationEnd} }

  procedureBegin                                                        { T.Token {T.aToken=T.TkSpell} }
  procedureEnd                                                          { T.Token {T.aToken=T.TkSpellEnd} }

  comma                                                                 { T.Token {T.aToken=T.TkComma} }
  brOpen                                                                { T.Token {T.aToken=T.TkBraceOpen} }
  brClose                                                               { T.Token {T.aToken=T.TkBraceClosed} }

  with                                                                  { T.Token {T.aToken=T.TkWith} }
  declarend                                                             { T.Token {T.aToken=T.TkDeclarationEnd} }

  const                                                                 { T.Token {T.aToken=T.TkConst} }
  var                                                                   { T.Token {T.aToken=T.TkVar} }
  asig                                                                  { T.Token {T.aToken=T.TkAsig} }

  instructionsBegin                                                     { T.Token {T.aToken=T.TkInstructionBegin} }
  instructionsEnd                                                       { T.Token {T.aToken=T.TkInstructionEnd} }
  seq                                                                   { T.Token {T.aToken=T.TkSeq} }

  cast                                                                  { T.Token {T.aToken=T.TkCast} }
  offering                                                              { T.Token {T.aToken=T.TkOffering} }
  toTheKnight                                                           { T.Token {T.aToken=T.TkInvocationParsEnd} }

  summon                                                                { T.Token {T.aToken=T.TkSummon} }
  granting                                                              { T.Token {T.aToken=T.TkGranting} }
  toTheEstusFlask                                                       { T.Token {T.aToken=T.TkSpellParsEnd} }

  return                                                                { T.Token {T.aToken=T.TkReturn} }
  returnWith                                                            { T.Token {T.aToken=T.TkReturnWith} }

  print                                                                 { T.Token {T.aToken=T.TkPrint} }
  read                                                                  { T.Token {T.aToken=T.TkRead} }

  whileBegin                                                            { T.Token {T.aToken=T.TkWhile} }
  whileEnd                                                              { T.Token {T.aToken=T.TkEndWhile} }
  covenantIsActive                                                      { T.Token {T.aToken=T.TkCovenantIsActive} }

  ifBegin                                                               { T.Token {T.aToken=T.TkIf} }
  ifEnd                                                                 { T.Token {T.aToken=T.TkEndIf} }
  colon                                                                 { T.Token {T.aToken=T.TkColon} }
  else                                                                  { T.Token {T.aToken=T.TkElse} }

  switchBegin                                                           { T.Token {T.aToken=T.TkSwitch} }
  switchDefault                                                         { T.Token {T.aToken=T.TkSwitchDefault} }
  switchEnd                                                             { T.Token {T.aToken=T.TkEndSwitch} }

  forBegin                                                              { T.Token {T.aToken=T.TkFor} }
  forEnd                                                                { T.Token {T.aToken=T.TkEndFor} }
  souls                                                                 { T.Token {T.aToken=T.TkSoul} }
  untilLevel                                                            { T.Token {T.aToken=T.TkLevel} }

  forEachBegin                                                          { T.Token {T.aToken=T.TkForEach} }
  forEachEnd                                                            { T.Token {T.aToken=T.TkEndForEach} }
  withTitaniteFrom                                                      { T.Token {T.aToken=T.TkWithTitaniteFrom} }

  parensOpen                                                            { T.Token {T.aToken=T.TkParensOpen} }
  parensClosed                                                          { T.Token {T.aToken=T.TkParensClosed} }

  plus                                                                  { T.Token {T.aToken=T.TkPlus} }
  minus                                                                 { T.Token {T.aToken=T.TkMinus} }
  mult                                                                  { T.Token {T.aToken=T.TkMult} }
  div                                                                   { T.Token {T.aToken=T.TkDiv} }
  mod                                                                   { T.Token {T.aToken=T.TkMod} }
  lt                                                                    { T.Token {T.aToken=T.TkLt} }
  gt                                                                    { T.Token {T.aToken=T.TkGt} }
  lte                                                                   { T.Token {T.aToken=T.TkLte} }
  gte                                                                   { T.Token {T.aToken=T.TkGte} }
  eq                                                                    { T.Token {T.aToken=T.TkEq} }
  neq                                                                   { T.Token {T.aToken=T.TkNeq} }
  not                                                                   { T.Token {T.aToken=T.TkNot} }
  and                                                                   { T.Token {T.aToken=T.TkAnd} }
  or                                                                    { T.Token {T.aToken=T.TkOr} }
  asciiOf                                                               { T.Token {T.aToken=T.TkAsciiOf} }
  colConcat                                                             { T.Token {T.aToken=T.TkConcat} }
  union                                                                 { T.Token {T.aToken=T.TkUnion} }
  intersect                                                             { T.Token {T.aToken=T.TkIntersect} }
  diff                                                                  { T.Token {T.aToken=T.TkDiff} }
  size                                                                  { T.Token {T.aToken=T.TkSize} }

  arrOpen                                                               { T.Token {T.aToken=T.TkArrayOpen} }
  arrClose                                                              { T.Token {T.aToken=T.TkArrayClose} }
  setOpen                                                               { T.Token {T.aToken=T.TkSetOpen} }
  setClose                                                              { T.Token {T.aToken=T.TkSetClose} }

  accessor                                                              { T.Token {T.aToken=T.TkAccessor} }
  memAccessor                                                           { T.Token {T.aToken=T.TkAccessMemory} }

  malloc                                                                { T.Token {T.aToken=T.TkRequestMemory} }
  free                                                                  { T.Token {T.aToken=T.TkFreeMemory} }
%%

PROGRAM :: { G.Program }
  : programBegin ALIASES METHODS NON_OPENER_CODEBLOCK PROGRAMEND        {% do
                                                                             checkRecoverableError $1 $5
                                                                             return $ G.Program $4 }

PROGRAMEND :: { Maybe G.RecoverableError }
  : programEnd                                                          { Nothing }
  | error                                                               { Just G.MissingProgramEnd }

NON_OPENER_CODEBLOCK :: { G.CodeBlock }
  : instructionsBegin DECLARS INSTRL NON_OPENER_INSTEND                 {% do
                                                                             checkRecoverableError $1 $4
                                                                             return $ G.CodeBlock $ reverse $3 }
  | instructionsBegin INSTRL NON_OPENER_INSTEND                         {% do
                                                                             checkRecoverableError $1 $3
                                                                             return $ G.CodeBlock $ reverse $2 }

NON_OPENER_INSTEND :: { Maybe G.RecoverableError }
  : instructionsEnd                                                     { Nothing }
  | error                                                               { Just G.MissingInstructionListEnd }

ALIASES :: { () }
  : aliasListBegin ALIASL ALIASLISTEND                                  {% do
                                                                            checkRecoverableError $1 $3
                                                                            addIdsToSymTable $ reverse $2
                                                                            (dict, _, s) <- RWS.get
                                                                            RWS.put (dict, [1, 0], s) }
  | {- empty -}                                                         { () }


ALIASLISTEND :: { Maybe G.RecoverableError }
 : aliasListEnd                                                         { Nothing }
 | error                                                                { Just G.MissingAliasListEnd }

ALIASL :: { [NameDeclaration] }
  : ALIASL comma ALIAS                                                  { $3:$1 }
  | ALIAS                                                               { [$1] }

ALIAS :: { NameDeclaration }
  : alias ID TYPE                                                       { (ST.Type, $2, $3, Nothing) }

LVALUE :: { G.Expr }
  : ID                                                                  {% do
                                                                            let (G.Id tk) = $1
                                                                            buildAndCheckExpr tk $ G.IdExpr $1 }
  | EXPR accessor ID                                                    {% do
                                                                            let expr = G.Access $1 $3
                                                                            buildAndCheckExpr $2 expr }
  | EXPR arrOpen EXPR arrClose                                          {% buildAndCheckExpr $2 $ G.IndexAccess $1 $3 }
  | memAccessor EXPR                                                    {% buildAndCheckExpr $1 $ G.MemAccess $2 }

EXPR :: { G.Expr }
  : intLit                                                              {% buildAndCheckExpr $1 $ G.IntLit (read (T.cleanedString $1) :: Int) }
  | floatLit                                                            {% buildAndCheckExpr $1 $ G.FloatLit (read (T.cleanedString $1) :: Float) }
  | charLit                                                             {% buildAndCheckExpr $1 $ G.CharLit $ head (T.cleanedString $1) }
  | stringLit                                                           {% buildAndCheckExpr $1 $ G.StringLit (T.cleanedString $1) }
  | trueLit                                                             {% buildAndCheckExpr $1 G.TrueLit }
  | falseLit                                                            {% buildAndCheckExpr $1 G.FalseLit }
  | unknownLit                                                          {% buildAndCheckExpr $1 G.UndiscoveredLit }
  | nullLit                                                             {% buildAndCheckExpr $1 G.NullLit }
  | arrOpen EXPRL arrClose                                              {% buildAndCheckExpr $1 $ G.ArrayLit $ reverse $2 }
  | setOpen EXPRL setClose                                              {% buildAndCheckExpr $1 $ G.SetLit $ reverse $2 }
  | parensOpen EXPR parensClosed                                        { $2{G.expTok=$1} }
  | minus EXPR                                                          {% buildAndCheckExpr $1 $ G.Op1 G.Negate $2 }
  | not EXPR                                                            {% buildAndCheckExpr $1 $ G.Op1 G.Not $2 }
  | asciiOf EXPR                                                        {% buildAndCheckExpr $1 $ G.AsciiOf $2 }
  | size EXPR                                                           {% buildAndCheckExpr $1 $ G.SetSize $2 }
  | EXPR plus EXPR                                                      {% buildAndCheckExpr $2 $ G.Op2 G.Add $1 $3 }
  | EXPR minus EXPR                                                     {% buildAndCheckExpr $2 $ G.Op2 G.Substract $1 $3 }
  | EXPR mult EXPR                                                      {% buildAndCheckExpr $2 $ G.Op2 G.Multiply $1 $3 }
  | EXPR div EXPR                                                       {% buildAndCheckExpr $2 $ G.Op2 G.Divide $1 $3 }
  | EXPR mod EXPR                                                       {% buildAndCheckExpr $2 $ G.Op2 G.Mod $1 $3 }
  | EXPR lt EXPR                                                        {% buildAndCheckExpr $2 $ G.Op2 G.Lt $1 $3 }
  | EXPR gt EXPR                                                        {% buildAndCheckExpr $2 $ G.Op2 G.Gt $1 $3 }
  | EXPR lte EXPR                                                       {% buildAndCheckExpr $2 $ G.Op2 G.Lte $1 $3 }
  | EXPR gte EXPR                                                       {% buildAndCheckExpr $2 $ G.Op2 G.Gte $1 $3 }
  | EXPR eq EXPR                                                        {% buildAndCheckExpr $2 $ G.Op2 G.Eq $1 $3 }
  | EXPR neq EXPR                                                       {% buildAndCheckExpr $2 $ G.Op2 G.Neq $1 $3 }
  | EXPR and EXPR                                                       {% buildAndCheckExpr $2 $ G.Op2 G.And $1 $3 }
  | EXPR or EXPR                                                        {% buildAndCheckExpr $2 $ G.Op2 G.Or $1 $3 }
  | EXPR colConcat EXPR                                                 {% buildAndCheckExpr $2 $ G.Op2 G.ColConcat $1 $3 }
  | EXPR union EXPR                                                     {% buildAndCheckExpr $2 $ G.Op2 G.SetUnion $1 $3 }
  | EXPR intersect EXPR                                                 {% buildAndCheckExpr $2 $ G.Op2 G.SetIntersect $1 $3 }
  | EXPR diff EXPR                                                      {% buildAndCheckExpr $2 $ G.Op2 G.SetDifference $1 $3 }
  | FUNCALL                                                             {% let (tk, i, params) = $1 in buildAndCheckExpr tk $ G.EvalFunc i params }
  | LVALUE                                                              { $1 }

EXPRL :: { [G.Expr] }
  : {- empty -}                                                         { [] }
  | NONEMPTYEXPRL                                                       { $1 }

NONEMPTYEXPRL :: { [G.Expr] }
  : EXPR                                                                { [$1] }
  | NONEMPTYEXPRL comma EXPR                                            { $3:$1 }

METHODS :: { () }
  : {- empty -}                                                         { () }
  | METHODL                                                             { () }

METHODL :: { () }
  : METHODL METHOD                                                      { () }
  | METHOD                                                              { () }

METHOD :: { () }
  : FUNC                                                                {% do
                                                                          (dict, _, s) <- RWS.get
                                                                          RWS.put (dict, [1, 0], s) }
  | PROC                                                                {% do
                                                                          (dict, _, s) <- RWS.get
                                                                          RWS.put (dict, [1, 0], s) }

FUNCPREFIX :: { Maybe (ST.Scope, G.Id) }
  : functionBegin ID METHODPARS functionType TYPE                       {% addFunction (ST.Function,
                                                                              $2, G.Callable (Just $5) $3, Nothing) }
FUNC :: { () }
  : FUNCPREFIX CODEBLOCK functionEnd                                    {% case $1 of
                                                                            Nothing -> return ()
                                                                            Just (s, i) -> updateCodeBlockOfFun s i $2 }

PROCPREFIX :: { Maybe (ST.Scope, G.Id) }
  : procedureBegin ID PROCPARSDEC                                       {% addFunction (ST.Procedure,
                                                                              $2, G.Callable Nothing $3, Nothing) }
PROC :: { () }
  : PROCPREFIX CODEBLOCK procedureEnd                                   {% case $1 of
                                                                            Nothing -> return ()
                                                                            Just (s, i) -> updateCodeBlockOfFun s i $2 }

PROCPARSDEC :: { [ArgDeclaration] }
  : METHODPARS toTheEstusFlask                                          { $1 }
  | {- empty -}                                                         { [] }

METHODPARS :: { [ArgDeclaration] }
  : paramRequest PARS                                                   { reverse $2 }
  | {- empty -}                                                         { [] }

PARS :: { [ArgDeclaration] }
  : PARS comma PAR                                                      { $3:$1 }
  | PAR                                                                 { [$1] }

PAR :: { ArgDeclaration }
  : PARTYPE ID ofType TYPE                                              { ($1, $2, $4) }

PARTYPE :: { G.ArgType }
  : parVal                                                              { G.Val }
  | parRef                                                              { G.Ref }

TYPE :: { G.GrammarType }
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
  | record brOpen STRUCTITS BRCLOSE                                     {% do
                                                                             checkRecoverableError $2 $4
                                                                             return $ G.Record $1 $ reverse $3 }
  | unionStruct brOpen STRUCTITS BRCLOSE                                {% do
                                                                             checkRecoverableError $2 $4
                                                                             return $ G.Record $1 $ reverse $3 }

BRCLOSE :: { Maybe G.RecoverableError }
  : brClose                                                             { Nothing }
  | error                                                               { Just G.MissingClosingBrace }

ENUMITS :: { [Int] }
  : ENUMITS comma ID                                                    { [] }
  | ID                                                                  { [] }

STRUCTITS :: { [RecordItem] }
  : STRUCTITS comma STRUCTIT                                            { $3:$1 }
  | STRUCTIT                                                            { [$1] }

STRUCTIT :: { RecordItem }
  : ID ofType TYPE                                                      { ($1, $3) }

ID :: { G.Id }
  : id                                                                  { G.Id $1 }

CODEBLOCK :: { G.CodeBlock }
  : INSTBEGIN DECLARS INSTRL INSTEND                                    {% do
                                                                             checkRecoverableError $1 $4
                                                                             return $ G.CodeBlock $ reverse $3 }
  | INSTBEGIN INSTRL INSTEND                                            {% do
                                                                             checkRecoverableError $1 $3
                                                                             return $ G.CodeBlock $ reverse $2 }

INSTBEGIN :: { T.Token }
INSTBEGIN : instructionsBegin                                           {% do
                                                                             ST.enterScope
                                                                             return $1 }

INSTEND :: { Maybe G.RecoverableError }
  : instructionsEnd                                                     {% do
                                                                             ST.exitScope
                                                                             return Nothing }
  | error                                                               { Just G.MissingInstructionListEnd }

DECLARS :: { () }
  : with DECLARSL DECLAREND                                             {% do
                                                                             checkRecoverableError $1 $3
                                                                             addIdsToSymTable (reverse $2) }

DECLAREND :: { Maybe G.RecoverableError }
  : declarend                                                           { Nothing }
  | error                                                               { Just G.MissingDeclarationListEnd }

DECLARSL :: { [NameDeclaration] }
  : DECLARSL comma DECLAR                                               { $3:$1 }
  | DECLAR                                                              { [$1] }

DECLAR :: { NameDeclaration }
  : var ID ofType TYPE                                                  { (ST.Variable, $2, $4, Nothing) }
  | var ID ofType TYPE asig EXPR                                        { (ST.Variable, $2, $4, Just $6) }
  | const ID ofType TYPE asig EXPR                                      { (ST.Constant, $2, $4, Just $6) }

INSTRL :: { G.Instructions }
  : INSTRL seq INSTR                                                    { $3 : $1 }
  | INSTR                                                               { [$1] }

INSTR :: { G.Instruction }
  : LVALUE asig EXPR                                                    {% do
                                                                          checkConstantReassignment $1
                                                                          return $ G.InstAsig $1 $3 }
  | malloc EXPR                                                         { G.InstMalloc $2 }
  | free EXPR                                                           { G.InstFreeMem $2 }
  | cast ID PROCPARS                                                    {% do
                                                                          checkIdAvailability $2
                                                                          return $ G.InstCallProc $2 $3 }
  | FUNCALL                                                             {% let (tk, i, params) = $1 in do
                                                                          buildAndCheckExpr tk $ G.EvalFunc i params
                                                                          return $ G.InstCallFunc i params }
  | return                                                              { G.InstReturn }
  | returnWith EXPR                                                     { G.InstReturnWith $2 }
  | print EXPR                                                          { G.InstPrint $2 }
  | read LVALUE                                                         { G.InstRead $2 }
  | whileBegin EXPR covenantIsActive colon CODEBLOCK whileEnd           { G.InstWhile $2 $5 }
  | ifBegin IFCASES ELSECASE ifEnd                                      { G.InstIf (reverse ($3 : $2)) }
  | ifBegin IFCASES ifEnd                                               { G.InstIf (reverse $2) }
  | switchBegin EXPR colon SWITCHCASES DEFAULTCASE switchEnd            { G.InstSwitch $2 (reverse ($5 : $4)) }
  | switchBegin EXPR colon SWITCHCASES switchEnd                        { G.InstSwitch $2 (reverse $4) }
  | forBegin ID with EXPR souls untilLevel EXPR CODEBLOCK forEnd        {% do
                                                                          checkIterVariable $2 $8
                                                                          return $ G.InstFor $2 $4 $7 $8 }
  | forEachBegin ID withTitaniteFrom EXPR CODEBLOCK forEachEnd          { G.InstForEach $2 $4 $5 }

FUNCALL :: { (T.Token, G.Id, G.Params) }
  : summon ID FUNCPARS                                                  { ($1, $2, $3) }

IFCASES :: { G.IfCases }
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
  : granting PARSLIST TOTHEKNIGHT                                       {% do
                                                                             checkRecoverableError $1 $3
                                                                             return $ reverse $2 }
  | {- empty -}                                                         { [] }

TOTHEKNIGHT :: { Maybe G.RecoverableError }
  : toTheKnight                                                         { Nothing }
  | error                                                               { Just G.MissingFunCallEnd }

PROCPARS :: { G.Params }
  : offering PARSLIST TOTHEESTUSFLASK                                   {% do
                                                                             checkRecoverableError $1 $3
                                                                             return $ reverse $2 }
  | {- empty -}                                                         { [] }

TOTHEESTUSFLASK :: { Maybe G.RecoverableError }
  : toTheEstusFlask                                                     { Nothing }
  | error                                                               { Just G.MissingProcCallEnd }

PARSLIST :: { G.Params }
  : PARSLIST comma EXPR                                                 { $3 : $1 }
  | EXPR                                                                { [$1] }

{

type NameDeclaration = (ST.Category, G.Id, G.GrammarType, Maybe G.Expr)
type ArgDeclaration = (G.ArgType, G.Id, G.GrammarType)
type AliasDeclaration = (G.Id, G.GrammarType)
type RecordItem = AliasDeclaration

buildAndCheckExpr :: T.Token -> G.BaseExpr -> ST.ParserMonad G.Expr
buildAndCheckExpr tk bExpr = do
  (t, expr) <- buildType bExpr tk
  return G.Expr
    { G.expAst = expr,
      G.expType = t,
      G.expTok = tk
    }

extractFieldsForNewScope :: G.GrammarType -> Maybe [RecordItem]
extractFieldsForNewScope (G.Callable (Just s) _) = extractFieldsForNewScope s
extractFieldsForNewScope (G.Compound _ s _) = extractFieldsForNewScope s
extractFieldsForNewScope (G.Record _ s) = Just s
extractFieldsForNewScope _ = Nothing

extractFunParamsForNewScope :: G.GrammarType -> Maybe [ArgDeclaration]
extractFunParamsForNewScope (G.Callable _ s) = Just s
extractFunParamsForNewScope _ = Nothing

parseErrors :: [T.Token] -> ST.ParserMonad a
parseErrors errors =
  let tk@T.Token {T.aToken=abst, T.posn=pn} = errors !! 0
      name = show tk
      line = show $ fst pn
      column = show $ snd pn
      header = bold ++ red ++ "YOU DIED!!" ++ nocolor ++ " Parser error: "
      endmsg = "\n\nFix your syntax errors, ashen one."
      position = "line " ++ bold ++ red ++ line ++ nocolor ++ ", column " ++ bold ++ red ++ column ++ nocolor ++ "."
      msg = header ++ "Unexpected token " ++ bold ++ red ++ name ++ nocolor ++ " at " ++ position ++ endmsg
  in  fail msg

addIdsToSymTable :: [NameDeclaration] -> ST.ParserMonad ()
addIdsToSymTable ids = do
  RWS.mapM_ (addIdToSymTable Nothing) ids

addIdToSymTable :: Maybe Int -> NameDeclaration -> ST.ParserMonad ()
addIdToSymTable mi d@(c, gId@(G.Id tk@(T.Token {T.aToken=at, T.cleanedString=idName})), t, maybeExp) = do
  maybeIdEntry <- ST.dictLookup idName
  maybeTypeEntry <- findTypeOnEntryTable t
  (_, (currScope:_), _) <- RWS.get
  case maybeIdEntry of
    -- The name doesn't exists on the table, we just add it
    Nothing -> do
      insertIdToEntry mi t ST.DictionaryEntry
        { ST.name = idName
        , ST.category = c
        , ST.scope = currScope
        , ST.entryType = ST.name <$> maybeTypeEntry
        , ST.extra = []
        }
      entry <- checkIdAvailability gId
      case (entry, maybeExp) of
        (Nothing, _) -> return ()
        (_, Nothing) -> return ()
        (Just en, Just exp) -> do
          checkTypeOfAssignmentOnInit en exp

    -- The name does exists on the table, we just add it depending on the scope
    Just entry -> do
      let scope = ST.scope entry
      let category = ST.category entry
      if category == ST.Type && c /= ST.RecordItem
      then RWS.tell $ [ST.SemanticError ("Name " ++ show tk ++ " conflicts with an type alias") tk]
      else if category == ST.Procedure
      then RWS.tell $ [ST.SemanticError ("Name " ++ show tk ++ " conflicts with a procedure") tk]
      else if category == ST.Function
      then RWS.tell $ [ST.SemanticError ("Name " ++ show tk ++ " conflicts with a function") tk]
      else if category == ST.RefParam || category == ST.ValueParam
      then RWS.tell $ [ST.SemanticError ("Name "++ show tk ++ " conflicts with a formal param") tk]
      else if currScope /= scope
      then insertIdToEntry mi t ST.DictionaryEntry
        { ST.name = idName
        , ST.category = c
        , ST.scope = currScope
        , ST.entryType = ST.name <$> maybeTypeEntry
        , ST.extra = []
        }
      else RWS.tell $ [ST.SemanticError ("Name " ++ idName ++ " was already declared on this scope") tk]


insertIdToEntry :: Maybe Int -> G.GrammarType -> ST.DictionaryEntry -> ST.ParserMonad ()
insertIdToEntry mi t entry = do
  maybeExtra <- buildExtraForType t
  let pos = (case mi of
              Nothing -> []
              Just i -> [ST.ArgPosition i])
  case maybeExtra of
    Nothing -> return ()
    Just ex -> do
      ST.addEntry entry{ST.extra = pos ++ ex}
      -- To add the record params to the dictionary
      case extractFieldsForNewScope t of
        Nothing -> return ()
        Just s ->  do
          let (ST.Fields _ scope) = fromJust . head $ filter isJust $ map ST.findFieldsExtra ex
          (dict, scopes, curr) <- RWS.get
          RWS.put (dict, scope:scopes, curr)
          addIdsToSymTable $ map (\(a, b) -> (ST.RecordItem, a, b, Nothing)) s
          ST.exitScope
      case extractFunParamsForNewScope t of
        -- If it is nothing then this is not a function
        Nothing -> return ()

        Just s ->
          -- it can be a function without parameters
          if not $ null $ filter ST.isEmptyFunction ex then
            return ()
          else do
            (dict, scopes, curr) <- RWS.get
            let (ST.Fields ST.Callable scope) = head $ filter ST.isFieldsExtra ex
            RWS.put (dict, scope:scopes, curr)
            RWS.mapM_ (\(i, n) -> addIdToSymTable (Just i) n) $ zip [0..] $
              map (\(argType, i, t) -> (if argType == G.Val
                                  then ST.ValueParam
                                  else ST.RefParam, i, t, Nothing)) s

checkConstantReassignment :: G.Expr -> ST.ParserMonad ()
checkConstantReassignment e = case G.expAst e of
  G.IdExpr (G.Id tk@(T.Token {T.cleanedString=idName})) -> do
    maybeEntry <- ST.dictLookup idName
    case maybeEntry of
      Nothing -> do
        return ()
      Just e ->
        case (ST.category e) of
          ST.Constant -> do
            RWS.tell [ST.SemanticError ("Name " ++ idName ++ " is a constant and must not be reassigned") tk]
            return ()
          _ ->
            return ()
  G.IndexAccess gId _ -> checkConstantReassignment gId
  _ -> return ()

checkIterVariable :: G.Id -> G.CodeBlock -> ST.ParserMonad ()
checkIterVariable id@(G.Id tk) (G.CodeBlock insts) = do
  let assignments = filter isAssignment insts
  let errors = filter modifiesIterVariable assignments
  if length errors == 0
  then return ()
  else do
    let (G.InstAsig f _) = head errors
    let faultyTk = G.expTok f
    RWS.tell [ST.SemanticError ("Iteration variable " ++ show id ++ " must not be reassigned") faultyTk]
    return ()
  where isAssignment (G.InstAsig _ _) = True
        isAssignment _ = False
        modifiesIterVariable (G.InstAsig l _) = (T.cleanedString (G.expTok l)) == (T.cleanedString tk)
        modifiesIterVariable _ = False

checkIdAvailability :: G.Id -> ST.ParserMonad (Maybe ST.DictionaryEntry)
checkIdAvailability (G.Id tk@(T.Token {T.cleanedString=idName})) = do
  maybeEntry <- ST.dictLookup idName
  case maybeEntry of
    Nothing -> do
      RWS.tell [ST.SemanticError ("Name " ++ idName ++ " is not available on this scope") tk]
      return Nothing
    Just e -> do
      return $ Just e

checkRecoverableError :: T.Token -> Maybe G.RecoverableError -> ST.ParserMonad ()
checkRecoverableError openTk maybeErr = do
  case maybeErr of
    Nothing ->
      return ()
    Just err -> do
      let errorName = show err
      RWS.tell [ST.SemanticError (errorName ++ " (recovered from to continue parsing)") openTk]
      return ()

extractFieldsFromExtra :: [ST.Extra] -> ST.Extra
extractFieldsFromExtra [] = error "The `extra` array doesn't have any `Fields` item"
extractFieldsFromExtra (s@ST.Fields{} : _) = s
extractFieldsFromExtra (_:ss) = extractFieldsFromExtra ss

addFunction :: NameDeclaration -> ST.ParserMonad (Maybe (ST.Scope, G.Id))
addFunction d@(_, i@(G.Id tk@(T.Token {T.cleanedString=idName})), _, _) = do
  (dict, stack, currScope) <- RWS.get
  maybeEntry <- ST.dictLookup idName
  case maybeEntry of
    Nothing -> do
      addIdToSymTable Nothing d
      return $ Just (currScope, i)
    Just entry -> do
      RWS.tell [ST.SemanticError ("Function " ++ idName ++ " was already declared") tk]
      return Nothing

updateCodeBlockOfFun :: ST.Scope -> G.Id -> G.CodeBlock -> ST.ParserMonad ()
updateCodeBlockOfFun currScope (G.Id tk@(T.Token {T.cleanedString=idName})) code = do
  let f x = (if and [ST.scope x == currScope, ST.name x == idName, ST.category x `elem` [ST.Function, ST.Procedure]]
            then let e = ST.extra x in x{ST.extra = (ST.CodeBlock code) : e}
            else x)
  ST.updateEntry (\ds -> Just $ map f ds) idName

findTypeOnEntryTable :: G.GrammarType -> ST.ParserMonad (Maybe ST.DictionaryEntry)

-- For simple data types
findTypeOnEntryTable (G.Simple tk mSize) = do
  maybeEntry <- ST.dictLookup $ ST.tokensToEntryName tk
  case maybeEntry of
    Nothing -> do
      RWS.tell [ST.SemanticError ("Type " ++ (show tk) ++ " not found") tk]
      return maybeEntry
    _ -> return maybeEntry

-- For compound data types, this extracts the constructor
findTypeOnEntryTable (G.Compound tk _ _) = do
  ST.dictLookup $ ST.tokensToEntryName tk

-- For record alike data types (unions and structs), this extracts their constructor
findTypeOnEntryTable (G.Record tk _) = ST.dictLookup $ ST.tokensToEntryName tk

-- For functions
findTypeOnEntryTable (G.Callable (Just t) _) = findTypeOnEntryTable t

findTypeOnEntryTable (G.Callable Nothing _) = return Nothing

buildExtraForType :: G.GrammarType -> ST.ParserMonad (Maybe [ST.Extra])

-- For string alike data types
buildExtraForType t@(G.Simple _ maybeSize) = do
  maybeType <- findTypeOnEntryTable t
  case maybeType of
    Nothing -> return Nothing
    Just t' -> return $ Just [case maybeSize of
      Just e -> ST.Compound (ST.name t') e
      Nothing -> ST.Simple (ST.name t')]

-- For array alike data types
buildExtraForType t@(G.Compound _ tt@(G.Simple _ _) maybeExpr) = do
  maybeTypeEntry <- findTypeOnEntryTable tt
  constructor <- (ST.name . fromJust) <$> findTypeOnEntryTable t -- This call should never fail
  case maybeTypeEntry of
    Just t -> do
      extras <- fromJust <$> buildExtraForType tt -- safe
      let newExtra = if null extras then (ST.Simple $ ST.name t) else (head extras)
      return $ Just (case maybeExpr of
        Just e -> [ST.CompoundRec constructor e newExtra]
        Nothing -> [ST.Recursive constructor newExtra])
    Nothing -> return Nothing

buildExtraForType t@(G.Compound _ tt@(G.Compound{}) maybeExpr) = do
  maybeExtra <- buildExtraForType tt
  case maybeExtra of
    Nothing -> return Nothing
    Just (extra':_) -> do
      constructor <- (ST.name . fromJust) <$> findTypeOnEntryTable t -- This call should never fail
      return $ case maybeExpr of
        Just e -> Just [ST.CompoundRec constructor e extra']
        Nothing -> Just [ST.Recursive constructor extra']

buildExtraForType t@(G.Compound _ tt@(G.Record{}) maybeExpr) = do
  maybeExtra <- buildExtraForType tt
  case maybeExtra of
    Nothing -> return Nothing
    Just (extra':_) -> do
      constructor <- (ST.name . fromJust) <$> findTypeOnEntryTable t -- This call should never fail
      return $ case maybeExpr of
        Just e -> Just [ST.CompoundRec constructor e extra']
        Nothing -> Just [ST.Recursive constructor extra']

buildExtraForType t@(G.Record tk _) = do
  (d, s, currScope) <- RWS.get
  let constr = (case T.aToken tk of
                  T.TkRecord -> ST.Record
                  T.TkUnionStruct -> ST.Union)

  let ret = Just [ST.Fields constr $ currScope + 1]
  RWS.put $ (d, s, currScope + 1)
  return ret

buildExtraForType t@(G.Callable t' []) = do
  case t' of
    -- For void functions
    Nothing -> return $ Just [ST.EmptyFunction]
    Just tt -> do
      mExtra <- buildExtraForType tt
      case mExtra of
        Nothing -> return Nothing
        Just extras -> return $ Just (ST.EmptyFunction : extras)

buildExtraForType t@(G.Callable t' _) = do
  case t' of
    Nothing -> do
      (d, s, currScope) <- RWS.get
      let ret = Just [ST.Fields ST.Callable $ currScope + 1]
      RWS.put (d, s, currScope + 1)
      return ret
    Just tt -> do
      mExtra <- buildExtraForType tt
      case mExtra of
        Nothing -> return $ Nothing
        Just extras -> do
          (d, s, currScope) <- RWS.get
          let ret = Just ((ST.Fields ST.Callable $ currScope + 1) : extras)
          RWS.put (d, s, currScope + 1)
          return ret

-- For anything else
buildExtraForType _ = return $ Just []

------------------
-- TYPECHECKING --
------------------

class TypeCheckable a where
  getType :: a -> ST.ParserMonad T.Type

isIntegerType :: T.Type -> Bool
isIntegerType t = t `elem` T.integerTypes

exprsToTypes :: [G.Expr] -> [T.Type]
exprsToTypes = map G.expType

-- this functions assumes that expressions can be casted to the
-- target type if they are different
addCastToExprs :: [(G.Expr, T.Type)] -> [G.Expr]
addCastToExprs = map caster
  where
    caster :: (G.Expr, T.Type) -> G.Expr
    caster (expr, t) = let t' = G.expType expr in
      if t == t' then expr
      else expr{G.expAst = G.Caster expr t'}

containerCheck :: [G.Expr] -> (T.Type -> T.Type) -> ([G.Expr] -> G.BaseExpr) -> T.Token -> ST.ParserMonad (T.Type, G.BaseExpr)
containerCheck [] c c' _ = return (c T.Any, c' [])
containerCheck exprs typeCons exprCons tk = do
  let types = exprsToTypes exprs
  let t = findTypeForContainerLiteral types
  -- if t == T.TypeError
  -- then RWS.tell [ST.SemanticError (T.typeMismatchMessage tk)tk]
  -- else return ()

  case t of
    -- case when there is already a type error on the list
    (Nothing, T.TypeError) -> return (T.TypeError, exprCons exprs)

    -- a particular expression couldn't be casted to the accumulated type
    (Just x, T.TypeError) -> do
      let tk = G.expTok $ exprs !! x -- not so efficient, but works
      RWS.tell [ST.SemanticError ("Type of item #" ++ show x ++ " of list mismatch") tk]
      return (T.TypeError, exprCons exprs)

    -- when a real type is found, we can safely ignore the first part of the tuple
    (_, t') -> do
      let castedExprs = addCastToExprs $ zip exprs (replicate (length exprs) t')
      return (typeCons t', exprCons castedExprs)

findTypeForContainerLiteral :: [T.Type] -> (Maybe Int, T.Type)
findTypeForContainerLiteral [] = (Nothing, T.Any)
findTypeForContainerLiteral types = findT (zip [0..] types) (head types)
  where
    findT :: [(Int, T.Type)] -> T.Type -> (Maybe Int, T.Type)
    findT [] a = (Nothing, a)
    findT ((i, a) : xs) b =
      if a `T.canBeConvertedTo` b then findT xs b
      else if b `T.canBeConvertedTo` a then findT xs a
      else (Just i, T.TypeError)

checkIndexAccess :: G.Expr -> G.Expr -> T.Token -> ST.ParserMonad T.Type
checkIndexAccess array index tk = do
  arrayType <- getType array
  indextype <- getType index
  case arrayType of
    T.ArrayT t -> if isIntegerType indextype
      then return t
      else return T.TypeError
    T.TypeError -> return T.TypeError
    _ -> do
      RWS.tell [ST.SemanticError "Left side is not a chest" tk]
      return T.TypeError

functionsCheck :: G.Id -> [G.Expr] -> T.Token -> ST.ParserMonad (T.Type, G.BaseExpr)
functionsCheck funId exprs tk = do
  funType <- getType funId
  let originalExpr = G.EvalFunc funId exprs
  case funType of
    T.FunctionT domain range -> do
      let exprsTypes = exprsToTypes exprs
      let exprsTypesL = length exprsTypes
      let domainL = length domain
      if exprsTypesL < domainL then do
        RWS.tell [ST.SemanticError ("Function " ++ show funId ++ " received less arguments than it expected") tk]
        return (T.TypeError, originalExpr)
      else if exprsTypesL > domainL then do
        RWS.tell [ST.SemanticError ("Function " ++ show funId ++ " received more arguments than it expected") tk]
        return (T.TypeError, originalExpr)
      else do
        if T.TypeError `elem` exprsTypes then
          return (T.TypeError, originalExpr)
        else case findInvalidArgument $ zip exprsTypes domain of
          -- all looking good, now cast each argument to it's correspondent parameter type
          Nothing -> do
            let castedExprs = addCastToExprs $ zip exprs domain
            return (range, G.EvalFunc funId castedExprs)

          -- oh oh, an argument could not be implicitly casted to its correspondent argument
          Just x -> do
            RWS.tell [ST.SemanticError ("Argument #" ++ show x ++ " type mismatch with parameter #" ++ show x ++ " type") tk]
            return (T.TypeError, originalExpr)

    T.TypeError -> return (T.TypeError, originalExpr) -- we don't need to log - getType already does that
    _ -> do
      RWS.tell [ST.SemanticError "You're trying to call a non-callable expression" tk]
      return (T.TypeError, originalExpr)
  where
    -- left side is the type of the argument, right side is the parameter type
    findInvalidArgument :: [(T.Type, T.Type)] -> Maybe Int
    findInvalidArgument ts = findInvalid ts 0

    findInvalid :: [(T.Type, T.Type)] -> Int -> Maybe Int
    findInvalid [] _ = Nothing
    findInvalid ((argT, paramT) : xs) i = if argT `T.canBeConvertedTo` paramT
      then findInvalid xs (i + 1)
      else Just i


memAccessCheck :: G.Expr -> T.Token -> ST.ParserMonad T.Type
memAccessCheck expr tk = do
  t <- getType expr
  case t of
    T.PointerT t' -> return t'
    T.TypeError -> return T.TypeError
    _ -> do
      RWS.tell [ST.SemanticError "Trying to access memory of non-arrow variable" tk]
      return T.TypeError

checkAsciiOf :: G.Expr -> T.Token -> ST.ParserMonad T.Type
checkAsciiOf e tk = do
  t <- getType e
  case t of
    T.CharT -> return T.BigIntT
    T.StringT -> return $ T.ArrayT T.BigIntT
    T.TypeError -> return T.TypeError
    _ -> do
      RWS.tell [ST.SemanticError ("ascii_of requires argument to be a miracle or a sign") tk]
      return T.TypeError


checkAccess :: G.Expr -> G.Id -> T.Token -> ST.ParserMonad T.Type
checkAccess e (G.Id T.Token{T.cleanedString=i}) tk = do
  t <- getType e
  case t of
    T.RecordT properties -> checkProperty properties
    T.UnionT properties -> checkProperty properties
    T.TypeError -> return T.TypeError
    _ -> do
      RWS.tell [ST.SemanticError "Left side of access is not a record nor union" tk]
      return T.TypeError
  where
    checkProperty :: [T.PropType] -> ST.ParserMonad T.Type
    checkProperty props =
      case filter ((==i) . fst) $ map (\(T.PropType e) -> e) props of
        ((_,a):_) -> return a
        _ -> do
          RWS.tell [ST.SemanticError ("Property " ++ i ++ " doesn't exist") tk]
          return T.TypeError

unaryOpCheck :: G.Expr -> G.Op1 -> T.Token -> ST.ParserMonad (T.Type, G.BaseExpr)
unaryOpCheck expr op tk = do
  t <- getType expr
  let finalExpr = G.Op1 op expr
  if t == T.TypeError then
    return (T.TypeError, finalExpr)
  else if t `elem` expectedForOperand then
    let finalType = if op == G.Negate then t else T.TrileanT in
    return (finalType, finalExpr)
  else do
    RWS.tell [ST.SemanticError (T.typeMismatchMessage tk) tk]
    return (T.TypeError, finalExpr)
  where
    expectedForOperand
      | op == G.Negate = T.arithmeticTypes
      | otherwise = T.booleanSingleton

binaryOpCheck :: G.Expr -> G.Expr -> G.Op2 -> T.Token -> ST.ParserMonad (T.Type, G.BaseExpr)
binaryOpCheck leftExpr rightExpr op tk = do
  leftType <- getType leftExpr
  rightType <- getType rightExpr
  if leftType == T.TypeError || rightType == T.TypeError then
    return (T.TypeError, G.Op2 op leftExpr rightExpr)
  else if leftType `T.canBeConvertedTo` rightType then
    let [castedLeftExpr] = addCastToExprs [(leftExpr, rightType)] in
    checkIfInExpected castedLeftExpr rightExpr
  else if rightType `T.canBeConvertedTo` leftType then
    let [castedRightExpr] = addCastToExprs [(rightExpr, leftType)] in
    checkIfInExpected leftExpr castedRightExpr
  else do
    RWS.tell [ST.SemanticError "Left and right side of operand can't be operated together" tk]
    return (T.TypeError, G.Op2 op leftExpr rightExpr)

  where
    expectedForOperands :: [T.Type]
    expectedForOperands
      | op `elem` G.arithmeticOp2 = T.arithmeticTypes
      | op `elem` G.comparableOp2 = T.anySingleton -- we can compare any type if they are the same
      | op `elem` G.booleanOp2 = T.booleanSingleton
      | op `elem` G.setOp2 = T.anySetSingleton
      | op `elem` G.arrayOp2 = T.anyArraySingleton
      | otherwise = error (show op ++ " doesn't have a value in expectedForOperands function")
    checkIfInExpected :: G.Expr -> G.Expr -> ST.ParserMonad (T.Type, G.BaseExpr)
    checkIfInExpected l r = do
      -- l and r have here the same type so it doesn't matter what we choose
      t <- getType l
      let exp = G.Op2 op l r
      if t `elem` expectedForOperands then

        -- the final type depends on the operator
        let finalType = if op `elem` G.comparableOp2 then T.TrileanT else t in
        return (finalType, exp)
      else do
        RWS.tell [ST.SemanticError (T.typeMismatchMessage tk) tk]
        return (T.TypeError, exp)


checkSetSize :: G.Expr -> T.Token -> ST.ParserMonad T.Type
checkSetSize expr tk = do
  t <- getType expr
  case t of
    T.SetT _ -> return T.BigIntT
    T.TypeError -> return T.TypeError
    _ -> do
      RWS.tell [ST.SemanticError "`size` expects a set" tk]
      return T.TypeError

checkTypeOfAssignmentOnInit :: ST.DictionaryEntry -> G.Expr -> ST.ParserMonad ()
checkTypeOfAssignmentOnInit entry expr = do
  entryType <- getType entry
  exprType <- getType expr
  if exprType `T.canBeConvertedTo` entryType
  then return ()
  else RWS.tell [ST.SemanticError "Invalid asignment" (G.expTok expr)]

minBigInt :: Int
minBigInt = - 2147483648

maxBigInt :: Int
maxBigInt = 2147483647

minSmallInt :: Int
minSmallInt = - 32768

maxSmallInt :: Int
maxSmallInt = 32767

buildTypeForNonCasterExprs :: ST.ParserMonad T.Type -> G.BaseExpr -> ST.ParserMonad (T.Type, G.BaseExpr)
buildTypeForNonCasterExprs tt bExpr = tt >>= \t -> return (t, bExpr)


buildType :: G.BaseExpr -> T.Token -> ST.ParserMonad (T.Type, G.BaseExpr)
buildType bExpr tk = case bExpr of
  -- Language literals, their types can be (almost everytime) infered
  G.TrueLit -> return (T.TrileanT, bExpr)
  G.FalseLit -> return (T.TrileanT, bExpr)
  G.UndiscoveredLit -> return (T.TrileanT, bExpr)
  G.NullLit -> return (T.PointerT T.Any, bExpr)

  -- A literal of an integer is valid if it is on the correct range
  G.IntLit n ->
    if minSmallInt <= n && n <= maxSmallInt
    then return (T.SmallIntT, bExpr)
    else if minBigInt <= n && n <= maxBigInt
    then return (T.BigIntT, bExpr)
    else error "TODO: check for the int size, modify grammar to carry token position"
  G.FloatLit _ -> return (T.FloatT, bExpr)
  G.CharLit _ -> return (T.CharT, bExpr)
  G.StringLit _ -> return (T.StringT, bExpr)

  G.ArrayLit exprs -> containerCheck exprs T.ArrayT G.ArrayLit tk
  G.SetLit exprs -> containerCheck exprs T.SetT G.SetLit tk

  G.Op1 op a -> unaryOpCheck a op tk

  G.Op2 op a b -> binaryOpCheck a b op tk

  G.IdExpr i -> buildTypeForNonCasterExprs (getType i) bExpr
  G.IndexAccess e i -> buildTypeForNonCasterExprs (checkIndexAccess e i tk) bExpr
  G.EvalFunc i params -> functionsCheck i params tk
  G.MemAccess e -> buildTypeForNonCasterExprs (memAccessCheck e tk) bExpr
  G.AsciiOf e -> buildTypeForNonCasterExprs (checkAsciiOf e tk) bExpr
  G.Access e i -> buildTypeForNonCasterExprs (checkAccess e i tk) bExpr
  G.SetSize e -> buildTypeForNonCasterExprs (checkSetSize e tk) bExpr

instance TypeCheckable G.Id where
  getType gId = do
    idEntry <- checkIdAvailability gId
    case idEntry of
      Nothing -> return T.TypeError
      Just entry -> getType entry

instance TypeCheckable G.Expr where
  getType = return . G.expType

instance TypeCheckable ST.DictionaryEntry where
  getType entry@ST.DictionaryEntry{ST.entryType=Just entryType, ST.category = cat, ST.extra = extras}
    -- If it is an alias, return just the name
    | cat == ST.Type = return $ T.AliasT (ST.name entry)


    | cat `elem` [ST.Function, ST.Procedure] = do
      let isEmptyFunction = not . null $ filter ST.isEmptyFunction extras
      range <- (case cat of
        ST.Procedure -> return T.VoidT
        ST.Function -> getType entry{ST.category=ST.Variable}) -- cheating
      domain <- (if isEmptyFunction
        then return []
        else do
          let fields = head $ filter ST.isFieldsExtra extras
          (T.TypeList list) <- getType fields
          return list
          )
      return $ T.FunctionT domain range

    | cat `elem` [
        ST.Variable, ST.Constant,
        ST.RecordItem, ST.UnionItem,
        ST.RefParam, ST.ValueParam] = getType $ ST.extractTypeFromExtra extras



    | otherwise = error "error on getType for dict entries"

instance TypeCheckable ST.Extra where
  getType (ST.Recursive s extra) = do
      t <- getType extra
      if s == ST.arrow then return $ T.PointerT t
      else if s == ST.armor then return $ T.SetT t
      else return T.TypeError -- This should not happen, but life is hard

  -- For the moment, ST.Compound only corresponds to string
  getType (ST.Compound s _) = return $ T.StringT

  -- For the moment, ST.CompoundRect only corresponds to array
  getType (ST.CompoundRec s _ extra) = getType extra >>= \t -> return $ T.ArrayT t

  getType (ST.Fields ST.Callable scope) = do
    (dict, _, _) <- RWS.get
    types <- mapM getType $ ST.sortByArgPosition $ ST.findAllInScope scope dict
    return $ T.TypeList types

  getType (ST.Fields b scope) = do
    (dict, _, _) <- RWS.get
    let entries = ST.findAllInScope scope dict
    let entryNames = map ST.name entries
    types <- mapM getType entries
    case b of
      ST.Record -> return $ T.RecordT $ map T.PropType $ zip entryNames types
      _ -> return $ T.UnionT $ map T.PropType $ zip entryNames types

  getType ST.EmptyFunction = return $ T.TypeList []

  getType (ST.Simple s)
      | s == ST.smallHumanity = return T.SmallIntT
      | s == ST.humanity = return T.BigIntT
      | s == ST.hollow = return T.FloatT
      | s == ST.sign = return T.CharT
      | s == ST.bonfire = return T.TrileanT
      | s == ST.void = return T.VoidT
      | otherwise = return $ T.AliasT s -- works because it always exists
                                        -- it shouldn't be added otherwise
}