{
module Parser (
  parse) where

import qualified Lexer as L
import qualified SymTable as ST
import Data.Maybe
import qualified Grammar as G
import qualified Control.Monad.RWS as RWS
import qualified TypeChecking as T
import Utils
}

%name                                                                     parse
%tokentype                                                              { L.Token }
%error                                                                  { parseErrors }
%monad                                                                  { ST.ParserMonad }


%nonassoc size memAccessor

%left ARRCLOSE

%left eq neq
%left plus minus
%left mult div mod
%left NEG

%left and or

%nonassoc lt lte gt gte

%left colConcat
%left diff
%left union intersect


%left asciiOf
%right not
%left accessor
%left arrOpen arrClose


%token
  programBegin                                                          { L.Token {L.aToken=L.TkProgramBegin} }
  programEnd                                                            { L.Token {L.aToken=L.TkProgramEnd} }

  aliasListBegin                                                        { L.Token {L.aToken=L.TkAliasListBegin} }
  aliasListEnd                                                          { L.Token {L.aToken=L.TkAliasListEnd} }
  alias                                                                 { L.Token {L.aToken=L.TkAlias} }

  id                                                                    { L.Token {L.aToken=L.TkId} }

  ofType                                                                { L.Token {L.aToken=L.TkOfType} }

  paramRequest                                                          { L.Token {L.aToken=L.TkRequesting} }
  parVal                                                                { L.Token {L.aToken=L.TkVal} }
  parRef                                                                { L.Token {L.aToken=L.TkRef} }

  bigInt                                                                { L.Token {L.aToken=L.TkBigInt} }
  smallInt                                                              { L.Token {L.aToken=L.TkSmallInt} }
  float                                                                 { L.Token {L.aToken=L.TkFloat} }
  char                                                                  { L.Token {L.aToken=L.TkChar} }
  bool                                                                  { L.Token {L.aToken=L.TkBool} }
  ltelit                                                                { L.Token {L.aToken=L.TkLteLit} }
  string                                                                { L.Token {L.aToken=L.TkString} }
  array                                                                 { L.Token {L.aToken=L.TkArray} }
  set                                                                   { L.Token {L.aToken=L.TkSet} }
  enum                                                                  { L.Token {L.aToken=L.TkEnum} }
  unionStruct                                                           { L.Token {L.aToken=L.TkUnionStruct} }
  record                                                                { L.Token {L.aToken=L.TkRecord} }
  pointer                                                               { L.Token {L.aToken=L.TkPointer} }

  intLit                                                                { L.Token {L.aToken=L.TkIntLit} }
  floatLit                                                              { L.Token {L.aToken=L.TkFloatLit} }
  charLit                                                               { L.Token {L.aToken=L.TkCharLit} }
  stringLit                                                             { L.Token {L.aToken=L.TkStringLit} }
  trueLit                                                               { L.Token {L.aToken=L.TkLit} }
  falseLit                                                              { L.Token {L.aToken=L.TkUnlit} }
  unknownLit                                                            { L.Token {L.aToken=L.TkUndiscovered} }
  nullLit                                                               { L.Token {L.aToken=L.TkNull} }

  functionBegin                                                         { L.Token {L.aToken=L.TkInvocation} }
  functionType                                                          { L.Token {L.aToken=L.TkInvocationType} }
  functionEnd                                                           { L.Token {L.aToken=L.TkInvocationEnd} }

  procedureBegin                                                        { L.Token {L.aToken=L.TkSpell} }
  procedureEnd                                                          { L.Token {L.aToken=L.TkSpellEnd} }

  comma                                                                 { L.Token {L.aToken=L.TkComma} }
  brOpen                                                                { L.Token {L.aToken=L.TkBraceOpen} }
  brClose                                                               { L.Token {L.aToken=L.TkBraceClosed} }

  with                                                                  { L.Token {L.aToken=L.TkWith} }
  declarend                                                             { L.Token {L.aToken=L.TkDeclarationEnd} }

  const                                                                 { L.Token {L.aToken=L.TkConst} }
  var                                                                   { L.Token {L.aToken=L.TkVar} }
  asig                                                                  { L.Token {L.aToken=L.TkAsig} }

  instructionsBegin                                                     { L.Token {L.aToken=L.TkInstructionBegin} }
  instructionsEnd                                                       { L.Token {L.aToken=L.TkInstructionEnd} }
  seq                                                                   { L.Token {L.aToken=L.TkSeq} }

  cast                                                                  { L.Token {L.aToken=L.TkCast} }
  offering                                                              { L.Token {L.aToken=L.TkOffering} }
  toTheKnight                                                           { L.Token {L.aToken=L.TkInvocationParsEnd} }

  summon                                                                { L.Token {L.aToken=L.TkSummon} }
  granting                                                              { L.Token {L.aToken=L.TkGranting} }
  toTheEstusFlask                                                       { L.Token {L.aToken=L.TkSpellParsEnd} }

  return                                                                { L.Token {L.aToken=L.TkReturn} }
  returnWith                                                            { L.Token {L.aToken=L.TkReturnWith} }

  print                                                                 { L.Token {L.aToken=L.TkPrint} }
  read                                                                  { L.Token {L.aToken=L.TkRead} }

  whileBegin                                                            { L.Token {L.aToken=L.TkWhile} }
  whileEnd                                                              { L.Token {L.aToken=L.TkEndWhile} }
  covenantIsActive                                                      { L.Token {L.aToken=L.TkCovenantIsActive} }

  ifBegin                                                               { L.Token {L.aToken=L.TkIf} }
  ifEnd                                                                 { L.Token {L.aToken=L.TkEndIf} }
  colon                                                                 { L.Token {L.aToken=L.TkColon} }
  else                                                                  { L.Token {L.aToken=L.TkElse} }

  switchBegin                                                           { L.Token {L.aToken=L.TkSwitch} }
  switchDefault                                                         { L.Token {L.aToken=L.TkSwitchDefault} }
  switchEnd                                                             { L.Token {L.aToken=L.TkEndSwitch} }

  forBegin                                                              { L.Token {L.aToken=L.TkFor} }
  forEnd                                                                { L.Token {L.aToken=L.TkEndFor} }
  souls                                                                 { L.Token {L.aToken=L.TkSoul} }
  untilLevel                                                            { L.Token {L.aToken=L.TkLevel} }

  forEachBegin                                                          { L.Token {L.aToken=L.TkForEach} }
  forEachEnd                                                            { L.Token {L.aToken=L.TkEndForEach} }
  withTitaniteFrom                                                      { L.Token {L.aToken=L.TkWithTitaniteFrom} }

  parensOpen                                                            { L.Token {L.aToken=L.TkParensOpen} }
  parensClosed                                                          { L.Token {L.aToken=L.TkParensClosed} }

  plus                                                                  { L.Token {L.aToken=L.TkPlus} }
  minus                                                                 { L.Token {L.aToken=L.TkMinus} }
  mult                                                                  { L.Token {L.aToken=L.TkMult} }
  div                                                                   { L.Token {L.aToken=L.TkDiv} }
  mod                                                                   { L.Token {L.aToken=L.TkMod} }
  lt                                                                    { L.Token {L.aToken=L.TkLt} }
  gt                                                                    { L.Token {L.aToken=L.TkGt} }
  lte                                                                   { L.Token {L.aToken=L.TkLte} }
  gte                                                                   { L.Token {L.aToken=L.TkGte} }
  eq                                                                    { L.Token {L.aToken=L.TkEq} }
  neq                                                                   { L.Token {L.aToken=L.TkNeq} }
  not                                                                   { L.Token {L.aToken=L.TkNot} }
  and                                                                   { L.Token {L.aToken=L.TkAnd} }
  or                                                                    { L.Token {L.aToken=L.TkOr} }
  asciiOf                                                               { L.Token {L.aToken=L.TkAsciiOf} }
  colConcat                                                             { L.Token {L.aToken=L.TkConcat} }
  union                                                                 { L.Token {L.aToken=L.TkUnion} }
  intersect                                                             { L.Token {L.aToken=L.TkIntersect} }
  diff                                                                  { L.Token {L.aToken=L.TkDiff} }
  size                                                                  { L.Token {L.aToken=L.TkSize} }

  arrOpen                                                               { L.Token {L.aToken=L.TkArrayOpen} }
  arrClose                                                              { L.Token {L.aToken=L.TkArrayClose} }
  setOpen                                                               { L.Token {L.aToken=L.TkSetOpen} }
  setClose                                                              { L.Token {L.aToken=L.TkSetClose} }

  accessor                                                              { L.Token {L.aToken=L.TkAccessor} }
  memAccessor                                                           { L.Token {L.aToken=L.TkAccessMemory} }

  malloc                                                                { L.Token {L.aToken=L.TkRequestMemory} }
  free                                                                  { L.Token {L.aToken=L.TkFreeMemory} }
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
  : alias ID TYPE                                                       { (ST.Type, $2, $3) }

LVALUE :: { G.Expr }
  : ID                                                                  {% do
                                                                            checkIdAvailability $1
                                                                            let (G.Id tk) = $1
                                                                            buildAndCheckExpr tk $ G.IdExpr $1 }
  | EXPR accessor ID                                                    {% do
                                                                            let expr = G.Access $1 $3
                                                                            ret <- buildAndCheckExpr $2 expr
                                                                            checkPropertyAvailability ret
                                                                            return ret }
  | EXPR arrOpen EXPR arrClose                                          {% buildAndCheckExpr $2 $ G.IndexAccess $1 $3 }
  | memAccessor EXPR                                                    {% buildAndCheckExpr $1 $ G.MemAccess $2 }

EXPR :: { G.Expr }
  : intLit                                                              {% buildAndCheckExpr $1 $ G.IntLit (read (L.cleanedString $1) :: Int) }
  | floatLit                                                            {% buildAndCheckExpr $1 $ G.FloatLit (read (L.cleanedString $1) :: Float) }
  | charLit                                                             {% buildAndCheckExpr $1 $ G.CharLit $ head (L.cleanedString $1) }
  | stringLit                                                           {% buildAndCheckExpr $1 $ G.StringLit (L.cleanedString $1) }
  | trueLit                                                             {% buildAndCheckExpr $1 G.TrueLit }
  | falseLit                                                            {% buildAndCheckExpr $1 G.FalseLit }
  | unknownLit                                                          {% buildAndCheckExpr $1 G.UndiscoveredLit }
  | nullLit                                                             {% buildAndCheckExpr $1 G.NullLit }
  | arrOpen EXPRL arrClose                                              {% buildAndCheckExpr $1 $ G.ArrayLit $ reverse $2 }
  | setOpen EXPRL setClose                                              {% buildAndCheckExpr $1 $ G.SetLit $ reverse $2 }
  | parensOpen EXPR parensClosed                                        { $2{G.expTok=$1} }
  | minus EXPR                                                          {% buildAndCheckExpr $1 $ G.Negative $2 }
  | not EXPR                                                            {% buildAndCheckExpr $1 $ G.Not $2 }
  | asciiOf EXPR                                                        {% buildAndCheckExpr $1 $ G.AsciiOf $2 }
  | size EXPR                                                           {% buildAndCheckExpr $1 $ G.SetSize $2 }
  | EXPR plus EXPR                                                      {% buildAndCheckExpr $2 $ G.Add $1 $3 }
  | EXPR minus EXPR                                                     {% buildAndCheckExpr $2 $ G.Substract $1 $3 }
  | EXPR mult EXPR                                                      {% buildAndCheckExpr $2 $ G.Multiply $1 $3 }
  | EXPR div EXPR                                                       {% buildAndCheckExpr $2 $ G.Divide $1 $3 }
  | EXPR mod EXPR                                                       {% buildAndCheckExpr $2 $ G.Mod $1 $3 }
  | EXPR lt EXPR                                                        {% buildAndCheckExpr $2 $ G.Lt $1 $3 }
  | EXPR gt EXPR                                                        {% buildAndCheckExpr $2 $ G.Gt $1 $3 }
  | EXPR lte EXPR                                                       {% buildAndCheckExpr $2 $ G.Lte $1 $3 }
  | EXPR gte EXPR                                                       {% buildAndCheckExpr $2 $ G.Gte $1 $3 }
  | EXPR eq EXPR                                                        {% buildAndCheckExpr $2 $ G.Eq $1 $3 }
  | EXPR neq EXPR                                                       {% buildAndCheckExpr $2 $ G.Neq $1 $3 }
  | EXPR and EXPR                                                       {% buildAndCheckExpr $2 $ G.And $1 $3 }
  | EXPR or EXPR                                                        {% buildAndCheckExpr $2 $ G.Or $1 $3 }
  | EXPR colConcat EXPR                                                 {% buildAndCheckExpr $2 $ G.ColConcat $1 $3 }
  | EXPR union EXPR                                                     {% buildAndCheckExpr $2 $ G.SetUnion $1 $3 }
  | EXPR intersect EXPR                                                 {% buildAndCheckExpr $2 $ G.SetIntersect $1 $3 }
  | EXPR diff EXPR                                                      {% buildAndCheckExpr $2 $ G.SetDiff $1 $3 }
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
                                                                              $2, G.Callable (Just $5) $3) }
FUNC :: { () }
  : FUNCPREFIX NON_OPENER_CODEBLOCK functionEnd                         {% case $1 of
                                                                            Nothing -> return ()
                                                                            Just (s, i) -> updateCodeBlockOfFun s i $2 }

PROCPREFIX :: { Maybe (ST.Scope, G.Id) }
  : procedureBegin ID PROCPARSDEC                                       {% addFunction (ST.Procedure,
                                                                              $2, G.Callable Nothing $3) }
PROC :: { () }
  : PROCPREFIX NON_OPENER_CODEBLOCK procedureEnd                        {% case $1 of
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

INSTBEGIN :: { L.Token }
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

VARTYPE :: { ST.Category }
  : const                                                               { ST.Constant }
  | var                                                                 { ST.Variable }

DECLAR :: { NameDeclaration }
  : VARTYPE ID ofType TYPE                                              { ($1, $2, $4) }
  | VARTYPE ID ofType TYPE asig EXPR                                    { ($1, $2, $4) }

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
  | FUNCALL                                                             { let (_, i, params) = $1 in
                                                                              G.InstCallFunc i params }
  | return                                                              { G.InstReturn }
  | returnWith EXPR                                                     { G.InstReturnWith $2 }
  | print EXPR                                                          { G.InstPrint $2 }
  | read LVALUE                                                         { G.InstRead $2 }
  | whileBegin EXPR covenantIsActive colon CODEBLOCK whileEnd           { G.InstWhile $2 $5 }
  | ifBegin IFCASES ELSECASE ifEnd                                      { G.InstIf (reverse ($3 : $2)) }
  | ifBegin IFCASES ifEnd                                               { G.InstIf (reverse $2) }
  | switchBegin EXPR colon SWITCHCASES DEFAULTCASE switchEnd            { G.InstSwitch $2 (reverse ($5 : $4)) }
  | switchBegin EXPR colon SWITCHCASES switchEnd                        { G.InstSwitch $2 (reverse $4) }
  | forBegin ID with EXPR souls untilLevel EXPR CODEBLOCK forEnd        { G.InstFor $2 $4 $7 $8 }
  | forEachBegin ID withTitaniteFrom EXPR CODEBLOCK forEachEnd          { G.InstForEach $2 $4 $5 }

FUNCALL :: { (L.Token, G.Id, G.Params) }
  : summon ID FUNCPARS                                                  {% do
                                                                          checkIdAvailability $2
                                                                          return ($1, $2, $3) }

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

type NameDeclaration = (ST.Category, G.Id, G.GrammarType)
type ArgDeclaration = (G.ArgType, G.Id, G.GrammarType)
type AliasDeclaration = (G.Id, G.GrammarType)
type RecordItem = AliasDeclaration

buildAndCheckExpr :: L.Token -> G.BaseExpr -> ST.ParserMonad G.Expr
buildAndCheckExpr tk bExpr = do
  t <- getType bExpr
  if t == T.TypeError
    then RWS.tell [ST.SemanticError "Type error" (L.Token
      { L.aToken = L.TkId
      , L.capturedString = "hola"
      , L.cleanedString = "hola"
      , L.posn = (L.posn tk)
      })]
    else return ()
  return G.Expr
    { G.expAst = bExpr
    , G.expType = t
    , G.expTok = tk
    }

extractFieldsForNewScope :: G.GrammarType -> Maybe [RecordItem]
extractFieldsForNewScope (G.Callable (Just s) _) = extractFieldsForNewScope s
extractFieldsForNewScope (G.Compound _ s _) = extractFieldsForNewScope s
extractFieldsForNewScope (G.Record _ s) = Just s
extractFieldsForNewScope _ = Nothing

extractFunParamsForNewScope :: G.GrammarType -> Maybe [ArgDeclaration]
extractFunParamsForNewScope (G.Callable _ s) = Just s
extractFunParamsForNewScope _ = Nothing

parseErrors :: [L.Token] -> ST.ParserMonad a
parseErrors errors =
  let tk@L.Token {L.aToken=abst, L.posn=pn} = errors !! 0
      name = show tk
      line = show $ L.row pn
      column = show $ L.col pn
      header = bold ++ red ++ "YOU DIED!!" ++ nocolor ++ " Parser error: "
      endmsg = "\n\nFix your syntax errors, ashen one."
      position = "line " ++ bold ++ red ++ line ++ nocolor ++ ", column " ++ bold ++ red ++ column ++ nocolor ++ "."
      msg = header ++ "Unexpected token " ++ bold ++ red ++ name ++ nocolor ++ " at " ++ position ++ endmsg
  in  fail msg

addIdsToSymTable :: [NameDeclaration] -> ST.ParserMonad ()
addIdsToSymTable ids = do
  RWS.mapM_ (addIdToSymTable Nothing) ids

addIdToSymTable :: Maybe Int -> NameDeclaration -> ST.ParserMonad ()
addIdToSymTable mi d@(c, (G.Id tk@(L.Token {L.aToken=at, L.cleanedString=idName})), t) = do
  maybeIdEntry <- ST.dictLookup idName
  maybeTypeEntry <- findTypeOnEntryTable t
  (_, (currScope:_), _) <- RWS.get
  case maybeIdEntry of
    -- The name doesn't exists on the table, we just add it
    Nothing -> insertIdToEntry mi t
      ST.DictionaryEntry
        { ST.name = idName
        , ST.category = c
        , ST.scope = currScope
        , ST.entryType = ST.name <$> maybeTypeEntry
        , ST.extra = []
        }

    -- The name does exists on the table, we just add it depending on the scope
    Just entry -> do
      let scope = ST.scope entry
      if currScope /= scope
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
      RWS.lift $ print t
      RWS.lift $ print ex
      RWS.lift $ print t
      -- To add the record params to the dictionary
      case extractFieldsForNewScope t of
        Nothing -> return ()
        Just s ->  do
          let (ST.Fields _ scope) = fromJust . head $ filter isJust $ map ST.findFieldsExtra ex
          (dict, scopes, curr) <- RWS.get
          RWS.put (dict, scope:scopes, curr)
          addIdsToSymTable $ map (\(a, b) -> (ST.RecordItem, a, b)) s
          ST.exitScope
      case extractFunParamsForNewScope t of
        -- If it is nothing then this is not a function
        Nothing -> return ()
        Just s -> do
          RWS.lift $ print s >> putStrLn "epa"
          (dict, scopes, curr) <- RWS.get
          let (ST.Fields ST.Callable scope) = head $ filter ST.isFieldsExtra ex
          RWS.put (dict, scope:scopes, curr)
          RWS.mapM_ (\(i, n) -> addIdToSymTable (Just i) n) $ zip [0..] $
            map (\(argType, i, t) -> (if argType == G.Val
                                then ST.ValueParam
                                else ST.RefParam, i, t)) s

checkConstantReassignment :: G.Expr -> ST.ParserMonad ()
checkConstantReassignment e = case G.expAst e of
  G.IdExpr (G.Id tk@(L.Token {L.cleanedString=idName})) -> do
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

checkIdAvailability :: G.Id -> ST.ParserMonad (Maybe ST.DictionaryEntry)
checkIdAvailability (G.Id tk@(L.Token {L.cleanedString=idName})) = do
  maybeEntry <- ST.dictLookup idName
  case maybeEntry of
    Nothing -> do
      RWS.tell [ST.SemanticError ("Name " ++ idName ++ " is not available on this scope") tk]
      return Nothing
    Just e -> do
      return $ Just e

checkRecoverableError :: L.Token -> Maybe G.RecoverableError -> ST.ParserMonad ()
checkRecoverableError openTk maybeErr = do
  case maybeErr of
    Nothing ->
      return ()
    Just err -> do
      let errorName = show err
      RWS.tell [ST.SemanticError (errorName ++ " (recovered from to continue parsing)") openTk]
      return ()

-- The following function only have sense (for the moment) on lvalues
--  - Ids
--  - Records
--  - Arrays
checkPropertyAvailability :: G.Expr -> ST.ParserMonad ()
checkPropertyAvailability e = case G.expAst e of
  -- If it is a record accessing, we need to find the _scope_ of the
  -- left side of the expression where to search for the variable
  a@(G.Access expr gId@(G.Id tk@(L.Token {L.cleanedString=s}))) -> do
    maybeScope <- findScopeToSearchOf expr
    case maybeScope of
      Nothing -> RWS.tell [ST.SemanticError ("Property " ++ s ++ " does not exists") tk]
      Just s -> do
        (dict, scopes, curr) <- RWS.get
        RWS.put (dict, s:scopes, curr)
        checkIdAvailability gId
        RWS.put (dict, scopes, curr)

  _ -> error "invalid usage of checkPropertyAvailability"

extractFieldsFromExtra :: [ST.Extra] -> ST.Extra
extractFieldsFromExtra [] = error "The `extra` array doesn't have any `Fields` item"
extractFieldsFromExtra (s@ST.Fields{} : _) = s
extractFieldsFromExtra (_:ss) = extractFieldsFromExtra ss

findScopeToSearchOf :: G.Expr -> ST.ParserMonad (Maybe ST.Scope)
findScopeToSearchOf e = case G.expAst e of
  -- The scope of an id is just the scope of its entry
  G.IdExpr gId -> do
    maybeEntry <- checkIdAvailability gId
    case maybeEntry of
      Nothing -> return Nothing
      Just ST.DictionaryEntry {ST.extra=extra} -> do
        let (ST.Fields _ s) = extractFieldsFromExtra extra
        return $ Just s

  -- The scope of an record accessing is the scope of its accessing property
  G.Access expr gId -> do
    maybeScopeOf <- findScopeToSearchOf expr
    case maybeScopeOf of
      Nothing -> return Nothing
      Just s -> do
        (dict, scopes, curr) <- RWS.get
        RWS.put (dict, s:scopes, curr)
        scope <- findScopeToSearchOf $ G.Expr
                                        { G.expAst=G.IdExpr gId
                                        , G.expType=T.TypeError
                                        , G.expTok=(G.expTok expr)}
        RWS.put (dict, scopes, curr)
        return scope

  -- The scope of a index acces is the scope of it's id
  G.IndexAccess expr _ -> findScopeToSearchOf expr

addFunction :: NameDeclaration -> ST.ParserMonad (Maybe (ST.Scope, G.Id))
addFunction d@(_, i@(G.Id tk@(L.Token {L.cleanedString=idName})), _) = do
  (dict, stack, currScope) <- RWS.get
  maybeEntry <- ST.dictLookup idName
  case maybeEntry of
    Nothing -> do
      addIdToSymTable Nothing d
      return $ Just (currScope, i)
    Just entry -> do
      if ST.scope entry == currScope
      then do
        RWS.tell [ST.SemanticError ("Name " ++ idName ++ " was already declared on this scope") tk]
        return Nothing
      else do
        addIdToSymTable Nothing d
        return $ Just (currScope, i)

updateCodeBlockOfFun :: ST.Scope -> G.Id -> G.CodeBlock -> ST.ParserMonad ()
updateCodeBlockOfFun currScope (G.Id tk@(L.Token {L.cleanedString=idName})) code = do
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
  let constr = (case L.aToken tk of
                  L.TkRecord -> ST.Record
                  L.TkUnionStruct -> ST.Union)

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
  typeMatches :: a -> a -> ST.ParserMonad Bool
  typeMatches a b = do
    aType <- getType a
    bType <- getType b
    return (aType == bType)

isOneOfTypes :: [T.Type] -> G.Expr -> ST.ParserMonad Bool
isOneOfTypes ts a = do
  t <- getType a
  return $ not . null $ filter (== t) ts

type TypeChecker = G.Expr -> ST.ParserMonad Bool

isLogicalType :: TypeChecker
isLogicalType = isOneOfTypes T.booleanTypes

isNumberType :: T.Type -> Bool
isNumberType t = not . null $ filter (==t) T.numberTypes

isIntegerType :: TypeChecker
isIntegerType = isOneOfTypes T.integerTypes

isComparableType :: TypeChecker
isComparableType = isOneOfTypes T.comparableTypes

isShowableType :: TypeChecker
isShowableType = isOneOfTypes T.showableTypes

exprsToTypes :: [G.Expr] -> [T.Type]
exprsToTypes = map G.expType

containerCheck :: [G.Expr] -> (T.Type -> T.Type) -> ST.ParserMonad T.Type
containerCheck [] c = return $ c T.Any
containerCheck exprs constructor = do
  let types = exprsToTypes exprs
  let t = head types
  let allAreSameType = and $ map (==t) types
  return (
    if allAreSameType && t /= T.TypeError
    then constructor t
    else T.TypeError
    )

typeCheck :: (G.Expr, [T.Type]) -> (G.Expr, [T.Type]) -> Maybe T.Type -> ST.ParserMonad T.Type
typeCheck (a, ea) (b, eb) mt = do
  let aType = G.expType a
  let bType = G.expType b
  let correctType = aType `elem` ea
  let correctType' = bType `elem` eb
  return (
    if correctType && correctType'
      then (case mt of
              Nothing -> aType `max` bType
              Just t -> t)
      else T.TypeError)

intArithmeticCheck :: G.Expr -> G.Expr -> ST.ParserMonad T.Type
intArithmeticCheck a b = typeCheck (a, T.integerTypes) (b, T.integerTypes) Nothing

arithmeticCheck :: G.Expr -> G.Expr -> ST.ParserMonad T.Type
arithmeticCheck a b = typeCheck (a, T.numberTypes) (b, T.numberTypes) Nothing

logicalCheck :: G.Expr -> G.Expr -> ST.ParserMonad T.Type
logicalCheck a b = typeCheck (a, T.booleanTypes) (b, T.booleanTypes) Nothing

equatableCheck :: G.Expr -> G.Expr -> ST.ParserMonad T.Type
equatableCheck a b = do
  t <- typeCheck (a, T.numberTypes) (b, T.numberTypes) (Just T.TrileanT)
  t' <- typeCheck (a, [T.CharT]) (b, [T.CharT]) (Just T.TrileanT)
  if t /= T.TypeError || t' /= T.TypeError
    then return T.TrileanT
    else return T.TypeError

comparableCheck :: G.Expr -> G.Expr -> ST.ParserMonad T.Type
comparableCheck = equatableCheck


functionsCheck :: G.Id -> [G.Expr] -> ST.ParserMonad T.Type
functionsCheck funId exprs = do
  let exprsTypes = exprsToTypes exprs
  maybeFunEntry <- checkIdAvailability funId
  case maybeFunEntry of
    Nothing -> return T.TypeError
    Just entry -> return T.TypeError

minBigInt :: Int
minBigInt = - 2147483648

maxBigInt :: Int
maxBigInt = 2147483647

minSmallInt :: Int
minSmallInt = - 32768

maxSmallInt :: Int
maxSmallInt = 32767

instance TypeCheckable G.Id where
  getType _ = return T.TypeError

instance TypeCheckable G.Expr where
  getType = return . G.expType

instance TypeCheckable G.BaseExpr where
  -- Language literals, their types can be (almost everytime) infered
  getType G.TrueLit = return T.TrileanT
  getType G.FalseLit = return T.TrileanT
  getType G.UndiscoveredLit = return T.TrileanT
  getType G.NullLit = return $ T.PointerT T.Any

  -- A literal of an integer is valid if it is on the correct range
  getType (G.IntLit n) =
    if minSmallInt <= n && n <= maxSmallInt
    then return T.SmallIntT
    else if minBigInt <= n && n <= maxBigInt
    then return T.BigIntT
    else error "TODO: check for the int size, modify grammar to carry token position"
  getType (G.FloatLit _) = return T.FloatT
  getType (G.CharLit _) = return T.CharT
  getType (G.StringLit _) = return T.StringT

  getType (G.ArrayLit a) = containerCheck a T.ArrayT
  getType (G.SetLit a) = containerCheck a T.SetT

  getType (G.Add a b) = arithmeticCheck a b
  getType (G.Substract a b) = arithmeticCheck a b
  getType (G.Multiply a b) = arithmeticCheck a b
  getType (G.Divide a b) = arithmeticCheck a b
  getType (G.Mod a b) = intArithmeticCheck a b
  getType (G.Negative a) = arithmeticCheck a a -- cheating
  getType (G.Lt a b) = comparableCheck a b
  getType (G.Lte a b) = comparableCheck a b
  getType (G.Gt a b) = comparableCheck a b
  getType (G.Gte a b) = comparableCheck a b
  getType (G.Eq a b) = equatableCheck a b
  getType (G.Neq a b) = equatableCheck a b
  getType (G.And a b) = logicalCheck a b
  getType (G.Or a b) = logicalCheck a b
  getType (G.Not a) = logicalCheck a a -- cheating

  getType (G.EvalFunc id _) = return T.TypeError -- TODO: Check if okay
  getType (G.Access e i) = return T.TypeError -- TODO: Accessor type
  getType (G.IndexAccess e1 e2) = return T.TypeError -- TODO: Accessor type
  getType (G.MemAccess e) = return T.TypeError -- TODO: Mem access
  getType (G.IdExpr i) = return T.TypeError
  getType (G.AsciiOf e) = return T.TypeError
  getType (G.ColConcat e e') = return T.TypeError
  getType (G.SetUnion e e') = return T.TypeError
  getType (G.SetIntersect e e') = return T.TypeError
  getType (G.SetDiff e e') = return T.TypeError
  getType (G.SetSize e e') = return T.TypeError

instance TypeCheckable ST.DictionaryEntry where
  getType entry@ST.DictionaryEntry{ST.entryType=Just entryType, ST.category = cat, ST.extra = extras}
    -- If it is an alias, return just the name
    | cat == ST.Type = return $ T.AliasT (ST.name entry)
    {- | cat `elem` [T.Constant, T.Function] = do
      let isEmptyFunction = not . null $ filter (\e -> case e of
                                                        ST.EmptyFunction -> True
                                                        _ -> False) extras
      range <- (case cat of
        T.Procedure -> return T.Void
        T.Function -> getType entry{ST.category=ST.variable}) -- cheating

      domain <- (if isEmptyFunction
        then return []
        else return [])
      then return $ T.FunctionT [] (if cat == T.Procedure then T.Void else ) -}
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
    RWS.lift $ print types
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