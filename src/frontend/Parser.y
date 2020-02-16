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
import Errors
}

%name                                                                     parse
%tokentype                                                              { T.Token }
%error                                                                  { parseErrors }
%monad                                                                  { ST.ParserMonad }


%nonassoc size memAccessor

%left ARRCLOSE

%left and or
%nonassoc lt lte gt gte
%left eq neq
%left plus minus
%left mult div mod
%left NEG


%left colConcat
%left diff
%left union intersect

%left isActive
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
  brClose                                                               { T.Token {T.aToken=T.TkBraceClose} }

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
  parensClose                                                           { T.Token {T.aToken=T.TkParensClose} }

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
  isActive                                                              { T.Token {T.aToken=T.TkIsActive} }
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
                                                                             return $ G.CodeBlock $ $2 ++ reverse $3 }
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
                                                                            st <- RWS.get
                                                                            RWS.put st{ST.stScopeStack=[1, 0]} }
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
                                                                            let (G.Id tk _) = $1
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
  | parensOpen EXPR PARENSCLOSE                                         {% do
                                                                            checkRecoverableError $1 $3
                                                                            return $ $2{G.expTok=$1} }
  | minus EXPR                                                          {% buildAndCheckExpr $1 $ G.Op1 G.Negate $2 }
  | not EXPR                                                            {% buildAndCheckExpr $1 $ G.Op1 G.Not $2 }
  | asciiOf EXPR                                                        {% buildAndCheckExpr $1 $ G.AsciiOf $2 }
  | isActive EXPR                                                       {% buildAndCheckExpr $1 $ G.IsActive $2 }
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
                                                                          st <- RWS.get
                                                                          RWS.put st{ST.stScopeStack=[1, 0]} }
  | PROC                                                                {% do
                                                                          st <- RWS.get
                                                                          RWS.put st{ST.stScopeStack=[1, 0]} }

FUNCPREFIX :: { Maybe (ST.Scope, G.Id) }
  : functionBegin ID METHODPARS functionType TYPE                       {% addFunction (ST.Function,
                                                                              $2, G.Callable (Just $5) $3, Nothing) }
FUNC :: { () }
  : FUNCPREFIX CODEBLOCK functionEnd                                    {% do
                                                                            ST.popVisitedMethod
                                                                            case $1 of
                                                                              Nothing -> return ()
                                                                              Just (s, i) -> updateCodeBlockOfFun s i $2 }

PROCPREFIX :: { Maybe (ST.Scope, G.Id) }
  : procedureBegin ID PROCPARSDEC                                       {% addFunction (ST.Procedure,
                                                                              $2, G.Callable Nothing $3, Nothing) }
PROC :: { () }
  : PROCPREFIX CODEBLOCK procedureEnd                                   {% do
                                                                            ST.popVisitedMethod
                                                                            case $1 of
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
  : ID                                                                  { let G.Id t _ = $1 in G.Simple t Nothing }
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

PARENSCLOSE :: { Maybe G.RecoverableError }
  : parensClose                                                         { Nothing }
  | error                                                               { Just G.MissingClosingParens }

STRUCTITS :: { [RecordItem] }
  : STRUCTITS comma STRUCTIT                                            { $3:$1 }
  | STRUCTIT                                                            { [$1] }

STRUCTIT :: { RecordItem }
  : ID ofType TYPE                                                      { ($1, $3) }

ID :: { G.Id }
  : id                                                                  {% do
                                                                            ST.SymTable {ST.stScopeStack = (currScope : _)} <- RWS.get
                                                                            return $ G.Id $1 currScope }

CODEBLOCK :: { G.CodeBlock }
  : INSTBEGIN DECLARS INSTRL INSTEND                                    {% do
                                                                             checkRecoverableError $1 $4
                                                                             return $ G.CodeBlock $ $2 ++ reverse $3 }
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

DECLARS :: { [G.Instruction] }
  : with DECLARSL DECLAREND                                             {% do
                                                                            let instrlist = getAssigsFromDeclarations (reverse $2)
                                                                            checkRecoverableError $1 $3
                                                                            return instrlist }

DECLAREND :: { Maybe G.RecoverableError }
  : declarend                                                           { Nothing }
  | error                                                               { Just G.MissingDeclarationListEnd }

DECLARSL :: { [NameDeclaration] }
  : DECLARSL comma DECLARADD                                            { $3:$1 }
  | DECLARADD                                                           { [$1] }

DECLARADD :: { NameDeclaration }
  : DECLAR                                                              {% do
                                                                            -- Adds a declaration to the ST as soon as it's parsed
                                                                            addIdToSymTable Nothing $1
                                                                            return $1 }

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
                                                                          checkIterVariables $1
                                                                          checkIterableVariables $1
                                                                          checkTypeOfAssignment $1 $3 $2
                                                                          return $ G.InstAsig $1 $3 }
  | malloc EXPR                                                         {% do
                                                                          checkPointerVariable $2
                                                                          return $ G.InstMalloc $2 }
  | free EXPR                                                           {% do
                                                                          checkPointerVariable $2
                                                                          return $G.InstFreeMem $2 }
  | cast ID PROCPARS                                                    {% do
                                                                          checkIdAvailability $2
                                                                          (_, params) <- methodsCheck $2 $3 $1
                                                                          return $ G.InstCallProc $2 params }
  | FUNCALL                                                             {% let (tk, i, params) = $1 in do
                                                                          buildAndCheckExpr tk $ G.EvalFunc i params
                                                                          return $ G.InstCallFunc i params }
  | return                                                              {% checkReturnScope $1 }
  | returnWith EXPR                                                     {% do
                                                                          retExpr <- checkReturnType $2 $1
                                                                          return $ G.InstReturnWith retExpr }
  | print EXPR                                                          { G.InstPrint $2 }
  | read LVALUE                                                         { G.InstRead $2 }
  | whileBegin EXPR covenantIsActive COLON CODEBLOCK WHILEEND           {% do
                                                                            checkRecoverableError $1 $4
                                                                            checkRecoverableError $1 $6
                                                                            checkBooleanGuard $2
                                                                            return $ G.InstWhile $2 $5 }
  | ifBegin IFCASES ELSECASE IFEND                                      {% do
                                                                          checkRecoverableError $1 $4
                                                                          return $ G.InstIf (reverse ($3 : $2)) }
  | ifBegin IFCASES IFEND                                               {% do
                                                                          checkRecoverableError $1 $3
                                                                          return $ G.InstIf (reverse $2) }
  | SWITCHBEGIN COLON SWITCHCASES DEFAULTCASE SWITCHEND                 {% do
                                                                          let (switchTk, switchExpr) = $1
                                                                          checkRecoverableError switchTk $2
                                                                          checkRecoverableError switchTk $5
                                                                          ST.popSwitchType
                                                                          return $ G.InstSwitch switchExpr (reverse ($4 : $3)) }
  | SWITCHBEGIN COLON SWITCHCASES SWITCHEND                             {% do
                                                                          let (switchTk, switchExpr) = $1
                                                                          checkRecoverableError switchTk $2
                                                                          checkRecoverableError switchTk $4
                                                                          ST.popSwitchType
                                                                          return $ G.InstSwitch switchExpr (reverse $3) }
  | FORBEGIN CODEBLOCK FOREND                                           {% do
                                                                          let (fstTk, iterVar, shouldPopIterVar, startExpr, stopExpr) = $1
                                                                          checkRecoverableError fstTk $3
                                                                          if shouldPopIterVar
                                                                          then do
                                                                            ST.popIteratorVariable
                                                                          else return ()
                                                                          return $ G.InstFor iterVar startExpr stopExpr $2 }
  | FOREACHBEGIN CODEBLOCK FOREACHEND                                   {% do
                                                                          let (fstTk, iterVar, shouldPopIterVar, shouldPopIterableVar, iteredExpr) = $1
                                                                          checkRecoverableError fstTk $3
                                                                          RWS.when shouldPopIterVar ST.popIteratorVariable
                                                                          RWS.when shouldPopIterableVar ST.popIterableVariable
                                                                          return $ G.InstForEach iterVar iteredExpr $2 }

SWITCHBEGIN :: { (T.Token, G.Expr) }
  : switchBegin EXPR                                                    {% do
                                                                          ST.addSwitchType (G.expType $2)
                                                                          return ($1, $2) }

FORBEGIN :: { (T.Token, G.Id, Bool, G.Expr, G.Expr) }
  : forBegin ID with EXPR souls untilLevel EXPR                         {% do
                                                                          t1 <- getType $4
                                                                          RWS.when (not $ isIntegerType t1) $
                                                                            logSemError "Step should be an integer" $ G.expTok $4
                                                                          t2 <- getType $7
                                                                          RWS.when (not $ isIntegerType t2) $
                                                                            logSemError "Stop should be an integer" $ G.expTok $7

                                                                          mid <- checkIdAvailability $2
                                                                          case mid of
                                                                            Just ST.DictionaryEntry {ST.name=varName} -> do
                                                                              checkIterVarType $2
                                                                              ST.addIteratorVariable varName
                                                                              return ($1, $2, True, $4, $7)
                                                                            Nothing -> return ($1, $2, False, $4, $7) }

FOREND :: { Maybe G.RecoverableError }
  : forEnd                                                              { Nothing }
  | error                                                               { Just G.MissingForEnd }

FOREACHBEGIN :: { (T.Token, G.Id, Bool, Bool, G.Expr) }
  : forEachBegin ID withTitaniteFrom EXPR                               {% do
                                                                          iteratorType <- getType $2
                                                                          containerType <- getType $4
                                                                          let isContainerAVariable = (case G.expAst $4 of
                                                                                                        G.IdExpr _ -> True
                                                                                                        _ -> False)
                                                                          let ret = ($1, $2, iteratorType /= T.TypeError, containerType /= T.TypeError && isContainerAVariable, $4)
                                                                          if iteratorType == T.TypeError then do
                                                                            if containerType == T.TypeError then return ()
                                                                            else
                                                                              case T.getTypeFromContainer containerType of
                                                                                Nothing -> do
                                                                                  logSemError "Iterable expression is not a container" $ G.expTok $4
                                                                                _ -> do
                                                                                  return ()
                                                                          else do
                                                                            checkIterVarType $2
                                                                            ST.addIteratorVariable $ G.extractIdName $2
                                                                            if containerType == T.TypeError then return ()
                                                                            else
                                                                              case T.getTypeFromContainer containerType of
                                                                                Nothing -> do
                                                                                  logSemError "Iterable expression is not a container" $ G.expTok $4
                                                                                Just t -> do
                                                                                  let (G.Id tk _) = $2
                                                                                  RWS.when (T.TypeError /= t && iteratorType /= t) $
                                                                                    logSemError "Iterator variable is not of the type of contained elements" tk
                                                                          if containerType /= T.TypeError && isContainerAVariable then do
                                                                            let (G.IdExpr (G.Id tk _)) = G.expAst $4
                                                                            ST.addIterableVariable (T.cleanedString tk)
                                                                          else return ()
                                                                          return ret
                                                                             }

FOREACHEND :: { Maybe G.RecoverableError }
  : forEachEnd                                                          { Nothing }
  | error                                                               { Just G.MissingForEachEnd }

WHILEEND :: { Maybe G.RecoverableError }
  : whileEnd                                                            { Nothing }
  | error                                                               { Just G.MissingWhileEnd }

COLON :: { Maybe G.RecoverableError }
  : colon                                                               { Nothing }
  | error                                                               { Just G.MissingColon }

FUNCALL :: { (T.Token, G.Id, G.Params) }
  : summon ID FUNCPARS                                                  { ($1, $2, $3) }

IFCASES :: { G.IfCases }
  : IFCASES IFCASE                                                      { $2 : $1 }
  | IFCASE                                                              { [$1] }

IFCASE :: { G.IfCase }
  : EXPR COLON CODEBLOCK                                                {% do
                                                                            checkRecoverableError (G.expTok $1) $2
                                                                            checkBooleanGuard $1
                                                                            return $ G.GuardedCase $1 $3 }

ELSECASE :: { G.IfCase }
  : else COLON CODEBLOCK                                                {% do
                                                                            checkRecoverableError $1 $2
                                                                            return $ G.GuardedCase (G.Expr
                                                                                                    { G.expAst = G.TrueLit
                                                                                                    , G.expType = T.TrileanT
                                                                                                    , G.expTok = (T.Token
                                                                                                                    { T.aToken = T.TkLit
                                                                                                                    , T.capturedString = "lit"
                                                                                                                    , T.cleanedString = "lit"})
                                                                                                    }) $3 }

IFEND :: { Maybe G.RecoverableError }
  : ifEnd                                                               { Nothing }
  | error                                                               { Just G.MissingIfEnd }

SWITCHCASES :: { G.SwitchCases }
  : SWITCHCASES SWITCHCASE                                              { $2 : $1 }
  | SWITCHCASE                                                          { [$1] }

SWITCHCASE :: { G.SwitchCase }
  : EXPR COLON CODEBLOCK                                                {% do
                                                                            checkRecoverableError (G.expTok $1) $2
                                                                            expr <- checkSwitchCaseType $1
                                                                            return $ G.Case expr $3 }

SWITCHEND :: { Maybe G.RecoverableError }
  : switchEnd                                                           { Nothing }
  | error                                                               { Just G.MissingSwitchEnd }

DEFAULTCASE :: { G.SwitchCase }
  : switchDefault COLON CODEBLOCK                                       {% do
                                                                            checkRecoverableError $1 $2
                                                                            return $ G.DefaultCase $3 }

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

getAssigsFromDeclarations :: [NameDeclaration] -> [G.Instruction]
getAssigsFromDeclarations = map buildAsigInstr . filter hasInitialization
  where
    hasInitialization :: NameDeclaration -> Bool
    hasInitialization (_, _, _, exp) = isJust exp
    buildAsigInstr :: NameDeclaration -> G.Instruction
    buildAsigInstr (_, gId@(G.Id tk _), _, Just exp) =
      let expr = G.Expr
                  { G.expAst = G.IdExpr gId
                  , G.expType = G.expType exp
                  , G.expTok = tk
                  } in G.InstAsig expr exp

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
  let tk@T.Token {T.aToken=abst, T.position=pn} = errors !! 0
      name = show tk
      line = show $ row pn
      column' = show $ column pn
      header = bold ++ red ++ "YOU DIED!!" ++ nocolor ++ " Parser error: "
      endmsg = "\n\nFix your syntax errors, ashen one."
      position = "line " ++ bold ++ red ++ line ++ nocolor ++ ", column " ++ bold ++ red ++ column' ++ nocolor ++ "."
      msg = header ++ "Unexpected token " ++ bold ++ red ++ name ++ nocolor ++ " at " ++ position ++ endmsg
  in  fail msg

addIdsToSymTable :: [NameDeclaration] -> ST.ParserMonad ()
addIdsToSymTable ids = do
  RWS.mapM_ (addIdToSymTable Nothing) ids

addIdToSymTable :: Maybe Int -> NameDeclaration -> ST.ParserMonad ()
addIdToSymTable mi d@(c, gId@(G.Id tk@(T.Token {T.aToken=at, T.cleanedString=idName}) _), t, maybeExp) = do
  maybeIdEntry <- ST.dictLookup idName
  maybeTypeEntry <- findTypeOnEntryTable t
  ST.SymTable {ST.stScopeStack=(currScope:_), ST.stIterationVars=iterVars} <- RWS.get
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
        (Just en, Just exp) -> do
          checkTypeOfAssignment en exp (G.expTok exp)
        _ -> return ()

    -- The name does exists on the table, we just add it depending on the scope
    Just entry -> do
      let scope = ST.scope entry
      let category = ST.category entry
      if category == ST.Type && c /= ST.RecordItem
      then logSemError ("Name " ++ show tk ++ " conflicts with an type alias") tk
      else if category == ST.Procedure
      then logSemError ("Name " ++ show tk ++ " conflicts with a procedure") tk
      else if category == ST.Function
      then logSemError ("Name " ++ show tk ++ " conflicts with a function") tk
      else if category == ST.RefParam || category == ST.ValueParam
      then logSemError ("Name " ++ show tk ++ " conflicts with a formal param") tk
      else if (T.cleanedString tk) `elem` iterVars
      then logSemError ("Name " ++ show tk ++ " conflicts or shadows an iteration variable") tk
      else if currScope /= scope
      then insertIdToEntry mi t ST.DictionaryEntry
        { ST.name = idName
        , ST.category = c
        , ST.scope = currScope
        , ST.entryType = ST.name <$> maybeTypeEntry
        , ST.extra = []
        }
      else logSemError ("Name " ++ idName ++ " was already declared on this scope") tk

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
          st@ST.SymTable {ST.stScopeStack=scopes} <- RWS.get
          RWS.put st{ST.stScopeStack=(scope:scopes)}
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
            st@ST.SymTable {ST.stScopeStack=scopes} <- RWS.get
            let (ST.Fields ST.Callable scope) = head $ filter ST.isFieldsExtra ex
            RWS.put st{ST.stScopeStack=(scope:scopes)}
            RWS.mapM_ (\(i, n) -> addIdToSymTable (Just i) n) $ zip [0..] $
              map (\(argType, i, t) -> (if argType == G.Val
                                  then ST.ValueParam
                                  else ST.RefParam, i, t, Nothing)) s

checkConstantReassignment :: G.Expr -> ST.ParserMonad ()
checkConstantReassignment e = case G.expAst e of
  G.IdExpr (G.Id tk@(T.Token {T.cleanedString=idName}) _) -> do
    maybeEntry <- ST.dictLookup idName
    case maybeEntry of
      Nothing -> do
        return ()
      Just e ->
        case (ST.category e) of
          ST.Constant -> do
            logSemError ("Name " ++ idName ++ " is a constant and must not be reassigned") tk
            return ()
          _ ->
            return ()
  G.IndexAccess gId _ -> checkConstantReassignment gId
  _ -> return ()

checkPointerVariable :: G.Expr -> ST.ParserMonad ()
checkPointerVariable G.Expr {G.expType=(T.PointerT _)} = return ()
checkPointerVariable G.Expr {G.expTok=tk} =
  logSemError ("Expresion " ++ show tk ++ " must be a valid pointer") tk

checkReturnScope :: T.Token -> ST.ParserMonad (G.Instruction)
checkReturnScope tk = do
  ST.SymTable {ST.stVisitedMethod=vMethod} <- RWS.get
  case vMethod of
    Nothing -> return (G.InstReturn)
    Just methodName -> do
      currMethod <- ST.dictLookup $ methodName
      case currMethod of
        Just (ST.DictionaryEntry {ST.category=cat}) -> do
          case cat of
            ST.Function -> do
              logSemError ("Returning without an expression is not allowed inside functions") tk
              return (G.InstReturn)
            _ -> return (G.InstReturn)
        _ -> return (G.InstReturn)

checkReturnType :: G.Expr -> T.Token -> ST.ParserMonad (G.Expr)
checkReturnType e tk = do
  let eType = G.expType e
  ST.SymTable {ST.stVisitedMethod=vMethod} <- RWS.get
  case vMethod of
    Nothing -> do
      logSemError ("Returning with an expression outside of a function not allowed") tk
      return e
    Just methodName -> do
      currMethod <- ST.dictLookup $ methodName
      case currMethod of
        Nothing -> do
          logSemError ("Returning with an expression outside of a function not allowed") tk
          return e
        Just method -> do
          let ST.DictionaryEntry {ST.category=cat} = method
          mType <- getType method
          case cat of
            ST.Function -> do
              let T.FunctionT _ fType = mType
              -- If they're equal, or compatible through casting, this raises no error
              if eType `T.canBeConvertedTo` fType
              then let [castedExpr] = addCastToExprs [(e, fType)] in
                return (castedExpr)
              else do
                logSemError ("Return expression type " ++ show eType ++ " incompatible with function return type " ++ show fType) tk
                return e
            _ -> do
              logSemError ("Returning with an expression outside of a function not allowed") tk
              return e

checkIterVariables :: G.Expr -> ST.ParserMonad ()
checkIterVariables e = case G.expAst e of
  G.IdExpr (G.Id tk@(T.Token {T.cleanedString=idName}) _) -> do
    ST.SymTable {ST.stIterationVars=iterVars} <- RWS.get
    let matchesIterVar = idName `elem` iterVars
    if matchesIterVar
    then logSemError ("Iteration variable " ++ show tk ++ " must not be reassigned") tk
    else return ()
  _ -> return ()

checkIterableVariables :: G.Expr -> ST.ParserMonad ()
checkIterableVariables e = case G.expAst e of
  G.IdExpr (G.Id tk@(T.Token {T.cleanedString=idName}) _) -> do
    ST.SymTable {ST.stIterableVars=iterableVars} <- RWS.get
    let matchesIterVar = idName `elem` iterableVars
    if matchesIterVar
    then logSemError ("Iterable container " ++ show tk ++ " must not be reassigned") tk
    else return ()
  _ -> return ()

checkIdAvailability :: G.Id -> ST.ParserMonad (Maybe ST.DictionaryEntry)
checkIdAvailability (G.Id tk@(T.Token {T.cleanedString=idName}) _) = do
  maybeEntry <- ST.dictLookup idName
  case maybeEntry of
    Nothing -> do
      logSemError ("Name " ++ show tk ++ " is not available on this scope") tk
      return Nothing
    Just e -> return $ Just e

checkIterVarType :: G.Id -> ST.ParserMonad ()
checkIterVarType (G.Id tk@(T.Token {T.cleanedString=idName}) _) = do
  maybeEntry <- ST.dictLookup idName
  case maybeEntry of
    Nothing -> return ()
    Just (ST.DictionaryEntry {ST.category=varCat}) -> do
      if varCat == ST.Constant
      then do
        logSemError ("Constant " ++ show tk ++ " can not be used as an iteration variable") tk
      else return ()

checkSwitchCaseType :: G.Expr -> ST.ParserMonad (G.Expr)
checkSwitchCaseType expr = do
  ST.SymTable {ST.stSwitchTypes=swTypes} <- RWS.get
  if swTypes == [] then return (expr)
  else do
    let swType = head swTypes
    case swType of
      T.TypeError -> return (expr)
      tp -> do
        let exprType = G.expType expr
        if exprType `T.canBeConvertedTo` tp
        then let [castedExpr] = addCastToExprs [(expr, tp)] in
          return (castedExpr)
        else do
          let tk = G.expTok expr
          RWS.tell [Error ("Expresion type " ++ show exprType ++ " conflicts with switch type (expected: " ++ show tp ++ ")") (T.position tk)]
          return (expr)

checkRecoverableError :: T.Token -> Maybe G.RecoverableError -> ST.ParserMonad ()
checkRecoverableError openTk maybeErr = do
  case maybeErr of
    Nothing -> return ()
    Just err -> do
      let errorName = show err
      logSemError (errorName ++ " (recovered from to continue parsing)") openTk

extractFieldsFromExtra :: [ST.Extra] -> ST.Extra
extractFieldsFromExtra [] = error "The `extra` array doesn't have any `Fields` item"
extractFieldsFromExtra (s@ST.Fields{} : _) = s
extractFieldsFromExtra (_:ss) = extractFieldsFromExtra ss

addFunction :: NameDeclaration -> ST.ParserMonad (Maybe (ST.Scope, G.Id))
addFunction d@(_, i@(G.Id tk@(T.Token {T.cleanedString=idName}) _), _, _) = do
  ST.SymTable {ST.stCurrScope=currScope} <- RWS.get
  ST.addVisitedMethod idName
  maybeEntry <- ST.dictLookup idName
  case maybeEntry of
    Nothing -> do
      addIdToSymTable Nothing d
      return $ Just (currScope, i)
    Just entry -> do
      logSemError ("Function " ++ idName ++ " was already declared") tk
      return Nothing

updateCodeBlockOfFun :: ST.Scope -> G.Id -> G.CodeBlock -> ST.ParserMonad ()
updateCodeBlockOfFun currScope (G.Id tk@(T.Token {T.cleanedString=idName}) _) code = do
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
      logSemError ("Type " ++ show tk ++ " not found") tk
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
  st@ST.SymTable {ST.stCurrScope=currScope} <- RWS.get
  let constr = (case T.aToken tk of
                  T.TkRecord -> ST.Record
                  T.TkUnionStruct -> ST.Union)

  let ret = Just [ST.Fields constr $ currScope + 1]
  RWS.put st{ST.stCurrScope=(currScope + 1)}
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
      st@ST.SymTable{ST.stCurrScope=currScope} <- RWS.get
      let ret = Just [ST.Fields ST.Callable $ currScope + 1]
      RWS.put st{ST.stCurrScope=currScope + 1}
      return ret
    Just tt -> do
      mExtra <- buildExtraForType tt
      case mExtra of
        Nothing -> return $ Nothing
        Just extras -> do
          st@ST.SymTable {ST.stCurrScope=currScope} <- RWS.get
          let ret = Just ((ST.Fields ST.Callable $ currScope + 1) : extras)
          RWS.put st{ST.stCurrScope=(currScope + 1)}
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
  case t of
    -- case when there is already a type error on the list
    (Nothing, T.TypeError) -> return (T.TypeError, exprCons exprs)

    -- a particular expression couldn't be casted to the accumulated type
    (Just x, T.TypeError) -> do
      let tk = G.expTok $ exprs !! x -- not so efficient, but works
      logSemError ("Type of item #" ++ show x ++ " of list mismatch") tk
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
      logSemError "Left side is not a chest" tk
      return T.TypeError

domainCallCheck :: G.Id -> [T.Type] -> [G.Expr] -> T.Token -> ST.ParserMonad (Maybe [G.Expr])
domainCallCheck methodId domain exprs tk = do
  let exprsTypes = exprsToTypes exprs
  let exprsTypesL = length exprsTypes
  let domainL = length domain
  if exprsTypesL < domainL then do
    logSemError ("Method " ++ show methodId ++ " called with less arguments than expected") tk
    return Nothing
  else if exprsTypesL > domainL then do
    logSemError ("Method " ++ show methodId ++ " called with more arguments than expected") tk
    return Nothing
  else do
    if T.TypeError `elem` exprsTypes then
      return Nothing
    else case findInvalidArgument $ zip exprsTypes domain of
      -- all looking good, now cast each argument to it's correspondent parameter type
      Nothing -> do
        let castedExprs = addCastToExprs $ zip exprs domain
        return $ Just castedExprs

      -- oh no, an argument could not be implicitly casted to its correspondent argument
      Just x -> do
        let formalType = domain !! x
        paramType <- getType $ exprs !! x
        let errMsgTypes = "Type mismatch: param. #" ++ show x ++ " received " ++ show paramType ++ " and expected " ++ show formalType
        let errMsg =  errMsgTypes ++ " while calling  " ++ show methodId
        logSemError errMsg tk
        return Nothing
  where
    -- left side is the type of the argument, right side is the parameter type
    findInvalidArgument :: [(T.Type, T.Type)] -> Maybe Int
    findInvalidArgument ts = findInvalid ts 0

    findInvalid :: [(T.Type, T.Type)] -> Int -> Maybe Int
    findInvalid [] _ = Nothing
    findInvalid ((argT, paramT) : xs) i = if argT `T.canBeConvertedTo` paramT
      then findInvalid xs (i + 1)
      else Just i

methodsCheck :: G.Id -> [G.Expr] -> T.Token -> ST.ParserMonad (T.Type, [G.Expr])
methodsCheck methodId exprs tk = do
  procType <- getType methodId
  case procType of
    T.ProcedureT domain -> do
      mParams <- domainCallCheck methodId domain exprs tk
      case mParams of
        Nothing -> return (T.VoidT, exprs)
        Just params -> return (T.VoidT, params)
    T.FunctionT domain range -> do
      mParams <- domainCallCheck methodId domain exprs tk
      case mParams of
        Nothing -> return (T.TypeError, exprs)
        Just params -> return (range, params)
    T.TypeError -> return (T.TypeError, exprs)
    _ -> do
      logSemError "You're trying to call a non-callable expression" tk
      return (T.TypeError, exprs)

functionsCheck :: G.Id -> [G.Expr] -> T.Token -> ST.ParserMonad (T.Type, G.BaseExpr)
functionsCheck i params tk = do
  (t, ps) <- methodsCheck i params tk
  return (t, G.EvalFunc i ps)

memAccessCheck :: G.Expr -> T.Token -> ST.ParserMonad T.Type
memAccessCheck expr tk = do
  t <- getType expr
  case t of
    T.PointerT t' -> return t'
    T.TypeError -> return T.TypeError
    _ -> do
      logSemError "Trying to access memory of non-arrow variable" tk
      return T.TypeError

checkAsciiOf :: G.Expr -> T.Token -> ST.ParserMonad T.Type
checkAsciiOf e tk = do
  t <- getType e
  case t of
    T.CharT -> return T.BigIntT
    T.StringT -> return $ T.ArrayT T.BigIntT
    T.TypeError -> return T.TypeError
    _ -> do
      logSemError ("ascii_of requires argument to be a miracle or a sign") tk
      return T.TypeError

checkIsActive :: G.Expr -> T.Token -> ST.ParserMonad T.Type
checkIsActive e tk = do
  t <- getType e
  case t of
    T.TypeError -> return T.TypeError
    _ ->
      if isUnionAttr then return T.TrileanT
      else do
        logSemError ("is_active requires argument to be a union attribute") tk
        return T.TypeError
  where
    isUnionAttr = case e of
      (G.Expr {G.expAst=(G.Access r _)}) -> isUnion r
      _ -> False
    isUnion (G.Expr {G.expType=(T.UnionT _ _)}) = True
    isUnion _ = False

checkAccess :: G.Expr -> G.Id -> T.Token -> ST.ParserMonad (T.Type, G.BaseExpr)
checkAccess e (G.Id tk'@T.Token{T.cleanedString=i} _) tk = do
  t <- getType e
  case t of
    T.RecordT scope properties -> checkProperty properties scope
    T.UnionT scope properties -> checkProperty properties scope
    T.TypeError -> return defaultReturn
    _ -> do
      RWS.liftIO $ print $ show t
      logSemError "Left side of access is not a record nor union" tk
      return defaultReturn
  where
    baseExpr :: G.Id -> G.BaseExpr
    baseExpr = G.Access e
    defaultReturn :: (T.Type, G.BaseExpr)
    defaultReturn = (T.TypeError, baseExpr $ G.Id tk' (-1))
    checkProperty :: [T.PropType] -> Int -> ST.ParserMonad (T.Type, G.BaseExpr)
    checkProperty props scope =
      case filter ((==i) . fst) $ map (\(T.PropType e) -> e) props of
        ((_,a):_) -> do
          return (a, baseExpr $ G.Id tk' scope)
        _ -> do
          logSemError ("Property " ++ i ++ " doesn't exist") tk
          return defaultReturn

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
    logSemError (T.typeMismatchMessage tk) tk
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
    logSemError "Left and right side of operand can't be operated together" tk
    return (T.TypeError, G.Op2 op leftExpr rightExpr)

  where
    expectedForOperands :: [T.Type]
    expectedForOperands
      | op `elem` G.arithmeticOp2 = T.arithmeticTypes
      | op `elem` G.comparableOp2 = T.anySingleton -- we can compare any type if they are the same
      | op `elem` G.booleanOp2 = T.booleanSingleton
      | op `elem` G.setOp2 = T.anySetSingleton
      | op `elem` G.arrayOp2 = T.arrayLikeSingleton
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
        logSemError (T.typeMismatchMessage tk) tk
        return (T.TypeError, exp)

checkSetSize :: G.Expr -> T.Token -> ST.ParserMonad T.Type
checkSetSize expr tk = do
  t <- getType expr
  case t of
    T.SetT _ -> return T.BigIntT
    T.TypeError -> return T.TypeError
    _ -> do
      logSemError "`size` expects a set" tk
      return T.TypeError

checkTypeOfAssignment :: (TypeCheckable a, TypeCheckable b) => a -> b -> T.Token -> ST.ParserMonad ()
checkTypeOfAssignment lval rval tk = do
  lvalType <- getType lval
  rvalType <- getType rval
  if rvalType `T.canBeConvertedTo` lvalType
  then return ()
  else do
    let errorDetails = "(couldn't convert " ++ (show rvalType) ++ " to " ++ (show lvalType) ++ ")"
    -- don't propagate type errors further
    if T.TypeError `elem` [lvalType, rvalType]
    then return ()
    else logSemError ("Type mismatch on assignment " ++ errorDetails) tk

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

checkBooleanGuard :: G.Expr -> ST.ParserMonad ()
checkBooleanGuard expr = do
  t <- getType expr
  RWS.when (t /= T.TrileanT) $ do
    -- Avoid propagating the error if it has been raised somewhere else already
    if t == T.TypeError
    then return ()
    else logSemError "Guard should be of trilean data type" $ G.expTok expr

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

  G.IdExpr i -> checkIdType i
  G.IndexAccess e i -> buildTypeForNonCasterExprs (checkIndexAccess e i tk) bExpr
  G.EvalFunc i params -> functionsCheck i params tk
  G.MemAccess e -> buildTypeForNonCasterExprs (memAccessCheck e tk) bExpr
  G.IsActive e -> buildTypeForNonCasterExprs (checkIsActive e tk) bExpr
  G.AsciiOf e -> buildTypeForNonCasterExprs (checkAsciiOf e tk) bExpr
  G.Access e i -> checkAccess e i tk
  G.SetSize e -> buildTypeForNonCasterExprs (checkSetSize e tk) bExpr

checkIdType :: G.Id -> ST.ParserMonad (T.Type, G.BaseExpr)
checkIdType gId@(G.Id tk _) = do
  idEntry <- checkIdAvailability gId
  case idEntry of
    Nothing -> return (T.TypeError, G.IdExpr gId)
    Just entry -> do
      t <- getType gId
      return (t, G.IdExpr $ G.Id tk $ ST.scope entry)

instance TypeCheckable G.Id where
  getType gId = do
    idEntry <- checkIdAvailability gId
    case idEntry of
      Nothing -> return T.TypeError
      Just entry -> getType entry

instance TypeCheckable G.Expr where
  getType = return . G.expType

instance TypeCheckable ST.DictionaryEntry where
  getType entry@ST.DictionaryEntry{ST.entryType=Just _, ST.category = cat, ST.extra = extras}
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
      case range of
        -- The range is returned as a one-type typelist, we return the actual type
        T.TypeList (r:_) -> return $ T.FunctionT domain r
        _ -> return $ T.FunctionT domain range

    | cat `elem` [
        ST.Variable, ST.Constant,
        ST.RecordItem, ST.UnionItem,
        ST.RefParam, ST.ValueParam] = getType $ ST.extractTypeFromExtra extras

    | otherwise = error "error on getType for expected dict entries"

  getType entry@ST.DictionaryEntry{ST.entryType=Nothing, ST.category=ST.Procedure, ST.extra=extras} = do
    let isEmptyProc = not . null $ filter ST.isEmptyFunction extras
    domain <- (if isEmptyProc
      then return []
      else do
        let fields = head $ filter ST.isFieldsExtra extras
        (T.TypeList list) <- getType fields
        return list
        )
    return $ T.ProcedureT domain

  getType _ = error "error on getType for unexpected dict entries"

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
    ST.SymTable {ST.stDict=dict} <- RWS.get
    types <- mapM getType $ ST.sortByArgPosition $ ST.findAllInScope scope dict
    return $ T.TypeList types

  getType (ST.Fields b scope) = buildStruct b scope

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
  getType _ = error "error on getType for SymTable extra"

buildStruct :: ST.TypeFields -> Int -> ST.ParserMonad (T.Type)
buildStruct t scope = do
  ST.SymTable {ST.stDict=dict} <- RWS.get
  let entries = ST.findAllInScope scope dict
  let entryNames = map ST.name entries
  types <- mapM getType entries
  case t of
    ST.Record -> return $ T.RecordT scope $ map T.PropType $ zip entryNames types
    ST.Union -> return $ T.UnionT scope $ map T.PropType $ zip entryNames types
    _ -> error "wrong data type for Fields extra"

logSemError :: String -> T.Token -> ST.ParserMonad ()
logSemError msg tk = RWS.tell [Error msg (T.position tk)]
}